@Part(03, Root="ada.mss")

@Comment{$Date: 2012/05/19 02:05:51 $}

@Comment{$Source: e:\\cvsroot/ARM/Source/03c.mss,v $}
@Comment{$Revision: 1.127 $}

@LabeledClause{Tagged Types and Type Extensions}

@begin{Intro}
@redundant[@PDefn{dispatching operation}
@Defn{polymorphism}
@IndexSee{Term=[dynamic binding],See=(dispatching operation)}
@IndexSeeAlso{Term=[generic unit],See=(dispatching operation)}
@IndexSeeAlso{Term=[variant],See=(tagged type)}
Tagged types and type extensions support
object-oriented programming, based on
inheritance with extension and run-time polymorphism via
@i(dispatching operations).
@IndexSee{Term=[object-oriented programming (OOP)],See=[tagged types and type extensions]}
@IndexSee{Term=[OOP (object-oriented programming)],See=[tagged types and type extensions]}
@IndexSeeAlso{Term=[inheritance],See=[tagged types and type extension]}]
@end{Intro}

@begin{MetaRules}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
The intended implementation model is for @Chg{Version=[2],New=[the static
portion of ],Old=[]}a tag to be represented as a
pointer to a statically allocated and link-time initialized type
descriptor. The type descriptor contains the address of the code for
each primitive operation of the type. It probably also contains
other information, such as might make membership tests convenient and
efficient.@Chg{Version=[2],New=[ Tags for nested type extensions must also have
a dynamic part that identifies the particular elaboration of the type.],Old=[]}

The primitive operations of a tagged type are known at its first
freezing point; the type descriptor is laid out at that point.
It contains linker symbols for each primitive operation; the linker
fills in the actual addresses.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
@ChgAdded{Version=[2],Text=[Primitive operations of type extensions that are
declared at a level deeper than the level of the ultimate ancestor from which
they are derived can
be represented by wrappers that use the dynamic part of the tag to call the
actual primitive operation. The dynamic part would generally be some way to
represent the static link or display necessary for making a nested call. One
implementation strategy would be to store that information in the extension
part of such nested type extensions, and use the dynamic part of the tag to
point at it. (That way, the @lquotes@;dynamic@rquotes@; part of the tag could
be static, at the cost of indirect access.)]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
@ChgAdded{Version=[2],Text=[If the tagged type is descended from any interface
types, it also will need to include @lquotes@;subtags@rquotes@; (one for
each interface) that describe the mapping of the primitive operations of the
interface to the primitives of the type. These subtags could directly reference
the primitive operations (for faster performance), or simply provide the tag
@lquotes@;slot@rquotes@; numbers for the primitive operations (for easier
derivation). In either case, the subtags would be used for calls that dispatch
through a class-wide type of the interface.]}

Other implementation models are possible.

The rules ensure that @lquotes@;dangling dispatching@rquotes@; is impossible;
that is, when a dispatching call is made, there is always a body to
execute. This is different from some other object-oriented
languages, such as Smalltalk, where it is possible to get a run-time
error from a missing method.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
Dispatching calls should be efficient, and should have a bounded
worst-case execution time. This is important in a language intended
for real-time applications. In the intended implementation model, a
dispatching call involves calling indirect through the appropriate
slot in the dispatch table. No complicated "method lookup" is
involved@Chg{Version=[2],New=[ although a call which is dispatching on
an interface may require a lookup of the appropriate interface subtag],Old=[]}.

The programmer should have the choice at each call site of a
dispatching operation whether to do a dispatching call or a
statically determined call (i.e. whether the body executed should be
determined at run time or at compile time).

The same body should be executed for a call where the tag is
statically determined to be T'Tag as for a dispatching call where the
tag is found at run time to be T'Tag. This allows one to test a
given tagged type with statically determined calls, with some
confidence that run-time dispatching will produce the same behavior.

All views of a type should share the same type descriptor and the
same tag.

The visibility rules determine what is legal at compile time; they
have nothing to do with what bodies can be executed at run time.
Thus, it is possible to dispatch to a subprogram whose declaration is
not visible at the call site. In fact, this is one of the primary
facts that gives object-oriented programming its power. The
subprogram that ends up being dispatched to by a given call might
even be designed long after the call site has been coded and
compiled.

Given that Ada has overloading, determining whether a given
subprogram overrides another is based both on the names and the type
profiles of the operations.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00401-01]}
When a type extension is declared, if there is any place within its
immediate scope where a certain subprogram of the parent
@Chg{Version=[2],New=[or progenitor ],Old=[]}is visible,
then a matching subprogram should override. If there is no such
place, then a matching subprogram should be totally unrelated, and
occupy a different slot in the type descriptor. This is important to
preserve the privacy of private parts; when an operation declared in
a private part is inherited, the inherited version can be overridden
only in that private part, in the package body, and in any children
of the package.

If an implementation shares code for instances
of generic bodies, it should be
allowed to share type descriptors of tagged types
declared in the generic body, so long as they are not extensions of types
declared in the specification of the generic unit.
@end{MetaRules}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@Defn{tagged type}
A record type or private type that has the reserved word @key(tagged)
in its declaration is called a @i(tagged) type.@Chg{Version=[2],New=[ In
addition, an interface type is a tagged type, as is a task or protected
type derived from an interface (see @RefSecNum{Interface Types}).],Old=[]}
@Redundant[When deriving
from a tagged type, @Chg{Version=[2],New=[as],Old=[additional components
may be defined. As]} for any derived type,
additional primitive subprograms may be defined,
and inherited primitive subprograms may be overridden.]
@Defn{type extension}
@Defn2{Term=[extension], Sec=(of a type)}
The derived type is called an @i(extension)
of @Chg{Version=[2],New=[its],Old=[the]} ancestor
@Chg{Version=[2],New=[types],Old=[type]}, or simply a @i(type
extension).@Chg{Version=[2],New=[],Old=[ @Defn2{Term=[extension], Sec=(of a record type)}
@Defn{private extension}
@Defn2{Term=[extension], Sec=(of a private type)}
Every type extension is also a tagged type, and
is either a @i(record extension) or a @i(private extension) of
some other tagged type.
A record extension is defined by a @nt<derived_type_definition>
with a @nt<record_extension_part>.
A private extension, which is a partial view of a record extension,
can be declared in the visible part of a package
(see @RefSecNum(Private Types and Private Extensions))
or in a generic formal part
(see @RefSecNum(Formal Private and Derived Types)).]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[extension], Sec=(of a record type)}
@Defn{private extension}
@Defn2{Term=[extension], Sec=(of a private type)}
Every type extension is also a tagged type, and
is a @i(record extension) or a @i(private extension) of some other
tagged type, or a noninterface synchronized tagged type (see
@RefSecNum{Interface Types}).
A record extension is defined by a @nt<derived_type_definition>
with a @nt<record_extension_part> (see @RefSecNum{Type Extensions})@Redundant[,
which may include the definition of additional components].
A private extension, which is a partial view of a record extension or
of a synchronized tagged type,
can be declared in the visible part of a package
(see @RefSecNum(Private Types and Private Extensions))
or in a generic formal part
(see @RefSecNum(Formal Private and Derived Types)).]}
@ToGlossary{Term=<Tagged type>,
  Text=<The objects of a tagged type have a run-time type tag,
  which indicates the specific type with which the object was originally
  created.
  An operand of a class-wide tagged type
  can be used in a dispatching call;
  the tag indicates which subprogram body to invoke.
  Nondispatching calls, in which the subprogram body to invoke
  is determined at compile time, are also allowed.
  Tagged types may be extended with additional components.>}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00218-03]}
If a tagged type is declared other than in a @nt{package_specification},
it is impossible to add new primitive subprograms for that type,
although it can inherit primitive subprograms,
and those can be overridden.
If the user incorrectly thinks a certain subprogram is primitive when it
is not, and tries to call it with a dispatching call, an error message
will be given at the call site.@Chg{Version=[2],New=[ Similarly, by using
an @nt{overriding_indicator} (see @RefSecNum{Subprogram Declarations}),
the user can declare that a subprogram is intended to be overriding, and
get an error message when they made a mistake. The use of
@nt{overriding_indicator}s is highly recommended in new code that does not
need to be compatible with Ada 95.],Old=[]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00344-01]}
@ChgDeleted{Version=[2],Text=[Note that the accessibility rules imply that a
tagged type declared in a library @nt{package_specification} cannot be
extended in a nested subprogram or task body.]}
@end{Ramification}

@Defn{tag of an object}
An object of a tagged type has an associated (run-time)
@i(tag) that identifies the specific tagged type used to create
the object originally.
@Redundant[
The tag of an operand of a class-wide tagged type @i(T)'Class
controls which subprogram body is to be executed when a
primitive subprogram of type @i(T) is applied to
the operand
(see @RefSecNum(Dispatching Operations of Tagged Types));
@Defn{dispatching}
using a tag to control which body to execute is called @i(dispatching).]
@IndexSee{Term=[type tag],See=[tag]}
@IndexSee{Term=[run-time type],See=[tag]}
@IndexSeeAlso{Term=[type],See=[tag]}
@IndexSeeAlso{Term=[class],See=[tag]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00344-01]}
The tag of a specific tagged type identifies
the @nt<full_type_declaration> of the type@Chg{Version=[2],New=[, and
for a type extension, is sufficient to uniquely identify the type among
all descendants of the same ancestor],Old=[]}.
If a declaration for a tagged type occurs within a
@nt{generic_package_declaration},
then the corresponding type declarations in distinct
instances of the generic package are associated with distinct tags.
For a tagged type that is local to a generic package body@Chg{Version=[2],
New=[ and with all of its ancestors (if any) also local to the
generic body],Old=[]},
the language does not specify whether repeated instantiations
of the generic body result in distinct tags.@PDefn{Unspecified}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00344-01]}
  @ChgDeleted{Version=[2],Text=[This eases generic code sharing.]}
@end{reason}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00344-01]}
  @ChgAdded{Version=[2],Text=[In most cases, a tag need only identify a particular
    tagged type declaration, and can therefore be a simple link-time-known
    address. However, for tag checks
    (see @RefSecNum{Dispatching Operations of Tagged Types}) it is essential
    that each descendant (that currently exists) of a given type have
    a unique tag. Hence, for types declared in shared generic bodies
    where an ancestor comes from outside the generic, or for types
    declared at a deeper level than an ancestor, the tag needs to be
    augmented with some kind of dynamic descriptor (which may be a
    static link, global display, instance descriptor pointer, or combination).
    This implies that type Tag may need to be two words, the second of which
    is normally null, but in these identified special cases needs to
    include a static link or equivalent. Within an object of one of
    these types with a two-word tag, the two parts of the tag would
    typically be separated, one part as the first word of the object,
    the second placed in the first extension part that corresponds to a
    type declared more nested than its parent or declared in a shared
    generic body when the parent is declared outside. Alternatively,
    by using an extra level of indirection, the type Tag could remain
    a single-word.]}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00344-01]}
  @Chg{Version=[2],New=[For types that are not type extensions (even
    for ones declared in nested scopes), we do not require that],
    Old=[The language does not specify whether]}
    repeated elaborations of the same @nt<full_type_declaration>
    correspond to distinct tags. @Chg{Version=[2],New=[This was done so that
    Ada 2005 implementations of tagged types could maintain representation
    compatibility with Ada 95 implementations. Only type extensions that were
    not allowed in Ada 95 require additional information with the tag.],
    Old=[In most cases, we
    expect that all elaborations will correspond to the same tag,
    since the tag will frequently be the address (or index) of a statically
    allocated type descriptor. However, with shared
    generics, the type descriptor might have to be allocated on a per-instance
    basis, which in some implementation models implies per-elaboration of the
    instantiation.]}
@end{implnote}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00344-01]}
  @ChgAdded{Version=[2],Text=[The wording @lquotes@;is sufficient to uniquely
  identify the type among all descendants of the same ancestor@rquotes@; only
  applies to types that currently exist. It is not necessary to distinguish
  between descendants that currently exist, and descendants of the same type
  that no longer exist.
  For instance, the address of the stack frame of the subprogram that created
  the tag is sufficient to meet the requirements of this rule, even though
  it is possible, after the subprogram returns, that a later call of the
  subprogram could have the same stack frame and thus have an identical tag.]}
@end{Honest}

@Leading@keepnext@;The following language-defined library package exists:
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00362-01]}
@ChildUnit{Parent=[Ada],Child=[Tags]}@key[package] Ada.Tags @key[is]
    @Chg{Version=[2],New=[@key[pragma] Preelaborate(Tags);
    ],Old=[]}@key[type] @AdaTypeDefn{Tag} @key[is] @key[private];@Chg{Version=[2],New=[
    @key[pragma] Preelaborable_Initialization(Tag);],Old=[]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02]}
@ChgAdded{Version=[2],Text=[    @AdaObjDefn{No_Tag} : @key[constant] Tag;]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00400-01]}
    @key[function] @AdaSubDefn{Expanded_Name}(T : Tag) @key[return] String;@Chg{Version=[2],New=[
    @key[function] @AdaSubDefn{Wide_Expanded_Name}(T : Tag) @key[return] Wide_String;
    @key[function] @AdaSubDefn{Wide_Wide_Expanded_Name}(T : Tag) @key[return] Wide_Wide_String;],Old=[]}
    @key[function] @AdaSubDefn{External_Tag}(T : Tag) @key[return] String;
    @key[function] @AdaSubDefn{Internal_Tag}(External : String) @key[return] Tag;

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00344-01]}
@ChgAdded{Version=[2],Text=[    @key[function] @AdaSubDefn{Descendant_Tag}(External : String; Ancestor : Tag) @key[return] Tag;
    @key[function] @AdaSubDefn{Is_Descendant_At_Same_Level}(Descendant, Ancestor : Tag)
        @key[return] Boolean;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02]}
@ChgAdded{Version=[2],Text=[    @key[function] @AdaSubDefn{Parent_Tag} (T : Tag) @key[return] Tag;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00405-01]}
@ChgAdded{Version=[2],Text=[    @key[type] @AdaTypeDefn{Tag_Array} @key[is array] (Positive @key[range] <>) @key[of] Tag;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00405-01]}
@ChgAdded{Version=[2],Text=[    @key[function] @AdaSubDefn{Interface_Ancestor_Tags} (T : Tag) @key[return] Tag_Array;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0173-1]}
@ChgAdded{Version=[3],Text=[    @key[function] @AdaSubDefn{Is_Abstract} (T : Tag) @key[return] Boolean;]}

    @AdaExcDefn{Tag_Error} : @key[exception];

@key[private]
   ... -- @RI{not specified by the language}
@key[end] Ada.Tags;
@end{Example}
@begin{Reason}
Tag is a nonlimited, definite subtype,
because it needs the equality operators,
so that tag checking makes sense.
Also, equality, assignment, and object declaration
are all useful capabilities for this subtype.

For an object X and a type T,
@lquotes@;X'Tag = T'Tag@rquotes@; is not needed,
because a membership test can be used.
However, comparing the tags of two objects
cannot be done via membership.
This is one reason to allow equality for type Tag.
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02]}
@ChgAdded{Version=[2],Text=[No_Tag is the default initial value of type Tag.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00260-02]}
@ChgAdded{Version=[2],Text=[This is similar to the requirement that all
access values be initialized to @key[null].]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00400-01]}
The function @Chg{Version=[2],New=[Wide_@!Wide_@!Expanded_@!Name],
Old=[Expanded_@!Name]} returns the full expanded name of the
first subtype of the specific type identified by the tag,
in upper case, starting with a root library unit.
The result is implementation defined if the type is declared within
an unnamed @nt{block_statement}.
@begin{Honest}
  This name, as well as each @nt{prefix} of it,
  does not denote a @nt{renaming_declaration}.
@end{Honest}
@ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],
Text=[The result of @Chg{Version=[2],
New=[Tags.Wide_@!Wide_@!Expanded_@!Name],Old=[Tags.Expanded_@!Name]} for types
declared within an unnamed @nt{block_statement}.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00400-01]}
@ChgAdded{Version=[2],Text=[The function Expanded_Name (respectively,
Wide_Expanded_Name) returns the same sequence of graphic characters as that
defined for Wide_Wide_Expanded_Name, if all the graphic characters are defined
in Character (respectively, Wide_Character); otherwise, the sequence of
characters is implementation defined, but no shorter than that returned by
Wide_Wide_Expanded_Name for the same value of the argument.]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@Chg{Version=[2],New=[
The sequence of characters of the value returned by Tags.Expanded_Name
(respectively, Tags.Wide_Expanded_Name)
when some of the graphic characters of Tags.Wide_Wide_Expanded_Name are not
defined in Character (respectively, Wide_Character).],Old=[]}]}

The function External_Tag returns a string to be used in an
external representation for the given tag. The call External_Tag(S'Tag)
is equivalent to the @nt<attribute_reference> S'External_Tag
(see @RefSecNum{Operational and Representation Attributes}).
@begin{Reason}
It might seem redundant to provide both the function External_Tag and
the attribute External_Tag.
The function is needed because the attribute can't be applied to
values of type Tag.
The attribute is needed so that it can be @Chg{Version=[2],
New=[specified],Old=[specifiable]}
via an @nt{attribute_definition_clause}.
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00417-01]}
@ChgAdded{Version=[2],Text=[The string returned by the functions Expanded_Name,
Wide_@!Expanded_@!Name, Wide_Wide_@!Expanded_@!Name, and External_Tag has lower bound
1.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00279-01]}
The function Internal_Tag returns @Chg{Version=[2],New=[a],Old=[the]} tag that
corresponds to the given external tag, or raises Tag_Error if the given string
is not the external tag for any specific
type of the partition.@Chg{Version=[2],New=[ Tag_Error is also raised
if the specific type identified is a library-level type whose tag
has not yet been created (see @RefSecNum{Freezing Rules}).],Old=[]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00279-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[The check for uncreated library-level types
  prevents a reference to the type before execution reaches the freezing point
  of the type. This is important so that T'Class'Input or an instance of
  Tags.Generic_Dispatching_Constructor do not try to create an object of a type
  that hasn't been frozen (which @Chg{Version=[3],New=[might],Old=[may]}
  not have yet elaborated its constraints).
  We don't require this behavior for non-library-level types as the tag can
  be created multiple times and possibly multiple copies can exist at the
  same time, making the check complex.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00344-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0113-1]}
@ChgAdded{Version=[2],Text=[The function Descendant_Tag returns the (internal)
tag for the type that corresponds to the given external tag and is both a
descendant of the type identified by the Ancestor tag and has the same
accessibility level as the identified ancestor. Tag_Error is raised if External
is not the external tag for such a type. Tag_Error is also raised if the
specific type identified is a library-level type whose tag has not yet been
created@Chg{Version=[3],New=[, or if the given external tag identifies more than
one type that has the appropriate Ancestor and accessibility level],Old=[]}.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Descendant_Tag is used by T'Class'Input to
    identify the type identified by an external tag. Because there can be
    multiple elaborations of a given type declaration, Internal_Tag does not
    have enough information to choose a unique such type. Descendant_Tag does
    not return the tag for types declared at deeper accessibility levels than
    the ancestor because there could be ambiguity in the presence of
    recursion or multiple tasks. Descendant_Tag can
    be used in constructing a user-defined replacement for T'Class'Input.]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0113-1]}
  @ChgAdded{Version=[3],Text=[Rules for specifying external tags will usually
  prevent an external tag from identifying more than one type. However, an
  external tag can identify multiple types if a generic body contains a
  derivation of a tagged type declared outside of the generic, and there are
  multiple instances at the same accessibility level as the type. (The Standard
  allows default external tags to not be unique in this case.)]}
  @end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00344-01]}
@ChgAdded{Version=[2],Text=[The function Is_Descendant_At_Same_Level returns
True if the Descendant tag identifies a type that is both a descendant of the
type identified by Ancestor and at the same accessibility level. If not, it
returns False.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Is_Descendant_At_Same_Level (or something similar
    to it) is used by T'Class'Output to determine whether the item being
    written is at the same accessibility level as T. It may be used to
    determine prior to using T'Class'Output whether Tag_Error will be raised,
    and also can be used in constructing a user-defined replacement
    for T'Class'Output.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0115-1]}
@ChgAdded{Version=[3],Text=[For the purposes of the dynamic semantics of
functions Descendant_Tag and Is_Descendant_At_Same_Level, a tagged type T2
is a @i<descendant> of
a type T1 if it is the same as T1, or if its parent type or one of its
progenitor types is a descendant of type T1 by this rule@Redundant[,
even if at the point of the declaration of T2, one of the derivations
in the chain is not visible].@Defn2{Term=[descendant],Sec=[at run-time]}]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[In other contexts, @ldquote@;descendant@rdquote
  is dependent on visibility, and the particular view a derived type has of
  its parent type. See @RefSecNum{Private Operations}.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}@ChgNote{Paragraph number changed}
@ChgAdded{Version=[2],Text=[The function Parent_Tag returns the tag of the
parent type of the type whose tag is T. If the type does not have a parent type
(that is, it was not declared by a derived_type_declaration), then No_Tag is
returned.]}

@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The parent type is always the parent of the full
type; a private extension appears to define a parent type, but it does not
(only the various forms of derivation do that). As this is a run-time
operation, ignoring privateness is OK.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00405-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}@ChgNote{Paragraph number changed}
@ChgAdded{Version=[2],Text=[The function Interface_Ancestor_Tags returns an
array containing the tag of each interface ancestor type of the type whose tag
is T, other than T itself. The lower bound of the returned array is 1, and the
order of the returned tags is unspecified. Each tag appears in the result
exactly once.@Redundant[ If the type whose tag is T has no interface ancestors,
a null array is returned.]]}@PDefn{Unspecified}

@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The result of Interface_Ancestor_Tags includes the
tag of the parent type, if the parent is an interface.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Indirect interface ancestors are included in the
result of Interface_Ancestor_Tags. That's because where an interface appears
in the derivation tree has no effect on the semantics of the type; the only
interesting property is whether the type has an interface as an ancestor.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0173-1]}
@ChgAdded{Version=[3],Text=[The function Is_Abstract returns True if the type
whose tag is T is abstract, and False otherwise.]}

For @PrefixType{every subtype S of a tagged type @i(T)
(specific or class-wide)}, the following attributes are
defined:
@begin(description)
@Attribute{Prefix=<S>, AttrName=<Class>,
  Text=[S'Class
  denotes a subtype of the class-wide type
  (called @i(T)'Class in this International Standard) for the class
  rooted at @i(T) (or if S already denotes a class-wide subtype,
  then S'Class is the same as S).

  @Noprefix@Defn2{Term=[unconstrained], Sec=(subtype)}
  @Defn2{Term=[constrained], Sec=(subtype)}
  S'Class is unconstrained. However,
  if S is constrained, then the values of S'Class are only those
  that when converted to the type @i(T) belong to S.]}
  @begin{Ramification}
    This attribute is defined for both specific
    and class-wide subtypes. The definition is such
    that S'Class'Class is the same as S'Class.

    Note that if S is constrained, S'Class is only partially constrained,
    since there might be additional discriminants
    added in descendants of @i(T) which are not constrained.
  @end{Ramification}
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00326-01]}
    The Class attribute is not defined for untagged subtypes
    (except for incomplete types and private types
    whose full view is tagged
    @em see @Chg{Version=[2],New=[@RefSecNum{The Class Attribute of Untagged Incomplete Types}],
    Old=[@RefSecNum{Incomplete Type Declarations}]} and
    @RefSecNum{Private Operations})
    so as to preclude implicit conversion in the absence of run-time
    type information. If it were defined for untagged subtypes, it would
    correspond to the concept of universal types provided for the predefined
    numeric classes.
  @end{Reason}

@Attribute{Prefix=<S>, AttrName=<Tag>,
  Text=[S'Tag denotes the tag of
  the type @i(T) (or if @i(T) is class-wide, the tag of the root type of the
  corresponding class).
  The value of this attribute is of type Tag.]}
  @begin{Reason}
S'Class'Tag equals S'Tag, to avoid generic
    contract model problems when S'Class is the actual
    type associated with a generic formal derived type.@end{reason}
@end(description)
@EndPrefixType{}

Given @PrefixType{a @nt<prefix> X that is of a class-wide
tagged type @Redundant[(after any implicit dereference)]},
the following attribute is defined:
@begin(description)
@Attribute{Prefix=<X>, AttrName=<Tag>,
  Text=[X'Tag denotes the tag of X.
  The value of this attribute is of type Tag.]}
  @begin(Reason)
    X'Tag is not defined if X is of a specific type.
    This is primarily to avoid confusion that might result
    about whether the Tag attribute should reflect the tag of
    the type of X, or the tag of X. No such
    confusion is possible if X is of a class-wide type.
  @end(Reason)
@end(description)
@EndPrefixType{}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02],ARef=[AI95-00441-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The following language-defined
generic function exists:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0229-1]}
@ChgAdded{Version=[2],Text=[@ChildUnit{Parent=[Ada.Tags],Child=[Generic_@!Dispatching_@!Constructor]}@key{generic}
    @key{type} T (<>) @key{is abstract tagged limited private};
    @key{type} Parameters (<>) @key{is limited private};
    @key{with function} Constructor (Params : @key{not null access} Parameters)
        @key{return} T @key{is abstract};
@key{function} Ada.Tags.Generic_Dispatching_Constructor
   (The_Tag : Tag;
    Params  : @key{not null access} Parameters) @key{return} T'Class@Chg{Version=[3],New=[
   @key{with} Convention => Intrinsic],Old=[]};
@key{pragma} Preelaborate(Generic_Dispatching_Constructor);@Chg{Version=[3],New=[],Old=[
@key{pragma} Convention(Intrinsic, Generic_Dispatching_Constructor);]}]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02]}
@ChgAdded{Version=[2],Text=[Tags.Generic_Dispatching_Constructor provides
a mechanism to create an object of an appropriate type from just a tag value.
The function Constructor is expected to create the object given a reference to
an object of type Parameters.]}

@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This specification is designed to make it easy to
create dispatching constructors for streams; in particular, this can be used to
construct overridings for T'Class'Input.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Note that any tagged type will match T (see
@RefSecNum{Formal Private and Derived Types}).]}
@end{Discussion}

@end{StaticSem}

@begin{RunTime}
@Leading@keepnext@;The tag associated with an object of a tagged type is
determined as follows:
@begin(Itemize)
@PDefn2{Term=[tag of an object], Sec=(stand-alone object, component, or @nt<aggregate>)}
The tag of a stand-alone object, a component, or an
@nt<aggregate> of a specific tagged type @i(T)
identifies @i(T).
@begin{Discussion}
  The tag of a formal parameter of type @i(T)
  is not necessarily the tag of @i(T), if, for example, the actual was a
  type conversion.
@end{Discussion}

@PDefn2{Term=[tag of an object], Sec=(object created by an @nt<allocator>)}
The tag of an object created by an allocator for an
access type with a specific designated tagged type @i(T),
identifies @i(T).
@begin{Discussion}
The tag of an object designated by a
  value of such an access type might not be @i(T), if, for
  example, the access value is the result of a type conversion.@end{discussion}

@PDefn2{Term=[tag of an object], Sec=(class-wide object)}
The tag of an object of a class-wide tagged type
is that of its initialization expression.
@begin{Ramification}
  The tag of an object (even a class-wide one)
  cannot be changed after it is initialized, since a @lquotes@;class-wide@rquotes@;
  @nt{assignment_statement} raises Constraint_Error if the tags don't
  match, and a @lquotes@;specific@rquotes@; @nt{assignment_statement} does not affect
  the tag.
@end{Ramification}

@PDefn2{Term=[tag of an object], Sec=(returned by a function)}
The tag of the result returned by a function whose
result type is a specific tagged type @i(T) identifies @i(T).
@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
  @Chg{Version=[2],New=[For a limited tagged type, the return object is
  @lquotes@;built in place@rquotes in the ultimate result object with the
  appropriate tag.], Old=[This requires
  a run-time check for limited tagged types, since they are
  returned "by-reference."]}
  For a nonlimited
  type, a new anonymous object with the appropriate tag
  is created as part of the function
  return@Chg{Version=[2],New=[],Old=[, and then assigned the value of the
  return expression]}.
  See @RefSec{Return Statements}.
@end{ImplNote}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
@PDefn2{Term=[tag of an object], Sec=(returned by a function)}
The tag of the result returned by a
function with a class-wide result
type is that of the return @Chg{Version=[2],New=[object],Old=[expression]}.
@end(Itemize)

@PDefn2{Term=[tag of an object], Sec=(preserved by type conversion and parameter passing)}
The tag is preserved by type conversion and by parameter passing.
The tag of a value is the tag of the associated object
(see @RefSecNum{Formal Parameter Modes}).

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02],ARef=[AI95-00344-01],ARef=[AI95-00405-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0092-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[Tag_Error is raised by a call of Descendant_Tag,
Expanded_Name, External_Tag, @Chg{Version=[3],New=[Interface_@!Ancestor_@!Tags],
Old=[Interface_Ancestor_Tag]},@Chg{Version=[3],New=[
Is_Abstract,],Old=[]}
Is_Descendant_@!At_Same_Level,
@Chg{Version=[3],New=[],Old=[or ]}Parent_Tag@Chg{Version=[3],New=[,
Wide_Expanded_Name, or Wide_Wide_Expanded_Name],Old=[]} if any
tag passed is No_Tag.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02]}
@ChgAdded{Version=[2],Text=[An instance of Tags.Generic_Dispatching_Constructor
raises Tag_Error if The_Tag does not represent a concrete descendant of T or
if the innermost master (see @RefSecNum{Completion and Finalization}) of this
descendant is not also a master of the instance.
Otherwise, it dispatches to the primitive function denoted by the formal
Constructor for the type identified by The_Tag, passing Params, and
returns the result. Any exception raised by the function is propagated.]}

@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The tag check checks both that The_Tag is in
T'Class, and that it is not abstract. These checks are similar to the ones
required by streams for T'Class'Input
(see @RefSecNum{Stream-Oriented Attributes}). In addition, there is a
check that the tag identifies a type declared on the current dynamic
call chain, and not a more nested type or a type declared by another
task. This check is not necessary for streams, because the stream attributes
are declared at the same dynamic level as the type used.]}
@end{Ramification}

@end{RunTime}

@begin{Erron}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=(erroneous execution),Sec=(cause)}
If an internal tag provided to an instance of
Tags.Generic_Dispatching_Constructor or to any subprogram declared in
package Tags identifies either a type that is not
library-level and whose tag has not been created
(see @RefSecNum{Freezing Rules}), or a type that does not exist in the
partition at the time of the call, then execution is erroneous.]}

@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[One reason that a type might not exist in
the partition is that the tag refers to a type whose declaration was
elaborated as part of an execution of a @nt{subprogram_body} which has been
left (see @RefSecNum{Completion and Finalization}).]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[We exclude tags of library-level types from
the current execution of the partition, because misuse of such tags
should always be detected. T'Tag freezes the type (and thus creates the tag),
and Internal_Tag and Descendant_Tag
cannot return the tag of a library-level type that has not been created.
All ancestors of a tagged type must be frozen no later than the (full)
declaration of a type that uses them, so Parent_Tag and Interface_Ancestor_Tags
cannot return a tag that has not been created.
Finally, library-level types never cease to exist while the partition is
executing. Thus, if the tag comes from
a library-level type, there cannot be erroneous execution (the use of
Descendant_Tag rather than Internal_Tag can help ensure that the tag is
of a library-level type). This is also similar to the rules for T'Class'Input
(see @RefSecNum{Stream-Oriented Attributes}).]}
@end{Ramification}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00344-01]}
@ChgAdded{Version=[2],Text=[Ada 95 allowed Tag_Error in this case, or expected
the functions to work. This worked because most implementations used tags
constructed at link-time, and each elaboration of the same @nt{type_declaration}
produced the same tag. However, Ada 2005 requires at least part of the tags
to be dynamically constructed for a type derived from a type at a shallower
level. For dynamically constructed tags, detecting the error can be expensive
and unreliable. To see
this, consider a program containing two tasks. Task A creates a nested tagged
type, passes the tag to task B (which saves it), and then terminates. The
nested tag (if dynamic) probably will need to refer in some way to the stack
frame for task A.
If task B later tries to use the tag created by task A, the tag's reference to
the stack frame of A probably is a dangling pointer. Avoiding this would
require some sort of protected tag manager, which would be a bottleneck in a
program's performance. Moreover, we'd still have a race condition; if task A
terminated after the tag check, but before the tag was used, we'd still have
a problem. That means that all of these operations would have to be serialized.
That could be a significant performance drain, whether or not nested tagged
types are ever used. Therefore, we allow execution to become erroneous
as we do for other dangling pointers. If the implementation can detect the
error, we recommend that Tag_Error be raised.]}
@end{Discussion}
@end{Erron}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00260-02],ARef=[AI95-00279-01]}
The implementation of @Chg{Version=[2],New=[Internal_Tag and Descendant_Tag],
Old=[the functions in Ada.Tags]}
may raise Tag_Error if no specific type corresponding to the
@Chg{Version=[2],New=[string External],Old=[tag]} passed
as a parameter exists in the partition at the time the function is
called@Chg{Version=[2],New=[, or if there is no such type whose innermost
master is a master of the point of the function call],Old=[]}.
@begin{Reason}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00260-02],ARef=[AI95-00279-01],ARef=[AI95-00344-01]}
@Chg{Version=[2],New=[Locking would be required to ensure that the mapping of
strings to tags never returned tags of types which no longer exist, because
types can cease to exist (because they belong to another task, as described
above) during the execution of these operations. Moreover, even if these
functions did use locking, that would not prevent the type from ceasing to
exist at the instant that the function returned. Thus, we do not require the
overhead of locking;],Old=[In most implementations,
repeated elaborations of the same
@nt{type_declaration} will all produce the same tag.
In such an implementation, Tag_Error will be raised in cases where the
internal or external tag was passed from a different partition.
However, some implementations might create a new tag value at run time
for each elaboration of a @nt{type_declaration}.
In that case, Tag_Error could also be raised if the created type
no longer exists because the subprogram
containing it has returned, for example.
We don't require the latter behavior;]} hence the word
@lquotes@;may@rquotes@; in this rule.
@end{Reason}
@end{ImplPerm}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0113-1]}
@ChgAdded{Version=[2],Text=[Internal_Tag should return the tag of
a type@Chg{Version=[3],New=[, if one exists,],Old=[]} whose
innermost master is @Chg{Version=[3],New=[a],Old=[the]} master of the point of the function call.]}
@ChgImplAdvice{Version=[3],Kind=[Revised],InitialVersion=[2],
Text=[@Chg{Version=[2],
New=[Tags.Internal_Tag should return the tag of
a type@Chg{Version=[3],New=[, if one exists,],Old=[]} whose innermost master
is @Chg{Version=[3],New=[a],Old=[the]} master of the point of the function call.],Old=[]}.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00260-02],ARef=[AI95-00344-01]}
  @ChgAdded{Version=[2],Text=[It's not helpful if Internal_Tag returns the tag of
  some type in another task when one is available in the task that made the call.
  We don't require this behavior (because it requires the same implementation
  techniques we decided not to insist on previously), but encourage it.]}
@end{Reason}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0113-1]}
  @ChgAdded{Version=[3],Text=[There is no Advice for the result of Internal_Tag
  if no such type exists. In most cases, the @ImplPermName can be
  used to raise Tag_Error, but some other tag can be returned as well.]}
@end{Discussion}
@end{ImplAdvice}

@begin{Notes}
A type declared with the reserved word @key[tagged]
should normally be declared in a @nt{package_specification},
so that new primitive subprograms can be declared for it.

Once an object has been created,
its tag never changes.

Class-wide types are defined to have unknown discriminants
(see @RefSecNum(Discriminants)). This means that objects of a class-wide
type have to be explicitly initialized (whether created by
an @nt<object_declaration> or an @nt<allocator>),
and that @nt<aggregate>s have to be explicitly qualified with a specific
type when their expected type is class-wide.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00260-02],ARef=[AI95-00326-01]}
@Chg{Version=[2],New=[The capability provided by
Tags.Generic_Dispatching_Constructor is sometimes known as a
@i<factory>.@Defn{factory}@Defn{class factory}],Old=[If S denotes an
untagged private type whose full type is tagged,
then S'Class is also allowed before the full type definition,
but only in the private part of the package in which the type is
declared
(see @RefSecNum(Private Operations)).
Similarly, the Class attribute is defined
for incomplete types whose full type is tagged, but only within
the library unit in which the incomplete type is declared
(see @RefSecNum(Incomplete Type Declarations)).]}
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of tagged record types:)
@begin(Example)
@key(type) Point @key(is tagged)
  @key(record)
    X, Y : Real := 0.0;
  @key(end record);

@key(type) Expression @key(is tagged null record);
  --@RI[ Components will be added by each extension]
@end(Example)
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
Tagged types are a new concept.
@end{Extend83}

@begin{Inconsistent95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00279-01]}
  @ChgAdded{Version=[2],Text=[@Defn{inconsistencies with Ada 95}
  @b[Amendment Correction:] Added wording specifying that Internal_Tag
  must raise Tag_Error if the tag of a library-level type has not yet been
  created. Ada 95 gave an Implementation Permission to do this; we require
  it to avoid erroneous execution when streaming in an object of a
  library-level type that has not yet been elaborated. This is technically
  inconsistent; a program that used Internal_Tag outside of streaming and
  used a compiler that didn't take advantage of the Implementation Permission
  would not have raised Tag_Error, and may have returned a useful tag. (If
  the tag was used in streaming, the program would have been erroneous.)
  Since such a program would not have been portable to a compiler that did
  take advantage of the Implementation Permission, this is not a significant
  inconsistency.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00417-01]}
  @ChgAdded{Version=[2],Text=<We now define the lower bound of the string
  returned from [[Wide_]Wide_]Expanded_Name and External_Name. This makes
  working with the returned string easier, and is consistent with many other
  string-returning functions in Ada. This is technically an inconsistency; if a
  program depended on some other lower bound for the string returned from one
  of these functions, it could fail when compiled with Ada 2005. Such code is
  not portable even between Ada 95 implementations, so it should be very
  rare.>}
@end{Inconsistent95}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00260-02],ARef=[AI95-00344-01],ARef=[AI95-00400-01],ARef=[AI95-00405-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  Constant No_Tag, and functions Parent_Tag, Interface_Ancestor_Tags,
  Descendant_Tag, Is_Descendant_At_Same_Level, Wide_Expanded_Name,
  and Wide_Wide_Expanded_Name are @Chg{Version=[3],New=[],Old=[newly ]}added
  to Ada.Tags.
  If Ada.Tags is referenced in a @nt{use_clause}, and an entity @i<E> with the
  same @nt{defining_identifier} as a new entity in Ada.Tags is defined in a
  package that is also referenced in a @nt{use_clause}, the entity @i<E> may no
  longer be use-visible, resulting in errors. This should be rare and is easily
  fixed if it does occur.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00362-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Ada.Tags is now defined to be preelaborated.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00260-02]}
  @ChgAdded{Version=[2],Text=[Generic function
    Tags.Generic_Dispatching_Constructor is new.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00318-02]}
  @ChgAdded{Version=[2],Text=[We talk about return objects rather than
  return expressions, as functions can return using an
  @nt{extended_return_statement}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00344-01]}
  @ChgAdded{Version=[2],Text=[Added wording to define that tags for all
  descendants of a tagged type must be distinct. This is needed to ensure
  that more nested type extensions will work properly. The wording does not
  require implementation changes for types that were allowed in Ada 95.]}
@end{DiffWord95}

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0113-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}
  @b[Correction:] Added wording specifying that Dependent_Tag
  must raise Tag_Error if there is more than one type which matches the
  requirements. If an implementation had returned a random tag of the matching
  types, a program may have worked properly. However, such a program would
  not be portable (another implementation may return a different tag) and the
  conditions that would cause the problem are unlikely (most likely, a tagged
  type extension declared in a generic body with multiple instances in the
  same scope).]}
@end{Inconsistent2005}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0173-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
  Function Is_Abstract is added to Ada.Tags.
  If Ada.Tags is referenced in a @nt{use_clause}, and an entity @i<E> with the
  @nt{defining_identifier} Is_Abstract is defined in a
  package that is also referenced in a @nt{use_clause}, the entity @i<E> may no
  longer be use-visible, resulting in errors. This should be rare and is easily
  fixed if it does occur.]}
@end{Incompatible2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0115-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> We explicitly define the meaning
  of "descendant" at runtime, so that it does not depend on visibility
  as does the usual meaning.]}
@end{DiffWord2005}


@LabeledSubClause{Type Extensions}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@Redundant[@Defn{type extension}
@Defn2{Term=[extension], Sec=(of a type)}
@Defn{record extension}
@Defn2{Term=[extension], Sec=(of a record type)}
@Defn{private extension}
@Defn2{Term=[extension], Sec=(of a private type)}
Every type extension is a tagged type, and
is @Chg{Version=[2],New=[],Old=[either ]}a @i(record extension) or a
@i(private extension) of
some other tagged type@Chg{Version=[2],New=[, or a noninterface
synchronized tagged type],Old=[]}.]
@end{Intro}

@begin{MetaRules}

We want to make sure that we can extend a generic formal
tagged type, without knowing its discriminants.

We don't want to allow components in an extension aggregate
to depend on discriminants inherited from the parent value,
since such dependence requires staticness in aggregates, at least for
variants.
@end{MetaRules}

@begin{Syntax}
@Syn{lhs=<record_extension_part>,rhs="@key(with) @Syn2{record_definition}"}
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00344-01],ARef=[AI95-00345-01],ARef=[AI95-00419-01]}
The parent type of a record extension shall not be
a class-wide type@Chg{Version=[2],New=[ nor shall it be a synchronized
tagged type (see @RefSecNum{Interface Types})],Old=[]}.
If the parent type@Chg{Version=[2],New=[ or any progenitor],Old=[]} is
nonlimited, then each of the
components of the @nt{record_extension_part} shall be
nonlimited.@Chg{Version=[2],New=[],Old=[
@PDefn2{Term=[accessibility rule],Sec=(record extension)}
The accessibility level
(see @RefSecNum(Operations of Access Types))
of a record extension shall not be statically deeper than that of its
parent type.]}
@PDefn{generic contract issue}
In addition to the places where @LegalityTitle normally apply
(see @RefSecNum{Generic Instantiation}),
these rules apply also in the private part of an
instance of a generic unit.
@begin{Reason}
If the parent is a limited formal type,
then the actual might be nonlimited.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00344-01]}
@Chg{Version=[2],New=[Ada 95 required the record extensions to be the
same level as the parent type. Now we use accessibility checks on class-wide
@nt{allocator}s and return statements to prevent objects from living
longer than their type.],
Old=[A similar accessibility rule is not needed for
private extensions, because in a package, the rule will apply to the
@nt{full_type_declaration},
and for a generic formal private extension,
the actual is all that matters.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[Synchronized tagged types cannot be extended. We
have this limitation so that all of the data of a task or protected type is
defined within the type. Data defined outside of the type wouldn't be
subject to the mutual exclusion properties of a protected type, and couldn't
be used by a task, and thus doesn't seem to be worth the potential impact
on implementations.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00344-01]}
@Chg{Version=[2],New=[Within the body of a generic unit, or the body of any of
its descendant library units, a tagged type],Old=[A type extension]}
shall not be declared
@Chg{Version=[2],New=[as a descendant of a formal type
declared within the formal part of the generic unit],
Old=[in a generic body if the parent type is declared outside that body]}.

@begin{Reason}
This paragraph ensures that a dispatching call will never
attempt to execute an inaccessible subprogram body.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00344-01]}
@ChgAdded{Version=[2],Text=[The convoluted wording (@ldquote@;formal type declared
within the formal part@rdquote@;) is necessary to include tagged types that
are formal parameters of formal packages of the generic unit, as well as
formal tagged and tagged formal derived types of the generic unit.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00344-01]}
@ChgNote{This rule is only about generic bodies (and always was only
about generic bodies. So we drop the extra text.}
@Chg{Version=[2],New=[This rule],Old=[The part about generic bodies]} is
necessary in order to preserve the contract model.

@Leading@;@ChgRef{Version=[2],Kind=[Revised],ARef=[AI05-0005-1]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00344-01]}
@Chg{Version=[2],New=[If an ancestor],Old=[Since a generic unit can be instantiated at a
deeper accessibility level than the generic unit, it is necessary to prevent
type extensions whose parent is declared outside the generic unit.
The same is true if the parent]} is a formal of the generic unit
@Chg{Version=[2],New=[, we have a problem],Old=[.
If the parent is declared in the @nt{generic_declaration}
(but is not a formal), we don't run afoul of the accessibility rules,
because we know that the instance declaration and body will be at the
same accessibility level.
However, we still have a problem in that case,]} because
it might have an unknown number of@Chg{Version=[3],New=[],Old=[ abstract]}
subprograms@Chg{Version=[3],New=[ that require overriding],Old=[]},
as in the following example:
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised]}
@key[package] P @key[is]
    @key[type] T @key[is] @key[tagged] @key[null] @key[record];
    @key[function] F @key[return] T; --@RI{ Inherited versions will @Chg{Version=[2],New=[require overriding],Old=[be abstract]}.}
@key[end] P;

@key[generic]
    @key[type] TT @key[is] @key[tagged] @key[private];
@key[package] Gp @key[is]
    @key[type] NT @key[is] @key[abstract new] TT @key[with] @key[null] @key[record];
    @key[procedure] Q(X : @key[in] NT) @key[is abstract];
@key[end] Gp;

@ChgRef{Version=[2],Kind=[Revised]}
@key[package] @key[body] Gp @key[is]
    @key[type] NT2 @key[is] @key[new] NT @key[with] @key[null] @key[record]; --@RI{ Illegal!}
    @key[procedure] Q(X : @key[in] NT2) @key[is] @key[begin] @key[null]; @key[end] Q;
    --@RI{ Is this legal or not? Can't decide because}
    --@RI{ we don't know whether TT had any functions that @Chg{Version=[2],New=[require],Old=[go abstract]}}
    --@RI{ @Chg{Version=[2],New=[overriding ],Old=[]}on extension.}
@key[end] Gp;

@key[package] I @key[is] @key[new] Gp(TT => P.T);
@end{Example}

@ChgRef{Version=[2],Kind=[Revised]}
I.NT is an abstract type with two abstract subprograms:
F (inherited as abstract) and Q (explicitly declared as abstract).
But the generic body doesn't know about F,
so we don't know that it needs to be overridden to make a nonabstract
extension of NT.@Chg{Version=[2],New=[],Old=[
Furthermore, a formal tagged limited private type can be extended with
limited components,
but the actual might not be limited,
which would allow assignment of limited types,
which is bad. ]}Hence, we have to disallow this
case@Chg{Version=[2],New=[],Old=[ as well]}.

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[Similarly, since the actual type for a
formal tagged limited private type can be a nonlimited type, we would have
a problem if a type extension of a limited private formal type could be
declared in a generic body. Such an
extension could have a task component, for example, and an object of that
type could be passed to a dispatching operation of a nonlimited ancestor
type. That operation could try to copy the object with the task component.
That would be bad. So we disallow this as well.]}

If TT were declared as abstract, then we could have the same
problem with abstract procedures.

We considered disallowing all tagged types in a generic body,
for simplicity.
We decided not to go that far,
in order to avoid unnecessary restrictions.

@PDefn2{Term=[accessibility rule],Sec=(not part of generic contract)}
We also considered trying make the accessibility level part of the
contract; i.e. invent some way of saying (in the
@nt{generic_declaration}) @lquotes@;all instances of this generic unit will
have the same accessibility level as the
@nt{generic_declaration}.@rquotes@;
Unfortunately, that doesn't solve the part of the problem having to do
with abstract types.

@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[Children of
generic units obviate the need for extension in the body somewhat.]}
@end{Reason}

@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00344]}
@ChgAdded{Version=[2],Text=[This rule applies to types with ancestors (directly
or indirectly) of formal interface types
(see @RefSecNum{Formal Interface Types}), formal tagged private types
(see @RefSecNum{Formal Private and Derived Types}), and
formal derived private types whose ancestor type is tagged
(see @RefSecNum{Formal Private and Derived Types}).]}
@end{Ramification}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00391-01]}
@ChgAdded{Version=[2],Text=[@defn{null extension}
A record extension is a @i{null extension} if its declaration
has no @nt{known_discriminant_part} and its @nt{record_extension_part}
includes no @nt{component_declaration}s.]}
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(record_extension_part)}
The elaboration of a @nt{record_extension_part} consists
of the elaboration of the @nt{record_definition}.
@end{RunTime}

@begin{Notes}
The term @lquotes@;type extension@rquotes@; refers to a type as a whole.
The term @lquotes@;extension part@rquotes@; refers to the piece
of text that defines the additional components (if any) the
type extension has relative to its specified ancestor type.
@begin(Discussion)
  We considered other terminology, such as @lquotes@;extended type.@rquotes@;
  However, the terms @lquotes@;private extended type@rquotes@; and @lquotes@;record extended type@rquotes@;
  did not convey the proper meaning. Hence, we have chosen
  to uniformly use the term @lquotes@;extension@rquotes@; as the type resulting
  from extending a type, with @lquotes@;private extension@rquotes@; being one
  produced by privately extending the type, and @lquotes@;record extension@rquotes@;
  being one produced by extending the type with an additional record-like set
  of components.
  Note also that the term @lquotes@;type extension@rquotes@; refers to the result
  of extending a type in the language Oberon as well (though there
  the term @lquotes@;extended type@rquotes@; is also used, interchangeably, perhaps because
  Oberon doesn't have the concept of a @lquotes@;private extension@rquotes@;).
@end(Discussion)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00344-01]}
@Chg{Version=[2],New=[],Old=[The accessibility rules imply that a tagged type
declared in a library @nt{package_specification} can be extended only
at library level or as a generic formal. ]}When
@Chg{Version=[2],New=[an],Old=[the]} extension is declared immediately within
a @Chg{Version=[2],New=[body],Old=[@nt{package_body}]},
primitive subprograms are inherited and are overridable,
but new primitive subprograms cannot be added.

A @nt<name> that denotes a component (including a discriminant) of
the parent type is not allowed within the
@nt{record_extension_part}.
Similarly, a @nt<name> that denotes a component defined within the
@nt{record_extension_part} is not allowed within
the @nt{record_extension_part}.
It is permissible to use a @nt<name>
that denotes a discriminant of the record extension, providing there is
a new @nt{known_discriminant_part} in the enclosing type declaration.
(The full rule is given in @RefSecNum(Record Types).)
@begin(Reason)
  The restriction against depending on
  discriminants of the parent is to simplify the definition of extension
  aggregates. The restriction against using parent components in other
  ways is methodological; it presumably simplifies implementation as
  well.
@end(Reason)

Each visible component of a record extension has to have a
unique name, whether the component is (visibly) inherited
from the parent type or declared in the
@nt<record_extension_part> (see @RefSecNum{Visibility}).
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Examples of record extensions (of types defined above in @RefSecNum(Tagged Types and Type Extensions)):}
@begin(Example)
@key(type) Painted_Point @key(is new) Point @key(with)
  @key(record)
    Paint : Color := White;
  @key(end record);
    --@RI[ Components X and Y are inherited]

Origin : @key(constant) Painted_Point := (X | Y => 0.0, Paint => Black);

@key(type) Literal @key(is new) Expression @key(with)
  @key(record)                 --@RI[ a leaf in an Expression tree]
    Value : Real;
  @key(end record);

@key(type) Expr_Ptr @key(is access all) Expression'Class;
                               --@RI[ see @RefSecNum(Access Types)]

@key(type) Binary_Operation @key(is new) Expression @key(with)
  @key(record)                 --@RI[ an internal node in an Expression tree]
    Left, Right : Expr_Ptr;
  @key(end record);

@key(type) Addition @key(is new) Binary_Operation @key(with null record);
@key(type) Subtraction @key(is new) Binary_Operation @key(with null record);
  --@RI[ No additional components needed for these extensions]

Tree : Expr_Ptr :=         --@RI[ A tree representation of @lquotes@;5.0 + (13.0@en@;7.0)@rquotes@;]
   @key(new) Addition'(
      Left  => @key(new) Literal'(Value => 5.0),
      Right => @key(new) Subtraction'(
         Left  => @key(new) Literal'(Value => 13.0),
         Right => @key(new) Literal'(Value => 7.0)));
@end(Example)

@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
Type extension is a new concept.
@end{Extend83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00344-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Type extensions now can be
  declared in more nested scopes than their parent types. Additional
  accessibility checks on @nt{allocator}s and return statements prevent
  objects from outliving their type.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01]}
  @ChgAdded{Version=[2],Text=[Added wording to prevent extending synchronized
  tagged types.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00391-01]}
  @ChgAdded{Version=[2],Text=[Defined null extension for use elsewhere.]}
@end{DiffWord95}


@LabeledSubClause{Dispatching Operations of Tagged Types}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00260-02],ARef=[AI95-00335-01]}
@RootDefn{dispatching operation}
@Defn2{Term=[dispatching call], Sec=(on a dispatching operation)}
@Defn2{Term=[nondispatching call], Sec=(on a dispatching operation)}
@Defn{statically determined tag}
@Defn{dynamically determined tag}
@Defn{polymorphism}
@Defn{run-time polymorphism}
@Defn2{Term=[controlling tag], Sec=(for a call on a dispatching operation)}
The primitive subprograms of a tagged type@Chg{Version=[2],New=[, the
subprograms declared by @nt{formal_@!abstract_@!subprogram_@!declaration}s,
and the stream attributes of a specific tagged type that are available (see
@RefSecNum{Stream-Oriented Attributes}) at the end of the declaration list
where the type is declared],Old=[]}
are called @i(dispatching operations).
@Redundant[A dispatching operation can be called using a statically
determined @i{controlling} tag, in which case the body to be
executed is determined at compile time.
Alternatively, the controlling tag can be dynamically determined,
in which case the call @i{dispatches} to a
body that is determined at run time;]
such a call is termed a @i{dispatching call}.
@Redundant[As explained below, the properties of the operands
and the context of a particular call on a dispatching operation
determine how the controlling tag is determined,
and hence whether or not the call is a dispatching call.
Run-time polymorphism is achieved when a dispatching operation is called
by a dispatching call.]
@IndexSee{Term=[object-oriented programming (OOP)],See=[dispatching operations of tagged types]}
@IndexSee{Term=[OOP (object-oriented programming)],See=[dispatching operations of tagged types]}
@IndexSee{Term=[message],See=[dispatching call]}
@IndexSee{Term=[method],See=[dispatching subprogram]}
@IndexSee{Term=[virtual function],See=[dispatching subprogram]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00335-01]}
@ChgAdded{Version=[2],Text=[For the stream attributes of a type
declared immediately within a @nt{package_specification} that has a
partial view, the declaration list to consider is the visible part of the
package. Stream attributes that are not available in the same declaration
list are not dispatching as there is no guarantee that descendants of the
type have available attributes (there is such a guarantee for visibly
available attributes). If we allowed dispatching for any available
attribute, then for attributes defined in the private part we could end up
executing a nonexistent body.]}
@end{Reason}

@end{Intro}

@begin{MetaRules}
The controlling tag determination rules are analogous to the
overload resolution rules, except they deal with run-time
type identification (tags) rather than compile-time type
resolution. As with overload resolution, controlling tag determination
may depend on operands or result context.
@end{MetaRules}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00260-02],ARef=[AI95-00416-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0076-1]}
@Defn{call on a dispatching operation}
@Defn{dispatching operation}
A @i{call on a dispatching operation} is a call whose @nt<name> or
@nt<prefix> denotes the declaration of@Chg{Version=[2],New=[],
Old=[ a primitive subprogram of a tagged type, that is,]} a dispatching
operation.
@Defn{controlling operand}
A @i{controlling operand} in a call on a dispatching operation of a tagged
type @i(T) is one whose corresponding formal parameter is of type @i(T)
or is of an anonymous access type with designated type @i(T);
@Defn{controlling formal parameter}
the corresponding formal parameter is called a
@i(controlling formal parameter).
If the controlling formal parameter is an access parameter, the
controlling operand is the object designated by the actual parameter,
rather than the actual parameter itself.
@Defn{controlling result}@Chg{Version=[3],New=[@Defn2{Term=[function],Sec=[with a controlling result]}],Old=[]}
If the call is to a (primitive) function with result type
@i(T)@Chg{Version=[3],New=[ (a @i{function with a controlling result})],Old=[]},
then the call has a @i(controlling result) @em
the context of the call can control the dispatching.@Chg{Version=[2],
New=[ Similarly, if the call is to a function with
@Chg{Version=[3],New=[an ],Old=[]}access result type designating
@i(T)@Chg{Version=[3],New=[ (a @i{function with a controlling access result})],Old=[]},
then the call has a @i(controlling access result), and
the context can similarly control dispatching.],Old=[]}@Chg{Version=[3],New=[@Defn{controlling access result}@Defn2{Term=[function],Sec=[with a controlling access result]}],Old=[]}
@begin{Ramification}
  This definition implies that a call through the dereference of an
  access-to-subprogram value is never considered a call on
  a dispatching operation.
  Note also that if the @nt{prefix} denotes a @nt{renaming_declaration},
  the place where the renaming occurs determines whether it is
  primitive; the thing being renamed is irrelevant.
@end{Ramification}

@Leading@;A @nt<name> or expression of a tagged type
is either @i(statically) tagged,
@i(dynamically) tagged, or @i(tag indeterminate), according
to whether, when used as a controlling operand, the tag
that controls dispatching is determined statically by the operand's
(specific) type,
dynamically by its tag at run time,
or from context.
A @nt<qualified_expression> or parenthesized expression is
statically, dynamically, or indeterminately tagged according
to its operand. For other kinds of @nt<name>s and expressions, this
is determined as follows:
@begin(Itemize)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
  @Defn{statically tagged}
  The @nt<name> or expression is @i(statically
  tagged) if it is of a specific tagged type and,
  if it is a call with a controlling result@Chg{Version=[2],New=[ or
  controlling access result],Old=[]}, it has at least
  one statically tagged controlling operand;
  @begin{Discussion}
    It is illegal to have both statically tagged and
    dynamically tagged controlling operands in the same call -- see below.
  @end{discussion}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
  @Defn{dynamically tagged}
  The @nt<name> or expression is @i(dynamically tagged)
  if it is of a class-wide type, or it is a call with
  a controlling result@Chg{Version=[2],New=[ or
  controlling access result],Old=[]} and at least one dynamically
  tagged controlling operand;

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
  @Defn{tag indeterminate}
  The @nt<name> or expression is @i(tag indeterminate)
  if it is a call with a controlling result@Chg{Version=[2],New=[ or
  controlling access result],Old=[]}, all of whose
  controlling operands (if any) are tag indeterminate.
@end(itemize)

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0010],ARef=[AI95-00127-01]}
@Redundant[A @nt<type_conversion> is statically or dynamically
tagged according to whether the type determined by the @nt<subtype_mark>
is specific or class-wide, respectively.]
@Chg{New=[For an object that is designated by an expression whose expected type
is an anonymous access-to-specific tagged type, the object is dynamically
tagged if the expression, ignoring enclosing parentheses, is of the form
X'Access, where X is of a class-wide type, or is of the form
@key(new) T'(...), where T denotes a class-wide subtype. Otherwise, the object],
Old=[For a controlling operand that is designated by an actual parameter,
the controlling operand]} is statically or dynamically tagged according to
whether the designated type @Chg{New=[of the type of the expression],
Old=[of the actual parameter]} is specific or class-wide, respectively.
@begin{Ramification}
  A @nt<type_conversion> is never tag indeterminate, even if its
  operand is. A designated object is never tag indeterminate.

  @ChgRef{Version=[1],Kind=[Added],Ref=[8652/0010],ARef=[AI95-00127-01]}
  @ChgAdded{Version=[1],Text=[Allocators and access attributes of class-wide types can be used as
  the controlling parameters of dispatching calls.]}
@end{Ramification}
@end{StaticSem}

@begin{Legality}
A call on a dispatching operation shall not
have both dynamically tagged and statically tagged controlling operands.
@begin{Reason}
  This restriction is intended to minimize
  confusion between whether the dynamically tagged operands
  are implicitly converted to, or tag checked against the
  specific type of the statically tagged operand(s).
@end{Reason}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0010],ARef=[AI95-00127-01]}
If the expected type for an expression or @nt<name>
is some specific tagged type, then the expression or @nt<name>
shall not be dynamically tagged unless it is a controlling operand in a call
on a dispatching operation.
Similarly, if the expected type for an expression
is an anonymous access-to-specific
tagged type, then the @Chg{New=[object designated by the expression
shall not be dynamically tagged unless it is],Old=[expression shall not be
of an access-to-class-wide type unless it designates]} a controlling
operand in a call on a dispatching operation.
@begin(Reason)
  This prevents implicit "truncation"
  of a dynamically-tagged value to the specific type of the
  target object/formal. An explicit conversion is required to request
  this truncation.
@end(Reason)
@begin(Ramification)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00252-01]}@ChgNote{Add info about prefix calls}
  This rule applies to all expressions
  or @nt<name>s with a specific expected type, not just those that
  are actual parameters to a dispatching call. This rule does not apply to
  a membership test whose @nt<expression> is class-wide,
  since any type that covers the tested type is explicitly allowed.
  See @RefSecNum(Relational Operators and Membership Tests).@Chg{Version=[2],
  New=[ This rule also doesn't apply to a @nt{selected_component} whose
  @nt{selector_name} is a subprogram, since the rules explicitly say that
  the prefix may be class-wide (see @RefSecNum{Selected Components}).],Old=[]}
@end(Ramification)

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0011],ARef=[AI95-00117-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00430-01]}
In the declaration of a dispatching operation of a tagged type,
everywhere a subtype of the tagged type appears as a
subtype of the profile (see @RefSecNum(Subprogram Declarations)),
it shall statically match the first subtype of the tagged type.
@PDefn2{Term=[statically matching],Sec=(required)}
If the dispatching operation overrides an inherited subprogram,
it shall be subtype conformant with the inherited subprogram.
@Defn2{Term=[subtype conformance],Sec=(required)}
@Chg{New=[The convention of an inherited @Chg{Version=[2],New=[],
Old=[or overriding ]}dispatching operation is
the convention of the corresponding primitive operation of the parent
@Chg{Version=[2],New=[or progenitor ],Old=[]}type. @Chg{Version=[2],New=[The
default convention of a dispatching operation that overrides an inherited
primitive operation is the convention of the inherited operation; if the
operation overrides multiple inherited operations, then they shall all
have the same convention. ],Old=[]}An
explicitly declared],Old=[A]} dispatching operation shall not be of convention
Intrinsic.@Chg{New=[],Old=[ If a dispatching operation overrides the predefined
equals operator, then it shall be of convention Ada @Redundant[(either
explicitly or by default @em see @RefSecNum{Conformance Rules})].]}
@begin{Reason}
  These rules ensure that constraint checks can be performed by the
  caller in a dispatching call, and parameter passing conventions
  match up properly. A special rule on aggregates
  prevents values of a tagged type from being created that
  are outside of its first subtype.
@end{reason}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
The @nt<default_expression> for a controlling formal parameter
of a dispatching operation shall be tag indeter@!minate.@Chg{Version=[2],
New=[],Old=[ A controlling formal parameter that is an access parameter
shall not have a @nt<default_expression>.]}
@begin(Reason)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
  @Chg{Version=[2],New=[This rule],Old=[The first part]} ensures
  that the @nt{default_expression} always produces the "correct"
  tag when called with or without dispatching,
  or when inherited by a descendant. If
  it were statically tagged, the default would be useless for
  a dispatching call; if it were dynamically tagged, the default
  would be useless for a nondispatching call.

  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00416-01]}
  @ChgDeleted{Version=[2],Text=[The second part is consistent with the
  first part, since designated objects are never tag-indeterminate.]}
@end(Reason)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00404-01]}
@ChgAdded{Version=[2],Text=[If a dispatching operation is defined by a
@nt{subprogram_renaming_declaration} or the instantiation of a generic
subprogram, any access parameter of the renamed subprogram or the generic
subprogram that corresponds to a controlling access parameter of the
dispatching operation, shall have a subtype that excludes null.]}

A given subprogram shall not be a dispatching operation of two
or more distinct tagged types.
@begin{Reason}
  This restriction minimizes
  confusion since multiple dispatching is not provided. The normal
  solution is to replace all but one of the tagged types with their
  class-wide types.
@end{reason}
@begin{ramification}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0098],ARef=[AI95-00183-01]}
  @ChgAdded{Version=[1],Text=[This restriction applies even if the partial view (see
  @RefSecNum{Private Types and Private Extensions}) of one or both
  of the types is untagged. This follows from the definition of dispatching
  operation: the operation is a dispatching operation anywhere the full
  views of the (tagged) types are visible.]}
@end{ramification}

The explicit declaration of a primitive subprogram of a
tagged type shall occur before the type is frozen
(see @RefSecNum{Freezing Rules}).
@Redundant[For example, new dispatching operations cannot be added after
objects or values of the type exist,
nor after deriving a record extension from it,
nor after a body.]

@comment{The following is a "fix" to keep consistent with v. 5.95;
appearently 6.0 is different.
@begin{Discussion}
    @ChgRef{Version=[1],Kind=[Deleted]}
    @ChgDeleted{Version=[1],Text=[Old @b{Change}.]}
@end{Discussion}}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00344-01]}@ChgNote{Tags now have two parts, logically}
This rule is needed
because (1) we don't want people dispatching to things that haven't
been declared yet, and (2) we want to allow @Chg{Version=[2],New=[the static
part of ],Old=[]}tagged type descriptors
to be static (allocated statically, and initialized to link-time-known
symbols). Suppose T2 inherits primitive P from T1, and then
overrides P. Suppose P is called @i{before} the declaration of the
overriding P. What should it dispatch to? If the answer is the new
P, we've violated the first principle above. If the answer is the
old P, we've violated the second principle. (A call
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
@begin{ImplNote}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00326-01]}@ChgNote{We have tagged incomplete types now, and they don't freeze}
Because of this rule,
the type descriptor can be created (presumably containing linker
symbols pointing at the not-yet-compiled bodies) at the first
freezing point of the type.
It also prevents, for a @Chg{Version=[2],New=[(nonincomplete) ],Old=[]}tagged
type declared in a
@nt{package_specification}, overriding in the body or by a child subprogram.
@end{ImplNote}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}@ChgNote{Interfaces cause this too}
A consequence is that for a @Chg{Version=[2],New=[tagged type declaration],
Old=[@ntf{derived_type_declaration}]} in a
@nt{declarative_part}, only the @Chg{Version=[2],New=[last (overriding)],Old=[first]}
primitive subprogram can be
declared by a @nt{subprogram_body}.@Chg{Version=[2],New=[ (Other overridings
must be provided by @nt{subprogram_declaration}s.)],Old=[]}
@end{Ramification}
@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0222-1]}
  @ChgAdded{Version=[3],Text=[This rule applies only to "original" declarations
  and not to the completion of a primitive subprogram, even though a completion
  is technically an explicit declaration, and it may declare a primitive
  subprogram.]}
@end{Honest}

@end{Legality}

@begin{RunTime}
@Leading@PDefn2{Term=[execution], Sec=(call on a dispatching operation)}
@Defn{controlling tag value}
For the execution of a call on a dispatching operation of a type @i(T),
the @i(controlling tag value) determines
which subprogram body is executed.
The controlling tag value is defined as follows:
@begin(itemize)
  @PDefn{statically determined tag}
  If one or more controlling operands are statically tagged, then
  the controlling tag value is @i(statically determined) to be the tag
  of @i(T).

  If one or more controlling operands are dynamically tagged, then
  the controlling tag value is not statically determined, but is
  rather determined by
  the tags of the controlling operands.
  @IndexCheck{Tag_Check}
  If there is more than one dynamically tagged controlling operand,
  a check is made that they all have the same tag.
  @Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
  If this check fails, Constraint_Error is raised
  unless the call is a @nt<function_call> whose @nt<name> denotes
  the declaration of an equality operator (predefined or user defined) that
  returns Boolean,
  in which case the result of the call is defined to indicate
  inequality, and no @nt<subprogram_body> is executed.
  This check is performed prior to evaluating any tag-indeterminate
  controlling operands.
  @begin(Reason)
    Tag mismatch is considered an error (except for "=" and "/=")
    since the corresponding primitive
    subprograms in each specific type expect all controlling
    operands to be of the same type.
    For tag mismatch with an equality operator, rather than raising
    an exception, "=" returns False and "/=" returns True.
    No equality operator is actually invoked, since there
    is no common tag value to control the dispatch.
    Equality is a special case to be consistent with the existing
    Ada 83 principle that equality comparisons, even
    between objects with different constraints, never raise Constraint_Error.
  @end(Reason)

@Leading@keepnext@;@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00196-01]}
If all of the controlling operands @Chg{Version=[2],New=[(if any) ],Old=[]}are
tag-indeterminate, then:
  @begin(inneritemize)
    @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00239-01],ARef=[AI95-00416-01]}
    If the call has a controlling result@Chg{Version=[2],New=[ or controlling
    access result],Old=[]} and is itself@Chg{Version=[2],New=[, or designates,],Old=[]}
    a (possibly parenthesized or qualified)
    controlling operand of an enclosing call on a dispatching operation
    of @Chg{Version=[2],New=[a descendant of ],Old=[]}type @i(T),
    then its controlling tag value is determined by the controlling tag
    value of this enclosing call;

    @begin{Discussion}
      @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00239-01]}
      @ChgAdded{Version=[2],Text=[For code that a user can write explicitly,
      the only contexts that can control dispatching of a function with a
      controlling result of type T are those that involve controlling operands
      of the same type T: if the two types differ there is an illegality and
      the dynamic semantics are irrelevant.]}

      @ChgRef{Version=[2],Kind=[AddedNormal]}
      @ChgAdded{Version=[2],Text=[In the case of an inherited subprogram
      however, if a default expression is a function call, it may be of type T
      while the parameter is of a type derived from T. To cover this case, we
      talk about "a descendant of T" above. This is safe, because if the type
      of the parameter is descended from the type of the function result, it is
      guaranteed to inherit or override the function, and this ensures that
      there will be an appropriate body to dispatch to. Note that abstract
      functions are not an issue here because the call to the function is a
      dispatching call, so it is guaranteed to always land on a concrete
      body.]}
    @end{Discussion}

    @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00196-01],ARef=[AI95-00416-01]}
    @ChgAdded{Version=[2],Text=[If the call has a controlling result or
    controlling access result and (possibly parenthesized, qualified, or
    dereferenced) is the expression of an @nt{assignment_statement} whose
    target is of a class-wide type, then its controlling tag value is
    determined by the target;]}

    @PDefn{statically determined tag}
    Otherwise, the controlling tag value is statically determined to be
    the tag of type @i(T).
      @begin{Ramification}
        This includes the cases of
        a tag-indeterminate procedure call, and
        a tag-indeterminate @nt{function_call} that is
        used to initialize a class-wide formal parameter or class-wide
        object.
      @end{Ramification}
  @end(inneritemize)
@end(itemize)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0126-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[]}@ChgNote{Dummy add to allow conditional "leading"}
For the execution of a call on a dispatching operation,
the @Chg{Version=[2],New=[action performed is determined by the properties
of the corresponding dispatching operation],Old=[body executed is the one for
the corresponding primitive subprogram]} of the specific type
identified by the controlling tag value@Chg{Version=[3],New=[:],Old=[.
@Chg{Version=[2],New=[If the corresponding operation is],Old=[The body
for an]} explicitly declared
@Chg{Version=[2],New=[for this type, @Redundant[even if the declaration occurs
in a private part], then the action comprises an invocation of the],
Old=[dispatching operation is the corresponding]}
explicit body for the
@Chg{Version=[2],New=[operation. If the corresponding operation is
implicitly declared for this type:],Old=[subprogram.
The body for an implicitly
declared dispatching operation that is overridden is the body for the
overriding subprogram, @Redundant[even if the overriding occurs in a private
part.] The body for an inherited dispatching operation that is not overridden
is the body of the corresponding subprogram of the parent or ancestor type.]}]}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0126-1]}
@ChgAdded{Version=[3],Text=[if the corresponding operation is explicitly
declared for this type, @Redundant[even if the declaration occurs in a private
part], then the action comprises an invocation of the explicit body for the
operation;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0126-1]}
@ChgAdded{Version=[2],Text=[if the
@Chg{Version=[3],New=[corresponding ],Old=[]}operation is
@Chg{Version=[3],New=[implicitly
declared for this type and is ],Old=[]}implemented by an entry or
protected subprogram (see @RefSecNum{Task Units and Task Objects} and
@RefSecNum{Protected Units and Protected Objects}), then the action comprises a
call on this entry or protected subprogram, with the target object being given
by the first actual parameter of the call, and the actual parameters of the
entry or protected subprogram being given by the remaining actual parameters of
the call, if any;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0197-1]}
@ChgAdded{Version=[3],Text=[if the corresponding operation is a predefined
operator then the action comprises an invocation of that operator;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0126-1],ARef=[AI05-0197-1],ARef=[AI05-0250-1],ARef=[AI05-0254-1]}
@ChgAdded{Version=[2],Text=[otherwise, the action is the same as the action for
the corresponding operation of the parent type@Chg{Version=[3],New=[ or
progenitor type from which the operation was inherited except that additional
invariant checks (see @RefSecnum{Type Invariants}) and class-wide postcondition
checks (see @RefSecNum{Preconditions and Postconditions}) may apply. If there is
more than one such corresponding operation, the action is that for the operation
that is not a null procedure, if any; otherwise, the action is that of an
arbitrary one of the operations],Old=[]}.]}
@end{Itemize}

@begin{Honest}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0126-1]}
@ChgDeleted{Version=[3],Text=[In the unusual case in which a
dispatching subprogram is explicitly declared (overridden) by a body (with no
preceding @nt{subprogram_declaration}), the body for that dispatching subprogram
is that body; that is, the @lquotes@;corresponding explicit body@rquotes@; in
the above rule is the body itself.]}
@end{Honest}
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0005-1],ARef=[AI05-0126-1]}
@ChgAdded{Version=[3],Text=[@ldquote@;Corresponding dispatching operation@rdquote
refers to the inheritance relationship between subprograms. Primitive
operations are always inherited for a type T, but they might not be declared if
the primitive operation is never visible within the immediate scope of the type
T. If no corresponding operation is declared, the last bullet is used and the
corresponding operation of the parent type is executed (an explicit body that
happens to have the same name and profile is not called in that case).]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0005-1],ARef=[AI05-0126-1]}
@ChgAdded{Version=[3],Text=[We have to talk about progenitors in the last
bullet in case the corresponding operation is a null procedure inherited
from an interface. In that case, the parent type might not even have the
operation in question.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0197-1]}
@ChgAdded{Version=[3],Text=[For the last bullet, if there are multiple
corresponding operations for the parent and progenitors, all but one of them
have to be a null procedure. (If the progenitors declared abstract routines,
there would have to be an explicit overriding of the operation, and then the
first bullet would apply.) We call the nonnull routine if one exists.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0126-1]}
@ChgAdded{Version=[3],Text=[Any explicit declaration for an inherited
corresponding operation has to be an overriding routine.
These rules mean that a dispatching call executes the
overriding routine (if any) for the specific type.]}
@end{Ramification}
@begin{Reason}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
@Leading@;The wording of the above @Chg{Version=[3],New=[rules],Old=[rule]}
is intended to ensure that the same body is executed for a given tag,
whether that tag is determined statically or dynamically.
For a type declared in a package,
it doesn't matter whether a given subprogram is
overridden in the visible part or the private part,
and it doesn't matter whether the call is inside or outside the package.
For example:
@begin{Example}
@key[package] P1 @key[is]
    @key[type] T1 @key[is] @key[tagged] @key[null] @key[record];
    @key[procedure] Op_A(Arg : @key[in] T1);
    @key[procedure] Op_B(Arg : @key[in] T1);
@key[end] P1;

@key[with] P1; @key[use] P1;
@key[package] P2 @key[is]
    @key[type] T2 @key[is] @key[new] T1 @key[with] @key[null] @key[record];
    @key[procedure] Op_A(Param : @key[in] T2);
@key[private]
    @key[procedure] Op_B(Param : @key[in] T2);
@key[end] P2;

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00009 & AI-00114}
@key[with] P1; @key[with] P2;
@key[procedure] Main @key[is]
    X : @Chg{New=[P2.],Old=[]}T2;
    Y : @Chg{New=[P1.],Old=[]}T1'Class := X;
@key[begin]
    P2.Op_A(Param => X); --@RI{ Nondispatching call@Chg{New=[ to a dispatching operation],Old=[]}.}
    P1.Op_A(Arg => Y); --@RI{ Dispatching call.}
    P2.Op_B(Arg => X); --@RI{ Nondispatching call@Chg{New=[ to a dispatching operation],Old=[]}.}
    P1.Op_B(Arg => Y); --@RI{ Dispatching call.}
@key[end] Main;
@end{Example}

The two calls to Op_A both execute the body of Op_A that has to occur in
the body of package P2.
Similarly,
the two calls to Op_B both execute the body of Op_B that has to occur in
the body of package P2,
even though Op_B is overridden in the private part of P2.
Note, however, that the formal parameter names are different for P2.Op_A
versus P2.Op_B.
The overriding declaration for P2.Op_B is not visible in Main,
so the name in the call actually denotes the implicit declaration
of Op_B inherited from T1.


If a call occurs in the program text before an overriding,
which can happen only if the call is part of a default expression,
the overriding will still take effect for that call.

@end{Reason}

@begin{ImplNote}
Even when a tag is not @i(statically determined), a compiler
  might still be able to figure it out and thereby avoid
  the overhead of run-time dispatching.
@end{implnote}

@end{RunTime}

@begin{Notes}

The body to be executed for a call on a dispatching operation is
determined by the tag;
it does not matter whether that tag is determined statically or
dynamically,
and it does not matter whether the subprogram's declaration is visible at
the place of the call.


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00260-02]}
This subclause covers calls on @Chg{Version=[2],New=[dispatching],
Old=[primitive]} subprograms of a tagged type.
Rules for tagged type membership tests are described
in @RefSecNum(Relational Operators and Membership Tests).
Controlling tag determination for an
@nt{assignment_statement}
is described in @RefSecNum(Assignment Statements).

A dispatching call can dispatch to a body whose declaration is
not visible at the place of the call.

A call through an access-to-subprogram value is never
a dispatching call, even if the
access value designates a dispatching operation. Similarly
a call whose @nt<prefix> denotes a @nt<subprogram_renaming_declaration>
cannot be a dispatching call unless the renaming itself is the
declaration of a primitive subprogram.
@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
The concept of dispatching operations is new.
@end{Extend83}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00404-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  If a dispatching operation is defined by a
  @nt{subprogram_renaming_declaration}, and it has a controlling access
  parameter, Ada 2005 requires the subtype of the parameter to exclude null.
  The same applies to instantiations. This is required so that all
  calls to the subprogram operate the same way (controlling access parameters
  have to exclude null so that dispatching calls will work).
  Since Ada 95 didn't have the notion of access subtypes
  that exclude null, and all access parameters excluded null, it had no such
  rules. These rules will require the
  addition of an explicit @key{not null} on nondispatching operations that are
  later renamed to be dispatching, or on a generic that is used to define a
  dispatching operation.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Functions that have an access result type can be dispatching in
  the same way as a function that returns a tagged object directly.]}
@end{Extend95}

@begin{Diffword95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0010],ARef=[AI95-00127-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:>@ChgNote{This is documented as an extension in the two sections referenced below.}
  Allocators and access attributes of objects of class-wide types
  can be used as the controlling parameter in a dispatching calls. This
  was an oversight in the definition of Ada 95. (See @RefSecNum{Operations of Access Types} and
  @RefSecNum{Allocators}).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0011],ARef=[AI95-00117-01],ARef=[AI95-00430-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the conventions of
  dispatching operations. This is extended in Ada 2005 to cover operations
  inherited from progenitors, and to ensure that the conventions of all
  inherited operations are the same.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00196-01]}
  @ChgAdded{Version=[2],Text=[Clarified the wording to ensure that functions with
  no controlling operands are tag-indeterminate, and to describe that the
  controlling tag can come from the target of an @nt{assignment_statement}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00239-01]}
  @ChgAdded{Version=[2],Text=[Fixed the wording to cover default expressions
  inherited by derived subprograms. A literal reading of the old wording
  would have implied that operations would be called with objects of the
  wrong type.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00260-02]}
  @ChgAdded{Version=[2],Text=[An abstract formal subprogram is a dispatching
  operation, even though it is not a primitive operation. See
  @RefSec{Formal Subprograms}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01]}
  @ChgAdded{Version=[2],Text=[Dispatching calls include operations
  implemented by entries and protected operations, so we have to update the
  wording to reflect that.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00335-01]}
  @ChgAdded{Version=[2],Text=[A stream attribute of a tagged type is
  usually a dispatching operation, even though it is not a primitive
  operation. If they weren't dispatching, T'Class'Input and T'Class'Output
  wouldn't work.]}
@end{Diffword95}

@begin{Diffword2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0076-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Defined
  @ldquote@;function with a controlling result@rdquote, as it is used
  in @RefSecNum{Abstract Types and Subprograms}.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0126-1],ARef=[AI05-0197-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected holes in the
  definition of dynamic dispatching: the behavior for operations that are
  never declared and/or inherited from a progenitor were not specified.]}
@end{Diffword2005}


@LabeledSubClause{Abstract Types and Subprograms}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@redundant[@Defn{abstract type}
@IndexSeeAlso{Term=[abstract data type (ADT)],See=(abstract type)}
@IndexSeeAlso{Term=[ADT (abstract data type)],See=(abstract type)}
@IndexSee{Term=[concrete type],See=(nonabstract type)}
An @i(abstract type) is a tagged type intended
for use as @Chg{Version=[2],New=[an ancestor of other types],Old=[a parent
type for type extensions]}, but which is not allowed to have objects of its own.
@Defn{abstract subprogram}
@IndexSee{Term=[concrete subprogram],See=(nonabstract subprogram)}
An @i(abstract subprogram) is a subprogram that has no body,
but is intended to be overridden at some point when inherited.
Because objects of an abstract type cannot be created,
a dispatching call to an abstract subprogram always
dispatches to some overriding body.]
@ChgToGlossary{Version=[2],Kind=[Added],Term=<Abstract type>,
  Text=<@ChgAdded{Version=[2],Text=[An abstract type is a tagged type
  intended for use as an ancestor of other types, but which is not allowed to
  have objects of its own.]}>}
@end{Intro}

@begin{MetaRules}
An abstract subprogram has no body, so the
rules in this clause are designed to ensure (at compile time)
that the body will never be invoked.
We do so primarily by disallowing the creation of
values of the abstract type.
Therefore, since type conversion and parameter passing
don't change the tag, we know we will
never get a class-wide value with a tag identifying an abstract type.
This means that we only have to disallow nondispatching calls
on abstract subprograms (dispatching calls will never reach them).
@end{MetaRules}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00218-03],ARef=[AI95-00348-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0183-1]}
@AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=<abstract_subprogram_declaration>,Old=<>}>,
rhs="@Chg{Version=[2],New=<
    [@Syn2{overriding_indicator}]
    @Syn2{subprogram_specification} @key{is} @key{abstract}@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]};>,Old=<>}"}
@end{Syntax}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}
@Chg{Version=[2],New=[@Defn{abstract type}
@Defn2{Term=[type], Sec=(abstract)}Interface types
(see @RefSecNum{Interface Types}) are
abstract types. In addition, a tagged type that has the reserved word
@key{abstract} in its declaration is an abstract type. The class-wide type
(see @RefSecNum{Derivation Classes}) rooted at an abstract type is not itself
an abstract type.],Old=[]}
@end{StaticSem}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@Chg{Version=[2],New=[Only a tagged type shall have],Old=[@Defn{abstract type}
@Defn2{Term=[type], Sec=(abstract)}
An @i{abstract type} is a specific type
that has]} the reserved word @key{abstract} in its
declaration.@Chg{Version=[2],New=[],Old=[Only a tagged type is allowed to be
declared abstract.]}
@begin{Ramification}@ChgNote{These AARM notes really belong on the
Static Semantics paragraph, but I won't move them, as it's not worth the time.}
  Untagged types are never abstract,
  even though they can have primitive abstract subprograms.
  Such subprograms cannot be called,
  unless they also happen to be dispatching operations of
  some tagged type, and then only via a dispatching call.

  Class-wide types are never abstract.
  If T is abstract, then it is illegal to declare a stand-alone
  object of type T,
  but it is OK to declare a stand-alone object of type T'Class;
  the latter will get a tag from its initial value,
  and this tag will necessarily be different from T'Tag.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00260-02],ARef=[AI95-00348-01]}
@Defn{abstract subprogram}
@Defn2{Term=[subprogram], Sec=(abstract)}
A subprogram declared by an @nt{abstract_@!subprogram_@!declaration}
@Chg{Version=[2],New=[or a
@nt{formal_@!abstract_@!subprogram_@!declaration} (see @RefSecNum{Formal Subprograms})],
Old=[(see @RefSecNum{Subprogram Declarations})]}
is an @i{abstract subprogram}.
If it is a primitive subprogram of a tagged type,
then the tagged type shall be abstract.
@begin{Ramification}
@Leading@keepnext@;Note that for a private type, this applies to both views.
  The following is illegal:
@begin{Example}
@key[package] P @key[is]
    @key[type] T @key[is] @key[abstract] @key[tagged] @key[private];
    @key[function] Foo (X : T) @key[return] Boolean @key[is] @key[abstract]; --@RI{ Illegal!}
@key[private]
    @key[type] T @key[is] @key[tagged] @key[null] @key[record]; --@RI{ Illegal!}
    X : T;
    Y : Boolean := Foo (T'Class (X));
@key[end] P;
@end{Example}

The full view of T is not abstract,
but has an abstract operation Foo,
which is illegal.
The two lines marked "--@i{ Illegal!}" are illegal when taken together.
@end{Ramification}
@begin{Reason}
@Leading@;@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00310-01]}
  We considered disallowing untagged types from having abstract
  primitive subprograms.
  However, we rejected that plan, because it introduced some silly
  anomalies, and because such subprograms are harmless@Chg{Version=[2],
  New=[],Old=[ (if not terribly useful)]}.
  For example:
@begin{Example}
@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00010}
@key[package] P @key[is]
   @key[type] Field_Size @key[is] @key[range] 0..100;
   @key[type] T @key[is] @key[abstract tagged] @key[null] @key[record];
   @key[procedure] Print(X : @key[in] T; F : @key[in] Field_Size := 0) @key[is] @Chg{New=[@key[abstract]],Old=[abstract]};
  . . .
@key[package] Q @key[is]
   @key[type] My_Field_Size @key[is] @key[new] Field_Size;
   --@RI{ implicit declaration of Print(X : T; F : My_Field_Size := 0) @Chg{New=[@key[is abstract]],Old=[is abstract]};}
@key[end] Q;
@end{Example}

It seemed silly to make the derivative of My_Field_Size illegal,
just because there was an implicitly declared abstract subprogram
that was not primitive on some tagged type.
Other rules could be formulated to solve this problem,
but the current ones seem like the simplest.

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00310-01]}
@ChgAdded{Version=[2],Text=[In Ada 2005, abstract primitive subprograms of
an untagged type may be used to @lquotes@;undefine@rquotes@; an operation.]}
@end{Reason}

@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00260-02]}
@ChgAdded{Version=[2],Text=[Note that the second sentence does not apply to
abstract formal subprograms, as they are never primitive operations of
a type.]}
@end{Ramification}

@Leading@;@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01],ARef=[AI95-00334-01],ARef=[AI95-00391-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0097-1],ARef=[AI05-0198-1]}
@Chg{Version=[2],New=[If a type has an implicitly declared primitive subprogram
that is inherited or is @Chg{Version=[3],New=[a],Old=[the]} predefined
@Chg{Version=[3],New=[],Old=[equality ]}operator, and the corresponding
primitive subprogram of],Old=[For a derived type, if]}
the parent or ancestor type
@Chg{Version=[2],New=[is abstract or is a function with a controlling access
result, or if a type other than a
@Chg{Version=[3],New=[nonabstract ],Old=[]}null extension inherits a],
Old=[has an abstract primitive subprogram, or a primitive]}
function with a controlling result, then:
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0068-1]}
  @ChgAdded{Version=[3],Text=[These rules apply to each view of the type
  individually. That is necessary to preserve privacy. For instance, in the
  following example:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[package] P @key[is]
   @key[type] I @key[is interface];
   @key[procedure] Op (X : I) @key[is abstract];
@key[end] P;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[with] P;
@key[package] Q @key[is]
   @key[type] T @key[is abstract new] P.I @key[with private];
   -- @RI[Op inherited here.]
@key[private]
   @key[type] T @key[is abstract new] P.I @key[with null record];
   @key[procedure] Op (X : T) @key[is null];
@key[end] Q;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[with] Q;
@key[package] R @key[is]
   @key[type] T2 @key[is new] Q.T @key[with null record];
   -- @RI[Illegal. Op inherited here, but requires overriding.]
@key[end] R;]}
@end{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[If this did not depend on the view,
this would be legal. But in that case, the fact that Op is overridden
in the private part would be visible; package R would have to be
illegal if no overriding was in the private part.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Note that this means that whether an inherited
subprogram is abstract or concrete depends on where it inherited.
In the case of Q, Q.Op in the visible part is abstract,
while Q.Op in the private part is concrete. That is, R is illegal since
it is an unrelated unit (and thus it cannot see the private part), but if
R had been a private child of Q, it would have been legal.]}
@end{Ramification}
@begin{Itemize}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01],ARef=[AI95-00334-01]}
  If the @Chg{Version=[2],New=[],Old=[derived ]}type is abstract or untagged,
  the @Chg{Version=[2],New=[implicitly declared],Old=[inherited]} subprogram is
  @i{abstract}.
  @begin{Ramification}
    Note that it is possible to override a concrete subprogram
    with an abstract one.
  @end{Ramification}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00391-01]}
  Otherwise, the subprogram shall be overridden with a nonabstract
  subprogram@Chg{Version=[2],New=[ or, in the case of a private extension
  inheriting a function with a controlling result, have a full type that is
  a null extension],Old=[]}@Redundant[;
  for a type declared in the visible part of a package,
  the overriding may be either in the visible or the private part].
  @Chg{Version=[2],New=[Such a subprogram is said to
  @i{require overriding}.@Defn{require overriding} ],Old=[]}However,
  if the type is a generic formal type,
  the subprogram need not be overridden for the formal type itself;
  @Redundant[a nonabstract version will necessarily be provided by the
  actual type.]
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00228-01],ARef=[AI95-00391-01]}
    A function that returns the parent type @Chg{Version=[2],New=[requires
    overriding],Old=[becomes abstract]}@ChgNote{Can't leave this ancient and broken terminology around here!!}
    for @Chg{Version=[2],New=[a],Old=[an abstract]} type
    extension @Chg{Version=[2],New=[(or becomes abstract for an abstract type)],
    Old=[(if not overridden)]} because conversion
    from a parent type to a type extension is
    not defined, and function return semantics is defined in terms
    of conversion@Chg{Version=[2],New=[ (other than for a null extension;
    see below)],Old=[]}. (Note that parameters of mode @key{in out} or
    @key{out} do not have this problem, because the tag of the actual
    is not changed.)

    @Leading@keepnext@;Note that the overriding required above can be in the
    private part, which allows the following:
    @begin{Example}
@key[package] Pack1 @key[is]
    @key[type] Ancestor @key[is] @key[abstract] ...;
    @key[procedure] Do_Something(X : @key[in] Ancestor) @key[is] @key[abstract];
@key[end] Pack1;

@key[with] Pack1; @key[use] Pack1;
@key[package] Pack2 @key[is]
    @key[type] T1 @key[is] @key[new] Ancestor @key[with] @key[record] ...;
        --@RI{ A concrete type.}
    @key[procedure] Do_Something(X : @key[in] T1); --@RI{ Have to override.}
@key[end] Pack2;

@key[with] Pack1; @key[use] Pack1;
@key[with] Pack2; @key[use] Pack2;
@key[package] Pack3 @key[is]
    @key[type] T2 @key[is] @key[new] Ancestor @key[with] @key[private];
        --@RI{ A concrete type.}
@key[private]
    @key[type] T2 @key[is] @key[new] T1 @key[with] --@RI{ Parent different from ancestor.}
      @key[record] ... @key[end] @key[record];
    --@RI{ Here, we inherit Pack2.Do_Something.}
@key[end] Pack3;
    @end{Example}

    @ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00011}
    @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00228-01]}
    T2 inherits an abstract Do_Something, but @Chg{New=[T2],Old=[T]} is not
    abstract, so Do_Something has to be overridden.
    However, it is OK to override it in the private part.
    In this case, we override it by inheriting a concrete version
    from a different type.
    Nondispatching calls to Pack3.Do_Something are allowed
    both inside and outside package Pack3@Chg{Version=[2],New=[, as the
    client @lquotes@;knows@rquotes@; that the subprogram was necessarily
    overridden somewhere],Old=[]}.

    @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00391-01]}
    @ChgAdded{Version=[2],Text=[For a null extension, the result of a function
    with a controlling result is defined in terms of an @nt{extension_aggregate}
    with a @key{null record} extension part
    (see @RefSecNum{Derived Types and Classes}). This means that these
    restrictions on functions with a controlling result do not have to apply to
    null extensions.]}

    @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00391-01]}
    @ChgAdded{Version=[2],Text=[However, functions with controlling access
    results still require overriding. Changing the tag in place might clobber
    a preexisting object, and allocating new memory would possibly change the
    pool of the object, leading to storage leaks. Moreover, copying the object
    isn't possible for limited types. We don't need to restrict functions
    that have an access return type of an untagged type, as derived types
    with primitive subprograms have to have the same representation as their
    parent type.]}
  @end{Reason}
@end{Itemize}

A call on an abstract subprogram shall be a dispatching call;
@Redundant[nondispatching calls to an abstract subprogram are not
allowed.]
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00310-01]}
  If an abstract subprogram is not a dispatching operation of
  some tagged type, then it cannot be called at
  all.@Chg{Version=[2],New=[ In Ada 2005, such subprograms are not
  even considered by name resolution (see @RefSecNum{Subprogram Calls}).],Old=[]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0073-1],ARef=[AI05-0203-1]}
The type of an @nt{aggregate}, or of an object created by an
@nt{object_declaration} or an @nt{allocator},
or a generic formal object of mode @key[in],
shall not be abstract.
The type of the target of an assignment
operation (see @RefSecNum{Assignment Statements}) shall not
be abstract.
The type of a component shall not be abstract.
If the result type of a function is abstract,
then the function shall be abstract.@Chg{Version=[3],New=[
If a function has an access result type
designating an abstract type, then the function shall be abstract.
The type denoted by a @nt{return_subtype_indication} (see
@RefSecNum{Return Statements}) shall not be abstract. A generic function
shall not have an abstract result type or an access result type designating an
abstract type.],Old=[]}
@begin{Reason}
  This ensures that values of an abstract type cannot be created,
  which ensures that a dispatching call to an abstract subprogram
  will not try to execute the nonexistent body.

  Generic formal objects of mode @key[in] are like constants;
  therefore they should be forbidden for abstract types. Generic formal
  objects of mode @key[in out] are like renamings; therefore, abstract
  types are OK for them, though probably not terribly useful.

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0073-1]}
  @ChgAdded{Version=[3],Text=[Generic functions returning a formal
  abstract type are illegal because any instance would have to be
  instantiated with a nonabstract type in order to avoid violating
  the function rule (generic functions cannot be declared abstract).
  But that would be an implied contract; it would be better for the
  contract to be explicit by the formal type not being declared
  abstract. Moreover, the implied contract does not add any capability.]}
@end{Reason}

If a partial view is not abstract, the corresponding
full view shall not be abstract.
If a generic formal type is abstract,
then for each primitive subprogram of the formal that is not abstract,
the corresponding primitive subprogram of the actual shall
not be abstract.
@begin{Discussion}
  By contrast, we allow the actual type to be nonabstract
  even if the formal type is declared abstract.
  Hence, the most general formal tagged type possible is "@key(type)
  T(<>) @key(is abstract tagged limited private);".

  For an abstract private extension declared in the visible
  part of a package, it is only possible for the
  full type to be nonabstract if the private extension has
  no abstract dispatching operations.
@end{Discussion}
@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00294-01]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[In the sentence about primitive
  subprograms above, there is some ambiguity as to what is meant by
  @lquotes@;corresponding@rquotes@; in the
  case where an inherited operation is overridden.  This is best explained by
  an example, where the implicit declarations are shown as comments:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} P1 @key{is}
   @key{type} T1 @key{is abstract tagged null record};
   @key{procedure} P (X : T1); -- @RI[(1)]
@key{end} P1;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} P2 @key{is}
   @key{type} T2 @key{is abstract new} P1.T1 @key{with null record};
   -- @RI[@key{procedure} P (X : T2); -- (2)]
   @key{procedure} P (X : T2) @key{is abstract}; -- (3)
end P2;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{generic}
   @key{type} D @key{is abstract new} P1.T1 @key{with private};
   -- @RI[@key{procedure} P (X : D); -- (4)]
@key{procedure} G (X : D);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{procedure} I @key{is new} G (P2.T2); -- @RI[Illegal.]]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[Type T2 inherits a nonabstract procedure P (2) from the
primitive procedure P (1) of T1. P (2) is overridden by the explicitly declared
abstract procedure P (3). Type D inherits a nonabstract procedure P (4) from P
(1). In instantiation I, the operation corresponding to P (4) is the one which
is not overridden, that is, P (3): the overridden operation P (2) does not
@lquotes@;reemerge@rquotes@;. Therefore, the instantiation is illegal.],Old=[]}
@end{Honest}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0073-1]}
For an abstract type declared in a visible part,
an abstract primitive subprogram
shall not be declared in the private part,
unless it is overriding an abstract subprogram
implicitly declared in the visible part.
For a tagged type declared in a visible part,
a primitive function with a controlling result@Chg{Version=[3],New=[
or a controlling access result],Old=[]} shall not be declared
in the private part, unless it is overriding a function
implicitly declared in the visible part.
@begin{Reason}
@Leading@;The @lquotes@;visible part@rquotes@; could be that of a package
or a generic package. This rule is needed because a nonabstract type extension
declared outside the package would not know about any abstract primitive
subprograms or primitive functions with controlling results
declared in the private part, and wouldn't know that they
need to be overridden with nonabstract subprograms.
The rule applies to a tagged record type or record extension declared
in a visible part,
just as to a tagged private type or private extension.
The rule applies to explicitly and implicitly declared abstract
subprograms:
@begin{Example}
@key[package] Pack @key[is]
    @key[type] T @key[is] @key[abstract] @key[new] T1 @key[with] @key[private];
@key[private]
    @key[type] T @key[is] @key[abstract] @key[new] T2 @key[with] @key[record] ... @key[end] @key[record];
    ...
@key[end] Pack;
@end{Example}

The above example would be illegal if T1 has a nonabstract primitive
procedure P, but T2 overrides P with an abstract one;
the private part should override P with a nonabstract version.
On the other hand, if the P were abstract for both T1 and T2,
the example would be legal as is.
@end{Reason}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00260-02]}
A generic actual subprogram shall not be an abstract
subprogram @Chg{Version=[2],New=[unless the generic formal subprogram is
declared by a @nt{formal_abstract_subprogram_declaration}],Old=[]}.
The @nt{prefix} of an @nt{attribute_reference} for the Access,
Unchecked_Access, or Address attributes shall not denote an abstract subprogram.
@begin{Ramification}
An @nt{abstract_subprogram_declaration} is not syntactically a
@nt{subprogram_declaration}.
Nonetheless, an abstract subprogram is a subprogram,
and an @nt{abstract_subprogram_declaration} is a declaration of a subprogram.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00260-02]}
The part about generic actual subprograms includes those given by
default.@Chg{Version=[2],New=[ Of course, an abstract formal subprogram's
actual subprogram can be abstract.],Old=[]}
@end{Ramification}
@end{Legality}

@begin{Runtime}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00348-01]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=[elaboration], Sec=(abstract_subprogram_declaration)}
The elaboration of an @nt{abstract_subprogram_declaration} has no effect.]}
@end{Runtime}

@begin{Notes}
Abstractness is not inherited; to declare an abstract type,
the reserved word @key[abstract] has to be used
in the declaration of the type extension.
@begin{Ramification}
A derived type can be abstract even if its parent is not.
Similarly, an inherited concrete subprogram can be overridden with an
abstract subprogram.
@end{Ramification}

A class-wide type is never abstract. Even if a class
is rooted at an abstract type, the class-wide type for the
class is not abstract, and an object of the class-wide type
can be created; the tag of such an object will identify
some nonabstract type in the class.
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Example of an abstract type representing a set of natural numbers:)
@begin{Example}
@key(package) Sets @key(is)
    @key(subtype) Element_Type @key(is) Natural;
    @key(type) Set @key(is abstract tagged null record);
    @key(function) Empty @key(return) Set @key(is abstract);
    @key(function) Union(Left, Right : Set) @key(return) Set @key(is abstract);
    @key(function) Intersection(Left, Right : Set) @key(return) Set @key(is abstract);
    @key(function) Unit_Set(Element : Element_Type) @key(return) Set @key(is abstract);
    @key(procedure) Take(Element : @key(out) Element_Type;
                   From : @key(in out) Set) @key(is abstract);
@key(end) Sets;
@end{Example}
@end{Examples}

@begin{Notes}
@i(Notes on the example:)
Given the above abstract type, one could then derive
various (nonabstract) extensions of the type, representing
alternative implementations of a set. One might use a bit
vector, but impose an upper bound on the largest element representable,
while another might use a hash table, trading off space for flexibility.
@begin{Discussion}
One way to export a type from a package with some components visible
and some components private is as follows:
@begin{Example}
@key[package] P @key[is]
    @key[type] Public_Part @key[is] @key[abstract] @key[tagged]
        @key[record]
            ...
        @key[end] @key[record];
    @key[type] T @key[is] @key[new] Public_Part @key[with] @key[private];
    ...
@key[private]
    @key[type] T @key[is] @key[new] Public_Part @key[with]
        @key[record]
            ...
        @key[end] @key[record];
@key[end] P;
@end{Example}

The fact that Public_Part is abstract tells clients they have to
create objects of type T instead of Public_Part.
Note that the public part has to come first;
it would be illegal to declare a private type Private_Part,
and then a record extension T of it,
unless T were in the private part after the full declaration of Private_Part,
but then clients of the package would not have visibility to T.
@end{Discussion}
@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00391-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  It is not necessary to override functions with a controlling result
  for a null extension. This makes it easier to derive a tagged type
  to complete a private type.]}
@end{Extend95}

@begin{Diffword95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01],ARef=[AI95-00345-01]}
  @ChgAdded{Version=[2],Text=[Updated the wording to reflect the addition of
  interface types (see @RefSecNum{Interface Types}).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00260-02]}
  @ChgAdded{Version=[2],Text=[Updated the wording to reflect the addition of
  abstract formal subprograms (see @RefSecNum{Formal Subprograms}).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00334-01]}
  @ChgAdded{Version=[2],Text=[The wording of shall-be-overridden was clarified
  so that it clearly applies to abstract predefined equality.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00348-01]}
  @ChgAdded{Version=[2],Text=[Moved the syntax and elaboration rule for
  @nt{abstract_subprogram_declaration} here, so the syntax and most of the
  semantics are together (which is consistent with null procedures).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00391-01]}
  @ChgAdded{Version=[2],Text=[We define the term @i<require overriding>
  to make other wording easier to understand.]}
@end{Diffword95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0073-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Added rules to eliminate holes with controlling access results and generic
  functions that return abstract types. While these changes are technically
  incompatible, it is unlikely that they could be used in a program without
  violating some other rule of the use of abstract types.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0097-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected a minor
  glitch having to do with abstract null extensions. The Ada 2005
  rule allowed such extensions to inherit concrete operations in some
  rare cases. It is unlikely that these cases exist in user code.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in an
  @nt{abstract_subprogram_declaration}.
  This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}

@begin{Diffword2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0198-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified that the predefined
  operator corresponding to an inherited abstract operator is also abstract. The
  Ada 2005 rules caused the predefined operator and the inherited operator to
  override each other, which is weird. But the effect is the same either way
  (the operator is not considered for resolution).]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0203-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added wording to
  disallow abstract return objects. These were illegal in Ada 2005 by other
  rules; the extension to support class-wide type better opened a hole which
  has now been plugged.]}
@end{Diffword2005}


@LabeledAddedSubClause{Version=[2],Name=[Interface Types]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[@Redundant[An interface type is an abstract tagged
type that provides a restricted form of multiple inheritance. A tagged type,
task type, or protected type may have one or more interface types as
ancestors.]]}
@ChgToGlossary{Version=[2],Kind=[AddedNormal],Term=<Interface type>,
  Text=<@ChgAdded{Version=[2],Text=[An interface type is a form of abstract
  tagged type which has no components or concrete operations
  except possibly null procedures. Interface types are used for
  composing other interfaces and tagged types and thereby
  provide multiple inheritance. Only an interface type can be used as a
  progenitor of another type.]}>}
@end{Intro}

@begin{MetaRules}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01],ARef=[AI95-00345-01]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[The rules are designed so that an
  interface can be used as either a parent type or a progenitor type without
  changing the meaning. That's important so that the order that interfaces are
  specified in a @nt{derived_type_definition} is not significant. In particular,
  we want:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{type} Con1 @key{is new} Int1 @key{and} Int2 @key{with null record};
@key{type} Con2 @key{is new} Int2 @key{and} Int1 @key{with null record};]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Trailing],Text=[to mean exactly the same thing.]}
@end{MetaRules}

@begin{Syntax}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01],ARef=[AI95-00345-01]}
@AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=<interface_type_definition>,Old=<>}>,
rhs="@Chg{Version=[2],New=<
    [@key{limited} | @key{task} | @key{protected} | @key{synchronized}] @key{interface} [@key{and} @Syn2{interface_list}]>,Old=<>}"}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01],ARef=[AI95-00419-01]}
@Syn{lhs=<@Chg{Version=[2],New=<interface_list>,Old=<>}>,
rhs="@Chg{Version=[2],New=<@SynI{interface_}@Syn2{subtype_mark} {@key{and} @SynI{interface_}@Syn2{subtype_mark}}>,Old=<>}"}

@end{Syntax}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
@Chg{Version=[2],New=[An interface type (also called an @i{interface})
is@RootDefn{interface}@PDefn2{Term=[interface],Sec=[type]}
a specific abstract tagged type that is defined by
an @nt{interface_type_definition}.],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[An interface with the reserved word @key{limited},
@key{task}, @key{protected},
or @key{synchronized} in its definition is termed, respectively, a @i{limited
interface}, a @i{task interface}, a @i{protected interface}, or a
@i{synchronized interface}.
In addition,@PDefn2{Term=[interface],Sec=[synchronized]}
@PDefn2{Term=[interface],Sec=[protected]}
@PDefn2{Term=[interface],Sec=[task]}
@PDefn2{Term=[interface],Sec=[limited]}
@PDefn2{Term=[interface],Sec=[nonlimited]}
@Defn{synchronized interface}
@Defn{protected interface}
@Defn{task interface}
@Defn{limited interface}
@Defn{nonlimited interface}
all task and protected interfaces
are synchronized interfaces, and all synchronized interfaces are limited
interfaces.]}
@ChgToGlossary{Version=[2],Kind=[AddedNormal],Term=<Synchronized>,
  Text=<@ChgAdded{Version=[2],Text=[A synchronized entity is one
  that will work safely with multiple tasks at one time. A synchronized
  interface can be an ancestor of a task or a protected type. Such a
  task or protected type is called a synchronized tagged type.]}>}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01],ARef=[AI95-00443-01]}
@ChgAdded{Version=[2],Text=[@Defn{synchronized tagged type}
@PDefn2{Term=[type],Sec=[synchronized tagged]}
@PDefn2{Term=[tagged type],Sec=[synchronized]}
@PDefn2{Term=[tagged type],Sec=[task]}
@PDefn2{Term=[tagged type],Sec=[protected]}
@Defn{task tagged type}
@Defn{protected tagged type}
@Redundant[A task or protected type derived from an interface is a tagged type.]
Such a tagged type is called a @i<synchronized> tagged
type, as are synchronized interfaces and private extensions whose declaration
includes the reserved word @b{synchronized}.]}

@begin{TheProof}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The full definition of tagged types given in
@RefSecNum{Tagged Types and Type Extensions} includes task and protected types
derived from interfaces.]}
@end{TheProof}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The class-wide type associated with a tagged task
type (including a task interface type) is a task type, because
@lquotes@;task@rquotes is one of the language-defined classes of types (see
@RefSecNum{Types and Subtypes}). However, the class-wide type associated with an
interface is @i<not> an interface type, as @lquotes@;interface@rquotes is
@i<not> one of the language-defined classes (as it is not closed under
derivation). In this sense, @lquotes@;interface@rquotes is similar to
@lquotes@;abstract@rquotes. The class-wide type associated with an interface is
a concrete (nonabstract) indefinite tagged composite type.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@lquotes@;Private extension@rquotes@; includes
generic formal private extensions, as explained in
@RefSecNum{Formal Private and Derived Types}.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[A task interface is an @Redundant[abstract] task
type. A protected interface is an @Redundant[abstract] protected type.]}
@begin{TheProof}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The @lquotes@;abstract@rquotes follows
  from the definition of an interface type.]}
@end{TheProof}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This ensures that task operations (like abort and
  the Terminated attribute) can be applied to a task interface type and the
  associated class-wide type. While there are no protected type operations,
  we apply the same rule to protected interfaces for consistency.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
@Chg{Version=[2],New=[@Redundant[An interface type has no components.]],Old=[]}
@begin{TheProof}
@ChgRef{Version=[2],Kind=[AddedNormal]}
   @ChgAdded{Version=[2],Text=[This follows from the syntax and the fact that
   discriminants are not allowed for interface types.]}
@end{TheProof}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00419-01]}
@ChgAdded{Version=[2],Text=[@Defn{progenitor subtype}@Defn{progenitor type}
An @Syni(interface_)@nt{subtype_mark} in an @nt{interface_list} names a
@i(progenitor subtype); its type is the @i(progenitor type).
An interface type inherits user-defined primitive subprograms from each
progenitor type in the same way that a derived type inherits user-defined
primitive subprograms from its progenitor types
(see @RefSecNum{Derived Types and Classes}).]}
@ChgToGlossary{Version=[2],Kind=[Added],Term=<Progenitor>,
  Text=<@ChgAdded{Version=[2],Text=[A progenitor of a derived type is
  one of the types given in the definition of the derived type other
  than the first. A progenitor is always an interface type.
  Interfaces, tasks, and protected types may also have progenitors.]}>}

@end{StaticSem}

@begin{Legality}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
@Chg{Version=[2],New=[All user-defined primitive subprograms of an interface
type shall be abstract subprograms or null procedures.],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
@Chg{Version=[2],New=[The type of a subtype named in an @nt{interface_list}
shall be an interface type.],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[A type derived from a nonlimited interface shall be
nonlimited.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[An interface derived from a task interface shall
include the reserved word @key{task} in its
definition; any other type derived from a task interface shall be a private
extension or a task type declared by a task declaration (see
@RefSecNum{Task Units and Task Objects}).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[An interface derived from a
protected interface shall include
the reserved word @key{protected} in its definition; any other type derived
from a protected interface shall be a private extension or a protected type
declared by a protected declaration (see
@RefSecNum{Protected Units and Protected Objects}).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[An interface derived from
a synchronized interface shall include one of the reserved words @key{task},
@key{protected}, or @key{synchronized} in its definition; any other type
derived from a synchronized interface shall be a private extension, a task
type declared by a task declaration, or a protected type declared by a protected
declaration.]}

@begin{Reason}
   @ChgRef{Version=[2],Kind=[AddedNormal]}
   @ChgAdded{Version=[2],Text=[We require that an interface descendant of a
   task, protected, or synchronized interface repeat the explicit kind of
   interface it will be, rather than simply inheriting it, so that a reader is
   always aware of whether the interface provides synchronization and whether
   it may be implemented only by a task or protected type. The only place where
   inheritance of the kind of interface might be useful would be in a generic
   if you didn't know the kind of the actual interface. However, the value of
   that is low because you cannot implement an interface properly if you don't
   know whether it is a task, protected, or synchronized interface. Hence, we
   require the kind of the actual interface to match the kind of the formal
   interface (see @RefSecNum{Formal Interface Types}).]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[No type shall be derived from both a task interface
and a protected interface.]}

@begin{Reason}
   @ChgAdded{Version=[2],Text=[This prevents a single private
   extension from inheriting from both a task and a protected interface. For a
   private type, there can be no legal completion. For a generic formal derived
   type, there can be no possible matching type (so no instantiation could be
   legal). This rule provides early detection of the errors.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
@ChgAdded{Version=[2],Text=[In addition to the places where @LegalityTitle
normally apply (see @RefSecNum{Generic Instantiation}), these rules apply also
in the private part of an instance of a generic
unit.@PDefn{generic contract issue}]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This paragraph is intended to apply to all of the
  @LegalityTitle@; in this clause. We cannot allow interface types which do not
  obey these rules, anywhere. Luckily, deriving from a formal type (which might
  be an interface) is not allowed for any tagged types in a generic body. So
  checking in the private part of a generic covers all of the cases.]}
@end{Ramification}

@end{Legality}

@begin{Runtime}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0070-1]}
@ChgAdded{Version=[2],Text=[The elaboration of an
@nt{interface_type_definition} @Chg{Version=[3],New=[creates
the interface type and its first subtype],Old=[has no effect]}.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[There is no other
  effect. ],Old=[]}An @nt{interface_list} is made up of
  @nt{subtype_mark}s, which do not need to be elaborated, so the
  @nt{interface_list} does not either. This is consistent with the
  handling of @nt{discriminant_part}s.]}
@end{Discussion}
@end{Runtime}

@begin{Notes}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00411-01]}
@ChgAdded{Version=[2],Text=[Nonlimited interface types have predefined
nonabstract equality operators. These may be overridden with user-defined
abstract equality operators. Such operators will then require
an explicit overriding for any nonabstract descendant of the interface.]}
@end{Notes}

@begin{Examples}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@i{Example of a limited
interface and a synchronized interface extending it:}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{type} Queue @key{is limited interface};
@key{procedure} Append(Q : @key{in out} Queue; Person : @key{in} Person_Name) @key{is abstract};
@key{procedure} Remove_First(Q      : @key{in out} Queue;
                       Person : @key{out} Person_Name) @key{is abstract};
@key{function} Cur_Count(Q : @key{in} Queue) @key{return} Natural @key{is abstract};
@key{function} Max_Count(Q : @key{in} Queue) @key{return} Natural @key{is abstract};
-- @RI[See @RefSecNum{Incomplete Type Declarations} for Person_Name.]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0004-1]}
@ChgAdded{Version=[2],Text=[Queue_Error : @key{exception};
--@RI[ Append raises Queue_Error if @Chg{Version=[3],New=[Cur_Count],Old=[Count]}(Q) = Max_Count(Q)]
--@RI[ Remove_First raises Queue_Error if @Chg{Version=[3],New=[Cur_Count],Old=[Count]}(Q) = 0]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{type} Synchronized_Queue @key{is synchronized interface and} Queue; --@RI[ see @RefSecNum{Example of Tasking and Synchronization}]
@key{procedure} Append_Wait(Q      : @key{in out} Synchronized_Queue;
                      Person : @key{in} Person_Name) @key{is abstract};
@key{procedure} Remove_First_Wait(Q      : @key{in out} Synchronized_Queue;
                            Person : @key{out} Person_Name) @key{is abstract};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[...]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{procedure} Transfer(From   : @key{in out} Queue'Class;
                   To     : @key{in out} Queue'Class;
                   Number : @key{in}     Natural := 1) @key{is}
   Person : Person_Name;
@key{begin}
   @key{for} I @key{in} 1..Number @key{loop}
      Remove_First(From, Person);
      Append(To, Person);
   @key{end loop};
@key{end} Transfer;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This defines a Queue interface defining a queue of
people. (A similar design could be created to define any kind of queue simply
by replacing Person_Name by an appropriate type.) The Queue interface has four
dispatching operations, Append, Remove_First, Cur_Count, and Max_Count. The
body of a class-wide operation, Transfer is also shown. Every nonabstract
extension of Queue must provide implementations for at least its four
dispatching operations, as they are abstract. Any object of a type derived from
Queue may be passed to Transfer as either the From or the To operand. The two
operands need not be of the same type in any given call.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The Synchronized_Queue interface inherits the four
dispatching operations from Queue and adds two additional dispatching
operations, which wait if necessary rather than raising the Queue_Error
exception. This synchronized interface may only be implemented by a task or
protected type, and as such ensures safe concurrent access.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@i{Example use of the interface:}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0004-1]}
@ChgAdded{Version=[2],Text=[@key{type} Fast_Food_Queue @key{is new} Queue @key{with record} ...;
@key{procedure} Append(Q : @key{in out} Fast_Food_Queue; Person : @key{in} Person_Name);
@key{procedure} Remove_First(Q : @key{in out} Fast_Food_Queue; Person : @Chg{Version=[3],New=[@key{out}],Old=[@key{in}]} Person_Name);
@key{function} Cur_Count(Q : @key{in} Fast_Food_Queue) @key{return} Natural;
@key{function} Max_Count(Q : @key{in} Fast_Food_Queue) @key{return} Natural;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[...]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Cashier, Counter : Fast_Food_Queue;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[...
-- @RI[Add George (see @RefSecNum{Incomplete Type Declarations}) to the cashier's queue:]
Append (Cashier, George);
-- @RI[After payment, move George to the sandwich counter queue:]
Transfer (Cashier, Counter);
...]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[An interface such as Queue can be used directly as
the parent of a new type (as shown here), or can be used as a progenitor when a
type is derived. In either case, the primitive operations of the
interface are inherited. For Queue, the implementation of the four inherited
routines must be provided. Inside the call of Transfer, calls will dispatch to
the implementations of Append and Remove_First for type Fast_Food_Queue.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@i{Example of a task interface:}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{type} Serial_Device @key{is task interface};  --@RI[ see @RefSecNum{Task Units and Task Objects}]
@key{procedure} Read (Dev : @key{in} Serial_Device; C : @key{out} Character) @key{is abstract};
@key{procedure} Write(Dev : @key{in} Serial_Device; C : @key{in}  Character) @key{is abstract};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The Serial_Device interface has two dispatching
operations which are intended to be implemented by task entries (see 9.1).]}

@end{Examples}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01],ARef=[AI95-00345-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}Interface types
  are new. They provide multiple inheritance of interfaces, similar to the
  facility provided in Java and other recent language designs.]}
@end{Extend95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0070-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected the definition
  of elaboration for an @nt{interface_type_definition} to match that
  of other type definitions.]}
@end{DiffWord2005}


@LabeledClause{Access Types}

@begin{Intro}
@Defn{access type}
@Defn{access value}
@Defn{designate}
A value of an access type (an @i(access value))
provides indirect access to the object or subprogram
it @i(designates). Depending on its type, an access value
can designate either subprograms, objects created by allocators
(see @RefSecNum(Allocators)), or more generally @i(aliased) objects of
an appropriate type.
@IndexSee{Term=[pointer],See=(access value)}
@IndexSee{Term=[pointer type],See=(access type)}
@begin(Discussion)
  A @nt<name> @i(denotes) an entity; an access value @i(designates) an
  entity. The @lquotes@;dereference@rquotes@; of an access value X, written
  @lquotes@;X.@key[all]@rquotes@;, is a @nt<name> that denotes the entity designated by X.
@end(Discussion)
@end{Intro}

@begin{MetaRules}
Access values should always be well defined
(barring uses of certain unchecked features of Section 13).
In particular, uninitialized access variables should be prevented
by compile-time rules.
@end{MetaRules}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01]}
@Syn{lhs=<access_type_definition>,rhs="
    @Chg{Version=[2],New=<[@Syn2{null_exclusion}] >,Old=<>}@Syn2{access_to_object_definition}
  | @Chg{Version=[2],New=<[@Syn2{null_exclusion}] >,Old=<>}@Syn2{access_to_subprogram_definition}"}

@Syn{lhs=<access_to_object_definition>,rhs="
    @key{access} [@Syn2{general_access_modifier}] @Syn2{subtype_indication}"}
@Syn{lhs=<general_access_modifier>,rhs="@key{all} | @key{constant}"}

@Syn{lhs=<access_to_subprogram_definition>,rhs="
    @key{access} [@key{protected}] @key{procedure} @Syn2{parameter_profile}
  | @key{access} [@key{protected}] @key{function}  @Syn2{parameter_and_result_profile}"}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00231-01]}
@AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=[null_exclusion],Old=[]}>,
rhs="@Chg{Version=[2],New=[@key{not} @key{null}],Old=[]}"}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01],ARef=[AI95-00254-01],ARef=[AI95-00404-01]}
@Syn{lhs=<access_definition>,rhs="@Chg{Version=[2],New=<
    [@Syn2{null_exclusion}] @key{access} [@key{constant}] @Syn2{subtype_mark}
  | [@Syn2{null_exclusion}] @key{access} [@key{protected}] @key{procedure} @Syn2{parameter_profile}
  | [@Syn2{null_exclusion}] @key{access} [@key{protected}] @key{function} @Syn2{parameter_and_result_profile}>,
Old=[@key{access} @Syn2{subtype_mark}]}"}

@end{Syntax}

@begin{StaticSem}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0012],ARef=[AI95-00062-01]}
@Defn{access-to-object type}
@Defn{access-to-subprogram type}
@Defn{pool-specific access type}
@Defn{general access type}
There are two kinds of access types, @i{access-to-object} types, whose
values designate objects, and @i(access-to-subprogram) types, whose
values designate subprograms.
@Defn{storage pool}
Associated with an access-to-object type is a @i(storage pool);
several access types may share the same storage pool.
@Chg{New=[All descendants of an access type share the same storage pool.],Old=[]}
@Defn{pool element}
A storage pool is an area of storage used to hold dynamically
allocated objects (called @i(pool elements)) created
by allocators@Redundant[; storage pools
are described further in @RefSec(Storage Management)].

@Defn{pool-specific access type}
@Defn{general access type}
Access-to-object types are further subdivided into
@i(pool-specific) access types, whose values can designate only
the elements of their associated storage pool,
and @i(general) access types, whose
values can designate the elements of any storage pool,
as well as aliased objects created by declarations
rather than allocators,
and aliased subcomponents of other objects.
@begin(ImplNote)
  The value of an access type will typically be a machine address.
  However, a value of a pool-specific access type can be
  represented as an offset (or index) relative to its storage pool,
  since it can point only to the elements of that pool.
@end(ImplNote)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00225-01],ARef=[AI95-00363-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0053-1],ARef=[AI05-0142-4],ARef=[AI05-0277-1]}
@Defn{aliased}
A view of an object is defined to be @i(aliased) if
it is defined by an @nt<object_@!declaration>@Chg{Version=[3],New=[,],Old=[ or]}
@nt<component_@!definition>@Chg{Version=[3],New=[,
@nt{parameter_@!specification}, or
@nt{extended_return_object_declaration}],Old=[]}
with the reserved word @key(aliased), or by a renaming of an aliased view.
In addition, the dereference of an access-to-object
value denotes an aliased view, as does a view conversion
(see @RefSecNum{Type Conversions}) of an aliased view.
@Chg{Version=[2],New=[The],Old=[Finally, the]} current instance of
@Chg{Version=[3],New=[an immutably limited type (see @RefSecNum{Limited Types})
is],Old=[a limited@Chg{Version=[2],New=[ tagged],Old=[]} type,
@Chg{Version=[2],New=[a protected type, a task type, or a type that has the
reserved word @key{limited} in its full definition is also],Old=[]}]}
@Chg{Version=[2],New=[defined to be aliased. Finally,],Old=[and]}
a formal parameter or generic formal object of a
tagged type @Chg{Version=[2],New=[is],Old=[are]} defined to be aliased.
@Redundant[Aliased views are the ones that can be designated by an
access value.]
@Chg{Version=[2],New=[],Old=[@Defn2{Term=[constrained], Sec=(object)}
@Defn2{Term=[unconstrained], Sec=(object)}
@Defn{constrained by its initial value}
If the view defined by an @nt{object_@!declaration} is aliased,
and the type of the object has discriminants,
then the object is constrained;
if its nominal subtype is unconstrained,
then the object is constrained by its initial value.
@Redundant[Similarly, if the object created by an @nt<allocator>
has discriminants, the object is constrained,
either by the designated subtype, or by its initial value.]]}
@ToGlossary{Term=<Aliased>,
  Text=<An aliased view of an object is one that can be designated by an
  access value.
  Objects allocated by allocators are aliased.
  Objects can also be explicitly declared as aliased with
  the reserved word @key(aliased).
  The Access attribute can be used to create an access value
  designating an aliased object.>}
@begin(Ramification)
  The current instance of a nonlimited type is not aliased.

  The object created by an allocator is aliased, but not its
  subcomponents, except of course for those that themselves have @key(aliased)
  in their @nt<component_definition>.

  The renaming of an aliased object is aliased.

  Slices are never aliased.
  See @RefSecNum{Slices} for more discussion.
@end(Ramification)
@begin(Reason)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00225-01]}
  The current instance of a limited type is defined to be aliased
  so that an access discriminant of a component can be initialized
  with T'Access inside the definition of T.@Chg{Version=[2],New=[ Note that
  we don't want this to apply to a type that could become nonlimited later
  within its immediate scope, so we require the full definition to be limited.],Old=[]}

  A formal parameter of a tagged type is defined to be aliased
  so that a (tagged) parameter X may be passed to an access parameter P
  by using P => X'Access. Access parameters are most important
  for tagged types because of dispatching-on-access-parameters
  (see @RefSecNum(Dispatching Operations of Tagged Types)).
  By restricting this to formal parameters, we minimize problems
  associated with allowing components that are not declared aliased
  to be pointed-to from within the same record.

@Leading@;A view conversion of an aliased view is aliased so that
  the type of an access parameter can be changed without
  first converting to a named access type. For example:
@begin{Example}
@key(type) T1 @key(is tagged) ...;
@key(procedure) P(X : @key(access) T1);

@key(type) T2 @key(is new) T1 @key(with) ...;
@key(procedure) P(X : @key(access) T2) @key(is)
@key(begin)
    P(T1(X.@key(all))'Access);  --@RI[ hand off to T1's P]
    . . .     --@RI[ now do extra T2-specific processing]
@key(end) P;
@end{Example}

  @ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00363-01]}
  @ChgDeleted{Version=[2],Text=[The rule about objects with discriminants is
  necessary because values of a constrained access subtype
  can designate an object whose nominal subtype is unconstrained;
  without this rule, a check on every use of such values would
  be required to ensure that the discriminants of the object had not
  changed.
  With this rule (among others), we ensure that if there might
  exist aliased views of a discriminated object, then the object is
  necessarily constrained.
  Note that this rule is necessary only for untagged types,
  since a discriminant of a tagged type can't have a default,
  so all tagged discriminated objects are always constrained
  anyway.]}
  @ChgNote{This rule was a disaster, so it thankfully has been repealed.
  We instead make general access constrained subtypes illegal if the type
  allows unconstrained instances, see Discriminant Constraints.}

  @ChgRef{Version=[2],Kind=[Revised]}@ChgNote{Eilers reported double word}
  We considered making more kinds of objects aliased by default.
  In particular, any object of a by-reference type will pretty
  much have to be allocated at an addressable location,
  so it can be passed by reference without using bit-field
  pointers. Therefore, one might wish to allow the Access and
  @Chg{Version=[2],New=[],Old=[and ]}Unchecked_Access attributes for such
  objects. However, private parts are transparent to the definition of
  @lquotes@;by-reference type@rquotes@;, so if we made all objects of a by-reference
  type aliased, we would be violating the privacy of private parts.
  Instead, we would have to define a concept of @lquotes@;visibly by-reference@rquotes@;
  and base the rule on that.
  This seemed to complicate the rules more than it was worth,
  especially since there is no way to declare an untagged limited
  private type to be by-reference, since the full type might by
  nonlimited.
@end(Reason)
@begin{Discussion}
  Note that we do not use the term @lquotes@;aliased@rquotes@; to refer to formal
  parameters that are referenced through multiple access paths
  (see @RefSecNum{Formal Parameter Modes}).
@end{Discussion}

An @nt{access_to_object_definition} defines
an access-to-object type and its first subtype;
@Defn2{Term=[designated subtype], Sec=(of a named access type)}
@Defn2{Term=[designated type], Sec=(of a named access type)}
the @nt<subtype_@!indication> defines the @i(designated subtype)
of the access type.
If a @nt<general_@!access_@!modifier> appears, then the access type
is a general access type.
@Defn{access-to-constant type}
If the modifier is the reserved word @key(constant), then the type is an
@i(access-to-constant type)@Redundant[; a designated object cannot be updated
through a value of such a type].
@Defn{access-to-variable type}
If the modifier is the reserved word @key(all),
then the type is an @i(access-to-variable type)@Redundant[; a designated object
can be both read and updated through a value of such a type].
If no @nt<general_@!access_@!modifier> appears in the
@nt<access_to_@!object_@!definition>, the access type is a
pool-specific access-to-variable type.
@begin{Honest}
  The type of the designated subtype is called the
  @i{designated type}.
@end{Honest}
@begin(Reason)
  The modifier @key(all) was picked to suggest that
  values of a general access type could point into @lquotes@;all@rquotes@; storage pools,
  as well as to objects declared aliased, and that @lquotes@;all@rquotes@; access
  (both read and update) to the designated object was provided.
  We couldn't think of any use for pool-specific
  access-to-constant types, so any access type defined with
  the modifier @key(constant) is considered a general access type,
  and can point into any storage pool or at other (appropriate)
  aliased objects.
@end(Reason)
@begin(ImplNote)
  The predefined generic Unchecked_Deallocation can be
  instantiated for any named access-to-variable type. There
  is no (language-defined) support for deallocating objects
  designated by a value of an access-to-constant type. Because of this,
  an allocator for an access-to-constant type can allocate
  out of a storage pool with no support for deallocation.
  Frequently, the allocation can be done at link-time,
  if the size and initial value are known then.
@end(ImplNote)
@begin(Discussion)
  For the purpose of generic formal type matching, the relevant
  subclasses of access types are access-to-subprogram
  types, access-to-constant types, and (named) access-to-variable types,
  with its subclass (named) general access-to-variable types.
  Pool-specific access-to-variable types are not a separately matchable
  subclass of types, since they don't have any @lquotes@;extra@rquotes@; operations
  relative to all (named) access-to-variable types.
@end(Discussion)

@Defn{access-to-subprogram type}
An @nt{access_to_subprogram_definition} defines
an access-to-subprogram type and its first subtype;
@Defn2{Term=[designated profile], Sec=(of an access-to-subprogram type)}
the @nt<parameter_profile> or @nt<parameter_and_result_profile>
defines the @i(designated profile) of the access type.
@Defn2{Term=[calling convention], Sec=(associated with a designated profile)}
There is a @i(calling convention) associated with the designated
profile@Redundant[; only subprograms with this
calling convention can be designated by
values of the access type.]
By default, the calling convention is @lquotes@i(protected)@rquotes@; if the reserved
word @key(protected) appears, and @lquotes@;Ada@rquotes@; otherwise.
@Redundant[See @RefSecNum{Interface to Other Languages}
for how to override this default.]
@begin(Ramification)
  The calling convention @i(protected) is
  in italics to emphasize that it cannot be specified explicitly
  by the user. This is a consequence of it being a reserved word.
@end(Ramification)
@begin(ImplNote)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00254-01]}
  For @Chg{Version=[2],New=[a named],Old=[an]} access-to-subprogram type, the
  representation of an access value might include
  implementation-defined information needed to support
  up-level references @em for example, a static link.
  The accessibility rules (see @RefSecNum(Operations of Access Types)) ensure
  that in a "global-display-based" implementation model (as opposed to
  a static-link-based model), @Chg{Version=[2],New=[a named],Old=[an]}
  access-to-(unprotected)-subprogram value need consist only of the
  address of the subprogram. The global display is guaranteed
  to be properly set up any time the designated subprogram is called.
  Even in a static-link-based model, the only time a static link
  is definitely required is for an access-to-subprogram type declared
  in a scope nested at least two levels deep within subprogram or
  task bodies, since values of such a type might designate subprograms
  nested a smaller number of levels. For the normal case of
  @Chg{Version=[2],New=[a named],Old=[an]} access-to-subprogram type
  declared at the outermost (library) level,
  a code address by itself should be sufficient to represent the
  access value in many implementations.

  For access-to-protected-subprogram, the access values will necessarily
  include both an address (or other identification) of the code of the
  subprogram, as well as the address of the associated protected object.
  This could be thought of as a static link, but it will be needed even
  for global-display-based implementation models. It corresponds
  to the value of the @lquotes@;implicit parameter@rquotes@; that is passed into
  every call of a protected operation, to identify the current instance
  of the protected type on which they are to operate.

  Any Elaboration_Check is performed when a call
  is made through an access value, rather than when the
  access value is first "created" via a 'Access.
  For implementation models that normally put that check at the
  call-site, an access value will have to point to a separate
  entry point that does the check. Alternatively, the
  access value could point to a "subprogram descriptor" that
  consisted of two words (or perhaps more), the first being
  the address of the code, the second being the elaboration bit.
  Or perhaps more efficiently, just the address of the code,
  but using the trick that the descriptor is initialized to
  point to a Raise-Program-Error routine initially, and then
  set to point to the "real" code when the body is elaborated.

  For implementations that share code between generic instantiations,
  the extra level of indirection suggested above to support
  Elaboration_Checks could also be used to provide a pointer to the
  per-instance data area normally required when calling shared code.
  The trick would be to put a pointer to the per-instance data
  area into the subprogram descriptor, and then make sure that
  the address of the subprogram descriptor is loaded into a
  "known" register whenever an indirect call is performed.
  Once inside the shared code, the address of the per-instance
  data area can be retrieved out of the subprogram descriptor,
  by indexing off the "known" register.

  @ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00344-01]}
  @ChgNote{This is not remotely true with nested extensions and with
  interfaces. I don't much feel like trying to explain this properly.}
  @ChgDeleted{Version=[2],Text=[Essentially the same implementation issues
  arise for calls on dispatching operations of tagged types, except that
  the static link is always known "statically."]}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00254-01]}
  Note that access parameters of an
  anonymous access-to-subprogram type are @Chg{Version=[2],New=[],Old=[not ]}permitted.
  @Chg{Version=[2],New=[Such],Old=[If there were such]} parameters@Chg{Version=[2],
  New=[ represent],Old=[,]} full @lquotes@;downward@rquotes@;
  closures@Chg{Version=[2],New=[],Old=[would be required]}, meaning that
  in an implementation that uses a per-task (global) display,
  the display @Chg{Version=[2],New=[will],Old=[would]} have to be passed
  as a hidden parameter,
  and reconstructed at the point of call.@Chg{Version=[2],New=[],
  Old=[ This was felt to be an undue implementation burden,
  given that an equivalent (actually, more general) capability
  is available via formal subprogram parameters to a generic.]}
@end(ImplNote)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01],ARef=[AI95-00231-01],ARef=[AI95-00254-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@Defn{anonymous access type}
@Defn2{Term=[designated subtype], Sec=(of an anonymous access type)}
@Defn2{Term=[designated type], Sec=(of an anonymous access type)}
An @nt{access_definition} defines an anonymous
general @Chg{Version=[2],New=[access type or an
anonymous access-to-subprogram type. For a general access type,],
Old=[access-to-variable type;]} the @nt<subtype_mark> denotes
its @i(designated subtype)@Chg{Version=[2],New=[; if the
@nt{general_@!access_@!modifier} @key{constant} appears, the type is an
access-to-constant type; otherwise@Chg{Version=[3],New=[,],Old=[]} it is
an access-to-variable type. For an access-to-subprogram type, the
@nt{parameter_@!profile} or @nt{parameter_@!and_@!result_@!profile} denotes its
@i{designated profile}.@Defn2{Term=[designated profile], Sec=(of an anonymous access type)}],
Old=[. @Redundant[An @nt<access_definition> is used in the
specification of an access discriminant
(see @RefSecNum(Discriminants)) or an access
parameter (see @RefSecNum(Subprogram Declarations)).]]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01],ARef=[AI95-00231-01]}
@Defn2{Term=[null value], Sec=(of an access type)}
For each @Chg{Version=[2],New=[],Old=[(named) ]}access type, there is
@Chg{Version=[2],New=[],Old=[a literal @key(null) which has ]}a null
access value designating no entity at all@Chg{Version=[2],New=[, which can be
obtained by (implicitly) converting the literal @key{null} to the access
type],Old=[]}.
@Redundant[The null value of @Chg{Version=[2],New=[an],Old=[a named]} access
type is the default initial value of the type.]
@Chg{Version=[2],New=[Nonnull],Old=[Other]} values of an
access@Chg{Version=[2],New=[-to-object],Old=[]} type are
obtained by evaluating @Chg{Version=[2],New=[],Old=[an
@nt<attribute_reference> for the Access or Unchecked_Access
attribute of an aliased view of an object or nonintrinsic
subprogram, or, in the case of a named access-to-object type,]}
an @nt<allocator>@Redundant[, which
returns an access value designating a newly created object
(see @RefSecNum(Operations of Access Types))]@Chg{Version=[2],New=[, or in the
case of a general access-to-object type, evaluating an
@nt{attribute_reference} for the Access or Unchecked_Access
attribute of an aliased view of an object. Nonnull values of an
access-to-subprogram type are obtained by evaluating an
@nt{attribute_reference} for the Access attribute of a
nonintrinsic subprogram],Old=[]}.

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00231-01]}
  @ChgDeleted{Version=[2],Text=[A value of an anonymous access type
  (that is, the value of an access parameter or access discriminant)
  cannot be null.]}
@end{ramification}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00231-01]}
@ChgDeleted{Version=[2],Text=[Access parameters allow dispatching on the
tag of the object designated
by the actual parameter (which gets converted to the anonymous access
type as part of the call).
In order for dispatching to work properly,
there had better be such an object.
Hence, the type conversion will raise Constraint_Error if the value of
the actual parameter is null.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00231-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[excludes null],Sec=[subtype]}
A @nt{null_exclusion} in a construct specifies
that the null value does not belong to the access subtype defined by the
construct, that is, the access subtype @i{excludes null}. In addition, the
anonymous access subtype defined by the @nt{access_definition} for a controlling
access parameter (see @RefSecNum{Dispatching Operations of Tagged Types})
excludes null. Finally, for a @nt{subtype_indication} without a
@nt{null_exclusion}, the subtype denoted by the @nt{subtype_indication}
excludes null if and only if the subtype denoted by the @nt{subtype_mark} in
the @nt{subtype_indication} excludes null.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Text=[An @nt{access_definition} used in a controlling
  parameter excludes null because it is necessary to read the tag to
  dispatch, and null has no tag. We would have preferred to
  require @key{not null} to be specified for such
  parameters, but that would have been too incompatible with Ada 95 code
  to require.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[Note that we considered imposing a similar
  implicit null exclusion for controlling access results, but chose not to do
  that, because there is no Ada 95 compatibility issue, and there is no
  automatic null check inherent in the use of a controlling access result. If a
  null check is necessary, it is because there is a dereference of the result,
  or because the value is passed to a parameter whose subtype excludes null.
  If there is no dereference of the result, a null return value is perfectly
  acceptable, and can be a useful indication of a particular status of the
  call.]}
@end{Reason}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0013],ARef=[AI95-00012-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@PDefn2{Term=[constrained],Sec=(subtype)}
@PDefn2{Term=[unconstrained],Sec=(subtype)}
@Redundant[All subtypes of an access-to-subprogram type
are constrained.] The first subtype of a type defined by
an @Chg{New=[@nt<access_definition>],Old=[@nt<access_type_definition>]} or an
@nt<access_to_object_definition> is unconstrained if the designated subtype
is an unconstrained array or discriminated @Chg{New=[subtype],Old=[type]};
otherwise@Chg{Version=[3],New=[,],Old=[]} it is constrained.
@begin(TheProof)
  The @LegalityTitle on @nt<range_constraint>s (see @RefSecNum(Scalar Types))
  do not permit the @nt<subtype_mark> of the @nt<subtype_indication> to denote
  an access-to-scalar type, only a scalar type.
  The @LegalityTitle on @nt<index_constraint>s
  (see @RefSecNum(Index Constraints and Discrete Ranges)) and
  @nt<discriminant_constraint>s (see @RefSecNum(Discriminant Constraints))
  both permit access-to-composite types in a @nt<subtype_indication>
  with such _@nt<constraint>s. Note that an access-to-access-to-composite
  is never permitted in a @nt<subtype_indication> with a @nt<constraint>.
@end(TheProof)
@begin(Reason)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00363-01]}
  Only @nt<composite_constraint>s are permitted
  for an access type, and only
  on access-to-composite types.
  A constraint on an access-to-scalar or access-to-access type
  might be violated due to assignments
  via other access paths that were not so constrained. By contrast,
  if the designated subtype is an array or discriminated
  type@Chg{Version=[2],New=[ without defaults],Old=[]},
  the constraint could not be violated by unconstrained assignments,
  since array objects are always constrained, and
  @Chg{Version=[2],New=[],Old=[aliased]} discriminated objects are
  also constrained @Chg{Version=[2],New=[when the type does not have defaults
  for its discriminants. Constraints are not allowed on general access-to-unconstrained
  discriminated types if the type has defaults for its discriminants;
  constraints on pool-specific access types are usually allowed because
  allocated objects are usually constrained by their initial value.],
  Old=[(by fiat, see @StaticSemTitle).]}
@end(Reason)
@end{StaticSem}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00231-01]}
@ChgAdded{Version=[2],Text=[If a
@nt{subtype_@!indication}, @nt{discriminant_@!specification},
@nt{parameter_@!specification}, @nt{parameter_@!and_@!result_@!profile},
@nt{object_@!renaming_@!declaration}, or @nt{formal_@!object_@!declaration}
has a @nt{null_@!exclusion}, the @nt{subtype_@!mark} in that construct
shall denote an access subtype that does not exclude null.]}
@begin(Honest)
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Text=[This means @lquotes@;directly allowed in@rquotes;
  we are not talking about a @nt{null_exclusion} that occurs in an
  @nt{access_definition} in one of these constructs (for an
  @nt{access_definition}, the @nt{subtype_mark} in such an
  @nt{access_definition} is not restricted).]}
@end(Honest)
@begin(Reason)
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Text=[This is similar to doubly constraining a
  composite subtype, which we also don't allow.]}
@end(Reason)
@end{Legality}

@begin{RunTime}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01]}
@PDefn2{Term=[compatibility],
  Sec=(composite_constraint with an access subtype)}
A @nt<composite_constraint> is @i(compatible) with an unconstrained
access subtype if it is compatible with the designated
subtype.@Chg{Version=[2],New=[ A @nt{null_exclusion} is compatible with any
access subtype that does not exclude null.],Old=[]}
@PDefn2{Term=[satisfies], Sec=(for an access value)}
An access value @i(satisfies) a @nt<composite_constraint> of an access
subtype if it equals the null value of its type
or if it designates an object whose value satisfies the
constraint.@Chg{Version=[2],New=[ An access value satisfies an exclusion
of the null value if it does not equal the
null value of its type.],Old=[]}

@PDefn2{Term=[elaboration], Sec=(access_type_definition)}
The elaboration of an @nt{access_type_definition}
creates the access type and its first subtype.
For an access-to-object type,
this elaboration includes the elaboration of the @nt{subtype_indication},
which creates the designated subtype.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01],ARef=[AI95-00254-01]}
@PDefn2{Term=[elaboration], Sec=(access_definition)}
The elaboration of an @nt{access_definition} creates
an anonymous@Chg{Version=[2],New=[],Old=[ general]} access@Chg{Version=[2],
New=[],Old=[-to-variable]} type@Chg{Version=[2],New=[],
Old=[ @Redundant[(this happens as part of the initialization of
an access parameter or access discriminant)]]}.
@end{RunTime}

@begin{Notes}
Access values are called @lquotes@;pointers@rquotes@; or @lquotes@;references@rquotes@; in
some other languages.

Each access-to-object type has an associated storage pool;
several access types can share the same pool.
An object can be created in the storage pool of an
access type by an @nt{allocator} (see @RefSecNum{Allocators})
for the access type.
A storage pool (roughly) corresponds to what some other languages
call a @lquotes@;heap.@rquotes@;
See @RefSecNum{Storage Management} for a discussion of pools.

Only @nt<index_constraint>s and @nt<discriminant_constraint>s
can be applied to access types
(see @RefSecNum{Index Constraints and Discrete Ranges} and
@RefSecNum{Discriminant Constraints}).
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Examples of access-to-object types:}
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@key[type] Peripheral_Ref @key<is @Chg{Version=[2],New=[not null ],Old=[]}access> Peripheral;  --@RI[  see @RefSecNum{Variant Parts and Discrete Choices}]
@key[type] Binop_Ptr @key[is access all] Binary_Operation'Class;
                                           --@RI[ general access-to-class-wide, see @RefSecNum{Type Extensions}]
@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Example of an access subtype:}
@end{WideAbove}
@begin{Example}
@key[subtype] Drum_Ref @key[is] Peripheral_Ref(Drum);  --@RI[  see @RefSecNum{Variant Parts and Discrete Choices}]
@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Example of an access-to-subprogram type:}
@end{WideAbove}
@begin{Example}
@key[type] Message_Procedure @key[is] @key[access] @key[procedure] (M : @key[in] String := "Error!");
@key[procedure] Default_Message_Procedure(M : @key[in] String);
Give_Message : Message_Procedure := Default_Message_Procedure'Access;
...
@key[procedure] Other_Procedure(M : @key[in] String);
...
Give_Message := Other_Procedure'Access;
...
Give_Message("File not found.");  --@RI{ call with parameter (.@key[all] is optional)}
Give_Message.@key[all];                 --@RI{ call with no parameters}
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The syntax for @nt{access_type_definition} is
changed to support general access
types (including access-to-constants) and access-to-subprograms.
The syntax rules for @nt{general_access_modifier} and
@nt{access_definition} are new.
@end{Extend83}

@begin{DiffWord83}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0190-1]}
  We use the term "storage pool" to talk about the data area from which
  allocation takes place. The term "collection" is @Chg{Version=[3],New=[only
  used for finalization],Old=[no longer used]}. ("Collection" and "storage pool"
  are not the same thing because multiple unrelated access types can share the
  same storage pool; see @RefSecNum(Storage Management) for more discussion.)
@end{DiffWord83}

@begin{Inconsistent95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Text=[@Defn{inconsistencies with Ada 95}
  Access discriminants and noncontrolling access parameters no longer
  exclude null. A program which passed @key{null} to such an access
  discriminant or access parameter and expected it to raise Constraint_Error
  may fail @ChgNote{but not if the parameter is dereferenced in the subprogram or
  record }when compiled with Ada 2005. One hopes that there no such programs
  outside of the ACATS. (Of course, a program which actually wants to pass
  @key{null} will work, which is far more likely.)]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
  @ChgAdded{Version=[2],Text=[Most unconstrained aliased objects
  with defaulted discriminants are no longer
  constrained by their initial values. This means that a program that
  raised Constraint_Error from an attempt to change the discriminants
  will no longer do so. The change only affects programs that depended
  on the raising of Constraint_Error in this case, so the inconsistency
  is unlikely to occur outside of the ACATS. This change may however cause
  compilers to implement these objects differently, possibly taking additional
  memory or time. This is unlikely to be worse than the differences caused by
  any major compiler upgrade.]}
@end{Inconsistent95}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00225-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @B[Amendment Correction:] The rule defining when a current instance of a
  limited type is considered to be aliased has been tightened to apply only to
  types that cannot become nonlimited. A program that attempts to take 'Access
  of the current instance of a limited type that can become nonlimited will be
  illegal in Ada 2005. While original Ada 95 allowed the current instance of
  any limited type to be treated as aliased, this was inconsistently
  implemented in compilers, and was likely to not work as expected for types
  that are ultimately nonlimited.]}
@end{Incompatible95}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01]}
@Chg{Version=[2],New=[@Defn{extensions to Ada 95}The @nt{null_exclusion} is
new. It can be used in both anonymous and named access type definitions.
It is most useful to declare that parameters cannot be @key{null},
thus eliminating the need for checks on use.],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01],ARef=[AI95-00254-01],ARef=[AI95-00404-01]}
@Chg{Version=[2],New=[The kinds of anonymous
access types allowed were increased by adding anonymous access-to-constant
and anonymous access-to-subprogram types. Anonymous access-to-subprogram
types used as parameters allow passing of subprograms at any level.],Old=[]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0012],ARef=[AI95-00062-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added accidentally-omitted
  wording that says that a derived access type shares its storage pool with its
  parent type. This was clearly intended, both because of a note in
  @RefSecNum{Derived Types and Classes}, and because anything else would
  have been incompatible with Ada 83.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0013],ARef=[AI95-00012-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Fixed typographical errors in
  the description of when access types are constrained.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01]}
  @ChgAdded{Version=[2],Text=[The wording was fixed to allow @nt{allocator}s and
  the literal @key{null} for anonymous access types. The former was clearly
  intended by Ada 95; see the @ImplAdviceTitle in @RefSecNum{Storage Management}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
  @ChgAdded{Version=[2],Text=[The rules about aliased objects being constrained by
  their initial values now apply only to allocated objects, and thus have
  been moved to @RefSec{Allocators}.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0053-1],ARef=[AI05-0277-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> The rule about a current instance
  being aliased now is worded in terms of immutably limited types.
  Wording was also added to make extended return object declarations that have the
  keyword @key[aliased] be considered aliased. This latter was a significant
  oversight in Ada 2005 @em technically, the keyword @key[aliased] had no
  effect. But of course implementations followed the intent, not the letter of
  the Standard.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0142-4]}
  @ChgAdded{Version=[3],Text=[Explicitly aliased parameters (see
  @RefSecNum{Subprogram Declarations}) are defined to be aliased.]}
@end{DiffWord2005}


@LabeledSubClause{Incomplete Type Declarations}

@begin{Intro}
There are no particular limitations on the designated type of an
access type. In particular, the type of a component of the
designated type can be another access type, or even the same access
type. This permits mutually dependent and recursive access types.
An @nt<incomplete_type_declaration> can be used to introduce a type
to be used as a designated type, while deferring its full definition
to a subsequent @nt<full_type_declaration>.
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00326-01]}
@Syn{lhs=<incomplete_type_declaration>,
rhs="@key{type} @Syn2{defining_identifier} [@Syn2{discriminant_part}]@Chg{Version=[2],New=< [@key{is tagged}]>,Old=<>};"}
@end{Syntax}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00326-01]}
@ChgAdded{Version=[2],Text=[@Defn{incomplete type}@Defn{incomplete view}
An @nt{incomplete_type_declaration} declares
an @i{incomplete view} of a
type and its first subtype; the first subtype is unconstrained if
a @nt<discriminant_part> appears. If the
@nt{incomplete_@!type_@!declaration} includes the reserved word @key{tagged}, it
declares a @i{tagged incomplete view}.@Defn2{Term=[incomplete view],Sec=[tagged]}@Defn{tagged incomplete view}
@Redundant[An incomplete view of a type is a limited view of the type (see @RefSecNum{Limited Types}).]]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00326-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[Given an access type @i{A} whose designated
type @i{T} is an incomplete view, a dereference of a value of type @i{A} also
has this incomplete view except when:]}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0208-1]}
  @ChgAdded{Version=[3],Text=[Whether the designated type is an incomplete view
  (and thus whether this set of rules applies) is determined by the view of the
  type at the declaration of the access type; it does not change during the life
  of the type.]}
@end{Discussion}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[it occurs within the immediate scope of the completion
of @i{T}, or],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0208-1]}
@ChgAdded{Version=[2],Text=[it occurs within the scope of a @nt{nonlimited_with_clause}
that mentions a library package in whose visible part the completion of @i{T}
is declared@Chg{Version=[3],New=[, or],Old=[.]}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0208-1]}
@ChgAdded{Version=[3],Text=[it occurs within the scope of the completion
of @i{T} and @i{T} is an incomplete view declared by an
@nt{incomplete_type_declaration}.]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0162-1]}
@ChgAdded{Version=[2],Text=[In these cases, the dereference has the
@Chg{Version=[3],New=[],Old=[full ]}view
of @i{T}@Chg{Version=[3],New=[ visible at the point of the dereference],Old=[]}.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[We need the @lquotes@;in
  whose visible part@rquotes@; rule so that the second rule doesn't trigger
  in the body of a package with a @key{with} of a child unit:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@Key{package} P @Key{is}
@Key{private}
   @Key{type} T;
   @Key{type} PtrT @Key{is access} T;
@Key{end} P;],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@Key{private package} P.C @Key{is}
   Ptr : PtrT;
@Key{end} P.C;],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
@Chg{Version=[2],New=[@Key{with} P.C;
@Key{package body} P @Key{is}
    -- @RI{Ptr.all'Size is not legal here, but @Chg{Version=[3],New=[we are within],Old=[it is in]} the scope}
    -- @RI{of a @nt{nonlimited_with_clause} @RI{for P.}}
@Key{type} T @Key{is} ...
    --  @RI{Ptr.all'Size is legal here.}
@Key{end} P;],Old=[]}
@end{Example}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00412-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0162-1],ARef=[AI05-0208-1]}
@ChgAdded{Version=[2],Text=[Similarly, if a @nt{subtype_mark} denotes a
@nt{subtype_declaration} defining a subtype of an incomplete view @i<T>, the
@nt{subtype_mark} denotes an incomplete view except under the
same @Chg{Version=[3],New=[three],Old=[two]}
circumstances given above, in which case it denotes the
@Chg{Version=[3],New=[],Old=[full ]}view of @i<T>@Chg{Version=[3],New=[ visible at the
point of the @nt{subtype_mark}],Old=[]}.]}
@end{StaticSem}

@begin{Legality}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0162-1]}
@PDefn2{Term=[requires a completion], Sec=(@nt<incomplete_type_declaration>)}
An @nt{incomplete_@!type_@!declaration} requires a completion, which shall
be a @Chg{Version=[3],New=[@nt{type_@!declaration} other than an
@nt{incomplete_@!type_@!declaration}],Old=[@nt{full_@!type_@!declaration}]}.
@Redundant[If the @nt{incomplete_@!type_@!declaration} occurs immediately
within either the visible part of a
@nt{package_@!specification} or a @nt<declarative_@!part>,
then the @Chg{Version=[3],New=[@nt{type_@!declaration}],
Old=[@nt{full_@!type_@!declaration}]} shall occur later and immediately within this
visible part or @nt<declarative_@!part>.
If the @nt{incomplete_@!type_@!declaration} occurs
immediately within the private part of a
given @nt<package_@!specification>, then the
@Chg{Version=[3],New=[@nt{type_@!declaration}],
Old=[@nt{full_@!type_@!declaration}]} shall occur later and immediately
within either the private part itself, or the @nt{declarative_@!part}
of the corresponding @nt{package_@!body}.]
@begin{TheProof}
This is implied by the next AARM-only rule,
plus the rules in @RefSec{Completions of Declarations}
which require a completion to appear later and immediately within
the same declarative region.
@end{TheProof}
@begin{Honest}
If the @nt{incomplete_type_declaration} occurs immediately within
the visible part of a @nt{package_specification},
then the @Chg{Version=[3],New=[completing @nt{type_@!declaration}],
Old=[@nt{full_@!type_@!declaration}]}
shall occur immediately within this visible part.
@end{Honest}
@begin(Honest)
  If the implementation supports it, an
  @nt{incomplete_type_declaration} can be @Chg{Version=[3],New=[imported
  (using aspect Import, see @RefSecNum{Interfacing Aspects}), in which
  case no explicit completion is allowed],Old=[completed by a @nt{pragma}
  Import]}.
@end(Honest)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00326-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0162-1]}
@Chg{Version=[2],New=[If an @nt{incomplete_@!type_@!declaration} includes the
reserved word @key{tagged}, then a @Chg{Version=[3],New=[@nt{type_@!declaration}],
Old=[@nt{full_@!type_@!declaration}]} that completes
it shall declare a tagged type. ],Old=[]}If an @nt{incomplete_@!type_@!declaration}
has a @nt{known_@!discriminant_@!part},
then a @Chg{Version=[3],New=[@nt{type_@!declaration}],
Old=[@nt{full_@!type_@!declaration}]} that completes it shall have a fully
conforming (explicit) @nt{known_@!discriminant_@!part}
(see @RefSecNum(Conformance Rules)).
@Defn2{Term=[full conformance],Sec=(required)}
@Redundant[If an @nt{incomplete_@!type_@!declaration} has no @nt<discriminant_part>
(or an @nt<unknown_@!discriminant_@!part>),
then a corresponding @Chg{Version=[3],New=[@nt{type_@!declaration}],
Old=[@nt{full_@!type_@!declaration}]} is nevertheless allowed
to have discriminants, either explicitly, or inherited via derivation.]


@Leading@keepnext@;@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00326-01]}
@Chg{Version=[2],New=[A],Old=[The only allowed uses of a]} @nt{name} that
denotes an @Chg{Version=[2],New=[incomplete view of a type may be used],
Old=[@nt{incomplete_type_declaration} are]} as follows:
@begin(Discussion)
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00326-01]}
  @ChgDeleted{Version=[2],Text=[No need to say "prior to the end of the
  @nt{full_type_declaration}" since the name would not denote the
  @nt{incomplete_type_declaration} after the end of the
  @nt{full_type_declaration}. Also, with child library units, it would not be
  well defined whether they come before or after the @nt<full_type_declaration>
  for deferred incomplete types.]}
@end(Discussion)
@begin(itemize)
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0098-1]}
  as the @nt{subtype_mark} in the @nt{subtype_indication}
  of an @nt{access_to_@!object_@!definition};
  @Redundant[the only form of @nt{constraint} allowed in this
  @nt{subtype_indication} is a @nt{discriminant_constraint}@Chg{Version=[3],New=[
  @Redundant[(a @nt{null_exclusion} is not allowed)]],Old=[]};]
  @begin(ImplNote)
    We now allow @nt<discriminant_constraint>s even if the
    full type is deferred to the package body. However, there
    is no particular implementation burden because we have
    dropped the concept of the dependent compatibility check.
    In other words, we have effectively repealed AI83-00007.
  @end(ImplNote)

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00326-01],ARef=[AI95-00412-01]}@ChgNote{The old rule is moved}
  as the @nt{subtype_mark} @Chg{Version=[2],New=[in the @nt{subtype_indication}
  of a @nt{subtype_declaration}; the @nt{subtype_@!indication} shall not have a
  @nt{null_@!exclusion} or a @nt{constraint};],Old=[defining the subtype
  of a parameter or result of an @nt{access_to_@!subprogram_@!definition};]}
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00326-01]}@ChgNote{Really moved}
    @ChgDeleted{Version=[2],Text=[This allows, for example, a record to have a
    component designating a subprogram that takes that same record
    type as a parameter.]}
  @end{Reason}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00326-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0151-1]}
  as the @nt<subtype_mark> in an
  @nt<access_definition>@Chg{Version=[2],New=[@Chg{Version=[3],New=[ for
  an access-to-object type;],Old=[.]}],Old=[;]}
@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This does not mean any random @nt{subtype_mark}
  in a construct that makes up an @nt<access_definition>, such as
  a @nt<formal_part>, just the one given directly in the syntax of
  @nt<access_definition>.]}
@end{Honest}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0151-1]}
  @ChgAdded{Version=[3],Text=[as the @nt{subtype_mark} defining the subtype
  of a parameter or result in a profile occurring within a
  @nt{basic_declaration};]}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[But not in the profile for a body or entry.]}
@end{Ramification}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0213-1]}
  @ChgAdded{Version=[3],Text=[as a generic actual parameter whose corresponding
  generic formal parameter is a formal incomplete type (see
  @RefSecNum{Formal Private and Derived Types}).]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00326-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[If such a @nt{name}
denotes a tagged incomplete view, it may also be used:]}

@begin{Itemize}
  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00326-01]}
  @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0151-1]}
  @ChgAdded{Version=[2],Text=[as the @nt{subtype_mark} defining the subtype of a
  parameter in @Chg{Version=[3],New=[the profile for a @nt{subprogram_body},
  @nt{entry_body}, or @nt{accept_statement}],Old=[a @nt{formal_part}]};]}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00326-01]}
  as the @nt{prefix} of an @nt{attribute_reference}
  whose @nt{attribute_@!designator}
  is Class; such an @nt{attribute_@!reference}
  is @Chg{Version=[2],New=[],Old=[similarly ]}restricted to the uses
  allowed here;
  @Chg{Version=[2],New=[it denotes a tagged incomplete view],
  Old=[when used in this way,
  the corresponding @nt{full_type_@!declaration} shall
  declare a tagged type, and the @nt<attribute_@!reference>
  shall occur in the same library unit as
  the @nt<incomplete_@!type_@!declaration>]}.
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00326-01]}
    @ChgDeleted{Version=[2],Text=[This is to prevent
    children from imposing requirements on their ancestor library
    units for deferred incomplete types.]}
  @end{reason}
@end(itemize)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00326-01]}
@ChgRef{Version=[3],Kind=[DeletedAdded],ARef=[AI05-0151-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Chg{Version=[3],New=[],Old=[If
such a @nt{name} occurs within
the declaration list containing the completion of the
incomplete view, it may also be used:]}]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00326-01]}@ChgNote{Really moved}
@ChgRef{Version=[3],Kind=[DeletedAdded],ARef=[AI05-0098-1],ARef=[AI05-0151-1]}@ChgNote{Now part of the next paragraph}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[as the @nt{subtype_mark} defining the subtype of a
parameter or result of an @nt{access_to_@!subprogram_@!definition}.]}]}
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgRef{Version=[3],Kind=[Deleted]}
    @ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[This allows,
    for example, a record to have a
    component designating a subprogram that takes that same record
    type as a parameter.]}]}
  @end{Reason}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00326-01]}
@ChgAdded{Version=[2],Text=[If any of the above uses occurs as part of the
declaration of a primitive subprogram of the incomplete view,
and the declaration occurs immediately within the private part of a package, then
the completion of the incomplete view shall also
occur immediately within the private part; it shall not be deferred to the
package body.]}
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[This fixes a hole in Ada 95 where a dispatching
    operation with an access parameter could be declared in a private part
    and a dispatching call on it could occur in a child even though there is
    no visibility on the full type, requiring access to the controlling tag
    without access to the representation of the type.]}
  @end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00326-01]}
@ChgAdded{Version=[2],Text=[No other uses of a @nt{name} that denotes an
incomplete view of a type are allowed.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00326-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0151-1]}
@Chg{Version=[2],New=[A @nt{prefix} that denotes an object],
Old=[A dereference (whether implicit or explicit @em see @RefSecNum(Names))]}
shall not be of an incomplete @Chg{Version=[2],New=[view],Old=[type]}.@Chg{Version=[3],New=[
An actual parameter in a call shall not be of an untagged incomplete
view. The result object of a function call shall not be of an
incomplete view. A @nt{prefix} shall not denote a subprogram
having a formal parameter of an untagged incomplete view,
nor a return type that is an incomplete view.],Old=[]}
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[We used to disallow all dereferences of an incomplete
    type. Now we only disallow such dereferences when used as a @nt{prefix}.
    Dereferences used in other contexts do not pose a problem since normal type
    matching will preclude their use except when the full type is @lquotes@;nearby@rquotes@;
    as context (for example, as the expected type).]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[This also disallows @nt{prefix}es that are
    directly of an incomplete view. For instance, a parameter @i{P} can be
    declared of a tagged incomplete type, but we don't want to allow @i{P}'Size,
    @i{P}'Alignment, or the like, as representation values aren't known for an
    incomplete view.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[We say @lquotes@;denotes an object@rquotes
    so that prefixes that directly name an incomplete view are not covered;
    the previous rules cover such cases, and we certainly don't want to ban
    Incomp'Class.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0151-1]}
    @ChgAdded{Version=[3],Text=[As subprogram profiles now may include any
    kind of incomplete type, we also disallow passing objects of untagged
    incomplete types in subprogram calls (as the parameter passing method
    is not known as it is for tagged types) and disallow returning any
    sort of incomplete objects (since we don't know how big they are).]}
  @end{Reason}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00326-01]}
@ChgDeleted{Version=[2],Text=[@Defn{incomplete type}
An @nt{incomplete_type_declaration} declares an incomplete
type and its first subtype; the first subtype is unconstrained if
a @nt<known_discriminant_part> appears.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[If an @nt<unknown_discriminant_part> or
no @nt{discriminant_part}
appears, then the constrainedness of the first subtype doesn't matter
for any other rules or semantics, so we don't bother defining it.
The case with a @nt<known_discriminant_part> is the only case in which
a constraint could later be given in a @nt{subtype_indication} naming
the incomplete type.]}
@end{Reason}
@end{StaticSem}
@begin{NotIso}
@ChgAdded{Version=[2],Noparanum=[T],Text=[@Shrink{@i<Paragraph 11 was
deleted.>}]}@Comment{This message should be deleted if the paragraphs
are ever renumbered.}
@end{NotIso}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(incomplete_type_declaration)}
The elaboration of an @nt{incomplete_type_declaration} has no effect.
@begin(Reason)
  An incomplete type has no real existence, so it doesn't need to be
  "created" in the usual sense we do for other types.
  It is roughly equivalent to a "forward;" declaration in Pascal.
  Private types are different, because they have a different
  set of characteristics from their full type.
@end(Reason)
@end{RunTime}

@begin{Notes}
@PDefn{completion legality}
Within a @nt<declarative_part>, an @nt<incomplete_type_declaration>
and a corresponding @nt<full_type_declaration> cannot be separated
by an intervening body.
This is because a type has to
be completely defined before it is frozen, and a body freezes
all types declared prior to it in the same @nt<declarative_part>
(see @RefSecNum{Freezing Rules}).

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0151-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[A @nt{name} that denotes an object of an
incomplete view is defined to be of a limited type. Hence, the target of
an assignment statement cannot be of an incomplete view.]}

@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Example of a recursive type:)
@begin(Example)
@key(type) Cell;  --@RI[  incomplete type declaration]
@key(type) Link @key(is) @key(access) Cell;

@key(type) Cell @key(is)
   @key(record)
      Value  : Integer;
      Succ   : Link;
      Pred   : Link;
   @key(end) @key(record);

Head   : Link  := @key(new) Cell'(0, @key(null), @key(null));
Next   : Link  := Head.Succ;
@end(Example)

@i(Examples of mutually dependent access types:)
@begin(Example)
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@key(type) Person(<>);    --@RI[ incomplete type declaration]
@key(type) Car@Chg{Version=[2],New=[ @key{is tagged};],Old=[;          ]} --@RI[ incomplete type declaration]

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@key(type) Person_Name @key(is) @key(access) Person;
@key(type) Car_Name    @key(is) @key(access) @key(all) Car@Chg{Version=[2],New=['Class],Old=[]};

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@key(type) Car @key(is)@Chg{Version=[2],New=[ @key(tagged)],Old=[]}
   @key(record)
      Number  : Integer;
      Owner   : Person_Name;
   @key(end) @key(record);


@key(type) Person(Sex : Gender) @key(is)
   @key(record)
      Name     : String(1 .. 20);
      Birth    : Date;
      Age      : Integer @key(range) 0 .. 130;
      Vehicle  : Car_Name;
      @key(case) Sex @key(is)
         @key(when) M => Wife           : Person_Name(Sex => F);
         @key(when) F => Husband        : Person_Name(Sex => M);
      @key(end) @key(case);
   @key(end) @key(record);

My_Car, Your_Car, Next_Car : Car_Name := @key[new] Car;  --@RI[ see @RefSecNum{Allocators}]
George : Person_Name := @key[new] Person(M);
   ...
George.Vehicle := Your_Car;
@end(Example)

@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The @nt{full_type_declaration} that completes an
@nt{incomplete_type_declaration} may have a
@nt{known_discriminant_part} even if the
@nt{incomplete_type_declaration} does not.

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00013}
A @nt<discriminant_constraint> may be applied
to an incomplete type, even if @Chg{New=[],Old=[it ]}its completion is
deferred to the package body, because there is no
@lquotes@;dependent compatibility check@rquotes@; required any more.
Of course, the constraint can be specified only if
a @nt<known_discriminant_part> was given in the
@nt<incomplete_type_declaration>. As mentioned in the previous
paragraph, that is no longer required even when the full type
has discriminants.
@end{Extend83}

@begin{DiffWord83}
@Leading@;Dereferences producing incomplete types
were not explicitly disallowed in RM83, though
AI83-00039 indicated that it was not strictly necessary since
troublesome cases would result in Constraint_Error at run time,
since the access value would necessarily be null.
However, this introduces an undesirable implementation burden,
as illustrated by Example 4 of AI83-00039:
@begin{example}
@key[package] Pack @key[is]
    @key[type] Pri @key[is] @key[private];
@key[private]
    @key[type] Sep;
    @key[type] Pri @key[is] @key[access] Sep;
    X : Pri;
@key[end] Pack;

@key[package] @key[body] Pack @key[is] --@RI{ Could be separately compiled!}
    @key[type] Sep @key[is] ...;
    X := @key[new] Sep;
@key[end] Pack;

@key[pragma] Elaborate(Pack);
@key[private] @key[package] Pack.Child @key[is]
    I : Integer := X.@key{all}'Size; --@RI{ Legal, by AI-00039.}
@key[end] Pack.Child;
@end{example}

Generating code for the above example could be a serious implementation
burden, since it would require all aliased objects to store size dope,
and for that dope to be in the same format for all kinds of types
(or some other equivalently inefficient implementation).
On the contrary, most implementations allocate dope differently (or
not at all) for different designated subtypes.
@end{DiffWord83}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00326-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  It is now illegal to use an incomplete view (type) as the parameter or result
  of an access-to-subprogram type unless the incomplete view is completed in
  the same declaration list as the use. This was allowed in Ada 95 for
  incomplete types where the completion was deferred to the body. By
  disallowing this rare use of incomplete views, we can allow the use of
  incomplete views in many more places, which is especially valuable for
  limited views.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00326-01]}
  @ChgAdded{Version=[2],Text=[It is now illegal to use an incomplete view
  (type) in a primitive subprogram of the type unless the incomplete view is
  completed in the package specification. This was allowed in Ada 95 for
  incomplete types where the completion was deferred to the body (the use would
  have to be in an access parameter). This incompatibility was caused by the
  fix for the hole noted in @LegalityTitle above.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00326-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}Tagged incomplete types
  are new. They are allowed in parameter declarations as well as the usual
  places, as tagged types are always by-reference types (and thus there can
  be no code generation issue).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00412-01]}
  @ChgAdded{Version=[2],Text=[A @nt{subtype_declaration} can be used to
  give a new name to an incomplete view of a type. This is valuable to
  give shorter names to entities imported with a @nt{limited_with_clause}.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00326-01]}
  @ChgAdded{Version=[2],Text=[The description of incomplete types as
  @i{incomplete views} is new. Ada 95 defined these as separate types, but
  neglected to give any rules for matching them with other types. Luckily,
  implementers did the right thing anyway. This change also makes it easier to
  describe the meaning of a limited view.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0098-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}@b<Correction:>
  Fixed the definition so that an anonymous access-to-subprogram type can use an
  incomplete view in the same way that a named access-to-subprogram type can.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0151-1]}
  @ChgAdded{Version=[3],Text=[Incomplete types now can be used
  in subprogram declarations. The type has to be complete before any calls
  or the body is declared. This reduces the places where access types are
  required for types imported from limited views of packages.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0162-1]}
  @ChgAdded{Version=[3],Text=[Incomplete types now can be
  completed by private types and private extensions. Since this can already
  happen for limited views, there is no remaining reason to disallow it for
  explicitly declared incomplete types.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0208-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Changed the
  rules of uses of dereferences of incomplete views such that it does not
  introduce an unintentional incompatibility with Ada 83 and Ada 95.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0213-1]}
  @ChgAdded{Version=[3],Text=[Incomplete types now can be used as actuals to
  formal incomplete types (see @RefSecNum{Formal Private and Derived Types}).]}
@end{DiffWord2005}


@LabeledSubClause{Operations of Access Types}

@begin{Intro}
@Redundant[The attribute Access is used to create access values
designating aliased objects and nonintrinsic subprograms.
The @lquotes@;accessibility@rquotes@; rules prevent dangling references
(in the absence of uses of certain unchecked features
@em see Section 13).]
@end{Intro}

@begin{MetaRules}
It should be possible for an access value to designate an object
declared by an object declaration, or a subcomponent thereof.
In implementation terms, this means pointing at stack-allocated and
statically allocated data structures.
However, dangling references should be prevented, primarily via
compile-time rules, so long as features like Unchecked_Access and
Unchecked_Deallocation are not used.

In order to create such access values, we require that the access type be a
general access type, that the designated object be aliased,
and that the accessibility rules be obeyed.
@end{MetaRules}

@begin{Resolution}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00235-01]}
@PDefn2{Term=[expected type], Sec=(access attribute_reference)}
For an @nt<attribute_reference> with @nt<attribute_designator>
Access (or Unchecked_Access @em see @RefSecNum(Unchecked Access Value Creation)),
the expected type shall be a single
access type@Chg{Version=[2],New=[ @i<A> such that:],Old=[@Redundant[; the
@nt<prefix> of such an @nt<attribute_reference>
is never interpreted as an @nt<implicit_dereference>].
@PDefn2{Term=[expected profile],
  Sec=(Access @nt<attribute_reference> @nt<prefix>)}
If the expected type is an access-to-subprogram type,
then the expected profile of the @nt<prefix> is the
designated profile of the access type.]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00235-01]}
@Chg{Version=[2],New=[@i{A} is an access-to-object type with designated type
@i{D} and the type of the @nt{prefix} is @i{D}'Class or is covered by @i{D}, or],Old=[]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00235-01]}
@Chg{Version=[2],New=[@i{A} is an access-to-subprogram type whose designated
profile is type conformant with that of the prefix.],Old=[]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00235-01]}
@Chg{Version=[2],New=[@Redundant[The @nt{prefix} of such an
@nt{attribute_reference} is never interpreted as an @nt{implicit_dereference}
or a parameterless @nt{function_call} (see @RefSecNum{Attributes}).]
@PDefn2{Term=[expected profile],
  Sec=(Access @nt<attribute_reference> @nt<prefix>)}
@PDefn2{Term=[expected type],
  Sec=(Access @nt<attribute_reference> @nt<prefix>)}
The designated type or profile of the expected type of the @nt{attribute_reference}
is the expected type or profile for the @nt{prefix}.],Old=[]}
@begin(Discussion)
  Saying that the expected type shall be a "single access type"
  is our "new" way of saying that the type has to be determinable from
  context using only the fact that it is an access type.
  See @RefSecNum{Literals} and @RefSecNum{The Context of Overload Resolution}.
  Specifying the expected profile only implies type conformance.
  The more stringent subtype conformance is required by a Legality
  Rule. This is the only Resolution Rule that applies to the
  @nt<name> in a @nt{prefix} of an @nt{attribute_reference}.
  In all other cases, the @nt{name} has to be resolved without
  using context. See @RefSecNum{Attributes}.

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00235-01]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[Saying @lquotes@;single access
  type@rquotes@; is a bit of a fudge. Both the context and the @nt{prefix} may
  provide both multiple types; @lquotes@;single@rquotes@; only means that a
  single, specific interpretation must remain after resolution. We say
  @lquotes@;single@rquotes@; here to trigger the @LegalityTitle of
  @RefSecNum{The Context of Overload Resolution}. The resolution of an access
  attribute is similar to that of an @nt{assignment_statement}. For example:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@key{type} Int_Ptr @key{is access all} Integer;
@key{type} Char_Ptr @key{is access all} Character;
@key{type} Float_Ptr @key{is access all} Float;],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@key{function} Zap (Val : Int_Ptr) @key{return} Float;   -- @RI[(1)]
@key{function} Zap (Val : Float_Ptr) @key{return} Float; -- @RI[(2)]
@key{function} Zop @key{return} Int_Ptr;  -- @RI[(3)]
@key{function} Zop @key{return} Char_Ptr; -- @RI[(4)]],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[Result : Float := Zap (Zop.@key{all}'Access); -- @RI[Resolves to Zap (1) and Zop (3).]],Old=[]}
@end{Example}
@end(Discussion)
@end{Resolution}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00162-01]}
@Defn{accessibility level}
@Defn2{Term=[level],Sec=(accessibility)}
@Defn2{Term=[deeper],Sec=(accessibility level)}
@Defn2{Term=[depth],Sec=(accessibility level)}
@Defn2{Term=[dangling references],Sec=(prevention via accessibility rules)}
@Defn{lifetime}
@Redundant[The accessibility rules,
which prevent dangling references,
are written in terms of @i{accessibility levels},
which reflect the run-time nesting of @i{masters}. As explained in
@RefSecNum{Completion and Finalization},
a master is the execution of a
@Chg{Version=[2],New=[certain construct, such as],Old=[,@nt{task_body}, a @nt{block_statement},]}
a @nt{subprogram_body}@Chg{Version=[2],New=[],Old=[, an @nt{entry_body}, or an @nt{accept_statement}]}.
An accessibility level is @i{deeper than} another if it is more
deeply nested at run time.
For example, an object declared local to a called subprogram has a deeper
accessibility level than an object declared local to the calling
subprogram.
The accessibility rules for access types require that the
accessibility level of an object designated by an access value be no
deeper than that of the access type.
This ensures that the object will live at least as long as the access
type, which in turn ensures that the access value cannot later
designate an object that no longer exists.
The Unchecked_Access attribute may be used to circumvent the
accessibility rules.]

@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
@ChgAdded{Version=[3],Text=[The Unchecked_Access attribute acts as if the
object was declared at library-level; this applies even when it is used as the
value of anonymous access type.
See @RefSecNum{Unchecked Access Value Creation}.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Clause @RefSecNum{Operations of Access Types},
home of the accessibility rules, is informally known
as the @ldquote@;Heart of Darkness@rdquote amongst the maintainers of Ada.
Woe unto all who enter here (well, at least unto anyone that needs to understand
any of these rules).
@Defn{Heart of Darkness}@IndexSeeAlso{Term=[accessibility rules],See=[Heart of Darkness]}]}
@end{Discussion}

@Defn{statically deeper}
@Defn2{Term=[deeper],Sec=(statically)}
@Redundant[A given accessibility level is said to be @i{statically
deeper} than another if the given level is known at compile time (as
defined below) to be deeper than the other for all possible executions.
In most cases, accessibility is enforced at compile time by
@LegalityTitle.
Run-time accessibility checks are also used,
since the @LegalityTitle do not cover
certain cases involving access parameters and generic packages.]

@Leading@keepnext@;Each master, and each entity and view created by it,
has an accessibility level:
@begin{Itemize}
The accessibility level of a given master is deeper than
that of each dynamically enclosing master,
and deeper than that of each master upon which the task executing the
given master directly depends
(see @RefSecNum{Task Dependence - Termination of Tasks}).

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00162-01],ARef=[AI95-00416-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0235-1]}
An entity or view @Chg{Version=[2],New=[defined],Old=[created]} by a
declaration@Chg{Version=[2],New=[ and created as part of its elaboration],Old=[]}
has the same accessibility level
as the innermost @Chg{Version=[2],New=[],Old=[enclosing ]}master
@Chg{Version=[2],New=[of the declaration ],Old=[]}except in the
cases of renaming and derived access types described below.
@Chg{Version=[3],New=[Other than for an explicitly aliased parameter, a
formal],Old=[A]} parameter of a
@Chg{Version=[3],New=[callable entity],Old=[master]} has the same
accessibility level as the master@Chg{Version=[3],New=[ representing the
invocation of the entity],Old=[]}.
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[This rule defines the @lquotes@;normal@rquotes
  accessibility of entities. In the absence of special rules below, we intend
  for this rule to apply.]}
@end{Reason}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[This rule defines the accessibility of all
  named access types, as well as the accessibility level of all anonymous
  access types other than those for access parameters and access discriminants.
  Special rules exist for the accessibility level of such anonymous types.
  Components, stand-alone objects, and function results whose
  (anonymous) type is defined by an @nt{access_definition} have
  accessibility levels corresponding to named access types defined at
  the same point.]}
@end{Discussion}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01]}
  @ChgAdded{Version=[2],Text=[Because accessibility level is determined
  by where the @nt{access_definition} is elaborated, for a type extension,
  the anonymous access types of components (other than access discriminants)
  inherited from the parent have the same accessibility as they did in the
  parent; those in the extension part have the accessibility
  determined by the scope where the type extension is declared.
  Similarly, the types of the nondiscriminant access components
  of a derived untagged type have the same accessibility as they
  did in the parent.]}
@end{Ramification}
@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0235-1]}
  @ChgAdded{Version=[3],Text=[We use "invocation of" in the parameter case
  as a master is formally an execution of something. But we mean this to be
  interpreted statically (for instance, as the body of the subprogram) for the
  purposes of computing "statically deeper than" (see below).]}
@end{Honest}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0235-1]}
  @ChgAdded{Version=[3],Text=[Note that accessibility can differ depending on
  the view of an object (for both static and dynamic accessibility). For
  instance, the accessibility level of a formal parameter may be different than
  the accessibility level of the corresponding actual parameter. This occurs
  in other cases as well.]}
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0235-1]}
  @ChgAdded{Version=[3],Text=[We define the (dynamic) accessibility of formal
  parameters in order that it does not depend on the parameter passing model
  (by-reference or by-copy) as that is implementation defined. Otherwise, there
  would be a portability issue.]}
@end{Reason}

The accessibility level of
a view of an object or subprogram defined by a @nt{renaming_declaration}
is the same as that of
the renamed view.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
The accessibility level of
a view conversion@Chg{Version=[2],New=[, @nt{qualified_expression}, or
parenthesized expression,],Old=[]}
is the same as that of
the operand.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0188-1]}
@ChgAdded{Version=[3],Text=[The accessibility level of a
@nt{conditional_expression} is the accessibility level of the evaluated
@Syni{dependent_}@nt{expression}.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02],ARef=[AI95-00416-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0234-1]}
@Chg{Version=[2],New=[The],Old=[For a function whose result type is a
return-by-reference type,
the accessibility level of the result object is the same as
that of the master that elaborated the function body. For
any other function, the]} accessibility level of @Chg{Version=[2],New=[an
@nt{aggregate} @Chg{Version=[3],New=[],Old=[or the result of a function call @Redundant[(or equivalent use
of an operator)] ]}that is
used (in its entirety) to directly initialize part of an],Old=[the result]}
object is that of the
@Chg{Version=[2],New=[object being initialized. In other contexts, the
accessibility level of an @nt{aggregate} @Chg{Version=[3],New=[],Old=[or the
result of a function call ]}is that of the innermost
master that evaluates the @nt{aggregate}@Chg{Version=[3],New=[],Old=[ or
function call]}],Old=[execution of the called function]}.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0234-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The accessibility level of the result
of a function call is that of the @i<master of the function call>, which is
determined by the point of call as follows:@Defn{master of a call}@Defn2{Term=[call],Sec=[master of]}@Defn2{Term=[function call],Sec=[master of]}]}

@begin{InnerItemize}

  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[If the result is used (in its entirety) to
  directly initialize part of an object, the master is that of the object being
  initialized. In the case where the initialized object is a coextension
  (see below) that becomes a coextension of another object, the master is that
  of the eventual object to which the coextension will be transferred.]}

  @begin{Honest}
    @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
    @ChgAdded{Version=[2],Text=[The first sentence is talking about a static
    use of the entire return object @em a slice that happens to be the entire
    return object doesn't count. On the other hand, this is intended to allow
    parentheses and @nt{qualified_expression}s.]}
  @end{Honest}
  @begin{Ramification}
    @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
    @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0234-1]}
    @ChgAdded{Version=[2],Text=[If the function is used as a @nt{prefix},
    @Chg{Version=[3],New=[this bullet does not apply],Old=[the second sentence
    applies]}. Similarly, an @nt{assignment_statement} is not an initialization
    of an object, so @Chg{Version=[3],New=[this bullet does not apply],Old=[the second sentence
    applies]}.]}
  @end{Ramification}

  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[If the result is of an anonymous access type and
  is the operand of an explicit conversion, the master is that of the target
  type of the conversion;]}

  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[If the result is of an anonymous access type and
  defines an access discriminant, the master is the same as that for an object
  created by an anonymous @nt{allocator} that defines an access discriminant
  (even if the access result is of an access-to-subprogram type).]}

  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[If the call itself defines the result of a
  function to which one of the above rules applies, these rules are applied
  recursively;]}

  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[In other cases, the master of the call is that of
  the innermost master that evaluates the function call.]}

  @begin{Ramification}
    @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00318-02],ARef=[AI95-00416-01]}
    @ChgAdded{Version=[2],Text=[The @lquotes@;innermost master which evaluated
    the function call@rquotes@; does not include the function call itself (which
    might be a master).]}

    @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00318-02],ARef=[AI95-00416-01]}
    @ChgAdded{Version=[2],Text=[We really mean the innermost master here,
    which could be a very short lifetime. Consider a function call used as
    a parameter of a procedure call. In this case the innermost master which
    evaluated the function call is the procedure call.]}
  @end{Ramification}

@end{InnerItemize}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0234-1]}
  @ChgAdded{Version=[3],Text=[These rules do not mention whether the result
  object is built-in-place (see @RefSecNum{Assignment and Finalization}). In
  particular, in the case where building in place is optional, the choice
  whether or not to build-in-place has no effect on masters, lifetimes, or
  accessibility.]}
@end{Ramification}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0234-1]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[There are several cases where the
  implementation may have to pass in the accessibility level of the result
  object on a call, to support later rules where the accessibility level comes
  from the master of the call:]}

  @begin{Itemize}
  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[when the function result may have a part with
    access discriminants;]}

  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[when the function result type is an anonymous
    access type;]}

  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[when the function result is built-in-place;]}

  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[when the function has an explicitly
    aliased parameter.]}
  @end{Itemize}

  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[In particular, this implies passing a level
  parameter when the result type is class-wide, since descendants may add access
  discriminants. For most implementations this will mean that functions with
  controlling results will also need a level parameter.]}
@end{ImplNote}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0284-1]}
@ChgAdded{Version=[3],NoPrefix=[T],Text=[In the case of a call to a function
whose result type is an anonymous access type, the accessibility level of the
type of the result of the function call is also determined by the point of call
as described above.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00416-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}@ChgNote{Just because the paragraph number is changed}
@ChgAdded{Version=[2],Text=[Within a return statement, the accessibility level
of the return object is that of the execution of the return statement. If the
return statement completes normally by returning from the
function, then prior to leaving the function, the accessibility level
of the return object changes to be a level determined by the point
of call, as does the level of any coextensions (see below) of the
return object.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We define the accessibility level of the return
  object during the return statement to be that of the return statement itself
  so that the object may be designated by objects local to the return
  statement, but not by objects outside the return statement. In addition, the
  intent is that the return object gets finalized if the return statement ends
  without actually returning (for example, due to propagating an exception, or
  a goto). For a normal return, of course, no finalization is done before
  returning.]}
@end{Reason}

The accessibility level of
a derived access type
is the same as that of
its ultimate ancestor.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01]}
@ChgAdded{Version=[2],Text=[The accessibility level of the anonymous access
type defined by an @nt{access_definition} of an
@nt{object_renaming_declaration} is the same as that of the renamed view.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01],ARef=[AI95-00416-01]}
@ChgNote{Use ChgAdded below to get conditional Leading}
@ChgAdded{Version=[2],Type=[Leading],Text=[]}The accessibility level
of the anonymous access type of an access discriminant
@Chg{Version=[2],New=[in the @nt{subtype_indication} or
@nt{qualified_expression} of an @nt{allocator}, or in the @nt{expression} or
@nt{return_@!subtype_@!indication} of a return statement is determined as
follows:],Old=[is the same as that of the containing object or associated
constrained subtype.]}

@begin{InnerItemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[If the value of the access discriminant is
determined by a @nt{discriminant_association} in a @nt{subtype_indication},
the accessibility level of the object or subprogram designated by
the associated value (or library level if the value is null);]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[This deals with the following cases,
    when they occur in the context of an @nt{allocator} or return statement:]}
  @begin{InnerItemize}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[An @nt{extension_aggregate} where the
    @nt{ancestor_part} is a @nt{subtype_mark} denoting a constrained subtype;]}

    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[An uninitialized @nt{allocator} where the
    @nt{subtype_indication} defines a constrained subtype;]}

    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[A discriminant of an object with a constrained
    nominal subtype, including constrained components, the result of calling
    a function with a constrained result subtype, the dereference of an
    access-to-constrained subtype, etc.]}
  @end{InnerItemize}
@end{Discussion}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0281-1]}
  @ChgAdded{Version=[3],Text=[The @nt{subtype_indication} mentioned in this
  bullet is not necessarily the one given in the @nt{allocator} or
  return statement that is determining the accessibility level; the constrained
  subtype might have been defined in an earlier declaration (as a named
  subtype).]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[3],Text=[If the value for this rule and the next one is
  derived from an Unchecked_Access attribute, the accessibility is library-level
  no matter what the accessibility level of the object is (see
  @RefSecNum{Unchecked Access Value Creation}).]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0234-1]}
@ChgAdded{Version=[3],Text=[If the value of the access discriminant is
determined by a @nt{default_expression} in the declaration of the discriminant,
the level of the object or subprogram designated by the associated value (or
library level if null);]}
@begin{Discussion}
@ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[This covers the case of an unconstrained
  subcomponent of a limited type with defaulted access discriminants.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0004-1]}
@ChgAdded{Version=[2],Text=[If the value of the access discriminant is
determined by a @Chg{Version=[3],New=[@nt{record_component_association}],
Old=[@ntf{component_association}]} in an @nt{aggregate}, the
accessibility level of the object or subprogram designated by the associated
value (or library level if the value is null);]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[In this bullet, the @nt{aggregate} has to occur
  in the context of an @nt{allocator} or return statement, while the
  @nt{subtype_indication} of the previous bullet can occur anywhere
  (it doesn't have to be directly given in the @nt{allocator} or
  return statement).]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}@ChgNote{Just because the paragraph number is changed}
@ChgAdded{Version=[2],Text=[In other cases, where the value of the access
discriminant is determined by an object
with an unconstrained nominal subtype, the accessibility level of the object.]}

@end{InnerItemize}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[In other words, if you know the value of the
  discriminant for an @nt{allocator} or return statement from a discriminant
  constraint or an @nt{aggregate} component
  association, then that determines the accessibility level; if you don't know
  it, then it is based on the object itself.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00416-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}@ChgNote{Just because the paragraph number is changed}
@ChgAdded{Version=[2],Text=[The accessibility level of the anonymous access
type of an access discriminant in any other context is that of the
enclosing object.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00162-01],ARef=[AI95-00254-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0270-1]}
The accessibility level of
the anonymous access type of an access
parameter@Chg{Version=[2],New=[ specifying an access-to-object type],Old=[]}
is the same as that of
the view designated by the actual@Chg{Version=[3],New=[ (or library-level
if the actual is null)],Old=[]}.@Chg{Version=[2],New=[],Old=[ If the
actual is an @nt{allocator},
this is the accessibility level of the execution of the called
subprogram.]}
@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
@ChgAdded{Version=[3],Text=[If the value of the actual is derived from an
Unchecked_Access attribute, the accessibility is always library-level (see
@RefSecNum{Unchecked Access Value Creation}).]}
@end{Ramification}


@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00254-01]}
@ChgAdded{Version=[2],Text=[The accessibility level of the anonymous access type
of an access parameter specifying an access-to-subprogram type is
deeper than that of any master; all such
anonymous access types have this same level.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@Defn{downward closure}
@Defn2{Term=[closure],Sec=(downward)}
These represent @lquotes@;downward closures@rquotes@; and
thus require passing of static links or global display information (along
with generic sharing information if the implementation does sharing) along
with the address of the subprogram. We must prevent conversions of these to
types with @lquotes@;normal@rquotes@; accessibility, as those typically don't
include the extra information needed to make a call.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0148-1],ARef=[AI05-0240-1]}
@ChgAdded{Version=[3],Text=[The accessibility level of the type of a stand-alone
object of an anonymous access-to-object type is the same as the accessibility
level of the type of the access value most recently assigned to the
object@Redundant[; accessibility checks ensure that this
is never deeper than that of the declaration of the stand-alone object].]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0142-4],ARef=[AI05-0240-1]}
@ChgAdded{Version=[3],Text=[The accessibility level of an explicitly aliased
(see @RefSecNum{Subprogram Declarations}) formal parameter in a function body is
determined by the point of call; it is the same level that the return object
ultimately will have.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0051-1],ARef=[AI05-0253-1]}
@ChgNote{Use ChgAdded below to get conditional Leading}
@ChgAdded{Version=[2],Type=[Leading],Text=[]}
The accessibility level of an object created by an @nt{allocator}
is the same as that of
the access type@Chg{Version=[2],New=[, except for an @nt{allocator} of an
anonymous access type @Chg{Version=[3],New=[(an @i<anonymous allocator>) in
@Defn{anonymous allocator}certain contexts, as follows: For an anonymous
allocator that defines the result of a function with an access result, the
accessibility level is determined as though the @nt{allocator} were in place
of the call of the function; in the special case of a call that is the operand of a
type conversion, the level is that of the target access type of the
conversion],Old=[that defines the value of an access parameter or an access
discriminant]}. For an @Chg{Version=[3],New=[anonymous
allocator],Old=[@nt{allocator}]} defining the value of an access parameter, the
accessibility level is that of the innermost master of the call.
@Chg{Version=[3],New=[For an anonymous allocator whose type is that of a
stand-alone object of an anonymous access-to-object type, the accessibility
level is that of the declaration of the stand-alone object. ],Old=[]}For one
defining an access discriminant, the accessibility level is determined as
follows:],Old=[.]}

@begin{InnerItemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00416-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0024-1]}
@ChgAdded{Version=[2],Text=[for an @nt{allocator} used to define the
@Chg{Version=[3],New=[discriminant of an object,
the level of the object],Old=[constraint in a
@nt{subtype_declaration}, the level of the @nt{subtype_declaration}]};]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00416-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0024-1]}
@ChgAdded{Version=[2],Text=[for an @nt{allocator} used to define the constraint
in a @Chg{Version=[3],New=[@nt{subtype_indication} in any other context, the
level of the master that elaborates the
@nt{subtype_indication}.],Old=[@nt{component_definition}, the level of the
enclosing type;]}]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00416-01]}
@ChgRef{Version=[3],Kind=[DeletedAdded],ARef=[AI05-0024-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[for an @nt{allocator}
used to define the discriminant of an object, the level of the object.]}]}

@end{InnerItemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00416-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0024-1],ARef=[AI05-0066-1]}
@ChgAdded{Version=[2],NoPrefix=[T],Text=[@Defn2{Term=[coextension],Sec=(of an object)}
In @Chg{Version=[3],New=[the first],Old=[this last]} case, the allocated object
is said to be a @i{coextension} of the object whose discriminant designates it,
as well as of any object of which the discriminated object is itself a
coextension or subcomponent.@Chg{Version=[3],New=[ If the allocated object is
a coextension of an
anonymous object representing the result of an aggregate or function
call that is used (in its entirety) to directly initialize a part of
an object, after the result is assigned, the coextension becomes a
coextension of the object being initialized and is no longer
considered a coextension of the anonymous object.],Old=[]}
All coextensions of an object @Chg{Version=[3],New=[@Redundant[(which have not
thus been transfered by such an initialization)] ],Old=[]}are finalized when
the object is finalized (see @RefSecNum{Completion and Finalization}).]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[The rules of access discriminants are such that
  when the space for an object with a coextension is reclaimed, the space for
  the coextensions can be reclaimed. Hence, there is implementation advice (see
  13.11) that an object and its coextensions all be allocated from the same
  storage pool (or stack frame, in the case of a declared object).]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0051-1]}
@ChgAdded{Version=[3],Text=[Within a return statement, the accessibility level
of the anonymous access type of an access result is that of the master of the
call.]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0014-1]}
The accessibility level of a view of an object or subprogram
@Chg{Version=[3],New=[designated by],Old=[denoted by a dereference of]} an
access value is the same as that of the access type.
@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1],ARef=[AI05-0014-1]}
@ChgAdded{Version=[3],Text=[This rule applies even when no dereference exists,
for example when an access value is passed as an access parameter.
This rule ensures that implementations are not required to include dynamic
accessibility values with all access values.]}
@end{Discussion}

The accessibility level of
a component, protected subprogram, or entry
of (a view of) a composite object
is the same as that of
(the view of) the composite object.
@end{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00416-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[In the above rules, the operand of a view
conversion, parenthesized
expression or @nt{qualified_expression} is considered to be used in a context
if the view conversion, parenthesized expression or @nt{qualified_expression}
itself is used in that context.@Chg{Version=[3],New=[
Similarly, a @SynI{dependent_}@nt{expression} of a @nt{conditional_expression}
is considered to be used in a context if the @nt{conditional_expression} itself
is used in that context.],Old=[]}]}

@begin{WideAbove}
@Leading@Defn{statically deeper}
@Defn2{Term=[deeper],Sec=(statically)}
One accessibility level is defined to be
@i{statically deeper} than another in the following cases:
@end{WideAbove}
@begin{Itemize}
For a master that is statically nested within another master,
the accessibility level of the inner master is statically deeper than
that of the outer master.
@begin{Honest}
Strictly speaking, this should talk about the @i{constructs}
(such as @ntf{subprogram_bodies})
being statically nested within one another;
the masters are really the @i{executions} of those constructs.
@end{Honest}
@begin{Honest}
If a given accessibility level is statically deeper than another,
then each level defined to be the same as the given
level is statically deeper than each level defined to be the same as
the other level.
@end{Honest}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00254-01]}
@ChgAdded{Version=[2],Text=[The accessibility level of the anonymous access type
of an access parameter specifying an access-to-subprogram type is
statically deeper than that of any master; all such
anonymous access types have this same level.]}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This rule means that it is illegal to convert an
access parameter specifying an access to subprogram to a
named access to subprogram type, but it is allowed to pass such an access
parameter to another access parameter (the implicit conversion's accessibility
will succeed).]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00254-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0082-1]}
The statically deeper relationship does not apply to the accessibility
level of the anonymous type of an access parameter@Chg{Version=[2],
New=[ specifying an access-to-object type],Old=[]}@Chg{Version=[3],
New=[ nor does it apply to a descendant of a generic formal type],Old=[]};
that is, such an accessibility level is not considered to be statically
deeper, nor statically shallower, than any other.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0148-1]}
@ChgAdded{Version=[3],Text=[The statically deeper relationship does not apply to
the accessibility level of the type of a stand-alone object of an anonymous
access-to-object type; that is, such an accessibility level is not considered to
be statically deeper, nor statically shallower, than any other.]}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[In these cases, we use dynamic accessibility
checks.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0142-4],ARef=[AI05-0235-1]}
@ChgAdded{Version=[3],Text=[Inside a return statement that applies to a function
@i<F>, when determining whether the accessibility level of an explicitly aliased
parameter of @i<F> is statically deeper than the level of the return object of
@i<F>, the level of the return object is considered to be the same as that of
the level of the explicitly aliased parameter; for statically comparing with the
level of other entities, an explicitly aliased parameter of @i<F> is considered to
have the accessibility level of the body of @i<F>.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0051-1],ARef=[AI05-0234-1],ARef=[AI05-0235-1]}
@ChgAdded{Version=[3],Text=[For determining whether a level is statically deeper
than the level of the anonymous access type of an access result of a function,
when within a return statement that applies to the function, the level
of the master of the call is presumed to be the same as that of the level
of the master that elaborated the function body.]}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0235-1]}
  @ChgAdded{Version=[3],Text=[This rule has no effect if the previous bullet
  also applies (that is, the @ldquote@;a level@rdquote is of
  an explicitly aliased parameter).]}
@end{Honest}



@Redundant[For determining whether one level is statically deeper than another
when within a generic package body, the generic package is presumed to be
instantiated at the same level as where it was declared;
run-time checks are needed in the case of more deeply nested instantiations.]
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0082-1]}
  @ChgAdded{Version=[3],Text=[A generic package does not introduce a new master,
  so it has the static level of its declaration; the rest follows from the
  other @ldquote@;statically deeper@rdquote rules.]}
@end{TheProof}

For determining whether one level is statically deeper than another when
within the declarative region of a @nt{type_declaration},
the current instance of the type is presumed to be an object created at
a deeper level than that of the type.
@begin{Ramification}
  In other words, the rules are checked at compile time of the
  @nt{type_declaration}, in an assume-the-worst manner.
@end{Ramification}
@end{Itemize}

@Defn{library level}
@Defn2{Term=[level],Sec=(library)}
The accessibility level of all library units is called the
@i{library level};
a library-level declaration or entity is one whose accessibility level
is the library level.
@begin{Ramification}
  @nt{Library_unit_declaration}s are library level.
  Nested declarations are library level if they are nested only within
  packages (possibly more than one),
  and not within subprograms, tasks, etc.
@end{Ramification}
@begin{Honest}
  @ChgRef{Version=[2],Kind=[Revised]}
  @Leading@;The definition of the accessibility level of the anonymous type
  of an access parameter@Chg{Version=[2],New=[ specifying an access-to-object
  type],Old=[]} cheats a bit, since it refers to the view designated
  by the actual, but access values designate objects, not views of objects.
  What we really mean is the view that @lquotes@;would be@rquotes@; denoted by an
  expression @lquotes@;X.@key[all]@rquotes@;, where X is the actual, even though such
  an expression is a figment of our imagination.
  The definition is intended to be equivalent to the
  following more verbose version:
  The accessibility level of the anonymous type of an access parameter is
  as follows:
  @begin{Itemize}
  if the actual is
  an expression of a named access type @em
  the accessibility level of that type;

  if the actual is
  an @nt{allocator} @em
  the accessibility level of the execution of the called
  subprogram;

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
  if the actual is
  a reference to the Access attribute @em
  the accessibility level of the view denoted by the @Chg{New=[@nt{prefix}],Old=[prefix]};

  if the actual is
  a reference to the Unchecked_Access attribute @em
  library accessibility level;

  if the actual is
  an access parameter @em
  the accessibility level of its type.
  @end{Itemize}

  Note that the @nt{allocator} case is explicitly mentioned in the RM95,
  because otherwise the definition would be circular:
  the level of the anonymous type is that of the view designated by the
  actual, which is that of the access type.
@end{Honest}
@begin{Discussion}
  A deeper accessibility level implies a shorter maximum lifetime.
  Hence, when a rule requires X to have a level that is
  @lquotes@;not deeper than@rquotes@; Y's level,
  this requires that X has a lifetime at least as long as Y.
  (We say @lquotes@;maximum lifetime@rquotes@; here, because the accessibility
  level really represents an upper bound on the lifetime;
  an object created by an @nt{allocator} can have its lifetime
  prematurely ended by an instance of Unchecked_Deallocation.)

  Package elaborations are not masters, and are therefore
  invisible to the accessibility rules:
  an object declared immediately within a package has the
  same accessibility level as an object declared immediately within the
  declarative region containing the package.
  This is true even in the body of a package;
  it jibes with the fact that objects declared in a
  @nt{package_body} live as long as objects declared
  outside the package,
  even though the body objects are not visible outside the package.

  Note that the level of the @i{view} denoted by X.@key[all]
  can be different from the level of the @i{object} denoted by X.@key[all].
  The former is determined by the type of X;
  the latter is determined either by the type of the @nt{allocator},
  or by the master in which the object was declared.
  The former is used in several @LegalityTitle and run-time checks;
  the latter is used to define when X.@key[all] gets finalized.
  The level of a view reflects what we can conservatively @lquotes@;know@rquotes@;
  about the object of that view;
  for example, due to @nt{type_conversion}s,
  an access value might designate an object that was allocated by an
  @nt{allocator} for a different access type.

  Similarly, the level of the view denoted by X.@key[all].Comp
  can be different from the level of the object denoted by X.@key[all].Comp.

  If Y is statically deeper than X,
  this implies that Y will be (dynamically) deeper than X in all
  possible executions.

  @Leading@;Most accessibility checking is done at compile time;
  the rules are stated in terms of @lquotes@;statically deeper than@rquotes@;.
  The exceptions are:
  @begin{Itemize}
  @ChgRef{Version=[2],Kind=[Revised]}
  Checks involving access parameters@Chg{Version=[2],New=[ of an
  access-to-object type],Old=[]}. The fact that @lquotes@;statically deeper
  than@rquotes@; is not defined for the anonymous access type of an access
  parameter implies that any rule saying @lquotes@;shall not be statically
  deeper than@rquotes@; does not apply to such a type, nor to anything defined
  to have @lquotes@;the same@rquotes@; level as such a type.

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0082-1]}
  @ChgAdded{Version=[3],Text=[Checks involving generic formal types
  and their descendants. This is because the actual type can be more
  or less deeply nested than the generic unit. Note that this only
  applies to the generic unit itself, and not to the instance. Any
  static checks needed in the instance will be performed. Any other
  checks (such as those in the generic body) will require a run-time
  check of some sort (although implementations that macro-expand
  generics can determine the result of the check when the generic
  is expanded).]}

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0082-1]}
  Checks involving @Chg{Version=[3],New=[other ],Old=[]}entities and
  views within generic packages. This is because an instantiation can
  be at a level that is more deeply nested than the generic package itself.
  In implementations that use a macro-expansion model of generics,
  these violations can be detected at macro-expansion time.
  For implementations that share generics,
  run-time code is needed to detect the error.

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02],ARef=[AI95-00344-01],ARef=[AI95-00416-01]}
  Checks during function return@Chg{Version=[2],New=[ and @nt{allocator}s,
  for nested type extensions and access discriminants],Old=[]}.
  @end{Itemize}

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  Note that run-time checks are not required
  for access discriminants@Chg{Version=[3],New=[ (except during
  function returns and @nt{allocator}s)],Old=[]}, because their accessibility is
  determined statically by the accessibility level of the enclosing object.

  @ChgRef{Version=[2],Kind=[Revised]}
  @Chg{Version=[2],New=[],Old=[This ]}The accessibility level of the result
  object of a function reflects
  the time when that object will be finalized;
  we don't allow pointers to the object to survive beyond that time.

  We sometimes use the terms @lquotes@;accessible@rquotes@;
  and @lquotes@;inaccessible@rquotes@; to mean that something has an
  accessibility level that is not deeper, or deeper,
  respectively, than something else.
@end{Discussion}
@begin(ImplNote)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02],ARef=[AI95-00344-01],ARef=[AI95-00416-01]}
  If an accessibility @LegalityName is satisfied,
  then the corresponding run-time check (if any)
  cannot fail (and a reasonable implementation will not
  generate any checking code)
  unless @Chg{Version=[2],New=[one of the cases requiring run-time checks mentioned
  previously is],Old=[access parameters or shared generic bodies are]} involved.

  Accessibility levels are defined in terms of the relations
  @lquotes@;the same as@rquotes@; and @lquotes@;deeper than@rquotes@;.
  To make the discussion more concrete,
  we can assign actual numbers to each level.
  Here, we assume that library-level accessibility is level 0,
  and each level defined as @lquotes@;deeper than@rquotes@; is one level deeper.
  Thus, a subprogram directly called from the environment task
  (such as the main subprogram) would be at level 1,
  and so on.

  @ChgRef{Version=[2],Kind=[Revised]}
  Accessibility is not enforced at compile time for access
  parameters@Chg{Version=[2],New=[ of an access-to-object type],Old=[]}.
  The @lquotes@;obvious@rquotes@; implementation of the run-time checks would be
  inefficient, and would involve distributed overhead;
  therefore, an efficient method is given below.
  The @lquotes@;obvious@rquotes@; implementation would be to pass
  the level of the caller at each subprogram call,
  task creation, etc.
  This level would be incremented by 1 for each
  dynamically nested master.
  An Accessibility_Check would be implemented as a simple
  comparison @em checking that X is not deeper than Y
  would involve checking that X <= Y.

  A more efficient method is based on passing @i{static}
  nesting levels
  (within constructs that correspond at run time to
  masters @em packages don't count).
  Whenever an access parameter is passed,
  an implicit extra parameter is passed with it.
  The extra parameter represents (in an indirect way)
  the accessibility level of the anonymous access type,
  and, therefore, the level of the view denoted
  by a dereference of the access parameter.
  This is analogous to the implicit @lquotes@;Constrained@rquotes@; bit associated
  with certain formal parameters of an unconstrained but definite
  composite subtype.
  In this method, we avoid distributed overhead:
  it is not necessary to pass any extra information
  to subprograms that have no access parameters.
  For anything other than an access parameter and its anonymous type,
  the static nesting level is known at compile time,
  and is defined analogously to the RM95 definition
  of accessibility level (e.g. derived access types
  get their nesting level from their parent).
  Checking @lquotes@;not deeper than@rquotes@; is a "<=" test on the levels.

  @ChgRef{Version=[2],Kind=[Revised]}
  @Leading@keepnext@;For each access parameter@Chg{Version=[2],New=[ of
  an access-to-object type],Old=[]}, the static depth passed depends on the
  actual, as follows:
  @begin{Itemize}
  If the actual is
  an expression of a named access type,
  pass the static nesting level of that type.

  If the actual is
  an @nt{allocator},
  pass the static nesting level of the caller,
  plus one.

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
  If the actual is
  a reference to the Access attribute,
  pass the level of the view denoted by the @Chg{New=[@nt{prefix}],Old=[prefix]}.

  If the actual is
  a reference to the Unchecked_Access attribute,
  pass 0 (the library accessibility level).

  @ChgRef{Version=[2],Kind=[Revised]}
  If the actual is an access parameter@Chg{Version=[2],New=[ of
  an access-to-object type],Old=[]},
  usually just pass along the level passed in.
  However, if the
  static nesting level of the formal (access) parameter is greater than
  the static nesting level of the actual (access) parameter,
  the level to
  be passed is the minimum of the static nesting level
  of the access parameter
  and the actual level passed in.
  @end{Itemize}

  @ChgRef{Version=[2],Kind=[Revised]}
  For the Accessibility_Check associated with a @nt{type_conversion}
  of an access parameter@Chg{Version=[2],New=[ of
  an access-to-object type],Old=[]} of a given subprogram to a
  named access type,
  if the target type is statically nested within the subprogram,
  do nothing; the check can't fail in this case.
  Otherwise, check that the value passed in is <= the static
  nesting depth of the target type.
  The other Accessibility_Checks are handled in a similar manner.

  This method, using statically known values most of the time,
  is efficient, and, more importantly, avoids distributed overhead.

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0148-1]}
  @ChgAdded{Version=[3],Text=[The implementation of accessibility checks for
  stand-alone objects of anonymous access-to-object types can be similar to
  that for anonymous access-to-object parameters. A static level suffices; it
  can be calculated using rules similar to those previously described for access
  parameters.]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0148-1]}
  @ChgAdded{Version=[3],Text=[One important difference between the stand-alone
  access variables and access parameters is that one can assign a local access
  parameter to a more global stand-alone access variable. Similarly, one can
  assign a more global access parameter to a more local stand-alone access
  variable.]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0148-1]}
  @ChgAdded{Version=[3],Text=[For these cases, it is important to note that the
  @ldquote@;correct@rdquote static accessibility level for an access parameter
  assigned to a stand-alone access object is the minimum of the passed in
  level and the static accessibility level of the stand-alone object itself.
  This is true since the static accessibility level passed in might be deeper
  than that of the stand-alone object, but the dynamic accessibility of the
  passed in object clearly must be shallower than the stand-alone object
  (whatever is passed in must live at least as long as the subprogram call). We
  do not need to keep a more local static level as accesses to objects
  statically deeper than the stand-alone object cannot be stored into the
  stand-alone object.]}
@end(ImplNote)
@begin{Discussion}
@Leading@keepnext@;Examples of accessibility:
@begin{Example}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}@Comment{A1 is access-to-variable, X better be a variable}
@key[package] @key[body] Lib_Unit @key[is]
    @key[type] T @key[is] @key[tagged] ...;
    @key[type] A0 @key[is] @key[access] @key[all] T;
    Global: A0 := ...;
    @key[procedure] P(X: @Chg{Version=[3],New=[@key[in out] ],Old=[]}T) @key[is]
        Y: @key[aliased] T;
        @key[type] A1 @key[is] @key[access] @key[all] T;
        Ptr0: A0 := Global; --@RI{ OK.}
        Ptr1: A1 := X'Access; --@RI{ OK.}
    @key[begin]
        Ptr1 := Y'Access; --@RI{ OK;}
        Ptr0 := A0(Ptr1); --@RI{ Illegal type conversion!}
        Ptr0 := X'Access; --@RI{ Illegal reference to Access attribute!}
        Ptr0 := Y'Access; --@RI{ Illegal reference to Access attribute!}
        Global := Ptr0; --@RI{ OK.}
    @key[end] P;
@key[end] Lib_Unit;
@end{Example}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
The above illegal statements are illegal because the accessibility
@Chg{Version=[3],New=[levels],Old=[level]} of X and Y are statically deeper
than the accessibility level of A0.
In every possible execution of any program including this library
unit, if P is called, the accessibility level of X will be
(dynamically) deeper than that of A0.
Note that the accessibility levels of X and Y are the same.

@ChgRef{Version=[2],Kind=[Revised]}
@Leading@keepnext@;Here's an example involving access
parameters@Chg{Version=[2],New=[ of an access-to-object type],Old=[]}:
@begin{Example}
@key[procedure] Main @key[is]
    @key[type] Level_1_Type @key[is] @key[access] @key[all] Integer;

    @key[procedure] P(X: @key[access] Integer) @key[is]
        @key[type] Nested_Type @key[is] @key[access] @key[all] Integer;
    @key[begin]
        ... Nested_Type(X) ... --@RI{ (1)}
        ... Level_1_Type(X) ... --@RI{ (2)}
    @key[end] P;

    @key[procedure] Q(X: @key[access] Integer) @key[is]
        @key[procedure] Nested(X: @key[access] Integer) @key[is]
        @key[begin]
            P(X);
        @key[end] Nested;
    @key[begin]
        Nested(X);
    @key[end] Q;

    @key[procedure] R @key[is]
        Level_2: @key[aliased] Integer;
    @key[begin]
        Q(Level_2'Access); --@RI{ (3)}
    @key[end] R;

    Level_1: @key[aliased] Integer;
@key[begin]
    Q(Level_1'Access); --@RI{ (4)}
    R;
@key[end] Main;
@end{Example}

The run-time Accessibility_Check at (1) can never fail,
and no code should be generated to check it.
The check at (2) will fail when called from (3),
but not when called from (4).

@Leading@;Within a @nt{type_declaration}, the rules are checked in an
assume-the-worst manner.
For example:
@begin{Example}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0298-1]}
@key[package] P @key[is]
    @key[type] Int_Ptr @key[is] @key[access] @key[all] Integer;
    @key[type] Rec(D: @key[access] Integer) @key[is] @key[limited] @key[private];
@key[private]
    @key[type] Rec_Ptr @key[is] @key[access] @key[all] Rec;
    @key[function] F(X: Rec_Ptr) @key[return] Boolean;
    @key[function] G(X: @key[access] Rec) @key[return] Boolean;
    @key[type] Rec(D: @key[access] Integer) @key[is]
        @Chg{Version=[3],New=[@key[limited] ],Old=[]}@key[record]
            C1: Int_Ptr := Int_Ptr(D); --@RI{ Illegal!}
            C2: Rec_Ptr := Rec'Access; --@RI{ Illegal!}
            C3: Boolean := F(Rec'Access); --@RI{ Illegal!}
            C4: Boolean := G(Rec'Access);
        @key[end] @key[record];
@key[end] P;
@end{Example}

C1, C2, and C3 are all illegal, because one might declare an object of
type Rec at a more deeply nested place than the declaration of the type.
C4 is legal, but the accessibility level of the object will be passed to
function G, and constraint checks within G will prevent it from
doing any evil deeds.

Note that we cannot defer the checks on C1, C2, and C3 until
compile-time of the object creation, because that would cause violation
of the privacy of private parts.
Furthermore, the problems might occur within a task or protected body,
which the compiler can't see while compiling an object creation.
@end{Discussion}

@begin{WideAbove}
@Leading@;The following attribute is defined for @PrefixType{a @nt{prefix} X that
denotes an aliased view of an object}:
@end{WideAbove}
@begin(description)
@ChgAttribute{Version=[1], Kind=[Revised], ChginAnnex=[F], Leading=[F],
  Prefix=<X>, AttrName=<Access>, Ref=[8652/0010], ARef=[AI95-00127-01],
  Text=<X'Access yields an access value that designates the object
  denoted by X. The type of X'Access is an access-to-object type,
  as determined by the expected type.
  The expected type shall be a general access type.>}
@IndexSeeAlso{Term=[Unchecked_Access attribute],See=(Access attribute)}
@EndPrefixType{}
  X shall denote an aliased view of an object@Redundant[, including possibly the
  current instance (see @RefSecNum{The Context of Overload Resolution})
  of a limited type within its definition, or a formal parameter
  or generic formal object of a tagged type].
  The view denoted by the @nt<prefix> X
  shall satisfy the following additional requirements, presuming the
  expected type for X'Access is the general access type
  @i(A)@Chg{New=[ with designated type @i(D)],Old=[]}:
@begin(itemize)
  If @i(A) is an access-to-variable type, then the view shall be a
  variable; @Redundant[on the other hand, if @i(A) is an
  access-to-constant type, the view may be either a constant
  or a variable.]
  @begin{Discussion}
    The current instance of a limited type
    is considered a variable.
  @end{discussion}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00363-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0008-1],ARef=[AI05-0041-1]}
  The view shall not be a subcomponent that
  depends on discriminants of @Chg{Version=[3],New=[an object unless
  the object is known to be constrained],Old=[a
  variable whose nominal subtype is unconstrained,
  unless this subtype is indefinite,
  or the variable is @Chg{Version=[2],New=[constrained by its initial value],
  Old=[aliased]}]}.

  @begin(Discussion)
    @comment{The following is a "fix" to keep consistent with v. 5.95;
    appearently 6.0 is different.
    @ChgRef{Version=[1],Kind=[Deleted]}
    @ChgDeleted{Version=[1],Text=[Old @b{Change}.]}}

     This restriction is intended to be similar to the restriction
     on renaming discriminant-dependent subcomponents.
  @end{Discussion}
  @begin{Reason}
    This prevents references to subcomponents that might
    disappear or move or change constraints after creating the reference.
  @end{Reason}
  @begin{ImplNote}
     @Leading@;There was some thought to making this restriction
     more stringent, roughly:
     "X shall not denote a subcomponent of a variable
     with discriminant-dependent subcomponents, if the nominal
     subtype of the variable is an unconstrained definite subtype."
     This was because in some implementations, it is not
     just the discriminant-dependent subcomponents that might
     move as the result of an assignment that changed the discriminants
     of the enclosing object. However, it was decided not to make
     this change because a reasonable implementation strategy was identified
     to avoid such problems, as follows:
     @begin(Itemize)
       Place non-discriminant-dependent components with any aliased parts
       at offsets preceding any discriminant-dependent components
       in a discriminated record type with defaulted discriminants.

       Preallocate the maximum space for unconstrained discriminated
       variables with aliased subcomponents, rather than allocating
       the initial size and moving them to a larger (heap-resident)
       place if they grow as the result of an assignment.
     @end(Itemize)

     Note that for objects of a by-reference type, it is not an error
     for a programmer to take advantage of the fact that such objects
     are passed by reference. Therefore, the above approach is also
     necessary for discriminated record types with components of
     a by-reference type.

     To make the above strategy work, it is important that
     a component of a derived type is defined to be
     discriminant-dependent if it is inherited and the parent subtype
     constraint is defined in terms of a discriminant of the derived type
     (see @RefSecNum(Discriminants)).
  @end{ImplNote}

  @ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0010],ARef=[AI95-00127-01]}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00363-01]}
  If @Chg{New=[@i(A) is a named access type and @i(D) is a tagged type],
  Old=[the designated type of @i(A) is tagged]}, then the type of the view
  shall be covered by @Chg{New=[@i(D)],Old=[the designated type]};
  @Chg{New=[if @i(A) is anonymous and @i(D) is tagged, then the type of the
  view shall be either @i(D)'Class or a type covered by
  @Chg{Version=[2],New=[@i<D>],Old=[D]};],Old=[]}
  if @Chg{New=[@i(D) is untagged],Old=[@i(A)'s designated type is not tagged]},
  then the type of the view shall be
  @Chg{New=[@i(D)],Old=[the same]},
  and @Chg{Version=[2],New=[either:],Old=[@Chg{New=[],Old=[either ]}@i(A)'s
  designated subtype shall @Chg{New=[either ],Old=[]}
  statically match the nominal subtype of the view@Chg{New=[ or be],
  Old=[, or the designated subtype shall be]} discriminated
  and unconstrained;
  @PDefn2{Term=[statically matching],Sec=(required)}]}
  @begin{InnerItemize}
    @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00363-01]}
    @ChgAdded{Version=[2],Text=[the designated subtype of @i{A} shall statically
    match the nominal subtype of the view;
    or@PDefn2{Term=[statically matching],Sec=(required)}]}

    @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00363-01]}
    @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0041-1]}
    @ChgAdded{Version=[2],Text=[@i{D} shall be discriminated in its full view
    and unconstrained in any partial view, and the designated subtype of
    @i{A} shall be unconstrained.@Chg{Version=[3],New=[ For the purposes
    of determining within a generic body whether @i{D} is unconstrained
    in any partial view, a discriminated subtype is
    considered to have a constrained partial view if it is a descendant
    of an untagged generic formal private or derived type.],Old=[]}]}
  @end{InnerItemize}
  @begin{ImplNote}
    This ensures
    that the dope for an aliased array object can always be stored contiguous
    with it, but need not be if its nominal subtype is constrained.
  @end{implnote}
  @begin{Ramification}
  @ChgRef{Version=[1],Kind=[Added],Ref=[8652/0010],ARef=[AI95-00127-01]}
  @Chg{New=[An access attribute can be used as the controlling operand in a
  dispatching call; see @RefSecNum{Dispatching Operations of Tagged Types}.],
  Old=[]}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00363-01]}
  @Chg{Version=[2],New=[This does not require that types have a partial view
  in order to allow an access attribute of an unconstrained discriminated
  object, only that any partial view that does exist is unconstrained.],Old=[]}
  @end{Ramification}

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0041-1]}
  The accessibility level of the view shall not be statically deeper
  than that of the access type @i{A}.@Chg{Version=[3],New=[],Old=[
  In addition to the places where @LegalityTitle normally apply
  (see @RefSecNum{Generic Instantiation}),
  this rule applies also in the private part of an
  instance of a generic unit.@PDefn{generic contract issue}]}
@PDefn2{Term=[accessibility rule],Sec=(Access attribute)}
  @begin(Ramification)
    In an instance body, a run-time check applies.

    @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
    If @i(A) is an anonymous@Chg{Version=[2],New=[ access-to-object type of
    an access parameter],Old=[access type]}, then the view can never have a
    deeper accessibility level than @i(A)@Chg{Version=[2],New=[. The same is
    true for an anonymous access-to-object type of an access
    discriminant],Old=[]}, except when X'Access is used to initialize
    an access discriminant of an object created by an @nt<allocator>.
    The latter case is illegal if the accessibility level of X
    is statically deeper than that of the access type of the
    @nt<allocator>; a run-time check is needed in the case where the
    initial value comes from an access parameter.@Chg{Version=[2],New=[ Other
    anonymous access-to-object types have "normal" accessibility checks.],Old=[]}
  @end(Ramification)
@end(itemize)

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0041-1]}
@ChgAdded{Version=[3],NoPrefix=[T],Text=[In addition to the places where
  @LegalityTitle normally apply
  (see @RefSecNum{Generic Instantiation}), these requirements apply
  also in the private part of an instance of a generic
  unit.@PDefn{generic contract issue}]}

  @NoPrefix@IndexCheck{Accessibility_Check}
  @Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
  A check is made that the accessibility level of X is not
  deeper than that of the access type @i(A).
  If this check fails, Program_Error is raised.
  @begin{Ramification}
    @ChgRef{Version=[2],Kind=[Revised]}
    The check is needed for access parameters @Chg{Version=[2],New=[ of
    an access-to-object type],Old=[]} and in instance bodies.

    @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0024-1]}
    @ChgAdded{Version=[3],Text=[Because there are no access parameters permitted for task
    entries, the accessibility levels are always comparable. We would have to
    switch to the terminology used in @RefSecNum{Allocators} and
    @RefSecNum{Return Statements} based on inclusion within
    masters if we relax this restriction. That might introduce unacceptable
    distributed overhead.]}
  @end{Ramification}
  @begin(ImplNote)
    @ChgRef{Version=[2],Kind=[Revised]}
    @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0148-1]}
    This check requires that some indication of lifetime is
    passed as an implicit parameter along with access parameters@Chg{Version=[2],
    New=[ of an access-to-object type],Old=[]}.@Chg{Version=[3],New=[ A similar
    indication is required for stand-alone objects of anonymous access-to-object
    types.],Old=[]}No such requirement applies to @Chg{Version=[2],New=[other
    anonymous access types],Old=[access discriminants]}, since
    the checks associated with them are all compile-time checks.
  @end(ImplNote)

  @NoPrefix@PDefn2{Term=[implicit subtype conversion],Sec=(Access attribute)}
  If the nominal subtype of X does not statically match the designated
  subtype of @i(A), a view conversion of X to the designated subtype
  is evaluated (which might raise Constraint_Error @em
  see @RefSecNum(Type Conversions))
  and the value of X'Access designates that view.
@end(description)

The following attribute is defined for @PrefixType{a @nt{prefix} P that
denotes a subprogram}:
@begin(description)
@ChgAttribute{Version=[3],Kind=[Revised],ChginAnnex=[F], Leading=[F],
  Prefix=<P>, AttrName=<Access>,ARef=[AI95-00229-01], ARef=[AI95-00254-01],ARef=[AI05-0239-1],
  Text=<P'Access yields an access value that designates the subprogram
  denoted by P.
  The type of P'Access is an access-to-subprogram type (@i(S)),
  as determined by the expected type.>}
@EndPrefixType{}
@PDefn2{Term=[accessibility rule],Sec=(Access attribute)}
  The accessibility level of P shall not be statically deeper than
  that of @i{S}.
  @PDefn{generic contract issue}
  In addition to the places where @LegalityTitle normally apply
  (see @RefSecNum{Generic Instantiation}),
  this rule applies also in the private part of an
  instance of a generic unit.
  The profile of P
  shall be @Chg{Version=[3],New=[subtype conformant],Old=[subtype-conformant]}
  with the designated profile of @i(S),
  and shall not be Intrinsic.
@Defn2{Term=[subtype conformance],Sec=(required)}
  If the subprogram denoted by P is declared within a
  generic @Chg{Version=[2],New=[unit, and the expression P'Access occurs within
  the body of that generic unit or within the body of a generic unit declared
  within the declarative region of the generic unit, then the ultimate ancestor
  of @i{S} shall be either a nonformal type declared within the generic unit
  or an anonymous access type of an access parameter.],
  Old=[body, @i{S} shall be declared within the generic body.]}

  @begin(Discussion)
    @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00229-01]}
    The part about generic bodies is worded in terms of the denoted
    subprogram, not the denoted view; this implies that renaming is
    invisible to this part of the
    rule.@Chg{Version=[2],New=[ @lquotes@;Declared within the declarative region of
    the generic@rquotes@; is referring to child and nested generic
    units.],Old=[]}This rule is partly to prevent contract model problems
    with respect to the accessibility rules,
    and partly to ease shared-generic-body implementations,
    in which a subprogram declared in an instance needs to have a
    different calling convention from other subprograms with the same
    profile.

    @Comment{The change is from AI05-0239-1, just punctuation}
    Overload resolution ensures only that the profile
    is @Chg{Version=[3],New=[type conformant],Old=[type-conformant]}.
    This rule specifies that subtype conformance is required (which also
    requires matching calling conventions).
    P cannot denote an entry because access-to-subprogram types
    never have the @i(entry) calling convention. P cannot denote
    an enumeration literal or an attribute function because
    these have intrinsic calling conventions.
  @end(Discussion)
@end(description)

@end{StaticSem}

@begin{Legality}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0188-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[An @nt{expression} is said to have
@i{distributed accessibility} if it
is@Defn{distributed accessibility}@Defn2{Term=[accessibility],Sec=[distributed]}]}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[a @nt{conditional_expression} (see
@RefSecNum{Conditional Expressions}); or]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[a view conversion, @nt{qualified_expression},
or parenthesized expression whose operand has distributed accessibility.]}
@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0188-1]}
@ChgAdded{Version=[3],Text=[The statically deeper relationship does not apply to
the accessibility level of an @nt{expression} having distributed accessibility;
that is, such an accessibility level is not considered to be statically deeper,
nor statically shallower, than any other.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0188-1]}
@ChgAdded{Version=[3],Text=[Any static accessibility requirement that is imposed
on an @nt{expression} that has distributed accessibility (or on its type) is
instead imposed on the @SynI{dependent_}@nt{expression}s of the underlying
@nt{conditional_expression}. This rule is applied recursively if a
@SynI{dependent_}@nt{expression} also has distributed accessibility.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This means that any @LegalityName requiring that
  the accessibility level of an @nt{expression} (or that of the type of an
  @nt{expression}) shall or shall not be statically deeper than some other level
  also applies, in the case where the @nt{expression} has distributed
  accessibility, to each @SynI{dependent_}@nt{expression} of the underlying
  @nt{conditional_expression}.]}
@end{Discussion}

@end{Legality}

@begin{Notes}
The Unchecked_Access attribute yields the same result as the Access
attribute for objects, but has fewer restrictions
(see @RefSecNum{Unchecked Access Value Creation}).
There are other predefined operations that yield access values:
an @nt<allocator> can be used to create an object, and return an
access value that designates it (see @RefSecNum(Allocators));
evaluating the literal @key(null) yields a null access value that designates
no entity at all (see @RefSecNum(Literals)).

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
@PDefn2{Term=[predefined operations],Sec=(of an access type)}
The predefined operations of an access type also include
the assignment operation, qualification, and membership tests.
Explicit conversion is allowed between general access types with
matching designated subtypes; explicit conversion is allowed
between access-to-subprogram types with subtype conformant profiles
(see @RefSecNum{Type Conversions}).
@PDefn{subtype conformance}
Named access types have predefined equality operators;
anonymous access types do not@Chg{Version=[2],New=[, but they can use the
predefined equality operators for @i<universal_access> ],Old=[]}(see
@RefSecNum{Relational Operators and Membership Tests}).
@begin(Reason)
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
@Chg{Version=[2],New=[Anonymous access types can use the universal access
equality operators declared in Standard, while named access types cannot
for compatibility reasons. ],Old=[]}By not
having equality operators for anonymous access types,
we eliminate the need to specify exactly where the
predefined operators for anonymous access types would be defined, as
well as the need for an implementer to insert an implicit
declaration for "=", etc. at the appropriate place in their symbol table.
Note that @Chg{Version=[2],New=[":=", ],Old=[]}'Access@Chg{Version=[2],New=[,],Old=[]} and
".@key[all]" are defined@Chg{Version=[2],New=[],Old=[, and ":=" is defined
though useless since all instances are constant. The literal
@key(null) is also defined for the
purposes of overload resolution, but is disallowed by a @LegalityTitle
of this subclause]}.
@end(Reason)

The object or subprogram designated by an access value can be named
with a dereference, either an @nt<explicit_@!dereference> or an
@nt<implicit_dereference>. See @RefSecNum{Names}.

A call through the dereference of an access-to-subprogram
value is never a dispatching call.
@begin{TheProof}
See @RefSecNum{Dispatching Operations of Tagged Types}.
@end{TheProof}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00254-01]}
@Defn{downward closure}
@Defn2{Term=[closure],Sec=(downward)}
@Chg{Version=[2],New=[The],Old=[The accessibility rules imply that it
is not possible to use the]} Access
attribute@Chg{Version=[2],New=[ for subprograms and parameters of an
anonymous access-to-subprogram type may together be used],Old=[]}
to implement @lquotes@;downward closures@rquotes@; @em that is,
to pass a more-nested subprogram as a parameter to a
less-nested subprogram, as might be @Chg{Version=[2],
New=[appropriate],Old=[desired for example]} for an
iterator abstraction@Chg{Version=[2],New=[ or numerical integration.
Downward],Old=[. Instead, downward]}
closures can @Chg{Version=[2],New=[also ],Old=[]}be implemented using
generic formal subprograms (see @RefSecNum{Formal Subprograms}).
Note that Unchecked_Access is not allowed for subprograms.

Note that using
an access-to-class-wide tagged type with a dispatching operation
is a potentially more structured alternative to using an
access-to-subprogram type.

An implementation may consider
two access-to-subprogram values to be unequal,
even though they designate the same subprogram. This might
be because one points directly to the subprogram,
while the other points to a special prologue that
performs an Elaboration_Check and then jumps to the subprogram.
See @RefSecNum(Relational Operators and Membership Tests).
@begin{Ramification}

If equality of
access-to-subprogram values is important to the logic
of a program, a reference to the Access attribute of a subprogram
should be evaluated only once
and stored in a global constant for subsequent use and equality
comparison.@end{ramification}
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Example of use of the Access attribute:}
@begin{Example}
Martha : Person_Name := @key[new] Person(F);       --@RI[ see @RefSecNum{Incomplete Type Declarations}]
Cars   : @key[array] (1..2) @key[of] @key[aliased] Car;
   ...
Martha.Vehicle := Cars(1)'Access;
George.Vehicle := Cars(2)'Access;
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
We no longer make things like 'Last and ".component"
(basic) operations of an access type that need to be "declared"
somewhere. Instead, implicit dereference
in a @nt{prefix} takes care of them all. This means that
there should never be a case when X.@key{all}'Last is legal
while X'Last is not. See AI83-00154.
@end{Extend83}

@begin{Incompatible95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
@Chg{Version=[2],New=[@Leading@;@Defn{incompatibilities with Ada 95} Aliased
variables are not necessarily constrained in Ada 2005 (see
@RefSecNum{Array Types}). Therefore, a subcomponent of an aliased variable
may disappear or change shape, and taking 'Access of such a subcomponent
thus is illegal, while the same operation would have been legal in Ada 95.
Note that most allocated objects are
still constrained by their initial value (see @RefSecNum{Allocators}), and thus
legality of 'Access didn't change for them. For example:],Old=[]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@key{type} T1 (D1 : Boolean := False) @key{is}
   @key{record}
      @key{case} D1 @key{is}
         @key{when} False =>
            C1 : @key{aliased} Integer;
         @key{when} True =>
            @key{null};
      @key{end} @key{case};
   @key{end} @key{record};
@key{type} Acc_Int @key{is access all} Integer;],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[A_T : @key{aliased} T1;
Ptr : Acc_Int := A_T.C1'Access; -- @RI[Illegal in Ada 2005, legal in Ada 95]
A_T := (D1 => True);            -- @RI[Raised Constraint_Error in Ada 95, but does not]
                                -- @RI[in Ada 2005, so Ptr would become invalid when this]
                                -- @RI[is assigned (thus Ptr is illegal).]],Old=[]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
@Chg{Version=[2],New=[If a discriminated full type has a partial view (private
type) that is constrained, we do not allow 'Access on objects to create a value
of an object of an
access-to-unconstrained type. Ada 95 allowed this attribute and various
access subtypes, requiring that the heap object be constrained and thus making
details of the implementation of the private type visible to the client of
the private type. See @RefSecNum{Allocators} for more on this topic.],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00229-01],ARef=[AI95-00254-01]}
@ChgAdded{Version=[2],Text=[@b[Amendment Correction:] Taking 'Access of a subprogram
declared in a generic unit in the body of that generic is no longer allowed.
Such references can easily be used to create dangling pointers, as
@LegalityTitle are not rechecked in instance bodies. At the same time, the
rules were loosened a bit where that is harmless, and also to allow any routine
to be passed to an access parameter of an access-to-subprogram type. The now
illegal uses of 'Access can almost always be moved to the private part of the
generic unit, where they are still legal (and rechecked upon instantiation for
possibly dangling pointers).]}
@end{Incompatible95}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0010],ARef=[AI95-00127-01]}
@ChgAdded{Version=[2],Text=[@b<Corrigendum:> @Defn{extensions to Ada 95}
Access attributes of objects of class-wide types
can be used as the controlling parameter in a dispatching calls (see
@RefSecNum{Dispatching Operations of Tagged Types}). This was an oversight in
Ada 95.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00235-01]}
@ChgAdded{Version=[2],Text=[@b[Amendment Correction:] The type of the prefix
can now be used in resolving Access attributes. This allows more uses of the
Access attribute to resolve. For example:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{type} Int_Ptr @key{is access all} Integer;
@key{type} Float_Ptr @key{is access all} Float;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{function} Zap (Val : Int_Ptr) @key{return} Float;
@key{function} Zap (Val : Float_Ptr) @key{return} Float;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Value : @key{aliased} Integer := 10;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Result1 : Float := Zap (Value'access); -- @RI[Ambiguous in Ada 95; resolves in Ada 2005.]
Result2 : Float := Zap (Int_Ptr'(Value'access)); -- @RI[Resolves in Ada 95 and Ada 2005.]]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This change is upward compatible; any expression
that does not resolve by the new rules would have failed a Legality Rule.]}
@end{Extend95}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00162-01]}
@ChgAdded{Version=[2],Text=[Adjusted the wording to reflect the fact that
expressions and function calls are masters.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01],ARef=[AI95-00254-01],ARef=[AI95-00318-02],ARef=[AI95-00385-01],ARef=[AI95-00416-01]}
@ChgAdded{Version=[2],Text=[Defined the accessibility of the various new kinds and
uses of anonymous access types.]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0008-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Simplified the description of when a discriminant-dependent component is
  allowed as the prefix of 'Access to when the object is known to be
  constrained. This fixes a confusion as to whether a subcomponent of an object
  that is not certain to be constrained can be used as a prefix of 'Access. The
  fix introduces an incompatibility, as the rule did not apply in Ada 95 if the
  prefix was a constant; but it now applies no matter what kind of object is
  involved. The incompatibility is not too bad, since most kinds of constants
  are known to be constrained.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0041-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected the checks for the
  constrainedness of the prefix of the Access attribute so that assume-the-worst
  is used in generic bodies. This may make some programs illegal, but those
  programs were at risk having objects disappear while valid access values still
  pointed at them.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0082-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}@b<Correction:>
  Eliminated the static accessibility definition for generic formal types, as
  the actual can be more or less nested than the generic itself. This allows
  programs that were illegal for Ada 95 and for Ada 2005.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0148-1],ARef=[AI05-0253-1]}
  @ChgAdded{Version=[3],Text=[Eliminate the static
  accessibility definition for stand-alone objects of anonymous access-to-object
  types. This allows such objects to be used as temporaries without causing
  accessibility problems.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0014-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected the rules so that the
  accessibility of the object designated by an access object is that of the
  access type, even when no dereference is given. The accessibility was not
  specified in the past. This correction applies to both Ada 95 and Ada 2005.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0024-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected accessibility rules for
  access discriminants so that no cases are omitted.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0051-1],ARef=[AI05-0234-1],ARef=[AI05-0235-1],ARef=[AI05-0284-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected accessibility rules for
  anonymous access return types and access discriminants in return statements.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0066-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Changed coextension rules so that
  coextensions that belong to an anonymous object are transfered to the ultimate
  object.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0142-4],ARef=[AI05-0188-1],ARef=[AI05-0235-1]}
  @ChgAdded{Version=[3],Text=[Defined the accessibility of explicitly
  aliased parameters (see @RefSecNum{Subprogram Declarations}) and
  @nt{conditional_expression}s (see @RefSecNum{Conditional Expressions}).]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0234-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Defined the term @ldquote@;master
  of the call@rdquote to simplify other wording, especially that for the
  accessibility checks associated with return statements and explicitly aliased
  parameters.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0270-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Defined the (omitted)
  accessibility level of null values when those are passed as the actual of an
  access-to-object parameter.]}
@end{DiffWord2005}



@RMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledClause{Declarative Parts}

@begin{Intro}
@Redundant[A @nt<declarative_part> contains @nt<declarative_item>s
(possibly none).]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<declarative_part>,rhs="{@Syn2{declarative_item}}"}


@Syn{lhs=<declarative_item>,rhs="
    @Syn2{basic_declarative_item} | @Syn2{body}"}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@Syn{lhs=<basic_declarative_item>,rhs="
    @Syn2{basic_declaration} | @Chg{New=[@Syn2{aspect_clause}],Old=[@Syn2{representation_clause}]} | @Syn2{use_clause}"}

@Syn{lhs=<body>,rhs="@Syn2{proper_body} | @Syn2{body_stub}"}

@Syn{lhs=<proper_body>,rhs="
    @Syn2{subprogram_body} | @Syn2{package_body} | @Syn2{task_body} | @Syn2{protected_body}"}
@end{Syntax}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00420-01]}
@ChgAdded{Version=[2],Text=[The list of @nt{declarative_item}s of a
@nt{declarative_part} is called the @i{declaration list} of the
@nt{declarative_part}.@PDefn2{Term=[declaration list],Sec=[declarative_part]}]}
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(declarative_part)}
The elaboration of a @nt{declarative_part} consists of the elaboration of
the @nt{declarative_item}s, if any, in the order in which they are given
in the @nt{declarative_part}.

@Defn{elaborated}
An elaborable construct is in the @i(elaborated) state after the
normal completion of its elaboration.
Prior to that, it is @i(not yet elaborated).
@begin{Ramification}
  The elaborated state is only important for bodies;
  certain uses of a body raise an exception if the body is not yet
  elaborated.

  Note that "prior" implies before the start of elaboration,
  as well as during elaboration.

  The use of the term "normal completion" implies that
  if the elaboration propagates an exception or is aborted, the
  declaration is not elaborated.
  RM83 missed the aborted case.
@end{Ramification}

@IndexCheck{Elaboration_Check}
For a construct that attempts to use a body,
a check (Elaboration_Check) is performed, as follows:
@begin{Itemize}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0014],ARef=[AI95-00064-01]}
For a call to a (non-protected) subprogram that has
an explicit body, a check is made that the
@Chg{New=[body],Old=[@nt{subprogram_body}]} is already elaborated.
This check and the evaluations of any actual parameters of the call
are done in an arbitrary order.@PDefn2{Term=[arbitrary order],Sec=[allowed]}
@begin{Discussion}
  AI83-00180 specifies that there is no elaboration check for
  a subprogram defined by a @nt{pragma} Interface (or equivalently,
  @nt{pragma} Import). AI83-00430 specifies that there is no elaboration check
  for an enumeration literal. AI83-00406 specifies that the evaluation
  of parameters and the elaboration check occur in an arbitrary order.
  AI83-00406 applies to generic instantiation as well (see below).

  @ChgRef{Version=[1],Kind=[Added],Ref=[8652/0014],ARef=[AI95-00064-01]}
  @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0177-1]}
  @ChgAdded{Version=[1],Text=[A subprogram can be completed by a
  renaming-as-body@Chg{Version=[3],New=[, a @nt{null_procedure_declaration}, or
  an @nt{expression_function_declaration}],Old=[]}, and we need
  to make an elaboration check on such a body, so we use
  @lquotes@;body@rquotes@; rather than @nt{subprogram_body} above.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
For a call to a protected operation of a protected type
(that has a body @em no check is performed if @Chg{Version=[3],New=[],Old=[a
@nt{pragma} Import applies to]} the protected type@Chg{Version=[3],New=[ is
imported @em see @RefSecNum{Interfacing Aspects}],Old=[]}),
a check is made that the @nt<protected_body> is already elaborated.
This check and the evaluations of any actual parameters of the call
are done in an arbitrary order.@PDefn2{Term=[arbitrary order],Sec=[allowed]}
@begin{Discussion}
  A protected type has only one elaboration @lquotes@;bit,@rquotes@; rather than
  one for each operation, because one call may result in evaluating
  the barriers of other entries, and because there are no elaborable
  declarations between the bodies of the operations. In fact,
  the elaboration of a @nt<protected_body> does not elaborate
  the enclosed bodies, since they are not considered independently
  elaborable.

  Note that there is no elaboration check when calling a task entry.
  Task entry calls are permitted even before the associated @nt<task_body>
  has been seen. Such calls are simply queued until the task is
  activated and reaches a corresponding @nt<accept_statement>.
  We considered a similar rule for protected entries @em simply
  queuing all calls until the @nt<protected_body> was seen, but felt
  it was not worth the possible implementation overhead, particularly
  given that there might be multiple instances of the protected type.
@end{Discussion}

For the activation of a task, a check is made by the
activator that the @nt{task_body} is already elaborated.
If two or more tasks are being activated together
(see @RefSecNum{Task Execution - Task Activation}),
as the result of the elaboration of
a @nt{declarative_part} or the initialization
for the object created by an allocator,
this check is done for all of them before activating any
of them.
@begin{Reason}
  As specified by AI83-00149, the check is done by the
  activator, rather than by the task itself. If it were
  done by the task itself, it would be turned into a Tasking_Error
  in the activator, and the other tasks would still be activated.
@end{Reason}

For the instantiation of a generic unit that has a body, a check is
made that this body is already elaborated.
This check and the evaluation of any @nt{explicit_generic_actual_parameter}s
of the instantiation are done in an arbitrary order.@PDefn2{Term=[arbitrary order],Sec=[allowed]}
@end{Itemize}

@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
The exception Program_Error is raised if any of these checks fails.
@end{RunTime}

@begin{Extend83}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
@Defn{extensions to Ada 83}
The syntax for @nt{declarative_part} is modified to remove the ordering
restrictions of Ada 83; that is, the distinction between
@nt{basic_declarative_item}s and @ntf{later_declarative_item}s
within @nt{declarative_part}s is removed.
This means that things like @nt{use_clause}s and
@Chg{Version=[2],New=[@nt{object_declaration}s],Old=[@ntf{variable_declaration}s]}
can be freely intermixed with things like bodies.

The syntax rule for @nt{proper_body} now allows a @nt{protected_body},
and the rules for elaboration checks now cover calls on
protected operations.
@end{Extend83}

@begin{DiffWord83}
The syntax rule for @ntf{later_declarative_item} is removed;
the syntax rule for @nt{declarative_item} is new.

RM83 defines @lquotes@;elaborated@rquotes@; and @lquotes@;not yet elaborated@rquotes@; for
@nt{declarative_item}s here, and for other things in @RefSec{Declarations}.
That's no longer necessary, since these terms are fully defined in
@RefSecNum{Declarations}.

In RM83, all uses of @nt{declarative_part} are optional
(except for the one in @nt{block_statement} with a @key(declare))
which is sort of strange, since a @nt{declarative_part} can be empty,
according to the syntax.
That is, @nt{declarative_part}s are sort of @lquotes@;doubly optional@rquotes@;.
In Ada 95, these @nt{declarative_part}s are always required
(but can still be empty).
To simplify description, we go further and say
(see @RefSec(Block Statements)) that a
@nt<block_statement> without an explicit @nt<declarative_part>
is equivalent to one with an empty one.
@end{DiffWord83}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0009],ARef=[AI95-00137-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Changed representation clauses
  to aspect clauses to reflect that they are used for more than just
  representation.]}

@ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0014],ARef=[AI95-00064-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that the elaboration
  check applies to all kinds of subprogram bodies.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00420-01]}
  @ChgAdded{Version=[2],Text=[Defined @lquotes@;declaration list@rquotes to
  avoid confusion for various rules. Other kinds of declaration list are
  defined elsewhere.]}
@end{DiffWord95}


@LabeledSubClause{Completions of Declarations}

@begin{Intro}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0014],ARef=[AI95-00064-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0177-1]}
Declarations sometimes come in two parts.
@Defn{requires a completion}
A declaration that requires a second part is said to @i(require completion).
@Defn2{Term=[completion], Sec=(compile-time concept)}
The second part is called the @i(completion) of the declaration (and of
the entity declared),
and is either another declaration, a body, or a @nt<pragma>.
@Chg{New=[A @defn<body>@i<body> is a @nt<body>,
an @nt<entry_body>,@Chg{Version=[3],New=[ a @nt{null_procedure_declaration} or an
@nt{expression_function_declaration} that completes another declaration,],Old=[]} or
a renaming-as-body
(see @RefSecNum<Subprogram Renaming Declarations>).],Old=[]}
@begin{Discussion}
@Leading@keepnext@;Throughout the RM95, there are rules about completions that
define the following:
@begin{Itemize}
Which declarations require a corresponding completion.

Which constructs can only serve as the completion
of a declaration.

Where the completion of a declaration is allowed to be.

What kinds of completions are allowed to correspond to each kind of
declaration that allows one.
@end{Itemize}

Don't confuse this compile-time concept with
the run-time concept of completion
defined in @RefSecNum{Completion and Finalization}.

Note that the declaration of a private type (if limited) can be
completed with the declaration of a task type, which is then completed
with a body.
Thus, a declaration can actually come in @i{three} parts.

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0162-1]}
@Chg{Version=[2],New=[@Chg{Version=[3],New=[An incomplete type (whether declared
in the limited view of a package or not) may be completed by a private type
declaration],Old=[In Ada 2005 the limited view of the package contains an
incomplete view of the private type]}, so we can
@Chg{Version=[3],New=[in fact ],Old=[]}have @i{four}
parts@Chg{Version=[3],New=[],Old=[ now]}.],Old=[]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[In Ada 2012, there are no language-defined pragmas
that act as completions. Pragma Import (which is obsolescent) has the effect of
setting aspect Import to True; such an aspect makes giving a completion illegal.
The wording that allows pragmas as completions was left as it is harmless and
appears in many places in this Standard.]}
@end{Discussion}
@end{Intro}

@begin{Resolution}
@Leading@keepnext@;A construct that can be a completion
is interpreted as the completion of a prior
declaration only if:
@begin(itemize)
The declaration and the completion occur immediately within
the same declarative region;

The defining name or @nt{defining_program_unit_name}
in the completion is the same as in the declaration,
or in the case of a @nt{pragma}, the @nt{pragma} applies to
the declaration;

If the declaration is overloadable, then the completion
either has a type-conformant profile,
or is a @nt{pragma}.
@Defn2{Term=[type conformance],Sec=(required)}
@end(itemize)
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
An implicit declaration shall not have a completion.
@RootDefn{requires a completion}
For any explicit declaration that is specified
to @i(require completion), there shall be a corresponding explicit
completion@Chg{Version=[3],New=[, unless the declared
entity is imported (see @RefSecNum{Interfacing Aspects})],Old=[]}.
@begin(Honest)
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00217-06]}
@ChgAdded{Version=[2],Text=[The implicit declarations occurring in a
limited view do have a completion (the explicit declaration occurring in the
full view) but that's a special case, since the implicit declarations are
actually built from the explicit ones. So they do not @i{require} a
completion, they have one by @i{fiat}.]}
@end{Honest}

@begin(Discussion)
  The implicit declarations of predefined operators are not allowed to
  have a completion.
  Enumeration literals, although they are subprograms, are not allowed to have a
  corresponding @nt{subprogram_body}.
  That's because the completion rules are described in terms of constructs
  (@nt{subprogram_declaration}s) and not entities (subprograms).
  When a completion is required, it has to be explicit;
  the implicit null @nt{package_body} that Section 7
  talks about cannot serve as the completion of a
  @nt{package_declaration} if a completion is required.
@end(Discussion)

At most one completion is allowed for a given declaration.
Additional requirements on completions appear where each kind
of completion is defined.
@begin{Ramification}
A subunit is not a completion;
the stub is.

If the completion of a declaration is also a declaration,
then @i{that} declaration might have a completion, too.
For example, a limited private type can be completed with a task type,
which can then be completed with a task body.
This is not a violation of the @lquotes@;at most one completion@rquotes@; rule.
@end{Ramification}

@Defn{completely defined}
A type is @i(completely defined) at a place that is after its
full type definition (if it has one) and after all of its
subcomponent types are completely defined.
A type shall be completely defined before it is frozen
(see @RefSecNum{Freezing Rules} and
@RefSecNum{Private Types and Private Extensions}).
@begin(Reason)
  @comment{The following is a "fix" to keep consistent with v. 5.95;
  appearently 6.0 is different.
  @ChgRef{Version=[1],Kind=[Deleted]}
  @ChgDeleted{Version=[1],Text=[Old @b{Change}.]}}

  Index types are always completely defined @em no need to mention them.
  There is no way for a completely defined type to depend on the value of
  a (still) deferred constant.
@end(Reason)
@end{Legality}

@begin{Notes}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
Completions are in principle allowed for any kind of explicit declaration.
However, for some kinds of declaration,
the only allowed completion is @Chg{Version=[3],New=[an implementation-defined
pragma], Old=[a @nt{pragma} Import]},
and implementations are not required to @Chg{Version=[3],New=[have any
such pragmas],Old=[support @nt{pragma} Import for
every kind of entity]}.
@begin(Discussion)
  @ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0229-1]}
  @ChgDeleted{Version=[3],Text=[In fact, we expect that implementations
  will @i{not} support pragma
  Import of things like types @em it's hard to even define the
  semantics of what it would mean.
  Therefore, in practice, @i{not} every explicit declaration
  can have a completion.
  In any case, if an implementation chooses to support pragma Import for,
  say, types, it can place whatever restrictions on the feature it wants
  to. For example, it might want the @nt{pragma} to be a freezing point for
  the type.]}
@end(Discussion)

There are rules that prevent premature uses of declarations that have a
corresponding completion.
The Elaboration_Checks of @RefSecNum{Declarative Parts} prevent such
uses at run time for subprograms, protected operations, tasks, and
generic units.
The rules of @RefSec{Freezing Rules}
prevent, at compile time, premature uses of other entities
such as private types and deferred constants.
@end{Notes}

@begin{DiffWord83}
This subclause is new. It is intended to cover all
kinds of completions of declarations, be they a body for a spec,
a full type for an incomplete or private type, a full constant
declaration for a deferred constant declaration, or a @nt{pragma} Import
for any kind of entity.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0014],ARef=[AI95-00064-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added a definition of @i{body},
  which is different than @nt{body} or @key{body}.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI95-0177-1]}
  @ChgAdded{Version=[3],Text=[Added null procedures and expression functions
  that are completions to the definition of @i<body>.]}
@end{DiffWord2005}
