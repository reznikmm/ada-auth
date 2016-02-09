@Part(08, Root="ada.mss")

@Comment{$Date: 2015/04/03 04:12:42 $}
@LabeledSection{Visibility Rules}

@Comment{$Source: e:\\cvsroot/ARM/Source/08.mss,v $}
@Comment{$Revision: 1.106 $}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@redundant[The rules defining the scope of declarations and the rules defining
which @nt{identifier}s, @nt{character_literal}s, and
@nt{operator_symbol}s are visible at (or from) various places in the text of
the program are described in this @Chg{Version=[3],New=[clause],Old=[section]}.
The formulation of these rules uses the notion of a declarative region.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
As explained in @Chg{Version=[3],New=[Clause],Old=[Section]}
@RefSecNum{Declarations and Types},
a declaration declares a view of an entity
and associates a defining name with that view.
The view comprises an identification of the viewed entity,
and possibly additional properties.
A usage name denotes a declaration.
It also denotes the view declared by that declaration,
and denotes the entity of that view.
Thus, two different usage names might denote two different views of
the same entity; in this case they denote the same entity.]
@begin{Honest}
In some cases, a usage name that denotes a declaration
does not denote the view declared by that declaration,
nor the entity of that view,
but instead denotes a view of the current instance of the entity,
and denotes the current instance of the entity.
This sometimes happens when the usage name occurs inside
the declarative region of the declaration.
@end{Honest}
@end{Intro}

@begin{DiffWord83}
We no longer define the term @lquotes@;basic operation;@rquotes@;
thus we no longer have to worry about the visibility of them.
Since they were essentially always visible in Ada 83, this change has
no effect. The reason for this change is that the definition in Ada
83 was confusing, and not quite correct,
and we found it difficult to fix. For example, one
wonders why an @nt{if_statement} was not a basic operation of type
Boolean. For another example, one wonders what it meant for a basic
operation to be @lquotes@;inherent in@rquotes@; something.
Finally, this fixes the problem addressed by AI83-00027/07.
@end{DiffWord83}

@LabeledClause{Declarative Region}

@begin{StaticSem}
@leading@Defn2{Term=[declarative region], Sec=(of a construct)}
For each of the following constructs,
there is a portion of the program text
called its @i{declarative region},
@Redundant[within which nested declarations can occur]:
@begin(itemize)
  any declaration, other than that of an enumeration type,
  that is not a completion @Redundant[of a previous declaration];

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0094-1]}
  @ChgAdded{Version=[4],Text=[an @nt{access_definition};]}

  a @nt{block_statement};

  a @nt{loop_statement};

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0255-1]}
  @ChgAdded{Version=[3],Text=[a @nt{quantified_expression};]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00318-02]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}@Comment{Paragraph number changed}
  @ChgAdded{Version=[2],Text=[an @nt{extended_return_statement};]}

  an @nt{accept_statement};

  an @nt{exception_handler}.
@end(itemize)

@begin{WideAbove}
@leading@;The declarative region includes the text of the construct
together with additional text determined @Redundant[(recursively)],
as follows:
@end{WideAbove}
@begin{Itemize}
If a declaration is included, so is its completion, if any.

If the declaration of a library unit
@Redundant[(including Standard @em
see @RefSecNum{Compilation Units - Library Units})] is included,
so are the declarations of any child units
@Redundant[(and their completions, by the previous rule)].
The child declarations occur after the declaration.

If a @nt{body_stub} is included,
so is the corresponding @nt{subunit}.

If a @nt{type_declaration} is included,
then so is a corresponding @nt{record_representation_clause},
if any.
@begin{Reason}
This is so that the @nt{component_declaration}s can be directly
visible in the @nt{record_representation_clause}.
@end{Reason}
@end{Itemize}

The declarative region of a declaration is also called the
@i{declarative region} of any view or entity declared by the
declaration.
@begin{Reason}
The constructs that have declarative regions
are the constructs that can have declarations nested inside them.
Nested declarations are declared in that declarative region.
The one exception is for enumeration literals;
although they are nested inside an enumeration type declaration,
they behave as if they were declared at the same level as the type.
@end{Reason}
@begin{Honest}
A declarative region does not include @nt{parent_unit_name}s.
@end{Honest}
@begin{Ramification}
A declarative region does not include @nt{context_clause}s.
@end{Ramification}

@Defn{occur immediately within}
@Defn{immediately within}
@Defn2{Term=[within],Sec=(immediately)}
@Defn{immediately enclosing}
@Defn2{Term=[enclosing],Sec=(immediately)}
A declaration occurs @i{immediately within} a declarative region if this
region is the innermost declarative region that
encloses the declaration
(the @i{immediately enclosing} declarative region),
not counting the declarative region (if any)
associated with the declaration itself.
@begin{Discussion}
Don't confuse the declarative region of a declaration
with the declarative region in which it immediately occurs.
@end{Discussion}

@redundant[@Defn{local to}
A declaration is @i{local} to a declarative region if
the declaration occurs immediately within the declarative region.]
@redundant[An entity is @i{local} to a declarative region if the entity is
declared by a declaration that is local to the declarative region.]
@begin{Ramification}
"Occurs immediately within" and "local to" are synonyms
(when referring to declarations).

Thus, @lquotes@;local to@rquotes@; applies to both declarations and entities,
whereas @lquotes@;occurs immediately within@rquotes@; only applies to declarations.
We use this term only informally; for cases where precision is required,
we use the term "occurs immediately within", since it is less likely to
cause confusion.
@end{Ramification}

@Defn{global to}
A declaration is @i{global} to a declarative region
if the declaration occurs immediately within
another declarative region that encloses the
declarative region.
An entity is @i{global} to a declarative region if the entity is
declared by a declaration that is global to the declarative region.
@end{StaticSem}

@begin{Notes}
The children of a parent library unit are inside the parent's
declarative region, even though they do not occur inside the
parent's declaration or body.
This implies that one can use (for example) "P.Q" to refer to
a child of P whose defining name is Q,
and that after "@key[use] P;" Q can refer (directly) to that child.

As explained above
and in @RefSec{Compilation Units - Library Units},
all library units are descendants of Standard,
and so are contained in the declarative region of Standard.
They are @i{not} inside the
declaration or body of Standard,
but they @i{are} inside its declarative region.

For a declarative region that comes in multiple parts,
the text of the declarative region does not contain any text that might appear
between the parts.
Thus, when a portion of a declarative region is said to extend from one
place to another in the declarative region,
the portion does not contain any text that
might appear between the parts of the declarative region.
@begin{Discussion}
It is necessary for the things that have a declarative region to
include
anything that contains declarations (except for enumeration type
declarations).
This includes any declaration that has a profile
(that is, @nt{subprogram_declaration},
@nt{subprogram_body},
@nt{entry_declaration},
@nt{subprogram_renaming_declaration},
@nt{formal_subprogram_declaration},
access-to-subprogram @nt{type_declaration}),
anything that has a @nt{discriminant_part}
(that is, various kinds of @nt{type_declaration}),
anything that has a @nt{component_list}
(that is, record @nt{type_declaration} and
record extension @nt{type_declaration}),
and finally the declarations of task and protected units and packages.
@end{Discussion}
@end{Notes}

@begin{DiffWord83}
@leading@;It was necessary to extend Ada 83's definition of declarative region
to take the following Ada 95 features into account:
@begin{Itemize}
Child library units.

Derived types/type extensions @em we need a declarative region for
inherited components and also for new components.

All the kinds of types that allow discriminants.

Protected units.

Entries that have bodies instead of accept statements.

The @nt{choice_parameter_specification} of an @nt{exception_handler}.

The formal parameters of access-to-subprogram types.

Renamings-as-body.
@end{Itemize}

Discriminated and access-to-subprogram type declarations
need a declarative region.
Enumeration type declarations cannot have one,
because you don't have to say "Color.Red" to refer to the literal Red
of Color.
For other type declarations,
it doesn't really matter whether or not there is an
associated declarative region,
so for simplicity, we give one to all types except enumeration types.

We now say that an @nt{accept_statement} has its own declarative
region, rather than being part of the declarative region of the
@nt{entry_declaration},
so that declarative regions are properly nested regions of text,
so that it makes sense to talk about "inner declarative regions,"
and "...extends to the end of a declarative region."
Inside an @nt{accept_statement}, the @nt{name} of one of the parameters
denotes the @nt{parameter_specification} of the @nt{accept_statement},
not that of the @nt{entry_declaration}. If the @nt{accept_statement} is
nested within a @nt{block_statement}, these
@nt{parameter_specification}s can hide declarations of the
@nt{block_statement}.
The semantics of such cases was unclear in RM83.
@begin{Honest}
  Unfortunately, we have the same problem for the entry name
  itself @em it should denote the @nt{accept_statement},
  but @nt{accept_statement}s are not declarations.
  They should be, and they should hide the entry from all visibility
  within themselves.
@end{Honest}

Note that we can't generalize this to @ntf{entry_bodies},
or other bodies, because the @nt{declarative_part} of a
body is not supposed to contain (explicit) homographs of
things in the declaration.
It works for @nt{accept_statement}s only because an
@nt{accept_statement} does not have a @nt{declarative_part}.

To avoid confusion,
we use the term @lquotes@;local to@rquotes@; only informally in Ada 95.
Even RM83 used the term incorrectly (see, for example, RM83-12.3(13)).

In Ada 83, (root) library units were inside Standard;
it was not clear whether the declaration or body of Standard
was meant.
In Ada 95, they are children of Standard,
and so occur immediately within Standard's declarative
region, but not within either the declaration or the body.
(See RM83-8.6(2) and RM83-10.1.1(5).)
@end{DiffWord83}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00318-02]}
@ChgAdded{Version=[2],Text=[@nt{Extended_return_statement}
(see @RefSecNum{Return Statements}) is added to the list
of constructs that have a declarative region.]}
@end{DiffWord95}

@begin{Extend2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0094-1]}
  @ChgAdded{Version=[4],Text=[@Defn{extensions to Ada 2012}@b<Corrigendum:>
  @nt{access_definition} is added to the list of constructs that have
  a declarative region. This allows parameter names declared in anonymous
  access type subprogram types to be the same as other names declared
  outside. For instance:]}
@begin{Example}
@ChgRef{Version=[4],Kind=[AddedNormal]}
@ChgAdded{Version=[4],Text=[@key[type] Foo @key[is record]
   A : Natural;
   B : @key[access] procedure (A : Boolean);
@key[end record];]}
@end{Example}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[This is now legal, as one would expect; it
  was illegal in previous versions of Ada as the parameter A and the
  component A were homographs in the same declarative region
  (see @RefSecNum{Visibility}). Note that some implementations already allow
  this common sense interpretation, so this extension may in fact be used
  in existing code.]}
@end{Extend2012}


@LabeledClause{Scope of Declarations}

@begin{Intro}
@redundant[For each declaration, the language rules define a certain
portion of the program text called the @i{scope} of the declaration.
The scope of a declaration is also called the scope of any view
or entity declared by the declaration.
Within the scope of an entity, and only there,
there are places where it is legal to refer
to the declared entity.
These places are defined by the rules of visibility and overloading.]
@end{Intro}

@begin{StaticSem}
@Leading@Defn2{Term=[immediate scope], Sec=(of a declaration)}
The @i{immediate scope} of a declaration
is a portion of the declarative region immediately enclosing
the declaration.
The immediate scope starts at the beginning of the declaration,
except in the case of an overloadable declaration,
in which case the immediate scope starts just after the place
where the profile of the callable entity is determined
(which is at the end of the @ntf<_specification> for the callable entity,
or at the end of the @nt<generic_instantiation> if an instance).
The immediate scope extends to the end of the declarative region,
with the following exceptions:
@begin{Reason}
The reason for making overloadable declarations with profiles
special is to simplify compilation:
until the compiler has determined the profile,
it doesn't know which other declarations are homographs of this one,
so it doesn't know which ones this one should hide.
Without this rule, two passes over the @ntf<_specification> or
@nt<generic_instantiation> would be required to
resolve names that denote things with the same name as this one.
@end{Reason}
@begin{Itemize}
The immediate scope of a @nt{library_item} includes only its semantic
dependents.
@begin{Reason}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Chg{Version=[3],New=[Clause],Old=[Section]} 10 defines only a partial ordering of @nt{library_item}s.
Therefore, it is a good idea to restrict the immediate scope
(and the scope, defined below)
to semantic dependents.

@leading@;Consider also examples like this:
@begin{Example}
@key[package] P @key[is] @key[end] P;

@key[package] P.Q @key[is]
    I : Integer := 0;
@key[end] P.Q;

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00080 - syntax error}
@key[with] P;
@key[package] R @key[is]
    @key[package] X @key[renames] P;
    @Chg{New=[J : Integer := ],Old=[]}X.Q.I@Chg{New=[],Old=[ := 17]}; --@RI{ Illegal!}
@key[end] R;
@end{Example}

The scope of P.Q does not contain R.
Hence, neither P.Q nor X.Q are visible within R.
However, the name R.X.Q would be visible in some other
library unit where both R and P.Q are visible
(assuming R were made legal by removing the offending declaration).
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
  @ChgAdded{Version=[2],Text=[This rule applies to limited views as well as
  @lquotes@;normal@rquotes library items. In that case, the semantic dependents
  are the units that have a @nt{limited_with_clause} for the limited view.]}
@end{Ramification}
The immediate scope of
a declaration in the private part of a library unit does
not include the visible part of any
public descendant of that library unit.
@Pdefn2{Term=[descendant],Sec=(relationship with scope)}
@begin{Ramification}
In other words, a declaration in the private part can be
visible within the visible part, private part and body of a private
child unit.
On the other hand, such a declaration
can be visible within only the private part and body of a public
child unit.
@end{Ramification}
@begin{Reason}
The purpose of this rule is to prevent children from giving
private information to clients.
@end{Reason}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01]}
For a public child subprogram,
this means that the parent's private part is not visible in the
@Chg{Version=[2],New=[profile],Old=[@nt{formal_part}s]} of the declaration
and of the body.
This is true even for @ntf{subprogram_bodies} that are not
completions.
For a public child generic unit,
it means that the parent's private part is not visible in the
@nt{generic_formal_part}, as well as in
the first list of @nt{basic_declarative_item}s (for a generic package),
or the @Chg{Version=[2],New=[(syntactic) profile],Old=[@nt{formal_part}(s)]}
(for a generic subprogram).
@end{Ramification}
@end{Itemize}

@Defn{visible part}
@Redundant[The @i(visible part) of (a view of) an entity
is a portion of the text of its declaration
containing declarations that are visible from outside.]
@RootDefn{private part}
The @i{private part} of (a view of) an entity that has a visible part
contains all declarations within the declaration of
(the view of) the entity,
except those in the visible part;
@Redundant[these are not visible from outside.
Visible and private parts are defined only for these kinds of
entities: callable entities, other program units,
and composite types.]
@begin{Itemize}
@PDefn2{Term=[visible part], Sec=(of a view of a callable entity)}
The visible part of a view of a callable entity is its profile.

@PDefn2{Term=[visible part], Sec=(of a view of a composite type)}
The visible part of a composite type other than a task or protected type
consists of the declarations of
all components declared @Redundant[(explicitly or implicitly)]
within the @nt{type_declaration}.

@PDefn2{Term=[visible part], Sec=(of a generic unit)}
The visible part of a generic unit
includes the @nt{generic_formal_part}.
For a generic package, it also includes
the first list of @nt{basic_declarative_item}s of the
@nt{package_specification}.
For a generic subprogram, it also includes
the profile.
@begin{Reason}
Although there is no way to reference anything but the formals from
outside a generic unit, they are still in the visible part in the sense that
the corresponding declarations in an instance can be referenced
(at least in some cases).
In other words, these declarations have an effect on the outside world.
The visible part of a generic unit needs to be defined
this way in order to properly support
the rule that makes a parent's private part invisible within a
public child's visible part.
@end{Reason}
@begin{Ramification}
The visible part of an instance of a generic unit is as defined
for packages and subprograms;
it is not defined in terms of the visible part of a generic unit.
@end{Ramification}

@Redundant[The visible part of a package, task unit, or protected
unit consists of declarations in the program unit's declaration
other than those following the reserved word @key{private}, if any;
see @RefSecNum{Package Specifications and Declarations}
and @RefSecNum{Formal Packages} for packages,
@RefSecNum{Task Units and Task Objects} for task units,
and @RefSecNum{Protected Units and Protected Objects}
for protected units.]
@end{Itemize}

@Defn2{Term=[scope], Sec=(of a declaration)}
The scope of a declaration always
contains the immediate scope of the declaration.
In addition, for a given declaration that occurs immediately within
the visible part of an outer declaration,
or is a public child of an outer declaration,
the scope of the given declaration
extends to the end of the scope of the outer declaration,
except that the scope of a @nt{library_item} includes only its
semantic dependents.
@begin{Ramification}
Note the recursion.
If a declaration appears in the visible part of a library unit,
its scope extends to the end of the scope of the library unit,
but since that only includes dependents of the declaration of the library unit,
the scope of the inner declaration also only includes those dependents.
If X renames library package P,
which has a child Q, a @nt{with_clause} mentioning P.Q is necessary to
be able to refer to X.Q,
even if P.Q is visible at the place where X is declared.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00408-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0183-1]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[scope], Sec=(of an attribute_@!definition_@!clause)}
The scope of an @nt{attribute_definition_clause} is identical to the scope of a
declaration that would occur at the point of the
@nt{attribute_definition_clause}.@Chg{Version=[3],New=[ The scope of an
@nt{aspect_specification} is identical to the scope of the associated
declaration.@Defn2{Term=[scope], Sec=(of an aspect_@!specification)}],Old=[]}]}

@Defn2{Term=[immediate scope], Sec=[of (a view of) an entity]}
The immediate scope of a declaration is also the immediate scope
of the entity or view declared by the declaration.
@Defn2{Term=[scope], Sec=[of (a view of) an entity]}
Similarly,
the scope of a declaration is also the scope
of the entity or view declared by the declaration.
@begin{Ramification}
@leading@;The rule for immediate scope implies the following:
@begin{Itemize}
If the declaration is that of a library unit,
then the immediate scope includes the declarative region of the
declaration itself, but not other places,
unless they are within the scope of a @nt{with_clause} that mentions the
library unit.

@NoPrefix@;It is necessary to attach the semantics of @nt{with_clause}s to
[immediate] scopes (as opposed to visibility),
in order for various rules to work properly.
A library unit should hide a homographic implicit declaration that
appears in its parent, but only within the scope of a @nt{with_clause}
that mentions the library unit.
Otherwise, we would violate the "legality determinable via semantic
dependences" rule of @RefSec{Program Structure and Compilation Issues}.
The declaration of a library unit should be allowed to be a homograph of
an explicit declaration in its parent's body,
so long as that body does not mention the library unit in a
@nt{with_clause}.

@NoPrefix@;This means that one cannot denote the declaration of the library unit,
but one might still be able to denote the library unit via another
view.

@NoPrefix@;A @nt{with_clause} does not make the declaration of a library unit
visible; the lack of a @nt{with_clause} prevents it from being visible.
Even if a library unit is mentioned in a @nt{with_clause},
its declaration can still be hidden.

The completion of the declaration of a library unit
(assuming that's also a declaration)
is not visible, neither directly nor by selection,
outside that completion.

The immediate scope of
a declaration immediately within the body of a library unit
does not include any child of that library unit.

@NoPrefix@;This is needed to prevent children from looking inside their
parent's body. The children are in the declarative region of the
parent, and they might be after the parent's body.
Therefore, the scope of a declaration that occurs immediately within
the body might include some children.
@end{Itemize}
@end{Ramification}

@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0003-1]}@Comment{As usual,
    we let the paragraph numbers of non-normative text change}
@ChgAdded{Version=[4],Text=[The immediate scope of a pragma that is not used
as a configuration pragma is defined to be the@Defn2{Term=[immediate scope], Sec=(of a pragma)}
region extending from immediately after the pragma
to the end of the declarative region immediately enclosing the pragma.]}
@end{StaticSem}

@begin{Notes}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
There are notations for denoting visible declarations
that are not directly visible.
For example, @nt{parameter_@!specification}s are in the visible part of a
@nt{subprogram_@!declaration} so that they can be used in
named-notation calls appearing outside the called subprogram.
For another example,
declarations of the visible part of a package can be denoted by expanded names
appearing outside the package,
and can be made directly visible by a @nt{use_clause}.
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
There are some obscure @Chg{Version=[2],New=[cases ],Old=[]}involving
generics @Chg{Version=[2],New=[],Old=[cases ]}in which there is
no such notation.
See @Chg{Version=[3],New=[Clause],Old=[Section]} @RefSecNum{Generic Units}.
@end{Ramification}
@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
The fact that the immediate scope of an overloadable declaration does
not include its profile is new to Ada 95. It replaces
RM83-8.3(16), which said that within
a subprogram specification and within the formal part of an
entry declaration or accept statement, all declarations with
the same designator as the subprogram or entry were hidden from all
visibility.
The RM83-8.3(16) rule seemed to be overkill, and created both
implementation difficulties and unnecessary semantic complexity.
@end{Extend83}

@begin{DiffWord83}
We no longer need to talk about the scope of notations,
@nt{identifier}s, @nt{character_literal}s, and @nt{operator_symbol}s.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
The notion of "visible part" has been extended in Ada 95.
The syntax of
task and protected units now allows private parts,
thus requiring us to be able to talk about the visible part as well.
It was necessary to extend the concept to subprograms
and to generic units, in order for the visibility rules
related to child library units to work properly.
It was necessary to define the concept separately for
generic formal packages, since their visible part is
slightly different from that of a normal package.
Extending the concept to composite types made the
definition of scope slightly simpler.
We define visible part for some things elsewhere,
since it makes a big difference to the user for those things.
For composite types and subprograms, however,
the concept is used only in arcane visibility rules,
so we localize it to this @Chg{Version=[3],New=[subclause],Old=[clause]}.

In Ada 83, the semantics of @nt{with_clause}s was described
in terms of visibility.
It is now described in terms of [immediate] scope.

@leading@;We have clarified that the following is illegal
(where Q and R are library units):
@begin{Example}
@key[package] Q @key[is]
    I : Integer := 0;
@key[end] Q;

@key[package] R @key[is]
    @key[package] X @key[renames] Standard;
    X.Q.I := 17; --@RI{ Illegal!}
@key[end] R;
@end{Example}

even though Q is declared in the declarative region of Standard,
because R does not mention Q in a @nt{with_clause}.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00408-01]}
  @ChgAdded{Version=[2],Text=[The scope of an @nt{attribute_definition_clause}
  is defined so that it can be used to define the visibility of such a clause,
  so @i<that> can be used by the stream attribute availability rules
  (see @RefSecNum{Stream-Oriented Attributes}).]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[The scope of an @nt{aspect_specification}
  is defined for similar reasons that it was defined for
  @nt{attribute_definition_clause}s.]}
@end{DiffWord2005}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0003-1]}
  @ChgAdded{Version=[4],Text=[The immediate scope of a @nt{pragma} is
  defined as it is used in other rules in the Standard.]}
@end{DiffWord2012}


@LabeledClause{Visibility}

@begin{Intro}
@redundant[@Defn{visibility rules}
The @i{visibility rules},
given below, determine which declarations are
visible and directly visible at each place within a program.
The visibility rules apply to both explicit and implicit declarations.]
@end{Intro}

@begin{StaticSem}
@Defn2{Term=[visibility], Sec=(direct)}
@Defn{directly visible}
@Defn{directly visible}
A declaration is defined to be @i{directly visible} at places
where a @nt<name> consisting of only an @nt{identifier} or
@nt{operator_symbol} is sufficient to denote the declaration;
that is, no @nt<selected_component> notation or special context
(such as preceding => in a named association) is necessary
to denote the declaration.
@Defn{visible}
A declaration is defined to be @i{visible} wherever it is
directly visible, as well as at other places where
some @nt<name> (such as a @nt<selected_component>) can denote
the declaration.

The syntactic category @nt<direct_name> is used to indicate
contexts where direct visibility is required.
The syntactic category @nt<selector_name> is used to indicate
contexts where visibility, but not direct visibility,
is required.

@Defn2{Term=[visibility], Sec=(immediate)}
@Defn2{Term=[visibility], Sec=(use clause)}
There are two kinds of direct visibility:
@i{immediate visibility} and @i{use-visibility}.
@Defn{immediately visible}
A declaration is immediately visible at a place if it is directly
visible because the place is within its immediate scope.
@Defn{use-visible}
A declaration is use-visible if it is directly visible
because of a @nt{use_clause} (see @RefSecNum{Use Clauses}).
Both conditions can apply.

@Defn{hiding}
A declaration can be @i{hidden}, either from direct visibility,
or from all visibility,
within certain parts of its scope.
@Defn{hidden from all visibility}
Where @i{hidden from all visibility},
it is not visible at all (neither using a @nt<direct_name>
nor a @nt<selector_name>).
@Defn{hidden from direct visibility}
Where @i{hidden from direct visibility}, only direct visibility is lost;
visibility using a @nt<selector_name> is still possible.

@redundant[@Defn{overloaded}
Two or more declarations are @i{overloaded} if
they all have the same defining name
and there is a place where they are all directly visible.]
@begin{Ramification}
Note that a @nt{name} can have more than one possible interpretation
even if it denotes a nonoverloadable entity.
For example, if there are two functions F that return records,
both containing a component called C, then
the name F.C has two possible interpretations,
even though component declarations are not overloadable.
@end{Ramification}

@Defn{overloadable}
The declarations of callable entities
@Redundant[(including enumeration literals)]
are @i{overloadable}@Redundant[,
meaning that overloading is allowed for them].
@begin{Ramification}
A @nt{generic_declaration} is not overloadable within its own
@nt{generic_formal_part}.
This follows from the rules about when a @nt{name} denotes a current
instance.
See AI83-00286. This implies that within a
@nt{generic_formal_part}, outer declarations with the same defining name
are hidden from direct visibility. It also implies that if a generic
formal parameter has the same defining name as the generic itself,
the formal parameter hides the generic from direct visibility.
@end{Ramification}

@Defn{homograph}
Two declarations are @i{homographs}
if they have the same defining name,
and, if both are overloadable,
their profiles are type conformant.
@PDefn{type conformance}
@redundant[An inner declaration hides any outer homograph from direct visibility.]


@ChgToGlossary{Version=[2],Kind=[AddedNormal],Term=<Overriding operation>,
Text=[@ChgAdded{Version=[2],Text=[An overriding operation
is one that replaces an inherited primitive operation. Operations may be marked
explicitly as overriding or not overriding.]}]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0025],ARef=[AI95-00044-01]}
@leading@Redundant[Two homographs are not generally allowed
immediately within the same declarative region unless one
@i{overrides} the other (see Legality Rules below).]
@Defn{override}
@Chg{New=[The only declarations that are @Defn{overridable}@i{overridable} are
the implicit declarations for predefined operators and inherited primitive
subprograms.],Old=[]}
A declaration overrides another homograph that occurs
immediately within the same declarative region in the
following cases:
@begin{Itemize}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0025],ARef=[AI95-00044-01]}
@Chg{New=[A declaration that is not overridable overrides one that is overridable],
Old=[An explicit declaration overrides an implicit declaration of a primitive
subprogram]}, @Redundant[regardless of which declaration occurs first];
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0025],ARef=[AI95-00044-01]}
And regardless of whether the @Chg{New=[nonoverridable],Old=[explicit]}
declaration is overloadable or not.
@Chg{New=[For example, @nt{statement_identifier}s are covered by this rule.],Old=[]}

The @lquotes@;regardless of which declaration occurs first@rquotes@;
is there because the explicit declaration could be a primitive subprogram
of a partial view, and then the full view might inherit a homograph.
We are saying that the explicit one wins
(within its scope), even though the implicit one comes later.

If the overriding declaration is also a subprogram,
then it is a primitive subprogram.

As explained in @RefSec{Private Operations},
some inherited primitive subprograms are never declared.
Such subprograms cannot be overridden,
although they can be reached by dispatching calls
in the case of a tagged type.
@end{Ramification}

The implicit declaration of an inherited operator overrides
that of a predefined operator;
@begin{Ramification}
In a previous version of Ada 9X, we tried to avoid the notion of
predefined operators, and say that they were inherited from some
magical root type.
However, this seemed like too much mechanism.
Therefore, a type can have a predefined "+" as well as an inherited "+".
The above rule says the inherited one wins.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
The @lquotes@;regardless of which declaration occurs first@rquotes@; applies here
as well, in the case where @Chg{Version=[2],New=[@nt{derived_type_definition}],
Old=[@ntf{derived_type_declaration}]} in the visible
part of a public library unit derives from a private type declared in
the parent unit, and the full view of the parent type has additional
predefined operators, as explained in @RefSec{Private Operations}.
Those predefined operators can be overridden by inherited subprograms
implicitly declared earlier.
@end{Ramification}

An implicit declaration of an inherited subprogram
overrides a previous implicit declaration of an inherited
subprogram.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[If two or more homographs are
implicitly declared at the same place:]}

@begin{InnerItemize}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
@ChgAdded{Version=[2],Text=[If at least one is a subprogram that is neither a
null procedure nor an abstract subprogram, and does not require overriding (see
@RefSecNum{Abstract Types and Subprograms}), then they override those that are
null procedures, abstract subprograms, or require overriding. If more than one
such homograph remains that is not thus overridden, then they are all hidden
from all visibility.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
@ChgAdded{Version=[2],Text=[Otherwise (all are null procedures, abstract
subprograms, or require overriding), then any null procedure overrides all
abstract subprograms and all subprograms that require overriding; if more than
one such homograph remains that is not thus overridden, then if they are all
fully conformant with one another, one is chosen arbitrarily; if not, they are
all hidden from all visibility.
@Defn2{Term=[full conformance],Sec=(required)}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[In the case where the
    implementation arbitrarily chooses one overrider from among a group
    of inherited subprograms, users should not be able to determine which
    member was chosen, as the set of inherited subprograms which are chosen
    from must be fully conformant. This rule is needed in order to
    allow]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@Key{package} Outer @Key{is}
   @Key{package} P1 @Key{is}
      @Key{type} Ifc1 @Key{is interface};
      @Key{procedure} Null_Procedure (X : Ifc1) @Key{is null};
      @Key{procedure} Abstract_Subp  (X : Ifc1) @Key{is abstract};
   @Key{end} P1;],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[   @Key{package} P2 @Key{is}
      @Key{type} Ifc2 @Key{is interface};
      @Key{procedure} Null_Procedure (X : Ifc2) @Key{is null};
      @Key{procedure} Abstract_Subp  (X : Ifc2) @Key{is abstract};
   @Key{end} P2;],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[   @Key{type} T @Key{is abstract new} P1.Ifc1 @Key{and} P2.Ifc2 @Key{with null record};
@Key{end} Outer;],Old=[]}
@end{Example}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[without requiring that T explicitly override
    any of its inherited operations.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Full conformance is required here,
  as we cannot allow the parameter names to differ. If they did differ, the
  routine which was selected for overriding could be determined by using
  named parameter notation in a call.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[When the subprograms do not conform,
  we chose not to adopt the @lquotes@;use clause@rquotes rule which would make
  them all visible resulting in likely ambiguity. If we had used such a rule,
  any successful calls would be confusing; and the fact that there are no
  Beaujolais-like effect to worry about means we can consider other rules.
  The hidden-from-all-visibility homographs are still inherited
  by further derivations, which avoids order-of-declaration dependencies
  and other anomalies.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We have to be careful to not include arbitrary
  selection if the routines have real bodies. (This can happen in generics, see
  the example in the incompatibilities section below.) We don't want the
  ability to successfully call routines where the body executed depends on the
  compiler or a phase of the moon.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Note that if the type is concrete, abstract
  subprograms are inherited as subprograms that require overriding. We include
  functions that require overriding as well; these don't have real bodies, so
  they can use the more liberal rules.]}
@end{Discussion}

@end{InnerItemize}

@Redundant[For an implicit declaration of a primitive subprogram in a
generic unit, there is a copy of this declaration in an instance.]
However, a whole new set of primitive subprograms is implicitly
declared for each type declared within the visible part of the instance.
These new declarations occur immediately after the type
declaration, and override the copied ones.
@Redundant[The copied ones can be called only from within the instance;
the new ones can be called only from outside the instance,
although for tagged types, the body of a new one can be executed
by a call to an old one.]
@begin{Discussion}
In addition, this is also stated redundantly (again),
and is repeated, in @RefSec{Generic Instantiation}.
The rationale for the rule is explained there.
@end{Discussion}
@begin{Honest}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0042-1]}
@ChgAdded{Version=[3],Text=[The implicit subprograms declared when an
operation of a progenitor is implemented by an entry or subprogram also
override the appropriate implicitly declared inherited operations of the
progenitor.@PDefn2{Term=[override],Sec=[when implemented by]}]}
@end{Honest}
@end{Itemize}

@leading@Defn{visible}
@RootDefn{hidden from all visibility}
A declaration is visible within its scope,
except where hidden from all visibility,
as follows:
@begin{Itemize}
@PDefn2{Term=[hidden from all visibility], Sec=(for overridden declaration)}
An overridden declaration is hidden from all visibility within the
scope of the overriding declaration.
@begin{Ramification}
We have to talk about the scope of the overriding declaration,
not its visibility, because it hides
even when it is itself hidden.

Note that the scope of an explicit @nt{subprogram_declaration}
does not start until after its profile.
@end{Ramification}

@leading@PDefn2{Term=[hidden from all visibility], Sec=(within the declaration itself)}
A declaration is hidden from all visibility until the end of the
declaration, except:
@begin(InnerItemize)
  For a record type or record extension,
  the declaration is hidden from all visibility only
  until the reserved word @b(record);

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0177-1]}
  For a @nt{package_declaration}, @Chg{Version=[2],New=[],Old=[task declaration,
  protected declaration, ]}@nt{generic_@!package_@!declaration},
  @Chg{Version=[3],New=[],Old=[or ]}@nt{subprogram_@!body},
  @Chg{Version=[3],New=[or @nt{expression_@!function_@!declaration}, ],Old=[]}the
  declaration is hidden from all visibility only until the
  reserved word @key(is)
  of the declaration@Chg{Version=[2],New=[;],Old=[.]}
  @begin{Ramification}
    We're talking about the @key{is} of the construct itself, here,
    not some random @key{is} that might appear in a
    @nt{generic_formal_part}.
  @end{Ramification}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}
  @ChgAdded{Version=[2],Text=[For a task declaration or protected declaration,
  the declaration is hidden from all visibility only
  until the reserved word @key(with) of the declaration if there is one, or the
  reserved word @key(is) of the declaration if there is no @key(with).]}

  @begin{Honest}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[If there is neither a @key(with) nor @key(is),
    then the exception does not apply and the name is hidden from all
    visibility until the end of the declaration. This oddity was inherited
    from Ada 95.]}
  @end{Honest}
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[We need the @lquotes@key(with) or @key(is)@rquotes
    rule so that the visibility within an @nt{interface_list} does not
    vary by construct. That would make it harder to complete private extensions
    and would complicate implementations.]}
  @end{Reason}

@end(InnerItemize)

@PDefn2{Term=[hidden from all visibility], Sec=(for a declaration completed by a subsequent declaration)}
If the completion of a declaration is a declaration,
then within the scope of the completion,
the first declaration is hidden from all visibility.
Similarly, a @nt{discriminant_@!specification} or
@nt{parameter_@!specification} is hidden within the scope of a
corresponding @nt{discriminant_@!specification} or
@nt{parameter_@!specification} of a corresponding completion,
or of a corresponding @nt{accept_@!statement}.
@begin{Ramification}
This rule means, for example, that within the scope of a
@nt{full_type_declaration} that completes a
@nt{private_type_declaration},
the name of the type will denote the @nt{full_type_declaration},
and therefore the full view of the type.
On the other hand, if the completion is not a declaration,
then it doesn't hide anything,
and you can't denote it.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00217-06],ARef=[AI95-00412-01]}
@PDefn2{Term=[hidden from all visibility], Sec=(by lack of a @nt{with_clause})}
The declaration of a library unit
(including a @nt{library_unit_renaming_declaration})
is hidden from all visibility @Chg{Version=[2],New=[],Old=[except ]}at
places @Chg{Version=[2],New=[outside],Old=[that are within]} its declarative
region @Chg{Version=[2],New=[that are not],Old=[or]} within the scope
of a @Chg{Version=[2],New=[@nt{nonlimited_with_clause}],Old=[@nt{with_clause}]} that
mentions it.@Chg{Version=[2],New=[ The limited view of a library package
is hidden from all visibility at places that are not within the scope
of a @nt{limited_with_clause} that mentions it; in addition, the limited view
is hidden from all visibility within the declarative region of the package, as
well as within the scope of any @nt{nonlimited_with_clause} that
mentions the package. Where the declaration of the limited view of a
package is visible, any name that denotes the package denotes the
limited view, including those provided by a package renaming.],Old=[@Redundant[For
each declaration or renaming of a generic unit as a child of
some parent generic package, there is a corresponding declaration nested
immediately within each instance of the parent.]
Such a nested declaration is hidden from all visibility
except at places that are
within the scope of a @nt{with_clause} that mentions the child.]}

@begin{Discussion}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00217-06]}
This is the rule that prevents @nt{with_clause}s from being
transitive; the [immediate] scope includes indirect semantic dependents.
@Chg{Version=[2],New=[This rule also prevents the limited view of a package
from being visible in the same place as the full view of the package, which
prevents various ripple effects.],Old=[]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00217-06],ARef=[AI95-00412-01]}@ChgNote{Just moved from above}
@ChgAdded{Version=[2],Text=[@Redundant[For each
declaration or renaming of a generic unit as a child of
some parent generic package, there is a corresponding declaration nested
immediately within each instance of the parent.]
Such a nested declaration is hidden from all visibility
except at places that are
within the scope of a @nt{with_clause} that mentions the child.]}

@end{Itemize}

@leading@Defn{directly visible}
@Defn{immediately visible}
@Defn2{Term=[visibility], Sec=(direct)}
@Defn2{Term=[visibility], Sec=(immediate)}
A declaration with a @nt{defining_identifier} or
@nt{defining_operator_symbol} is immediately visible
@Redundant[(and hence
directly visible)] within its immediate scope
@RootDefn{hidden from direct visibility} except where hidden
from direct visibility, as follows:
@begin{Itemize}
  @PDefn2{Term=[hidden from direct visibility], Sec=(by an inner homograph)}
  A declaration is hidden from direct visibility
  within the immediate scope of a homograph of the
  declaration, if the homograph occurs within an inner declarative
  region;

  @PDefn2{Term=[hidden from direct visibility], Sec=(where hidden from all visibility)}
  A declaration is also hidden from direct visibility
  where hidden from all visibility.
@end{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00195-01],ARef=[AI95-00408-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0183-1]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=[visible], Sec=(attribute_@!definition_@!clause)}
An @nt{attribute_definition_clause} @Chg{Version=[3],New=[or an
@nt{aspect_specification}@PDefn2{Term=[visible], Sec=(aspect_@!specification)}
],Old=[]}is @i{visible} everywhere within its scope.]}

@end{StaticSem}

@begin{Resolution}
@PDefn2{Term=[possible interpretation], Sec=(for @nt{direct_name}s)}
A @nt{direct_name} shall resolve to denote a directly visible
declaration whose defining name is the same as the @nt{direct_name}.
@PDefn2{Term=[possible interpretation], Sec=(for @nt{selector_name}s)}
A @nt{selector_name} shall resolve to denote
a visible declaration whose defining name is the same as the
@nt{selector_name}.
@begin{Discussion}
"The same as" has the obvious meaning here,
so for +,
the possible interpretations are declarations whose defining name is "+"
(an @nt{operator_symbol}).
@end{Discussion}

These rules on visibility and direct visibility do not apply
in a @nt{context_clause}, a @nt{parent_unit_name},
or a @nt{pragma} that appears at the place of a
@nt{compilation_unit}.
For those contexts, see the rules
in @RefSec{Environment-Level Visibility Rules}.
@begin{Ramification}
Direct visibility is irrelevant for @nt{character_literal}s.
In terms of overload resolution
@nt{character_literal}s are similar to other literals,
like @key{null} @em see @RefSecNum{Literals}.
For @nt{character_literal}s, there is no need to worry about
hiding, since there is no way to declare homographs.
@end{Ramification}
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0025],Ref=[8652/0026],ARef=[AI95-00044-01],ARef=[AI95-00150-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00377-01]}
@Chg{New=[A nonoverridable],Old=[An explicit]} declaration is illegal if there is a
homograph occurring immediately within the same
declarative region that is visible at the place of the
declaration, and is not hidden from all visibility by the
@Chg{New=[nonoverridable],Old=[explicit]} declaration.
@Chg{New=[In addition, a type extension is illegal if somewhere within its
immediate scope it has two visible components with the same name.],Old=[]}
Similarly, the @nt<context_clause> for a @Chg{Version=[2],
New=[compilation unit],Old=[@nt<subunit>]} is illegal if it mentions (in a
@nt<with_clause>) some library unit, and there is a homograph
of the library unit that is visible at the place of the @Chg{Version=[2],
New=[compilation unit],Old=[corresponding stub]}, and the
homograph and the mentioned library unit are both
declared immediately within the same declarative region.@PDefn{generic contract issue}
These rules also apply to dispatching operations declared
in the visible part of an instance of a generic unit.
However, they do not apply to other overloadable declarations in
an instance@Redundant[; such declarations may have type conformant profiles
in the instance, so long as the corresponding declarations in the generic
were not type conformant].
@PDefn{type conformance}

@begin{Discussion}
@leading@;Normally, these rules just mean you can't explicitly
declare two homographs
immediately within the same declarative region.
The wording is designed to handle the
following special cases:
@begin{Itemize}
If the second declaration completes the first one,
the second declaration is legal.

@leading@;If the body of a library unit contains an explicit homograph of
a child of that same library unit, this is illegal only if the body
mentions the child in its @nt<context_clause>, or if
some subunit mentions the child.
Here's an example:
@begin{Example}
@key[package] P @key[is]
@key[end] P;

@key[package] P.Q @key[is]
@key[end] P.Q;

@key[package] @key[body] P @key[is]
    Q : Integer; --@RI{ OK; we cannot see package P.Q here.}
    @key[procedure] Sub @key[is] @key[separate];
@key[end] P;

@key[with] P.Q;
@key[separate](P)
@key[procedure] Sub @key[is] --@RI{ Illegal.}
@key[begin]
    @key[null];
@key[end] Sub;
@end{Example}

@NoPrefix@;If package body P said "@key[with] P.Q;", then it would be illegal
to declare the homograph Q: Integer. But it does not, so the
body of P is OK.
However, the subunit would be able to see both P.Q's,
and is therefore illegal.

@NoPrefix@;A previous version of Ada 9X allowed the subunit,
and said that references to P.Q would tend to be ambiguous.
However, that was a bad idea, because it requires overload resolution
to resolve references to directly visible nonoverloadable
homographs, which is something compilers have never before been
required to do.

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0026],Ref=[8652/0102],ARef=[AI95-00150-01],ARef=[AI95-00157-01]}
@ChgAdded{Version=[1],Type=[Leading],Text=[If a type extension contains a
component with
the same name as a component in an ancestor type, there must be no place
where both components are visible. For instance:]}
@begin{Example}@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[@key[package] A @key[is]
   @key[type] T @key[is tagged private];
   @key[package] B @key[is]
      @key[type] NT @key[is new] T @key[with record]
         I: Integer; -- @RI{Illegal because T.I is visible in the body.}
      @key[end record]; -- @RI{T.I is not visible here.}
   @key[end] B;
@key[private]
   @key[type] T @key[is tagged record]
      I: Integer; -- @RI{Illegal because T.I is visible in the body.}
   @key[end record];
@key[end] A;],Old=[]}

@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00114-01]}
@Chg{New=[@Chg{Version=[2],New=[],Old=[@key[package] A @key[is]
]}@key[package] @key[body] A @key[is]
   @key[package] @key[body] B @key[is]
      -- @RI{T.I becomes visible here.}
   @key[end] B;
@key[end] A;],Old=[]}

@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[@key[package] A.C @key[is]
   @key[type] NT2 @key[is new] A.T @key[with record]
      I: Integer; -- @RI{Illegal because T.I is visible in the private part.}
   @key[end record]; -- @RI{T.I is not visible here.}
@key[private]
    -- @RI{T.I is visible here.}
@key[end] A.C;],Old=[]}

@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[@key[with] A;
@key[package] D @key[is]
   @key[type] NT3 @key[is new] A.T @key[with record]
      I: Integer; -- @RI{Legal because T.I is never visible in this package.}
   @key[end record];
@key[end] D;],Old=[]}

@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[@key[with] D;
@key[package] A.E @key[is]
   @key[type] NT4 @key[is new] D.NT3 @key[with null record];
   X : NT4;
   I1 : Integer := X.I;        -- @RI{D.NT3.I}
   I2 : Integer := D.NT3(X).I; -- @RI{D.NT3.I}
   I3 : Integer := A.T(X).I;   -- @RI{A.T.I}
@key[end] A.E;],Old=[]}
@end{Example}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0102],ARef=[AI95-00157-01]}
@ChgAdded{Version=[1],NoPrefix=[T],Text=[D.NT3 can have a component I because
the component I of the parent type is never visible. The parent component
exists, of course, but is never declared for the type D.NT3. In the child
package A.E, the component I of A.T is visible, but that does not change the
fact that the A.T.I component was never declared for type D.NT3. Thus, A.E.NT4
does not (visibly) inherit the component I from A.T, while it does inherit the
component I from D.NT3. Of course, both components exist, and can be accessed
by a type conversion as shown above. This behavior stems from the fact that
every characteristic of a type (including components) must be declared
somewhere in the innermost declarative region containing the type @em if the
characteristic is never visible in that declarative region, it is never
declared. Therefore, such characteristics do not suddenly become available even
if they are in fact visible in some other scope.
See @RefSecNum{Private Operations} for more on the rules.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00377-01]}
@ChgAdded{Version=[2],Text=[It is illegal to mention both an explicit child of
an instance, and a child of the generic from which the instance was
instantiated. This is easier to understand with an example:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[@key{generic}
@key{package} G1 @key{is}
@key{end} G1;],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[@key{generic}
@key{package} G1.G2 @key{is}
@key{end} G1.G2;],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[@key{with} G1;
@key{package} I1 @key{is new} G1;],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@key{package} I1.G2 @key{renames} ...]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[@key{with} G1.G2;
@key{with} I1.G2;             -- @RI{Illegal}
@key{package} Bad @key{is} ...],Old=[]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],NoPrefix=[T],Text=[The context clause for Bad is illegal
as I1 has an implicit declaration of I1.G2 based on the generic child G1.G2,
as well as the mention of the explicit child I1.G2. As in the previous cases,
this is illegal only if the context clause makes both children visible; the
explicit child can be mentioned as long as the generic child is not (and
vice-versa).]}
@end{Itemize}

Note that we need to be careful which things we make "hidden from all
visibility" versus which things we make simply illegal for names to
denote. The distinction is subtle.
The rules that disallow names denoting components within a type
declaration (see @RefSecNum{Discriminants}) do not make the components
invisible at those places, so that the above rule makes components with
the same name illegal.
The same is true for the rule that disallows names denoting formal
parameters within a @nt{formal_part} (see @RefSecNum{Subprogram Declarations}).
@end{Discussion}
@begin{Discussion}
The part about instances is from AI83-00012.
The reason it says @lquotes@;overloadable declarations@rquotes@; is because
we don't want it to apply to type extensions that appear in an instance;
components are not overloadable.
@end{Discussion}
@end{Legality}

@begin{Notes}
Visibility for compilation units
follows from the definition of the environment
in @RefSecNum{The Compilation Process},
except that it is necessary to apply a @nt{with_clause} to obtain
visibility to a @nt{library_unit_declaration}
or @nt{library_unit_renaming_declaration}.

In addition to the visibility rules given above,
the meaning of the occurrence of a @nt{direct_name} or
@nt{selector_name} at a given place in the text can depend on
the overloading rules
(see @RefSecNum{The Context of Overload Resolution}).

Not all contexts where an @nt<identifier>, @nt<character_literal>,
or @nt<operator_symbol> are allowed require visibility of a corresponding
declaration.
Contexts where visibility is not required
are identified by using one of these three syntactic categories
directly in a syntax rule, rather than using @nt<direct_name> or
@nt<selector_name>.
@begin{Ramification}
@leading@;An @nt{identifier}, @nt{character_literal} or @nt{operator_symbol}
that occurs in one of the following contexts is not
required to denote a visible or directly
visible declaration:
@begin{enumerate}
A defining name.

The @nt{identifier}s or @nt{operator_symbol} that appear after the
reserved word @key{end} in a @nt{proper_body}.
Similarly for @lquotes@;@key{end loop}@rquotes@;, etc.

An @nt{attribute_designator}.

A @nt{pragma} @nt{identifier}.

A @SynI{pragma_argument_}@nt{identifier}.

An @nt{identifier} specific to a
pragma used in a pragma argument.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[An @nt{aspect_mark};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[An @nt{identifier} specific to an aspect used
in an @nt{aspect_definition}.]}
@end{enumerate}

The visibility rules have nothing to do with the above cases;
the meanings of such things are defined elsewhere.
Reserved words are not @nt{identifier}s;
the visibility rules don't apply to them either.

Because of the way we have defined "declaration",
it is possible for a usage name to denote a @nt{subprogram_body}, either
within that body, or (for a nonlibrary unit) after it
(since the body hides the corresponding declaration, if any).
Other bodies do not work that way.
Completions of @nt{type_declaration}s and
deferred constant declarations do work that way.
@nt{Accept_statement}s are never denoted, although the
@nt{parameter_specification}s in their profiles can be.

@leading@;The scope of a subprogram does not start until after its profile.
Thus, the following is legal:
@begin{Example}
X : @key[constant] Integer := 17;
...
@key[package] P @key[is]
    @key[procedure] X(Y : @key[in] Integer := X);
@key[end] P;
@end{Example}

The body of the subprogram will probably be illegal,
however, since the constant X will be hidden by then.

@leading@;The rule is different for generic subprograms,
since they are not overloadable;
the following is illegal:
@begin{Example}
X : @key[constant] Integer := 17;
@key[package] P @key[is]
    @key[generic]
      Z : Integer := X; --@RI{ Illegal!}
    @key[procedure] X(Y : @key[in] Integer := X); --@RI{ Illegal!}
@key[end] P;
@end{Example}

The constant X is hidden from direct visibility by the generic
declaration.
@end{Ramification}
@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
Declarations with the same defining
name as that of a subprogram or entry being defined
are nevertheless visible within
the subprogram specification or entry declaration.
@end{Extend83}

@begin{DiffWord83}
The term @lquotes@;visible by selection@rquotes@; is no longer defined.
We use the terms @lquotes@;directly visible@rquotes@; and @lquotes@;visible@rquotes@; (among other things).
There are only two regions of text that are of interest, here: the
region in which a declaration is visible,
and the region in which it is directly visible.

Visibility is defined only for declarations.
@end{DiffWord83}


@begin{Incompatible95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Defn{incompatibilities with Ada 95}
Added rules to handle the inheritance and overriding of
multiple homographs for a single type declaration, in order to support
multiple inheritance from interfaces. The new rules are intended to be
compatible with the existing rules so that programs that do not use
interfaces do not change their legality. However, there is a very rare
case where this is not true:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{generic}
   @key{type} T1 @key{is private};
   @key{type} T2 @key{is private};
@key{package} G @key{is}
   @key{type} T @key{is null record};
   @key{procedure} P (X : T; Y : T1);
   @key{procedure} P (X : T; Z : T2);
@key{end} G;]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} I @key{is new} G (Integer, Integer); -- @RI[Exports homographs of P.]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{type} D @key{is new} I.T; -- @RI[Both Ps are inherited.]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Obj : D;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[P (Obj, Z => 10); -- @RI[Legal in Ada 95, illegal in Ada 2005.]]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The call to P would resolve in Ada 95 by using the
parameter name, while the procedures P would be hidden from all visibility
in Ada 2005 and thus would not resolve.
This case doesn't seem worth making the rules any more
complex than they already are.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00377-01]}
  @ChgAdded{Version=[2],Text=[@B[Amendment Correction:] A @nt{with_clause} is
  illegal if it would create a homograph of an implicitly declared generic
  child (see @RefSecNum{Compilation Units - Library Units}). An Ada 95 compiler
  could have allowed this, but which unit of the two units involved would be
  denoted wasn't specified, so any successful use isn't portable. Removing one
  of the two @nt{with_clause}s involved will fix the problem.]}
@end{Incompatible95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0025],ARef=[AI95-00044-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified the overriding rules so
  that "/=" and @nt{statement_identifier}s are covered.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0026],ARef=[AI95-00150-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that is it never
  possible for two components with the same name to be visible; any such program
  is illegal.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00195-01],ARef=[AI95-00408-01]}
  @ChgAdded{Version=[2],Text=[The visibility of an
  @nt{attribute_definition_clause} is defined so that it can be used by the
  stream attribute availability rules (see @RefSecNum{Stream-Oriented Attributes}).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
  @ChgAdded{Version=[2],Text=[The visibility of a limited view of a library
  package is defined (see @RefSecNum{Compilation Units - Library Units}).]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0177-1]}
  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0157-1]}
  @ChgAdded{Version=[3],Text=[Added wording so that the parameters of an
  @nt{expression_@!function_@!declaration} are visible in the
  @Chg{Version=[4],New=[return expression],Old=[@nt{expression}]}
  of the function. (It would be pretty useless without such a rule.)]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[The visibility of an
  @nt{aspect_specification} is defined so that it can be used in various other
  rules.]}
@end{DiffWord2005}


@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
@LabeledAddedSubClause{Version=[2],Name=[Overriding Indicators]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00218-03]}
@ChgAdded{Version=[2],Text=[An @nt{overriding_indicator} is used to declare
that an operation is intended to override (or not override) an inherited
operation.]}
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00218-03]}
@AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=<overriding_indicator>,Old=<>}>,
rhs="@Chg{Version=[2],New=<[@key{not}] @key{overriding}>,Old=<>}"}
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00218-03],ARef=[AI95-00348-01],ARef=[AI95-00397-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0177-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[If an
@nt{abstract_@!subprogram_@!declaration},
@nt{null_@!procedure_@!declaration},@Chg{Version=[3],New=[
@nt{expression_@!function_@!declaration},],Old=[]}
@nt{subprogram_body}, @nt{subprogram_@!body_stub},
@nt{subprogram_@!renaming_@!declaration}, @nt{generic_@!instantiation} of a
subprogram, or @nt{subprogram_@!declaration}
other than a protected subprogram has an @nt{overriding_@!indicator}, then:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[the operation shall be a primitive operation for some
type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[if the @nt{overriding_indicator} is
@key{overriding}, then the operation shall override a homograph at the place of
the declaration or body;]}

@begin{Honest}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
@ChgAdded{Version=[3],Text=[This doesn't require that the overriding happen
at precisely the place of the declaration or body; it only requires that the
region in  which the overriding is known to have happened includes this
place. That is, the overriding can happen at or before the place of the
declaration or body.]}
@end{Honest}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[if the @nt{overriding_indicator} is
@key{not overriding}, then the operation shall not override any homograph
(at any place).]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@PDefn{generic contract issue}In addition to the
places where @LegalityTitle normally
apply, these rules also apply in the private part of an instance of a generic
unit.]}

@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The @Key{overriding} and @Key{not overriding} rules
differ slightly. For @Key{overriding}, we want the indicator to reflect the
overriding state at the place of the declaration; otherwise the indicator would
be @LQuotes@;lying@RQuotes@;. Whether a homograph is implicitly declared after
the declaration (see 7.3.1 to see how this can happen)
has no impact on this check. However, @Key{not overriding} is different;
@LQuotes@;lying@RQuotes@; would happen if a homograph declared later actually
is overriding. So, we require this check to take into account later overridings.
That can be implemented either by looking ahead, or by rechecking when
additional operations are declared.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The @LQuotes@;no lying@RQuotes@; rules are
needed to prevent a @nt{subprogram_declaration} and @nt{subprogram_body}
from having contradictory @nt{overriding_indicator}s.]}
@end{Discussion}
@end{Legality}

@begin{Notes}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00397-01]}
@ChgAdded{Version=[2],Text=[Rules for @nt{overriding_indicator}s of task and
protected entries and of protected subprograms are found in
@RefSecNum{Entries and Accept Statements}
and @RefSecNum{Protected Units and Protected Objects}, respectively.]}
@end{Notes}

@begin{Examples}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The use of
@nt{overriding_indicator}s allows the detection of errors at compile-time that
otherwise might not be detected at all. For instance, we might declare a
security queue derived from the Queue interface of 3.9.4 as:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{type} Security_Queue @key{is new} Queue @key{with record} ...;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{overriding}
@key{procedure} Append(Q : @key{in out} Security_Queue; Person : @key{in} Person_Name);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{overriding}
@key{procedure} Remove_First(Q : @key{in out} Security_Queue; Person : @key{in} Person_Name);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{overriding}
@key{function} Cur_Count(Q : @key{in} Security_Queue) @key{return} Natural;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{overriding}
@key{function} Max_Count(Q : @key{in} Security_Queue) @key{return} Natural;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{not overriding}
@key{procedure} Arrest(Q : @key{in out} Security_Queue; Person : @key{in} Person_Name);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The first four subprogram declarations guarantee
that these subprograms will override the four subprograms inherited from the
Queue interface. A misspelling in one of these subprograms will be detected
by the implementation. Conversely, the declaration of Arrest guarantees that
this is a new operation.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[In this case, the subprograms are abstract, so
  misspellings will get detected anyway. But for other subprograms
  (especially when deriving from concrete types), the error might never be
  detected, and a body other than the one the programmer intended might be
  executed without warning. Thus our new motto: @lquotes@;Overriding
  indicators @em don't derive a type without them!@rquotes]}
@end{Discussion}
@end{Examples}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00218-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @nt{Overriding_indicator}s are new. These let the
  programmer state her overriding intentions to the compiler; if the compiler
  disagrees, an error will be produced rather than a hard to find bug.]}
@end{Extend95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI95-0177-1]}
  @ChgAdded{Version=[3],Text=[Expression functions can have overriding
  indicators.]}
@end{DiffWord2005}


@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
@LabeledClause{Use Clauses}

@begin{Intro}
@redundant[A @nt{use_package_clause} achieves direct visibility of declarations that
appear in the visible part of a package;
a @nt{use_type_clause} achieves direct visibility of the primitive
operators of a type.]
@end{Intro}

@begin{MetaRules}
@Defn{equivalence of @nt{use_clause}s and @nt{selected_component}s}
If and only if the visibility rules allow P.A,
"@key[use] P;" should make A directly visible
(barring name conflicts).
This means, for example, that child library units, and
generic formals of a formal package whose
@nt{formal_package_actual_part} is (<>),
should be made visible by
a @nt{use_clause} for the appropriate package.

@Defn{Beaujolais effect}
The rules for @nt{use_clause}s were carefully constructed to avoid
so-called @i(Beaujolais) effects, where the addition or removal
of a single @nt{use_clause}, or a single declaration in a "use"d
package, would
change the meaning of a program from one legal interpretation to another.
@end{MetaRules}

@begin{Syntax}
@Syn{lhs=<use_clause>,rhs="@Syn2{use_package_clause} | @Syn2{use_type_clause}"}


@Syn{lhs=<use_package_clause>,rhs="@key{use} @SynI{package_}@Syn2{name} {, @SynI{package_}@Syn2{name}};"}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0150-1]}
@Syn{lhs=<use_type_clause>,rhs="@key[use] @Chg{Version=[3],New={[@key[all]] },Old=[]}@key[type] @Syn2{subtype_mark} {, @Syn2{subtype_mark}};"}
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00217-06]}
A @SynI{package_}@nt{name} of a @nt{use_package_clause}
shall denote @Chg{Version=[2],New=[a nonlimited view of ],Old=[]}a package.
@begin{Ramification}
This includes formal packages.
@end{Ramification}
@end{Legality}

@begin{StaticSem}
@Defn2{Term=[scope], Sec=(of a @nt{use_clause})}
For each @nt{use_clause},
there is a certain region of text called the @i{scope} of the @nt{use_clause}.
For a @nt{use_clause} within a @nt{context_clause} of a
@nt{library_unit_declaration}
or @nt{library_unit_renaming_declaration},
the scope is the entire declarative region of the declaration.
For a @nt{use_clause} within a @nt{context_clause} of a
body, the scope is the entire body @Redundant[and any
subunits (including multiply
nested subunits).
The scope does not include @nt<context_clause>s themselves.]

For a @nt{use_clause} immediately within a declarative region,
the scope is the portion of the declarative region
starting just after the @nt{use_clause}
and extending to the end of the declarative region.
However, the scope of a @nt{use_clause} in the private part of a library
unit does not include the visible part of
any public descendant of that library unit.
@begin{Reason}
@leading@;The exception echoes the similar exception for
@lquotes@;immediate scope (of a declaration)@rquotes@;
(see @RefSecNum{Scope of Declarations}).
It makes @nt{use_clause}s work like this:
@begin{Example}
@key[package] P @key[is]
    @key[type] T @key[is] @key[range] 1..10;
@key[end] P;

@key[with] P;
@key[package] Parent @key[is]
@key[private]
    @key[use] P;
    X : T;
@key[end] Parent;

@key[package] Parent.Child @key[is]
    Y : T; --@RI{ Illegal!}
    Z : P.T;
@key[private]
    W : T;
@key[end] Parent.Child;
@end{Example}

The declaration of Y is illegal because the scope of the @lquotes@;@key[use] P@rquotes@;
does not include that place, so T is not directly visible there.
The declarations of X, Z, and W are legal.
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00217-06]}
@Chg{Version=[2],New=[A package is @i{named} in a @nt{use_package_clause} if
it is denoted by a @SynI{package_}@nt{name} of that clause. A type is @i{named}
in a @nt{use_type_clause} if it is determined by a @nt{subtype_mark} of that
clause.@Defn2{Term=[named],Sec=[in a use clause]}],Old=[]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00217-06]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0150-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[]}@Comment{Dummy to add conditional leading}
@Defn{potentially use-visible}
For each package @Chg{Version=[2],New=[named in ],Old=[denoted by
a @SynI{package_}@nt{name} of ]}a @nt{use_package_clause} whose scope
encloses a place, each declaration that occurs immediately within
the declarative region of the package is
@i(potentially use-visible) at this place
if the declaration is visible at this place.
For each type @i(T) or @i(T)'Class @Chg{Version=[2],New=[named in ],Old=[
determined by a @nt<subtype_mark> of ]}a @nt{use_type_clause} whose scope
encloses a place, the declaration of each primitive operator of type @i(T)
is potentially use-visible at this place
if its declaration is visible at this place.@Chg{Version=[3], New=[ If a
@nt{use_type_clause} whose scope encloses a place includes the reserved
word @key[all], then the following entities are also potentially use-visible
at this place if the declaration of the entity is visible at this place:],Old=[]}
@begin{Itemize}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0150-1]}
  @ChgAdded{Version=[3],Text=[Each primitive subprogram of @i<T> including each
  enumeration literal (if any);]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0150-1]}
  @ChgAdded{Version=[3],Text=[Each subprogram that is declared immediately
  within the declarative region in which an ancestor type of @i<T> is declared
  and that operates on a class-wide type that covers @i<T>.]}
@end{Itemize}

  @begin{Ramification}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0150-1]}
  Primitive subprograms whose defining name is an @nt{identifier} are
  @i{not} made potentially visible by a
  @nt{use_type_clause}@Chg{Version=[3],New=[ unless reserved word @key[all] is
  included],Old=[]}.
  A @nt{use_type_clause} @Chg{Version=[3],New=[without @key[all] ],Old=[]}is only for operators.

  The semantics described here should be similar
  to the semantics for expanded names given
  in @RefSec{Selected Components}
  so as to achieve the effect requested by
  the @lquotes@;principle of equivalence of @nt{use_clause}s and
  @nt{selected_component}s.@rquotes@;
  Thus, child library units and generic formal parameters of a formal
  package are
  potentially use-visible when their enclosing package is use'd.

  The "visible at that place" part implies that
  applying a @nt{use_clause} to a parent unit does not make all of its
  children use-visible @em only those that have been made
  visible by a @nt{with_clause}.
  It also implies that we don't have to worry about hiding in the
  definition of "directly visible" @em a declaration cannot be use-visible
  unless it is visible.

  Note that
  "@key[use type] T'Class;" is equivalent to "@key[use type] T;",
  which helps avoid breaking the generic contract model.
  @end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0131-1]}
@ChgAdded{Version=[3],Text=[Certain implicit declarations may become
potentially use-visible in certain contexts as described in
@RefSecNum{Formal Subprograms}.]}


@leading@Defn{use-visible}
@Defn2{Term=[visibility],Sec=(use clause)}
A declaration is @i{use-visible} if it is potentially use-visible,
except in these naming-conflict cases:
@begin{itemize}
  A potentially use-visible declaration is not use-visible if the place
  considered is within the immediate scope of a homograph of the
  declaration.

  Potentially use-visible declarations that have the same @nt{identifier}
  are not use-visible unless each of them is an overloadable
  declaration.
@begin{Ramification}
  Overloadable declarations don't cancel each other out,
  even if they are homographs,
  though if they are not distinguishable
  by formal parameter names or the presence or absence of
  @nt{default_expression}s, any use will be ambiguous.
  We only mention @nt{identifier}s here, because
  declarations named by @nt<operator_symbol>s are
  always overloadable, and hence never cancel each other.
  Direct visibility is irrelevant for @nt{character_literal}s.
@end{Ramification}
@end{itemize}
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(use_clause)}
The elaboration of a @nt{use_clause} has no effect.
@end{RunTime}

@begin{Examples}

@leading@keepnext@i{Example of a use clause in a context clause:}
@begin{Example}
@key[with] Ada.Calendar; @key[use] Ada;
@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Example of a use type clause:}
@end{WideAbove}
@begin{Example}
@key[use type] Rational_Numbers.Rational; --@RI{ see @RefSecNum{Package Specifications and Declarations}}
Two_Thirds: Rational_Numbers.Rational := 2/3;
@end{Example}

@begin{Ramification}
In @lquotes@;@key[use] X, Y;@rquotes@;, Y cannot refer to something made visible by the
@lquotes@;@key[use]@rquotes@; of X.
Thus, it's not (quite) equivalent to @lquotes@;@key[use] X; @key[use] Y;@rquotes@;.

If a given declaration is already immediately visible,
then a @nt{use_clause} that makes it potentially use-visible
has no effect.
Therefore,
a @nt{use_type_clause} for a type whose declaration appears
in a place other than the visible part
of a package has no effect;
it cannot make a declaration use-visible
unless that declaration is already immediately visible.

"@key[Use] @key[type] S1;" and "@key[use] @key[type] S2;"
are equivalent if S1 and S2 are both subtypes of the same type.
In particular,
"@key[use] @key[type] S;" and "@key[use] @key[type] S'Base;"
are equivalent.
@end{Ramification}
@begin{Reason}
We considered adding a rule that prevented several declarations of
views of the same entity that all have the same semantics from
cancelling each other out.
For example, if a (possibly implicit)
@nt{subprogram_declaration} for "+" is potentially use-visible,
and a fully conformant renaming of it is also potentially
use-visible, then they (annoyingly) cancel each other out;
neither one is use-visible.
The considered rule would have made just one of them use-visible.
We gave up on this idea due to the complexity of the rule.
It would have had to account for both overloadable and
nonoverloadable @nt{renaming_declaration}s,
the case where the rule should apply only to some subset of the
declarations with the same defining name,
and the case of @nt{subtype_declaration}s
(since they are claimed to be sufficient for renaming of subtypes).
@end{Reason}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The @nt{use_type_clause} is new to Ada 95.
@end{Extend83}

@begin{DiffWord83}
The phrase @lquotes@;omitting from this set any packages that
enclose this place@rquotes@; is no longer necessary to avoid making something
visible outside its scope, because we explicitly state that the
declaration has to be visible in order to be
potentially use-visible.
@end{DiffWord83}


@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
  @ChgAdded{Version=[2],Text=[Limited views of packages are not allowed in use clauses.
  Defined @i<named in a use clause> for use in other limited view rules (see
  @RefSecNum{Context Clauses - With Clauses}).]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0150-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}The
  @key[use all type] version of the @nt{use_type_clause} is new to Ada 2012.
  It works similarly to prefixed views.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0131-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added wording to allow other
  declarations to be potentially use-visible, to support corrections to
  formal subprograms.]}
@end{DiffWord2005}



@LabeledClause{Renaming Declarations}

@begin{Intro}
@redundant[A @nt{renaming_declaration} declares another name for an entity,
such as an object, exception, package, subprogram, entry,
or generic unit.
Alternatively, a @nt{subprogram_renaming_declaration} can be the
completion of a previous @nt{subprogram_declaration}.]

@ChgToGlossary{Version=[2],Kind=[Added],Term=<Renaming>,
Text=<@ChgAdded{Version=[2],Text=[A @nt{renaming_declaration} is a declaration
that does not define a new entity, but instead defines a view of an existing
entity.]}>}

@end{Intro}

@begin{Syntax}
@Syn{lhs=<renaming_declaration>,rhs="
      @Syn2{object_renaming_declaration}
    | @Syn2{exception_renaming_declaration}
    | @Syn2{package_renaming_declaration}
    | @Syn2{subprogram_renaming_declaration}
    | @Syn2{generic_renaming_declaration}"}
@end{Syntax}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(renaming_declaration)}
The elaboration of a @nt{renaming_declaration} evaluates the @nt{name} that
follows the reserved word @key{renames} and thereby determines the
view and entity denoted by this name
@Defn{renamed view}
@Defn{renamed entity}
(the @i{renamed view} and @i{renamed entity}).
@Redundant[A @nt{name} that denotes the @nt{renaming_declaration}
denotes (a new view of) the renamed entity.]
@end{RunTime}

@begin{Notes}
Renaming may be used to resolve name conflicts and to act as a
shorthand. Renaming with a different @nt{identifier} or
@nt{operator_symbol} does not hide the old @nt{name}; the new
@nt{name} and the old @nt{name} need not be visible at the same
places.

A task or protected object that is declared by an explicit
@nt{object_declaration} can be renamed as an object. However, a
single task or protected object cannot be renamed since the
corresponding type is anonymous (meaning it has no nameable subtypes).
For similar reasons, an object of an anonymous array or access type
cannot be renamed.

@leading@keepnext@;A subtype defined without any additional constraint
can be used to achieve the effect of renaming another subtype
(including a task or protected subtype) as in
@begin{Example}
   @key[subtype] Mode @key[is] Ada.Text_IO.File_Mode;
@end{Example}
@end{Notes}

@begin{DiffWord83}
The second sentence of RM83-8.5(3),
@lquotes@;At any point where a renaming declaration is visible,
the identifier, or operator symbol of this declaration denotes the
renamed entity.@rquotes@; is incorrect. It doesn't say directly visible.
Also, such an @nt{identifier} might resolve to something else.

The verbiage about renamings being legal @lquotes@;only if exactly one...@rquotes@;,
which appears in RM83-8.5(4) (for objects) and RM83-8.5(7) (for subprograms) is
removed, because it follows from the normal rules about overload resolution.
For language lawyers, these facts are obvious; for programmers, they are
irrelevant, since failing these tests is highly unlikely.
@end{DiffWord83}


@LabeledSubClause{Object Renaming Declarations}

@begin{Intro}
@redundant[An @nt{object_renaming_declaration} is used to rename an object.]
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01],ARef=[AI95-00423-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<object_renaming_declaration>,rhs="@Chg{Version=[2],New=[
    ],Old=[]}@Syn2{defining_identifier} : @Chg{Version=[2],New=<[@Syn2{null_exclusion}] >,Old=<>}@Syn2{subtype_mark} @key{renames} @SynI{object_}@Syn2{name}@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]};@Chg{Version=[2],New=[
  | @Syn2{defining_identifier} : @Syn2{access_definition} @key{renames} @SynI{object_}@Syn2{name}@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]};],Old=[]}"}
@end{Syntax}

@begin{Resolution}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01],ARef=[AI95-00254-01],ARef=[AI95-00409-01]}
The type of the @SynI{object_}@nt{name} shall resolve to
the type determined by the @nt{subtype_mark}@Chg{Version=[2],New=[,
or in the case where the type is defined by an @nt{access_definition}, to an
anonymous access type. If the anonymous access type is an access-to-object type,
the type of the @SynI{object_}@nt{name}
shall have the same designated type as that of the @nt{access_definition}.
If the anonymous access type is an access-to-subprogram type,
the type of the @SynI{object_}@nt{name} shall have a designated profile
that is type conformant with that of the @nt{access_definition}],Old=[]}.

@begin{Reason}
@leading@;A previous version of Ada 9X used the usual
@lquotes@;expected type@rquotes@; wording:@*
@lquotes@;The expected type for the @SynI{object_}@nt{name} is
that determined by the @nt{subtype_mark}.@rquotes@;@*
We changed it so that this would be illegal:
@begin{Example}
X: T;
Y: T'Class @key[renames] X; --@RI{ Illegal!}
@end{Example}

@leading@;When the above was legal, it was unclear whether Y
was of type T or T'Class.
Note that we still allow this:
@begin{Example}
Z: T'Class := ...;
W: T @key[renames] F(Z);
@end{Example}

where F is a function with a controlling parameter and result.
This is admittedly a bit odd.

Note that the matching rule for generic formal parameters of mode
@key[in out] was changed to keep it consistent with the rule
for renaming.
That makes the rule different for @key[in] vs. @key[in out].

@end{Reason}
@end{Resolution}

@begin{Legality}
The renamed entity shall be an object.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00231-01],ARef=[AI95-00409-01]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[In the case
where the type is defined by an @nt{access_definition},
the type of the renamed object and the type defined by the
@nt{access_definition}:]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00231-01],ARef=[AI95-00409-01]}
@ChgAdded{Version=[2],Text=[shall both be access-to-object types with
statically matching designated subtypes and with both or neither being
access-to-constant types; or
@PDefn2{Term=[statically matching],Sec=(required)}]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00409-01]}
@ChgAdded{Version=[2],Text=[shall both be access-to-subprogram types with
subtype conformant designated profiles.
@Defn2{Term=[subtype conformance],Sec=(required)}]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00423-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For an
@nt{object_renaming_declaration} with a @nt{null_exclusion} or an
@nt{access_definition} that has a @nt{null_exclusion}:]}

@begin{Itemize}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[if the @Syni{object_}@nt{name} denotes a
  generic formal object of a generic unit @i{G}, and the
  @nt{object_renaming_declaration} occurs within the body of @i{G} or within
  the body of a generic unit declared within the declarative region of @i{G},
  then the declaration of the formal object of @i{G} shall have a
  @nt{null_exclusion};]}

  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[otherwise, the subtype of the
  @Syni{object_}@nt{name} shall exclude null.
  @PDefn{generic contract issue}
  In addition to the places where @LegalityTitle normally apply
  (see @RefSecNum{Generic Instantiation}),
  this rule applies also in the private part of an
  instance of a generic unit.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[This rule prevents
  @lquotes@;lying@rquotes.
  @b<Null> must never be the value of an object with an explicit
  @nt{null_exclusion}. The first bullet is an assume-the-worst rule
  which prevents trouble in one obscure case:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{type} Acc_I @key{is access} Integer;
@key{subtype} Acc_NN_I @key{is not null} Acc_I;
Obj : Acc_I := @key{null};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{generic}
   B : @key{in out} Acc_NN_I;
@key{package} Gen @key{is}
   ...
@key{end} Gen;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package body} Gen @key{is}
   D : @key{not null} Acc_I @key{renames} B;
@key{end} Gen;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} Inst @key{is new} Gen (B => Obj);]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Without the first bullet rule, D would
  be legal, and contain the value @key{null}, because the rule about lying
  is satisfied for generic matching (Obj matches B; B does not explicitly
  state @key{not null}),
  @LegalityTitle are not rechecked in the body of any instance, and the
  template passes the lying rule as well. The rule is so complex because it
  has to apply to formals used in bodies of child generics as well as in
  the bodies of generics.]}
@end{Reason}
@end{Itemize}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0017],ARef=[AI95-00184-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00363-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0008-1]}
The renamed entity shall not be a subcomponent that depends on
discriminants of @Chg{Version=[3],New=[an object],Old=[a variable]}
whose nominal subtype is unconstrained@Chg{Version=[3],New=[],Old=[,]}
unless @Chg{Version=[3],New=[the object is known to be constrained],
Old=[this subtype is indefinite, or the variable is @Chg{Version=[2],
New=[constrained by its initial value],Old=[aliased]}]}.
A @nt{slice} of an array shall not be renamed if
this restriction disallows renaming of the array.
@Chg{New=[@PDefn{generic contract issue}In addition to the places where
Legality Rules normally apply, these rules apply also in the private part of an
instance of a generic unit.@Chg{Version=[3],New=[],Old=[ These rules also apply for a renaming that appears
in the body of a generic unit, with the additional requirement that even if the
nominal subtype of the variable is indefinite, its type shall not be a
descendant of an untagged generic formal derived type.]}],Old=[]}

@begin{Reason}
This prevents renaming of subcomponents that might
disappear, which might leave dangling references.
Similar restrictions exist for the Access attribute.

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0017],ARef=[AI95-00184-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0008-1]}
@ChgAdded{Version=[1],Type=[Leading],Text=[The @lquotes@;recheck on
instantiation@rquotes@; @Chg{Version=[3],New=[requirement], Old=[and
@lquotes@;assume-the-worst in the body@rquotes@;
restrictions]} on generics @Chg{Version=[3],New=[is],Old=[are]}
necessary to avoid renaming of components which
could disappear even when the nominal subtype would prevent the problem:]}

@begin{Example}
@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[@key{type} T1 (D1 : Boolean) @key{is}
   @key{record}
      @key{case} D1 @key{is}
         @key{when} False =>
            C1 : Integer;
         @key{when} True =>
            @key{null};
         @key{end} @key{case};
      @key{end} @key{record};],Old=[]}

@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[@key{generic}
   @key{type} F @key{is} @key{new} T1;
   X : @key{in out} F;
@key{package} G @key{is}
   C1_Ren : Integer @key{renames} X.C1;
@key{end} G;],Old=[]}

@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[@key{type} T2 (D2 : Boolean := False) @key{is} @key{new} T1 (D1 => D2);
@Comment{Blank line}
Y : T2;
@Comment{Blank line}
@key{package} I @key{is new} G (T2, Y);
@Comment{Blank line}
Y := (D1 => True); -- @RI[Oops!  What happened to I.C1_Ren?]],Old=[]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0008-1]}
@ChgAdded{Version=[3],Text=[In addition, the @ldquote@;known to be constrained@rdquote
rules include assume-the-worst rules for generic bodies partially to
prevent such problems.]}

@end{Reason}
@begin{ImplNote}
Note that if an implementation chooses to deallocate-then-reallocate
on @nt{assignment_@!statement}s assigning to unconstrained definite objects,
then it cannot represent renamings and access values as simple
addresses, because the above rule does not apply to all components of
such an object.
@end{ImplNote}
@begin{Ramification}
If it is a generic formal object,
then the assume-the-best or assume-the-worst rules are applied as
appropriate.
@end{Ramification}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01],ARef=[AI95-00409-01]}
An @nt{object_renaming_declaration} declares a new view
@Redundant{of the renamed object} whose
properties are identical to those of the renamed view.
@Redundant[Thus, the properties of the renamed object are not affected by the
@nt{renaming_declaration}.
In particular, its value and whether or not it is a constant
are unaffected; similarly, the@Chg{Version=[2],New=[ null exclusion or],Old=[]}
constraints that apply to an object are
not affected by renaming (any constraint implied by the
@nt{subtype_mark} @Chg{Version=[2],New=[or @nt{access_definition} ],Old=[]}of
the @nt{object_renaming_declaration} is ignored).]
@begin{Discussion}
Because the constraints are ignored,
it is a good idea
to use the nominal subtype of the renamed object
when writing an @nt{object_renaming_declaration}.

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00409-01]}
@ChgAdded{Version=[2],Text=[If no @nt{null_exclusion} is given in the
renaming, the
object may or may not exclude null. This is similar to the way that
constraints need not match, and @key{constant} is not specified. The
renaming defines a view of the
renamed entity, inheriting the original properties.]}
@end{Discussion}
@end{StaticSem}

@begin{Examples}
@leading@keepnext@i{Example of renaming an object:}
@begin{Example}
@key[declare]
   L : Person @key[renames] Leftmost_Person; --@RI{ see @RefSecNum{Incomplete Type Declarations}}
@key[begin]
   L.Age := L.Age + 1;
@key[end];
@end{Example}
@end{Examples}

@begin{DiffWord83}
The phrase @lquotes@;subtype ... as defined in a corresponding
object declaration, component declaration, or component subtype
indication,@rquotes@; from RM83-8.5(5), is incorrect in Ada 95;
therefore we removed it.
It is incorrect in the case of an object with an indefinite
unconstrained nominal subtype.
@end{DiffWord83}

@begin{Incompatible95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Leading@;@Defn{incompatibilities with Ada 95}
Aliased variables are not necessarily constrained in Ada
2005 (see @RefSecNum{Array Types}). Therefore, a subcomponent of an aliased
variable may disappear or change shape, and renaming such a subcomponent thus
is illegal, while the same operation would have been legal in Ada 95. Note that
most allocated objects are still constrained by their initial value (see
@RefSecNum{Allocators}), and thus have no change in the
legality of renaming for them. For example,
using the type T2 of the previous example:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   AT2 : @key{aliased} T2;
   C1_Ren : Integer @key{renames} AT2.C1; -- @RI[Illegal in Ada 2005, legal in Ada 95]
   AT2 := (D1 => True);             -- @RI[Raised Constraint_Error in Ada 95,]
                                    -- @RI[but does not in Ada 2005, so C1_Ren becomes]
                                    -- @RI[invalid when this is assigned.]]}
@end{Example}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01],ARef=[AI95-00231-01],ARef=[AI95-00254-01],ARef=[AI95-00409-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  A renaming can have an anonymous access type. In that case, the accessibility
  of the renaming is that of the original object (accessibility is not
  lost as it is for assignment to a component or stand-alone object).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01],ARef=[AI95-00423-01]}
  @ChgAdded{Version=[2],Text=[A renaming can have a @nt{null_exclusion}; if so,
  the renamed object must also exclude null, so that the @nt{null_exclusion}
  does not lie. On the other hand, if the renaming does not have a
  @nt{null_exclusion}. it excludes null if the renamed object does.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0017],ARef=[AI95-00184-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Fixed to forbid renamings of
  depends-on-discriminant components if the type @i{might} be definite.]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0008-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Simplified the description of when a discriminant-dependent component is
  allowed to be renamed @em it's now simply when the object is
  known to be constrained. This
  fixes a confusion as to whether a subcomponent of an object that is not
  certain to be constrained can be renamed. The fix introduces an
  incompatibility, as the rule did not apply in Ada 95 if the prefix was a
  constant; but it now applies no matter what kind of object is involved. The
  incompatibility is not too bad, since most kinds of constants are known to be
  constrained.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in an
  @nt{object_renaming_declaration}.
  This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}


@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledSubClause{Exception Renaming Declarations}

@begin{Intro}
@redundant[An @nt{exception_renaming_declaration} is used to rename an exception.]
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<exception_renaming_declaration>,rhs="@Syn2{defining_identifier} : @key{exception} @key{renames} @SynI{exception_}@Syn2{name}@Chg{Version=[3],New=<
   [@Syn2{aspect_specification}]>,Old=[]};"}
@end{Syntax}

@begin{Legality}
The renamed entity shall be an exception.
@end{Legality}

@begin{StaticSem}
An @nt{exception_renaming_declaration} declares a new view
@Redundant{of the renamed exception}.
@end{StaticSem}

@begin{Examples}
@leading@keepnext@i{Example of renaming an exception:}
@begin{Example}
EOF : @key[exception] @key[renames] Ada.IO_Exceptions.End_Error; @RI{-- see @RefSecNum{Exceptions in Input-Output}}
@end{Example}
@end{Examples}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in an
  @nt{exception_renaming_declaration}.
  This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}


@LabeledSubClause{Package Renaming Declarations}

@begin{Intro}
@redundant[A @nt{package_renaming_declaration} is used to rename a package.]
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<package_renaming_declaration>,rhs="@key{package} @Syn2{defining_program_unit_name} @key{renames} @SynI{package_}@Syn2{name}@Chg{Version=[3],New=<
   [@Syn2{aspect_specification}]>,Old=[]};"}
@end{Syntax}

@begin{Legality}
The renamed entity shall be a package.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00217-06],ARef=[AI95-00412-01]}
@ChgAdded{Version=[2],Text=[If the @SynI{package_}@nt{name} of a
@nt{package_renaming_declaration} denotes a limited view of a package @i{P},
then a name that denotes the @nt{package_renaming_declaration} shall occur
only within the immediate scope of the renaming or the scope of a
@nt{with_clause} that mentions the package @i{P} or, if @i{P} is a nested
package, the innermost library package enclosing @i{P}.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[The use of a renaming that designates a limited
  view is restricted to locations where we know whether the view is limited
  or nonlimited (based on a @nt{with_clause}). We don't want to make an
  implicit limited view, as those are not transitive like a regular view.
  Implementations should be able to see all limited views needed based on the
  @nt{context_clause}.]}
@end{Discussion}
@end{Legality}

@begin{StaticSem}
A @nt{package_renaming_declaration} declares a new view
@Redundant{of the renamed package}.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00412-01]}
@ChgAdded{Version=[2],Text=[@Redundant[At places where the declaration of the
limited view of the renamed package is visible, a @nt{name} that denotes the
@nt{package_renaming_declaration} denotes a limited view of the package (see
@RefSecNum{Compilation Units - Library Units}).]]}
@begin{TheProof}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[This rule is found in @RefSec{Visibility}.]}
@end{TheProof}
@end{StaticSem}

@begin{Examples}
@leading@keepnext@i{Example of renaming a package:}
@begin{Example}
@key[package] TM @key[renames] Table_Manager;
@end{Example}
@end{Examples}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06],ARef=[AI95-00412-01]}
  @ChgAdded{Version=[2],Text=[Uses of renamed limited views of packages can
  only be used within the scope of a with_clause for the renamed package.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in a
  @nt{package_renaming_declaration}.
  This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}


@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledSubClause{Subprogram Renaming Declarations}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
A @nt{subprogram_renaming_declaration} can serve as the completion of
a @nt{subprogram_declaration};
@Defn{renaming-as-body}
such a @nt{renaming_declaration} is called a @i{renaming-as-body}.
@Defn{renaming-as-declaration}
A @nt{subprogram_renaming_declaration} that is not a completion is
called a @i{renaming-as-declaration}@Redundant[,
and is used to rename a subprogram
(possibly an enumeration literal) or an entry].
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
A renaming-as-body is a declaration,
as defined in @Chg{Version=[3],New=[Clause],Old=[Section]}
@RefSecNum{Declarations and Types}.
@end{Ramification}
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00218-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<subprogram_renaming_declaration>,rhs="@Chg{Version=[2],New=<
    [@Syn2{overriding_indicator}]
    >,Old=<>}@Syn2{subprogram_specification} @key{renames} @SynI{callable_entity_}@Syn2{name}@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]};"}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected profile],
  Sec=(subprogram_renaming_declaration)}
The expected profile for the @i(callable_entity_)@nt<name>
is the profile given in the @nt<subprogram_specification>.
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0239-1]}
The profile of a renaming-as-declaration
shall be @Chg{Version=[3],New=[mode conformant],Old=[mode-conformant]},
 with that of the renamed callable entity.
@Defn2{Term=[mode conformance],Sec=(required)}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00423-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For a parameter or result subtype of
the @nt{subprogram_specification} that has an explicit @nt{null_exclusion}:]}

@begin{Itemize}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[if the @Syni{callable_entity_}@nt{name}
  denotes a generic formal subprogram of
  a generic unit @i{G}, and the @nt{subprogram_renaming_declaration} occurs
  within the body of a generic unit @i{G} or within the body of a generic unit
  declared within the declarative region of the generic unit @i{G}, then the
  corresponding parameter or result subtype of the formal subprogram of @i{G}
  shall have a @nt{null_exclusion};]}

  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[otherwise, the subtype of the corresponding
  parameter or result type of the renamed callable entity shall exclude null.
  @PDefn{generic contract issue}
  In addition to the places where @LegalityTitle normally apply
  (see @RefSecNum{Generic Instantiation}),
  this rule applies also in the private part of an
  instance of a generic unit.]}
@end{Itemize}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[This rule prevents
  @lquotes@;lying@rquotes.
  @b<Null> must never be the value of a parameter or result with an explicit
  @nt{null_exclusion}. The first bullet is an assume-the-worst rule
  which prevents trouble in generic bodies (including bodies of child
  units) when the formal subtype excludes null implicitly.]}
@end{Reason}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0027],Ref=[8652/0028],ARef=[AI95-00135-01],ARef=[AI95-00145-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0239-1]}
The profile of a renaming-as-body
@Chg{New=[],Old=[shall be subtype-conformant with that of the renamed
callable entity, and ]}shall conform fully to that of the declaration it
completes.
@Defn2{Term=[full conformance],Sec=(required)}
If the renaming-as-body completes that declaration
before the subprogram it declares is frozen,
@Chg{New=[the profile shall be @Chg{Version=[3],New=[mode conformant],Old=[mode-conformant]}
@Defn2{Term=[mode conformance],Sec=(required)}with that of the renamed
callable entity and ],Old=[]}the subprogram it declares
takes its convention from the renamed subprogram;
otherwise@Chg{New=[, the profile shall be @Chg{Version=[3],New=[subtype conformant],Old=[subtype-conformant]}
with that of the
renamed callable entity and],Old=[]} the convention of the renamed subprogram
shall not be Intrinsic.
@Defn2{Term=[subtype conformance],Sec=(required)}
@Chg{New=[A renaming-as-body is illegal if the declaration occurs before the
subprogram whose declaration it completes is frozen, and the renaming renames
the subprogram itself, through one or more subprogram renaming declarations,
none of whose subprograms has been frozen.],Old=[]}
@begin{Reason}
@ChgRef{Version=[1],Kind=[Revised]}
The @Chg{New=[otherwise part of the second sentence],Old=[first part of the first sentence]}
is to allow an implementation of a renaming-as-body
as a single jump instruction to the target subprogram.
Among other things, this prevents a subprogram from being completed with
a renaming of an entry.
(In most cases, the target of the jump can be filled in at link time.
In some cases, such as a renaming of a name like "A(I).@key[all]", an indirect
jump is needed. Note that the name is evaluated at renaming time, not at
call time.)

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0028],ARef=[AI95-00145-01]}
@Chg{New=[The first part of the second sentence is intended to allow
renaming-as-body of predefined operators before the @nt{subprogram_declaration}
is frozen. For some types (such as integer types), the parameter type for
operators is the base type, and it would be very strange for@*
@f{@ @ @ @key{function} Equal (A, B : @key{in} T) @key{return} Boolean;}@*
@f{@ @ @ @key{function} Equal (A, B : @key{in} T) @key{return} Boolean @key{renames} "=";}@*
to be illegal. (Note that predefined operators cannot be renamed this way
after the @nt{subprogram_declaration} is frozen, as they have convention
Intrinsic.)],Old=[]}

@ChgRef{Version=[1],Kind=[Revised]}
The @Chg{New=[],Old=[second part of the ]}first sentence is
the normal rule for completions of @nt{subprogram_declaration}s.
@end{Reason}
@begin{Ramification}
An @nt{entry_declaration}, unlike a @nt{subprogram_declaration},
cannot be completed with a @nt{renaming_@!declaration}.
Nor can a @nt{generic_@!subprogram_@!declaration}.

The syntax rules prevent a protected subprogram declaration from being
completed by a renaming.
This is fortunate, because it allows us to avoid worrying about whether
the implicit protected object parameter of a protected operation is
involved in the conformance rules.
@end{Ramification}
@begin{Reason}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0027],ARef=[AI95-00135-01]}
@ChgAdded{Version=[1],Text=[Circular renames before freezing is illegal, as the compiler
would not be able to determine the convention of the subprogram. Other
circular renames are handled below; see @BoundedTitle.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00228-01]}
@ChgAdded{Version=[2],Text=[The @Syni{callable_entity_}@nt{name} of a renaming
shall not denote a subprogram that requires overriding
(see @RefSecNum{Abstract Types and Subprograms}).]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00228-01]}
@ChgAdded{Version=[2],Text=[Such a rename cannot be of the inherited subprogram
(which requires overriding because it cannot be called),
and thus cannot squirrel away a subprogram (see below). That would be
confusing, so we make it illegal. The renaming is allowed after the
overriding, as then the @nt{name} will denote the overriding subprogram,
not the inherited one.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00228-01]}
@ChgAdded{Version=[2],Text=[The @Syni{callable_entity_}@nt{name} of a
renaming-as-body shall not denote an abstract subprogram.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00228-01]}
@ChgAdded{Version=[2],Text=[Such a subprogram has no body, so it hardly
can replace one in the program.]}
@end{Reason}

A @nt{name} that denotes a formal parameter
of the @nt{subprogram_specification}
is not allowed within the @Syni{callable_entity_}@nt{name}.
@begin{Reason}
@leading@keepnext@;This is to prevent things like this:
@begin{Example}
@key[function] F(X : Integer) @key[return] Integer @key[renames] Table(X).@key[all];
@end{Example}

@begin{WideAbove}
@leading@;A similar rule in @RefSecNum{Subprogram Declarations}
forbids things like this:
@end{WideAbove}
@begin{Example}
@key[function] F(X : Integer; Y : Integer := X) @key[return] Integer;
@end{Example}
@end{Reason}
@end{Legality}

@begin{StaticSem}
A renaming-as-declaration
declares a new view of the renamed entity.
The profile of this new view takes its subtypes, parameter modes,
and calling convention from the original profile of the
callable entity, while taking the formal parameter
@nt{name}s and @nt{default_expression}s from the profile given in the
@nt{subprogram_renaming_declaration}.
The new view is a function or procedure, never an entry.
@begin{Honest}
When renaming an entry as a procedure,
the compile-time rules apply as if the new view is a procedure,
but the run-time semantics of a call are that of an entry call.
@end{Honest}
@begin{Ramification}
For example, it is illegal for the @nt{entry_call_statement} of a
@nt{timed_entry_call} to call the new view.
But what looks like a procedure call will do things like barrier
waiting.

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0105],ARef=[AI95-00211-01]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00228-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0095-1]}
@Chg{New=[All properties of the renamed entity are inherited by the new view
unless otherwise stated by this International Standard. In particular, if the
renamed entity is abstract@Chg{Version=[2],New=[],Old=[ or requires
overriding (see @RefSecNum{Abstract Types and Subprograms})]}, the new view
also is abstract@Chg{Version=[2],New=[.],Old=[ or requires overriding. (The
renaming will often be illegal in these cases,
as a renaming cannot be overridden.)]}],Old=[]}
@ChgAdded{Version=[3],Text=[Similarly, if the renamed entity is not a program
unit, then neither is the renaming. (Implicitly declared subprograms are not
program units, see @RefSecNum{Separate Compilation}).]}
@end{Ramification}
@end{StaticSem}

@begin{RunTime}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0014],ARef=[AI95-00064-01]}
@Chg{New=[For a call to a subprogram whose body is given as a renaming-as-body,
the execution of the renaming-as-body is equivalent to the execution of a
@nt{subprogram_body} that simply calls the renamed subprogram with its formal
parameters as the actual parameters and, if it is a function, returns the
value of the call.],Old=[]}
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Added]}
@ChgAdded{Version=[1],Text=[This implies that the subprogram completed by the
renaming-as-body has its own elaboration check.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0123-1]}
For a call on a renaming of a dispatching subprogram that is overridden,
if the overriding occurred before the renaming, then the body executed
is that of the overriding declaration,
even if the overriding declaration is not visible at the place of the renaming;
otherwise, the inherited or predefined subprogram is
called.@Chg{Version=[3],New=[ A corresponding rule applies to a call on a
renaming of a predefined equality operator for an untagged record type.],Old=[]}
@begin{Discussion}
Note that whether or not the renaming is itself primitive has
nothing to do with the renamed subprogram.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0123-1]}
Note that the above rule is only for tagged types@Chg{Version=[3],New=[ and
equality of untagged record types],Old=[]}.

@leading@keepnext@;Consider the following example:
@begin{Example}
@key[package] P @key[is]
    @key[type] T @key[is] @key[tagged] @key[null] @key[record];
    @key[function] Predefined_Equal(X, Y : T) @key[return] Boolean @key[renames] "=";
@key[private]
    @key[function] "="(X, Y : T) @key[return] Boolean; --@RI{ Override predefined "=".}
@key[end] P;

@key[with] P; @key[use] P;
@key[package] Q @key[is]
    @key[function] User_Defined_Equal(X, Y : T) @key[return] Boolean @key[renames] P."=";
@key[end] Q;
@end{Example}

A call on Predefined_Equal will execute the predefined equality operator
of T, whereas a call on User_Defined_Equal will execute the body of the
overriding declaration in the private part of P.

Thus a renaming allows one to squirrel away a copy of an inherited or
predefined subprogram before later overriding it.@Defn{squirrel away}
@end{Discussion}
@end{RunTime}

@begin{Bounded}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0027],ARef=[AI95-00135-01]}
@ChgAdded{Version=[1],Text=[@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
@Defn2{Term=[Storage_Error],Sec=(raised by failure of run-time check)}
If a subprogram directly or indirectly renames itself, then it is a bounded
error to call that subprogram. Possible consequences are that Program_Error or
Storage_Error is raised, or that the call results in infinite recursion.]}
@begin{Reason}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0027],ARef=[AI95-00135-01]}
@ChgAdded{Version=[1],Text=[This has to be a bounded error, as it is possible
for a renaming-as-body appearing in a package body to cause this problem.
Thus it is not possible in general to detect this problem at compile time.]}
@end{Reason}
@end{Bounded}

@begin{Notes}
A procedure can only be renamed as a procedure.
A function whose @nt{defining_designator} is either an
@nt{identifier} or an @nt{operator_symbol}
can be renamed with either an
@nt{identifier} or an @nt{operator_symbol};
for renaming as an operator, the subprogram specification given in
the @nt{renaming_declaration} is subject to the rules given in
@RefSecNum{Overloading of Operators}
for operator declarations. Enumeration literals can be
renamed as functions; similarly, @nt{attribute_reference}s that
denote functions (such as references to Succ and Pred) can be renamed
as functions. An entry can only be renamed as a procedure; the new
@nt{name} is only allowed to appear in contexts that allow a
procedure @nt{name}. An entry of a family can be renamed, but an
entry family cannot be renamed as a whole.


The operators of the root numeric types cannot be renamed because the
types in the profile are anonymous, so the corresponding specifications
cannot be written; the same holds for certain attributes, such as Pos.


Calls with the new @nt{name} of a renamed entry are
@nt{procedure_call_statement}s and are not allowed at places
where the syntax requires an @nt{entry_call_statement} in
@ntf{conditional_} and @nt{timed_entry_call}s,
nor in an @nt{asynchronous_select}; similarly, the Count
attribute is not available for the new @nt{name}.

The primitiveness of a renaming-as-declaration is determined by its
profile, and by where it occurs, as for any declaration of
(a view of) a subprogram;
primitiveness is not determined by the renamed view.
In order to perform a dispatching call,
the subprogram name has to denote a primitive subprogram,
not a nonprimitive renaming of a primitive subprogram.
@begin{Reason}
A @nt{subprogram_renaming_declaration} could more properly be called
@ntf{renaming_@!as_@!subprogram_@!declaration}, since you're renaming something
as a subprogram, but you're not necessarily renaming a subprogram.
But that's too much of a mouthful. Or, alternatively, we could call it a
@ntf{callable_@!entity_@!renaming_@!declaration}, but that's even worse.
Not only is it a mouthful, it emphasizes the entity being renamed,
rather than the new view, which we think is a bad idea.
We'll live with the oddity.
@end{Reason}
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Examples of subprogram renaming declarations:}
@begin{Example}
@key[procedure] My_Write(C : @key[in] Character) @key[renames] Pool(K).Write; --@RI{  see @RefSecNum{Selected Components}}

@key[function] Real_Plus(Left, Right : Real   ) @key[return] Real    @key[renames] "+";
@key[function] Int_Plus (Left, Right : Integer) @key[return] Integer @key[renames] "+";

@key[function] Rouge @key[return] Color @key[renames] Red;  --@RI{  see @RefSecNum{Enumeration Types}}
@key[function] Rot   @key[return] Color @key[renames] Red;
@key[function] Rosso @key[return] Color @key[renames] Rouge;

@key[function] Next(X : Color) @key[return] Color @key[renames] Color'Succ; --@RI{ see @RefSecNum{Enumeration Types}}
@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Example of a subprogram renaming declaration with new parameter names:}
@end{WideAbove}
@begin{Example}
@key[function] "*" (X,Y : Vector) @key[return] Real @key[renames] Dot_Product; --@RI{ see @RefSecNum{Subprogram Declarations}}
@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Example of a subprogram renaming declaration with a new default expression:}
@end{WideAbove}
@begin{Example}
@key[function] Minimum(L : Link := Head) @key[return] Cell @key[renames] Min_Cell; --@RI{ see @RefSecNum{Subprogram Declarations}}
@end{Example}
@end{Examples}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0028],ARef=[AI95-00145-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @b<Corrigendum:> Allowed a renaming-as-body to be just
  mode conformant with the specification if the subprogram is not yet frozen.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00218-03]}
  @ChgAdded{Version=[2],Text=[@nt{Overriding_indicator} (see
  @RefSecNum{Overriding Indicators}) is
  optionally added to subprogram renamings.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0014],ARef=[AI95-00064-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Described the semantics of
  renaming-as-body, so that the location of elaboration checks is clear.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0027],ARef=[AI95-00135-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that circular
  renaming-as-body is illegal (if it can be detected in time) or a
  bounded error.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00228-01]}
  @ChgAdded{Version=[2],Text=[@B[Amendment Correction:] Clarified that
  renaming a shall-be-overridden
  subprogram is illegal, as well as renaming-as-body an abstract subprogram.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00423-01]}
  @ChgAdded{Version=[2],Text=[Added matching rules for @nt{null_exclusion}s.]}
@end{DiffWord95}

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0123-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}
  Renaming of user-defined untagged record equality is now defined to call the
  overridden body so long as the overriding occurred before the renames.
  This could change the body called in unusual cases; the change is necessary
  to preserve the principle that the body called for an explicit call to
  "=" (via a renames in this case) is the same as the one inherited for a
  derived type and used in generics. Note that any renamings before the
  overriding will be unchanged. Any differences caused by the change will be
  rare and most likely will fix a bug.]}
@end{Inconsistent2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in a
  @nt{subprogram_renaming_declaration}.
  This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}


@ISOOnlyRMNewPageVer{Version=[3]}@Comment{For ISO version of Ada 2012 Standard}
@LabeledSubClause{Generic Renaming Declarations}

@begin{Intro}
@redundant[A @nt{generic_renaming_declaration} is used to rename a generic unit.]
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{tabs=[P22], lhs=<generic_renaming_declaration>,rhs="
    @key{generic package}@\@Syn2{defining_program_unit_name} @key{renames} @SynI{generic_package_}@Syn2{name}@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]};
  | @key{generic procedure}@\@Syn2{defining_program_unit_name} @key{renames} @SynI{generic_procedure_}@Syn2{name}@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]};
  | @key{generic function}@\@Syn2{defining_program_unit_name} @key{renames} @SynI{generic_function_}@Syn2{name}@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]};"}
@end{Syntax}

@begin{Legality}
The renamed entity shall be a generic unit of the corresponding kind.
@end{Legality}

@begin{StaticSem}
A @nt{generic_renaming_declaration} declares a new view
@Redundant{of the renamed generic unit}.
@end{StaticSem}

@begin{Notes}
Although the properties of the new view are the same as those of the
renamed view, the place where the @nt<generic_renaming_declaration> occurs
may affect the legality of subsequent renamings and instantiations
that denote the @nt<generic_renaming_declaration>,
in particular if the renamed generic unit is a library unit
(see @RefSecNum{Compilation Units - Library Units}).
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of renaming a generic unit:}
@begin{Example}
@key[generic package] Enum_IO @key[renames] Ada.Text_IO.Enumeration_IO;  @RI{-- see @RefSecNum{Input-Output for Enumeration Types}}
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
Renaming of generic units is new to Ada 95.
It is particularly important for renaming child library
units that are generic units. For example, it might
be used to rename Numerics.Generic_Elementary_Functions as simply
Generic_Elementary_Functions, to match the name for
the corresponding Ada-83-based package.
@end{Extend83}

@begin{DiffWord83}
The information in RM83-8.6, @lquotes@;The Package Standard,@rquotes@;
has been updated for the child unit feature,
and moved to @RefSecNum{Predefined Language Environment},
except for the definition of @lquotes@;predefined type,@rquotes@;
which has been moved to @RefSecNum{Type Declarations}.
@end{DiffWord83}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in a
  @nt{generic_renaming_declaration}.
  This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}


@LabeledClause{The Context of Overload Resolution}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@redundant[@Defn{overload resolution}
Because declarations can be overloaded,
it is possible for an occurrence of a usage name
to have more than one possible interpretation;
in most cases, ambiguity is disallowed.
This @Chg{Version=[3],New=[subclause],Old=[clause]} describes
how the possible interpretations resolve
to the actual interpretation.

@Defn{overloading rules}
Certain rules of the language (the @ResolutionTitle)
are considered @lquotes@;overloading rules@rquotes@;.
If a possible interpretation violates an overloading rule,
it is assumed not to be the intended interpretation;
some other possible interpretation
is assumed to be the actual interpretation.
On the other hand,
violations of nonoverloading rules do not affect which
interpretation is chosen; instead,
they cause the construct to be illegal.
To be legal, there usually has to be exactly one acceptable
interpretation of a construct that is a @lquotes@;complete context@rquotes@;,
not counting any nested complete contexts.

@Defn2{Term=[grammar],Sec=(resolution of ambiguity)}
The syntax rules of the language and the visibility rules
given in @RefSecNum{Visibility}
determine the possible interpretations.
Most type checking rules
(rules that require a particular type,
or a particular class of types,
for example)
are overloading rules.
Various rules for the matching of formal and actual parameters are
overloading rules.]
@end{Intro}

@begin{MetaRules}
The type resolution rules are
intended to minimize the need for implicit declarations
and preference rules associated with implicit conversion and dispatching
operations.
@end{MetaRules}

@begin{Resolution}
@leading@Defn{complete context}
@Redundant{Overload resolution is applied separately to each
@i{complete context},
not counting inner complete contexts.}
Each of the following constructs is a @i{complete context}:
@begin{itemize}
A @nt{context_item}.

A @nt{declarative_item} or declaration.
@begin{Ramification}
A @nt{loop_parameter_specification} is a declaration,
and hence a complete context.
@end{Ramification}

A @nt{statement}.

A @nt{pragma_argument_association}.
@begin{Reason}
  We would make it the whole @nt{pragma},
  except that certain pragma arguments are allowed to be ambiguous,
  and ambiguity applies to a complete context.
@end{Reason}

@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0040-1]}
The @Chg{Version=[4],New=[@SynI[selecting_]@nt{expression}],Old=[@nt{expression}]}
of a @nt{case_statement}@Chg{Version=[4],New=[ or @nt{case_expression}],Old=[]}.
@begin{Ramification}
  This means that the @nt{expression} is resolved without looking
  at the choices.
@end{Ramification}
@end{itemize}

@leading@Defn2{Term=[interpretation], Sec=(of a complete context)}
@Defn2{Term=[overall interpretation], Sec=(of a complete context)}
An (overall) @i{interpretation} of a complete context
embodies its meaning, and includes
the following information about the constituents of the complete
context,
not including constituents of inner complete contexts:
@begin{Itemize}
for each constituent of the complete context,
to which syntactic categories it belongs,
and by which syntax rules; and
@begin{Ramification}
Syntactic categor@i{ies} is plural here,
because there are lots of trivial productions @em
an @nt{expression} might also be all of the following,
in this order: @nt{identifier},
@nt{name},
@nt{primary},
@nt{factor},
@nt{term},
@nt{simple_expression}, and
@nt{relation}.
Basically, we're trying to capture all the information in the parse tree
here, without using compiler-writer's jargon like @lquotes@;parse tree@rquotes@;.
@end{Ramification}

for each usage name, which declaration it denotes
(and, therefore, which view and which entity it denotes); and
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00382-01]}
In most cases, a usage name denotes the view declared by the denoted
declaration.
However, in certain cases, a usage name that denotes a declaration and
appears inside the declarative region of that same declaration, denotes
the current instance of the declaration.
For example, within a @nt{task_body}@Chg{Version=[2],New=[ other than in
an @nt{access_definition}],Old=[]}, a usage name that denotes the
@nt{task_type_declaration} denotes the object containing the
currently executing task,
and not the task type declared by the declaration.
@end{Ramification}

for a complete context that is a @nt{declarative_item},
whether or not it is a completion of a declaration,
and (if so) which declaration it completes.
@end{Itemize}
@begin{Ramification}
Unfortunately, we are not confident that the above list is complete.
We'll have to live with that.
@end{Ramification}
@begin{Honest}
For @lquotes@;possible@rquotes@; interpretations, the above information is tentative.
@end{Honest}
@begin{Discussion}
A possible interpretation (an @i{input} to overload
resolution) contains information about what a
usage name @i{might} denote, but what it actually @i{does} denote
requires overload resolution to determine.
Hence the term @lquotes@;tentative@rquotes@; is needed for possible interpretations;
otherwise, the definition would be circular.
@end{Discussion}

@Defn{possible interpretation}
A @i{possible interpretation} is one
that obeys the syntax rules and the visibility rules.
@Defn{acceptable interpretation}
@Defn2{Term=[resolve],Sec=(overload resolution)}
@Defn2{Term=[interpretation],Sec=(overload resolution)}
An @i{acceptable interpretation} is a possible interpretation that
obeys the @i{overloading rules}@Redundant{,
that is, those rules that specify an expected type or
expected profile, or specify how a construct shall @i(resolve)
or be @i(interpreted).}
@begin{Honest}
One rule that falls into this category,
but does not use the above-mentioned magic words,
is the rule about numbers of parameter associations in a call
(see @RefSecNum{Subprogram Calls}).
@end{Honest}
@begin{Ramification}
The @ResolutionName@;s are the ones that appear under the
@ResolutionTitle heading.
Some @SyntaxName@;s are written in English, instead of BNF.
No rule is a @SyntaxName or @ResolutionName unless it appears under the
appropriate heading.
@end{Ramification}

@Defn2{Term=[interpretation], Sec=(of a constituent of a complete context)}
The @i{interpretation} of a constituent of a complete context is
determined from the overall interpretation of the complete context as a
whole.
@Redundant{Thus,
for example, @lquotes@;interpreted as a @nt{function_call},@rquotes@;
means that the construct's interpretation says that it belongs
to the syntactic category @nt{function_call}.}

@leading@Defn{denote}
@Redundant[Each occurrence of]
a usage name @i{denotes} the declaration determined by its
interpretation.
It also denotes the view declared by its denoted
declaration, except in the following cases:
@begin{Ramification}
As explained below, a pragma argument is allowed to be ambiguous,
so it can denote several declarations,
and all of the views declared by those declarations.
@end{Ramification}
@begin(itemize)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00382-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0287-1]}
  @Defn2{Term=[current instance], Sec=(of a type)}
  If a usage name appears within the declarative region of a
  @nt{type_declaration} and denotes that same @nt{type_declaration},
  then it denotes the @i{current instance} of the type (rather than
  the type itself)@Chg{Version=[2],New=[; the],Old=[. The]} current
  instance of a type is the object or value
  of the type that is associated with the execution that
  evaluates the usage name.@Chg{Version=[2],New=[ @Chg{Version=[3],
  New=[ Similarly, if a usage name appears within the declarative region
  of a @nt{subtype_declaration} and denotes that same @nt{subtype_declaration},
  then it denotes the current instance of the subtype. These rules do],
  Old=[This rule does]} not apply if the usage name appears within the @nt{subtype_mark} of an
  @nt{access_definition} for an access-to-object type, or within the subtype
  of a parameter or result of an access-to-subprogram type.],Old=[]}
  @begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00382-01]}
  This is needed, for example, for references to the Access attribute
  from within the @nt{type_declaration}.
  Also, within a @nt{task_body} or @nt{protected_body},
  we need to be able to denote the current task or protected object.
  (For a @nt{single_task_declaration} or
  @nt{single_protected_declaration}, the rule about current instances
  is not needed.)@Chg{Version=[2],New=[ We exclude anonymous access types
  so that they can be used to create self-referencing types in the natural
  manner (otherwise such types would be illegal).],Old=[]}
  @end{Reason}
  @begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00382-01]}
  @ChgAdded{Version=[2],Text=[The phrase @lquotes@;within the @nt{subtype_mark}@rquotes@;
  in the @lquotes@;this rule does not apply@rquotes@; part is intended to cover
  a case like @key{access} T'Class appearing within the declarative region of
  T: here T denotes the type, not the current instance.]}
  @end{Discussion}

  @ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0068-1]}
  @ChgAdded{Version=[4],NoPrefix=[T],Text=[Within an @nt{aspect_specification}
  for a type or subtype, the current instance represents a value of the type;
  it is not an object. The nominal subtype of this value is given by the
  subtype itself (the first subtype in the case of a @nt{type_declaration}),
  prior to applying any predicate specified directly on the type or subtype. If
  the type or subtype is by-reference, the associated object with the value
  is the object associated (see @RefSecNum{Formal Parameter Modes}) with the
  execution of the usage name.]}

  @begin{Ramification}
    @ChgRef{Version=[4],Kind=[AddedNormal]}
    @ChgAdded{Version=[4],Text=[For the purposes of @LegalityTitle, the current
    instance acts as a value within an @nt{aspect_specification}. It might
    really be an object (and has to be for a by-reference type), but
    that isn't discoverable by direct use of the name of the current instance.]}
  @end{Ramification}

  @Defn2{Term=[current instance], Sec=(of a generic unit)}
  If a usage name appears within the declarative region of a
  @nt{generic_declaration} (but not within its @nt{generic_formal_part})
  and it denotes that same @nt{generic_declaration}, then it
  denotes the @i{current instance} of the generic unit (rather than
  the generic unit itself).
  See also @RefSecNum{Generic Instantiation}.
  @begin{Honest}
    The current instance of a generic unit is the instance created
    by whichever @nt{generic_instantiation} is of interest at any
    given time.
  @end{Honest}
  @begin{Ramification}
    Within a @nt{generic_formal_part}, a @nt{name} that denotes the
    @nt{generic_declaration} denotes the generic unit,
    which implies that it is not overloadable.
  @end{Ramification}
@end(itemize)

A usage name that denotes a view also denotes the entity of that view.
@begin{Ramification}
Usually, a usage name denotes only one declaration,
and therefore one view and one entity.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01]}
@leading@RootDefn[expected type]
The @i(expected type) for a given @nt<expression>, @nt<name>,
or other construct determines, according to the @i{type resolution
rules} given below, the types considered for the construct during
overload resolution.
@Defn{type resolution rules}
@Redundant[
The type resolution rules provide support for class-wide programming,
universal @Chg{Version=[2],New=[],Old=[numeric ]}literals, dispatching
operations, and anonymous access types:]
@begin{Ramification}
  Expected types are defined throughout the RM95.
  The most important definition is that, for a
  subprogram, the expected type for the
  actual parameter is the type of the formal parameter.

  The type resolution rules are trivial unless either the
  actual or expected type is universal, class-wide, or of
  an anonymous access type.
@end{Ramification}
@begin{Itemize}
@PDefn2{Term=[type resolution rules],
  Sec=(if any type in a specified class of types is expected)}
@PDefn2{Term=[type resolution rules],
  Sec=(if expected type is universal or class-wide)}
If a construct is expected to be of any type in a class of types,
or of the universal or class-wide type for a class,
then the type of the construct shall resolve to a type in that class
or to a universal type that covers the class.
@begin{Ramification}
This matching rule handles (among other things) cases like the
Val attribute, which denotes a function that takes a parameter of type
@i(universal_integer).

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00021 - Quoted text doesn't match}
The last part of the rule,
@lquotes@;or to a universal type that @Chg{New=[covers],Old=[includes]} the
class@rquotes implies that if the expected type for an expression is
@i{universal_fixed}, then an expression whose type is @i{universal_real}
(such as a real literal) is OK.
@end{Ramification}

@leading@PDefn2{Term=[type resolution rules],
  Sec=(if expected type is specific)}
If the expected type for a construct is a specific type @i(T), then the type
of the construct shall resolve either to @i(T), or:
@begin{Ramification}
@PDefn{Beaujolais effect}
This rule is @i{not} intended to create a preference for the specific
type @em such a preference would cause Beaujolais effects.
@end{Ramification}
@begin(Inneritemize)
    to @i(T)'Class; or
@begin{Ramification}
      This will only be legal as part of a call on a dispatching operation;
      see @RefSec(Dispatching Operations of Tagged Types).
      Note that that rule is not a @ResolutionName.
@end{Ramification}

    to a universal type that covers @i(T); or

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01],ARef=[AI95-00231-01],ARef=[AI95-00254-01],ARef=[AI95-00409-01]}
    when @i(T) is @Chg{Version=[2],New=[a specific],Old=[an]} anonymous
    access@Chg{Version=[2],New=[-to-object],Old=[]} type
    (see @RefSecNum{Access Types}) with designated type @i(D),
    to an access-to-@Chg{Version=[2],New=[object],Old=[variable]} type
    whose designated type is @i(D)'Class or is covered by
    @i(D)@Chg{Version=[2],New=[; or],Old=[.]}

@begin{Ramification}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00409-01]}
      @ChgNote{We use Chg here, rather than ChgDeleted so that the prefix is
      left behind.}@Chg{Version=[2],New=[],Old=[Because it says @lquotes@;access-to-variable@rquotes@;
      instead of @lquotes@;access-to-object,@rquotes@;
      two subprograms that differ only in that one has a parameter
      of an access-to-constant type,
      and the other has an
      access parameter,
      are distinguishable during overload resolution.]}

      The case where the actual is access-to-@i(D)'Class will only
      be legal as part of a call on a dispatching operation;
      see @RefSec(Dispatching Operations of Tagged Types).
      Note that that rule is not a @ResolutionName.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0149-1]}
@ChgAdded{Version=[3],Text=[when @i(T) is a named general access-to-object type
    (see @RefSecNum{Access Types}) with designated type @i(D), to an anonymous
    access-to-object type whose designated type covers or is covered by @i(D);
    or]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00254-01],ARef=[AI95-00409-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0239-1]}
@ChgAdded{Version=[2],Text=[when @i(T) is an anonymous access-to-subprogram
     type (see @RefSecNum{Access Types}), to an access-to-subprogram type
     whose designated profile is @Chg{Version=[3],New=[type conformant],Old=[type-conformant]} with that of @i{T}.]}

@end(Inneritemize)
@end{Itemize}


@RootDefn[expected profile]
In certain contexts,
@Redundant[such as in a @nt{subprogram_renaming_declaration},]
the @ResolutionTitle define an @i(expected profile) for a given
@nt<name>;
@Defn2{Term=[profile resolution rule],
  Sec=(@nt<name> with a given expected profile)}
in such cases, the @nt{name}
shall resolve to the name of a callable entity whose profile is type
conformant with the expected profile.
@Defn2{Term=[type conformance],Sec=(required)}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0239-1]}
  The parameter and result @i{sub}types are not used in overload
  resolution.
  Only type conformance of profiles
  is considered during overload resolution.
  Legality rules generally require at least @Chg{Version=[3],New=[mode conformance],Old=[mode-conformance]}
  in addition, but those rules are not used in overload resolution.
@end{Ramification}
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00332-01]}
@Defn2{Term=[single], Sec=[class expected type]}
When @Chg{Version=[2],New=[],Old=[the expected type for ]}a construct is
@Chg{Version=[2],New=[one that requires that its expected type],
Old=[required to]}
be a @i<single> type in a given class, the type
@Chg{Version=[2],New=[of],Old=[expected for]}
the construct shall be determinable solely
from the context in which the construct appears,
excluding the construct itself,
but using the requirement that it be in the given class@Chg{Version=[2],
New=[],Old=[; the type of the construct is then this single expected type]}.
Furthermore, the context shall not be one that expects any type in
some class that contains types of the given class;
in particular, the construct shall not be the operand of a
@nt{type_conversion}.

@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}@ChgNote{Null no longer requires a single type}
For example, the expected type for @Chg{Version=[2],New=[a string literal],
Old=[the literal @key{null}]} is required to be a
single @Chg{Version=[2],New=[string],Old=[access]} type.
But the expected type for the operand of a @nt{type_conversion} is
any type.
Therefore, @Chg{Version=[2],New=[a string literal],
Old=[the literal @key{null}]} is not allowed as the operand of a
@nt{type_conversion}.
This is true even if there is only one @Chg{Version=[2],New=[string],
Old=[access]} type in scope@Chg{Version=[2],New=[ (which is never the
case)],Old=[]}.
The reason for these rules is so that the compiler will not have to
search @lquotes@;everywhere@rquotes@; to see if there is exactly one type
in a class in scope.
@end{Ramification}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00332-01]}
  @ChgAdded{Version=[2],Text=[The first sentence is carefully worded so that it
  only mentions @lquotes@;expected type@rquotes as part of identifying the
  interesting case, but doesn't require that the context actually provide such
  an expected type. This allows such constructs to be used inside of constructs
  that don't provide an expected type (like qualified expressions and renames).
  Otherwise, such constructs wouldn't allow @nt{aggregate}s, 'Access, and so
  on.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0102-1],ARef=[AI05-0149-1],ARef=[AI05-0299-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0039-1]}
@ChgAdded{Version=[3],Text=[Other than for the
@Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
of a membership test, if the expected type for a @nt{name} or @nt{expression}
is not the same as the actual type of the @nt{name} or @nt{expression}, the
actual type shall be convertible to the expected type (see
@RefSecNum{Type Conversions});@Defn2{Term=[implicit conversion],
Sec=[legality]}@PDefn2{Term=[convertible],Sec=(required)} further, if the
expected type is a named access-to-object type with designated type @i<D1> and
the actual type is an anonymous access-to-object type with designated type
@i<D2>, then @i<D1> shall cover @i<D2>, and the @nt{name} or @nt{expression}
shall denote a view with an accessibility level for which the statically deeper
relationship applies@Redundant[; in particular it shall not denote an access
parameter nor a stand-alone access object].]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This rule prevents an implicit conversion that
  would be illegal if it was an explicit conversion. For instance, this
  prevents assigning an access-to-constant value into a stand-alone anonymous
  access-to-variable object. It also covers convertibility of the designated
  type and accessibility checks.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The rule also minimizes cases of implicit
  conversions when the tag check or the accessibility check might fail. We
  word it this way because access discriminants should also be disallowed if
  their enclosing object is designated by an access parameter.]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This rule does not apply to expressions that
  don't have expected types (such as the operand of a qualified expression or
  the expression of a renames). We don't need a rule like this in those cases,
  as the type needs to be the same; there is no implicit conversion.]}
@end{Ramification}

A complete context shall have at least one acceptable interpretation;
if there is exactly one, then that one is chosen.
@begin{Ramification}
This, and the rule below
about ambiguity, are the ones that suck in all the @SyntaxName@;s and
@ResolutionName@;s as compile-time rules.
Note that this and the ambiguity rule have to be @LegalityName@;s.
@end{Ramification}

@Defn2{Term=[preference], Sec=(for root numeric operators and @nt<range>s)}
There is a @i{preference} for the primitive operators (and @nt<range>s)
of the root numeric
types @i{root_integer} and @i{root_real}.
In particular,
if two acceptable interpretations of a constituent of a complete
context differ only in that one is for a primitive operator (or
@nt<range>) of the
type @i{root_integer} or @i{root_real}, and the other is not,
the interpretation using the primitive operator (or @nt<range>)
of the root numeric type is @i{preferred}.

@begin{Reason}
@leading@;The reason for this preference is so that expressions involving
literals and named numbers can be unambiguous.
For example, without the preference rule, the following would be ambiguous:
@begin{Example}
@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00022}
N : @key[constant] := 123;
@key[if] N > 100 @key[then] --@RI{ Preference for root_integer "@Chg{New=[>],Old=[<]}" operator.}
    ...
@key[end] @key[if];
@end{Example}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0149-1]}
@ChgAdded{Version=[3],Text=[Similarly,
there is a preference for the equality operators of the @i{universal_access}
type (see @RefSecNum{Relational Operators and Membership Tests}). If two
acceptable interpretations of a constituent of a
complete context differ only in that one is for an equality operator of the
@i{universal_access} type, and the other is not, the interpretation using the
equality operator of the @i{universal_access} type is
preferred.@Defn2{Term=[preference], Sec=(for universal access equality operators)}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This preference is necessary because of
  implicit conversion from an anonymous access type to a named access type,
  which would allow the equality operator of any named access type to be used
  to compare anonymous access values (and that way lies madness).]}
@end{Reason}

For a complete context, if there is exactly one
overall acceptable interpretation where each constituent's interpretation
is the same as or preferred (in the
above sense) over those in all other overall acceptable interpretations, then
that one overall acceptable interpretation is chosen.
@Defn{ambiguous}
Otherwise, the complete context is @i{ambiguous}.

A complete context other than a @nt{pragma_argument_association}
shall not be ambiguous.

A complete context that is a @nt{pragma_argument_association}
is allowed to be ambiguous (unless otherwise specified
for the particular pragma),
but only
if every acceptable interpretation of the pragma argument is as a
@nt{name} that statically denotes a callable entity.
@PDefn2{Term=[denote],Sec=(name used as a pragma argument)}
Such a @nt{name} denotes
all of the declarations determined by its interpretations,
and all of the views declared by these declarations.
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00224-01]}@ChgNote{Pragma is obsolete}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
This applies to Inline, Suppress,
Import, Export, and Convention @nt{pragma}s.
For example, it is OK to say @lquotes@;@key[pragma] @Chg{Version=[2],
New=[Export(C, Entity_Name],Old=[Suppress(Elaboration_Check, On]}
=> P.Q);@rquotes@;, even if there are two directly visible P's, and
there are two Q's declared in the visible part of each P.
In this case, P.Q denotes four different declarations.
This rule also applies to certain pragmas defined in the
Specialized Needs Annexes.
It almost applies to Pure, Elaborate_Body, and Elaborate_All @nt{pragma}s,
but those can't have overloading for other reasons.
@Chg{Version=[3],New=[ Note that almost all of
these pragmas are obsolescent (see @RefSecNum{Specific Suppression of Checks} and
@RefSecNum{Aspect-related Pragmas}), and a major reason is that this rule
has proven to be too broad in practice (it is common to want to specify something
on a single subprogram of an overloaded set, that can't be done easily with this
rule). @nt{Aspect_specification}s,
which are given on individual declarations, are preferred in Ada 2012.],Old=[]}

Note that if a pragma argument denotes a @i{call} to a callable
entity, rather than the entity itself,
this exception does not apply, and ambiguity is disallowed.

Note that we need to carefully define which pragma-related rules are
@ResolutionName@;s,
so that, for example, a @nt{pragma} Inline does not pick up
subprograms declared in enclosing declarative regions,
and therefore make itself illegal.

We say @lquotes@;statically denotes@rquotes@; in the above rule in order to avoid
having to worry about how many times the @nt{name} is evaluated,
in case it denotes more than one callable entity.
@end{Ramification}
@end{Legality}

@begin{Notes}
If a usage name has only one acceptable interpretation,
then it denotes the corresponding entity.
However, this does not mean that the usage name is necessarily legal
since other requirements exist which are not considered for overload
resolution; for example, the fact that an expression is static, whether
an object is constant, mode and subtype conformance rules, freezing
rules, order of elaboration, and so on.

@NoPrefix@;Similarly, subtypes are not considered for overload resolution (the
violation of a constraint does not make a program illegal but raises an
exception during program execution).
@end{Notes}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
@PDefn{Beaujolais effect}
The new preference rule for operators of root numeric types
is upward incompatible,
but only in cases that involved @i(Beaujolais) effects in Ada 83.
Such cases are ambiguous in Ada 95.
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
The rule that allows an expected type to match an actual expression of a
universal type,
in combination with the new preference rule for operators of root numeric
types, subsumes the Ada 83 "implicit conversion" rules
for universal types.
@end{Extend83}

@begin{DiffWord83}
In Ada 83, it is not clear what the @lquotes@;syntax rules@rquotes@; are.
AI83-00157 states that a certain textual rule is a syntax rule,
but it's still not clear how one tells in general which textual rules are
syntax rules.
We have solved the problem by stating exactly which
rules are syntax rules @em the ones that appear under the @lquotes@;@SyntaxTitle@rquotes@;
heading.

RM83 has a long list of the @lquotes@;forms@rquotes@; of rules that are to be
used in overload resolution (in addition to the syntax rules).
It is not clear exactly which rules fall under each form.
We have solved the problem by explicitly
marking all rules that are used in overload resolution.
Thus, the list of kinds of rules is unnecessary.
It is replaced with some introductory
(intentionally vague)
text explaining the basic idea
of what sorts of rules are overloading rules.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
It is not clear from RM83 what information is embodied in a @lquotes@;meaning@rquotes@;
or an @lquotes@;interpretation.@rquotes@;
@lquotes@;Meaning@rquotes@; and @lquotes@;interpretation@rquotes@; were intended to be synonymous;
we now use the latter only in defining the rules about overload
resolution.
@lquotes@;Meaning@rquotes@; is used only informally.
This @Chg{Version=[3],New=[subclause],Old=[clause]} attempts to clarify what
is meant by @lquotes@;interpretation.@rquotes@;

@NoPrefix@;For example,
RM83 does not make it clear that overload resolution is required in
order to match @ntf{subprogram_bodies} with their corresponding
declarations (and even to tell whether a given @nt{subprogram_body}
is the completion of a previous declaration).
Clearly, the information needed to do this is part of the
@lquotes@;interpretation@rquotes@; of a @nt{subprogram_body}.
The resolution of such things is defined in terms of
the @lquotes@;expected profile@rquotes@; concept.
Ada 95 has some new cases where expected profiles
are needed @em the resolution of P'Access,
where P might denote a subprogram,
is an example.

@Leading@NoPrefix@;RM83-8.7(2) might seem to imply that an interpretation
embodies information about what is denoted by each usage name,
but not information about which syntactic category each construct belongs to.
However, it seems necessary to include such information,
since the Ada grammar is highly ambiguous.
For example, X(Y) might be a @nt{function_call} or an
@nt{indexed_component}, and no context-free/syntactic information can
tell the difference.
It seems like we should view X(Y) as being, for example, @lquotes@;interpreted as a
@nt{function_call}@rquotes@; (if that's what overload resolution decides it is).
Note that there are examples where the denotation of each usage name
does not imply the syntactic category.
However, even if that were not true, it seems that intuitively,
the interpretation includes that information.
Here's an example:
@begin{Example}
@key[type] T;
@key[type] A @key[is] @key[access] T;
@key[type] T @key[is] @key[array](Integer @key[range] 1..10) @key[of] A;
I : Integer := 3;
@key[function] F(X : Integer := 7) @key[return] A;
Y : A := F(I); --@RI{ Ambiguous? (We hope so.)}
@end{Example}

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
@NoPrefix@;Consider the declaration of Y (a complete context).
In the above example, overload resolution can easily determine the
declaration, and therefore the entity,
denoted by Y, A, F, and I.
However, given all of that information,
we still don't know whether F(I) is a @nt{function_call}
or an @nt{indexed_component} whose @Chg{New=[@nt{prefix}],Old=[prefix]} is
a @nt{function_call}.
(In the latter case, it is equivalent to F(7).@key[all](I).)

@NoPrefix@;It seems clear that the declaration of Y ought to be considered
ambiguous.
We describe that by saying that there are two interpretations,
one as a @nt{function_call}, and one as an @nt{indexed_component}.
These interpretations are both acceptable to the overloading
rules.
Therefore, the complete context is ambiguous, and therefore illegal.

@PDefn{Beaujolais effect}
It is the intent that the Ada 95 preference rule for root numeric
operators is more locally enforceable than that of RM83-4.6(15).
It should also eliminate interpretation shifts due to the
addition or removal of a @nt{use_clause}
(the so called @i{Beaujolais} effect).

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
RM83-8.7 seems to be missing some complete contexts,
such as @nt{pragma_argument_association}s,
@nt{declarative_item}s that are not
declarations or @Chg{Version=[2],New=[@nt{aspect_clause}s],
Old=[@nt{representation_clause}s]},
and @nt{context_item}s.
We have added these, and also replaced the @lquotes@;must be determinable@rquotes@;
wording of RM83-5.4(3) with the notion that the expression of a
@nt{case_statement} is a complete context.

Cases like the Val attribute are now handled using the normal type
resolution rules, instead of having special cases that explicitly allow
things like @lquotes@;any integer type.@rquotes@;
@end{DiffWord83}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00409-01]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[@Defn{incompatibilities with Ada 95}
  Ada 95 allowed name resolution to distinguish between anonymous
  access-to-variable and access-to-constant types. This is similar to
  distinguishing between subprograms with @key{in} and @key{in out} parameters,
  which is known to be bad. Thus, that part of the rule was dropped as we now
  have anonymous access-to-constant types, making this much more likely.]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{type} Cacc @key{is access constant} Integer;
@key{procedure} Proc (Acc : @key{access} Integer) ...
@key{procedure} Proc (Acc : Cacc) ...
List : Cacc := ...;
Proc (List); -- @RI[OK in Ada 95, ambiguous in Ada 2005.]]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If there is any code like this (such code should
  be rare), it will be ambiguous in Ada 2005.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01],ARef=[AI95-00231-01],ARef=[AI95-00254-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Generalized the anonymous access resolution rules to support the new
  capabilities of anonymous access types (that is, access-to-subprogram and
  access-to-constant).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00382-01]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[We now allow the creation of
  self-referencing types via anonymous access types. This is an extension
  in unusual cases involving task and protected types. For example:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{task type} T;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{task body} T @key{is}
   @key{procedure} P (X : @key{access} T) @key{is} -- @RI[Illegal in Ada 95, legal in Ada 2005]
      ...
   @key{end} P;
@key{begin}
   ...
@key{end} T;]}
@end{Example}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00332-01]}
  @ChgAdded{Version=[2],Text=[Corrected the @lquotes@;single expected
  type@rquotes@; so that it works in contexts that don't have expected types
  (like object renames and qualified expressions). This fixes a hole in Ada 95
  that appears to prohibit using @nt{aggregate}s, 'Access, character literals,
  string literals, and @nt{allocator}s in qualified expressions.]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0149-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}Implicit
  conversion is now allowed from anonymous access-to-object types to
  general access-to-object types. Such conversions can make calls ambiguous.
  That can only happen when there are two visible subprograms with the same name
  and have profiles that differ only by a parameter that is of a named or
  anonymous access type, and the actual argument is of an anonymous access type.
  This should be rare, as many possible calls would be ambiguous even in Ada
  2005 (including @nt{allocator}s and any actual of a named access type if the
  designated types are the same).]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0149-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}Implicit conversion
  is allowed from anonymous access-to-object types to general access-to-object
  types if the designated type is convertible and runtime checks are minimized.
  See also the incompatibilities section.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0102-1]}
  @ChgAdded{Version=[3],Text=[Added a requirement
  here that implicit conversions are convertible to the appropriate type.
  This rule was scattered about the Standard, we moved a single generalized
  version here.]}
@end{DiffWord2005}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0068-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  Added a rule to specify that the current instance of a type or subtype is
  a value within an @nt{aspect_specification}. This could be inconsistent if
  a predicate or invariant uses the Constrained attribute on the current
  instance (it will always be False now, while it might have returned True
  in original Ada 2012). More likely, a usage of a current instance as a prefix
  of an attribute will become illegal (such as Size or Alignment). Any such
  code is very tricky. Moreover, as this is a new feature of Ada 2012, there
  are not that many predicates and invariants, and the ones that exist are
  very unlikely to be this tricky. Thus we do not believe that there will be
  any practical effect to this change, other than to explicitly allow
  common implementation strategies.]}
@end{Inconsistent2012}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0040-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Added wording to clarify that
  the @SynI{selecting_}@nt{expression} of a @nt{case_expression} is a
  complete context, just like that of a @nt{case_statement}. Clearly, everyone
  expects these to work the same way. Moreover, since it would be a lot of extra
  work to treat @nt{case_expression}s differently, it is quite unlikely that any
  compiler would implement the much more complicated resolution necessary (and
  we are not aware of any that did). Therefore, we didn't document this as a
  potential incompatibility.]}
@end{DiffWord2012}

