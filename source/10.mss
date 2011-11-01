@Part(10, Root="ada.mss")

@Comment{$Date: 2011/11/01 05:34:03 $}
@LabeledSection{Program Structure and Compilation Issues}

@Comment{$Source: e:\\cvsroot/ARM/Source/10.mss,v $}
@Comment{$Revision: 1.98 $}
@Comment{Corrigendum changes added, 2000/04/24, RLB}

@begin{Intro}
@redundant[The overall structure of programs and the facilities for separate
compilation are described in this section.
A @i(program) is a set of @i(partitions), each of which
may execute in a separate address space, possibly on a separate
computer.
@ToGlossary{Term=<Program>,
  Text=<A @i(program) is a set of @i(partitions), each of which
  may execute in a separate address space, possibly on a separate
  computer.
  A partition consists of a set of library units.>}
@ToGlossary{Term=<Partition>,
  Text=<A @i(partition) is a part of a program.
  Each partition consists of a set of library units.
  Each partition may run in a separate address space,
  possibly on a separate computer.
  A program may contain just one partition.
  A distributed program typically contains multiple partitions,
  which can execute concurrently.>}

@Defn2{Term=[library unit],Sec=(informal introduction)}
@Defn2{Term=[library_item],Sec=(informal introduction)}
@Defn2{Term=[library],Sec=(informal introduction)}
As explained below,
a partition is constructed from @i{library units}.
Syntactically, the declaration of a library unit is a @nt{library_item},
as is the body of a library unit.
An implementation may support a concept of a @i{program library}
(or simply, a @lquotes@;library@rquotes@;),
which contains @nt{library_item}s
and their subunits.
@IndexSee{Term=[program library],See=(library)}
Library units may be organized into a hierarchy of
children, grandchildren, and so on.]

This section has two clauses:
@RefSec{Separate Compilation}
discusses compile-time issues related to separate compilation.
@RefSec{Program Execution}
discusses issues related to what is traditionally known as @lquotes@;link time@rquotes@;
and @lquotes@;run time@rquotes@; @em building and executing partitions.

@end{Intro}

@begin{MetaRules}
@Defn{avoid overspecifying environmental issues}
We should avoid specifying details that are outside the domain of the
language itself.
The standard is intended (at least in part)
to promote portability of Ada programs at the source level.
It is not intended to standardize extra-language issues such as how one
invokes the compiler (or other tools),
how one's source is represented and organized,
version management, the format of error messages, etc.

@Defn{safe separate compilation}
@Defn2{Term=[separate compilation], Sec=(safe)}
The rules of the language should be enforced
even in the presence of separate compilation.
Using separate compilation should not make a
program less safe.

@Defn{legality determinable via semantic dependences}
It should be possible to determine the legality of a
compilation unit by looking only at the compilation unit itself
and the compilation units upon which it depends semantically.
As an example, it should be possible to analyze the legality of
two compilation units in parallel if they do not depend semantically
upon each other.

On the other hand, it may be necessary to look outside that set in
order to generate code @em this is generally true for
generic instantiation and inlining, for example.
Also on the other hand,
it is generally necessary to look outside that set in
order to check @LinkTimeName@;s.

See also the @lquotes@;generic contract model@rquotes@; @MetaRulesName of
@RefSec{Generic Instantiation}.
@end{MetaRules}

@begin{DiffWord83}
The section organization mentioned above is different from that of
RM83.
@end{DiffWord83}

@LabeledClause{Separate Compilation}

@begin{Intro}
@redundant[@Defn{separate compilation}
@Defn2{Term=[compilation], Sec=(separate)}
@ToGlossaryAlso{Term=<Program unit>,
  Text=<A @i(program unit) is either a package, a task unit,
  a protected unit, a protected entry, a generic unit,
  or an explicitly declared subprogram other than an enumeration
  literal.
  Certain kinds of program units can be separately compiled.
  Alternatively, they can appear physically nested within
  other program units.>}

@ToGlossaryAlso{Term=<Compilation unit>,
  Text=<The text of a program can be submitted to the compiler in one or
  more @nt(compilation)s.
  Each @nt(compilation) is a succession of @nt(compilation_unit)s.
  A @nt(compilation_unit) contains either
  the declaration, the body, or a renaming of a program unit.>}]
The representation for a @nt<compilation> is implementation-defined.
@ImplDef{The representation for a @nt{compilation}.}
@begin{Ramification}
Some implementations might choose to make a
@nt{compilation} be a source (text) file.
Others might allow multiple source files to be automatically
concatenated to form a single @nt{compilation}.
Others still may represent the source in a nontextual form such as a parse
tree.
Note that the RM95 does not even define the concept of a
source file.

Note that a protected subprogram is a subprogram,
and therefore a program unit.
An instance of a generic unit is a program unit.

A protected entry is a program unit,
but protected entries cannot be separately compiled.
@end{Ramification}

@ToGlossaryAlso{Term=<Library unit>,
  Text=<A library unit is a separately compiled
  program unit,
  and is always a package, subprogram, or generic unit.
  Library units may have other (logically nested) library units as children,
  and may have other program units physically nested within them.
  @Defn(subsystem)
  A root library unit, together with its children and grandchildren
  and so on, form a @i(subsystem).>}

@end{Intro}

@begin{ImplPerm}
An implementation may impose implementation-defined restrictions
on @nt{compilation}s that contain multiple @nt<compilation_unit>s.
@ImplDef{Any restrictions on @nt<compilation>s that contain
multiple @nt<compilation_unit>s.}
@begin{Discussion}
For example, an implementation might disallow a @nt{compilation} that
contains two versions of the same compilation unit, or that
contains the declarations for library packages P1 and P2,
where P1 precedes P2 in the @nt<compilation> but P1 has a
@nt<with_clause> that mentions P2.
@end{Discussion}
@end{ImplPerm}

@begin{DiffWord83}
The interactions between language issues and environmental issues
are left open in Ada 95. The environment concept is new.
In Ada 83, the concept of the program library, for example,
appeared to be quite concrete,
although the rules had no force,
since implementations could get around them simply by defining various
mappings from the concept of an Ada program library to whatever data
structures were actually stored in support of separate compilation.
Indeed, implementations were encouraged to do so.

In RM83, it was unclear which was the official definition of
@lquotes@;program unit.@rquotes@;
Definitions appeared in RM83-5, 6, 7, and 9, but not 12.
Placing it here seems logical,
since a program unit is sort of a potential compilation unit.
@end{DiffWord83}

@LabeledSubClause{Compilation Units - Library Units}

@begin{Intro}
@redundant[A @nt{library_item} is a compilation unit that is the declaration, body,
or renaming of a library unit.
Each library unit (except Standard) has a @i{parent unit},
which is a library package or generic library package.]
@Defn2{Term=[child], Sec=(of a library unit)}
A library unit is a @i{child} of its parent unit.
The @i{root} library units are the children of
the predefined library package Standard.
@begin{Ramification}
Standard is a library unit.
@end{Ramification}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<compilation>,rhs="{@Syn2{compilation_unit}}"}


@Syn{lhs=<compilation_unit>,rhs="
    @Syn2{context_clause} @Syn2{library_item}
  | @Syn2{context_clause} @Syn2{subunit}"}


@Syn{lhs=<library_item>,rhs="[@key{private}] @Syn2{library_unit_declaration}
  | @Syn2{library_unit_body}
  | [@key{private}] @Syn2{library_unit_renaming_declaration}"}


@Syn{tabs=[P25], lhs=<library_unit_declaration>,rhs="
     @Syn2{subprogram_declaration}@\| @Syn2{package_declaration}
   | @Syn2{generic_declaration}@\| @Syn2{generic_instantiation}"}


@Syn{lhs=<library_unit_renaming_declaration>,rhs="
   @Syn2{package_renaming_declaration}
 | @Syn2{generic_renaming_declaration}
 | @Syn2{subprogram_renaming_declaration}"}


@Syn{lhs=<library_unit_body>,rhs="@Syn2{subprogram_body} | @Syn2{package_body}"}


@Syn{lhs=<parent_unit_name>,rhs="@Syn2{name}"}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00397-01]}
@ChgAdded{Version=[2],Text=[An @nt{overriding_indicator} is not allowed in a
@nt{subprogram_declaration}, @nt{generic_instantiation}, or
@nt{subprogram_renaming_declaration} that declares a library unit.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[All of the listed items syntactically include
  @nt{overriding_indicator}, but a library unit can never override anything.
  A majority of the ARG thought that allowing @key{not overriding} in that
  case would be confusing instead of helpful.]}
@end{Reason}
@end{Syntax}

@begin{Intro}
@Defn{library unit}
@PDefn{library}@Comment{There is a different definition of library in 10.1.4.}
@Seealso{Primary=[library],Other=[library level]}
@Seealso{Primary=[library],Other=[library unit]}
@Seealso{Primary=[library],Other=[library_item]}
A @i{library unit} is a program unit that is declared by a
@nt{library_item}.
When a program unit is a library unit,
the prefix @lquotes@;library@rquotes@; is used to refer to it (or @lquotes@;generic library@rquotes@;
if generic),
as well as to its declaration and body,
as in @lquotes@;library procedure@rquotes@;, @lquotes@;library @nt{package_body}@rquotes@;, or
@lquotes@;generic library package@rquotes@;.
@Defn{compilation unit}
The term @i{compilation unit} is used to refer to
a @nt{compilation_unit}.
When the meaning is clear from context,
the term is also used to refer to the @nt{library_item}
of a @nt{compilation_unit}
or to the @nt{proper_body} of a @nt{subunit}
@Redundant[(that is, the @nt{compilation_unit} without the
@nt{context_clause} and the @key[separate] (@nt{parent_unit_name}))].
@begin{Discussion}
@leading@keepnext@;In this example:
@begin{Example}
@key[with] Ada.Text_IO;
@key[package] P @key[is]
    ...
@key[end] P;
@end{Example}

the term @lquotes@;compilation unit@rquotes@; can refer to this text: @lquotes@;@key[with] Ada.Text_IO;
@key[package] P @key[is] ... @key[end] P;@rquotes@; or to this text:
@lquotes@;@key[package] P @key[is] ... @key[end] P;@rquotes@;. We use this shorthand
because it corresponds to common usage.

We like to use the word @lquotes@;unit@rquotes@; for declaration-plus-body things,
and @lquotes@;item@rquotes@; for declaration or body separately (as in
@nt{declarative_item}).
The terms @lquotes@;@nt{compilation_unit},@rquotes@; @lquotes@;compilation unit,@rquotes@;
and @lquotes@;@nt{subunit}@rquotes@; are exceptions to this rule.
We considered changing @lquotes@;@nt{compilation_unit},@rquotes@; @lquotes@;compilation unit@rquotes@;
to @lquotes@;@ntf{compilation_item},@rquotes@; @lquotes@;compilation item,@rquotes@;
respectively, but we decided not to.
@end{Discussion}

@Defn2{Term=[parent declaration], Sec=(of a @nt{library_item})}
@Defn2{Term=[parent declaration], Sec=(of a library unit)}
The @i{parent declaration} of a @nt{library_item}
(and of the library unit) is the declaration denoted
by the @nt{parent_@!unit_name}, if any, of the
@nt{defining_@!program_@!unit_name} of the @nt<library_item>.
@Defn{root library unit}
If there is no @nt{parent_@!unit_name},
the parent declaration is the declaration of Standard,
the @nt{library_item} is a @i{root} @nt{library_item}, and
the library unit (renaming) is a @i{root} library
unit (renaming).
The declaration and body of Standard itself have no parent declaration.
@Defn2{Term=[parent unit], Sec=(of a library unit)}
The @i{parent unit} of a @nt{library_item} or library unit is the
library unit declared by its parent declaration.
@begin{Discussion}
The declaration and body of Standard are presumed to exist from
the beginning of time, as it were.
There is no way to actually write them,
since there is no syntactic way to indicate lack of a parent.
An attempt to compile a package Standard would result in
Standard.Standard.
@end{Discussion}
@begin{Reason}
Library units (other than Standard)
have @lquotes@;parent declarations@rquotes@; and @lquotes@;parent units@rquotes@;.
Subunits have @lquotes@;parent bodies@rquotes@;.
We didn't bother to define the other possibilities:
parent body of a library unit,
parent declaration of a subunit,
parent unit of a subunit.
These are not needed,
and might get in the way of a correct definition of @lquotes@;child.@rquotes@;
@end{Reason}

@Redundant[The children of a library unit occur immediately
within the declarative region of the declaration of the library unit.]
@Defn2{Term=[ancestor], Sec=(of a library unit)}
The @i{ancestors} of a library unit are itself, its parent,
its parent's parent, and so on.
@Redundant[(Standard is an ancestor of every library unit.)]
@Defn{descendant}
The @i{descendant} relation is the inverse of the ancestor relation.
@begin{Reason}
These definitions are worded carefully to avoid defining subunits as
children. Only library units can be children.

We use the unadorned term @lquotes@;ancestors@rquotes@; here to concisely define both
@lquotes@;ancestor unit@rquotes@; and @lquotes@;ancestor declaration.@rquotes@;
@end{Reason}

@Defn{public library unit}
@Defn{public declaration of a library unit}
@Defn{private library unit}
@Defn{private declaration of a library unit}
A @nt<library_unit_declaration>
or a @nt<library_@!unit_@!renaming_@!declaration> is @i{private}
if the declaration is immediately preceded
by the reserved word @key{private};
it is otherwise @i{public}. A library unit is private or public
according to its declaration.
@Defn2{Term=[public descendant], Sec=(of a library unit)}
The @i{public descendants} of a library unit are the library unit
itself, and the public descendants of its public children.
@Defn2{Term=[private descendant], Sec=(of a library unit)}
Its other descendants are @i{private descendants}.
@begin{Discussion}
The first concept defined here is that a @nt<library_item> is either public
or private (not in relation to anything else @em it's just a property of
the library unit).
The second concept is that a @nt<library_item>
is a public descendant or
private descendant @i{of a given ancestor}.
A given @nt<library_item> can be a
public descendant of one of its ancestors,
but a private descendant of some other ancestor.

A subprogram declared by a @nt{subprogram_body}
(as opposed to a @nt{subprogram_declaration})
is always public,
since the syntax rules disallow the reserved word @key[private]
on a body.

Note that a private library unit is a @i{public}
descendant of itself,
but a @i{private} descendant of its parent.
This is because it is visible outside itself @em its
privateness means that it is not visible outside its
parent.

Private children of Standard are legal, and follow the normal rules.
It is intended that implementations might have some method for taking an
existing environment, and treating it as a package to be @lquotes@;imported@rquotes@; into
another environment, treating children of Standard in the imported environment
as children of the imported package.
@end{Discussion}
@begin{Ramification}
@leading@;Suppose we have a public library unit A,
a private library unit A.B, and a public library unit A.B.C.
A.B.C is a public descendant of itself and of A.B,
but a private descendant of A;
since A.B is private to A, we don't allow A.B.C to escape outside A
either.
This is similar to the situation that would occur with physical
nesting, like this:
@begin{Example}
@key[package] A @key[is]
@key[private]
    @key[package] B @key[is]
        @key[package] C @key[is]
        @key[end] C;
    @key[private]
    @key[end] B;
@key[end] A;
@end{Example}

Here, A.B.C is visible outside itself and outside A.B, but not outside A.
(Note that this example is intended to illustrate the visibility
of program units from the outside;
the visibility within child units is not quite identical to that of
physically nested units,
since child units are nested after their parent's declaration.)
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00217-06]}
@ChgAdded{Version=[2],Text=[For each library @nt{package_declaration} in the
environment, there is an implicit declaration of a
@i{limited view} of that library package.@Defn{limited view} The
limited view of a package contains:]}

@begin(Itemize)
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00217-06]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0129-1]}
@ChgAdded{Version=[2],Text=[For each nested
@nt{package_declaration}@Chg{Version=[3],New=[ immediately within the visible part],Old=[]},
a declaration of the limited view of that package, with the
same @nt{defining_program_unit_name}.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00217-06],ARef=[AI95-00326-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0108-1],ARef=[AI05-0129-1]}
@ChgAdded{Version=[2],Text=[For each @nt{type_declaration}
@Chg{Version=[3],New=[immediately within],Old=[in]} the visible
part@Chg{Version=[3],New=[ that is not an
@nt{incomplete_type_declaration}],Old=[]}, an incomplete view
of the type@Chg{Version=[3],New=[ with no @nt{discriminant_part}],Old=[]};
if the @nt{type_declaration} is tagged, then the view is a
tagged incomplete view.]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0108-1]}
  @ChgAdded{Version=[3],Text=[The incomplete view of a type does not have
  a discriminant_part even if the @nt{type_declaration} does have
  one. This is necessary because semantic analysis (and the associated
  dependence on @nt{with_clause}s) would be necessary
  to determine the types of the discriminants.]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0129-1]}
  @ChgAdded{Version=[3],Text=[No incomplete views of incomplete types are
  included in the limited view. The rules of
  @RefSecNum{Incomplete Type Declarations} ensure that the completion of any
  visible incomplete type is declared in the same visible part, so such
  an incomplete view would simply be redundant.]}
@end{Reason}
@end(Itemize)

@begin(Discussion)
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00217-06]}
@ChgAdded{Version=[2],Text=[The implementation model of a limited view is that it
can be determined solely from the syntax of the source of the unit, without
any semantic analysis. That allows it to be created without the semantic
dependences of a full unit, which is necessary for it to break mutual
dependences of units.]}
@end(Discussion)

@begin(Ramification)
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[The limited view does not include package instances and
their contents. Semantic analysis of a unit (and dependence on its
@nt{with_clause}s) would be needed to determine the contents of an instance.]}
@end(Ramification)

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[The limited view of a library @nt{package_declaration} is private if that
library @nt{package_declaration} is immediately preceded by the reserved word
@key{private}.]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@Redundant[There is no syntax for declaring limited
views of packages, because they are always implicit.] The implicit declaration
of a limited view of a library package @Redundant[is not the declaration of a
library unit (the library @nt{package_declaration} is); nonetheless, it] is a
@nt{library_item}. The implicit declaration of the limited view of a library
package forms an (implicit) compilation unit whose @nt{context_clause} is
empty.]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[A library @nt{package_declaration} is the completion of
the declaration of its limited view.]}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is notwithstanding the rule in
  @RefSecNum{Completions of Declarations} that says that implicit declarations
  don't have completions.]}
@end{Honest}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This rule explains where to find the completions
  of the incomplete views defined by the limited view.]}
@end{Reason}

@end{Intro}

@begin{Legality}
The parent unit of a @nt<library_item> shall be a
@Redundant[library] package or generic @Redundant[library] package.

If a @nt{defining_program_unit_name} of a given
declaration or body has a @nt{parent_unit_name},
then the given declaration or body shall be a @nt<library_item>.
The body of a program unit shall be a @nt{library_item} if and only
if the declaration of the program unit is a @nt<library_item>.
In a @nt{library_@!unit_@!renaming_@!declaration},
the @Redundant[(old)] @nt{name}
shall denote a @nt<library_item>.
@begin{Discussion}
We could have allowed nested program units to be children
of other program units;
their semantics would make sense.
We disallow them to keep things simpler
and because they wouldn't be particularly useful.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00217-06]}
A @nt{parent_unit_name} @Redundant[(which can be used within a
@nt<defining_program_unit_name> of a @nt<library_item>
and in the @key[separate] clause of a @nt<subunit>)],
and each of its @nt{prefix}es,
shall not denote a @nt{renaming_declaration}.
@Redundant[On the other hand,
a name that denotes a @nt{library_@!unit_@!renaming_@!declaration} is allowed
in a @Chg{Version=[2],New=[@nt{nonlimited_with_clause}],Old=[@nt{with_clause}]}
and other places where the name of a library unit is allowed.]

If a library package is an instance of
a generic package, then every child of the
library package shall either be itself an instance or be a renaming of
a library unit.
@begin{Discussion}
A child of an instance of a given generic unit
will often be an instance of a (generic) child of the given
generic unit.
This is not required, however.
@end{Discussion}
@begin{Reason}
@leading@;Instances are forbidden from having noninstance children for two reasons:
@begin(Enumerate)
We want all source code that can depend on information from
the private part of a library unit to be inside the "subsystem"
rooted at the library unit.
If an instance of a generic unit were allowed to have a noninstance
as a child, the source code of that child might depend on information
from the private part of the generic unit, even though it is outside
the subsystem rooted at the generic unit.

Disallowing noninstance children simplifies the description of the
semantics of children of generic packages.
@end(Enumerate)
@end{Reason}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0004-1]}
A child of a generic library package shall either be itself a generic unit
or be a renaming of some other child of the same generic unit.@Chg{Version=[3],
New=[],Old=[ The renaming of a child of a generic package shall occur
only within the declarative region of the generic package.]}


A child of a parent generic package shall be
instantiated or renamed only within the declarative region
of the parent generic.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00331-01]}
For each @Chg{Version=[2],New=[child @i<C>],
Old=[declaration or renaming of a generic unit as a child]} of some parent
generic package@Chg{Version=[2],New=[ @i<P>],Old=[]}, there is
a corresponding declaration @Chg{Version=[2],New=[@i<C>],Old=[]} nested
immediately within each instance @Chg{Version=[2],New=[of @i<P>. For the
purposes of this rule, if a child @i<C> itself has a child @i<D>, each
corresponding declaration for @i<C> has a corresponding
child @i<D>], Old=[of the parent]}.
@Redundant[@Chg{Version=[2],New=[The corresponding],Old=[This]}
declaration @Chg{Version=[2],New=[for a child within an instance ],Old=[]}is
visible only within the scope of a @nt{with_clause} that
mentions the @Chg{Version=[2],New=[(original) ],Old=[]}child generic unit.]

@begin{ImplNote}
Within the child, like anything nested in a generic unit,
one can make up-level references to the current instance of its
parent, and thereby gain access to the formal parameters of the
parent, to the types declared in the parent, etc.
This @lquotes@;nesting@rquotes@; model applies even within the
@nt{generic_formal_part} of the child,
as it does for a generic child of a nongeneric unit.
@end{ImplNote}
@begin{Ramification}

  Suppose P is a generic library package,
  and P.C is a generic child of P.
  P.C can be instantiated inside the declarative region of P.
  Outside P, P.C can be mentioned only in a @nt{with_clause}.
  Conceptually, an instance I of P is a package that has a nested
  generic unit called I.C.
  Mentioning P.C in a @nt{with_clause} allows I.C to be instantiated.
  I need not be a library unit,
  and the instantiation of I.C need not be a library unit.
  If I is a library unit, and an instance of I.C is a child of I,
  then this instance has to be called something other than C.

@end{Ramification}

A library subprogram shall not override a primitive subprogram.
@begin{Reason}
This prevents certain obscure anomalies.
For example, if a library subprogram were to override a subprogram
declared in its parent package, then in a compilation unit that depends
@i{in}directly on the library subprogram, the library subprogram could
hide the overridden operation from all visibility,
but the library subprogram itself would not be visible.

Note that even without this rule, such subprograms would be illegal for
tagged types, because of the freezing rules.
@end{Reason}

The defining name of a function that is a compilation unit
shall not be an @nt{operator_symbol}.
@begin{Reason}
  Since overloading is not permitted among compilation units,
  it seems unlikely that it would be useful to define one as
  an operator. Note that a subunit could be renamed within its
  parent to be an operator.
@end{Reason}

@end{Legality}

@begin{StaticSem}

A @nt<subprogram_renaming_declaration>
that is a @nt{library_@!unit_@!renaming_@!declaration} is a
renaming-as-declaration, not a renaming-as-body.



@leading@keepnext@redundant[There are two kinds of dependences among
compilation units:]
@begin{Itemize}
@redundant[The @i{semantic dependences}
(see below)
are the ones needed to check the
compile-time rules across compilation unit boundaries;
a compilation unit depends semantically on
the other compilation units needed to determine its legality.
The visibility rules are based on the semantic dependences.

The @i{elaboration dependences}
(see @RefSecNum{Program Execution})
determine the order of elaboration of @nt{library_item}s.]
@end{Itemize}
@begin{Discussion}
Don't confuse these kinds of dependences with the run-time
dependences among tasks and masters defined in
@RefSec{Task Dependence - Termination of Tasks}.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00217-06]}
@Defn2{Term=[semantic dependence], Sec=(of one compilation unit upon another)}
@Defn2{Term=[dependence], Sec=(semantic)}
A @nt{library_item} depends semantically upon its
parent declaration.
A subunit depends semantically upon its parent body.
A @nt{library_unit_body} depends semantically upon the corresponding
@nt{library_unit_declaration}, if any.
@Chg{Version=[2],New=[The declaration of the limited view of a
library package depends semantically upon the declaration of the
limited view of its parent.
The declaration of a library package depends semantically upon the
declaration of its limited view.],Old=[]}
A compilation unit depends semantically upon each @nt<library_item>
mentioned in a @nt{with_clause} of the compilation unit.
In addition, if a given compilation unit contains an @nt{attribute_reference}
of a type defined in another compilation unit,
then the given compilation unit depends semantically upon the
other compilation unit.
The semantic dependence relationship is transitive.

@begin{Discussion}
The @lquotes@;if any@rquotes@; in the third sentence is necessary
because library subprograms are not required to have a
@nt{subprogram_declaration}.
@end{Discussion}
@begin{Honest}
If a given compilation unit contains a
@nt{choice_parameter_specification},
then the given compilation unit depends semantically upon the
declaration of Ada.Exceptions.

If a given compilation unit contains a @nt{pragma} with an
argument of a type defined in another compilation unit,
then the given compilation unit depends semantically upon the
other compilation unit.
@end{Honest}
@begin{Discussion}
For example, a compilation unit containing
X'Address depends semantically upon the declaration of package
System.

For the Address attribute, this fixes a hole in Ada 83.
Note that in almost all cases, the dependence will need to exist
due to @nt{with_clause}s, even without this rule.
Hence, the rule has very little effect on programmers.

Note that the semantic dependence does not have the same effect as a
@nt{with_clause}; in order to denote a declaration in one of those
packages, a @nt{with_clause} will generally be needed.

Note that no special rule is needed for an
@nt{attribute_definition_clause}, since an expression after @key[use]
will require semantic dependence upon the compilation unit containing
the @nt{type_declaration} of interest.

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
@ChgAdded{Version=[2],Text=[Unlike a full view of a package, a limited view
does not depend semantically on units mentioned in @nt{with_clause}s of the
@nt{compilation_unit} that defines the package. Formally, this is
achieved by saying that the limited view has an empty @nt{context_clause}.
This is necessary so that they
can be useful for their intended purpose: allowing mutual dependences between
packages. The lack of semantic dependence limits the contents of a limited view
to the items that can be determined solely from the syntax of the source of the
package, without any semantic analysis. That allows it to be created without
the semantic dependences of a full package.]}
@end{Discussion}
@end{StaticSem}

@begin{RunTime}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00217-06]}
@ChgAdded{Version=[2],Text=[The elaboration of the declaration of the limited
view of a package has no effect.]}
@end{RunTime}

@begin{Notes}
A simple program may consist of a single compilation unit.
A @nt{compilation} need not have any compilation units;
for example, its text can consist of @nt{pragma}s.
@begin{Ramification}
Such @nt{pragma}s cannot have any arguments that are @nt{name}s,
by a previous rule of this subclause.
A @nt{compilation} can even be entirely empty,
which is probably not useful.

Some interesting properties of the three kinds of dependence:
The elaboration dependences also include the semantic dependences,
except that subunits are taken together with their parents.
The semantic dependences partly determine the order in which the
compilation units appear in the environment at compile time.
At run time, the order is partly determined by the elaboration
dependences.

@leading@;The model whereby a child is inside its parent's declarative region,
after the parent's declaration,
as explained in @RefSecNum{Declarative Region},
has the following ramifications:
@begin(itemize)
The restrictions on @lquotes@;early@rquotes@; use of a private type (RM83-7.4.1(4))
or a deferred constant (RM83-7.4.3(2))
do not apply to uses in child units, because they follow
the full declaration.

A library subprogram is never primitive, even if its profile includes a
type declared immediately within the parent's @nt{package_specification},
because the child is not declared immediately within the same
@nt{package_specification} as the type (so it doesn't declare a new
primitive subprogram), and because the child is forbidden from overriding
an old primitive subprogram. It is immediately within the same declarative
region, but not the same @nt{package_specification}. Thus, for a tagged type,
it is not possible to call a child subprogram in a dispatching manner.
(This is also forbidden by the freezing rules.)
Similarly, it is not possible for the user to declare primitive
subprograms of the types declared in the declaration of Standard,
such as Integer (even if the rules were changed to allow a library unit
whose name is an operator symbol).

When the parent unit is @lquotes@;used@rquotes@; the simple names of the
with'd child units are directly visible (see @RefSec{Use Clauses}).

When a parent body with's its own child, the defining name of
the child is directly visible, and the parent body
is not allowed to
include a declaration of a homograph of the child unit
immediately within the @nt{declarative_part} of the body
(RM83-8.3(17)).
@end(itemize)

Note that @lquotes@;declaration of a library unit@rquotes@;
is different from @lquotes@;@nt{library_unit_declaration}@rquotes@;
@em the former includes @nt{subprogram_body}.
Also, we sometimes really mean @lquotes@;declaration of a view of a
library unit@rquotes@;, which includes
@nt{library_@!unit_@!renaming_@!declaration}s.

The visibility rules generally imply that the renamed view of a
@nt{library_@!unit_@!renaming_@!declaration} has to be mentioned in a
@nt{with_@!clause} of the @nt{library_@!unit_@!renaming_@!declaration}.
@end{Ramification}
@begin{Honest}
The real rule is that the renamed library unit
has to be visible in the @nt{library_@!unit_@!renaming_@!declaration}.
@end{Honest}
@begin{Reason}
In most cases, @lquotes@;has to be visible@rquotes@; means there has to be a
@nt{with_clause}.
However, it is possible in obscure cases to avoid the need for a
@nt{with_clause}; in particular, a compilation unit such as
@lquotes@;@key[package] P.Q @key[renames] P;@rquotes@;
is legal with no @nt{with_clause}s
(though not particularly interesting).
ASCII is physically nested in Standard,
and so is not a library unit,
and cannot be renamed as a library unit.
@end{Reason}

The @nt{designator} of a library function cannot be an @nt{operator_symbol},
but a nonlibrary @nt{renaming_declaration} is allowed to rename a library
function as an operator.
Within a partition,
two library subprograms are required to have distinct
names and hence cannot overload each other.
However,
@nt{renaming_declaration}s are allowed to define overloaded names for
such subprograms, and a locally declared subprogram is allowed to
overload a library subprogram. The expanded name Standard.L can be
used to denote a root library unit L (unless the declaration of
Standard is hidden)
since root library unit declarations occur immediately within the
declarative region of package Standard.
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Examples of library units:}
@begin{Example}
@key[package] Rational_Numbers.IO @key[is]  --@RI[ public child of Rational_Numbers, see @RefSecNum{Package Specifications and Declarations}]
   @key[procedure] Put(R : @key[in]  Rational);
   @key[procedure] Get(R : @key[out] Rational);
@key[end] Rational_Numbers.IO;

@key[private procedure] Rational_Numbers.Reduce(R : @key[in out] Rational);
                                --@RI[ private child of Rational_Numbers]

@key[with] Rational_Numbers.Reduce;   --@RI[ refer to a private child]
@key[package body] Rational_Numbers @key[is]
   ...
@key[end] Rational_Numbers;

@key[with] Rational_Numbers.IO; @key[use] Rational_Numbers;
@key[with] Ada.Text_io;               --@RI[ see @RefSecNum{Text Input-Output}]
@key[procedure] Main @key[is]               --@RI[ a root library procedure]
   R : Rational;
@key[begin]
   R := 5/3;                    --@RI[ construct a rational number, see @RefSecNum{Package Specifications and Declarations}]
   Ada.Text_IO.Put("The answer is: ");
   IO.Put(R);
   Ada.Text_IO.New_Line;
@key[end] Main;

@key[with] Rational_Numbers.IO;
@key[package] Rational_IO @key[renames] Rational_Numbers.IO;
                                --@RI[ a library unit renaming declaration]
@end{Example}

Each of the above @nt{library_item}s can be submitted to the compiler
separately.
@begin{Discussion}
@leading@keepnext@i{Example of a generic package with children:}

@begin{Example}
@key[generic]
   @key[type] Element @key[is] @key[private];
   @key[with] @key[function] Image(E : Element) @key[return] String;
@key[package] Generic_Bags @key[is]
   @key[type] Bag @key[is] @key[limited] @key[private]; --@RI{ A bag of Elements.}
   @key[procedure] Add(B : @key[in] @key[out] Bag; E : Element);
   @key[function] Bag_Image(B : Bag) @key[return] String;
@key[private]
   @key[type] Bag @key[is] ...;
@key[end] Generic_Bags;

@key[generic]
@key[package] Generic_Bags.Generic_Iterators @key[is]
   ... --@RI{ various additional operations on Bags.}

   @key[generic]
      @key[with] @key[procedure] Use_Element(E : @key[in] Element);
         --@RI{ Called once per bag element.}
   @key[procedure] Iterate(B : @key[in] Bag);
@key[end] Generic_Bags.Generic_Iterators;
@end{Example}

@begin{WideAbove}
@leading@keepnext@;A package that instantiates the above generic units:
@end{WideAbove}
@begin{Example}
@key[with] Generic_Bags;
@key[with] Generic_Bags.Generic_Iterators;
@key[package] My_Abstraction @key[is]
    @key[type] My_Type @key[is] ...;
    @key[function] Image(X : My_Type) @key[return] String;
    @key[package] Bags_Of_My_Type @key[is] @key[new] Generic_Bags(My_Type, Image);
    @key[package] Iterators_Of_Bags_Of_My_Type @key[is] @key[new] Bags_Of_My_Type.Generic_Iterators;
@key[end] My_Abstraction;
@end{Example}

In the above example, Bags_Of_My_Type has a nested generic unit called
Generic_Iterators.
The second @nt{with_clause} makes that nested unit visible.

@leading@;Here we show how the generic body could depend on one of its own
children:
@begin{Example}
@key[with] Generic_Bags.Generic_Iterators;
@key[package] @key[body] Generic_Bags @key[is]
   @key[procedure] Add(B : @key[in] @key[out] Bag; E : Element) @key[is] ... @key[end] Add;

   @key[package] Iters @key[is] @key[new] Generic_Iterators;

   @key[function] Bag_Image(B : Bag) @key[return] String @key[is]
      Buffer : String(1..10_000);
      Last : Integer := 0;

      @key[procedure] Append_Image(E : @key[in] Element) @key[is]
         Im : @key[constant] String := Image(E);
      @key[begin]
         @key[if] Last /= 0 @key[then] --@RI{ Insert a comma.}
            Last := Last + 1;
            Buffer(Last) := ',';
         @key[end] @key[if];
         Buffer(Last+1 .. Last+Im'Length) := Im;
         Last := Last + Im'Length;
      @key[end] Append_Image;

      @key[procedure] Append_All @key[is] @key[new] Iters.Iterate(Append_Image);
   @key[begin]
      Append_All(B);
      @key[return] Buffer(1..Last);
   @key[end] Bag_Image;
@key[end] Generic_Bags;
@end{Example}

@end{Discussion}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The syntax rule for @nt{library_item} is modified to allow the
reserved word @key{private} before a @nt{library_unit_declaration}.

Children (other than children of Standard)
are new in Ada 95.

Library unit renaming is new in Ada 95.
@end{Extend83}

@begin{DiffWord83}
Standard is considered a library unit in Ada 95.
This simplifies the descriptions,
since it implies that the parent of each library unit is a library unit.
(Standard itself has no parent, of course.)
As in Ada 83, the language does not define any way to recompile
Standard, since the name given in the declaration of a library unit is
always interpreted in relation to Standard.
That is,
an attempt to compile a package Standard would result in
Standard.Standard.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The concept of a limited view is new. Combined with @nt{limited_with_clause}s
  (see @RefSecNum{Context Clauses - With Clauses}), they facilitate
  construction of mutually recursive types in multiple packages.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00331-01]}
  @ChgAdded{Version=[2],Text=[Clarified the wording so that a grandchild
  generic unit will work as expected.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0108-1],ARef=[AI05-0129-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified the wording so that
  it is clear that limited views of types never have discriminants and never
  are of incomplete types.]}
@end{DiffWord2005}


@LabeledSubClause{Context Clauses - With Clauses}

@begin{Intro}
@redundant[A @nt{context_clause} is used to specify
the @nt<library_item>s whose
names are needed within a compilation unit.]
@end{Intro}

@begin{MetaRules}
@Defn{one-pass @nt{context_clause}s}
The reader should be able to understand a @nt{context_clause} without
looking ahead.
Similarly, when compiling a @nt{context_clause},
the compiler should not have to look ahead at
subsequent @nt{context_item}s, nor at the compilation unit
to which the @nt{context_clause} is attached.
(We have not completely achieved this.)

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
@ChgAdded{Version=[2],Text=[@Defn{ripple effect}A @i<ripple effect> occurs
when the legality of a compilation unit could be affected by adding or removing
an otherwise unneeded @nt{with_clause} on some compilation unit on which the
unit depends, directly or indirectly. We try to avoid ripple effects because
they make understanding and maintenance more difficult. However, ripple effects
can occur because of direct visibility (as in child units); this seems
impossible to eliminate. The ripple effect for @nt{with_clause}s is somewhat
similar to the Beaujolais effect (see @RefSecNum{Use Clauses}) for
@nt{use_clause}s, which we also try to avoid.]}
@end{MetaRules}

@begin{Syntax}
@Syn{lhs=<context_clause>,rhs="{@Syn2{context_item}}"}


@Syn{lhs=<context_item>,rhs="@Syn2{with_clause} | @Syn2{use_clause}"}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00217-06],ARef=[AI95-00262-01]}
@Syn{lhs=<with_clause>,rhs="@Chg{Version=[2],New=<@Syn2{limited_with_clause} | @Syn2{nonlimited_with_clause}>,Old=<@key{with} @SynI{library_unit_}@Syn2{name} {, @SynI{library_unit_}@Syn2{name}};>}"}

@ChgRef{Version=[2],Kind=[Added]}
@AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=<limited_with_clause>,Old=<>}>,rhs="@Chg{Version=[2],New=<@key{limited} [@key{private}] @key{with} @SynI{library_unit_}@Syn2{name} {, @SynI{library_unit_}@Syn2{name}};>,Old=<>}"}

@ChgRef{Version=[2],Kind=[Added]}
@AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=<nonlimited_with_clause>,Old=<>}>,rhs="@Chg{Version=[2],New=<[@key{private}] @key{with} @SynI{library_unit_}@Syn2{name} {, @SynI{library_unit_}@Syn2{name}};>,Old=<>}"}

@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
@ChgAdded{Version=[2],Text=[A @nt{limited_with_clause} makes a limited view
of a unit visible.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00262-01]}
@ChgAdded{Version=[2],Text=[@Defn{private with_clause}A @nt{with_clause}
containing the reserved word @key{private} is called a @i{private with_clause}.
It can be thought of as making items visible only in the private part, although
it really
makes items visible everywhere except the visible part. It can be used both for
documentation purposes (to say that a unit is not used in the visible part),
and to allow access to private units that otherwise would be
prohibited.]}
@end{Discussion}
@end{Syntax}

@begin{Resolution}
@Defn2{Term=[scope], Sec=(of a @nt{with_clause})}
The @i{scope} of a @nt{with_clause} that appears on a
@nt{library_@!unit_@!declaration}
or @nt{library_@!unit_@!renaming_@!declaration}
consists of the entire declarative region of the declaration@Redundant[,
which includes all children and subunits].
The scope of a @nt{with_clause} that appears on a
body consists of the body@Redundant[, which includes all subunits].
@begin{Discussion}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00262-01]}
Suppose a @Chg{Version=[2],New=[non-private ],Old=[]} @nt{with_clause} of a
public library unit mentions one of its private siblings.
(This is only allowed on the body of the public library unit.)
We considered making the scope of that @nt{with_clause}
not include the visible part of the public library unit.
(This would only matter for a @nt{subprogram_body},
since those are the only kinds of body that have a visible part,
and only if the @nt{subprogram_body} completes a
@nt{subprogram_declaration}, since otherwise the @nt{with_clause}
would be illegal.)
We did not put in such a rule for two reasons:
(1) It would complicate the wording of the rules,
because we would have to split each @nt{with_clause}
into pieces, in order to correctly handle @lquotes@;@key[with] P, Q;@rquotes@;
where P is public and Q is private.
(2) The conformance rules prevent any problems.
It doesn't matter if a type name in the spec of the body denotes
the completion of a @nt{private_type_declaration}.

A @nt{with_clause} also affects visibility within subsequent
@nt{use_clause}s and @nt{pragma}s of the same @nt{context_clause},
even though those are not in the scope of the @nt{with_clause}.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00217-06]}
@Defn2{Term=[mentioned],Sec=[in a @nt<with_clause>]}
@Defn2{Term=[with_clause], Sec=(mentioned in)}
A @nt<library_item> @Chg{Version=[2],New=[(and the corresponding library
unit) ],Old=[]}is
@Chg{Version=[2],New=[@i{named}
@Defn2{Term=[named],Sec=[in a @nt<with_clause>]}
@Defn2{Term=[with_clause], Sec=(named in)}],Old=[@i{mentioned} ]}in a
@nt<with_clause> if it is denoted by
a @i(library_unit_)@nt<name> @Chg{Version=[2],New=[],Old=[or a @nt<prefix> ]}
in the @nt<with_clause>.
@Chg{Version=[2],New=[A @nt{library_item} (and the corresponding library unit)
is @i<mentioned> in a @nt{with_clause}
if it is named in the @nt{with_clause} or if it is denoted by a @nt{prefix} in
the @nt{with_clause}.],Old=[]}

@begin{Discussion}
 @nt{With_clause}s control the visibility of
declarations or renamings of library units.
Mentioning a root library unit in a
@nt{with_clause} makes its declaration
directly visible. Mentioning a non-root library unit
makes its declaration visible.
See Section 8 for details.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
Note that this rule implies that @lquotes@;@key{with} A.B.C;@rquotes@; is
@chg{Version=[2],New=[almost ],Old=[]}equivalent to
@lquotes@;@key{with} A, A.B, A.B.C;@rquotes@;@Chg{Version=[2],New=[.],Old=[]}
The reason for making a @nt{with_clause} apply to all the ancestor
units is to avoid @lquotes@;visibility holes@rquotes@; @em situations in which an inner
program unit is visible while an outer one is not.
Visibility holes would cause semantic complexity and implementation
difficulty.@Chg{Version=[2],New=[ (This
is not exactly equivalent because the latter @nt{with_clause} names
A and A.B, while the previous one does not. Whether a unit is
@ldquote@;named@rdquote does not have any effect on visibility, however,
so it is equivalent for visibility purposes.)],Old=[]}

@end{Discussion}

@redundant[Outside its own declarative region,
the declaration or renaming of
a library unit can be visible only within the scope of a
@nt{with_clause} that mentions it.
The visibility of the declaration or renaming of a
library unit otherwise
follows from its placement in the environment.]
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00262-01]}
If a @nt{with_clause} of a given @nt<compilation_unit> mentions
a private child of some library unit,
then the given @nt{compilation_unit} shall be @Chg{Version=[2],New=[one of:],
Old=[either the declaration of a private descendant of that library unit
or the body or subunit of a @Redundant[(public or private)] descendant of
that library unit.]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00262-01]}
@ChgAdded{Version=[2],Text=[the declaration, body, or subunit of a private
descendant of that library unit;]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00220-01],ARef=[AI95-00262-01]}
@ChgAdded{Version=[2],Text=[the body or subunit of a public descendant of that
library unit, but not a subprogram body acting as a subprogram declaration (see
@RefSecNum{The Compilation Process}); or]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00262-01]}
@ChgAdded{Version=[2],Text=[the declaration of a public descendant of that
library unit, in which case the @nt<with_clause> shall include the reserved word
@key<private>.]}
@end{Itemize}

@begin{Reason}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00262-01]}
The purpose of this rule is to prevent a private child from being
visible @Chg{Version=[2],New=[],Old=[(or even semantically depended-on) ]}from
outside the subsystem rooted at its parent. @Chg{Version=[2],New=[A private
child can be semantically depended-on without violating this principle if it is
used in a private @nt{with_clause}.],Old=[]}
@end{Reason}
@begin{Discussion}
This rule violates the one-pass @nt{context_clause}s
@MetaRulesName. We rationalize this by saying that at least that
@MetaRulesName works for legal compilation units.

@leading@keepnext@;Example:
@begin{Example}
@key[package] A @key[is]
@key[end] A;

@key[package] A.B @key[is]
@key[end] A.B;

@key[private] @key[package] A.B.C @key[is]
@key[end] A.B.C;

@key[package] A.B.C.D @key[is]
@key[end] A.B.C.D;

@key[with] A.B.C; -- @RI[(1)]
@key[private] @key[package] A.B.X @key[is]
@key[end] A.B.X;

@key[package] A.B.Y @key[is]
@key[end] A.B.Y;

@key[with] A.B.C; -- @RI[(2)]
@key[package] @key[body] A.B.Y @key[is]
@key[end] A.B.Y;

@ChgRef{Version=[2],Kind=[Added]}@ChgAdded{Version=[2],Text=[@key[private] @key[with] A.B.C; -- @RI[(3)]
@key[package] A.B.Z @key[is]
@key[end] A.B.Z;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00262-01]}
(1) is OK because it's a private child of A.B @em it would be illegal if
we made A.B.X a public child of A.B.
(2) is OK because it's the body of a child of A.B.
@Chg{Version=[2],New=[(3) is OK because it's a child of A.B, and it is a
private @nt{with_clause}.],Old=[]}
It would be illegal to say @lquotes@;@key[with] A.B.C;@rquotes@; on any
@nt{library_item} whose name does not start with @lquotes@;A.B@rquotes@;.
Note that mentioning A.B.C.D in a @nt{with_clause} automatically
mentions A.B.C as well,
so @lquotes@;@key[with] A.B.C.D;@rquotes@; is illegal in the same places as
@lquotes@;@key[with] A.B.C;@rquotes@;.
@end{Discussion}
@begin{Honest}
  @ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0005-1]}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00262-01]}
  @ChgDeleted{Version=[3],Text=[For the purposes of this rule,
  if a @nt{subprogram_body} has no preceding @nt{subprogram_declaration},
  the @nt{subprogram_body} should be considered a declaration and not a body.
  Thus, it is illegal for such a @nt{subprogram_body} to mention one of
  its siblings in a @Chg{Version=[2],New=[non-private ],Old=[]}@nt{with_clause}
  if the sibling is a private library unit.]}
@end{Honest}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00262-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0077-1],ARef=[AI05-0122-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[A @nt<name> denoting a
@Chg{Version=[3],New=[@nt{library_item} (or the corresponding declaration
for a child of a generic within an instance @em see
@RefSecNum{Compilation Units - Library Units}), if it],Old=[library item that]}
is visible only due to being mentioned in
one or more @nt<with_clause>s that include the reserved word
@key<private>@Chg{Version=[3],New=[,],Old=[]} shall appear only within:]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[a private part;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[a body, but not within the
@nt<subprogram_specification> of a library subprogram body;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[a private descendant of the unit on which one of these
@nt<with_clause>s appear; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[a pragma within a context clause.]}
@end{Itemize}

@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[These rules apply only if all of the
@nt{with_clause}s that mention the name include the reserved word
@key{private}. They do not apply if the name is mentioned in any
@nt{with_clause} that does not include @key{private}.]}
@end{Ramification}

@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0077-1]}
@ChgAdded{Version=[2],Text=[These rules make the
@Chg{Version=[3],New=[@nt{library_item}],Old=[library item]}
visible anywhere that
is not visible outside the subsystem rooted at the @nt{compilation_unit} having
the private @nt{with_clause}, including private parts of packages
nested in the visible part, private parts of child packages, the visible part
of private children, and context clause pragmas like Elaborate_All.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[We considered having the scope of a private
@nt{with_clause} not include the visible part. However, that rule would mean
that moving a declaration between the visible part and the private part could
change its meaning from one legal interpretation to a different legal
interpretation. For example:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} A @key{is}
    @key{function} B @key{return} Integer;
@key{end} A;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{function} B @key{return} Integer;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{with} A;
@key{private} @key{with} B;
@key{package} C @key{is}
    @key{use} A;
    V1 : Integer := B; -- (1)
@key{private}
    V2 : Integer := B; -- (2)
@key{end} C;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[If we say that library subprogram B is not in scope
in the visible part of C, then the B at (1) resolves to A.B, while (2)
resolves to library unit B. Simply moving a declaration could silently change
its meaning. With the legality rule defined above, the B at (1) is illegal.
If the user really meant A.B, they still can say that.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
@ChgAdded{Version=[2],Text=[@Redundant[A @nt{library_item} mentioned in a
@nt{limited_with_clause} shall be the implicit declaration of the limited view
of a library package, not the declaration of a subprogram, generic unit,
generic instance, or a renaming.]]}

@begin{TheProof}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is redundant because only such implicit
  declarations are visible in a @nt{limited_with_clause}. See
  @RefSecNum{Environment-Level Visibility Rules}.]}
@end{TheProof}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06],ARef=[AI95-00412-01]}
@ChgAdded{Version=[2],Text=[A @nt{limited_with_clause} shall not appear on a
@nt{library_unit_body}, @nt{subunit}, or @nt{library_@!unit_@!renaming_@!declaration}.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00412-01]}
  @ChgAdded{Version=[2],Text=[We don't allow a @nt{limited_with_clause} on a
  @nt{library_@!unit_@!renaming_@!declaration} because it would be useless and
  therefore probably is a mistake. A
  renaming cannot appear in a @nt{limited_with_clause} (by the rule prior
  to this one), and a renaming of a limited view cannot appear in a
  @nt{nonlimited_with_clause} (because the name
  would not be within the scope of a @nt{with_clause} denoting the package, see
  @RefSecNum{Package Renaming Declarations}). Nor could it be the parent of
  another unit. That doesn't leave anywhere that the name of such a renaming
  @b<could> appear, so we simply make writing it illegal.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[A
@nt{limited_with_clause} that names a library package shall not appear:]}

@begin(Itemize)
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0040-1]}
@ChgAdded{Version=[2],Text=[in the @nt{context_clause} for the
explicit declaration of the named library package@Chg{Version=[3],New=[ or
any of its descendants],Old=[]};]}

@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[We have to explicitly disallow]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{limited} @key{with} P;
@key{package} P @key{is} ...]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[as we can't depend on the semantic dependence
  rules to do it for us as with regular withs. This says @lquotes@;named@rquotes
  and not @lquotes@;mentioned@rquotes in order that]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{limited} @key{private} @key{with} P.Child;
@key{package} P @key{is} ...]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[can be used to allow a mutual dependence between
  the private part of P and the private child P.Child, which occurs in
  interfacing and other problems. Since the child always semantically depends
  on the parent, this is the only way such a dependence can be broken.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0040-1]}
  @ChgAdded{Version=[3],Text=[The part about descendants catches examples like]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{limited} @key{with} P;
@key{package} P.Child @key{is} ...]}
@end{Example}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0077-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[within a], Old=[in the same]}
@nt{context_clause}
@Chg{Version=[3],New=[for a @nt{library_item} that is],Old=[as, or]}
within the scope of@Chg{Version=[3],New=[],Old=[,]}
a @nt{nonlimited_with_clause} that mentions the same library package; or]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0077-1]}
  @ChgAdded{Version=[3],Text=[This applies to @nt{nonlimited_with_clause}s
  found in the same @nt{context_clause}, as well as @nt{nonlimited_with_clause}s
  found on parent units.]}
@end{Ramification}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0077-1]}
  @ChgAdded{Version=[2],Text=[Such a @nt{limited_with_clause} could have no
  effect, and would be confusing. If@Chg{Version=[3],New=[],Old=[ it is
  within the scope of]} a @nt{nonlimited_with_clause}@Chg{Version=[3],New=[
  for the same package is inherited from a parent unit or given],Old=[, or
  if such a clause is]} in the @nt{context_clause},
  the full view is available, which strictly provides more information than
  the limited view.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0077-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[within a], Old=[in the same]}
@nt{context_clause}
@Chg{Version=[3],New=[for a @nt{library_item} that is],Old=[as, or]}
within the scope of@Chg{Version=[3],New=[],Old=[,]}
a @nt{use_clause} that names an entity declared within the
declarative region of the library package.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0077-1]}
  @ChgAdded{Version=[3],Text=[This applies to @nt{use_clause}s found in the
  same @nt{context_clause}, as well as @nt{use_clause}s found in (or on)
  parent units.]}
@end{Ramification}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This prevents visibility issues, where whether an
  entity is an incomplete or full view depends on how the name of the entity is
  written. The @nt{limited_with_clause} cannot be useful, as we must have the
  full view available in the parent in order for the @nt{use_clause} to be
  legal.]}
@end{Reason}

@end(Itemize)

@end{Legality}

@begin{Notes}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00217-06]}
A @nt<library_item> mentioned in a @Chg{Version=[2],
New=[@nt{nonlimited_with_clause}],Old=[@nt{with_clause}]} of a compilation unit
is visible within the compilation unit and hence acts
just like an ordinary declaration.
Thus, within a compilation unit that mentions its declaration, the name of a
library package can be given in @nt{use_clause}s and can be used to
form expanded names, a library subprogram can be called,
and instances of a generic library unit can be declared.
If a child of a parent generic package is mentioned in a
@Chg{Version=[2], New=[@nt{nonlimited_with_clause}],Old=[@nt{with_clause}]},
then the corresponding declaration nested within each visible instance
is visible within the compilation unit.@Chg{Version=[2], New=[ Similarly,
a @nt{library_item} mentioned in a
@nt{limited_with_clause} of a compilation unit is visible within the
compilation unit and thus can be used to form expanded names.],Old=[]}

@begin{Ramification}
The rules given for @nt{with_clause}s are such that the same effect
is obtained whether the name of a library unit is mentioned once or
more than once by the applicable @nt{with_clause}s, or even within a
given @nt{with_clause}.

If a @nt{with_clause} mentions a
@nt{library_@!unit_@!renaming_@!declaration},
it only @lquotes@;mentions@rquotes@; the @nt<prefix>es appearing explicitly
in the @nt<with_clause>
(and the renamed view itself);
the @nt{with_clause} is not defined to mention the ancestors of the
renamed entity.
Thus, if X renames Y.Z, then @lquotes@;with X;@rquotes@; does not make the
declarations of Y or Z visible.
Note that this does not cause the dreaded visibility holes mentioned
above.
@end{Ramification}
@end{Notes}

@begin{Examples}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Text=[@key(package) Office @key(is)
@key{end} Office;]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Text=[@key(with) Ada.Strings.Unbounded;
@key(package) Office.Locations @key(is)
   @key(type) Location @key(is new) Ada.Strings.Unbounded.Unbounded_String;
@key(end) Office.Locations;]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Text=[@key(limited with) Office.Departments;  --@RI[ types are incomplete]
@key(private with) Office.Locations;    --@RI[ only visible in private part]
@key(package) Office.Employees @key(is)
   @key(type) Employee @key(is private);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key(function) Dept_Of(Emp : Employee) @key(return access) Departments.Department;
   @key(procedure) Assign_Dept(Emp  : @key(in out) Employee;
                         Dept : @key(access) Departments.Department);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   ...
@key(private
   type) Employee @key(is
      record)
         Dept : @key(access) Departments.Department;
         Loc : Locations.Location;
         ...
      @key(end record);
@key(end) Office.Employees;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key(limited with) Office.Employees;
@key(package) Office.Departments @key(is)
   @key(type) Department @key(is private);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key(function) Manager_Of(Dept : Department) @key(return access) Employees.Employee;
   @key(procedure) Assign_Manager(Dept : @key(in out) Department;
                            Mgr  : @key(access) Employees.Employee);
   ...
@key(end) Office.Departments;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Text=[The @nt{limited_with_clause} may be used to support
mutually dependent abstractions that are split across multiple packages. In
this case, an employee is assigned to a department, and a department has a
manager who is an employee. If a @nt{with_clause} with the reserved word
@key(private) appears on one library unit and mentions a second library unit,
it provides visibility to the second library unit, but restricts that
visibility to the private part and body of the first unit. The compiler checks
that no use is made of the second unit in the visible part of the first unit.]}

@end{Examples}



@begin{Extend83}
@Defn{extensions to Ada 83}
The syntax rule for @nt{with_clause} is modified to allow expanded name
notation.

A @nt<use_clause> in a @nt<context_clause> may be for
a package (or type) nested in a library package.
@end{Extend83}

@begin{DiffWord83}
The syntax rule for @nt{context_clause} is modified to more closely reflect
the semantics.
The Ada 83 syntax rule implies that the @nt{use_clause}s that appear
immediately after a particular @nt{with_clause} are somehow attached to
that @nt{with_clause}, which is not true.
The new syntax allows a @nt{use_clause} to appear first,
but that is prevented by a textual rule that already exists in Ada 83.

The concept of @lquotes@;scope of a @nt{with_clause}@rquotes@; (which is a region of text)
replaces RM83's notion of @lquotes@;apply to@rquotes@;
(a @nt{with_clause} applies to a @nt{library_item})
The visibility rules are interested in a region of text,
not in a set of compilation units.

No need to define @lquotes@;apply to@rquotes@; for @nt{use_clause}s.
Their semantics are fully covered by the @lquotes@;scope (of a @nt{use_clause})@rquotes@;
definition in @RefSecNum{Use Clauses}.
@end{DiffWord83}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00220-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @b[Amendment Correction:] A subprogram body acting as a declaration cannot
  @key[with] a private child unit. This would allow public export of types
  declared in private child packages, and thus cannot be allowed. This was
  allowed by mistake in Ada 95; a subprogram that does this will now be
  illegal.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @nt{limited_with_clause}s are new. They make a limited view of a package
  visible, where all of the types in the package are incomplete. They facilitate
  construction of mutually recursive types in multiple packages.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00262-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0077-1]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The syntax rules for @nt{with_clause} are modified to allow the reserved
  word @key{private}. Private @nt{with_clause}s do not allow the use of their
  @Chg{Version=[3],New=[@nt{library_item}],Old=[library item]}
  in the visible part of their @nt{compilation_unit}. They also
  allow using private units in more locations than in Ada 95.]}
@end{Extend95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0040-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Added missing rule that
  a limited with clause cannot name an ancestor unit. This is incompatible
  if an Ada 2005 program does this, but as this is a new Ada 2005 feature and
  the unintentionally allowed capability is not useful, the
  incompatibility is very unlikely to occur in practice.]}
@end{Incompatible2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0077-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Fixed wording so that
  we are not checking whether something in a @nt{context_clause}
  is @ldquote@;within the scope of@rdquote something, as @nt{context_clause}s
  are never included in anything's scope. The intended meaning is unchanged,
  however.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0122-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Fixed wording so the
  rules for private with clauses also apply to "sprouted" generic child
  units.]}
@end{DiffWord2005}


@LabeledSubClause{Subunits of Compilation Units}

@begin{Intro}
@redundant[Subunits are like child units, with these (important) differences:
subunits support the separate compilation of bodies
only (not declarations);
the parent contains a @nt{body_stub}
to indicate the existence and place of each of its subunits;
declarations appearing in the parent's body
can be visible within the subunits.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<body_stub>,rhs="@Syn2{subprogram_body_stub} | @Syn2{package_body_stub} | @Syn2{task_body_stub} | @Syn2{protected_body_stub}"}


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00218-03]}
@Syn{lhs=<subprogram_body_stub>,rhs="@Chg{Version=[2],New=<
   [@Syn2{overriding_indicator}]
   >,Old=[]}@Syn2{subprogram_specification} @key{is} @key{separate};"}

@begin{Discussion}
Although this syntax allows a @nt{parent_unit_name},
that is disallowed by @RefSec{Compilation Units - Library Units}.
@end{Discussion}


@Syn{lhs=<package_body_stub>,rhs="@key{package} @key{body} @Syn2{defining_identifier} @key{is} @key{separate};"}


@Syn{lhs=<task_body_stub>,rhs="@key{task} @key{body} @Syn2{defining_identifier} @key{is} @key{separate};"}


@Syn{lhs=<protected_body_stub>,rhs="@key{protected} @key{body} @Syn2{defining_identifier} @key{is} @key{separate};"}


@Syn{lhs=<subunit>,rhs="@key{separate} (@Syn2{parent_unit_name}) @Syn2{proper_body}"}
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00243-01]}
@Defn2{Term=[parent body], Sec=(of a subunit)}
The @i{parent body} of a subunit is the body of the program unit
denoted by its @nt{parent_unit_name}.
@Defn{subunit} The term @i{subunit} is used to refer to
a @nt{subunit} and also to the @nt{proper_body} of a @nt{subunit}.
@Chg{Version=[2],New=<The @i<subunits
of a program unit> include any subunit that
names that program unit as its parent, as well as any subunit that
names such a subunit as its parent (recursively).@Defn2{Term=[subunit],
Sec=(of a program unit)}>,Old=[]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00243-01]}
@ChgAdded{Version=[2],Text=[We want any rule that applies to a subunit to apply
to a subunit of a subunit as well.]}
@end{Reason}

The parent body of a subunit shall be present in the current environment,
and shall contain a corresponding @nt{body_stub}
with the same @nt{defining_identifier} as the subunit.
@begin{Discussion}
This can't be a @ResolutionName, because a @nt{subunit} is not a
complete context.
@end{Discussion}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0004-1]}
A @nt{package_body_stub} shall be the completion of a
@nt{package_@!declaration} or @nt{generic_@!package_@!declaration};
a @nt{task_@!body_@!stub} shall be the completion of a
@Chg{Version=[3],New=[task declaration],Old=[@ntf{task_@!declaration}]};
a @nt{protected_@!body_stub} shall be the completion of a
@Chg{Version=[3],New=[protected declaration],Old=[@ntf{protected_@!declaration}]}.

In contrast,
a @nt{subprogram_body_stub} need not be the completion of a previous
declaration,
@Redundant[in which case the @ntf{_stub} declares the subprogram].
If the @ntf{_stub} is a completion, it shall be the completion of a
@nt{subprogram_declaration} or @nt{generic_subprogram_declaration}.
The profile of a @nt{subprogram_body_stub} that completes a declaration
shall conform fully to that of the declaration.
@Defn2{Term=[full conformance],Sec=(required)}
@begin{Discussion}
The part about @nt{subprogram_body_stub}s echoes the corresponding rule
for @ntf{subprogram_bodies} in @RefSec{Subprogram Bodies}.
@end{Discussion}

A subunit that corresponds to a @nt{body_stub} shall be of
the same kind
(@ntf{package_}, @ntf{subprogram_}, @ntf{task_}, or @ntf{protected_})
as the @nt{body_stub}.
The profile of a @nt{subprogram_body}
subunit shall be fully conformant to
that of the corresponding @nt{body_stub}.
@Defn2{Term=[full conformance],Sec=(required)}

A @nt{body_stub} shall appear immediately within the
@nt{declarative_part} of a compilation unit body.
This rule does not apply within an instance of a generic unit.
@begin{Discussion}
@Defn{methodological restriction}
This is a methodological restriction;
that is, it is not necessary for the semantics of the language to
make sense.
@end{Discussion}

The @nt{defining_identifier}s of all @nt{body_stub}s that appear
immediately within a particular @nt{declarative_part} shall be
distinct.
@end{Legality}

@begin{LinkTime}
For each @nt{body_stub}, there shall be a subunit containing
the corresponding @nt{proper_body}.
@end{LinkTime}

@begin{Notes}
@leading@;The rules in @RefSec{The Compilation Process}
say that a @nt{body_stub} is equivalent to the corresponding
@nt{proper_body}. This implies:
@begin{Itemize}
Visibility within a subunit is the
visibility that would be obtained at the place of the corresponding
@nt{body_stub} (within the parent body) if the
@nt{context_clause} of the subunit were appended to
that of the parent body.
@begin{Ramification}
Recursively.
Note that this transformation might make the parent illegal;
hence it is not a true equivalence, but applies only to visibility
within the subunit.
@end{Ramification}

The effect of the elaboration of a @nt{body_stub} is to elaborate
the subunit.
@begin{Ramification}
The elaboration of a subunit is part of its parent body's elaboration,
whereas the elaboration of a child unit is not part of its parent
declaration's elaboration.
@end{Ramification}
@end{Itemize}
@begin{Ramification}
A @nt<library_item> that is mentioned in a @nt{with_clause} of a
subunit can be hidden (from direct @Chg{Version=[2],New=[visibility],
Old=[visiblity]}) by a declaration (with the same
@nt{identifier}) given in the subunit.
Moreover, such a @nt<library_item> can even be hidden by a declaration
given within the parent body since a library unit is declared in its
parent's declarative region; this however does not affect the
interpretation of the @nt{with_clause}s themselves, since only
@nt<library_item>s
are visible or directly visible
in @nt{with_clause}s.

The body of a protected operation cannot be a subunit.
This follows from the syntax rules.
The body of a protected unit can be a subunit.
@end{Ramification}
@end{Notes}

@begin{Examples}
@leading@keepnext@;The package Parent is first written without subunits:
@begin{Example}
@key[package] Parent @key[is]
    @key[procedure] Inner;
@key[end] Parent;

@key[with] Ada.Text_IO;
@key[package] @key[body] Parent @key[is]
    Variable : String := "Hello, there.";
    @key[procedure] Inner @key[is]
    @key[begin]
        Ada.Text_IO.Put_Line(Variable);
    @key[end] Inner;
@key[end] Parent;
@end{Example}

@begin{WideAbove}
@leading@;The body of procedure Inner may be turned into a subunit by rewriting
the package body as follows (with the declaration of Parent remaining
the same):
@end{WideAbove}
@begin{Example}
@key[package] @key[body] Parent @key[is]
    Variable : String := "Hello, there.";
    @key[procedure] Inner @key[is] @key[separate];
@key[end] Parent;

@key[with] Ada.Text_IO;
@key[separate](Parent)
@key[procedure] Inner @key[is]
@key[begin]
    Ada.Text_IO.Put_Line(Variable);
@key[end] Inner;
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
Subunits of the same ancestor library unit are no longer restricted
to have distinct identifiers.
Instead, we require only that the full expanded names be distinct.
@end{Extend83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00218-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  An @nt{overriding_indicator} (see
  @RefSecNum{Overriding Indicators}) is allowed on a subprogram stub.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00243-01]}
  @ChgAdded{Version=[2],Text=[Clarified that a subunit of a subunit is still a
  subunit.]}
@end{DiffWord95}


@LabeledSubClause{The Compilation Process}

@begin{Intro}
@Defn{environment}
@Defn{environment @nt<declarative_part>}
Each compilation unit submitted to the compiler is compiled in the
context of an @i{environment} @nt<declarative_part> (or simply, an
@i{environment}),
which is a conceptual @nt<declarative_part> that forms
the outermost declarative region of the
context of any @nt{compilation}.
At run time, an environment forms the @nt<declarative_part> of
the body of the environment task of a partition
(see @RefSec{Program Execution}).
@begin{Ramification}
At compile time, there is no particular construct that the
declarative region is considered to be nested within
@em the environment is the universe.
@end{Ramification}
@begin{Honest}
The environment is really just a portion of a @nt{declarative_part},
since there might, for example, be bodies that do not yet exist.
@end{Honest}

The @nt{declarative_item}s of the environment
are @nt{library_item}s appearing in an order
such that there are no forward semantic dependences.
Each included subunit
occurs in place of the corresponding stub.
The visibility rules apply as if the environment were the outermost
declarative region,
except that @nt{with_@!clause}s are needed to make
declarations of library units visible
(see @RefSecNum{Context Clauses - With Clauses}).

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00217-06]}
The mechanisms for creating an environment
and for adding and replacing compilation units within an environment
are implementation defined.@Chg{Version=[2],New=[ The mechanisms for adding a
compilation unit mentioned in a @nt{limited_with_clause} to an
environment are implementation defined.],Old=[]}
@ImplDef{The mechanisms for creating an environment
and for adding and replacing compilation units.}
@ChgImplDef{Version=[2],Kind=[Added],Text=[@Chg{Version=[2],New=[The
mechanisms for adding a compilation unit mentioned in a @nt{limited_with_clause}
to an environment.],Old=[]}]}
@begin{Ramification}
The traditional model, used by most Ada 83 implementations,
is that one places a compilation unit in the environment
by compiling it.
Other models are possible.
For example, an implementation might define the environment to
be a directory;
that is, the compilation units in the environment are all the
compilation units in the source files contained in the directory.
In this model, the mechanism for replacing a compilation unit with a
new one is simply to edit the source file containing that compilation
unit.
@end{Ramification}

@end{Intro}

@begin{Resolution}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0032],ARef=[AI95-00192-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
If a @nt<library_unit_body> that is a @nt<subprogram_body> is
submitted to the compiler, it is interpreted only as a completion
if a @nt<library_unit_declaration> @Chg{New=[], Old=[for a subprogram
or a generic subprogram ]}with the same @nt<defining_program_unit_name>
already exists in the environment @Chg{New=[for a subprogram other than
an instance of a generic subprogram or for a generic subprogram ], Old=[]}
(even if the profile of the body is not type conformant with that of the
declaration); otherwise@Chg{Version=[3],New=[,],Old=[]} the
@nt<subprogram_body> is
interpreted as both the declaration and body of a library subprogram.
@PDefn{type conformance}
@begin{Ramification}
  The principle here is that a @nt{subprogram_body} should be
  interpreted as only a completion if and only if it @lquotes@;might@rquotes@;
  be legal as the completion of some preexisting declaration,
  where @lquotes@;might@rquotes@; is defined in a way that does not require overload
  resolution to determine.

  Hence, if the preexisting declaration is a @nt{subprogram_declaration}
  or @nt{generic_subprogram_declaration}, we treat the new
  @nt{subprogram_body} as its completion, because it @lquotes@;might@rquotes@; be legal.
  If it turns out that the profiles don't fully conform,
  it's an error.
  In all other cases (the preexisting declaration is a
  package or a generic package,
  or an instance of a generic subprogram,
  or a renaming,
  or a @lquotes@;spec-less@rquotes@; subprogram,
  or in the case where there is no preexisting thing),
  the @nt{subprogram_body} declares a new subprogram.

  See also AI83-00266/09.
@end{Ramification}
@end{Resolution}

@begin{Legality}
When a compilation unit is compiled,
all compilation units upon which it depends semantically
shall already exist in the environment;
@Defn2{Term=[consistency], Sec=(among compilation units)}
the set of these compilation units shall be @i{consistent} in the
sense that the new compilation unit shall not semantically depend
(directly or indirectly) on two different versions of the same compilation
unit, nor on an earlier version of itself.
@begin{Discussion}
For example, if package declarations A and B both say
@lquotes@;@key[with] X;@rquotes@;, and the user compiles a compilation unit that says
@lquotes@;@key[with] A, B;@rquotes@;, then the A and B have to be talking about the
same version of X.
@end{Discussion}
@begin{Ramification}
  What it means to be a @lquotes@;different version@rquotes@; is not specified by the
  language. In some implementations, it means that the compilation
  unit has been recompiled. In others, it means that the
  source of the compilation unit has been edited in some significant way.

Note that an implementation cannot require the existence of
compilation units upon which the given one does not semantically
depend.
For example, an implementation is required to be able to compile a
compilation unit that says "@key{with} A;" when A's body does not
exist.
It has to be able to detect errors without looking at A's body.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
Similarly, the implementation has to be able to compile a call to a
subprogram for which @Chg{Version=[3],New=[aspect],Old=[a @nt{pragma}]}
Inline has been specified without
seeing the body of that subprogram @em inlining would not be
achieved in this case, but the call is still legal.

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
@ChgAdded{Version=[2],Text=[The @Chg{Version=[2],New=[consistency],
Old=[second]} rule applies to limited views as well
as the full view of a compilation unit. That means that an implementation needs
a way to enforce consistency of limited views, not just of full views.]}
@end{Ramification}
@end{Legality}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00217-06]}
The implementation may require that a compilation unit be legal before
@Chg{Version=[2],New=[it can be mentioned in a @nt{limited_with_clause} or
it can be inserted],Old=[inserting it]} into the environment.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00214-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
When a compilation unit that declares or renames a library unit
is added to the environment,
the implementation may remove from the environment
any preexisting @nt<library_item>
@Chg{Version=[2],New=[or @nt<subunit> with the same full expanded name],
Old=[with the same @nt<defining_@!program_@!unit_name>]}.
When a compilation unit that is a subunit or the body
of a library unit is added to the environment,
the implementation may remove from the environment
any preexisting version of the same compilation unit.
@Chg{Version=[2],New=[When a compilation unit that
contains a @nt<body_stub> is added to the environment, the implementation may
remove any preexisting @nt<library_item> or @nt<subunit> with the same full
expanded name as the @nt<body_stub>.],Old=[]}
When a given compilation unit is removed from the environment,
the implementation may also remove any compilation unit that depends
semantically upon the given one.
If the given compilation unit contains the body of a subprogram @Chg{Version=[3],New=[for],Old=[to]}
which @Chg{Version=[3],New=[aspect],Old=[a @nt{pragma}]} Inline @Chg{Version=[3],New=[is True],Old=[applies]},
the implementation may also remove any compilation unit containing a
call to that subprogram.
@begin{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
The permissions given in this paragraph correspond to the traditional
model, where compilation units enter the environment by being
compiled into it, and the compiler checks their legality at that time.
@Chg{Version=[3],New=[An],Old=[A]} implementation model in which the
environment consists of all source files in a given directory might not
want to take advantage of these permissions.
Compilation units would not be checked for
legality as soon as they enter the environment; legality checking
would happen later, when compilation units are compiled.
In this model, compilation units might never be automatically removed
from the environment;
they would be removed when the user explicitly deletes a source file.

Note that the rule is recursive: if the above permission is used to
remove a compilation unit containing an inlined subprogram call,
then compilation units that depend semantically upon the removed one
may also be removed, and so on.

Note that here we are talking about dependences among existing
compilation units in the environment;
it doesn't matter what @nt{with_clause}s are attached to the new
compilation unit that triggered all this.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
An implementation may have other modes in which compilation units
in addition to the ones mentioned above are removed.
For example, an implementation might inline subprogram calls without
an explicit @Chg{Version=[3],New=[aspect],Old=[@nt{pragma}]} Inline.
If so, it either has to have a mode in which that optimization is
turned off, or it has to automatically regenerate code for the inlined
calls without requiring the user to resubmit them to the compiler.
@end{Ramification}
@begin{Discussion}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0108],ARef=[AI95-00077-01]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00114-01]}
@ChgAdded{Version=[1],Text=[In the standard mode, implementations may only remove
units from the environment for one of the reasons listed here, or in response
to an explicit user command to modify the environment. It is not intended that
the act of compiling a unit is one of the
@lquotes@;@Chg{Version=[2],New=[mechanisms],Old=[mechansisms]}@rquotes for
removing units other than those specified by this International Standard.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00214-01]}
@ChgAdded{Version=[2],Text=[These rules are intended to ensure that an
implementation never need keep more than one compilation unit with any full
expanded name. In particular, it is not necessary to be able to have a subunit
and a child unit with the same name in the environment at one time.]}

@end{Discussion}
@end{ImplPerm}

@begin{Notes}
The rules of the language are enforced across
@nt{compilation} and compilation unit boundaries,
just as they are enforced within a single compilation unit.
@begin{Ramification}
Note that Section 1 requires an implementation to detect illegal
compilation units at compile time.
@end{Ramification}

@Defn{library}
An implementation may support a concept of a @i{library},
which contains @nt{library_item}s.
If multiple libraries are supported,
the implementation has to define how a single environment is
constructed when a compilation unit is submitted to the compiler.
Naming conflicts between different libraries might be resolved by
treating each library as the root of a hierarchy of child library
units.
@IndexSee{Term=[program library],See=(library)}
@begin{ImplNote}
Alternatively, naming conflicts could be resolved via some sort of
hiding rule.
@end{ImplNote}
@begin{Discussion}
For example, the implementation might support a command to import
library Y into library X.
If a root library unit called LU (that is, Standard.LU) exists in Y,
then from the point of view of library X,
it could be called Y.LU.
X might contain library units that say, @lquotes@;@key[with] Y.LU;@rquotes@;.
@end{Discussion}

A compilation unit containing an instantiation of a separately
compiled generic unit does not semantically depend on the body
of the generic unit.
Therefore, replacing the generic body in the environment
does not result in the removal of the compilation unit
containing the instantiation.
@begin{ImplNote}
  Therefore, implementations have to be prepared to automatically
  instantiate generic bodies at link-time, as needed. This might
  imply a complete automatic recompilation, but it is the intent
  of the language that generic bodies can be (re)instantiated without
  forcing all of the compilation units that semantically depend
  on the compilation unit containing the instantiation to be
  recompiled.
@end{ImplNote}
@end{Notes}

@begin{Extend83}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00077-01],ARef=[AI95-00114-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 83}
  Ada 83 allowed implementations to require that the body of a generic unit
  be available when the instantiation is compiled; that permission is dropped
  in Ada 95. This isn't really an extension (it doesn't allow Ada users to
  write anything that they couldn't in Ada 83), but there isn't a more
  appropriate category, and it does allow users more flexibility when
  developing programs.]}
@end{Extend83}


@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0032],ARef=[AI95-00192-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> The wording was clarified to
  ensure that a @nt{subprogram_body} is not considered a completion of
  an instance of a generic subprogram.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00214-01]}
  @ChgAdded{Version=[2],Text=[The permissions to remove a unit from the
  environment were clarified to ensure that it is never necessary to keep
  multiple (sub)units with the same full expanded name in the environment.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
  @ChgAdded{Version=[2],Text=[Units mentioned in a @nt{limited_with_clause}
  were added to several rules; limited views have the same presence in the
  environment as the corresponding full views.]}
@end{DiffWord95}

@LabeledSubClause{Pragmas and Program Units}

@begin{Intro}
@Redundant[This subclause discusses pragmas related to program units,
library units, and @nt{compilation}s.]
@end{Intro}

@begin{Resolution}
@RootDefn{program unit pragma}
@RootDefn{pragma, program unit}
Certain @nt{pragma}s are defined to be @i{program unit pragmas}.
@PDefn2{Term=[apply], Sec=(to a program unit by a program unit pragma)}
A @nt{name} given as the argument of a program unit pragma shall resolve to
denote the declarations or renamings of one or more program
units that occur immediately within the declarative region or @nt<compilation>
in which the @nt<pragma> immediately occurs, or it shall resolve to denote
the declaration of the immediately enclosing program unit (if any);
the @nt{pragma} applies to the denoted program
unit(s).
If there are no @nt{name}s given as arguments,
the @nt{pragma} applies to the immediately
enclosing program unit.
@begin{Ramification}
The fact that this is a @ResolutionName means that the @nt{pragma}
will not apply to declarations from outer declarative regions.
@end{Ramification}

@end{Resolution}

@begin{Legality}
@leading@keepnext@;A program unit pragma shall appear in one of these places:
@begin{Itemize}
At the place of a @nt{compilation_unit},
in which case the @nt<pragma>
shall immediately follow in the same @nt<compilation> (except for
other @nt{pragma}s)
a @nt{library_@!unit_@!declaration} that is a @nt<subprogram_@!declaration>,
@nt<generic_@!subprogram_@!declaration>, or @nt<generic_@!instantiation>, and
the @nt<pragma> shall have an argument that is a @nt<name> denoting
that declaration.
@begin{Ramification}
The @nt{name} has to denote the immediately preceding
@nt{library_unit_declaration}.
@end{Ramification}

@ChgRef{Version=[1], Kind=[Revised], Ref=[8652/0033], ARef=[AI95-00136-01]}
Immediately within the @Chg{New=[visible part],Old=[declaration]} of a
program unit and before any nested declaration@Chg{New=[ (but not within a
generic formal part)], Old=[]}, in which case the argument,
if any, shall be a @nt{direct_name}
that denotes the immediately enclosing program unit declaration.
@begin{Ramification}
The argument is optional in this case.
@end{Ramification}

At the place of a declaration
other than the first,
of a @nt{declarative_part} or program unit declaration,
in which case the @nt{pragma} shall have an argument,
which shall be a @nt{direct_name}
that denotes one or more of the following (and nothing else):
a @nt{subprogram_@!declaration}, a @nt{generic_@!subprogram_@!declaration},
or a @nt{generic_@!instantiation}, of the same @nt<declarative_@!part> or
program unit declaration.
@begin{Ramification}
If you want to denote a @nt<subprogram_body> that is not a
completion, or a
@nt{package_declaration}, for example, you have to put the @nt{pragma}
inside.
@end{Ramification}
@end{Itemize}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0132-1]}
@RootDefn{library unit pragma}
@RootDefn{pragma, library unit}
@PDefn2{Term=[program unit pragma], Sec=(library unit pragmas)}
@PDefn2{Term=[pragma, program unit], Sec=(library unit pragmas)}
Certain program unit pragmas are defined to be
@i{library unit pragmas}.
@Chg{Version=[3],New=[If a library unit pragma applies to a program unit,
the program unit shall be],Old=[The @nt{name}, if
any, in a library unit pragma shall denote the declaration of]} a library unit.
@begin{Ramification}
This, together with the rules for program unit pragmas above,
implies that if a library unit pragma applies to a @nt{subprogram_declaration}
(and similar things), it has to appear immediately after the
@nt{compilation_unit}, whereas if the @nt{pragma} applies to a
@nt{package_declaration}, a @nt{subprogram_body} that is not a
completion (and similar things), it has to appear inside, as the first
@nt{declarative_item}.
@end{Ramification}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[1], Kind=[Added], Ref=[8652/0034], ARef=[AI95-00041-01]}
@ChgAdded{Version=[1],Text=[A library unit pragma that applies to a generic
unit does not apply to its instances, unless a specific rule for the pragma
specifies the contrary.]}
@end{StaticSem}

@begin{LinkTime}
@RootDefn{configuration pragma}
@RootDefn{pragma, configuration}
Certain @nt{pragma}s are defined to be @i{configuration pragmas};
they shall appear before the first @nt{compilation_unit}
of a @nt{compilation}.
@Redundant[They are generally used to select a partition-wide
or system-wide option.]
The @nt<pragma> applies to all @nt{compilation_unit}s
appearing in the @nt{compilation},
unless there are none, in which case it applies to all future
@nt{compilation_unit}s compiled into the same environment.
@end{LinkTime}

@begin{ImplPerm}
@ChgRef{Version=[2], Kind=[Revised], ARef=[AI95-00212-01]}
An implementation may @Chg{Version=[2], New=[require that configuration
pragmas that select partition-wide or system-wide options be compiled],
Old=[place restrictions on
configuration pragmas, so long as it allows them]} when the environment
contains no @nt{library_item}s other than those of the predefined environment.
@Chg{Version=[2], New=[In this case, the implementation shall still accept
configuration pragmas in individual compilations that confirm the initially
selected partition-wide or system-wide options.],Old=[]}

@end{ImplPerm}

@begin{ImplAdvice}
@ChgRef{Version=[1], Kind=[AddedNormal], Ref=[8652/0034], ARef=[AI95-00041-01]}
@ChgAdded{Version=[1],Text=[When applied to a generic unit, a program unit pragma that
is not a library unit pragma should apply to each instance of the generic unit
for which there is not an overriding pragma applied directly to the instance.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[When applied to a generic unit, a program unit pragma that
is not a library unit pragma should apply to each instance of the generic unit
for which there is not an overriding pragma applied directly to the instance.]}]}
@end{ImplAdvice}


@begin{DiffWord95}
  @ChgRef{Version=[2], Kind=[AddedNormal], Ref=[8652/0033], ARef=[AI95-00136-01]}
  @ChgAdded{Version=[2], Text=[@B<Corrigendum:> The wording was corrected to
  ensure that a program unit pragma cannot appear in private parts or
  generic formal parts.]}

  @ChgRef{Version=[2], Kind=[AddedNormal], Ref=[8652/0034],
  ARef=[AI95-00041-01]} @ChgAdded{Version=[2], Text=[@B<Corrigendum:> The
  wording was clarified to explain the meaning of program unit and library unit
  pragmas in generic units.]}

  @ChgRef{Version=[2], Kind=[AddedNormal]}
  @ChgAdded{Version=[2], Text=[The Implementation Advice added by the
  Corrigendum was moved, as it was not in the normal order. (This changes the
  paragraph number.) It originally was directly after the new Static Semantics
  rule.]}

  @ChgRef{Version=[2], Kind=[AddedNormal], ARef=[AI95-00212-01]}
  @ChgAdded{Version=[2], Type=[Leading],Keepnext=[T],Text=[The permission to
  place restrictions was clarified to:]}
  @begin{itemize}
  @ChgRef{Version=[2], Kind=[AddedNormal]}
  @ChgAdded{Version=[2], Text=[Ensure that it applies only to partition-wide
  configuration pragmas, not ones like Assertion_Policy (see
  @RefSecNum{Pragmas Assert and Assertion_Policy}), which can be different
  in different units; and]}

  @ChgRef{Version=[2], Kind=[AddedNormal]}
  @ChgAdded{Version=[2], Text=[Ensure that confirming pragmas are always allowed.]}
  @end{itemize}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0132-1]}
  @ChgAdded{Version=[3], Text=[@b<Correction:> A library unit pragma must
  apply directly to a library unit, even if no name is given in the pragma.]}
@end{DiffWord2005}


@LabeledSubClause{Environment-Level Visibility Rules}

@begin{Intro}
@Redundant[The normal visibility rules do not apply within a
@nt{parent_unit_name} or a @nt{context_clause},
nor within a @nt{pragma} that appears at the place of a compilation unit.
The special visibility rules for those contexts
are given here.]
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00217-06],ARef=[AI95-00312-01]}
@PDefn2{Term=[directly visible], Sec=(within the
@nt{parent_unit_name} of a library unit)}
@PDefn2{Term=[visible], Sec=(within the
@nt{parent_unit_name} of a library unit)}
@PDefn2{Term=[directly visible], Sec=(within a @nt{with_clause})}
@PDefn2{Term=[visible], Sec=(within a @nt{with_clause})}
Within the @nt{parent_unit_name} at the beginning of
@Chg{Version=[2],New=[an explicit],Old=[a]} @nt{library_item},
and within a @Chg{Version=[2],New=[@nt{nonlimited_with_clause}],
Old=[@nt{with_clause}]}, the only declarations
that are visible are those that are@Chg{Version=[2],New=[ explicit],Old=[]}
@nt<library_item>s of the environment,
and the only declarations that are directly visible are those that
are@Chg{Version=[2],New=[ explicit],Old=[]} root @nt<library_item>s
of the environment.
@Chg{Version=[2],New=[Within a @nt{limited_with_clause}, the only declarations
that are visible are those that are the implicit declaration of the
limited view of a library package of the environment, and the only
declarations that are directly visible are those that are the
implicit declaration of the limited view of a root library package.],
Old=[@Defn{notwithstanding}
Notwithstanding the rules of @RefSecNum(Selected Components),
an expanded name in a @nt{with_clause} may consist of a
@nt<prefix> that denotes a generic package and a @nt<selector_name> that
denotes a child of that generic package.
@Redundant[(The child is necessarily a generic unit;
see @RefSecNum{Compilation Units - Library Units}.)]]}

@begin{Ramification}
In @lquotes@;@key{package} P.Q.R @key{is} ... @key{end} P.Q.R;@rquotes@;,
this rule requires P to be a root library unit,
and Q to be a library unit
(because those are the things that are directly visible and visible).
Note that visibility does not apply between the @lquotes@;@key{end}@rquotes@;
and the @lquotes@;;@rquotes@;.

Physically nested declarations are not visible at these places.

@ChgNote{Changed the classification of this note from Reason to Ramification,
as it certainly isn't a "reason". Besides, there were two reasons, which was
goofy.}
Although Standard is visible at these places,
it is impossible to name it,
since it is not directly visible,
and it has no parent.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00217-06]}
@ChgAdded{Version=[2],Text=[Only compilation units defining limited views
can be mentioned in a @nt{limited_with_clause}, while only compilation
units defining full views (that is, the explicit declarations) can be mentioned
in a @nt{nonlimited_with_clause}. This resolves the conflict inherent in having
two compilation units with the same defining name.]}
@end{Ramification}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00312-01]}
@ChgDeleted{Version=[2],Text=[The @lquotes@;notwithstanding@rquotes@; part
allows @lquotes@;@key[with] A.B;@rquotes@;
where A is a generic library package and B is one of its
(generic) children.
This is necessary because it is not normally legal to use an expanded
name to reach inside a generic package.]}
@end{Reason}

@PDefn2{Term=[directly visible], Sec=(within a @nt{use_clause} in a @nt{context_clause})}
@PDefn2{Term=[visible], Sec=(within a @nt{use_clause} in a @nt{context_clause})}
@PDefn2{Term=[directly visible], Sec=(within a @nt{pragma} in a @nt{context_clause})}
@PDefn2{Term=[visible], Sec=(within a @nt{pragma} in a @nt{context_clause})}
Within a @nt{use_clause} or @nt{pragma}
that is within a @nt{context_clause},
each @nt<library_item> mentioned in a previous
@nt{with_clause} of the same @nt{context_clause} is visible,
and each root @nt<library_item> so mentioned is directly visible.
In addition,
within such a @nt{use_clause},
if a given declaration is visible or directly
visible,
each declaration that occurs immediately within the given declaration's
visible part is also visible.
No other declarations are visible or directly visible.
@begin{Discussion}
Note the word @lquotes@;same@rquotes@;.
For example, if a @nt{with_clause} on a declaration mentions X,
this does not make X visible in @nt{use_clause}s and @nt{pragma}s
that are on the body.
The reason for this rule is
the one-pass @nt{context_clause}s @MetaRulesName.

Note that the second part of the rule does not
mention @nt{pragma}s.
@end{Discussion}

@PDefn2{Term=[directly visible], Sec=(within the
@nt{parent_unit_name} of a subunit)}
@PDefn2{Term=[visible], Sec=(within the
@nt{parent_unit_name} of a subunit)}
Within the @nt{parent_unit_name} of a subunit, @nt<library_item>s
are visible as they are in
the @nt{parent_unit_name} of a @nt{library_item};
in addition, the declaration corresponding to each
@nt{body_stub} in the environment
is also visible.
@begin{Ramification}
For a subprogram without a separate @nt<subprogram_declaration>,
the @nt<body_stub> itself is the declaration.
@end{Ramification}

@PDefn2{Term=[directly visible], Sec=(within a @nt{pragma}
that appears at the place of a compilation unit)}
@PDefn2{Term=[visible], Sec=(within a @nt{pragma}
that appears at the place of a compilation unit)}
Within a @nt{pragma} that appears at the place of a compilation unit,
the immediately preceding @nt<library_item> and each of its
ancestors is visible.
The ancestor root @nt<library_item> is directly visible.

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00312-01]}
@ChgAdded{Version=[2],Text=[@Defn{notwithstanding}
Notwithstanding the rules of @RefSecNum(Selected Components),
an expanded name in a @nt{with_clause}, a @nt{pragma} in a @nt{context_clause},
or a @nt{pragma} that appears at the place of a compilation unit
may consist of a @nt{prefix} that denotes a generic package and
a @nt<selector_name> that denotes a child of that generic package.
@Redundant[(The child is necessarily a generic unit;
see @RefSecNum{Compilation Units - Library Units}.)]]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This rule allows @key[with] A.B; and
@key[pragma] Elaborate(A.B); where A is a
generic library package and B is one of its (generic) children. This is
necessary because it is not normally legal to use an expanded
name to reach inside a generic package.]}
@end{Reason}

@end{StaticSem}

@begin{DiffWord83}
The special visibility rules that apply within a
@nt{parent_unit_name} or a @nt{context_clause},
and within a @nt{pragma} that appears at the place of a
@nt{compilation_unit} are clarified.

Note that a @nt{context_clause} is not part of any declarative region.

We considered making the visibility rules within
@nt{parent_unit_name}s and @nt{context_clause}s follow from the
context of compilation.
However, this attempt failed for various reasons.
For example, it would require @nt{use_clause}s in
@nt{context_clause}s to be within the declarative region of Standard,
which sounds suspiciously like a kludge.
And we would still need a special rule to prevent seeing things
(in our own @nt{context_clause})
that were with-ed by our parent, etc.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
  @ChgAdded{Version=[2],Text=[Added separate visibility rules for
  @nt{limited_with_clause}s; the existing rules apply only to
  @nt{nonlimited_with_clause}s.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00312-01]}
  @ChgAdded{Version=[2],Text=[Clarified that the name of a generic child unit
  may appear in a @nt{pragma} in a @nt{context_clause}.]}
@end{DiffWord95}


@LabeledClause{Program Execution}

@begin{Intro}
@Defn{program}
@Defn{program execution}
@IndexSee{Term=[running a program],See=(program execution)}
An Ada @i{program} consists
of a set of @i{partitions}@Redundant[,
which can execute in parallel with one another,
possibly in a separate address space,
and possibly on a separate computer.]
@end{Intro}

@begin{LinkTime}

@RootDefn{partition}
@Defn{partition building}
A partition is a program or part of a program
that can be invoked from outside
the Ada implementation.
@Redundant[For example, on many systems,
a partition might be an executable file
generated by the system linker.]
@Defn{explicitly assign}
The user can @i{explicitly assign} library units to a partition.
The assignment is done in an implementation-defined manner.
The compilation units included in a partition are
those of the explicitly assigned library units,
as well as other compilation units @i{needed by} those library units.
The compilation units needed by a given compilation unit are determined
as follows
(unless specified otherwise via an implementation-defined @nt{pragma},
or by some other implementation-defined means):
@IndexSee{Term=[linking],See=(partition building)}
@RootDefn2{Term=[compilation units needed], Sec=(by a compilation unit)}
@RootDefn2{Term=[needed], Sec=(of a compilation unit by another)}
@begin{Discussion}
From a run-time point of view,
an Ada 95 partition is identical to an Ada 83 program
@em implementations were always allowed to provide inter-program
communication mechanisms.
The additional semantics of partitions is that interfaces between
them can be defined to obey normal language rules
(as is done in @RefSec{Distributed Systems}),
whereas interfaces between separate programs had no particular
semantics.
@end{Discussion}
@ImplDef{The manner of explicitly assigning library units to a partition.}
@ImplDef{The implementation-defined means, if any, of specifying which
compilation units are needed by a given compilation unit.}
@begin{Discussion}
There are no pragmas that @lquotes@;specify otherwise@rquotes@; defined by the
core language. However, an implementation is allowed to provide such
pragmas, and in fact @RefSec{Distributed Systems} defines some
pragmas whose semantics includes reducing the set of
compilation units described here.
@end{Discussion}
@begin{Itemize}
A compilation unit needs itself;

If a compilation unit is needed, then so are any compilation units
upon which it depends semantically;

If a @nt{library_unit_declaration} is needed,
then so is any corresponding
@nt{library_unit_body};

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00217-06]}
If a compilation unit with stubs is needed,
then so are any corresponding subunits@Chg{Version=[2],New=[;],Old=[.]}

@begin{Discussion}
Note that in the environment, the stubs are replaced with the
corresponding @ntf{proper_bodies}.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00217-06]}
@ChgAdded{Version=[2],Text=[If the (implicit) declaration of the limited view
of a library package is needed, then so is the explicit declaration of the
library package.]}

@end{Itemize}
@begin{Discussion}
  Note that a child unit is not included
  just because its parent is included @em to include a child,
  mention it in a @nt{with_clause}.

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
  @ChgAdded{Version=[2],Text=[A package is included in a partition even if the
  only reference to it is in a @nt{limited_with_clause}. While this isn't
  strictly necessary (no objects of types imported from such a unit can be
  created), it ensures that all incomplete types are eventually completed, and
  is the least surprising option.]}
@end{Discussion}

@Defn2{Term=[main subprogram], Sec=(for a partition)}
The user can optionally designate
(in an implementation-defined manner)
one subprogram as the @i{main subprogram} for the partition.
A main subprogram, if specified, shall be a subprogram.
@begin{Discussion}
This may seem superfluous,
since it follows from the definition.
But we would like to have every error message that might be generated
(before run time) by an implementation correspond to some
explicitly stated @lquotes@;shall@rquotes@; rule.

Of course, this does not mean that the @lquotes@;shall@rquotes@; rules correspond
one-to-one with an implementation's error messages. For example,
the rule that says overload resolution @lquotes@;shall@rquotes@; succeed in producing a
single interpretation would correspond to many error messages in a good
implementation @em the implementation would want to explain to the user
exactly why overload resolution failed.
This is especially true for the syntax rules @em they are considered
part of overload resolution, but in most cases, one would expect an
error message based on the particular syntax rule that was violated.
@end{Discussion}
@ImplDef{The manner of designating the main subprogram of a partition.}
@begin{Ramification}
An implementation cannot require the user to specify, say, all of the
library units to be included.
It has to support, for example, perhaps the most typical case,
where the user specifies just one library unit,
the main program.
The implementation has to do the work of tracking down all the other ones.
@end{Ramification}

@Defn{environment task}
Each partition has an anonymous @i{environment task}@Redundant[,
which is an implicit outermost task whose execution
elaborates the @nt{library_item}s of the environment
@nt{declarative_part}, and then calls the main subprogram,
if there is one.
A partition's execution is that of its tasks.]
@begin{Ramification}
An environment task has no master;
all nonenvironment tasks have masters.

An implementation is allowed to support
multiple concurrent executions of the same partition.
@end{Ramification}

@Redundant[The order of elaboration of library units is determined
primarily by the @i{elaboration dependences}.]
@Defn2{Term=[elaboration dependence], Sec=(library_item on another)}
@Defn2{Term=[dependence], Sec=(elaboration)}
There is an elaboration dependence of a given @nt{library_item} upon
another if the given @nt{library_item} or any of its subunits depends
semantically on the other @nt{library_item}.
In addition, if a given @nt{library_item} or any of its subunits
has a @nt{pragma} Elaborate or Elaborate_All that
@Chg{Version=[2],New=[names],Old=[mentions]} another library unit,
then there is an elaboration dependence of the given
@nt{library_item} upon the body of the other library unit,
and, for Elaborate_All only, upon
each @nt{library_item} needed by
the declaration of the other library unit.
@begin{Discussion}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0107],ARef=[AI95-00180-01]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00256-01]}
@ChgAdded{Version=[1],Text=[@Lquotes@;Mentions@rquotes
@Chg{Version=[2],New=[was],Old=[is]}
used informally in the above rule; it @Chg{Version=[2],New=[was],Old=[is]}
not intended to refer to the definition of @i{mentions} in
@RefSecNum{Context Clauses - With Clauses}.
@Chg{Version=[2],New=[It was changed to @Lquotes@;names@rquotes to make this clear.],
Old=[It would have been better to use
@Lquotes@;names@rquotes instead of @Lquotes@;mentions@rquotes above.]}]}

See above for a definition of which @nt{library_item}s are @lquotes@;needed by@rquotes@;
a given declaration.

Note that elaboration dependences are among @nt{library_item}s,
whereas the other two forms of dependence are among compilation units.
Note that elaboration dependence includes semantic dependence.
It's a little bit sad that pragma Elaborate_Body can't be folded into
this mechanism.
It follows from the definition that the elaboration dependence
relationship is transitive.
Note that the wording of the rule does not need to take into account
a semantic dependence of a @nt{library_item} or one of its subunits
upon a subunit of a different library unit,
because that can never happen.
@end{Discussion}

@leading@keepnext@;The environment task for a partition has the following
structure:
@begin{Example}
@key[task] @RI{Environment_Task};

@key[task] @key[body] @RI{Environment_Task} @key[is]
    ... (1) --@RI{ The environment }@nt{declarative_part}
            --@RI{ (that is, the sequence of }@nt{library_item}@RI{s) goes here.}
@key[begin]
    ... (2) --@RI{ Call the main subprogram, if there is one.}
@key[end] @RI{Environment_Task};
@end{Example}
@begin{Ramification}
The name of the environment task is written in italics here to indicate
that this task is anonymous.
@end{Ramification}
@begin{Discussion}
  The model is different for a @lquotes@;passive partition@rquotes@;
  (see @RefSecNum{Partitions}).
  Either there is no environment task, or its @nt<sequence_of_statements>
  is an infinite loop rather than a call on a main subprogram.
@end{Discussion}

@leading@PDefn2{Term=[environment @nt<declarative_part>], Sec=(for the
environment task of a partition)}
The environment @nt{declarative_part} at (1) is a sequence of
@nt{declarative_item}s consisting of copies of
the @nt{library_item}s included in the partition@Redundant[.
The order of elaboration of @nt{library_item}s is
the order in which they appear in the environment
@nt{declarative_part}]:
@begin{Itemize}
  The order of all included @nt<library_item>s
  is such that there are no forward elaboration
  dependences.
  @begin{Ramification}
    This rule is written so that if a @nt{library_item}
    depends on itself, we don't require it to be elaborated
    before itself. See AI83-00113/12.
    This can happen only in pathological circumstances.
    For example, if a library @nt{subprogram_body}
    has no corresponding @nt{subprogram_declaration},
    and one of the subunits of the @nt{subprogram_body}
    mentions the @nt{subprogram_body} in a @nt{with_clause},
    the @nt{subprogram_body} will depend on itself.
    For another example, if a @nt{library_unit_body}
    applies a @nt{pragma} Elaborate_All to its own declaration,
    then the @nt{library_unit_body} will depend on itself.
  @end{Ramification}

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
  Any included @nt{library_unit_declaration} @Chg{Version=[3],New=[for],Old=[to]}
  which @Chg{Version=[3],New=[aspect],Old=[a @nt{pragma}]}
  Elaborate_Body @Chg{Version=[3],New=[is True @Redundant[(including when a
  @nt{pragma} Elaborate_Body applies)]],Old=[applies]}
  is immediately followed by its @nt{library_unit_body},
  if included.
@begin{Discussion}
  This implies that the body of such a library unit
  shall not @lquotes@;with@rquotes@; any of its own children,
  or anything else that depends semantically upon
  the declaration of the library unit.
@end{Discussion}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[@nt{Pragma} Elaborate_Body sets
  aspect Elaborate_Body, see @RefSecNum{Elaboration Control}.]}
@end{TheProof}

  All @nt{library_item}s declared pure occur before any
  that are not declared pure.

  All preelaborated @nt<library_item>s occur before any
  that are not preelaborated.
@end{Itemize}
@begin{Discussion}
Normally, if two partitions contain the same compilation unit,
they each contain a separate @i{copy} of that
compilation unit.
See @RefSec{Distributed Systems} for cases where two partitions share
the same copy of something.

There is no requirement that the main subprogram be elaborated last.
In fact, it is possible to write a partition in which the main
subprogram cannot be elaborated last.
@end{Discussion}
@begin{Ramification}
This @nt{declarative_part} has the properties required of all
environments (see @RefSecNum{The Compilation Process}).
However, the environment @nt{declarative_part} of a partition will
typically contain fewer compilation units than the environment
@nt{declarative_part} used at compile time @em only the @lquotes@;needed@rquotes@;
ones are included in the partition.
@end{Ramification}

There shall be a total order of the @nt{library_item}s that obeys the
above rules.
The order is otherwise implementation defined.
@begin{Discussion}
The only way to violate this rule is to have
Elaborate, Elaborate_All, or Elaborate_Body @nt{pragma}s that cause circular ordering
requirements, thus preventing an order that has no forward
elaboration dependences.
@end{Discussion}
@ImplDef{The order of elaboration of @nt{library_item}s.}
@begin{Honest}
@PDefn2{Term=[requires a completion], Sec=(library_unit_declaration)}
@Defn{notwithstanding}
Notwithstanding what the RM95 says elsewhere,
each rule that requires a declaration to have a corresponding
completion is considered to be a @LinkTimeName
when the declaration is that of a library unit.
@end{Honest}
@begin{Discussion}
Such rules may be checked at @lquotes@;link time,@rquotes@; for example.
Rules requiring the completion to have certain properties,
on the other hand, are checked at compile time of the completion.
@end{Discussion}


The full expanded names of the library units and subunits included in a
given partition shall be distinct.

@begin{Reason}
This is a @LinkTimeName because making it a @LegalityName
would violate the @MetaRulesName labeled
@lquotes@;legality determinable via semantic dependences.@rquotes@;
@end{Reason}

@leading@;The @nt{sequence_of_statements} of the environment task (see (2)
above) consists of either:
@begin{Itemize}
  A call to the main subprogram,
  if the partition has one.
  If the main subprogram has parameters, they are passed;
  where the actuals come from is implementation defined.
  What happens to the result of a main function is also
  implementation defined.
  @ImplDef{Parameter passing and function return for the main
  subprogram.}
@end{Itemize}

@leading@keepnext@;or:
@begin{Itemize}
  A @nt{null_statement}, if there is no main subprogram.
  @begin{Discussion}
    For a passive partition, either there is no environment task,
    or its @nt<sequence_of_statements> is an infinite loop.
    See @RefSecNum{Partitions}.
  @end{Discussion}
@end{Itemize}

The mechanisms for building and running partitions are implementation
defined.
@Redundant[These might be combined into one operation,
as, for example, in dynamic linking, or @lquotes@;load-and-go@rquotes@; systems.]
@ImplDef{The mechanisms for building and running partitions.}
@end{LinkTime}

@begin{RunTime}
@PDefn2{Term=[execution], Sec=(program)}
The execution of a program consists of the execution of a set of
partitions. Further details are implementation defined.
@PDefn2{Term=[execution], Sec=(partition)}
The execution of a partition starts with the execution
of its environment task,
ends when the environment task terminates,
and includes the executions of all tasks of the partition.
@Redundant[The execution of the (implicit) @nt<task_body> of the
environment task acts as a master
for all other tasks created as part of the execution of the partition.
When the environment task completes (normally or abnormally),
it waits for the termination of all such tasks,
and then finalizes any remaining objects of the partition.]
@begin{Ramification}
The @lquotes@;further details@rquotes@; mentioned above include,
for example, program termination @em it
is implementation defined.
There is no need to define it here;
it's entirely up to the implementation whether it wants
to consider the program as a whole to exist beyond the existence of
individual partitions.
@end{Ramification}
@ImplDef{The details of program execution,
including program termination.}
@begin{Honest}
@PDefn2{Term=[termination], Sec=(of a partition)}
@PDefn2{Term=[normal termination], Sec=(of a partition)}
@PDefn2{Term=[termination], Sec=(normal)}
@PDefn2{Term=[abnormal termination], Sec=(of a partition)}
@PDefn2{Term=[termination], Sec=(abnormal)}
The execution of the partition terminates (normally or abnormally) when the
environment task terminates (normally or abnormally, respectively).
@end{Honest}

@end{RunTime}

@begin{Bounded}
@PDefn2{Term=(bounded error),Sec=(cause)}
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
Once the environment task has awaited the termination of all other
tasks of the partition, any further attempt to create a task
(during finalization) is a bounded error, and may result in
the raising of Program_Error either upon creation or activation
of the task.
@PDefn{unspecified}
If such a task is activated, it is not specified
whether the task is awaited prior to termination of the
environment task.
@end{Bounded}

@begin{ImplReq}
@Leading@;The implementation shall ensure that all compilation units
included in a partition are consistent with one another,
and are legal according to the rules of the language.
@begin{Discussion}
The consistency requirement implies that
a partition cannot contain two versions of the same compilation unit.
That is,
a partition cannot contain two different library units with the
same full expanded name, nor two different bodies for the same
program unit.
For example, suppose we compile the following:
@begin{Example}
@key[package] A @key[is] --@RI{ Version 1.}
    ...
@key[end] A;

@key[with] A;
@key[package] B @key[is]
@key[end] B;

@key[package] A @key[is] --@RI{ Version 2.}
    ...
@key[end] A;

@key[with] A;
@key[package] C @key[is]
@key[end] C;
@end{Example}

It would be wrong for a partition containing B and C to contain both
versions of A.
Typically, the implementation would require the use of Version 2 of A,
which might require the recompilation of B.
Alternatively, the implementation might automatically recompile B
when the partition is built.
A third alternative would be an incremental compiler that,
when Version 2 of A is compiled,
automatically patches the object code for B to reflect the changes to
A (if there are any relevant changes @em there might not be any).

An implementation that supported fancy version management
might allow the use of Version 1 in some circumstances.
In no case can the implementation allow the use of both versions in
the same partition
(unless, of course, it can prove that the two versions are
semantically identical).

The core language says nothing about inter-partition
consistency; see also @RefSec{Distributed Systems}.
@end{Discussion}
@end{ImplReq}

@begin{ImplPerm}
@Defn{active partition}
The kind of partition described in this clause is known as an
@i{active} partition.
An implementation is allowed to support other kinds of partitions,
with implementation-defined semantics.
@ImplDef{The semantics of any nonactive partitions supported by the
implementation.}
@begin{Discussion}
@RefSec{Distributed Systems} defines the concept of passive partitions;
they may be thought of as a partition without an environment task,
or as one with a particularly simple form of environment task,
having an infinite loop rather than a call on a main subprogram
as its @nt<sequence_of_statements>.
@end{Discussion}

An implementation may restrict the kinds of subprograms it supports
as main subprograms.
However, an implementation is required to support all main
subprograms that are public parameterless library procedures.
@begin{Ramification}
The implementation is required to support main subprograms that are
procedures declared by @nt{generic_instantiation}s,
as well as those
that are children of library units other than Standard.
Generic units are, of course, not allowed to be main subprograms,
since they are not subprograms.

Note that renamings are irrelevant to this rule.
This rules says which subprograms (not views)
have to be supported.
The implementation can choose any way it wants for the
user to indicate which subprogram should be the main subprogram.
An implementation might allow any name of any view,
including those declared by renamings.
Another implementation might require it to be the original name.
Another implementation still might use the name of the source file or
some such thing.
@end{Ramification}

If the environment task completes abnormally,
the implementation may abort any dependent tasks.
@begin{Reason}
If the implementation does not take advantage of this permission,
the normal action takes place @em the environment task awaits those
tasks.

The possibility of aborting them
is not shown in the @i{Environment_Task} code above,
because there is nowhere to put an @nt{exception_handler} that can
handle exceptions raised in both the environment @nt{declarative_part}
and the main subprogram,
such that the dependent tasks can be aborted.
If we put an @nt{exception_handler} in the body of the environment
task, then it won't handle exceptions that occur during elaboration of
the environment @nt{declarative_part}.
If we were to move those things into a nested @nt{block_statement},
with the @nt{exception_handler} outside that,
then the @nt{block_statement} would await the library tasks we are
trying to abort.

Furthermore, this is merely a permission, and is not fundamental to the
model, so it is probably better to state it separately anyway.

Note that implementations (and tools like debuggers)
can have modes that provide other behaviors in addition.
@end{Reason}
@end{ImplPerm}

@begin{Notes}
An implementation may provide inter-partition
communication mechanism(s) via special packages and pragmas.
Standard pragmas for distribution and methods for specifying
inter-partition communication are defined in @RefSec{Distributed Systems}.
If no such mechanisms are provided, then each partition is isolated from all
others, and behaves as a program in and of itself.
@begin{Ramification}
Not providing such mechanisms is equivalent to disallowing multi-partition
programs.

An implementation may provide mechanisms to facilitate checking the
consistency of library units elaborated in different partitions;
@RefSec{Distributed Systems} does so.
@end{Ramification}

Partitions are not required to run in separate address spaces.
For example, an implementation might support dynamic linking
via the partition concept.

An order of elaboration of @nt{library_item}s that is consistent with
the partial ordering defined above does not always ensure that each
@nt{library_unit_body} is elaborated before any other compilation unit
whose elaboration necessitates that the @nt{library_unit_body} be
already elaborated.
(In particular, there is no requirement that the body of a library unit
be elaborated as soon as possible after the
@nt{library_unit_declaration} is elaborated,
unless the pragmas in subclause @RefSecNum{Elaboration Control} are
used.)

A partition (active or otherwise) need not have a main subprogram.
In such a case, all the work done by the partition would be done by
elaboration of various @nt{library_item}s, and by tasks created by that
elaboration.
Passive partitions, which cannot have main subprograms,
are defined in @RefSec{Distributed Systems}.
@begin{Ramification}
The environment task is the outermost semantic level
defined by the language.

Standard has no private part.
This prevents strange implementation-dependences involving private
children of Standard having visibility upon Standard's private part.
It doesn't matter where the body of Standard appears in the environment,
since it doesn't do anything.
See @RefSec{Predefined Language Environment}.

Note that elaboration dependence is carefully defined in such a way that
if (say) the body of something doesn't exist yet,
then there is no elaboration dependence upon the nonexistent body.
(This follows from the fact that @lquotes@;needed by@rquotes@; is defined that way,
and the elaboration dependences caused by a @nt{pragma} Elaborate or
Elaborate_All are defined in terms of @lquotes@;needed by@rquotes@;.)
This property allows us to use the environment concept both at compile
time and at partition-construction time/run time.
@end{Ramification}
@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
The concept of partitions is new to Ada 95.

A main subprogram is now optional.
The language-defined restrictions on main subprograms are relaxed.
@end{Extend83}

@begin{DiffWord83}
Ada 95 uses the term @lquotes@;main subprogram@rquotes@; instead of Ada 83's @lquotes@;main
program@rquotes@; (which was inherited from Pascal).
This is done to avoid confusion @em a main subprogram is a subprogram, not a
program.
The program as a whole is an entirely different thing.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00256-01]}
  @ChgAdded{Version=[2],Text=[The mistaken use of @lquotes@;mentions@rquotes@;
  in the elaboration dependence rule was fixed.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
  @ChgAdded{Version=[2],Text=[The @i<needs> relationship was extended to
  include limited views.]}
@end{DiffWord95}

@LabeledSubClause{Elaboration Control}

@begin{Intro}
@Redundant[@Defn{elaboration control}
This subclause defines pragmas that help control
the elaboration order of @nt{library_item}s.]
@end{Intro}

@begin{MetaRules}
The rules governing preelaboration are designed to allow
it to be done largely by bulk initialization of statically
allocated storage from information in a @lquotes@;load module@rquotes@; created by a linker.
Some implementations may require run-time code to be executed in some
cases, but we consider these cases rare enough that we need not
further complicate the rules.

It is important that programs be able to declare data structures that
are link-time initialized with @nt{aggregate}s, @nt{string_literal}s,
and concatenations thereof.
It is important to be able to write link-time evaluated expressions
involving the First, Last, and Length attributes of such data structures
(including variables), because they might be initialized with positional
@nt{aggregate}s or @nt{string_literal}s, and we don't want the user to
have to count the elements.
There is no corresponding need for accessing discriminants,
since they can be initialized with a static constant,
and then the constant can be referred to elsewhere.
It is important to allow link-time initialized data structures
involving discriminant-dependent components.
It is important to be able to write link-time evaluated expressions
involving pointers (both access values and addresses)
to the above-mentioned data structures.

The rules also ensure that no Elaboration_Check need be performed
for calls on library-level subprograms declared within
a preelaborated package.
This is true also of the Elaboration_Check on task activation
for library level task types declared in a preelaborated package.
However, it is not true of the Elaboration_Check on instantiations.

A static expression should never prevent a library unit from being
preelaborable.


@end{MetaRules}

@begin{Syntax}
@begin{SyntaxText}
@Leading@Keepnext@;The form of a @nt{pragma} Preelaborate is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Preelaborate)[(@SynI{library_unit_}@Syn2{name})];'

@begin{SyntaxText}
@PDefn2{Term=[library unit pragma], Sec=(Preelaborate)}
@PDefn2{Term=[pragma, library unit], Sec=(Preelaborate)}
A @nt{pragma} Preelaborate is a library unit pragma.
@end{SyntaxText}

@begin{SyntaxText}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00161-01]}
@ChgAdded{Version=[2],Type=[Leading],KeepNext=[T],Text=[The form of a
@nt{pragma} Preelaborable_Initialization is as follows:]}
@end{SyntaxText}
@ChgRef{Version=[2],Kind=[Added]}
@AddedPragmaSyn{Version=[2],@ChgAdded{Version=[2],
Text=<@key{pragma} @prag<Preelaborable_Initialization>(@Syn2{direct_name});>}}
@end{Syntax}

@begin{Legality}
@Leading@RootDefn2{Term=[preelaborable], Sec=(of an elaborable construct)}
An elaborable construct is preelaborable unless its
elaboration performs any of the following actions:
@begin{Ramification}
A @i{preelaborable} construct can be elaborated
without using any information that is available only at run time.
Note that we don't try to prevent exceptions in preelaborable
constructs; if the implementation wishes to generate code to
raise an exception, that's OK.

Because there is no flow of control and there are no calls
(other than to predefined subprograms),
these run-time properties can actually be detected at compile
time.
This is necessary in order to require compile-time enforcement of the
rules.
@end{Ramification}
@begin{Itemize}
The execution of a @nt{statement} other than a @nt{null_statement}.
@begin{Ramification}
A preelaborable construct can contain @nt{label}s and
@nt{null_statement}s.
@end{Ramification}

A call to a subprogram other than a static function.

The evaluation of a @nt{primary} that is a @nt{name}
of an object, unless the @nt{name} is a static expression,
or statically denotes a discriminant of an enclosing type.
@begin{Ramification}
One can evaluate such a @nt{name}, but not as a @nt{primary}.
For example, one can evaluate an attribute of the object.
One can evaluate an @nt{attribute_reference},
so long as it does not denote an object,
and its @nt{prefix} does not disobey any of these rules.
For example, Obj'Access, Obj'Unchecked_Access,
and Obj'Address are generally legal in preelaborated library
units.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00161-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0028-1]}
The creation of @Chg{Version=[2],New=[an object @Redundant[(including a
component)] @Chg{Version=[3],New=[that is initialized by default, if
its],Old=[of a]} type @Chg{Version=[3],New=[],Old=[that ]}does not have
preelaborable initialization. Similarly,],
Old=[a default-initialized object @Redundant[(including
a component)] of a descendant of a private type,
private extension, controlled type,
task type, or protected type with
@nt{entry_@!declaration}s; similarly]} the evaluation of
an @nt<extension_@!aggregate> with
an ancestor @nt<subtype_@!mark> denoting a subtype of such a type.

@begin{Ramification}
One can declare these kinds of types,
but one cannot create objects of those types.

It is also non-preelaborable to create an object if that will cause the
evaluation of a default expression that will call a user-defined
function.
This follows from the rule above forbidding non-null statements.
@end{Ramification}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00161-01]}
@ChgDeleted{Version=[2],Text=[Controlled objects are disallowed because most
implementations will have to take some run-time action during initialization,
even if the Initialize procedure is null.]}
@end{Reason}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00403-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[]}@Comment{Phony addition to get conditional leading}
A generic body is
preelaborable only if elaboration of a corresponding
instance body would not perform any such actions,
presuming that@Chg{Version=[2],New=[:],Old=[ the actual for each formal
private type (or extension) is
a private type (or extension), and the actual for each
formal subprogram is a user-defined subprogram.]}
@Defn{generic contract issue}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00403-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI95-0028-1]}
@ChgAdded{Version=[2],Text=[the actual for each@Chg{Version=[3],
New=[ discriminated formal derived type,],Old=[]} formal private
type@Chg{Version=[3],New=[, ],Old=[ (]}or
@Chg{Version=[3],New=[formal private ],Old=[]}extension@Chg{Version=[3],New=[],Old=[)]}
declared within the formal part of the generic unit is
a @Chg{Version=[3],New=[],Old=[private ]}type@Chg{Version=[3],New=[],Old=[ (or extension)]}
that does not have preelaborable initialization@Chg{Version=[3],
New=[, unless @nt<pragma>
Preelaborable_Initialization has been applied to the formal type],Old=[]};]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00403-01]}
@ChgAdded{Version=[2],Text=[the actual for each formal type is nonstatic;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00403-01]}
@ChgAdded{Version=[2],Text=[the actual for each formal object is nonstatic; and]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00403-01]}
@ChgAdded{Version=[2],Text=[the actual for each formal subprogram is a
user-defined subprogram.]}
@end{Itemize}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00403-01]}
  @ChgAdded{Version=[2],Text=[This is an @lquotes@;assume-the-worst@rquotes
  rule. The elaboration of a generic unit doesn't perform any of the actions
  listed above, because its sole effect is to establish that the generic can
  from now on be instantiated. So the elaboration of the generic itself is not
  the interesting part when it comes to preelaboration rules. The interesting
  part is what happens when you elaborate @lquotes@;any instantiation@rquotes
  of the generic. For instance, declaring an object of a limited formal private
  type might well start tasks, call functions, and do all sorts of
  non-preelaborable things. We prevent these situations by assuming that the
  actual parameters are as badly behaved as possible.]}
@end{Discussion}
@begin{Reason}
  Without this rule about generics, we would have to forbid
  instantiations in preelaborated library units,
  which would significantly reduce their usefulness.
@end{Reason}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0035],ARef=[AI95-00002-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0034-1],ARef=[AI05-0243-1]}
@PDefn{preelaborated}
@Chg{Version=[3],New=[A],Old=[If a]} @nt{pragma} Preelaborate (or
@nt<pragma> Pure @em see below) @Chg{Version=[3],New=[is used to specify that],
Old=[applies to]}
a library unit@Chg{Version=[3],New=[],Old=[, then it]} is
@i{preelaborated}@Chg{Version=[3],New=[, namely that the Preelaborate
aspect@AspectDefn{Preelaborate} of the library unit is True; all
compilation units of the
library unit are preelaborated],Old=[]}.
@Chg{Version=[3],New=[],Old=[@Redundant[
@RootDefn{preelaborated}
If a library unit is preelaborated, then its declaration, if any,
and body, if any, are elaborated
prior to all non-preelaborated @nt{library_item}s of the partition.]
]}@Chg{New=[The declaration and body of a preelaborated library
unit, and all subunits that are elaborated as part of elaborating the library
unit,], Old=[All compilation units of a preelaborated
library unit]} shall be preelaborable.@Chg{Version=[3],New=[
All
compilation units of a preelaborated library unit shall depend semantically only
on declared pure or preelaborated @nt{library_item}s.],Old=[]}
@PDefn{generic contract issue}
In addition to the places where @LegalityTitle normally apply
(see @RefSecNum{Generic Instantiation}),
@Chg{Version=[3],New=[these rules also apply],Old=[this rule applies also]}
in the private part of an instance of a generic unit.
@Chg{Version=[3],New=[@Redundant[
@RootDefn{preelaborated}
If a library unit is preelaborated, then its declaration, if any,
and body, if any, are elaborated
prior to all non-preelaborated @nt{library_item}s of the partition.]],
Old=[In addition, all compilation units of a preelaborated library unit
shall depend semantically only on
compilation units of other preelaborated library units.]}
@begin{Ramification}
In a generic body, we assume the worst about
formal private types and extensions.

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0035],ARef=[AI95-00002-01]}
@ChgAdded{Version=[1],Text=[Subunits of a preelaborated subprogram unit
do not need
to be preelaborable. This is needed in order to be consistent with units
nested in a subprogram body, which do not need to be preelaborable even if
the subprogram is preelaborated. However, such subunits cannot depend
semantically on non-preelaborated units, which is also consistent with
nested units.]}
@end{Ramification}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Preelaborate],
    Text=[@ChgAdded{Version=[3],Text=[Code execution during elaboration is
      avoided for a given package.]}]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00161-01]}
@ChgAdded{Version=[2],Text=[@defn{preelaborable initialization}The following rules
specify which entities have @i{preelaborable initialization}:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0028-1]}
@ChgAdded{Version=[2],Text=[The partial view
of a private type or private extension, a protected type without
@nt<entry_declaration>s, a generic formal private type, or a generic formal
derived type, @Chg{Version=[3],New=[has],Old=[have]} preelaborable
initialization if and only if the @nt<pragma> Preelaborable_Initialization has
been applied to them. @Redundant[A protected type with @nt{entry_declaration}s
or a task type never has preelaborable initialization.]]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[A component (including a discriminant) of a record or
protected type has preelaborable initialization if its declaration includes a
@nt<default_expression> whose execution does not perform any actions prohibited
in preelaborable constructs as described above, or if its declaration does not
include a default expression and its type has preelaborable
initialization.]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0028-1],ARef=[AI05-0221-1]}
@ChgAdded{Version=[2],Text=[A derived type has preelaborable initialization
if its parent type has preelaborable initialization and
@Chg{Version=[3],New=[],Old=[(in the case of a
derived record extension) ]}if the non-inherited components all have
preelaborable initialization. However, a @Chg{Version=[3],New=[],
Old=[user-defined ]}controlled type with an
@Chg{Version=[3],New=[],Old=[overriding ]}Initialize procedure
@Chg{Version=[3],New=[that is not a null procedure ],Old=[]}does not
have preelaborable initialization.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00161-01],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[A view of a type has preelaborable initialization if it
is an elementary type, an array type whose component type has preelaborable
initialization, a record type whose components all have preelaborable
initialization, or an interface type.]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00161-01]}
@ChgAdded{Version=[2],Text=[A @nt<pragma> Preelaborable_Initialization specifies that a type has
preelaborable initialization. This pragma shall appear in the visible part
of a package or generic package.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00161-01],ARef=[AI95-00345-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0028-1]}
@ChgAdded{Version=[2],Text=[If the pragma appears in the first list of
@nt{basic_declarative_item}s of a
@nt<package_specification>, then the @nt<direct_name> shall denote the first
subtype of a @Chg{Version=[3],New=[composite],Old=[private]}
type@Chg{Version=[3],New=[],Old=[, private extension, or protected type that is not
an interface type and is without
@nt<entry_declaration>s]}, and the type shall be declared immediately within
the same package
as the @nt<pragma>. If the @nt<pragma> is applied to a private type or a
private extension, the full view of the type shall have preelaborable
initialization. If the @nt<pragma> is applied to a protected type,
@Chg{Version=[3],New=[the protected type shall not have
entries, and ],Old=[]}each
component of the protected type shall have preelaborable initialization.
@Chg{Version=[3],New=[For any other composite type, the type shall have
preelaborable initialization. ],Old=[]}@PDefn{generic contract issue}In
addition to the places where @LegalityTitle normally apply
(see @RefSecNum{Generic Instantiation}), these rules apply
also in the private part of an instance of a generic unit.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0028-1]}
  @ChgAdded{Version=[3],Text=[The reason why we need the pragma for private
  types, private extensions, and protected types is fairly clear: the
  properties of the full view determine whether the type has preelaborable
  initialization or not; in order to preserve privacy we need a way to express
  on the partial view that the full view is well-behaved. The reason why we
  need the pragma for other composite types is more subtle: a non-null override
  for Initialize might occur in the private part, even for a nonprivate type;
  in order to preserve privacy, we need a way to express on a type declared in
  a visible part that the private part does not contain any nasty override of
  Initialize.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00161-01]}
@ChgAdded{Version=[2],Text=[If the @nt<pragma> appears in a @nt<generic_formal_part>, then the
@nt<direct_name> shall denote a generic formal private type or a generic formal
derived type declared in the same @nt<generic_formal_part> as the @nt<pragma>.
In a @nt<generic_instantiation> the corresponding actual type shall have
preelaborable initialization.]}

@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Not only do protected types with
@nt{entry_declaration}s
and task types not have preelaborable initialization, but they cannot have
pragma Preelaborable_Initialization applied to them.]}
@end{Ramification}
@end{Legality}

@begin{ImplAdvice}
In an implementation, a type declared in a preelaborated package
should have the same representation in every elaboration
of a given version of the package,
whether the elaborations occur in distinct executions of
the same program, or in executions of distinct programs or partitions
that include the given version.
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[A type declared in a preelaborated package
should have the same representation in every elaboration
of a given version of the package.]}]}
@end{ImplAdvice}

@begin{Syntax}
@begin{SyntaxText}
@leading@keepnext@;The form of a @nt{pragma} Pure is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Pure)[(@SynI{library_unit_}@Syn2{name})];'

@begin{SyntaxText}
@PDefn2{Term=[library unit pragma], Sec=(Pure)}
@PDefn2{Term=[pragma, library unit], Sec=(Pure)}
A @nt{pragma} Pure is a library unit pragma.
@end{SyntaxText}
@end{Syntax}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00366-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0035-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Defn{pure}
A @i{pure} @Chg{Version=[3],New=[compilation unit],Old=[@nt{library_item}]}
is a preelaborable @Chg{Version=[3],New=[compilation unit],Old=[@nt{library_item}]} whose
elaboration does not perform any of the following actions:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[the elaboration of a variable declaration;]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[the evaluation of an @nt{allocator} of an
access-to-variable type; for the purposes of this rule, the partial view of a
type is presumed to have non-visible components whose default initialization
evaluates such an @nt{allocator};]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0004-1]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[Such an @nt{allocator} would
  provide a backdoor way to get a global variable into a pure unit, so it is
  prohibited. Most such @nt{allocator}s are illegal anyway, as their type is
  required to have Storage_Size = 0 (see the next two rules). But access
  parameters and access discriminants don't necessarily disallow @nt{allocator}s.
  However, a call is also illegal here (by the preelaboration rules), so access
  parameters cannot cause trouble. So this rule is really about prohibiting
  allocators in discriminant constraints:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key[type] Rec (Acc : @key[access] Integer) @key[is record]
    C : Character;
@key[end record];]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Not_Const : @key[constant] Rec (Acc => @key[new] Integer'(2)); -- @RI{Illegal in a pure unit}.]}
@end{Example}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0004-1]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[The second half of the],Old=[This]}
  rule is needed because aggregates can specify the default initialization
  of a private type or extension using <> or the ancestor subtype of an
  extension aggregate. The
  subtype of a component could use an @nt{allocator} to initialize an access
  discriminant; the type still could have a pragma Preelaborable_Initialization
  given. Ada 95 did not allow such private types to have preelaborable
  initialization, so such a default initialization could not have occurred.
  Thus this rule is not incompatible with Ada 95.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0035-1]}
@ChgAdded{Version=[2],Text=[the elaboration of the declaration of a
@Chg{Version=[3],New=[non-derived ],Old=[]}named
access-to-variable type unless the Storage_Size of the type
has been specified by a static expression with value zero or
is defined by the language to be zero;]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[A remote access-to-class-wide type
  (see @RefSecNum{Remote Types Library Units}) has its Storage_Size defined to
  be zero.]}
@end{Discussion}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00366-01]}
  @ChgAdded{Version=[2],Text=[We disallow most named access-to-object types
  because an @nt{allocator} has a side effect; the pool constitutes variable
  data. We allow access-to-subprogram types because they
  don't have @nt{allocator}s. We even allow named access-to-object types if
  they have an empty predefined pool (they can't have a user-defined pool as
  System.Storage_Pools is not pure). In this case, most attempts to use an
  @nt{allocator} are illegal, and any others (in a generic body) will raise
  Storage_Error.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0035-1]}
@ChgAdded{Version=[2],Text=[the elaboration of the declaration of a
@Chg{Version=[3],New=[non-derived ],Old=[]}named
access-to-constant type for which the Storage_Size has been specified by an
expression other than a static expression with value zero.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We allow access-to-constant types so long as
  there is no user-specified non-zero Storage_Size; if there were a
  user-specified non-zero Storage_Size restricting the size of the storage
  pool, allocators would be problematic since the package is supposedly
  @lquote@;stateless@rquote, and the allocated size count for the storage pool
  would represent state.]}
@end{Discussion}

@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0035-1]}
@ChgAdded{Version=[3],Text=[A generic body is pure only if elaboration of a
corresponding instance body would not perform any such actions presuming any
composite formal types have non-visible components whose default initialization
evaluates an @nt{allocator} of an access-to-variable type.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00366-01]}
@ChgAdded{Version=[2],Text=[The Storage_Size for an anonymous
access-to-variable type declared at library level in a library unit that is
declared pure is defined to be zero.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This makes @nt{allocator}s illegal for such types
  (see @RefSecNum{Allocators}), making a storage pool unnecessary for these
  types. A storage pool would represent state.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Note that access discriminants and access
  parameters are never library-level, even when they are declared in a
  type or subprogram declared at library-level. That's because they have their
  own special accessibility rules (see @RefSecNum{Operations of Access Types}).]}
@end{Ramification}

@end{StaticSem}


@begin{Legality}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00366-01]}
@ChgDeleted{Version=[2],Text=[@Defn{pure}
A @i{pure} @nt{library_item} is a preelaborable @nt{library_item}
that does not contain the declaration of any
variable or named access within a
subprogram, generic
subprogram,
task unit, or protected unit.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00366-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0034-1],ARef=[AI05-0035-1],ARef=[AI05-0243-1]}
@Defn{declared pure}
A @nt{pragma} Pure is used to @Chg{Version=[3],New=[specify],Old=[declare]}
that a library unit is @Chg{Version=[3],New=[@i{declared pure}, namely
that the Pure aspect@AspectDefn{Pure} of the library unit is True; all
compilation units of the library unit are declared ],Old=[]}pure.
@Chg{Version=[3],New=[In addition, the limited view
of any library package is declared pure. The declaration and
body of a declared pure library unit, and all subunits that are
elaborated as part of elaborating the library unit, shall be pure.
All],Old=[If a @nt{pragma} Pure applies to a library unit,
then its]} compilation units @Chg{Version=[3],New=[of a declared pure
library unit],Old=[shall be pure, and they]} shall depend semantically
only on @Chg{Version=[3],New=[],Old=[compilation units of other library units
that are ]}declared pure@Chg{Version=[3],New=[ @nt{library_item}s],
Old=[]}.@Chg{Version=[3],New=[ @PDefn{generic contract issue}
In addition to the places where @LegalityTitle normally apply
(see @RefSecNum{Generic Instantiation}), these rules also
apply in the private part of an instance of a generic unit.],
Old=[]}@Chg{Version=[2],New=[ Furthermore, the full view of any
partial view declared in the visible part of @Chg{Version=[3],New=[a declared
pure],Old=[the]} library unit that has
any available stream attributes shall support external streaming
(see @RefSecNum{Stream-Oriented Attributes}).],Old=[]}
@begin{Honest}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0243-1]}@ChgNote{This is now described normatively}
@ChgDeleted{Version=[3],Text=[A @i{declared-pure} library unit is one to
which a @nt{pragma} Pure applies.
Its declaration and body are also said to be declared pure.]}
@end{Honest}
@begin{Discussion}
A declared-pure package is useful for defining types to be shared
between partitions with no common address space.
@end{Discussion}
@begin{Reason}
Note that generic packages are not mentioned
in the list of things that can contain variable declarations.
Note that the Ada 95 rules for deferred constants make them
allowable in library units that are declared pure;
that isn't true of Ada 83's deferred constants.
@end{Reason}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00366-01]}
Anonymous access types
@Chg{Version=[2],New=[],Old=[(that is, access discriminants and
access parameters) ]}are allowed.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0243-1]}
@ChgAdded{Version=[3],Text=[A limited view is not a library unit, so any rule
that starts @ldquote@;declared pure library unit@rdquote does not apply to a
limited view. In particular, the 3rd and last sentences never apply to limited
views. However, a limited view is a @nt{library_item}, so rules that discuss
@ldquote@;declared pure @nt{library_item}s@rdquote do include limited views.]}
@end{Ramification}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00366-01]}
@Chg{Version=[2],New=[Ada 95 didn't allow any access types as],
Old=[The primary reason for disallowing named
access types is that an @nt{allocator} has a side effect;
the pool constitutes variable data. We considered somehow allowing
@nt{allocator}-less access types. However,]} these
(including access-to-subprogram types)
@Chg{Version=[2],New=[],Old=[would ]}cause trouble
for @RefSec{Distributed Systems}, because such types @Chg{Version=[2],New=[],
Old=[would ]} allow access values in a shared passive
partition to designate objects in an active partition,
thus allowing inter-address space references.@Chg{Version=[2],New=[ We decided
to disallow such uses in the relatively rare cases where they cause problems,
rather than making life harder for the majority of users. Types declared
in a pure package can be used in remote operations only if they are externally
streamable. That simply means that there is a means to transport values of the
type; that's automatically true for nonlimited types that don't have an
access part. The only tricky part about this is to avoid privacy leakage; that
was handled by ensuring that any private types (and private
extensions) declared in a pure package that have available stream attributes
(which include all nonlimited types by definition) have to be externally
streamable.],
Old=[Furthermore, a named access-to-object type without a pool would be a new
concept, adding complexity from the user's point of view. Finally, the
prevention of @nt{allocator}s would have to be
a run-time check, in order to avoid violations of the generic contract
model.]}
@end{Reason}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Pure],
    Text=[@ChgAdded{Version=[3],Text=[Side effects are avoided in the
      subprograms of a given package.]}]}

@end{Legality}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00366-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0219-1]}
If a library unit is declared pure, then the implementation
is permitted to omit a call on a library-level
subprogram of the library unit if the
results are not needed after the call. @Chg{Version=[2],New=[In addition,
the implementation],Old=[Similarly, it]} may omit @Chg{Version=[2],New=[],
Old=[such ]}a call@Chg{Version=[2],New=[ on such a subprogram],Old=[]} and
simply reuse the results produced by an earlier call on
the same subprogram, provided that none of the
parameters @Chg{Version=[2],New=[nor any object accessible via access
values from the parameters ],Old=[]}@Chg{Version=[3],New=[have any part that
is of a type whose full type is an immutably],Old=[are of
a]} limited type, and the addresses and values of all by-reference
actual parameters, @Chg{Version=[2],New=[],Old=[and ]}the values of all
by-copy-in actual parameters, @Chg{Version=[2],New=[and the values of all
objects accessible via access values from the parameters, ],Old=[]}are
the same as they were at the earlier call.
@Redundant[This permission applies even if the subprogram
produces other side effects when called.]
@begin{Discussion}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00366-01]}
A declared-pure @nt{library_item} has no variable state.
Hence, a call on one of its (nonnested) subprograms cannot
@Chg{Version=[2],New=[normally],Old=[@lquotes@;normally@rquotes]}
have side effects.
The only possible side effects from such a call would be
through machine code insertions,@Chg{Version=[2],New=[ imported subprograms,],
Old=[]}unchecked conversion to an access type declared within the
subprogram, and similar features.
The compiler may omit a call to such a subprogram even if such
side effects exist, so the writer of such a subprogram
has to keep this in mind.
@end{Discussion}

@end{ImplPerm}

@begin{Syntax}
@begin{SyntaxText}
@Leading@Keepnext@;The form of a @nt{pragma} Elaborate, Elaborate_All, or
Elaborate_Body is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Elaborate)(@SynI{library_unit_}@Syn2{name}{, @SynI{library_unit_}@Syn2{name}});'

@PragmaSyn`@key{pragma} @prag(Elaborate_All)(@SynI{library_unit_}@Syn2{name}{, @SynI{library_unit_}@Syn2{name}});'

@PragmaSyn`@key{pragma} @prag(Elaborate_Body)[(@SynI{library_unit_}@Syn2{name})];'

@begin{SyntaxText}
A @nt{pragma} Elaborate or Elaborate_All is only allowed within a
@nt{context_clause}.
@begin{Ramification}
  @lquotes@;Within a @nt{context_clause}@rquotes@; allows it to be the last
  item in the @nt{context_clause}.
  It can't be first, because the @nt{name} has to denote something
  mentioned earlier.
@end{Ramification}

@PDefn2{Term=[library unit pragma], Sec=(Elaborate_Body)}
@PDefn2{Term=[pragma, library unit], Sec=(Elaborate_Body)}
A @nt{pragma} Elaborate_Body is a library unit pragma.
@end{SyntaxText}
@begin{Discussion}
Hence, a @nt{pragma} Elaborate or Elaborate_All is not
elaborated, not that it makes any practical difference.

Note that a @nt{pragma} Elaborate or Elaborate_All is neither a program unit
pragma, nor a library unit pragma.

@end{Discussion}
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@PDefn2{Term=[requires a completion], Sec=(declaration to which a @nt{pragma} Elaborate_Body applies)}
If @Chg{Version=[3],New=[the aspect],Old=[a @nt{pragma}]} Elaborate_Body
@Chg{Version=[3],New=[is True for],Old=[applies to]} a
declaration@Chg{Version=[3],New=[ @Redundant[(including when @nt{pragma}
Elaborate_Body applies)]],Old=[]},
then the declaration requires a completion @Redundant[(a
body)].@Chg{Version=[3],New=[@PDefn2{Term=[requires a completion],
Sec=(declaration for which aspect Elaborate_Body is True)}],Old=[]}

@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[@nt{Pragma} Elaborate_Body sets the
  aspect (see below).]}
@end{TheProof}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00217-06]}
@ChgAdded{Version=[2],Text=[The @SynI{library_unit_}@nt{name} of a
@nt{pragma} Elaborate or Elaborate_All shall denote a nonlimited view
of a library unit.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[These @nt{pragma}s are intended to prevent
  elaboration check failures. But a limited view does not make anything visible
  that has an elaboration check, so the @nt{pragma}s cannot do anything useful.
  Moreover, the @nt{pragma}s would probably reintroduce
  the circularity that the @nt{limited_with_clause} was intended to break.
  So we make such uses illegal.]}
@end{Reason}

@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@redundant[A @nt{pragma} Elaborate specifies that the body of the named library
unit is elaborated before the current @nt{library_item}.
A @nt{pragma} Elaborate_All specifies that each @nt{library_item} that is
needed by the named library unit declaration
is elaborated before the current @nt{library_item}.@Chg{Version=[3],New=[],Old=[
A @nt{pragma} Elaborate_Body specifies that the body
of the library unit is elaborated immediately after its declaration.]}]

@begin{TheProof}
The official statement of the semantics of these @nt{pragma}s is given in
@RefSecNum{Program Execution}.
@end{TheProof}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[A @nt{pragma} Elaborate_Body sets the Elaborate_Body
representation aspect of the library unit to which it applies to the value True.
@Redundant[If the Elaborate_Body aspect of a library unit is True, the body of the library
unit is elaborated immediately after its declaration.@AspectDefn{Elaborate_Body}]]}

@begin{TheProof}
  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[The official statement of the semantics of this
  aspect is given in @RefSecNum{Program Execution}.]}
@end{TheProof}
@begin{ImplNote}
The presence of a @nt{pragma} Elaborate_Body simplifies the removal of
unnecessary Elaboration_Checks.
For a subprogram declared immediately within a library unit to which
a @nt{pragma} Elaborate_Body applies, the only calls that can fail the
Elaboration_Check are those that occur in the library unit itself,
between the declaration and body of the called subprogram;
if there are no such calls
(which can easily be detected at compile time
if there are no @ntf{stub}s),
then no Elaboration_Checks are needed for that subprogram.
The same is true for Elaboration_Checks on task activations and
instantiations, and for library subprograms and generic units.
@end{ImplNote}
@begin{Ramification}
The fact that the unit of elaboration is the @nt{library_item}
means that if a @nt{subprogram_body} is not a completion, it is
impossible for any @nt{library_item} to be elaborated between the
declaration and the body of such a subprogram.
Therefore, it is impossible for a call to such a subprogram to fail its
Elaboration_Check.
@end{Ramification}
@begin{Discussion}
The visibility rules imply that
each @i{library_unit_}@nt{name} of a @nt{pragma} Elaborate
or Elaborate_All has to denote a library unit mentioned by a
previous @nt{with_clause} of the same @nt{context_clause}.
@end{Discussion}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Elaborate_Body],
    Text=[@ChgAdded{Version=[3],Text=[A given package must have a body, and that
      body is elaborated immediately after the declaration.]}]}

@end{StaticSem}

@begin{Notes}
A preelaborated library unit is allowed to have non-preelaborable
children.
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0035],ARef=[AI95-00002-01]}
But @Chg{New=[generally ], Old=[]}not non-preelaborated subunits.
@Chg{New=[(Non-preelaborated subunits of subprograms are allowed as
discussed above.)], Old=[]}
@end{Ramification}

A library unit that is declared pure is allowed to have impure
children.
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0035],ARef=[AI95-00002-01]}
But @Chg{New=[generally ], Old=[]}not impure subunits.
@Chg{New=[(Impure subunits of subprograms are allowed as discussed above.)],
Old=[]}
@end{Ramification}
@begin{Ramification}
Pragma Elaborate is mainly for closely related library units,
such as when two package bodies 'with' each other's declarations.
In such cases, Elaborate_All sometimes won't work.
@end{Ramification}
@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
The concepts of preelaborability
and purity are new to Ada 95.
The Elaborate_All, Elaborate_Body, Preelaborate,
and Pure @nt{pragma}s are new to Ada 95.

Pragmas Elaborate are allowed to be mixed in with the other
things in the @nt{context_clause} @em in Ada 83, they were
required to appear last.
@end{Extend83}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00366-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  The requirement that a partial view with available stream attributes
  be externally streamable can cause an incompatibility in rare cases.
  If there is a limited tagged
  type declared in a pure package with available attributes, and that
  type is used to declare a private extension in another pure package,
  and the full type for the private extension has a component of an
  explicitly limited record type, a protected type, or a type with
  access discriminants, then the stream attributes will have to be
  user-specified in the visible part of the package. That is not a requirement
  for Ada 95, but this combination seems very unlikely in pure packages.
  Note that this cannot be an incompatibility for a nonlimited type,
  as all of the types that are allowed in Ada 95 that would require
  explicitly defined stream attributes are limited (and thus cannot be used
  as components in a nonlimited type).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00403-01]}
  @ChgAdded{Version=[2],Text=[@B[Amendment Correction:] Added wording
  to cover missing cases for preelaborated generic units. This is
  incompatible as a preelaborated unit could have used a formal
  object to initialize a library-level object; that isn't allowed in Ada 2005. But such a unit wouldn't really
  be preelaborable, and Ada 95 compilers can reject such units (as this
  is a Binding Interpretation), so such units should be very rare.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00161-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @b[Amendment Correction:] The concept of preelaborable initialization and
  @nt{pragma} Preelaborable_Initialization are new. These allow more types
  of objects to be created in preelaborable units, and fix holes in the
  old rules.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00366-01]}
  @ChgAdded{Version=[2],Text=[Access-to-subprogram types and access-to-object
  types with a Storage_Size of 0 are allowed in pure units. The permission
  to omit calls was adjusted accordingly (which also fixes a hole in Ada 95, as
  access parameters are allowed, and changes in the values accessed by them
  must be taken into account).]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00002-01]}
  @ChgAdded{Version=[2],Text=[@B<Corrigendum:> The wording was changed so that
  subunits of a preelaborated subprogram are also preelaborated.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00217-06]}
  @ChgAdded{Version=[2],Text=[Disallowed pragma Elaborate and Elaborate_All
  for packages that are mentioned in a @nt{limited_with_clause}.]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0028-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@B<Correction:>
  Corrected a serious
  unintended incompatibility with Ada 95 in the new preelaboration wording
  @em explicit initialization of objects of types that don't have
  preelaborable initialization was not allowed. Ada 2012 switches
  back to the Ada 95 rule in these cases. This is unlikely to
  occur in practice, as it is unlikely that a compiler would have
  implemented the more restrictive rule (it would fail many ACATS tests
  if it did).]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0035-1]}
  @ChgAdded{Version=[3],Text=[@B<Correction:> Added an assume-the-worst
  rule for generic bodies (else they would never be checked for purity)
  and added the boilerplate so that the entire generic specification is
  rechecked. Also fixed wording to have consistent handling for subunits
  for Pure and Preelaborate. An Ada 95 program could have depended on
  marking a generic pure that was not really pure, although this
  would defeat the purpose of the categorization and likely cause
  problems with distributed programs.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0035-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}@B<Correction:>
  Adjusted wording so that a subunit can be pure (it is not a
  @nt<library_item>, but it is a compilation unit).]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0035-1]}
  @ChgAdded{Version=[3],Text=[@B<Correction:> Adjusted wording so
  that the rules for access types only apply to non-derived types
  (derived types share their storage pool with their parent, so if
  the parent access type is legal, so is any derived type.)]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[Elaborate_Body is now an aspect,
  so it can be specified by an @nt{aspect_specification} @em
  although the pragma is still preferred by the Standard.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0243-1]}
  @ChgAdded{Version=[3],Text=[Pure and Preelaborate are now aspects,
  so they can be specified by an @nt{aspect_specification} @em
  although the pragmas are still preferred by the Standard.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0034-1]}
  @ChgAdded{Version=[3],Text=[@B<Correction:> Added wording so that
  a limited view is always treated as pure, no matter what categorization
  is used for the originating unit. This was undefined in Ada 2005.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0028-1],ARef=[AI05-0221-1]}
  @ChgAdded{Version=[3],Text=[@B<Correction:> Fixed minor issues with
  preelaborable initialization (PI): null Initialize procedures do not make
  a type non-PI; formal types with pragma PI can be assumed to have PI;
  formal extensions are assumed to not have PI; all composite types
  can have pragma PI (so that the possibility of hidden Initialize routines
  can be handled); added discriminants of a derived type are not considered
  in calculating PI.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0219-1]}
  @ChgAdded{Version=[3],Text=[@B<Correction:> Clarified that the implementation
  permission to omit pure subprogram calls does not apply if any part of the
  parameters or any designated object has a part that is immutably limited.
  The old wording just said "limited type", which can change via visibility
  and thus isn't appropriate for dynamic semantics permissions.]}
@end{DiffWord2005}

