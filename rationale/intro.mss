@Part(intro, Root="rat.msm")

@comment{$Source: e:\\cvsroot/ARM/Rationale/intro.mss,v $}
@comment{$Revision: 1.7 $ $Date: 2006/02/03 07:39:46 $}

@LabeledSection{Introduction}

@Subheading{Abstract}

@i{This is the first of a number of papers describing the
rationale for Ada 2005. In due course it is anticipated that
the papers will be combined (after appropriate reformatting
and editing) into a single volume for formal publication.
This first paper covers the background to the development of
Ada 2005 and gives a brief overview of the main changes from
Ada 95. Other papers will then look at the changes in more
detail.}

@i{These papers are being published in the Ada User Journal. An
earlier version of this first paper appeared in the Ada User
Journal, Vol. 25, Number 4, December 2004. Other papers in
this series will be found in later issues of the Journal or
elsewhere on this website. The full series is expected to be}

@i{1@ @ @ Introduction}

@i{2@ @ @ Object oriented model}

@i{3@ @ @ Access types}

@i{4@ @ @ Structure and visibility}

@i{5@ @ @ Tasking and Real-Time}

@i{6@ @ @ Exceptions, generics etc}

@i{7@ @ @ Predefined library}

@i{8@ @ @ Containers}

@i{9@ @ @ Epilogue}


@LabeledClause{Revision process}

Readers will recall that the development of Ada 95 from Ada
83 was an extensive process funded by the USDoD. Formal
requirements were established after comprehensive surveys of
user needs and competitive proposals were then submitted
resulting in the selection of Intermetrics as the developer
under the devoted leadership of Tucker Taft. The whole
technical development process was then comprehensively
monitored by a distinct body of Distinguished Reviewers. Of
course, the process was also monitored by the ISO committee
concerned and the new language finally became an ISO standard
in 1995.

The development of Ada 2005 from Ada 95 has been (and
continues to be) on a more modest scale. The work has almost
entirely been by voluntary effort with support from within
the industry itself through bodies such as the Ada Resource
Association and Ada-Europe.

The development is being performed under the guidance of
ISO/IEC JTC1/SC22 WG9 (hereinafter just called WG9) chaired
adroitly by James Moore whose deep knowledge leads us safely
through the minefield of ISO procedures. This committee has
included national representatives of many nations including
Belgium, Canada, France, Germany, Italy, Japan, Sweden,
Switzerland, the UK and the USA. WG9 developed guidelines
@LocalLink{Target=[R1],Sec=[References],Text={[1]}}
for a revision to Ada 95 which were then used by the Ada
Rapporteur Group (the ARG) in drafting the revised standard.
@Defn{WG9}@Defn{ARG}

The ARG is a team of experts nominated by the national bodies
represented on WG9 and the two liaison organizations, ACM
SIGAda and Ada-Europe. The ARG was originally led with
Teutonic precision by Erhard Pl”dereder and is currently led
with Transalpine Gallic flair by Pascal Leroy. The editor,
who at the end of the day actually writes the words of the
standard, is the indefatigable Randy (fingers) Brukardt.

Suggestions for the revised standard have come from a number
of sources such as individuals on the ARG, national bodies on
WG9, users via email discussions on Ada-Comment and so on.

At the time of writing (June 2005), the revision process is
essentially finished. The details of all individual changes
are now clear and they have been integrated to form a new
version of the Annotated Ada Reference Manual. This is
currently being reviewed and the final approved standard
should emerge in the first half of 2006.

There has been much discussion on whether the language should
be called Ada 2005 or Ada 2006. For various reasons the WG9
meeting in York in June 2005 decided that the vernacular name
should be Ada 2005.

@LabeledClause{Scope of revision}
@leading@;The changes from Ada 83 to Ada 95 were large. They included
several major new items such as
@begin{Itemize}
polymorphism through tagged types, class-wide types and
dispatching,

the hierarchical library system including public and
private child packages,

protected objects for better real-time control,

more comprehensive predefined library, especially for
character and string handling,

specialized annexes such as those for system programming,
real-time, and numerics.
@end{Itemize}

By contrast the changes from Ada 95 to Ada 2005 are
relatively modest. Ada 95 was almost a new language which
happened to be compatible with Ada 83. However, a new
language always brings surprises and despite very careful
design things do not always turn out quite as expected when
used in earnest.

Indeed, a number of errors in the Ada 95 standard were
corrected in the Corrigendum issued in 2001
@LocalLink{Target=[R2],Sec=[References],Text={[2]}} and then
incorporated into the Consolidated Ada Reference Manual
@LocalLink{Target=[R3],Sec=[References],Text={[3]}}.
But it was still essentially the same language and further
improvement needed to be done.

Technically, Ada 2005 is defined as an Amendment to rather
than a Revision of the Ada 95 standard and this captures the
flavour of the changes as not being very extensive.

In a sense we can think of Ada 2005 as rounding out the rough
edges in Ada 95 rather than making major leaps forward. This
is perhaps not quite true of the Real-Time Systems annex
which includes much new material of an optional nature.
Nevertheless I am sure that the changes will bring big
benefits to users at hopefully not too much cost to
implementors.

The scope of the Amendment was guided by a document issued by
WG9 to the ARG in September 2002 @LocalLink{Target=[R1],Sec=[References],Text={[1]}}.
The key paragraph is:

"The main purpose of the Amendment is to address identified
problems in Ada that are interfering with Ada's usage or
adoption, especially in its major application areas (such as
high-reliability, long-lived real-time and/or embedded
applications and very large complex systems). The resulting
changes may range from relatively minor, to more
substantial."

Note that by saying "identified problems" it implicitly
rejects a major redesign such as occurred with Ada 95. The
phrase in parentheses draws attention to the areas where Ada
has a major market presence. Ada has carved an important
niche in the safety-critical areas which almost inevitably
are of a real-time and/or embedded nature. But Ada is also in
successful use in very large systems where the inherent
reliability and composition features are extremely valuable.
So changes should aim to help in those areas. And the final
sentence is really an exhortation to steer a middle course
between too much change and not enough.

The document then identifies two specific worthwhile changes,
namely, inclusion of the Ravenscar profile
@LocalLink{Target=[R4],Sec=[References],Text={[4]}} (for predictable real-time)
and a solution to the problem of mutually dependent types across two packages
(see Section @RefSecNum{Overview: Structure, visibility, and limited types} below).

@leading@keepnext@;The ARG is then requested to pay particular attention to
@begin{Description}
A@\Improvements that will maintain or improve Ada's
 advantages, especially in those user domains where safety
 and criticality are prime concerns. Within this area it
 cites as high priority, improvements in the real-time
 features and improvements in the high integrity features.
 Of lesser priority are features that increase static error
 checking. Improvements in interfacing to other languages
 are also mentioned.

B@\Improvements that will remedy shortcomings in Ada. It
 cites in particular improvements in OO features,
 specifically, adding a Java-like interface feature and
 improved interfacing to other OO languages.
@end{Description}

So the ARG is asked to improve both OO and real-time with a
strong emphasis on real-time and high integrity features. It
is interesting that WG9 rejected the thought that "design by
contract" features should be added to the above general
categories on the grounds that they would not be static.

@leading@keepnext@;The ARG is also asked to consider the following factors in
selecting features for inclusion:
@begin{Itemize}

Implementability. Can the feature be implemented at
reasonable cost?

Need. Do users actually need it? [A good one!]

Language stability. Would it appear disturbing to current
users?

Competition and popularity. Does it help to improve the
perception of Ada and make it more competitive?

Interoperability. Does it ease problems of interfacing
with other languages and systems? [That's the third
mention of interfacing.]

Language consistency. Is it syntactically and semantically
consistent with the language's current structure and
design philosophy?
@end{Itemize}

An important further statement is that "In order to produce a
technically superior result, it is permitted to compromise
backwards compatibility when the impact on users is judged to
be acceptable." In other words don't be paranoid about
compatibility.

Finally, there is a warning about secondary standards. Its
essence is don't use secondary standards if you can get the
material into the RM itself. And please put the stuff on
vectors and matrices from ISO/IEC 13813
@LocalLink{Target=[R5],Sec={References},Text={[5]}} into the language
itself. The reason for this exhortation is that secondary
standards have proved themselves to be almost invisible and
hence virtually useless.

The guidelines conclude with the target schedule. This
includes WG9 approval of the scope of the amendment in June
2004 which was achieved and submission to ISO/IEC JTC1 in
late 2005.

@LabeledClause{Overview of changes}

@leading@;It would be tedious to give a section by section review of
the changes as seen by the Reference Manual language lawyer.
Instead, the changes will be presented by areas as seen by
the user. There can be considered to be six areas:

@begin{Enumerate}

Improvements to the OO model. These include a more
traditional notation for invoking an operation of an
object without needing to know precisely where the
operation is declared (the @Exam{Obj.Op(...)} or prefixed style),
Java-like multiple inheritance using the concept of
interfaces, the introduction of null procedures as a
category of operation rather like an abstract operation,
and the ability to do type extension at a more nested
level than that of the parent type. There are also
explicit features for overcoming nasty bugs that arise
from confusion between overloading and overriding.

More flexible access types. Ada 95 access types have a
hair-shirt flavour compared with other languages because
of the general need for explicit conversions with named
access types. This is alleviated by permitting anonymous
access types in more contexts. It is also possible to
indicate whether an access type is an access to a constant and whether a null
value is permitted. Anonymous access-to-subprogram types are also introduced
thus permitting so-called downward closures.

Enhanced structure and visibility control. The most
important change here is the introduction of limited with
clauses which allow types in two packages to refer to each
other (the mutual dependence problem referred to in the
WG9 guidelines). This is done by extending the concept of
incomplete types (and introducing tagged incomplete
types). There are also private with clauses just providing
access from a private part. And there are significant
changes to limited types to make them more useful; these
include initialization using limited aggregates and
composition using a new form of return statement.

Tasking and real-time improvements. Almost all of the
changes are in the Real-Time Systems annex. They include
the introduction of the Ravenscar profile (as explicitly
mentioned in the WG9 guidelines) and a number of new
scheduling and dispatching policies. There are also new
predefined packages for controlling execution time clocks
and execution time budgets and for the notification of
task termination and similar matters. A change related to
the OO model is the introduction of protected and task
interfaces thereby drawing the OO and tasking aspects of
the language closer together.

Improvements to exceptions, numerics, generics etc. There
are some minor improvements in the exception area, namely,
neater ways of testing for null occurrence and raising an
exception with a message. Two small but vital numeric
changes are a @Exam{Mod} attribute to solve problems of mixing
signed and unsigned integers and a fix to the fixed-fixed
multiplication problem (which has kept some users locked
into Ada 83). There are also a number of new pragmas such
as: @Exam{Unsuppress} to complement the @Exam{Suppress} pragma, @Exam{Assert}
which was already offered by most vendors,
@Exam{Preelaborable_Initialization} which works with the existing
pragma @Exam{Preelaborate}, @Exam{No_Return} which indicates that a
procedure never returns normally, and @Exam{Unchecked_Union} to
ease interfacing to unchecked unions in C. There is also
the ability to have more control of partial parameters of
generic formal packages to improve package composition.

Extensions to the standard library. New packages include a
comprehensive Container library, mechanisms for directory
operations and access to environment variables, further
operations on times and dates, the vectors and matrices
material from ISO/IEC 13813 (as directed in the WG9
guidelines) plus commonly required simple linear algebra
algorithms. There are also wide-wide character types and
operations for 32-bit characters, the ability to use more
characters in identifiers, and improvements and extensions
to the existing string packages.
@end{Enumerate}

Of course, the areas mentioned above interact greatly and
much of 2 and 3 could be classified as improvements to the OO
model. There are also a number of changes not mentioned which
will mostly be of interest to experts in various areas. These
cover topics such as streams, object factory functions,
subtle aspects of the overload resolution rules, and the
categorization of packages with pragmas @Exam{Pure} and
@Exam{Preelaborate}.

The reader might feel that the changes are quite extensive
but each has an important role to play in making Ada more
useful. Indeed many other changes were rejected as really
unnecessary. These include old chestnuts such as @key{in out} and
@key{out} parameters for functions (ugh), extensible enumeration
types (a slippery slope), defaults for all generic parameters
(would lead one astray), and user-defined operator symbols (a
nightmare).

Before looking at the six areas in a little more detail it is
perhaps worth saying a few words about compatibility with Ada
95. The guidelines gave the ARG freedom to be sensible in
this area. Of course, the worst incompatibilities are those
where a valid program in Ada 95 continues to be valid in Ada
2005 but does something different. It is believed that
serious incompatibilities of this nature will never arise.
There are however, a very few minor and benign such
incompatibilities concerning the raising of exceptions such
as that with access parameters discussed in Section @RefSecNum{Overview: Access types}.

However, incompatibilities whereby a valid Ada 95 program
fails to compile in Ada 2005 are tolerable provided they are
infrequent. A few such incompatibilities are possible. The
most obvious cause is the introduction of three more reserved
words: @key{interface}, @key{overriding}, and @key{synchronized}. Thus if an
existing Ada 95 program uses any of these as an identifier
then it will need modification. The introduction of a new
category of unreserved keywords was considered for these so
that incompatibilities would not arise. However, it was felt
that this was ugly, confusing, and prone to introducing nasty
errors. In any event the identifiers @Exam{Overriding} and
@Exam{Synchronized} are likely to be rare and although @Exam{Interface} is
clearly a likely identifier nevertheless to have it both as
an identifier and as a keyword in the same program would be
nasty. Note also that the pragma Interface which many
compilers still support from Ada 83 (although not mentioned
by Ada 95 at all) is being put into
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-J-12.html],Text=[Annex J]}
for obsolescent features.

@LabeledSubClause{Overview: The object oriented model}

@leading@;The Ada 95 object oriented model has been criticized as not
following the true spirit of the OO paradigm in that the
notation for applying subprograms to objects is still
dominated by the subprogram and not by the object concerned.
It is claimed that real OO people always give the object
first and then the method (subprogram). Thus given

@begin{Example}
@key{package} P @key{is}
   @key{type} T @key{is tagged} ... ;

   @key{procedure} Op(X: T; ... );

   ...
@key{end} P;
@end{Example}

@leading@;then assuming that some variable @Exam{Y} is declared of type
@Exam{T}, in Ada 95 we have to write
@begin{Example}
P.Op(Y, ... );
@end{Example}
@leading@;in order to apply the procedure @Exam{Op} to the object @Exam{Y}
whereas a real OO person would expect to write something like
@begin{Example}
Y.Op( ... );
@end{Example}
where the object @Exam{Y} comes first and only any auxiliary
parameters are given in the parentheses.

A real irritation with the Ada 95 style is that the package @Exam{P}
containing the declaration of @Exam{Op} has to be mentioned as well.
(This assumes that use clauses are not being employed as is
often the case.) However, given an object, from its type we
can find its primitive operations and it is illogical to
require the mention of the package @Exam{P}. Moreover, in some cases
involving a complicated type hierarchy, it is not always
obvious to the programmer just which package contains the
relevant operation.

@leading@;The prefixed notation@Defn{prefixed notation} giving the object
first is now permitted in Ada 2005. The essential rules are that a
subprogram call of the form @Exam{P.Op(Y, ... );} can be replaced by
@Exam{Y.Op( ... );} provided that
@begin{Itemize}
@Exam{T} is a tagged type,

@Exam{Op} is a primitive (dispatching) or class wide operation of
@Exam{T},

@Exam{Y} is the first parameter of @Exam{Op}.
@end{Itemize}

@Leading@;The new prefixed notation has other advantages in unifying
the notation for calling a function and reading a component
of a tagged type. Thus consider the following geometrical
example which is based on that in a (hopefully familiar)
textbook @LocalLink{Target=[R6],Sec=[References],Text={[6]}}
@begin{Example}
@key{package} Geometry @key{is}
   @key{type} Object @key{is abstract tagged}
      @key{record}
         X_Coord: Float;
         Y_Coord: Float;
      @key{end record};

   @key[function] Area(O: Object) @key[return] Float @key[is abstract];
   @key[function] MI(O: Object) @key[return] Float @key[is abstract];
@key[end];
@end{Example}
@Leading@;The type @Exam{Object} has two components and two primitive
operations @Exam{Area} and @Exam{MI} (@Exam{Area} is the area of an object and @Exam{MI}
is its moment of inertia but the fine details of Newtonian
mechanics need not concern us). The key point is that with
the new notation we can access the coordinates and the area
in a unified way. For example, suppose we derive a concrete
type @Exam{Circle} thus

@begin{Example}
@key{package} Geometry.Circle @key{is}
   @key{type} Circle @key{is new} Object @key{with}
      @key{record}
         Radius: Float;
      @key{end record};

   @key{function} Area(C: Circle) @key{return} Float;
   @key{function} MI(C: Circle) @key{return} Float;
@key{end};
@end{Example}

@leading@;where we have provided concrete operations for @Exam{Area}
and @Exam{MI}.
Then in Ada 2005 we can access both the coordinates and area
in the same way

@begin{Example}
X:= A_Circle.X_Coord;
A:= A_Circle.Area;  -- @ExamCom{call of function Area}
@end{Example}

@leading@;Note that since @Exam{Area} just has one parameter (@Exam{A_Circle}) there
are no parentheses required in the call. This uniformity is
well illustrated by the body of @Exam{MI} which can be written as

@begin{Example}
   @key{function} MI(C: Circle) @key{is}
   @key{begin}
      @key{return} 0.5 * C.Area * C.Radius**2;
   @key{end} MI;
@end{Example}
whereas in Ada 95 we had to write
@begin{Example}
      @key{return} 0.5 * Area(C) * C.Radius**2;
@end{Example}
which is perhaps a bit untidy.

@leading@keepnext@;A related advantage concerns dereferencing. If we have an
access type such as
@begin{Example}
@key[type] Pointer @key[is access all] Object'Class;
...
This_One: Pointer := A_Circle'Access;
@end{Example}
@leading@keepnext@;and suppose we wish to print out the coordinates and area
then in Ada 2005 we can uniformly write
@begin{Example}
@tabset(P28)
Put(This_One.X_Coord); ...
Put(This_One.Y_Coord); ...
Put(This_One.Area); ...@\-- @ExamCom[Ada 2005]
@end{Example}
@leading@keepnext@;whereas in Ada 95 we have to write
@begin{Example}
@tabset(P28)
Put(This_One.X_Coord); ...
Put(This_One.Y_Coord); ...
Put(Area(This_One.@key[all])); ...@\-- @ExamCom[Ada 95]
@end{Example}

In Ada 2005 the dereferencing is all implicit whereas in Ada
95 some dereferencing has to be explicit which is ugly.

The reader might feel that this is all syntactic sugar for
the novice and of no help to real macho programmers. So we
shall turn to the topic of multiple inheritance. In Ada 95,
multiple inheritance is hard. It can sometimes be done using
generics and/or access discriminants (not my favourite topic)
but it is hard work and often not possible at all. So it is a
great pleasure to be able to say that Ada 2005 introduces
real multiple inheritance in the style of Java.@Defn{multiple inheritance}

The problem with multiple inheritance in the most general
case is clashes between the parents. Assuming just two
parents, what happens if both parents have the same component
(possibly inherited from a common ancestor)? Do we get two
copies? And what happens if both parents have the same
operation but with different implementations? These and
related problems are overcome by placing firm restrictions on
the possible properties of parents. This is done by
introducing the notion of an interface.@Defn{interface}

@leading@;An interface can be thought of as an abstract type with no
components @en but it can of course have abstract operations.
It has also proved useful to introduce the idea of a null
procedure as an operation of a tagged type; we don't have to
provide an actual body for such a null procedure (and indeed
cannot) but it behaves as if it has a body consisting of just
a null statement. So we might have
@begin{Example}
@key[package] P1 @key[is]
   @key[type] Int1 @key[is interface];
   @key[procedure] Op1(X: Int1) @key[is abstract];
   @key[procedure] N(X: Int1) @key[is null];
end P1;
@end{Example}
@leading@;Note carefully that @key[interface] is a new reserved word.@Defn2{Term=[interface],Sec=[keyword]}
We could now derive a concrete type from the interface @Exam{Int1} by
@begin{Example}
   @key[type] DT @key[is new] Int1 @key[with record] ... @key[end record];
   @key[procedure] Op1(NX: DT);
@end{Example}

We can provide some components for @Exam{DT} as shown (although this
is optional). We must provide a concrete procedure for @Exam{Op1}
(we wouldn't if we had declared @Exam{DT} itself as abstract). But
we do not have to provide an overriding of @Exam{N} since it behaves
as if it has a concrete null body anyway (but we could
override @Exam{N} if we wanted to).

We can in fact derive a type from several interfaces plus
possibly one conventional tagged type. In other words we can
derive a tagged type from several other types (the ancestor
types) but only one of these can be a normal tagged type (it
has to be written first). We refer to the first as the parent
(so the parent can be an interface or a normal tagged type)
and any others as progenitors (and these have to be
interfaces).

@leading@;So assuming that @Exam{Int2} is another interface type and that
@Exam{T1} is a normal tagged type then all of the following are
permitted
@begin{Example}
@key[type] DT1 @key[is new] T1 @key[and] Int1 @key[with null record];

@key[type] DT2 @key[is new] Int1 @key[and] Int2 @key[with]
   @key[record] ... @key[end record];

@key[type] DT3 @key[is new] T1 and Int1 and Int2 @key[with] ...
@end{Example}

@Leading@keepnext@;It is also possible to compose interfaces to create further
interfaces thus
@begin{Example}
@key[type] Int3 @key[is interface and] Int1;
...
@key[type] Int4 @key[is interface and] Int1 @key[and] Int2 @key[and] Int3;
@end{Example}

Note carefully that @key[new] is not used in this construction.
Such composed interfaces have all the operations of all their
ancestors and further operations can be added in the usual
way but of course these must be abstract or null.

There are a number of simple rules to resolve what happens if
two ancestor interfaces have the same operation. Thus a null
procedure overrides an abstract one but otherwise repeated
operations have to have the same profile.

@leading@keepnext@;Interfaces can also be marked as limited.
@begin{Example}
@key[type] LI @key[is limited interface];
@end{Example}

An important rule is that a descendant of a nonlimited
interface must be nonlimited. But the reverse is not true.

Some more extensive examples of the use of interfaces will be
given in a later paper (see @RefSecNum{Interfaces}).

@leading@;Incidentally, the newly introduced null procedures are not
just for interfaces. We can give a null procedure as a
specification whatever its profile and no body is then
required or allowed. But they are clearly of most value with
tagged types and inheritance. Note in particular that the
package @Exam{Ada.Finalization} in Ada 2005 is
@begin{Example}
@key[package] Ada.Finalization @key[is]
   @key[pragma] Preelaborate(Finalization);
   @key[pragma] Remote_Types(Finalization);

   @key[type] Controlled @key[is abstract tagged private];
   @key[pragma] Preeleborable_Initialization(Controlled);
   @key[procedure] Initialize(Object: @key[in out] Controlled) @key[is null];
   @key[procedure] Adjust(Object: @key[in out] Controlled) @key[is null];
   @key[procedure] Finalize(Object: @key[in out] Controlled) @key[is null];

   -- @ExamCom[similarly for Limited_Controlled]
   ...
@key[end] Ada.Finalization;
@end{Example}

The procedures @Exam{Initialize}, @Exam{Adjust}, and @Exam{Finalize} are now
explicitly given as null procedures. This is only a cosmetic
change since the Ada 95 RM states that the default
implementations have no effect. However, this neatly
clarifies the situation and removes ad hoc semantic rules.
(The pragma @Exam{Preelaborable_Initialization} will be explained in
a later paper @en see @RefSecNum{Pragmas and Restrictions}.)

Another important change is the ability to do type extension
at a level more nested than that of the parent type. This
means that controlled types can now be declared at any level
whereas in Ada 95, since the package @Exam{Ada.Finalization} is at
the library level, controlled types could only be declared at
the library level. There are similar advantages in generics
since currently many generics can only be instantiated at the
library level.

The final change in the OO area to be described here is the
ability to (optionally) state explicitly whether a new
operation overrides an existing one or not.

@leading@;At the moment, in Ada 95, small careless errors in subprogram
profiles can result in unfortunate consequences whose cause
is often difficult to determine. This is very much against
the design goal of Ada to encourage the writing of correct
programs and to detect errors at compilation time whenever
possible. Consider
@begin{Example}
@key[with] Ada.Finalization; @key[use] Ada.Finalization;
@key[package] Root @key[is]
   @key[type] T @key[is new] Controlled @key[with] ... ;
   @key[procedure] Op(Obj: @key[in out] T; Data: @key[in] Integer);
   @key[procedure] Finalise(Obj: @key[in out] T);
@key[end] Root;
@end{Example}

Here we have a controlled type plus an operation @Exam{Op} of that
type. Moreover, we intended to override the automatically
inherited null procedure @Exam{Finalize} of @Exam{Controlled} but, being
foolish, we have spelt it @Exam{Finalise}. So our new procedure does
not override @Exam{Finalize} at all but merely provides another
operation. Assuming that we wrote @Exam{Finalise} to do something
useful then we will find that nothing happens when an object
of the type @Exam{T} is automatically finalized at the end of a
block because the inherited null procedure is called rather
than our own code. This sort of error can be very difficult
to track down.

@leading@;In Ada 2005 we can protect against such errors since it is
possible to mark overriding operations as such thus
@begin{Example}
   @key{overriding}
   @key{procedure} Finalize(Obj: in out T);
@end{Example}
And now if we spell @Exam{Finalize} incorrectly then the compiler
will detect the error. Note that @key{overriding} is another new
reserved word. However, partly for reasons of compatibility,
the use of overriding indicators is optional; there are also
deeper reasons concerning private types and generics which
will be discussed in a later paper @en see
@RefSecNum{Overriding and overloading}.@Defn2{Term=[overriding],
Sec=[keyword]}

@Leading@;Similar problems can arise if we get the profile wrong.
Suppose we derive a new type from @Exam{T} and attempt to override
@Exam{Op} thus
@begin{Example}
@key[package] Root.Leaf @key[is]
   @key[type] NT @key[is new] T @key[with null record];
   @key[procedure] Op(Obj: @key[in out] NT; Data: @key[in] String);
@key[end] Root.Leaf;
@end{Example}

@leading@;In this case we have given the identifier @Exam{Op} correctly but
the profile is different because the parameter @Exam{Data} has
inadvertently been declared as of type @Exam{String} rather than
@Exam{Integer}. So this new version of @Exam{Op} will simply be an
overloading rather than an overriding. Again we can guard
against this sort of error by writing
@begin{Example}
   @key[overriding]
   @key[procedure] Op(Obj: @key[in out] NT; Data: @key[in] Integer);
@end{Example}
@leading@;On the other hand maybe we truly did want to provide a new
operation. In this case we can write @key[not overriding] and the
compiler will then ensure that the new operation is indeed
not an overriding of an existing one thus
@begin{Example}
   @key[not overriding]
   @key[procedure] Op(Obj: @key[in out] NT; Data: @key[in] String);
@end{Example}
The use of these overriding indicators prevents errors during
maintenance. Thus if later we add a further parameter to @Exam{Op}
for the root type @Exam{T} then the use of the indicators will
ensure that we modify all the derived types appropriately.@Defn{overriding indicator}

@LabeledSubclause{Overview: Access types}

It has been said that playing with pointers is like playing
with fire @en properly used all is well but carelessness can
lead to disaster. In order to avoid disasters, Ada 95 takes a
stern view regarding the naming of access types and their
conversion. However, experience has shown that the Ada 95
view is perhaps unnecessarily stern and leads to tedious
programming.

@leading@;We will first consider the question of giving names to access
types. In Ada 95 all access types are named except for access
parameters and access discriminants. Thus we might have
@begin{Example}
@tabset(P49)
@key[type] Animal @key[is tagged]
   @key[record] Legs: Integer; ... @key[end record];

@key[type] Acc_Animal @key[is access] Animal;@\-- @ExamCom{named}

@key[procedure] P(Beast: @key[access] Animal; ... );@\-- @ExamCom{anonymous}
@end{Example}

@leading@;Moreover, there is a complete lack of symmetry between named
access types and access parameters. In the case of named
access types, they all have a null value (and this is the
default on declaration if no initial value be given). But in
the case of access parameters, a null value is not permitted
as an actual parameter. Furthermore, named access types can
be restricted to be access to constant types such as
@begin{Example}
@key[type] Rigid_Animal @key[is access constant] Animal;
@end{Example}

@leading@;which means that we cannot change the value of the @Exam{Animal}
referred to. But in the case of access parameters, we cannot say
@begin{Example}
@tabset(P49)
@key[procedure] P(Beast: @key[access constant] Animal);@\-- @ExamCom{not Ada 95}
@end{Example}

In Ada 2005 almost all these various restrictions are swept
away in the interests of flexibility and uniformity.

@leading@;First of all we can explicitly specify whether an access type
(strictly subtype) has a null value. We can write
@begin{Example}
@key[type] Acc_Animal @key[is not null access all] Animal'Class;
@end{Example}

@leading@;This means that we are guaranteed that an object of type
@Exam{Acc_Animal} cannot refer to a null animal. Therefore, on
declaration such an object should be initialized as in the
following sequence
@begin{Example}
@key[type] Pig @key[is new] Animal @key[with] ... ;
Empress_Of_Blandings: @key[aliased] Pig := ... ;

My_Animal: Acc_Animal := Empress_Of_Blandings'Access; -- @ExamCom{must initialize}
@end{Example}

(The Empress of Blandings is a famous pig in the novels
concerning Lord Emsworth by the late P G Wodehouse.) If we
forget to initialize @Exam{My_Animal} then @Exam{Constraint_Error} is
raised; technically the underlying type still has a null
value but @Exam{Acc_Animal} does not. We can also write @key[not null
access constant] of course.@Defn{null exclusion}

@leading@;The advantage of using a null exclusion is that when we come
to do a dereference
@begin{Example}
Number_of_Legs: Integer := My_Animal.Legs;
@end{Example}

then no check is required to ensure that we do not
dereference a null pointer. This makes the code faster.

@leading@;The same freedom to add @key[constant] and @key[not null] also
applies to access parameters. Thus we can write all of the following in
Ada 2005
@begin{Example}
procedure P(Beast: @key[access] Animal);
procedure P(Beast: @key[access constant] Animal);
procedure P(Beast: @key[not null access] Animal);
procedure P(Beast: @key[not null access constant] Animal);
@end{Example}

Note that @key[all] is not permitted in this context since access
parameters always are general (that is, they can refer to
declared objects as well as to allocated ones).

Note what is in practice a minor incompatibility, the first
of the above now permits a null value as actual parameter in
Ada 2005 whereas it was forbidden in Ada 95. This is actually
a variation at runtime which is normally considered
abhorrent. But in this case it just means that any check that
will still raise @Exam{Constraint_Error} will be in a different
place @en and in any event the program was presumably
incorrect.

@Leading@;Another change in Ada 2005 is that we can use anonymous
access types other than just as parameters (and
discriminants).@Defn2{Term=[anonymous access type],Sec=[generalized]} We
can in fact also use anonymous access types in
@begin{Itemize}
the declaration of stand-alone objects and components of
arrays and records,

a renaming declaration,

a function return type.
@end{Itemize}

@leading@keepnext@;Thus we can extend our farmyard example
@begin{Example}
@key[type] Horse @key[is new] Animal @key[with] ... ;

@key[type] Acc_Horse @key[is access all] Horse;
@key[type] Acc_Pig @key[is access all] Pig;

Napoleon, Snowball: Acc_Pig := ... ;

Boxer, Clover: Acc_Horse := ... ;
@end{Example}
@leading@keepnext@;and now we can declare an array of animals
@begin{Example}
Animal_Farm: @key[constant array] (Positive @key[range] <>) @key[of access] Animal'Class :=
                                             (Napoleon, Snowball, Boxer, Clover);
@end{Example}

@leading@;(With acknowledgments to George Orwell.) Note that the
components of the array are of an anonymous access type. We
can also have record components of an anonymous type
@begin{Example}
@key[type] Ark @key[is]
   @key[record]
      Stallion, Mare: @key[access] Horse;
      Boar, Sow: @key[access] Pig;
      Cockerel, Hen: @key[access] Chicken;
      Ram, Ewe: @key[access] Sheep;
      ...
   @key[end record];

Noahs_Ark: Ark := (Boxer, Clover, ... );
@end{Example}

This is not a very good example since I am sure that Noah
took care to take actual animals into the Ark and not merely
their addresses.

@leading@keepnext@;A more useful example is given by the classic linked list. In
Ada 95 (and Ada 83) we have
@begin{Example}
@key[type] Cell;
@key[type] Cell_Ptr @key[is access] Cell;
@key[type] Cell @key[is]
   @key[record]
      Next: Cell_Ptr;
      Value: Integer;
   @key[end record];
@end{Example}

@leading@;In Ada 2005, we do not have to declare the type @Exam{Cell_Ptr} in
order to declare the type @Exam{Cell} and so we do not need to use
the incomplete declaration to break the circularity. We can
simply write
@begin{Example}
@key[type] Cell @key[is]
   @key[record]
      Next: @key[access] Cell;
      Value: Integer;
   @key[end record];
@end{Example}

Here we have an example of the use of the type name @key[Cell]
within its own declaration. In some cases this is interpreted
as referring to the current instance of the type (for
example, in a task body) but the rule has been changed to
permit its usage as here.

@leading@keepnext@;We can also use an anonymous access type for a single
variable such as
@begin{Example}
List: @key[access] Cell := ... ;
@end{Example}

@leading@;An example of the use of an anonymous access type for a
function result might be in another animal function such as
@begin{Example}
@key[function] Mate_Of(A: @key[access] Animal'Class) @key[return access] Animal'Class;
@end{Example}
@leading@keepnext@;We could then perhaps write
@begin{Example}
@key[if] Mate_Of(Noahs_Ark.Ram) /= Noahs_Ark.Ewe @key[then]
   ... -- @ExamCom{better get Noah to sort things out}
@key[end if];
@end{Example}
Anonymous access types can also be used in a renaming
declaration. This and other detailed points on matters such
as accessibility will be discussed in a later paper
(see @RefSecNum{Anonymous access types}).

The final important change in access types concerns access to
subprogram types. Access to subprogram types were introduced
into Ada 95 largely for the implementation of callback. But
important applications of such types in other languages
(going back to Pascal and even Algol 60) are for mathematical
applications such as integration where a function to be
manipulated is passed as a parameter. The Ada 83 and Ada 95
approach has always been to say "use generics". But this can
be clumsy and so a direct alternative is now provided.

@leading@keepnext@;Recall that in Ada 95 we can write
@begin{Example}
@key[type] Integrand @key[is access function](X: Float) @key[return] Float;
@key[function] Integrate(Fn: Integrand; Lo, Hi: Float) @key[return] Float;
@end{Example}

@leading@;The idea is that the function @Exam{Integrate} finds the value of
the integral of the function passed as parameter @Exam{Fn} between
the limits @Exam{Lo} and @Exam{Hi}. This works fine in Ada 95 for simple
cases such as where the function is declared at library
level. Thus to evaluate
@begin{Example}@Comment{This might be better displayed as a graphic; a lot of systems won't have these characters.}
@Roman{@grow{@grow{@Unicode(8992)}@+{1}
@grow{@Unicode(9474)}  @Unicode(8730)x @i{dx}
@grow{@Unicode(8993)}@-{0}}}
@end{Example}

@leading@keepnext@;we can write
@begin{Example}
Result := Integrate(Sqrt'Access, 0.0, 1.0);
@end{Example}

where the function @Exam{Sqrt} is from the library package
@Exam{Ada.Numerics.Elementary_Functions}.

@leading@;However, if the function to be integrated is more elaborate
then we run into difficulties in Ada 95 if we attempt to use
access to subprogram types. Consider the following example
which aims to compute the integral of the expression @i{xy} over
the square region 0 @Unicode(8804) x, y @Unicode(8804) 1.
@begin{Example}
@tabset(P42)
@key{with} Integrate;
@key{procedure} Main @key{is}
   @key{function} G(X: Float) @key{return} Float @key{is}
      @key{function} F(Y: Float) @key{return} Float @key{is}
      @key{begin}
         @key{return} X*Y;
      @key{end} F;
   @key{begin}
      @key{return} Integrate(F'Access, 0.0, 1.0);@\-- @ExamCom{illegal in Ada 95}
   @key{end} G;

   Result: Float;

@key{begin}
   Result:= Integrate(G'Access, 0.0, 1.0);@\-- @ExamCom{illegal in Ada 95}
   ...
@key[end] Main;
@end{Example}

But this is illegal in Ada 95 because of the accessibility
rules necessary with named access types in order to prevent
dangling references. Thus we need to prevent the possibility
of storing a pointer to a local subprogram in a global
structure. This means that both @Exam{F'Access} and @Exam{G'Access} are
illegal in the above.

Note that although we could make the outer function @Exam{G} global
so that @Exam{G'Access} would be allowed nevertheless the function @Exam{F}
has to be nested inside @Exam{G} in order to gain access to the
parameter @Exam{X} of @Exam{G}. It is typical of functions being integrated
that they have to have information passed globally @en the
number of parameters of course is fixed by the profile used
by the function Integrate.

@leading@;The solution in Ada 2005 is to introduce anonymous access to
subprogram types by analogy with anonymous access to object
types.@Defn{anonymous access-to-subprogram} Thus the function
@Exam{Integrate} becomes
@begin{Example}
@key[function] Integrate(Fn: @key[access function](X: Float) @key[return] Float;
                   Lo, Hi: Float) @key[return] Float;
@end{Example}

Note that the parameter @Exam{Fn} has an anonymous type defined by
the profile so that we get a nesting of profiles. This may
seem a bit convoluted but is much the same as in Pascal.

The nested example above is now valid and no accessibility
problems arise. (The reader will recall that accessibility
problems with anonymous access to object types are prevented
by a runtime check; in the case of anonymous access to
subprogram types the corresponding problems are prevented by
decreeing that the accessibility level is infinite @en actually
the RM says larger than that of any master which comes to the
same thing.)

Anonymous access to subprogram types are also useful in many
other applications such as iterators as will be illustrated
later.

Note that we can also prefix all access to subprogram types,
both named and anonymous, by constant and not null in the
same way as for access to object types.

@LabeledSubclause{Overview: Structure, visibility, and limited types}

Structure is vital for controlling visibility and thus
abstraction. There were huge changes in Ada 95. The whole of
the hierarchical child unit mechanism was introduced with
both public and private children. It was hoped that this
would provide sufficient flexibility for the future.

@leading@;But one problem has remained. Suppose we have two types where
each wishes to refer to the other. Both need to come first!
Basically we solve the difficulty by using incomplete types.
We might have a drawing package concerning points and lines
in a symmetric way. Each line contains a list or array of the
points on it and similarly each point contains a list or
array of the lines through it. We can imagine that they are
both derived from some root type containing printing
information such as color. In Ada 95 we might write
@begin{Example}
@key[type] Object @key[is abstract tagged]
   @key[record]
      Its_Color: Color;
      ...
   @key[end record];

@key[type] Point;
@key[type] Line;
@key[type] Acc_Point @key[is access all] Point;
@key[type] Acc_Line @key[is access all] Line;

@key[subtype] Index @key[is] Integer @key[range] 0 .. Max;
@key[type] Acc_Line_Array @key[is array] (1 .. Max) @key[of] Acc_Line;
@key[type] Acc_Point_Array @key[is array] (1 .. Max) @key[of] Acc_Point;

@key[type] Point @key[is new] Object @key[with]
   @key[record]
      No_Of_Lines: Index;
      LL: Acc_Line_Array;
      ...
   @key[end record];

@key[type] Line @key[is new] Object @key[with]
   @key[record]
      No_Of_Points: Index;
      PP: Acc_Point_Array;
      ...
   @key[end record];
@end{Example}

This is very crude since it assumes a maximum number @Exam{Max} of
points on a line and vice versa and declares the arrays
accordingly. The reader can flesh it out more flexibly. Well
this is all very well but if the individual types get
elaborate and each has a series of operations, we might want
to declare them in distinct packages (perhaps child packages
of that containing the root type). In Ada 95 we cannot do
this because both the incomplete declaration and its
completion have to be in the same package.

The net outcome is that we end up with giant cumbersome
packages.

What we need therefore is some way of logically enabling the
incomplete view and the completion to be in different
packages. The elderly might remember that in the 1980 version
of Ada the situation was even worse @en the completion had to
be in the same list of declarations as the incomplete
declaration. Ada 83 relaxed this (the so-called Taft
Amendment) and permits the private part and body to be
treated as one list @en the same rule applies in Ada 95. We now
go one step further.

@leading@;Ada 2005 solves the problem by introducing a variation on the
with clause @en the limited with clause.@Defn{limited with clause}@Defn2{Term=[with clause],Sec=[limited]}
The idea is that a
library package (and subprogram) can refer to another library
package that has not yet been declared and can refer to the
types in that package but only as if they were incomplete
types. Thus we might have a root package @Exam{Geometry} containing
the declarations of @Exam{Object}, @Exam{Max}, @Exam{Index}, and so on and then
@begin{Example}
@key[limited with] Geometry.Lines;
@key[package] Geometry.Points @key[is]

   @key[type] Acc_Line_Array @key[is array] (1 .. Max) @key[of access] Lines.Line;

   @key[type] Point @key[is new] Object @key[with]
      @key[record]
         No_Of_Lines: Index;
         LL: Acc_Line_Array;
         ...
      @key[end record];

      ...
@key[end] Geometry.Points;
@end{Example}

The package @Exam{Geometry.Lines} is declared in a similar way. Note
especially that we are using the anonymous access type
facility discussed in Section 3.2 and so we do not even have
to declare named access types such as @Exam{Acc_Line} in order to
declare @Exam{Acc_Line_Array}.

By writing @key[limited with] @Exam{Geometry.Lines;} we get access to all
the types visible in the specification of @Exam{Geometry.Lines} but
as if they were declared as incomplete. In other words we get
an incomplete view of the types. We can then do all the
things we can normally do with incomplete types such as use
them to declare access types. (Of course the implementation
checks later that @Exam{Geometry.Lines} does actually have a type
@Exam{Line}.)

Not only is the absence of the need for a named type @Exam{Acc_Line}
a handy shorthand, it also prevents the proliferation of
named access types. If we did want to use a named type
@Exam{Acc_Line} in both packages then we would have to declare a
distinct type in each package. This is because from the point
of view of the package @Exam{Points}, the @Exam{Acc_Line} in @Exam{Lines} would
only be an incomplete type (remember each package only has a
limited view of the other) and thus would be essentially
unusable. The net result would be many named access types and
wretched type conversions all over the place.

@leading@keepnext@;There are also some related changes to the notation for
incomplete types. We can now write
@begin{Example}
@key{type} T @key{is tagged};
@end{Example}

and we are then guaranteed that the full declaration will
reveal @Exam{T} to be a tagged type. The advantage is that we also
know that, being tagged, objects of the type @Exam{T} will be passed
by reference. Consequently we can use the type @Exam{T} for
parameters before seeing its full declaration. In the example
of points and lines above, since @Exam{Line} is visibly tagged in
the package @Exam{Geometry.Lines} we will thus get an incomplete
tagged view of Lines.@Defn{tagged incomplete type}

@leading@keepnext@;The introduction of tagged incomplete types clarifies the
ability to write
@begin{Example}
@key[type] T_Ptr @key[is access all] T'Class;
@end{Example}

This was allowed in Ada 95 even though we had not declared T
as tagged at this point. Of course it implied that T would be
tagged. In Ada 2005 this is frowned upon since we should now
declare that T is tagged incomplete if we wish to declare a
class wide access type. For compatibility the old feature has
been retained but banished to
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-J-11.html],Text=[Annex J]}
for obsolescent features.

Further examples of the use of limited with clauses will be
given in a later paper (see @RefSecNum{Mutually dependent types}).

Another enhancement in this area is the introduction of
private with clauses which overcome a problem with private
child packages.

@leading@;Private child packages were introduced to enable the details
of the implementation of part of a system to be decomposed
and yet not be visible to the external world. However, it is
often convenient to have public packages that use these
details but do not make them visible to the user. In Ada 95 a
parent or sibling body can have a with clause for a private
child. But the specifications cannot. These rules are
designed to ensure that information does not leak out via the
visible part of a specification. But there is no logical
reason why the private part of a package should not have
access to a private child. Ada 2005 overcomes this by
introducing private with clauses. We can write
@begin{Example}
@key[private package] App.Secret_Details @key[is]
   @key[type] Inner @key[is] ...
   ...  -- @ExamCom{various operations on Inner etc}
@key[end] App.Secret_Details;

@key[private with] App.Secret_Details;
@key[package] App.User_View @key[is]

   @key[type] Outer @key[is private];
   ...  -- @ExamCom{various operations on Outer visible to the user}

   -- @ExamCom{type Inner is not visible here}
@key{private}
   -- @ExamCom{type Inner is visible here}

   @key[type] Outer @key[is]
      @key[record]
         X: Secret_Details.Inner;
         ...
      @key[end record];
   ...
@key[end] App.User_View;
@end{Example}

@Leading@;thus the private part of the public child has access to the
type @Exam{Inner} but it is still hidden from the external user.
Note that the public child and private child might have
mutually declared types as well in which case they might also
wish to use the limited with facility. In this case the
public child would have a limited private with clause for the
private child written thus
@begin{Example}
@key[limited private with] App.Secret_Details;
@key[package] App.User_View @key[is] ...
@end{Example}

@Leading@;In the case of a parent package, its specification cannot
have a with clause for a child @en logically the specification
cannot know about the child because the parent must be
declared (that is put into the program library) first.
Similarly a parent cannot have a private with clause for a
private child. But it can have a limited with clause for any
child (thereby breaking the circularity) and in particular it
can have a limited private with clause for a private child.
So we might also have
@begin{Example}
@key[limited private with] App.Secret_Details;
@key[package] App @key[is] ...
@end{Example}

The final topic in this section is limited types. The reader
will recall that the general idea of a limited type is to
restrict the operations that the user can perform on a type
to just those provided by the developer of the type and in
particular to prevent the user from doing assignment and thus
making copies of an object of the type.

However, limited types have never quite come up to
expectation both in Ada 83 and Ada 95. Ada 95 brought
significant improvements by disentangling the concept of a
limited type from a private type but problems have remained.

The key problem is that Ada 95 does not allow the
initialization of limited types because of the view that
initialization requires assignment and thus copying. A
consequence is that we cannot declare constants of a limited
type either. Ada 2005 overcomes this problem by allowing
initialization by aggregates.

@leading@keepnext@;As a simple example, consider
@begin{Example}
@key[type] T @key[is limited]
   @key[record]
      A: Integer;
      B: Boolean;
      C: Float;
   @key[end record];
@end{Example}

@leading@;in which the type as a whole is limited but the components are not.
If we declare an object of type @exam[T] in Ada 95 then we have to
initialize the components (by assigning to them) individually thus
@begin[Example]
   X: T;
@key[begin]
   X.A := 10;  X.B := True;  X.C := 45.7;
@end[Example]

Not only is this annoying but it is prone to errors as well. If we
add a further component @exam[D] to the record type @exam[T] then
we might forget to initialize it. One of the advantages of aggregates
is that we have to supply all the components (allowing automatic so-called
full coverage analysis, a key benefit of Ada).

@leading@keepnext@;Ada 2005 allows the initialization with aggregates thus

@begin[Example]
   X: T := (A => 10,  B => True,  C => 45.7);
@end[Example]

Technically, Ada 2005 just recognizes properly that initialization
is not assignment. Thus we should think of the individual components
as being initialized individually @i[in situ] @en an actual aggregated
value is not created and then assigned. (Much the same happens when
initializing controlled types with an aggregate.)@Defn2{Term=[aggregate],Sec=[for limited types]}

@leading@;Sometimes a limited type has components where an initial value cannot
be given. This happens with task and protected types. For example
@begin[Example]
@key[protected type] Semaphore @key[is] ... ;

@key[type] PT @key[is]
   @key[record]
      Guard: Semaphore;
      Count: Integer;
      Finished: Boolean := False;
   @key[end record];
@end[Example]

@leading@;Remember that a protected type is inherently limited. This means
that the type @exam[PT] is limited because a type with a limited component
is itself limited. It is good practice to explicitly put @key[limited]
on the type @exam[PT] in such cases but it has been omitted here for
illustration. Now we cannot give an explicit initial value for a
@exam[Semaphore]  but we would still like to use an aggregate to
get the coverage check.
In such cases we can use the box symbol @exam[<>] to mean use the
default value for the type (if any). So we can write
@begin[Example]
X: PT := (Guard => <>, Count => 0, Finished => <>);
@end[Example]

Note that the ability to use @exam[<>] in an aggregate for a default
value is not restricted to the initialization of limited types. It
is a new feature applicable to aggregates in general. But, in order
to avoid confusion, it is only permitted with named notation.

Limited aggregates are also allowed in other similar contexts where
copying is not involved including as actual parameters of mode @key[in].

There are also problems with returning results of a limited type from
a function. This is overcome in Ada 2005 by the introduction of an
extended form of return statement. This will be described in detail
in a later paper (see @RefSecNum{Limited types and return statements}).

@LabeledSubclause{Overview: Tasking and real-time facilities}

Unless mentioned otherwise all the changes in this section concern
the Real-Time Systems annex.

@leading@;First, the well-established Ravenscar profile is included in
Ada 2005  as directed by WG9. A profile is a mode of operation and is
specified  by the pragma @exam[Profile] which defines the particular profile
to be used. Thus to ensure that a program conforms to the Ravenscar
profile we write@Defn{Ravenscar profile}
@begin[Example]
@key[pragma] Profile(Ravenscar);
@end[Example]

@leading@;The purpose of Ravenscar is to restrict the use of many of the
tasking  facilities so that the effect of the program is predictable. This
is very important for real-time safety-critical systems. In the case
of Ravenscar the pragma is equivalent to the joint effect of the following
pragmas

@begin[Example]
@key[pragma] Task_Dispatching_Policy(FIFO_Within_Priorities);
@key[pragma] Locking_Policy(Ceiling_Locking);
@key[pragma] Detect_Blocking;
@end[Example]

plus a @key[pragma] @exam[Restrictions] with a host of arguments such
as @exam[No_Abort_Statements] and @exam[No_Dynamic_Priorities].

The pragma @exam[Detect_Blocking] plus many of the @exam[Restrictions]
identifiers are new to Ada 2005. Further details will be given in
a later paper (see @RefSecNum{The Ravenscar profile}).

@leading@;Ada 95 allows the priority of a task to be changed but does not
permit the ceiling priority of a protected object to be changed. This is
rectified in Ada 2005 by the introduction of an attribute @exam[Priority]
for protected objects and the ability to change it by a simple assignment
such as@Defn{priority attribute}@Defn2{Term=[attribute],Sec=[Priority]}

@begin[Example]
My_PO'Priority := P;
@end[Example]

inside a protected operation of the object @exam[My_PO]. The change
takes effect at the end of the protected operation.

@leading@;The monitoring and control of execution time naturally are
important for real-time programs.@Defn{execution time} Ada 2005 includes
packages for three different aspects of this

@begin[Description]
@exam[Ada.Execution_Time] @en@\This is the root
package and enables the monitoring of execution time of individual
tasks.

@exam[Ada.Execution_Time.Timers] @en@\This provides
facilities for defining and enabling timers and for establishing a
handler which is called by the run time system when the execution
time of the task reaches a given value.

@exam[Ada.Execution_Time.Group_Budgets] @en@\This
allows several tasks to share a budget and provides means whereby
action can be taken when the budget expires.
@end[Description]

@leading@;The execution time of a task or CPU time, as it is commonly called,
is the time spent by the system executing the task and services on
its behalf. CPU times are represented by the private type @exam[CPU_Time].
The CPU time of a particular task is obtained by calling the following
function @exam[Clock] in the package @exam[Ada.Execution_Time]

@begin[Example]
@key[function] Clock(T: Task_Id := Current_Task) @key[return] CPU_Time;
@end[Example]

A value of type @exam[CPU_Time] can be converted to a @exam[Seconds_Count]
plus residual @exam[Time_Span] by a procedure @exam[Split] similar
to that in the package @exam[Ada.Real_Time]. Incidentally we are guaranteed
that the granularity of CPU times is no greater than one millisecond
and that the range is at least 50 years.

@leading@;In order to find out when a task reaches a particular CPU time we
use the facilities of the child package @exam[Ada.Execution_Time.Timers].
This includes a discriminated type @exam[Timer] and a type @exam[Handler]
thus

@begin[Example]
@key[type] Timer(T: @key[not null access] @key[constant] Task_Id) @key[is tagged limited private];
@key[type] Timer_Handler @key[is access protected procedure ](TM: @key[in out] Timer);
@end[Example]
Note how the access discriminant illustrates the use of both @key[not]
@key[null] and @key[constant].

@leading@keepnext@;We can then set the timer to expire at some absolute time by

@begin[Example]
Set_Handler(My_Timer, Time_Limit, My_Handler'Access);
@end[Example]

and then when the CPU time of the task reaches @exam[Time_Limit] (of
type @exam[CPU_Time]), the protected procedure @exam[My_Handler] is
executed. Note how the timer object incorporates the information regarding
the task concerned using an access discriminant and that this is passed
to the handler via its parameter. Another version of @exam[Set_Handler]
enables the timer to be triggered after a given interval (of type
@exam[Time_Span]).

@leading@;In order to program various aperiodic servers it is necessary for
tasks to share a CPU budget.@Defn{CPU budget} This can be done using the
child package @exam[Ada.Execution_Time.Group_Budgets]. In this case we have

@begin[Example]
@key[type] Group Budget @key[is tagged limited private];
@key[type] Group_Budget_Handler @key[is access protected procedure ](GB: @key[in out] Group_Budget);
@end[Example]

The type @exam[Group_Budget] both identifies the group of tasks it
belongs to and the size of the budget. Various subprograms enable
tasks to be added to and removed from a group budget. Other procedures
enable the budget to be set and replenished.

@leading@;A procedure @exam[Set_Handler] associates a particular handler with
a budget.

@begin[Example]
Set_Handler(GB => My_Group_Budget, Handler => My_Handler'Access);
@end[Example]

When the group budget expires the associated protected procedure is
executed.

@leading@;A somewhat related topic is that of low level timing events. The
facilities are provided by the package
@exam[Ada.Real_Time.Timing_Events].@Defn{timing event} In this case we have

@begin[Example]
@key[type] Timing_Event @key[is tagged limited private];
@key[type] Timing_Event_Handler @key[is access protected procedure](Event: @key[in out ]Timing_Event);
@end[Example]

@leading@;The idea here is that a protected procedure can be nominated to be
executed at some time in the future. Thus to ring a pinger when our
egg is boiled after four minutes we might have a protected procedure

@begin[Example]
@key[protected] @key[body] Egg @key[is]
   @key[procedure] Is_Done(Event: @key[in out] Timing_Event) @key[is]
   @key[begin]
      Ring_The_Pinger;
   @key[end] Is_Done;
@key[end] Egg;
@end[Example]

@leading@keepnext@;and then

@begin[Example]
Egg_Done: Timing_Event;
Four_Min: Time_Span := Minutes(4);
...
Put_Egg_In_Water;
Set_Handler(Event => Egg_Done, In_Time => Four_Min, Handler => Egg.Is_Done'Access);
-- @examcom[ now read newspaper whilst waiting for egg]
@end[Example]

This facility is of course very low level and does not involve Ada
tasks at all. Note that we can set the event to occur at some absolute
time as well as at a relative time as above. Incidentally, the function
@exam[Minutes] is a new function added to the parent package
@exam[Ada.Real_Time]. Otherwise we would have had to write something revolting
such as @exam[4*60*Milliseconds(1000)]. A similar function @exam[Seconds] has
also been added.

There is a minor flaw in the above example. If we are interrupted
by the telephone between putting the egg in the water and setting
the handler then our egg will be overdone. We will see how to cure
this in a later paper (see @RefSecNum{CPU clocks and timers}).

Readers will recall the old problem of how tasks can have a silent
death. If something in a task goes wrong in Ada 95 and an exception
is raised which is not handled by the task, then it is propagated
into thin air and just vanishes. It was always deemed impossible for
the exception to be handled by the enclosing unit because of the inherent
asynchronous nature of the event.

@leading@;This is overcome in Ada 2005 by the package
@exam[Ada.Task_Termination] which provides facilities for associating a
protected procedure with a task. The protected procedure is invoked when the
task terminates with an indication of the reason. Thus we might declare a
protected object @exam[Grim_Reaper]@Defn{termination handler}

@begin[Example]
@key[protected] Grim_Reaper @key[is]
   @key[procedure] Last_Gasp(C: Cause_Of_Termination; T: Task_Id; X: Exception_Occurrence);
@key[end] Grim_Reaper;
@end[Example]

@leading@;We can then nominate @exam[Last_Gasp] as the protected procedure to
be called when task @exam[T] dies by

@begin[Example]
Set_Specific_Handler(T'Identity, Grim_Reaper.Last_Gasp'Access);
@end[Example]

@leading@;The body of the protected procedure @exam[Last_Gasp] might then
output various diagnostic messages

@begin[Example]
@key[procedure] Last_Gasp(C: Cause_Of_Termination; T: Task_Id; X: Exception_Occurrence) @key[is]
@key[begin]
   @key[case] C @key[is]
      @key[when] Normal => @key[null];
      @key[when] Abnormal =>
         Put("Something nasty happened"); ...
      @key[when] Unhandled_Exception =>
         Put("Unhandled exception occurred"); ...
   @key[end case];
@key[end] Last_Gasp;
@end[Example]

There are three possible reasons for termination, it could be normal,
abnormal, or caused by an unhandled exception. In the last case the
parameter @exam[X] gives details of the exception occurrence.

Another area of increased flexibility in Ada 2005 is that of task
dispatching policies. In Ada 95, the only predefined policy is @exam[FIFO_Within_Priorities]
although other policies are permitted. Ada 2005 provides further pragmas,
policies and packages which facilitate many different mechanisms such
as non-preemption within priorities, the familiar Round Robin using
timeslicing, and the more recently acclaimed Earliest Deadline First
(EDF) policy. Moreover, it is possible to mix different policies according
to priority level within a partition.

@leading@;Various facilities are provided by the package @exam[Ada.Dispatching]
plus two child packages

@begin[Description]
@exam[Ada.Dispatching] @en@\This is the root package
and simply declares an exception @exam[Dispatching_Policy_Error].

@exam[Ada.Dispatching.Round_Robin] @en@\This enables
the setting of the time quanta for time slicing within one or more
priority levels.

@exam[Ada.Dispatching.EDF] @en@\This enables the
setting of the deadlines for various tasks.
@end[Description]

@leading@keepnext@;A policy can be selected for a whole partition by one of
@begin[Example]
@key[pragma] Task_Dispatching_Policy(Non_Preemptive_FIFO_Within_Priorities);

@key[pragma] Task_Dispatching_Policy(Round_Robin_Within_Priorities);

@key[pragma] Task_Dispatching_Policy(EDF_Across_Priorities);
@end[Example]

@leading@;In order to mix different policies across different priority levels
we use the pragma @exam[Priority_Specific_Dispatching] with various
policy identifiers thus
@begin[Example]
@key[pragma] Priority_Specific_Dispatching(Round_Robin_Within_Priorities, 1, 1);
@key[pragma] Priority_Specific_Dispatching(EDF_Across_Priorities, 2, 10);
@key[pragma] Priority_Specific_Dispatching(FIFO_Within_Priorities, 11, 24);
@end[Example]

This sets Round Robin at priority level 1, EDF at levels 2 to 10,
and FIFO at levels 11 to 24.

The final topic in this section concerns the core language and not
the Real-Time Systems annex. Ada 2005 introduces a means whereby object
oriented and real-time features can be closely linked together through
inheritance.

@leading@;Recall from Section 3.1 that we can declare an interface to be
limited thus
@begin[Example]
@key[type] LI @key[is limited interface];
@end[Example]

@leading@;We can also declare an interface to be synchronized, task, or
protected thus
@begin[Example]
@key[type] SI @key[is synchronized interface];
@key[type] TI @key[is task interface];
@key[type] PI @key[is protected interface];
@end[Example]

A task interface or protected interface has to be implemented by a
task type or protected type respectively. However, a synchronized
interface can be implemented by either a task type or a protected
type. These interfaces can also be composed with certain restrictions.
Detailed examples will be given in a later paper
(see @RefSecNum{Synchronized interfaces}).


@LabeledSubclause{Overview: Exceptions, numerics, generics etc}

As well as the major features discussed above there are also a number
of improvements in various other areas.

@leading@;There are two small changes concerning exceptions. One is that we
can give a message with a raise statement, thus@Defn{raise with message}
@begin[Example]
@key[raise] Some_Error @key[with] "A message";
@end[Example]

@leading@keepnext@;This is a lot neater than having to write (as in Ada 95)
@begin[Example]
Ada.Exceptions.Raise_Exception(Some_Error'Identity, "A message");
@end[Example]

@leading@;The other change concerns the detection of a null exception
occurrence which might be useful in a package analysing a log of exceptions.
The problem is that exception occurrences are of a limited private type and so
we cannot compare an occurrence with @exam[Null_Occurrence] to see if they are
equal. In Ada 95 applying the function @exam[Exception_Identity] to a null
occurrence unhelpfully raises @exam[Constraint_Error]. This has been changed in
Ada 2005 to return @exam[Null_Id] so that we can now write
@begin[Example]
@key[procedure] Process_Ex(X: Exception_Occurrence) @key[is]
@key[begin]
   @key[if] Exception_Identity(X) = Null_Id @key[then]
      -- @examcom[process the case of a Null_Occurrence]
   ...
@key[end] Process_Ex;
@end[Example]

@leading@;Ada 95 introduced modular types which are of course unsigned integers.
However it has in certain cases proved very difficult to get unsigned
integers and signed integers to work together. This is a trivial matter
in fragile languages such as C but in Ada the type model has proved
obstructive. The basic problem is converting a value of a signed type
which happens to be negative to an unsigned type. Thus suppose we
want to add a signed offset to an unsigned address value, we might have
@begin[Example]
@key[type] Offset_Type @key[is range] @en@;(2**31) .. 2**31@en@;1;
@key[type] Address_Type @key[is mod] 2**32;

Offset: Offset_Type;
Address: Address_Type;
@end[Example]

@leading@;We cannot just add @exam[Offset] to @exam[Address] because they are
of different types. If we convert the @exam[Offset] to the address
type then we might get @exam[Constraint_Error] and so on. The solution
in Ada 2005 is to use a new functional attribute @exam[S'Mod] which
applies to any modular subtype @exam[S] and converts a universal integer
value to the modular type using the corresponding mathematical mod
operation. So we can now write@Defn{mod attribute}@Defn2{Term=[attribute],Sec=[Mod]}
@begin[Example]
Address := Address + Address_Type'Mod(Offset);
@end[Example]

Another new attribute is @exam[Machine_Rounding]. This enables high-performance
conversions from floating point types to integer types when the exact
rounding does not matter.@Defn{machine_rounding attribute}@Defn2{Term=[attribute],Sec=[Machine_Rounding]}

The third numeric change concerns fixed point types. It was common
practice for some Ada 83 programs to define their own multiply and
divide operations, perhaps to obtain saturation arithmetic. These
programs ran afoul of the Ada 95 rules that introduced universal fixed
operations and resulted in ambiguities. Without going into details,
this problem has been fixed in Ada 2005 so that user-defined operations
can now be used.

@leading@;Ada 2005 has several new pragmas. The first is
@begin[Example]
@key[pragma] Unsuppress(Identifier);
@end[Example]
@leading@;where the identifier is that of a check such as
@exam[Range_Check].@Defn{unsuppress pragma}@Defn2{Term=[pragma],Sec=[Unsuppress]}
The general idea is to ensure that checks are performed in a declarative
region irrespective of the use of a corresponding pragma @exam[Suppress].
Thus we might have a type @exam[My_Int] that behaves as a saturated
type. Writing
@begin[Example]
@key[function] "*" (Left, Right: My_Int) @key[return] My_Int @key[is]
   @key[pragma] Unsuppress(Overflow_Check);
@key[begin]
   @key[return] Integer(Left) * Integer(Right);
@key[exception]
   @key[when] Constraint_Error =>
      @key[if] (Left>0 @key[and] Right>0) @key[or] (Left<0 @key[and] Right<0) @key[then]
         @key[return] My_Int'Last;
      @key[else]
         @key[return] My_Int'First;
      @key[end if];
@key[end] "*";
@end[Example]

ensures that the code always works as intended even if checks are
suppressed in the program as a whole. Incidentally the @exam[On] parameter
of pragma @exam[Suppress] which never worked well has been banished
to Annex J.

@leading@;Many implementations of Ada 95 support a pragma @exam[Assert] and
this is now consolidated into Ada 2005. The general idea is that we
can write pragmas such as@Defn{assert pragma}@Defn2{Term=[pragma],Sec=[Assert]}
@begin[Example]
@key[pragma] Assert(X >50);

@key[pragma] Assert(@key[not] Buffer_Full, "buffer is full");
@end[Example]

@leading@;The first parameter is a Boolean expression and the second (and
optional) parameter is a string. If at the point of the pragma at execution
time, the expression is @exam[False] then action can be taken. The action is
controlled by another pragma @exam[Assertion_Policy] which can switch the
assertion mechanism on and off by one of@Defn{assertion policy}@Defn2{Term=[pragma],Sec=[Assertion_Policy]}
@begin[Example]
@key[pragma] Assertion_Policy(Check);

@key[pragma] Assertion_Policy(Ignore);
@end[Example]

If the policy is to check then the exception @exam[Assertion_Error]
is raised with the message, if any. This exception is declared in
the predefined package @exam[Ada.Assertions]. There are some other
facilities as well.

@leading@;The pragma @exam[No_Return] also concerns exceptions. It can be applied
to a procedure (not to a function) and indicates that the procedure
never returns normally but only by propagating an exception (it might
also loop for ever). Thus@Defn{non-returning procedures}@Defn2{Term=[pragma],Sec=[No_Return]}
@begin[Example]
@key[procedure] Fatal_Error(Message: @key[in] String);
@key[pragma] No_Return(Fatal_Error);
@end[Example]

And now whenever we call @exam[Fatal_Error] the compiler is assured
that control is not returned and this might enable some optimization
or better diagnostic messages.

Note that this pragma applies to the predefined procedure
@exam[Ada.Exceptions.Raise_Exception].

Another new pragma is @exam[Preelaborable_Initialization]. This is
used with private types and indicates that the full type will have
preelaborable initialization. A number of examples occur with the
predefined packages such as@Defn{preelaborable initialization}@Defn2{Term=[pragma],Sec=[Preelaborable_Initialization]}
@begin[Example]
@key[pragma] Preelaborable_Initialization(Controlled);
@end[Example]
in @exam[Ada.Finalization].

Finally, there is the pragma @exam[Unchecked_Union].@Defn{union}@Defn{C union}@Defn2{Term=[pragma],Sec=[Unchecked_Union]}
This is useful
for interfacing to programs written in C that use the concept of unions.
Unions in C correspond to variant types in Ada but do not store any
discriminant which is entirely in the mind of the C programmer. The
pragma enables a C union to be mapped to an Ada variant record type
by omitting the storage for the discriminant.

@leading@keepnext@;If the C program has
@begin[Example]
union {
   double spvalue;
   struct {
      int length;
      double* first;
      } mpvalue;
} number;
@end[Example]

@leading@keepnext@;then this can be mapped in the Ada program by
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

One problem with pragmas (and attributes) is that many implementations
have added implementation defined ones (as they are indeed permitted
to do). However, this can impede portability from one implementation
to another. To overcome this there are further @exam[Restrictions]
identifiers so we can write@Defn{restrictions identifier}
@begin[Example]
@key[pragma] Restrictions(No_Implementation_Pragmas, No_Implementation_Attributes);
@end[Example]

Observe that one of the goals of Ada 2005 has been to standardize
as many of the implementation defined attributes and pragmas as possible.

Readers might care to consider the paradox that GNAT has an
(implementation-defined) restrictions identifier
@exam[No_Implementation_Restrictions].

@leading@;Another new restrictions identifier prevents us from inadvertently
using features in Annex J thus
@begin[Example]
@key[pragma] Restrictions(No_Obsolescent_Features);
@end[Example]

@leading@;Similarly we can use the restrictions identifier @exam[No_Dependence]
to state that a program does not depend on a given library unit. Thus
we might write
@begin[Example]
@key[pragma] Restrictions(No_Dependence => Ada.Command_Line);
@end[Example]

Note that the unit mentioned might be a predefined library unit as
in the above example but it can also be used with any library unit.

The final new general feature concerns formal generic package parameters.
Ada 95 introduced the ability to have formal packages as parameters
of generic units. This greatly reduced the need for long generic parameter
lists since the formal package encapsulated them.

@leading@;Sometimes it is necessary for a generic unit to have two (or more)
formal packages. When this happens it is often the case that some
of the actual parameters of one formal package must be identical to
those of the other. In order to permit this there are two forms of
generic parameters. One possibility is

@begin[Example]
@key[generic]
   @key[with package] P @key[is new] Q(<>);
@key[package] Gen @key[is] ...
@end[Example]

@leading@;and then the package @exam[Gen] can be instantiated with any package
that is an instantiation of @exam[Q]. On the other hand we can have
@begin[Example]
@key[generic]
   @key[with package] R @key[is new] S(P1, P2, ... );
@key[package] Gen @key[is] ...
@end[Example]

and then the package @exam[Gen] can only be instantiated with a package
that is an instantiation of @exam[S] with the given actual parameters
@exam[P1], @exam[P2] etc.

@leading@keepnext@;These mechanisms are often used together as in
@begin[Example]
@key[generic]
   @key[with package] P @key[is new] Q(<>);
   @key[with package] R @key[is new] S(P.F1);
@key[package] Gen @key[is] ...
@end[Example]

This ensures that the instantiation of @exam[S] has the same actual
parameter (assumed only one in this example) as the parameter @exam[F1]
of @exam[Q] used in the instantiation of @exam[Q] to create the actual
package corresponding to @exam[P].

@leading@;There is an example of this in one of the packages for vectors and
matrices in ISO/IEC 13813 which is now incorporated into Ada 2005
(see Section @RefSecNum{Overview: Standard library}). The generic package for
complex arrays has two
package parameters. One is the corresponding package for real arrays
and the other is the package @exam[Generic_Complex_Types] from the
existing Numerics annex. Both of these packages have a floating type
as their single formal parameter and it is important that both instantiations
use the same floating type (eg both @exam[Float] and not one @exam[Float]
and one @exam[Long_Float]) otherwise a terrible mess will occur. This
is assured by writing (using some abbreviations)
@begin[Example]
@key[with] ... ;
@key[generic]
   @key[with package] Real_Arrays @key[is] @key[new] Generic_Real_Arrays(<>);
   @key[with package] Complex_Types @key[is new] Generic_Complex_Types(Real_Arrays.Real);
@key[package] Generic_Complex_Arrays @key[is] ...
@end[Example]

Well this works fine in simple cases (the reader may wonder whether
this example is simple anyway) but in more elaborate situations it
is a pain. The trouble is that we have to give all the parameters
for the formal package or none at all in Ada 95.

@leading@;Ada 2005 permits only some of the parameters to be specified, and
any not specified can be indicated using the box. So we can write any of
@begin[Example]
@key[with package] Q @key[is new] R(P1, P2, F3 => <>);
@key[with package] Q @key[is new] R(P1, @key[others] => <>);
@key[with package] Q @key[is new] R(F1 => <>, F2 => P2, F3 => P3);
@end[Example]

Note that the existing form @exam[(<>)] is now deemed to be a shorthand
for@exam[ (]@key[others]@exam[ => <>)]. As with aggregates, the form
@exam[<>] is only permitted with named notation.

Examples using this new facility will be given in a later paper
(see @RefSecNum{Generic units}).


@LabeledSubClause{Overview: Standard library}

There are significant improvements to the standard library in Ada
2005. One of the strengths of Java is the huge library that comes
with it. Ada has tended to take the esoteric view that it is a language
for constructing programs from components and has in the past rather
assumed that the components would spring up by magic from the user
community. There has also perhaps been a reluctance to specify standard
components in case that preempted the development of better ones.
However, it is now recognized that standardizing useful stuff is a
good thing. And moreover, secondary ISO standards are not very helpful
because they are almost invisible. Ada 95 added quite a lot to the
predefined library and Ada 2005 adds more.

First, there are packages for manipulating vectors and matrices already
mentioned in Section @RefSecNum{Overview: Exceptions, numerics, generics etc}
when discussing formal package parameters.
There are two packages, @exam[Ada.Numerics.Generic_Real_Arrays] for
real vectors and matrices and @exam[Ada.Numerics.Generic_Complex_Arrays]
for complex vectors and matrices. They can be instantiated according
to the underlying floating point type used. There are also nongeneric
versions as usual.@Defn{vector}@Defn{matrix}

@leading@;These packages export types for declaring vectors and matrices and
many operations for manipulating them. Thus if we have an expression
in mathematical notation such as
@begin[Example]
@b{@i{y}} = @b{@i{Ax}} + @b{@i{z}}
@end[Example]

where @b{@i{x}}, @b{@i{y}} and @b{@i{z}}
are vectors and @b{@i{A}} is a square matrix, then
this calculation can be simply programmed as

@begin[Example]
X, Y, Z: Real_Vector(1 .. N);
A: Real_Matrix(1 .. N, 1 .. N);
...
Y := A * X + Z;
@end[Example]

and the appropriate operations will
be invoked. The packages also include subprograms for the most useful
linear algebra computations, namely, the solution of linear equations,
matrix inversion and determinant evaluation, plus the determination
of eigenvalues and eigenvectors for symmetric matrices (Hermitian
in the complex case). Thus to determine @exam[X]
given @exam[Y], @exam[Z] and @exam[A] in the above example we can write
@begin[Example]
X := Solve(A, Y @en Z);
@end[Example]

It should not be thought that these
Ada packages in any way compete with the very comprehensive BLAS (Basic
Linear Algebra Subprograms). The purpose of the Ada packages is to
provide simple implementations of very commonly used algorithms (perhaps
for small embedded systems or for prototyping) and to provide a solid
framework for developing bindings to the BLAS for more demanding situations.
Incidentally, they are in the Numerics annex.

Another (but very trivial) change to the Numerics annex is that nongeneric
versions of @exam[Ada.Text_IO.Complex_IO]
have been added in line with the standard principle of providing
nongeneric versions of generic predefined packages for convenience.
Their omission from Ada 95 was an oversight.

@leading@;There is a new predefined package in Annex A for accessing
tree-structured file systems. The scope is perhaps indicated by this fragment
of its specification@Defn{directory operations}

@begin[Example]
@key[with] ...
@key[package] Ada.Directories @key[is]

   -- @examcom[Directory and file operations]

   @key[function] Current_Directory @key[return] String;
   @key[procedure] Set_Directory(Directory: @key[in] String);
   ...
   -- @examcom[File and directory name operations]

   @key[function] Full_Name(Name: @key[in] String) @key[return] String;
   @key[function] Simple_Name(Name: @key[in] String) @key[return] String;
   ...
   -- @examcom[File and directory queries]

   @key[type] File_Kind @key[is] (Directory, Ordinary_File, Special_File);
   @key[type] File_Size @key[is range] 0 .. @examcom[implementation-defined];
   @key[function] Exists(Name: @key[in] String) @key[return] Boolean;
   ...
   -- @examcom[Directory searching]

   @key[type] Directory_Entry_Type @key[is limited private];
   @key[type] Filter_Type @key[is array] (File_Kind) @key[of] Boolean;
   ...
   -- @examcom[Operations on directory entries]

   ...
@key[end] Ada.Directories;
@end[Example]

The package contains facilities which
will be useful on any Unix or Windows system. However, it has to be
recognized that like @exam[Ada.Command_Line]
it might not be supportable on every environment.

There is also a package @exam[Ada.Environment_Variables]
for accessing the environment variables that occur in most operating
systems.@Defn{environment variables}

A number of additional subprograms
have been added to the existing string handling packages. There are
several problems with the Ada 95 packages. One is that conversion
between bounded and unbounded strings and the raw type @exam[String]
is required rather a lot and is both ugly and inefficient. For example,
searching only part of a bounded or unbounded string can only be done
by converting it to a @exam[String]
and then searching the appropriate slice (or by making a truncated
copy first).

In brief the additional subprograms are as follows

@begin[Itemize]
Three further versions of function @exam[Index] with an additional parameter
@exam[From] indicating the start of the search are added to each of
@exam[Strings.Fixed], @exam[Strings.Bounded] and @exam[Strings.Unbounded].

A further version of function @exam[Index_Non_Blank] is similarly added to all
three packages.

A procedure @exam[Set_Bounded_String] with similar behaviour to the function
@exam[To_Bounded_String] is added to @exam[Strings.Bounded]. This avoids the
overhead of using a function. A similar procedure @exam[Set_Unbounded_String]
is added to @exam[Strings.Unbounded].

A function and procedure @exam[Bounded_Slice] are added to
@exam[Strings.Bounded]. These avoid conversions from type @exam[String]. A
similar function and procedure @exam[Unbounded_Slice]are added to
@exam[Strings.Unbounded].

@end[Itemize]

As well as these additions there
is a new package @exam[Ada.Text_IO.Unbounded_IO]
for the input and output of unbounded strings. This again avoids
unnecessary conversion to the type @exam[String]. Similarly, there is a
generic package @exam[Ada.Text_IO.Bounded_IO]; this is generic because
the package @exam[Strings.Bounded]
has an inner generic package which is parameterized by the maximum
string length.

Finally, two functions @exam[Get_Line] are added to @exam[Ada.Text_IO]
itself. These avoid difficulties with the length of the string which
occurs with the existing procedures @exam[Get_Line].

In Ada 83, program identifiers used
the 7-bit ASCII set. In Ada 95 this was extended to the 8-bit Latin-1
set. In Ada 2005 this is extended yet again to the entire ISO/IEC
10646:2003 character repertoire. This means that identifiers can now
use Cyrillic and Greek characters. Thus we could extend the animal
example by@Defn{ISO 10646 characters}

@begin[Example]@comment{Cyrillic codes in hex: 421h, 442h, 430h, 43bh, 438h, 43dh}
@unicode(1057)@unicode(1090)@unicode(1072)@unicode(1083)@unicode(1080)@unicode(1085) : @key[access] Pig @key[renames] Napoleon;
@unicode(928)@unicode(949)@unicode(947)@unicode(945)@unicode(963)@unicode(965)@unicode(962) : Horse;
@end[Example]

@leading@;In order to encourage us to write
our mathematical programs nicely the additional constant
@begin[Example]
@pi : @key[constant] := Pi;
@end[Example]

has been added to the package @exam[Ada.Numerics] in Ada 2005.

In a similar way types @exam[Wide_String] and @exam[Wide_Character] were
added to Ada 95. In Ada 2005 this process is also extended and a set
of wide-wide types and packages for 32-bit characters are added. Thus
we have types @exam[Wide_Wide_Character] and @exam[Wide_Wide_String]
and so on.

A major addition to the predefined
library is the package @exam[Ada.Containers]
and its children plus some auxiliary child functions of @exam[Ada.Strings].
These are very important and considerable additions to the predefined
capability of Ada and bring the best in standard data structure manipulation
to the fingers of every Ada programmer. The scope is perhaps best
illustrated by listing the units involved.@Defn{containers}

@begin[Description]
@exam[Ada.Containers] @en@\This
is the root package and just declares types @exam[Hash_Type] and
@exam[Count_Type] which are an implementation-defined modular and integer type
respectively.

@exam[Ada.Strings.Hash] @en@\This function hashes a string into the type
@exam[Hash_Type]. There are also versions for bounded and unbounded strrings.

@exam[Ada.Containers.Vectors] @en@\This
is a generic package with parameters giving the index type and element
type of a vector plus @exam["="]
for the element type. This package declares types and operations
for manipulating vectors. (These are vectors in the sense of flexible
arrays and not the mathematical vectors used for linear algebra as
in the vectors and matrices packages mentioned earlier.) As well as
subprograms for adding, moving and removing elements there are also
generic subprograms for searching, sorting and iterating over vectors.

@exam[Ada.Containers.Doubly_Linked_Lists] @en@\This is a generic package with
parameters giving the element type and @exam["="] for the element type. This
package declares types and operations for manipulating doubly-linked lists. It
has similar functionality to the vectors package. Thus, as well as subprograms
for adding, moving and removing elements there are also generic subprograms for
searching, sorting and iterating over lists.

@exam[Ada.Containers.Hashed_Maps] @en@\This is a generic package with
parameters giving a key type and an element type plus a hash function for the
key, a function to test for equality between keys and @exam["="] for the
element type. It declares types and operations for manipulating hashed maps.

@exam[Ada.Containers.Ordered_Maps] @en@\This is a similar generic package for
ordered maps with parameters giving a key type and an element type and
@exam["<"] for the key type and @exam["="] for the element type.

@exam[Ada.Containers.Hashed_Sets] @en@\This is a generic package with
parameters giving the element type plus a hash function for the elements and a
function to test for equality between elements. It declares types and
operations for manipulating hashed sets.

@exam[Ada.Containers.Ordered_Sets] @en@\This is a similar generic package for
ordered sets with parameters giving the element type and @exam["<"] and
@exam["="] for the element type.
@end[Description]

There are then another six packages
with similar functionality but for indefinite types with corresponding
names such as @exam[Ada.Containers.Indefinite_Vectors].

@begin[Description]
@exam[Ada.Containers.Generic_Array_Sort] @en@\This is a generic procedure for
sorting arrays. The generic parameters give the index type, the element type,
the array type and @exam["<"] for the element type. The array type is
unconstrained.@Defn{array sorting}
@end[Description]

Finally there is a very similar generic procedure
@exam[Ada.Containers.Generic_Constrained_Array_Sort] but for constrained array
types.

It is hoped that the above list gives a flavour of the capability of the
package @exam[Containers]. Some examples of the use of the facilities will be
given in a later paper.@Comment{** Ref TBD - entire containers section}

Finally, there are further packages for manipulating times (that is
of type @exam[Ada.Calendar.Time] and not @exam[Ada.Real_Time.Time]
and thus more appropriate in a discussion of the predefined library
than the real-time features). The package @exam[Ada.Calendar]
has a number of obvious omissions and in order to rectify this the
following packages are added.@Defn{time operations}

@begin[Description]
@exam[Ada.Calendar.Time_Zones] @en@\This
declares a type @exam[Time_Offset]
describing in minutes the difference between two time zones and a
function @exam[UTC_Time_Offset]
which given a time returns the difference between the time zone of
@exam[Calendar] at that time and
UTC (Coordinated Universal Time which is close to Greenwich Mean Time).
It also has an exception which is raised if the time zone of @exam[Calendar]
is not known (maybe the clock is broken).

@exam[Ada.Calendar.Arithmetic] @en@\This
declares various types and operations for coping with leap seconds.

@exam[Ada.Calendar.Formatting] @en@\This
declares further types and operations for dealing with formatting
and related matters.

@end[Description]

@leading@;Most of the new calendar features are clearly only for the
chronological addict but the need for them does illustrate
that this is a tricky area. However, a feature that all will
appreciate is that the package @Exam{Ada.Calendar.Formatting}
includes the following declarations
@begin{Example}
@key[type] Day_Name @key[is] (Monday, Tuesday, Wednesday,
                  Thursday, Friday, Saturday, Sunday);
@key[function] Day_Of_Week(Date: Time) @key[return] Day_Name;
@end{Example}

@leading@;There is also a small change in the parent package
@Exam{Ada.Calendar} itself. The subtype @Exam{Year_Number} is now

@begin{Example}
@key[subtype] Year_Number @key[is] Integer @key[range] 1901 .. 2399;
@end{Example}

This reveals confidence in the future of Ada by adding
another three hundred years to the range of dates.

@LabeledClause{Conclusions}

This overview of Ada 2005 should have given the reader an
appreciation of the important new features in Ada 2005. Some
quite promising features failed to be included partly because
the need for them was not clear and also because a conclusive
design proved elusive. We might think of them as Forthcoming
Attractions for any further revision!

Some esoteric topics have been omitted in this overview; they
concern features such as: streams, object factory functions,
the partition control system in distributed systems,
partition elaboration policy for high integrity systems, a
subtlety regarding overload resolution, the title of Annex H,
quirks of access subtypes, rules for pragma @Exam{Pure}, and the
classification of various units as pure or preelaborable.

Further papers will expand on the six major topics of this
overview in more detail.

It is worth briefly reviewing the guidelines (see Section
@RefSecNum{Scope of revision}
above) to see whether Ada 2005 meets them. Certainly the
Ravenscar profile has been added and the problem of mutually
dependent types across packages has been solved.

The group A items were about real-time and high-integrity,
static error checking and interfacing. Clearly there are
major improvements in the real-time area. And high-integrity
and static error checking are addressed by features such as
the @key[overriding prefix], various pragmas such as @Exam{Unsuppress} and
@Exam{Assert} and additional @Exam{Restrictions} identifiers. Better
interfacing is provided by the pragma @Exam{Unchecked_Union} and the
@Exam{Mod} attribute.

The group B items were about improvements to the OO model,
the need for a Java-like interface feature and better
interfacing to other OO languages. Major improvements to the
OO model are brought by the prefixed (@Exam{Obj.Op}) notation and
more flexible access types. The Java-like interface feature
has been added and this provides better interfacing.

The final direct instruction was to incorporate the vectors
and matrices stuff and this has been done. There are also
many other improvements to the predefined library as we have
seen.

It seems clear from this brief check that indeed Ada 2005
does meet the objectives set for it.

Finally, I need to thank all those who have helped in the
preparation of this paper. First I must acknowledge the
financial support of Ada-Europe and the Ada Resource
Association. And then I must thank those who reviewed earlier
versions. There are almost too many to name, but I must give
special thanks to Randy Brukardt, Pascal Leroy and Tucker
Taft of the ARG, to my colleagues on the UK Ada Panel
(BSI/IST/5/-/9), and to James Moore of WG9. I am especially
grateful for a brilliant suggestion of Randy Brukardt which
must be preserved for the pleasure of future generations. He
suggests that this document when complete be called the Ada
Language Enhancement Guide. This means that if combined with
the final Ada Reference Manual, the whole document can then
be referred to as the ARM and ALEG. Thanks Randy.


