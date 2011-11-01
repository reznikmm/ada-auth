@Part(frontmatter, root="ada.mss")
@comment{$Source: e:\\cvsroot/ARM/Source/front_matter.mss,v $}
@comment{$Revision: 1.73 $ $Date: 2011/11/01 05:34:04 $}

@comment{@begin{Comment} (*Removed the below for Ada 2012, which is supposely going
to be processed as a revision*)
@ChgNote{Following is a foreword for the consolidated edition of the RM/AARM.}
@UnNumberedSection(Foreword to this version of the Ada Reference Manual)
@ChgNote{The above has to be manually commented out if creating "original" RMs.
We shouldn't need to do that again.}
@ChgNote{We needed boilerplate like this to avoid objections from various
outside parties. It seems unlikely that the same objections won't be raised
again.}
@begin{Intro}
@begin{NotISO}
@ChgRef{Version=[1],Kind=[Added]}
@ChgAdded{Version=[1],Text=[The International Standard for the
programming language Ada is ISO/IEC 8652:1995(E).]}

@ChgRef{Version=[1],Kind=[Added]}
@ChgAdded{Version=[1],Text=[The Ada Working Group ISO/IEC JTC 1/SC 22/WG 9 is tasked by ISO with
the work item to interpret and maintain the International Standard and to
produce Technical Corrigenda, as appropriate. The technical work on the
International Standard is performed by the Ada Rapporteur Group (ARG) of WG 9.
In September 2000, WG 9 approved and forwarded Technical Corrigendum 1 to
SC 22 for ISO approval, which was granted in February 2001. Technical
Corrigendum 1 was published in June 2001.]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgAdded{Version=[2],Text=[In October 2002, WG 9 approved a schedule and
guidelines for the preparation of an Amendment to the International Standard.
WG 9 approved the scope of the Amendment in June 2004. In April 2006, WG 9
approved and forwarded the Amendment to SC 22 for approval, which was granted
in August 2006. Final ISO/IEC approval @Chg{Version=[3],New=[came in January 2007,
and the Amendment was published as ISO/IEC 8652:1995/Amd 1:2007(E) in March],Old=[is expected
by early]} 2007.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[In November 2008, WG 9 requested
the preparation of a second Amendment to the International Standard.
The completion of Amendment 2 is expected at the end of 2010, with
final approval in 2011.]}

@ChgRef{Version=[1],Kind=[Added]}
@ChgAdded{Version=[1],Text=[The Technical Corrigendum lists the individual
changes that need to be made to the text of the International Standard to
correct errors, omissions or inconsistencies. The corrections specified in
Technical Corrigendum 1 are
part of the International Standard ISO/IEC 8652:1995(E).]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgAdded{Version=[2],Text=[Similarly, Amendment 1 @Chg{Version=[3],New=[and
Amendment 2 list],Old=[lists]} the individual changes that
need to be made to the text of the International Standard to add new features
as well as correct errors.]}

@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[2],Kind=[RevisedAdded]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@Chg{New=[When ISO published Technical Corrigendum 1, it did not also publish
a document that merges the @Chg{Version=[2],New=[changes from the ],Old=[]}Technical
Corrigendum @Chg{Version=[2],New=[],Old=[changes ]}into
the text of the International Standard.@Chg{Version=[2],New=[ @Chg{Version=[3],
New=[Similarly,],Old=[It is not
known whether]} ISO @Chg{Version=[3],New=[did not],Old=[will]} publish a
document that merges the changes from
Technical Corrigendum and Amendment 1 into the text of the International
Standard.],Old=[]}@Chg{Version=[3],New=[ It is not known whether ISO will
publish a document that merges the changes from Technical Corrigendum 1,
Amendment 1, and Amendment 2 into the text of the International
Standard.],Old=[]}
However, ISO rules require that the
project editor for the @Chg{Version=[2],New=[International Standard],
Old=[Technical Corrigendum]} be able to produce such a document
on demand.],Old=[]}

@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[2],Kind=[RevisedAdded]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@Chg{New=[This version of the Ada Reference Manual is what the project editor would
provide to ISO in response to such a request. It incorporates the changes
specified in the Technical Corrigendum @Chg{Version=[2],New=[and
@Chg{Version=[3],New=[Amendments 1 and 2],Old=[Amendment]} ],Old=[]}into
the text of ISO/IEC 8652:1995(E).
It should be understood that the publication of any ISO document involves
changes in general format, boilerplate, headers, etc., as well as a review by professional
editors that may introduce editorial changes to the text. This version of the
Ada Reference Manual is therefore neither an official ISO document, nor a
version guaranteed to be identical to an official ISO document, should ISO
decide to reprint the International Standard incorporating an approved Technical
Corrigendum@Chg{Version=[2],New=[ and @Chg{Version=[3],New=[Amendments],Old=[Amendment]}],
Old=[]}. It is nevertheless a
best effort to be as close as possible to the
technical content of such an updated document. In the case of a conflict between this
document and @Chg{Version=[2],New=[Amendment],Old=[Technical Corrigendum]}
@Chg{Version=[3],New=[2],Old=[1]} as approved by ISO (or between this document
and@Chg{Version=[3],New=[ Amendment 1 in the case of paragraphs
not changed by Amendment 2; or between this document and],Old=[]}
@Chg{Version=[2],New=[Technical Corrigendum 1 in the case of paragraphs
not changed by @Chg{Version=[3],New=[either ],Old=[]}Amendment@Chg{Version=[3],New=[],Old=[ 1]};
or between this document and ],Old=[]}the
original 8652:1995 in the case of paragraphs not changed by @Chg{Version=[2],
New=[either Amendment@Chg{Version=[3],New=[],Old=[ 1]} or ],
Old=[]}Technical Corrigendum 1), the other documents contain the official
text of the
International Standard ISO/IEC 8652:1995(E)@Chg{Version=[2],New=[ and
its @Chg{Version=[3],New=[Amendments],Old=[Amendment]}],Old=[]}.],Old=[]}

@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[2],Kind=[RevisedAdded]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@Chg{New=[As it is very inconvenient to have the Reference Manual for Ada
specified in @Chg{Version=[2],New=[@Chg{Version=[3],New=[four],Old=[three]}],
Old=[two]} documents, this
consolidated version of the Ada Reference
Manual is made available to the public.],Old=[]}

@end{NotISO}
@end{Intro}
(*End of Foreword Foreword, not needed for Ada 2012*)
@end{Comment}}


@UnNumberedSection(Foreword)
@begin{Intro}

@ChgRef{Version=[3],Kind=[Revised]}@ChgNote{Updated boilerplate}
ISO (the International Organization for Standardization)
and IEC (the International Electrotechnical Commission)
form the specialized system for worldwide standardization.
National bodies that are members of ISO or IEC
participate in the development of International Standards through
technical committees established by the respective organization to deal
with particular fields of technical activity.
ISO and IEC technical committees collaborate in fields of mutual
interest.
Other international organizations, governmental and non-governmental,
in liaison with ISO and IEC, also take part in the work.@Chg{Version=[3],New=[ In
the field of information technology,
ISO and IEC have established a joint technical committee, ISO/IEC JTC 1.],Old=[]}

@ChgRef{Version=[3],Kind=[Added]}@ChgNote{Updated boilerplate}
@ChgAdded{Version=[3],Text=[International Standards are drafted in accordance
with the rules given in the ISO/IEC Directives, Part 2.]}

@ChgRef{Version=[3],Kind=[Revised]}@ChgNote{Updated boilerplate}
@Chg{Version=[3],New=[The main task of the],Old=[In the field of information
technology, ISO and IEC have established a]} joint technical
committee@Chg{Version=[3],New=[ is to prepare
International Standards],Old=[, ISO/IEC JTC 1]}.
Draft International Standards adopted by the joint technical committee
are circulated to national bodies for voting.
Publication as an International Standard requires approval by
at least 75 % of the national bodies casting a vote.

@ChgRef{Version=[3],Kind=[Added]}@ChgNote{Updated boilerplate}
@ChgAdded{Version=[3],Text=[Attention is drawn to the possibility that some of
the elements of this document may be the subject of patent rights. ISO and IEC
shall not be held responsible for identifying any or all such patent rights.]}

@ChgRef{Version=[3],Kind=[Revised]}
International Standard ISO/IEC 8652 was prepared by
Joint Technical Committee ISO/IEC JTC 1,
@i{Information Technology}@Chg{Version=[3],New=[ Subcommittee SC22, @i{Programming
languages, their environments and system software interfaces}],Old=[]}.

@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
This @Chg{Version=[3],New=[third],Old=[@Chg{Version=[2],New=[consolidated],Old=[second]}]}
edition @Chg{Version=[3],New=[cancels and replaces],Old=[@Chg{Version=[2],
New=[updates],Old=[cancels and replaces]}]} the @Chg{Version=[2],New=[second],Old=[first]}
edition (ISO@Chg{Version=[3],New=[/IEC],Old=[]} 8652:@Chg{Version=[2],New=[1995)],Old=[1987), of which it constitutes
a technical revision]}@Chg{Version=[3],New=[, of which it constitutes
a technical revision. This edition incorporates the contents of Technical Corrigendum 1
(ISO/IEC 8652:1995:COR.1:2001) and Amendment 1 (ISO/IEC 8652:1995:AMD 1:2007)],Old=[]}.
@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[The above is
unofficial wording added just to avoid confusion. If ISO decides
to publish a new standard, the above would be replaced by @ldquote@;This third
edition cancels and replaces the second edition (ISO 8652:1995), of which it
constitutes a technical revision@rdquote. The first three paragraphs in this
section also would be replaced by the current ISO boilerplate.]}]}
@end{Discussion}


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00440-01]}
Annexes A to J form an integral part of this
International Standard.
Annexes K to @Chg{Version=[2],New=[Q],Old=[P]} are for information only.


@begin{Discussion}
@ChgRef{Version=[3],Kind=[Revised]}
This document is the Annotated Ada Reference Manual (AARM).
It contains the entire text of the Ada @Chg{Version=[3],New=[2012],Old=[95]}
standard
(ISO/IEC 8652:@Chg{Version=[3],New=[201x],Old=[1995]}), plus various annotations.
It is intended primarily for compiler writers,
validation test writers, and other language lawyers.
The annotations include detailed rationale for individual rules
and explanations of some of the more arcane interactions among the
rules.
@end{Discussion}
@end{Intro}
@begin{Comment} (*Was Syntax9XOnly - We don't generate this document anymore*)
This document lists the syntax rules of Ada 95.
@end{Comment}
@begin{Comment} (*Was Chg839XOnly - We don't generate this document anymore*)

This document lists in detail the changes introduced in the second
(Ada 95) edition of the Ada standard (ISO/IEC 8652:1995)
with respect to the first (Ada 83) edition (ISO 8652:1987).

@end{Comment}


@UnNumberedSection{Introduction}

@begin{Intro}
@begin{AARMOnly}
This is the Annotated Ada Reference Manual.
@end{AARMOnly}
@begin{RMOnly}
This is the Ada Reference Manual.
@end{RMOnly}

@Leading@;Other available Ada documents include:
@comment{We have to put the itemize inside of the AARMOnly, because otherwise
the formatter thinks this is a nested bullet, making a mess. Since this is
the only place this is used, it is easier to fix the text than the program.
RLB - 2000-05-17}
@begin{AARMOnly}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@Chg{Version=[2],New=[Ada 95 Rationale. This],Old=[Rationale for the Ada
Programming Language @em 1995 edition, which]} gives an introduction to the
new features of Ada@Chg{Version=[2],New=[ incorporated in the 1995 edition
of this Standard],Old=[]}, and explains the rationale behind them.
Programmers @Chg{Version=[2],New=[unfamiliar with Ada 95 ],
Old=[]}should read this first.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00387-01]}
@ChgAdded{Version=[2],Text=[Ada 2005 Rationale. This gives an introduction to the
changes and new features in Ada 2005 (compared with the 1995 edition), and
explains the rationale behind them. Programmers should read this rationale
before reading this Standard in depth.]}

@ChgRef{Version=[1],Kind=[Deleted]}
@ChgDeleted{Version=[1],Text=[Changes to Ada @em 1987 to 1995. This document
lists in detail the changes made to the 1987 edition of the standard.]}

@ChgRef{Version=[3],Kind=[Revised]}
The Ada Reference Manual (RM).
This is the International Standard @em ISO/IEC 8652:@Chg{Version=[3],New=[201x],Old=[1995]}.

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[Technical Corrigendum 1
@em ISO/IEC 8652:1995:COR.1:2001. This document lists corrections to the
International Standard.]}]}@ChgNote{This is consolidated into the Ada 2012 RM}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[Amendment 1 @em ISO/IEC
8652:1995:AMD 1:2007. This document outlines additional features and corrections
to the International Standard.]}]}@ChgNote{This is consolidated into the Ada 2012 RM}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[The consolidated Ada Reference Manual. An @i{unofficial}
document combining the above three
documents into a single document.]}]}
@end{Itemize}
@end{AARMOnly}

@begin{RMOnly}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@Chg{Version=[2],New=[Ada 95 Rationale. This],Old=[Rationale for the Ada
Programming Language @em 1995 edition, which]} gives an introduction to the
new features of Ada@Chg{Version=[2],New=[ incorporated in the 1995 edition of this Standard],Old=[]},
and explains the rationale behind them. Programmers @Chg{Version=[2],New=[unfamiliar
with Ada 95 ],Old=[]}should read this first.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00387-01]}
@ChgAdded{Version=[2],Text=[Ada 2005 Rationale. This gives an introduction
to the changes and new features in Ada 2005 (compared with the 1995 edition),
and explains the rationale behind them. Programmers should read this rationale
before reading this Standard in depth.]}

@ChgRef{Version=[1],Kind=[Deleted]}
@ChgDeleted{Version=[1], Text=[Changes to Ada @em 1987 to 1995. This document lists in
detail the changes made to the 1987 edition of the standard.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[Revised]}
The Annotated Ada Reference Manual (AARM).@Defn{Annotated Ada Reference Manual}
@Defn{AARM}The AARM contains all of the text
in @Chg{Version=[3],New=[this International Standard],
Old=[@Chg{Version=[2],New=[the consolidated Ada Reference Manual],Old=[the RM95]}]},
@ChgNote{Version=[2]: John had "this International Standard", but it's really
related to the unofficial document. Change it back if ISO decides to publish
this consolidated RM.}
@ChgNote{Version=[3]: Changed back for Ada 2012.}
plus various annotations. It is intended primarily for compiler writers,
validation test writers,
and others who wish to study the fine details.
The annotations include detailed rationale for individual rules
and explanations of some of the more arcane interactions among the
rules.
@end{Itemize}
@end{RMOnly}
@end{Intro}

@SubHeading(Design Goals)

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[Revised]}
Ada was originally designed with three overriding concerns:
program reliability and maintenance, programming as a human
activity, and efficiency. @Chg{Version=[2],New=[The 1995],Old=[This]} revision
to the language was designed to provide greater flexibility and extensibility,
additional control over storage management and
synchronization, and standardized packages oriented toward
supporting important application areas, while at the same
time retaining the original emphasis on reliability,
maintainability, and efficiency.@Chg{Version=[2],New=[ This
@Chg{Version=[3],New=[third edition],Old=[amended version]}
provides further flexibility and adds more standardized packages within the
framework provided by the 1995 revision.],Old=[]}

The need for languages that promote reliability and simplify
maintenance is well established. Hence emphasis was placed
on program readability over ease of writing. For example,
the rules of the language require that program variables be
explicitly declared and that their type be specified. Since
the type of a variable is invariant, compilers can ensure
that operations on variables are compatible with the
properties intended for objects of the type. Furthermore,
error-prone notations have been avoided, and the syntax of
the language avoids the use of encoded forms in favor of
more English-like constructs. Finally, the language offers
support for separate compilation of program units in a way
that facilitates program development and maintenance, and
which provides the same degree of checking between units as
within a unit.

Concern for the human programmer was also stressed during
the design. Above all, an attempt was made to keep to a
relatively small number of underlying concepts integrated in
a consistent and systematic way while continuing to avoid
the pitfalls of excessive involution. The design especially
aims to provide language constructs that correspond
intuitively to the normal expectations of users.

Like many other human activities, the development of
programs is becoming ever more decentralized and
distributed. Consequently, the ability to assemble a
program from independently produced software components
continues to be a central idea in the design. The concepts
of packages, of private types, and of generic units are
directly related to this idea, which has ramifications in
many other aspects of the language. An allied concern is
the maintenance of programs to match changing requirements;
type extension and the hierarchical library enable a program
to be modified while minimizing disturbance to existing
tested and trusted components.

No language can avoid the problem of efficiency. Languages
that require over-elaborate compilers, or that lead to the
inefficient use of storage or execution time, force these
inefficiencies on all machines and on all programs. Every
construct of the language was examined in the light of
present implementation techniques. Any proposed construct
whose implementation was unclear or that required excessive
machine resources was rejected.
@end{Intro}

@SubHeading{Language Summary}

@begin{Intro}
An Ada program is composed of one or more program units.
Program units may be subprograms (which define executable
algorithms), packages (which define collections of
entities), task units (which define concurrent
computations), protected units (which define operations for
the coordinated sharing of data between tasks), or generic
units (which define parameterized forms of packages and
subprograms). Each program unit normally consists of two parts: a
specification, containing the information that must be
visible to other units, and a body, containing the
implementation details, which need not be visible to other
units. Most program units can be compiled separately.

This distinction of the specification and body, and the
ability to compile units separately, allows a program to be
designed, written, and tested as a set of largely
independent software components.

An Ada program will normally make use of a library of
program units of general utility. The language provides
means whereby individual organizations can construct their
own libraries. All libraries are structured in a
hierarchical manner; this enables the logical decomposition
of a subsystem into individual components. The text of a
separately compiled program unit must name the library units
it requires.


@keepnext@i(Program Units)

A subprogram is the basic unit for expressing an algorithm.
There are two kinds of subprograms: procedures and
functions. A procedure is the means of invoking a series of
actions. For example, it may read data, update variables,
or produce some output. It may have parameters, to provide
a controlled means of passing information between the
procedure and the point of call.
A function is the means of invoking the computation of a
value. It is similar to a procedure, but in addition will
return a result.

A package is the basic unit for defining a collection of
logically related entities. For example, a package can be
used to define a set of type declarations and
associated operations. Portions of a package can be hidden
from the user, thus allowing access only to the logical
properties expressed by the package specification.

Subprogram and package units may be compiled separately and
arranged in hierarchies of parent and child units giving
fine control over visibility of the logical properties and
their detailed implementation.

A task unit is the basic unit for defining a task whose
sequence of actions may be executed concurrently with those
of other tasks. Such tasks may be implemented on
multicomputers, multiprocessors, or with interleaved
execution on a single processor. A task unit may define
either a single executing task or a task type permitting the
creation of any number of similar tasks.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
A protected unit is the basic unit for defining protected
operations for the coordinated use of data shared between
tasks. Simple mutual exclusion is provided automatically,
and more elaborate sharing protocols can be defined. A
protected operation can either be a subprogram or an entry.
A protected entry specifies a Boolean expression (an entry
barrier) that must be @Chg{Version=[2],New=[True],Old=[true]}
before the body of the entry is
executed. A protected unit may define a single protected
object or a protected type permitting the creation of
several similar objects.

@keepnext@i(Declarations and Statements)

The body of a program unit generally contains two parts: a
declarative part, which defines the logical entities to be
used in the program unit, and a sequence of statements,
which defines the execution of the program unit.

The declarative part associates names with declared
entities. For example, a name may denote a type, a
constant, a variable, or an exception. A declarative part
also introduces the names and parameters of other nested
subprograms, packages, task units, protected units, and
generic units to be used in the program unit.

The sequence of statements describes a sequence of actions
that are to be performed. The statements are executed in
succession (unless a transfer of control causes execution to continue
from another place).

An assignment statement changes the value of a variable. A
procedure call invokes execution of a procedure after
associating any actual parameters provided at the call with
the corresponding formal parameters.

Case statements and if statements allow the selection of an
enclosed sequence of statements based on the value of an
expression or on the value of a condition.

The loop statement provides the basic iterative mechanism in
the language. A loop statement specifies that a sequence of
statements is to be executed repeatedly as directed by an
iteration scheme, or until an exit statement is encountered.

A block statement comprises a sequence of statements
preceded by the declaration of local entities used by the
statements.

Certain statements are associated with concurrent execution.
A delay statement delays the execution of a task for a
specified duration or until a specified time. An entry call
statement is written as a procedure call statement; it
requests an operation on a task or on a protected
object, blocking the caller until the operation can be
performed. A called task may accept an entry call by
executing a corresponding accept statement, which specifies
the actions then to be performed as part of the rendezvous
with the calling task. An entry call on a protected object
is processed when the corresponding entry barrier evaluates
to true, whereupon the body of the entry is executed. The
requeue statement permits the provision of a service as a
number of related activities with preference control. One
form of the select statement allows a selective wait for one
of several alternative rendezvous. Other forms of the
select statement allow conditional or timed entry calls and
the asynchronous transfer of control in response to some
triggering event.

Execution of a program unit may encounter error situations
in which normal program execution cannot continue. For
example, an arithmetic computation may exceed the maximum
allowed value of a number, or an attempt may be made to
access an array component by using an incorrect index value.
To deal with such error situations, the statements of a
program unit can be textually followed by exception handlers
that specify the actions to be taken when the error
situation arises. Exceptions can be raised explicitly by a
raise statement.

@keepnext@i(Data Types)

Every object in the language has a type, which characterizes
a set of values and a set of applicable operations. The
main classes of types are elementary types (comprising
enumeration, numeric, and access types) and composite types
(including array and record types).

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01],ARef=[AI95-00387-01]}
An enumeration type defines an ordered set of distinct
enumeration literals, for example a list of states or an
alphabet of characters. The enumeration types Boolean,
Character, @Chg{Version=[2],New=[],
Old=[and ]}Wide_Character@Chg{Version=[2],New=[, and Wide_Wide_Character],
Old=[]} are predefined.

Numeric types provide a means of performing exact or
approximate numerical computations. Exact computations use
integer types, which denote sets of consecutive integers.
Approximate computations use either fixed point types, with
absolute bounds on the error, or floating point types, with
relative bounds on the error. The numeric types Integer,
Float, and Duration are predefined.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01],ARef=[AI95-00387-01]}
Composite types allow definitions of structured objects with
related components. The composite types in the language
include arrays and records. An array is an object with
indexed components of the same type. A record is an object
with named components of possibly different types. Task and
protected types are also forms of composite types. The
array types String@Chg{Version=[2],New=[,],Old=[ and]}
Wide_String@Chg{Version=[2],
New=[, and Wide_Wide_String],Old=[]} are predefined.

Record, task, and protected types may have special components
called discriminants which parameterize the type.
Variant record structures that depend on the values of
discriminants can be defined within a record type.

Access types allow the construction of linked data
structures. A value of an access type represents a
reference to an object declared as aliased or to an object
created by the evaluation of an allocator. Several
variables of an access type may designate the same object,
and components of one object may designate the same or other
objects. Both the elements in such linked data structures
and their relation to other elements can be altered during
program execution. Access types also permit references to
subprograms to be stored, passed as parameters, and
ultimately dereferenced as part of an indirect call.

Private types permit restricted views of a type. A private
type can be defined in a package so that only the logically
necessary properties are made visible to the users of the
type. The full structural details that are externally
irrelevant are then only available within the package and
any child units.

From any type a new type may be defined by derivation. A type,
together with its derivatives (both direct and indirect)
form a derivation class. Class-wide operations may be
defined that accept as a parameter an operand of any type in
a derivation class. For record and private types, the derivatives may
be extensions of the parent type. Types that support these
object-oriented capabilities of class-wide operations and type extension
must be tagged, so that
the specific type of an operand within a derivation class can
be identified at run time. When an operation of a tagged
type is applied to an operand whose specific type is not
known until run time, implicit dispatching is performed
based on the tag of the operand.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00387-01]}
@ChgAdded{Version=[2],Text=[
Interface types provide abstract models from which other interfaces and
types may be composed and derived. This provides a reliable form of multiple
inheritance. Interface types may also be implemented by
task types and protected types thereby enabling concurrent programming and
inheritance to be merged.]}

The concept of a type is further refined by the concept of a
subtype, whereby a user can constrain the set of allowed
values of a type. Subtypes can be used to define subranges
of scalar types, arrays with a limited set of index values,
and records and private types with particular discriminant
values.

@keepnext@i(Other Facilities)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@Chg{Version=[2],New=[Aspect],Old=[Representation]} clauses can be
used to specify the mapping
between types and features of an underlying machine. For
example, the user can specify that objects of a given type
must be represented with a given number of bits, or that the
components of a record are to be represented using a given
storage layout. Other features allow the controlled use of
low level, nonportable, or implementation-dependent aspects,
including the direct insertion of machine code.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
The predefined environment of the language provides
for input-output and other capabilities @Chg{Version=[2],New=[],Old=[(such
as string manipulation and random number generation) ]}by means of
standard library packages.
Input-output is supported for
values of user-defined as well as of predefined types.
Standard means of representing values in display form are
also provided.@Chg{Version=[2],New=[],Old=[ Other standard library packages are defined
in annexes of the standard to support systems with
specialized requirements.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00387-01]}
@Chg{Version=[2],New=[The predefined standard library packages provide
facilities such as string manipulation, containers of various kinds (vectors,
lists, maps, etc.), mathematical functions, random number generation, and access
to the execution environment.],Old=[]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00387-01]}
@Chg{Version=[2],New=[The specialized annexes define further predefined library
packages and facilities with emphasis on areas such as real-time
scheduling, interrupt handling, distributed systems, numerical computation, and
high-integrity systems.],Old=[]}

Finally, the language provides a powerful means of
parameterization of program units, called generic program
units. The generic parameters can be types and subprograms
(as well as objects and packages) and so allow general
algorithms and data structures to be defined that are
applicable to all types of a given class.
@end{Intro}

@SubHeading{Language Changes}

@begin{Intro}

@begin{NotIso}
@ChgAdded{Version=[2],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 44
through 57 have been removed as they described differences from the first edition
of Ada (Ada 83).>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Type=[Leading],Text=[This @Chg{Version=[2],New=[amended ],Old=[]}International Standard
@Chg{Version=[2],New=[updates the edition of 1995 which replaced],
Old=[replaces]} the first edition of 1987.
In @Chg{Version=[2],New=[the 1995],Old=[this]} edition, the following major
language changes @Chg{Version=[2],New=[were],Old=[have been]} incorporated:]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[Support for standard 8-bit and 16-bit
character@Chg{Version=[2],New=[s was added],Old=[ sets]}.
See @Chg{Version=[2],New=[clauses @RefSecNum{Character Set}],Old=[Section 2]},
@RefSecNum{Character Types},
@RefSecNum{String Types},
@RefSecNum{The Package Standard},
@RefSecNum{Character Handling}, and
@RefSecNum{String Handling}.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[@Chg{Version=[2],New=[The type model was extended to include facilities
for o],Old=[O]}bject-oriented programming
with @Chg{Version=[2],New=[dynamic],Old=[run-time]} polymorphism.
See the discussions of classes, derived types, tagged types,
record extensions, and private extensions
in clauses @RefSecNum{Derived Types and Classes},
@RefSecNum{Tagged Types and Type Extensions}, and
@RefSecNum{Private Types and Private Extensions}.
@Chg{Version=[2],New=[Additional],Old=[See also the new]} forms of generic
formal parameters @Chg{Version=[2],New=[were],Old=[that are]} allowed
@Chg{Version=[2],New=[as described in clauses
@RefSecNum{Formal Private and Derived Types} and @RefSecNum{Formal Packages}],
Old=[by @RefSec{Formal Private and Derived Types} and @RefSec{Formal Packages}]}.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[Access types @Chg{Version=[2],New=[were],Old=[have been]} extended to allow
an access value to designate a subprogram or an object declared by an
object declaration @Chg{Version=[2],New=[],Old=[(]}as opposed to just
@Chg{Version=[2],New=[an object ],Old=[a heap-]}allocated
@Chg{Version=[2],New=[on a heap],Old=[object)]}.
See @Chg{Version=[2],New=[clause ],Old=[]}@RefSecNum{Access Types}.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[Efficient data-oriented synchronization @Chg{Version=[2],New=[was],
Old=[is]} provided @Chg{Version=[2],New=[by the introduction of],
Old=[via]} protected types. See @Chg{Version=[2],
New=[clause @RefSecNum{Protected Units and Protected Objects}],Old=[Section 9]}.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[The library @Chg{Version=[2],New=[structure was extended to allow
library units to],Old=[units of a library may]} be organized into a
hierarchy of parent and child units.
See @Chg{Version=[2], New=[clause @RefSecNum{Separate Compilation}],
Old=[Section 10]}.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[Additional support
@Chg{Version=[2],New=[was],Old=[has been]} added for interfacing to other
languages. See @RefSecNum{Interface to Other Languages}.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Type=[Leading],Text=[The Specialized Needs Annexes
@Chg{Version=[2],New=[were],Old=[have been]} added
to provide specific support for certain application areas:]}
@begin{InnerItemize}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[@RefSec{Systems Programming}]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[@RefSec{Real-Time Systems}]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[@RefSec{Distributed Systems}]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[@RefSec{Information Systems}]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[@RefSec{Numerics}]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[@RefSec{High Integrity Systems}]}
@end{InnerItemize}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[This International Standard
replaces the second edition of 1995. It],Old=[Amendment 1]} modifies the
@Chg{Version=[3],New=[previous edition],Old=[1995 International Standard]} by
making changes and additions that improve the capability of the language and
the reliability of programs written in the language.@Chg{Version=[3],New=[ This
edition incorporates the changes from Amendment 1 (ISO/IEC 8652:1995:AMD 1:2007),
which],Old=[In particular the changes]}
were designed to improve the portability of programs, interfacing to other
languages, and both the object-oriented and real-time capabilities.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Chg{Version=[3],New=[Significant],
Old=[The following significant]} changes@Chg{Version=[3],New=[ originating
in Amendment 1],Old=[with respect to the 1995 edition]} are incorporated:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[Support for program text is extended to cover the
entire ISO/IEC 10646:2003 repertoire. Execution support now includes the
32-bit character set. See clauses @RefSecNum{Character Set},
@RefSecNum{Character Types},
@RefSecNum{String Types},
@RefSecNum{The Package Standard},
@RefSecNum{Character Handling}, and
@RefSecNum{String Handling}.],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[
The object-oriented model has been improved by the addition of an
interface facility which provides multiple inheritance and additional
flexibility for type extensions. See clauses @RefSecNum{Derived Types and Classes},
@RefSecNum{Tagged Types and Type Extensions}, and
@RefSecNum{Private Types and Private Extensions}. An
alternative notation for calling operations more akin to that used in
other languages has also been added. See clause @RefSecNum{Selected Components}.],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[
Access types have been further extended to unify properties such as
the ability to access constants and to exclude null values. See clause
@RefSecNum{Access Types}. Anonymous access types are now permitted more
freely and anonymous access-to-subprogram types are introduced. See clauses
@RefSecNum{Objects and Named Numbers}, @RefSecNum{Array Types},
@RefSecNum{Access Types}, and @RefSecNum{Object Renaming Declarations}.],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[
The control of structure and visibility has been enhanced to
permit mutually dependent references between units and finer control
over access from the private part of a package. See clauses
@RefSecNum{Incomplete Type Declarations} and @RefSecNum{Context Clauses - With Clauses}.
In addition, limited types have been made more useful by the
provision of aggregates, constants, and constructor functions. See clauses
@RefSecNum{Aggregates}, @RefSecNum{Return Statements},
and @RefSecNum{Limited Types}.],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[
The predefined environment has been extended to include additional time
and calendar operations,
improved string handling, a comprehensive container library, file and
directory management, and access to environment variables. See clauses
@RefSecNum{Formatting, Time Zones, and other operations for Time},
@RefSecNum{String Handling},
@RefSecNum{The Package Directories},
@RefSecNum{The Package Environment_Variables},
and @RefSecNum{Containers}.],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[
Two of the Specialized Needs Annexes have been considerably enhanced:],Old=[]}
@begin{InnerItemize}
@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[
The Real-Time Systems Annex now includes the Ravenscar profile for
high-integrity systems, further dispatching policies such as Round Robin
and Earliest Deadline First, support for timing events, and support for
control of CPU time utilization. See clauses @RefSecNum{Priority Scheduling},
@RefSecNum{The Ravenscar Profile},
@RefSecNum{Execution Time}, and
@RefSecNum{Timing Events}.],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[
The Numerics Annex now includes support for real and complex vectors
and matrices as previously defined in ISO/IEC 13813:1997 plus further basic
operations for linear algebra.
See clause @RefSecNum{Vector and Matrix Manipulation}.],Old=[]}
@end{InnerItemize}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[
The overall reliability of the language has been enhanced by
a number of improvements. These include new syntax which detects
accidental overloading, as well as pragmas for making assertions and
giving better control over the suppression of checks. See clauses
@RefSecNum{Subprogram Declarations},
@RefSecNum{Pragmas Assert and Assertion_Policy}, and
@RefSecNum{Suppressing Checks}.],Old=[]}

@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0245-1]}
@ChgAdded{Version=[3],Text=[In addition, this third edition makes
enhancements to address two important issues, namely, the particular problems of
multiprocessor architectures, and the need to further increase the capabilities
regarding assertions for correctness.]}

@ChgNote{The original "Amendment 2" version of this text follows:
Amendment 2 modifies the 1995 International Standard
by making changes and additions that improve the capability of the language and
the reliability of programs written in the language. In particular, enhancements
are made to address two important issues, namely, the particular problems of
multiprocessor architectures, and the need to further increase the capabilities
regarding assertions for correctness.}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0245-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The following significant changes
with respect to the 1995 edition as amended by Amendment 1 are incorporated:]}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[New syntax (the aspect specification) is introduced
to enable properties to be specified for various entities in a more structured
manner than through pragmas. See clause @RefSecNum{Aspect Specifications}.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The concept of assertions introduced in the 2005
edition is extended with the ability to specify preconditions and postconditions
for subprograms, and invariants for private types. The concept of constraints in
defining subtypes is supplemented with subtype predicates that enable subsets
to be specified other than as simple ranges. These properties are all indicated
using aspect specifications. See clauses @RefSecNum{Subtype Predicates},
@RefSecNum{Preconditions and Postconditions}, and @RefSecNum{Type Invariants}.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[New forms of expressions are introduced. These are
if expressions, case expressions, quantified expressions, and expression
functions. As well as being useful for programming in general by avoiding the
introduction of unnecessary assignments, they are especially valuable in
conditions and invariants since they avoid the need to introduce auxiliary
functions. See clauses @RefSecNum{Conditional Expressions},
@RefSecNum{Quantified Expressions}, and @RefSecNum{Expression Functions}.
Membership tests are also made more flexible. See clauses
@RefSecNum{Expressions} and @RefSecNum{Relational Operators and Membership Tests}.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[A number of changes are made to parameters.
Functions may now have parameters of all modes. In order to mitigate consequent
(and indeed existing) problems of inadvertent order dependence, rules are
introduced to reduce aliasing. A parameter may now be explicitly marked as
aliased and the type of a parameter may be incomplete in
certain circumstances. See clauses @RefSecNum{Incomplete Type Declarations},
@RefSecNum{Subprogram Declarations}, and @RefSecNum{Parameter Associations}.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The use of access types is now more flexible. The
rules for accessibility and certain conversions are improved. See clauses
@RefSecNum{Operations of Access Types}, @RefSecNum{Relational Operators and Membership Tests},
@RefSecNum{Type Conversions}, and @RefSecNum{The Context of Overload Resolution}.
Furthermore, better control of storage pools is
provided. See clause @RefSecNum{Storage Subpools}.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The Real-Time Systems Annex now includes facilities
for defining domains of processors and assigning tasks to them. Improvements are
made to scheduling and budgeting facilities. See clauses @RefSecNum{Synchronous Barriers},
@RefSecNum{Execution Time}, and @RefSecNum{Multiprocessor Implementation}.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[A number of important improvements are made to the
standard library. These include packages for conversions between strings and UTF
encodings, and classification functions for wide and wide wide characters.
Internationalization is catered for by a package giving locale information. See
clauses @RefSecNum{Character Handling}, @RefSecNum{String Encoding}, and
@RefSecNum{The Package Locales}. The container library is extended to include
bounded forms of the existing containers and new containers for indefinite
objects, multiway trees, and queues. See clause @RefSecNum{Containers}.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Finally, certain features are added primarily to
ease the use of containers, such as the ability to iterate over all elements in a
container without having to encode the iteration. These can also be used for
iteration over arrays. See clauses @RefSecNum{User-Defined References},
@RefSecNum{User-Defined Indexing},  @RefSecNum{User-Defined Iterator Types},
and @RefSecNum{Generalized Loop Iteration}.]}
@end{Itemize}
@end{Intro}

@NewPage
@SubHeading(Instructions for Comment Submission)

@begin{Intro}


@ChgRef{Version=[1],Kind=[Revised]}
@Defn{instructions for comment submission}
@Defn{comments, instructions for submission}
Informal comments on this International Standard may be sent via
e-mail to @Chg{New=[@b(ada-comment@@ada-auth.org)],
Old=[@b(ada-comment@@sw-eng.falls-church.va.us)]}.
If appropriate, the Project Editor will initiate
the defect correction procedure.


Comments should use the following format:
@begin(display)
@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
@TabSet(L6)@\@b(!topic) @i[Title summarizing comment]
@\@b(!reference) @Chg{Version=[2],New=[Ada @Chg{Version=[3],New=[2012],Old=[2005]} RM],Old=[RM95-]}@i{ss.ss(pp)}
@\@b(!from) @i{Author Name yy-mm-dd}
@\@b(!keywords) @i{keywords related to topic}
@\@b(!discussion)
@comment{Blank line}
@\@i{text of discussion}
@end(display)

where @i(ss.ss) is the section, clause or subclause number,
@i(pp) is the paragraph number where applicable,
and @i(yy-mm-dd) is the date the comment was sent.
The date is optional, as is the @b(!keywords) line.

@ChgRef{Version=[1],Kind=[Revised]}
@Chg{New=[], Old=[Multiple comments per e-mail message are acceptable.]}
Please use a descriptive @lquotes@;Subject@rquotes@; in your e-mail
message@Chg{New=[, and limit each message to a single comment.], Old=[.]}

When correcting typographical errors or making minor wording
suggestions, please put the correction directly as the topic of the
comment; use square brackets [ ] to indicate text to be omitted and
curly braces { } to indicate text to be added, and provide enough
context to make the nature of the suggestion self-evident or put
additional information in the body of the comment, for example:
@begin{Display}
@TabSet(L6)@\@b(!topic) [c]{C}haracter
@\@b(!topic) it[']s meaning is not defined
@end{Display}


Formal requests for interpretations and for reporting defects in this
International Standard may be made in accordance with the ISO/IEC
JTC 1 Directives and the ISO/IEC JTC 1/SC 22 policy for interpretations.
National Bodies may submit a Defect Report to ISO/IEC JTC 1/SC 22 for resolution
under the JTC 1 procedures.
A response will be provided and, if appropriate,
a Technical Corrigendum will be issued in accordance with the procedures.


@end{Intro}

@begin{NotISO}
@NewPage
@AddedSubHeading{Version=[3],Acknowledgements for the Ada 83 edition}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Ada is the result of a collective effort to design a
common language for programming large scale and real-time systems.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The common high order language program began in
1974. The requirements of the United States Department of Defense were
formalized in a series of documents which were extensively reviewed by the
Services, industrial organizations, universities, and foreign military
departments. The Ada language was designed in accordance with the final (1978)
form of these requirements, embodied in the Steelman specification.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The Ada design team was led by Jean D. Ichbiah and
has included Bernd Krieg-Brueckner, Brian A. Wichmann, Henry F. Ledgard,
Jean-Claude Heliard, Jean-Loup Gailly, Jean-Raymond Abrial, John G.P. Barnes,
Mike Woodger, Olivier Roubine, Paul N. Hilfinger, and Robert Firth.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[At various stages of the project, several people
closely associated with the design team made major contributions. They include
J.B. Goodenough, R.F. Brender, M.W. Davis, G. Ferran, K. Lester, L. MacLaren, E.
Morel, I.R. Nassi, I.C. Pyle, S.A. Schuman, and S.C. Vestal.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Two parallel efforts that were started in the second
phase of this design had a deep influence on the language. One was the
development of a formal definition using denotational semantics, with the
participation of V. Donzeau-Gouge, G. Kahn, and B. Lang. The other was the
design of a test translator with the participation of K. Ripken, P. Boullier, P.
Cadiou, J. Holden, J.F. Hueras, R.G. Lange, and D.T. Cornhill. The entire effort
benefitted from the dedicated assistance of Lyn Churchill and Marion Myers, and
the effective technical support of B. Gravem, W.L. Heimerdinger, and P. Cleve.
H.G. Schmitz served as program manager.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Over the five years spent on this project, several
intense week-long design reviews were conducted, with the participation of P.
Belmont, B. Brosgol, P. Cohen, R. Dewar, A. Evans, G. Fisher, H. Harte, A.L.
Hisgen, P. Knueven, M. Kronental, N. Lomuto, E. Ploedereder, G. Seegmueller, V.
Stenning, D. Taffs, and also F. Belz, R. Converse, K. Correll, A.N. Habermann,
J. Sammet, S. Squires, J. Teller, P. Wegner, and P.R. Wetherall.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Several persons had a constructive influence with
their comments, criticisms and suggestions. They include P. Brinch Hansen, G.
Goos, C.A.R. Hoare, Mark Rain, W.A. Wulf, and also E. Boebert, P. Bonnard, H.
Clausen, M. Cox, G. Dismukes, R. Eachus, T. Froggatt, H. Ganzinger, C. Hewitt,
S. Kamin, R. Kotler, O. Lecarme, J.A.N. Lee, J.L. Mansion, F. Minel, T. Phinney,
J. Roehrich, V. Schneider, A. Singer, D. Slosberg, I.C. Wand, the reviewers of
Ada-Europe, AdaTech, Afcet, those of the LMSC review team, and those of the Ada
Tokyo Study Group.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[These reviews and comments, the numerous evaluation
reports received at the end of the first and second phase, the nine hundred
language issue reports and test and evaluation reports received from fifteen
different countries during the third phase of the project, the thousands of
comments received during the ANSI Canvass, and the on-going work of the IFIP
Working Group 2.4 on system implementation languages and that of the Purdue
Europe LTPL-E committee, all had a substantial influence on the final definition
of Ada.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The Military Departments and Agencies have provided
a broad base of support including funding, extensive reviews, and countless
individual contributions by the members of the High Order Language Working Group
and other interested personnel. In particular, William A. Whitaker provided
leadership for the program during the formative stages. David A. Fisher was
responsible for the successful development and refinement of the language
requirement documents that led to the Steelman specification.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The Ada 83 language definition was developed by Cii
Honeywell Bull and later Alsys, and by Honeywell Systems and Research Center,
under contract to the United States Department of Defense. William E. Carlson
and later Larry E. Druffel served as the technical representatives of the United
States Government and effectively coordinated the efforts of all participants in
the Ada program.]}


@SubHeading(Acknowledgements@Chg{Version=[2],New=[ for the Ada 95 edition],Old=[]})

@begin{Intro}

This International Standard was prepared by the Ada 9X Mapping/Revision
Team based at Intermetrics, Inc., which has included:
W. Carlson, Program Manager;
T. Taft, Technical Director;
J. Barnes (consultant);
B. Brosgol (consultant);
R. Duff (Oak Tree Software);
M. Edwards;
C. Garrity;
R. Hilliard;
O. Pazy (consultant);
D. Rosenfeld;
L. Shafer;
W. White;
M. Woodger.

The following consultants to the Ada 9X Project
contributed to the Specialized Needs Annexes:
T. Baker (Real-Time/Systems Programming @em SEI, FSU);
K. Dritz (Numerics @em Argonne National Laboratory);
A. Gargaro (Distributed Systems @em Computer Sciences);
J. Goodenough (Real-Time/Systems Programming @em SEI);
J. McHugh (Secure Systems @em consultant);
B. Wichmann (Safety-Critical Systems @em NPL: UK).

This work was regularly reviewed by
the Ada 9X Distinguished Reviewers
and the members of the Ada 9X Rapporteur Group (XRG):
E. Ploedereder, Chairman of DRs and XRG (University of Stuttgart: Germany);
B. Bardin (Hughes);
J. Barnes (consultant: UK); @Comment{XRG - UK}
B. Brett (DEC);
B. Brosgol (consultant);
R. Brukardt (RR Software);
N. Cohen (IBM);
R. Dewar (NYU);
G. Dismukes (TeleSoft);
A. Evans (consultant);
A. Gargaro (Computer Sciences);
M. Gerhardt (ESL);
J. Goodenough (SEI); @Comment{Also XRG - U.S.}
S. Heilbrunner (University of Salzburg: Austria); @Comment{Also XRG - Belgium}
P. Hilfinger (UC/Berkeley); @Comment{No longer a DR.}
B. K@latin1(228)llberg (CelsiusTech: Sweden); @Comment{XRG - Sweden}
M. Kamrad II (Unisys);
J. van Katwijk (Delft University of Technology: The Netherlands); @Comment{XRG - The Netherlands}
V. Kaufman (Russia); @Comment{XRG - Russia}
P. Kruchten (Rational); @Comment{Also XRG - France}
R. Landwehr (CCI: Germany); @Comment{Also XRG - Germany}
C. Lester (Portsmouth Polytechnic: UK);
L. M@latin1(229)nsson (TELIA Research: Sweden); @Comment{No longer a DR.}
S. Michell (Multiprocessor Toolsmiths: Canada); @Comment{Also XRG - Canada}
M. Mills (US Air Force);
D. Pogge (US Navy);
K. Power (Boeing);
O. Roubine (Verdix: France);
A. Strohmeier (Swiss Fed Inst of Technology: Switzerland); @Comment{XRG - Switzerland}
W. Taylor (consultant: UK);
J. Tokar (Tartan);
E. Vasilescu (Grumman);
J. Vladik (Prospeks s.r.o.:
   Czech Republic); @Comment{XRG - Czech Republic}
S. Van Vlierberghe (OFFIS: Belgium). @Comment{XRG - Belgium}


Other valuable feedback influencing the revision
process was provided by
the Ada 9X Language Precision
Team (Odyssey Research Associates), the Ada 9X User/Implementer
Teams (AETECH, Tartan, TeleSoft), the Ada 9X Implementation
Analysis Team (New York University) and the Ada community-at-large.


Special thanks go to R. Mathis,
Convenor of ISO/IEC JTC 1/SC 22 Working Group 9.
@Comment{Also XRG - U.S.}


The Ada 9X Project was sponsored by the Ada Joint Program Office.
Christine M. Anderson at the Air Force Phillips Laboratory (Kirtland
AFB, NM) was the project manager.

@AddedSubHeading{Version=[1],Acknowledgements for the Corrigendum version}

@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@Chg{New=<The editor [R. Brukardt (USA)] would like to thank the many people
whose hard work and assistance has made this
@Chg{Version=[3],New=[update],Old=[revision]} possible.>,Old=[]}

@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[Thanks go out to all of the members of the ISO/IEC JTC 1/SC 22/WG 9
Ada Rapporteur Group, whose work on creating and editing the wording
corrections was critical to the entire process. Especially valuable
contributions came from the chairman of the ARG, E. Ploedereder (Germany), who
kept the process moving; J. Barnes (UK) and K. Ishihata (Japan), whose
extremely detailed reviews kept the editor on his toes; G. Dismukes (USA),
M. Kamrad (USA), P. Leroy (France), S. Michell (Canada), T. Taft (USA),
J. Tokar (USA), and other members too numerous to mention.],Old=[]}

@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[Special thanks go to R. Duff (USA) for his explanations of the
previous system of formatting of these documents during the tedious conversion
to more modern formats. Special thanks also go to the convenor of
ISO/IEC JTC 1/SC 22/WG 9, J. Moore (USA), without whose help and support
the Corrigendum and this consolidated reference manual would not have been possible.],Old=[]}

@AddedSubHeading{Version=[2],Acknowledgements for the Amendment 1 version}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@Chg{Version=[2],New=<The editor [R. Brukardt (USA)] would like to thank the many
people whose hard work and assistance has made this
@Chg{Version=[3],New=[update],Old=[revision]} possible.>,Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[Thanks go out to all of the members of the
ISO/IEC JTC 1/SC 22/WG 9
Ada Rapporteur Group, whose work on creating and editing the wording
corrections was critical to the entire process. Especially valuable
contributions came from the chairman of the ARG, P. Leroy (France), who kept
the process on schedule; J. Barnes (UK) whose careful reviews found many
typographical errors; T. Taft (USA), who always seemed to have a suggestion
when we were stuck, and who also was usually able to provide the valuable
service of explaining why things were as they are; S. Baird (USA), who found
many obscure problems with the proposals; and A. Burns (UK), who pushed many of
the real-time proposals to completion. Other ARG members who contributed were:
R. Dewar (USA), G. Dismukes (USA), R. Duff (USA), K. Ishihata (Japan), S.
Michell (Canada), E. Ploedereder (Germany), J.P. Rosen (France), E. Schonberg
(USA), J. Tokar (USA), and T. Vardanega (Italy).]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[Special thanks go to Ada-Europe and the Ada Resource
Association, without whose help and support the Amendment and this consolidated
reference manual would not have been possible. M. Heaney (USA) requires special
thanks for his tireless work on the containers packages. Finally, special
thanks go to the convenor of ISO/IEC JTC 1/SC 22/WG 9, J. Moore (USA), who
guided the document through the standardization process.],Old=[]}

@end{Intro}

@AddedSubHeading{Version=[3],Acknowledgements for the Ada 2012 edition}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=<The editor [R. Brukardt (USA)] would like to thank the many
people whose hard work and assistance has made this revision possible.>}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Thanks go out to all of the members of the
ISO/IEC JTC 1/SC 22/WG 9
Ada Rapporteur Group, whose work on creating and editing the wording
changes was critical to the entire process. Especially valuable
contributions came from the chairman of the ARG, E. Schonberg (USA), who
guided the work; T. Taft (USA), whose insights broke many logjams, both in
design and wording; J. Barnes (UK) whose careful reviews uncovered many
editorial errors; S. Baird (USA), who
repeatedly found obscure interactions with the proposals that the rest of
us missed. Other ARG members who contributed were:
A. Burns (UK), J. Cousins (UK), R. Dewar (USA), G. Dismukes (USA), R. Duff (USA), P. Leroy
(France), B. Moore (Canada), E. Ploedereder (Germany), J.P. Rosen (France),
B. Thomas (USA), and T. Vardanega (Italy).]}@Comment{Pascal Leroy worked
extensively on this work in the early days, although he hasn't participated
recently.}

@ChgRef{Version=[3],Kind=[Added]}
@Chg{Version=[3],New=[Special thanks go to Ada-Europe and the Ada Resource
Association, without whose help and support this third edition of the
Ada Standard would not have been possible. A special mention has to go to
A. Beneschan (USA) for his efforts in eliminating sloppiness in our wording.
M. Heaney (USA) also requires a mention for his efforts to improve the
containers packages. Finally, special thanks go to the convenor of ISO/IEC JTC
1/SC 22/WG 9, J. Tokar (USA), who guided the document through the
standardization process.],Old=[]} @Comment{The other financial contributors
wanted to remain anonymous, so they are not mentioned here.}

@NewPage
@SubHeading{Changes}

@begin{Intro}

@Leading@;The International Standard is the same as this version
of the Reference Manual, except:
@begin{Itemize}
This list of Changes is not included in the International Standard.

The @lquotes@;Acknowledgements@rquotes@; page
is not included in the International Standard.

The text in the running headers and footers on each page is slightly
different in the International Standard.

The title page(s) are different in the International Standard.

This document is formatted for 8.5-by-11-inch paper, whereas
the International Standard is formatted for A4 paper
(210-by-297mm);
thus, the page breaks are in different places.

@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[The @lquotes@;Foreword to this version of the Ada Reference Manual@rquotes
clause is not included in the International Standard.],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[The @lquotes@;Using this version of the Ada
Reference Manual@rquotes clause is not included in the International Standard.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Paragraph numbers are not included
in the International Standard.]}
@end{Itemize}
@end{Intro}
@end{NotISO}

@begin{NotISO}
@begin{Intro}
@AddedSubHeading{Version=[1],Using this version of the Ada Reference Manual}

@begin{RMOnly}
@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[2],Kind=[RevisedAdded]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@Chg{New=[This document has been revised with the corrections specified in Technical Corrigendum 1
(ISO/IEC 8652:1995/COR.1:2001)@Chg{Version=[2],New=[ and
Amendment 1 (ISO/IEC 8652/AMD 1:2007)@Chg{Version=[3],New=[, along with
changes specifically for this third edition],Old=[]}],Old=[]}.
In addition, a variety of editorial errors have been corrected.],Old=[]}
@end{RMOnly}
@begin{AARMOnly}
@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[2],Kind=[RevisedAdded]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@Chg{New=[This document has been revised with the corrections specified in Technical Corrigendum 1
(ISO/IEC 8652:1995/COR.1:2001)@Chg{Version=[2],New=[ and
Amendment 1 (ISO/IEC 8652/AMD 1:2007)@Chg{Version=[3],New=[, along with
changes specifically for this third edition],Old=[]}],Old=[]}.
In addition, @Chg{Version=[3],New=[more],Old=[additional]} annotations
have been added and a variety of editorial errors have been corrected.],Old=[]}
@end{AARMOnly}

@begin{RMOnly}
@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[2],Kind=[RevisedAdded]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@Chg{New=[Changes to the original 8652:1995 can be identified by the version
number @Chg{Version=[2],New=[],Old=[/1 ]}following the paragraph
number.@Chg{Version=[2],New=[ Paragraphs with a version number of /1 were
changed by Technical Corrigendum 1 or were editorial corrections at that time,
while paragraphs with a version number of /2 were changed by Amendment 1 or were
more recent editorial corrections@Chg{Version=[3],New=[, and paragraphs with a
version number of /3 were changed by the third (2012) edition of the Standard
or were still
more recent editorial corrections],Old=[]}.],Old=[]} Paragraphs not so marked are
unchanged by @Chg{Version=[2],New=[@Chg{Version=[3],New=[the third edition, ],Old=[]}Amendment 1, ],Old=[]}Technical Corrigendum
1@Chg{Version=[2],New=[,],Old=[]} or editorial corrections. Paragraph numbers
of unchanged paragraphs are the same as in
the @Chg{Version=[3],New=[1995 edition of the],Old=[original]} Ada Reference
Manual. In addition, some versions of this document include revision bars near the
paragraph numbers. Where paragraphs are inserted, the paragraph numbers are of
the form pp.nn, where pp is the number of the preceding paragraph, and nn is an
insertion number. For instance, the first paragraph inserted after paragraph 8
is numbered 8.1, the second paragraph inserted is numbered 8.2, and so on.
Deleted paragraphs are indicated by the text @i{@shrink{This paragraph was
deleted.}} Deleted paragraphs include empty paragraphs that were numbered in
the @Chg{Version=[3],New=[1995 edition of the],Old=[original]}
Ada Reference Manual.],Old=[]}
@end{RMOnly}
@begin{AARMOnly}
@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[2],Kind=[RevisedAdded]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@Chg{New=[Changes to the original 8652:1995 can be identified by the version
number @Chg{Version=[2],New=[],Old=[/1 ]}following the paragraph
number.@Chg{Version=[2],New=[ Paragraphs with a version number of /1 were
changed by Technical Corrigendum 1 or were editorial corrections at that time,
while paragraphs with a version number of /2 were changed by Amendment 1 or were
more recent editorial corrections@Chg{Version=[3],New=[, and paragraphs with a
version number of /3 were changed by the third (2012) edition of the Standard
or were still
more recent editorial corrections],Old=[]}.],Old=[]} Paragraphs not so marked are
unchanged by @Chg{Version=[2],New=[@Chg{Version=[3],New=[the third edition,
],Old=[]}Amendment 1, ],Old=[]}Technical Corrigendum
1@Chg{Version=[2],New=[,],Old=[]} or editorial corrections. Paragraph numbers
of unchanged paragraphs are the same as in the @Chg{Version=[3],New=[1995
edition of the],Old=[original]} Ada Reference Manual. Inserted text is indicated
by underlining, and deleted text is
indicated by strikethroughs. @Chg{Version=[2],New=[Some versions also use
color to indicate the version of the change.],Old=[]}Where paragraphs are
inserted, the paragraph numbers are of the form pp.nn, where pp is the number
of the preceding paragraph, and nn is an insertion number. For instance, the
first paragraph inserted after paragraph 8 is numbered 8.1, the second
paragraph inserted is numbered 8.2, and so on. Deleted paragraphs are indicated
by the text @i{@shrink{This paragraph was deleted.}} Deleted paragraphs include
empty paragraphs that were numbered in the @Chg{Version=[3],New=[1995 edition of
the],Old=[original]} Ada Reference Manual. Similar markings and numbering
@Chg{Version=[3],New=[are],Old=[is]} used for changes to annotations.],Old=[]}
@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The paragraph number is considered part of the
  paragraph; when a paragraph is moved to a different paragraph number, it is
  marked as changed even if the contents have not changed.]}
@end{Honest}
@end{AARMOnly}
@end{Intro}
@end{NotISO}

