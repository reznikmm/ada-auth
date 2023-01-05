@Part(intro, root="ada.mss")
@comment{$Source: e:\\cvsroot/ARM/Source/intro.mss,v $}
@comment{$Revision: 1.5 $ $Date: 2023/01/05 05:49:12 $}


@UnNumberedSection{Introduction}

@begin{Intro}
@begin{NotISO}
@begin{AARMOnly}
This is the Annotated Ada Reference Manual.
@begin{Discussion}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[5],Kind=[Revised]}
This document is the Annotated Ada Reference Manual (AARM).
It contains the entire text of the Ada
@Chg{Version=[3],New=[@Chg{Version=[5],New=[2022],Old=[2012]}],Old=[95]} standard
(ISO/IEC 8652:@Chg{Version=[3],New=[@Chg{Version=[5],New=[2022],Old=[2012]}],Old=[1995]}),
plus various annotations.
It is intended primarily for compiler writers,
validation test writers, and other language lawyers.
The annotations include detailed rationale for individual rules
and explanations of some of the more arcane interactions among the rules.
@end{Discussion}
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
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0245-1]}
@ChgRef{Version=[5],Kind=[Revised]}
@Chg{Version=[3],New=[Ada @Chg{Version=[5],New=[2022 Overview],Old=[2012 Rationale]}.
This gives an introduction to the changes and
new features in Ada @Chg{Version=[5],New=[2022],Old=[2012, and explains the
rationale behind them]}. Programmers should read this
@Chg{Version=[5],New=[overview],Old=[rationale]} before reading @StdTitle in
depth.@Chg{Version=[5],New=[],Old=[ Rationales for Ada 83, Ada 95, and Ada 2005 are also
available.]}],Old=[@Chg{Version=[2],New=[Ada 95 Rationale. This],Old=[Rationale for the Ada
Programming Language @em 1995 edition, which]} gives an introduction to the
new features of Ada@Chg{Version=[2],New=[ incorporated in the 1995 edition
of this@StdTitle],Old=[]}, and explains the rationale behind them.
Programmers @Chg{Version=[2],New=[unfamiliar with Ada 95 ],
Old=[]}should read this first.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0245-1]}
  @ChgRef{Version=[4],Kind=[DeletedAddedNoDelMsg]}@Comment{Done now.}
  @ChgAdded{Version=[3],Text=[@Chg{Version=[4],New=[],Old=[As of this writing
  (December 2012), only five
  chapters of the Ada 2012 Rationale have been published. Additional
  chapters are in development and should be published during 2013.]}]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg],ARef=[AI05-0245-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[Ada 2005 Rationale.
This gives an introduction to the changes and new features in Ada 2005 (compared
with the 1995 edition), and explains the rationale behind them. Programmers
should read this rationale before reading this @StdTitle in depth.]}]}

@ChgRef{Version=[1],Kind=[Deleted]}
@ChgDeleted{Version=[1],Text=[Changes to Ada @em 1987 to 1995. This document
lists in detail the changes made to the 1987 edition of the standard.]}

@ChgRef{Version=[3],Kind=[Revised]}
The Ada Reference Manual (RM).
This directly corresponds to the International Standard @em
ISO/IEC 8652:@Chg{Version=[3],New=[@Chg{Version=[5],New=[2022],Old=[2012]}],Old=[1995]}.

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[Technical Corrigendum 1
@em ISO/IEC 8652:1995:COR.1:2001. This document lists corrections to the
International Standard.]}]}@ChgNote{The Corrigenda are consolidated into
this document, so we don't mention them separately.}

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
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0245-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0441-1]}
@Chg{Version=[3],New=[Ada @Chg{Version=[5],New=[2022 Overview],Old=[2012 Rationale]}.
This gives an introduction to the changes and
new features in Ada @Chg{Version=[5],New=[2022],Old=[2012, and explains the
rationale behind them]}. Programmers should read this
@Chg{Version=[5],New=[overview],Old=[rationale]} before reading @StdTitle in
depth.@Chg{Version=[5],New=[],Old=[ Rationales for Ada 83, Ada 95, and Ada 2005 are also
available.]}],Old=[@Chg{Version=[2],New=[Ada 95 Rationale. This],Old=[Rationale for the Ada
Programming Language @em 1995 edition, which]} gives an introduction to the
new features of Ada@Chg{Version=[2],New=[ incorporated in the 1995 edition
of this @StdTitle],Old=[]}, and explains the rationale behind them.
Programmers @Chg{Version=[2],New=[unfamiliar with Ada 95 ],
Old=[]}should read this first.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg],ARef=[AI05-0245-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[Ada 2005 Rationale.
This gives an introduction to the changes and new features in Ada 2005 (compared
with the 1995 edition), and explains the rationale behind them. Programmers
should read this rationale before reading this @StdTitle in depth.]}]}

@ChgRef{Version=[1],Kind=[Deleted]}
@ChgDeleted{Version=[1], Text=[Changes to Ada @em 1987 to 1995. This document lists in
detail the changes made to the 1987 edition of the standard.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[Revised]}
The Annotated Ada Reference Manual (AARM).@Defn{Annotated Ada Reference Manual}
@Defn{AARM}The AARM contains all of the text
in @Chg{Version=[2],New=[the consolidated Ada Reference Manual],Old=[the RM95]},
plus various annotations. It is intended primarily for compiler writers,
validation test writers,
and others who wish to study the fine details.
The annotations include detailed rationale for individual rules
and explanations of some of the more arcane interactions among the
rules.
@end{Itemize}
@end{RMOnly}
@end{NotISO}
@end{Intro}

@SubHeading(Design Goals)

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0313-1]}
Ada was originally designed with three overriding concerns:
program reliability and maintenance, programming as a human
activity, and efficiency. @Chg{Version=[2],New=[The 1995],Old=[This]} revision
to the language was designed to provide greater flexibility and extensibility,
additional control over storage management and
synchronization, and standardized packages oriented toward
supporting important application areas, while at the same
time retaining the original emphasis on reliability,
maintainability, and efficiency.@Chg{Version=[2],New=[ @Chg{Version=[5],New=[Subsequent
editions, including this],Old=[This]}
@Chg{Version=[3],New=[@Chg{Version=[5],New=[fourth],Old=[third]}
edition],Old=[amended version]}@Chg{Version=[5],New=[, have
provided],Old=[provides]}
further flexibility and @Chg{Version=[5],New=[added],Old=[adds]}
more standardized packages within the
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

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0313-1]}
No language can avoid the problem of efficiency. Languages
that require over-elaborate compilers, or that lead to the
inefficient use of storage or execution time, force these
inefficiencies on all machines and on all programs. Every
construct of the language was examined in the light of
present implementation techniques. Any proposed construct
whose implementation was unclear or that required excessive
machine resources was rejected.@Chg{Version=[5],New=[ Parallel
constructs were introduced to simplify making safe and efficient
use of modern multicore architectures.],Old=[]}

@end{Intro}

@SubHeading{Language Summary}

@begin{Intro}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0438-1],ARef=[AI12-0441-1]}
An Ada program is composed of one or more program units.
Program units @Chg{Version=[5],New=[can],Old=[may]}
be subprograms (which define executable
algorithms), packages (which define collections of
entities), task units (which define concurrent
computations), protected units (which define operations for
the coordinated sharing of data between tasks), or generic
units (which define parameterized forms of packages and
subprograms). Each program unit normally consists of two parts: a
specification, containing the information that
@Chg{Version=[5],New=[is],Old=[must be]}
visible to other units, and a body, containing the
implementation details, which @Chg{Version=[5],New=[are],Old=[need]}
not @Chg{Version=[5],New=[],Old=[be ]}visible to other
units. Most program units can be compiled separately.

This distinction of the specification and body, and the
ability to compile units separately, allows a program to be
designed, written, and tested as a set of largely
independent software components.

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0446-1]}
An Ada program will normally make use of a library of
program units of general utility. The language provides
means whereby individual organizations can construct their
own libraries. All libraries are structured in a
hierarchical manner; this enables the logical decomposition
of a subsystem into individual components. The text of a
separately compiled program unit
@Chg{Version=[5],New=[names],Old=[must name]} the library units it requires.

@keepnext@i(Program Units)

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0441-1]}
A subprogram is the basic unit for expressing an algorithm.
There are two kinds of subprograms: procedures and
functions. A procedure is the means of invoking a series of
actions. For example, it @Chg{Version=[5],New=[can],Old=[may]} read data,
update variables, or produce some output. It
@Chg{Version=[5],New=[can],Old=[may]} have parameters, to provide
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

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0441-1]}
Subprogram and package units @Chg{Version=[5],New=[can],Old=[may]} be compiled
separately and arranged in hierarchies of parent and child units giving
fine control over visibility of the logical properties and
their detailed implementation.

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0441-1]}
A task unit is the basic unit for defining a task whose
sequence of actions @Chg{Version=[5],New=[can],Old=[may]} be executed
concurrently with those of other tasks. Such tasks
@Chg{Version=[5],New=[can],Old=[may]} be implemented on multicomputers, 
multiprocessors, or with interleaved execution on a single processor. A task
unit @Chg{Version=[5],New=[can],Old=[may]} define either a single executing
task or a task type permitting the creation of any number of similar tasks.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0441-1],ARef=[AI12-0446-1]}
A protected unit is the basic unit for defining protected
operations for the coordinated use of data shared between
tasks. Simple mutual exclusion is provided automatically,
and more elaborate sharing protocols can be defined. A
protected operation can either be a subprogram or an entry.
A protected entry specifies a Boolean expression (an entry
barrier) that @Chg{Version=[5],New=[blocks the execution of the body until
it evaluates to True],Old=[must be @Chg{Version=[2],New=[True],Old=[true]}
before the body of the entry is executed]}. A protected unit
@Chg{Version=[5],New=[can],Old=[may]} define a
single protected object or a protected type permitting the creation of
several similar objects.

@keepnext@i(Declarations and Statements)

The body of a program unit generally contains two parts: a
declarative part, which defines the logical entities to be
used in the program unit, and a sequence of statements,
which defines the execution of the program unit.

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0441-1]}
The declarative part associates names with declared
entities. For example, a name @Chg{Version=[5],New=[can],Old=[may]} denote
a type, a constant, a variable, or an exception. A declarative part
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

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0446-1]}
The loop statement provides the basic iterative mechanism in the language.
A loop statement specifies @Chg{Version=[5],New=[],Old=[that ]}a sequence of
statements @Chg{Version=[5],New=[that are],Old=[is to be]} executed
repeatedly as directed by an iteration scheme, or until an exit statement
is encountered.

A block statement comprises a sequence of statements
preceded by the declaration of local entities used by the
statements.

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0119-1]}
Certain statements are associated with concurrent execution.
A delay statement delays the execution of a task for a
specified duration or until a specified time. An entry call
statement is written as a procedure call statement; it
requests an operation on a task or on a protected
object, blocking the caller until the operation can be
performed. A called task @Chg{Version=[5],New=[can],Old=[may]} accept an 
entry call by executing a corresponding accept statement, which specifies
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
triggering event.@Chg{Version=[5],New=[ Various parallel
constructs, including parallel loops and parallel blocks,
support the initiation of multiple logical threads of control designed
to execute in parallel when multiple processors are available.],Old=[]}

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0441-1]}
Execution of a program unit @Chg{Version=[5],New=[can],Old=[may]} encounter
error situations in which normal program execution cannot continue. For
example, an arithmetic computation @Chg{Version=[5],New=[can],Old=[may]}
exceed the maximum allowed value of a number, or an attempt
@Chg{Version=[5],New=[can],Old=[may]} be made to access an array component
by using an incorrect index value. To deal with such error situations, the
statements of a program unit can be textually followed by exception handlers
that specify the actions to be taken when the error
situation arises. Exceptions can be raised explicitly by a
raise statement.

@keepnext@i(Data Types)

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0313-1]}
Every object in the language has a type, which characterizes
a set of values and a set of applicable operations. The
main @Chg{Version=[5],New=[categories],Old=[classes]} of types are 
elementary types (comprising
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

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0441-1]}
Record, task, and protected types @Chg{Version=[5],New=[can],Old=[may]} have 
special components called discriminants which parameterize the type.
Variant record structures that depend on the values of
discriminants can be defined within a record type.

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0441-1]}
Access types allow the construction of linked data
structures. A value of an access type represents a
reference to an object declared as aliased or to an object
created by the evaluation of an allocator. Several
variables of an access type @Chg{Version=[5],New=[can],Old=[may]} designate
the same object, and components of one object
@Chg{Version=[5],New=[can],Old=[may]} designate the same or other
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

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0438-1],ARef=[AI12-0441-1]}
From any type a new type @Chg{Version=[5],New=[can],Old=[may]} be defined by
derivation. A type, together with its derivatives (both direct and indirect)
form a derivation class. Class-wide operations
@Chg{Version=[5],New=[can],Old=[may]} be defined that accept as a parameter
an operand of any type in a derivation class. For record and private types,
the derivatives @Chg{Version=[5],New=[can],Old=[may]} be extensions of the 
parent type. Types that support these object-oriented capabilities of
class-wide operations and type extension
@Chg{Version=[5],New=[are],Old=[must be]} tagged, so that
the specific type of an operand within a derivation class can
be identified at run time. When an operation of a tagged
type is applied to an operand whose specific type is not
known until run time, implicit dispatching is performed based on the tag of
the operand.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00387-01]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0441-1]}
@ChgAdded{Version=[2],Text=[
Interface types provide abstract models from which other interfaces and
types @Chg{Version=[5],New=[can],Old=[may]} be composed and derived. This
provides a reliable form of multiple inheritance. Interface types
@Chg{Version=[5],New=[can],Old=[may]} also be implemented by task types and
protected types thereby enabling concurrent programming and inheritance
to be merged.]}

The concept of a type is further refined by the concept of a
subtype, whereby a user can constrain the set of allowed
values of a type. Subtypes can be used to define subranges
of scalar types, arrays with a limited set of index values,
and records and private types with particular discriminant values.

@keepnext@i(Other Facilities)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0438-1]}
@Chg{Version=[2],New=[Aspect],Old=[Representation]} clauses can be used to
specify the mapping between types and features of an underlying machine. For
example, the user can specify that objects of a given type
@Chg{Version=[5],New=[are to],Old=[must]} be represented with a given number
of bits, or that the components of a record are to be represented using a
given storage layout. Other features allow the controlled use of
low level, nonportable, or implementation-dependent aspects,
including the direct insertion of machine code.

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0313-1]}
@ChgAdded{Version=[5],Text=[Aspect clauses can also be used to specify more
abstract properties of program entities, such as the pre- and postconditions of
a subprogram, or the invariant for a private type. Additional aspects are
specifiable to allow user-defined types to use constructs of the language, such
as literals, aggregates, or indexing, normally reserved for particular
language-defined categories of types, such as numeric types, record types, or
array types.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00387-01]}
The predefined environment of the language provides
for input-output and other capabilities @Chg{Version=[2],New=[],Old=[(such
as string manipulation and random number generation) ]}by means of
standard library packages. Input-output is supported for
values of user-defined as well as of predefined types.
Standard means of representing values in display form are
also provided.@Chg{Version=[2],New=[],Old=[ Other standard library packages are
defined in annexes of the standard to support systems with
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

@DeletedSubHeading{Version=[5],Language Changes}

@begin{Intro}

@begin{NotIso}
@ChgAdded{Version=[2],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 44
through 57 have been @Chg{Version=[5],New=[replaced and moved to the 
Foreword.],Old=[removed as they described differences from the first edition
of Ada (Ada 83).]}>}]}@Comment{This message should be
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

@begin{ISOOnly}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgRef{Version=[5],Kind=[DeletedAdded],ARef=[AI12-0313-1]}@Comment{Moved to Foreword}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[@Chg{Version=[3],New=[This
International Standard replaces the @Chg{Version=[5],New=[third],Old=[second]} edition of
@Chg{Version=[5],New=[2012],Old=[1995]}. It],Old=[Amendment 1]} modifies the
@Chg{Version=[3],New=[previous edition],Old=[1995 International Standard]} by
making changes and additions that improve the capability of the language and
the reliability of programs written in the
language.@Chg{Version=[5],New=[],Old=[@Chg{Version=[3],New=[ This
edition incorporates the changes from Amendment 1 (ISO/IEC 8652:1995:AMD 1:2007),
which],Old=[In particular the changes]}
were designed to improve the portability of programs, interfacing to other
languages, and both the object-oriented and real-time capabilities.]}]}]}
@end{ISOOnly}
@begin{NotISO}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[@Chg{Version=[3],New=[This Ada Reference Manual
replaces the edition of 1995. It],Old=[Amendment 1]} modifies the
@Chg{Version=[3],New=[previous edition],Old=[1995 edition]} by
making changes and additions that improve the capability of the language and
the reliability of programs written in the
language.]}]}
@end{NotISO}


@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00387-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0299-1]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Chg{Version=[5],New=[],Old=[@Chg{Version=[3],New=[Significant],
Old=[The following significant]} changes@Chg{Version=[3],New=[ originating
in Amendment 1],Old=[with respect to the 1995 edition]} are incorporated:]}]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@Chg{Version=[5],New=[],Old=[@Chg{Version=[2],New=[Support for
program text is extended to cover the
entire ISO/IEC 10646:2003 repertoire. Execution support now includes the
32-bit character set. See @Chg{Version=[3],New=[subclauses],Old=[clauses]}
@RefSecNum{Character Set},
@RefSecNum{Character Types},
@RefSecNum{String Types},
@RefSecNum{The Package Standard},
@RefSecNum{Character Handling}, and
@RefSecNum{String Handling}.],Old=[]}]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@Chg{Version=[5],New=[],Old=[@Chg{Version=[2],New=[The
object-oriented model has been improved by the addition of an
interface facility which provides multiple inheritance and additional
flexibility for type extensions. See @Chg{Version=[3],New=[subclauses],Old=[clauses]} @RefSecNum{Derived Types and Classes},
@RefSecNum{Tagged Types and Type Extensions}, and
@RefSecNum{Private Types and Private Extensions}. An
alternative notation for calling operations more akin to that used in
other languages has also been added. See @Chg{Version=[3],New=[subclause],Old=[clause]} @RefSecNum{Selected Components}.],Old=[]}]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@Chg{Version=[5],New=[],Old=[@Chg{Version=[2],New=[Access
types have been further extended to unify properties such as
the ability to access constants and to exclude null values. See clause
@RefSecNum{Access Types}. Anonymous access types are now permitted more
freely and anonymous access-to-subprogram types are introduced. See
@Chg{Version=[3],New=[subclauses],Old=[clauses]}
@RefSecNum{Objects and Named Numbers}, @RefSecNum{Array Types},
@RefSecNum{Access Types}, and @RefSecNum{Object Renaming Declarations}.],Old=[]}]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@Chg{Version=[5],New=[],Old=[@Chg{Version=[2],New=[The
control of structure and visibility has been enhanced to
permit mutually dependent references between units and finer control
over access from the private part of a package. See @Chg{Version=[3],New=[subclauses],Old=[clauses]}
@RefSecNum{Incomplete Type Declarations} and @RefSecNum{Context Clauses - With Clauses}.
In addition, limited types have been made more useful by the
provision of aggregates, constants, and constructor functions. See
@Chg{Version=[3],New=[subclauses],Old=[clauses]}
@RefSecNum{Aggregates}, @RefSecNum{Return Statements},
and @RefSecNum{Limited Types}.],Old=[]}]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@Chg{Version=[5],New=[],Old=[@Chg{Version=[2],New=[The
predefined environment has been extended to include additional time
and calendar operations,
improved string handling, a comprehensive container library, file and
directory management, and access to environment variables. See
@Chg{Version=[3],New=[subclauses],Old=[clauses]}
@RefSecNum{Formatting, Time Zones, and other operations for Time},
@RefSecNum{String Handling},
@RefSecNum{The Package Directories},
@RefSecNum{The Package Environment_Variables},
and @RefSecNum{Containers}.],Old=[]}]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg]}
@Chg{Version=[5],New=[],Old=[@Chg{Version=[2],New=[Two
of the Specialized Needs Annexes have been considerably enhanced:],Old=[]}]}
@begin{InnerItemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg]}
@Chg{Version=[5],New=[],Old=[@Chg{Version=[2],New=[
The Real-Time Systems Annex now includes the Ravenscar profile for
high-integrity systems, further dispatching policies such as Round Robin
and Earliest Deadline First, support for timing events, and support for
control of CPU time utilization. See
@Chg{Version=[3],New=[subclauses],Old=[clauses]} @RefSecNum{Priority Scheduling},
@RefSecNum{The Ravenscar and Jorvik Profiles},
@RefSecNum{Execution Time}, and
@RefSecNum{Timing Events}.],Old=[]}]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg]}
@Chg{Version=[5],New=[],Old=[@Chg{Version=[2],New=[
The Numerics Annex now includes support for real and complex vectors
and matrices as previously defined in ISO/IEC 13813:1997 plus further basic
operations for linear algebra.
See @Chg{Version=[3],New=[subclause],Old=[clause]} @RefSecNum{Vector and Matrix Manipulation}.],Old=[]}]}
@end{InnerItemize}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0441-1]}
@Chg{Version=[5],New=[],Old=[@Chg{Version=[2],New=[
The overall reliability of the language has been enhanced by
a number of improvements. These include new syntax which detects
accidental overloading, as well as pragmas for making assertions and
giving better control over the suppression of checks. See @Chg{Version=[3],New=[subclauses],Old=[clauses]}
@RefSecNum{Subprogram Declarations},
@RefSecNum{Pragmas Assert and Assertion_Policy}, and
@RefSecNum{Suppressing Checks}.],Old=[]}]}


@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0245-1]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1]}
@ChgDeleted{Version=[5],Text=[@Chg{Version=[3],New=[In addition, this third edition makes
enhancements to address two important issues, namely, the particular problems of
multiprocessor architectures, and the need to further increase the capabilities
regarding assertions for correctness. It also makes additional changes and
additions that improve the capability of the language and the reliability of
programs written in the language.],Old=[]}]}

@ChgNote{The original "Amendment 2" version of this text follows:
Amendment 2 modifies the 1995 International Standard
by making changes and additions that improve the capability of the language and
the reliability of programs written in the language. In particular, enhancements
are made to address two important issues, namely, the particular problems of
multiprocessor architectures, and the need to further increase the capabilities
regarding assertions for correctness.}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0245-1],ARef=[AI05-0299-1]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1]}
@ChgDeleted{Version=[5],Text=[@Chg{Version=[3],New=[The following significant changes
with respect to the 1995 edition as amended by Amendment 1 are incorporated:],Old=[]}]}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1]}
@ChgDeleted{Version=[5],Text=[@Chg{Version=[3],New=[New syntax (the aspect specification) is introduced
to enable properties to be specified for various entities in a more structured
manner than through pragmas. See subclause @RefSecNum{Aspect Specifications}.],Old=[]}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0141-1]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1]}
@ChgDeleted{Version=[5],Text=[@Chg{Version=[3],New=[The concept of assertions introduced in the 2005
edition is extended with the ability to specify preconditions and postconditions
for subprograms, and invariants for private types@Chg{Version=[4],New=[ and
interfaces],Old=[]}. The concept of constraints in
defining subtypes is supplemented with subtype predicates that enable subsets
to be specified other than as simple ranges. These properties are all indicated
using aspect specifications. See subclauses @RefSecNum{Subtype Predicates},
@RefSecNum{Preconditions and Postconditions}, and @RefSecNum{Type Invariants}.],Old=[]}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0141-1]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1]}
@ChgDeleted{Version=[5],Text=[@Chg{Version=[3],New=[New forms of expressions are introduced. These are
if expressions, case expressions, quantified expressions,
@Chg{Version=[4],New=[],Old=[and ]}expression
functions@Chg{Version=[4],New=[, and raise expressions],Old=[]}. As well as
being useful for programming in general by avoiding the
introduction of unnecessary assignments, they are especially valuable in
conditions and invariants since they avoid the need to introduce auxiliary
functions. See subclauses @RefSecNum{Conditional Expressions},
@RefSecNum{Quantified Expressions},
@Chg{Version=[4],New=[],Old=[and ]}@RefSecNum{Expression Functions}@Chg{Version=[4],New=[,
and @RefSecNum{Raise Statements and Raise Expressions}],Old=[]}.
Membership tests are also made more flexible. See subclauses
@RefSecNum{Expressions} and @RefSecNum{Relational Operators and Membership Tests}.],Old=[]}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1]}
@ChgDeleted{Version=[5],Text=[@Chg{Version=[3],New=[A number of changes are made to subprogram parameters.
Functions may now have parameters of all modes. In order to mitigate consequent
(and indeed existing) problems of inadvertent order dependence, rules are
introduced to reduce aliasing. A parameter may now be explicitly marked as
aliased and the type of a parameter may be incomplete in
certain circumstances. See subclauses @RefSecNum{Incomplete Type Declarations},
@RefSecNum{Subprogram Declarations}, and @RefSecNum{Parameter Associations}.],Old=[]}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1]}
@ChgDeleted{Version=[5],Text=[@Chg{Version=[3],New=[The use of access types is now more flexible. The
rules for accessibility and certain conversions are improved. See subclauses
@RefSecNum{Operations of Access Types}, @RefSecNum{Relational Operators and Membership Tests},
@RefSecNum{Type Conversions}, and @RefSecNum{The Context of Overload Resolution}.
Furthermore, better control of storage pools is
provided. See subclause @RefSecNum{Storage Subpools}.],Old=[]}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1]}
@ChgDeleted{Version=[5],Text=[@Chg{Version=[3],New=[The Real-Time Systems Annex now includes facilities
for defining domains of processors and assigning tasks to them. Improvements are
made to scheduling and budgeting facilities. See subclauses @RefSecNum{Synchronous Barriers},
@RefSecNum{Execution Time}, and @RefSecNum{Multiprocessor Implementation}.],Old=[]}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1]}
@ChgDeleted{Version=[5],Text=[@Chg{Version=[3],New=[A number of important improvements are made to the
standard library. These include packages for conversions between strings and UTF
encodings, and classification functions for wide and wide wide characters.
Internationalization is catered for by a package giving locale information. See
subclauses @RefSecNum{Character Handling}, @RefSecNum{String Encoding}, and
@RefSecNum{The Package Locales}. The container library is extended to include
bounded forms of the existing containers and new containers for indefinite
objects, multiway trees, and queues. See subclause @RefSecNum{Containers}.],Old=[]}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0313-1]}
@ChgDeleted{Version=[5],Text=[@Chg{Version=[3],New=[Finally, certain features are added primarily to
ease the use of containers, such as the ability to iterate over all elements in a
container without having to encode the iteration. These can also be used for
iteration over arrays, and within quantified expressions. See
subclauses @RefSecNum{User-Defined References},
@RefSecNum{User-Defined Indexing}, @RefSecNum{User-Defined Iterator Types},
and @RefSecNum{Generalized Loop Iteration}.],Old=[]}]}
@end{Itemize}
@end{Intro}

