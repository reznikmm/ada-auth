@Part(frontmatter, root="ada.mss")
@UnNumberedSection(Foreword)
@comment{$Source: e:\\cvsroot/ARM/Source/front_matter.mss,v $}
@comment{$Revision: 1.21 $ $Date: 2000/08/12 00:40:18 $}

@begin{Intro}

ISO (the International Organization for Standardization)
and IEC (the International Electrotechnical Commission)
form the specialized system for worldwide standardization.
National bodies that are members of ISO or IEC
participate in the development of International Standards through
technical committees established by the respective organization to deal
with particular fields of technical activity.
ISO and IEC technical committees collaborate in fields of mutual
interest.
Other international organizations,
governmental and non-governmental,
in liaison with ISO and IEC,
also take part in the work.

In the field of information technology,
ISO and IEC have established a joint technical committee,
ISO/IEC JTC 1.
Draft International Standards adopted by the joint technical committee
are circulated to national bodies for voting.
Publication as an International Standard requires approval by
at least 75 % of the national bodies casting a vote.

International Standard ISO/IEC 8652 was prepared by
Joint Technical Committee ISO/IEC JTC 1,
@i{Information Technology}.

This second edition cancels and replaces the first edition
(ISO 8652:1987),
of which it constitutes a technical revision.


Annexes A to J form an integral part of this
International Standard.
Annexes K to P are for information only.


@begin{Discussion}
This document is the Annotated Ada Reference Manual (AARM).
It contains the entire text of the Ada 95 standard
(ISO/IEC 8652:1995), plus various annotations.
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
Rationale for the Ada Programming Language @em 1995 edition,
which gives an introduction to the new features of Ada,
and explains the rationale behind them.
Programmers should read this first.

@ChgRef{Version=[1],Kind=[Deleted]}
@Chg{New=[], Old=[Changes to Ada @em 1987 to 1995. This document lists in detail the changes made
to the 1987 edition of the standard.]}

The Ada Reference Manual (RM).
This is the International Standard @em ISO/IEC 8652:1995.
@end{Itemize}
@end{AARMOnly}

@begin{RMOnly}
@begin{Itemize}
Rationale for the Ada Programming Language @em 1995 edition,
which gives an introduction to the new features of Ada,
and explains the rationale behind them.
Programmers should read this first.

@ChgRef{Version=[1],Kind=[Deleted]}
@Chg{New=[], Old=[Changes to Ada @em 1987 to 1995. This document lists in detail the changes made
to the 1987 edition of the standard.]}

The Annotated Ada Reference Manual (AARM).@Defn{Annotated Ada Reference Manual}
@Defn{AARM}The AARM contains all of the text in the RM95,
plus various annotations.
It is intended primarily for compiler writers,
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
Ada was originally designed with three overriding concerns:
program reliability and maintenance, programming as a human
activity, and efficiency. This revision to the language was
designed to provide greater flexibility and extensibility,
additional control over storage management and
synchronization, and standardized packages oriented toward
supporting important application areas, while at the same
time retaining the original emphasis on reliability,
maintainability, and efficiency.

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

A protected unit is the basic unit for defining protected
operations for the coordinated use of data shared between
tasks. Simple mutual exclusion is provided automatically,
and more elaborate sharing protocols can be defined. A
protected operation can either be a subprogram or an entry.
A protected entry specifies a Boolean expression (an entry
barrier) that must be true before the body of the entry is
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

An enumeration type defines an ordered set of distinct
enumeration literals, for example a list of states or an
alphabet of characters. The enumeration types Boolean,
Character, and Wide_Character are predefined.

Numeric types provide a means of performing exact or
approximate numerical computations. Exact computations use
integer types, which denote sets of consecutive integers.
Approximate computations use either fixed point types, with
absolute bounds on the error, or floating point types, with
relative bounds on the error. The numeric types Integer,
Float, and Duration are predefined.

Composite types allow definitions of structured objects with
related components. The composite types in the language
include arrays and records. An array is an object with
indexed components of the same type. A record is an object
with named components of possibly different types. Task and
protected types are also forms of composite types. The
array types String and Wide_String are predefined.

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

The concept of a type is further refined by the concept of a
subtype, whereby a user can constrain the set of allowed
values of a type. Subtypes can be used to define subranges
of scalar types, arrays with a limited set of index values,
and records and private types with particular discriminant
values.

@keepnext@i(Other Facilities)

Representation clauses can be used to specify the mapping
between types and features of an underlying machine. For
example, the user can specify that objects of a given type
must be represented with a given number of bits, or that the
components of a record are to be represented using a given
storage layout. Other features allow the controlled use of
low level, nonportable, or implementation-dependent aspects,
including the direct insertion of machine code.

The predefined environment of the language provides
for input-output and other capabilities (such
as string manipulation and random number generation)
by means of standard library packages.
Input-output is supported for
values of user-defined as well as of predefined types.
Standard means of representing values in display form are
also provided. Other standard library packages are defined
in annexes of the standard to support systems with
specialized requirements.

Finally, the language provides a powerful means of
parameterization of program units, called generic program
units. The generic parameters can be types and subprograms
(as well as objects and packages) and so allow general
algorithms and data structures to be defined that are
applicable to all types of a given class.
@end{Intro}

@SubHeading{Language Changes}

@begin{Intro}

@Leading@;This International Standard replaces the first edition of 1987.
In this edition, the following major language changes have been incorporated:
@begin{Itemize}
Support for standard 8-bit and 16-bit character sets.
See Section 2,
@RefSecNum{Character Types},
@RefSecNum{String Types},
@RefSecNum{The Package Standard},
@RefSecNum{Character Handling}, and
@RefSecNum{String Handling}.

Object-oriented programming with run-time polymorphism.
See the discussions of classes, derived types, tagged types,
record extensions, and private extensions
in clauses @RefSecNum{Derived Types and Classes},
@RefSecNum{Tagged Types and Type Extensions}, and
@RefSecNum{Private Types and Private Extensions}.
See also the new forms of generic formal parameters
that are allowed by @RefSec{Formal Private and Derived Types}
and @RefSec{Formal Packages}.

Access types have been extended to allow an access value to designate
a subprogram or an object declared by an object declaration
(as opposed to just a heap-allocated object).
See @RefSecNum{Access Types}.

Efficient data-oriented synchronization is provided via
protected types.
See Section 9.

The library units of a library may be organized into a hierarchy of
parent and child units.
See Section 10.

Additional support has been added for interfacing to other languages.
See @RefSecNum{Interface to Other Languages}.

@leading@;The Specialized Needs Annexes have been added
to provide specific support for certain
application areas:
@begin{InnerItemize}
@RefSec{Systems Programming}

@RefSec{Real-Time Systems}

@RefSec{Distributed Systems}

@RefSec{Information Systems}

@RefSec{Numerics}

@RefSec{Safety and Security}
@end{InnerItemize}
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
@TabSet(L6)@\@b(!topic) @i[Title summarizing comment]
@\@b(!reference) RM95-@i{ss.ss(pp)}
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

Multiple comments per e-mail message are acceptable.
Please use a descriptive @lquotes@;Subject@rquotes@; in your e-mail message.

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
JTC1 Directives and the ISO/IEC JTC1/SC22 policy for interpretations.
National Bodies may submit a Defect Report to ISO/IEC JTC1/SC22 for resolution
under the JTC1 procedures.
A response will be provided and, if appropriate,
a Technical Corrigendum will be issued in accordance with the procedures.


@end{Intro}

@begin{NotISO}
@NewPage
@SubHeading(Acknowledgements)

@begin{Intro}

This International Standard was prepared by the Ada 9X Mapping/Revision Team based
at Intermetrics, Inc., which has included:
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
Convenor of ISO/IEC JTC1/SC22 Working Group 9.
@Comment{Also XRG - U.S.}


The Ada 9X Project was sponsored by the Ada Joint Program Office.
Christine M. Anderson at the Air Force Phillips Laboratory (Kirtland
AFB, NM) was the project manager.

@end{Intro}

@NewPage
@SubHeading{Changes}

@begin{Intro}

@Leading@;The International Standard is the same as this version
of the Reference Manual, except:
@begin{Itemize}
This list of Changes
is not included in the International Standard.

The @lquotes@;Acknowledgements@rquotes@; page
is not included in the International Standard.

The text in the running headers and footers on each page is slightly
different in the International Standard.

The title page(s) are
different in the International Standard.

This document is formatted for 8.5-by-11-inch paper, whereas
the International Standard is formatted for A4 paper
(210-by-297mm);
thus, the page breaks are in different places.
@end{Itemize}
@end{Intro}
@end{NotISO}
