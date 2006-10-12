@Part(rat, Root="asis.msm")

@Comment{$Date: 2006/10/10 05:10:36 $}

@comment{$Source: e:\\cvsroot/ARM/ASIS/rat.mss,v $}
@comment{$Revision: 1.2 $}

@LabeledInformativeAnnex{Rationale}


@LabeledClause{Benefits of code analysis}

Why is something like ASIS needed? ASIS provides a basis for implementing
portable tools that are aimed at analyzing static properties in Ada source
code. Such code analysis capability has in general been
under leveraged in software organizations; but for the Ada language in
particular, it can greatly enhance the development process. Code analysis
automation can harness the excellent software engineering features of Ada to
facilitate code comprehension, high reliability, and high quality of the
software product. The following text presents some motivational background.

@LabeledSubClause{Definition}


Code analysis@Defn{Code analysis} is the inspection of software source code for
the purpose of extracting information about the software. Such information can
pertain to individual software elements (e.g., standards compliance, test
coverage), the element attributes (e.g., quality, correctness, size, metrics),
and element relationships (e.g., complexity, dependencies, data usage, call
trees); thus, it can support documentation generation, code review,
maintainability assessment, reverse engineering, and other software development
activities.

Extracted information falls into two major categories: @i{descriptive}
reports@Defn{Descriptive report}@Defn2{Term=[Report],Sec=[Descriptive]}
which present some view of the software without judgment (e.g., dependency
trees, call trees), and @i{proscriptive}
reports@Defn{Proscriptive report}@Defn2{Term=[Report],Sec=[Proscriptive]}
which look for particular deficiencies @em or their absence (e.g., stack
overflow, excessive complexity, unintended recursion).


@LabeledSubClause{Applicability}

Broadly speaking, the application of automated code analysis in the software
development process promises, among other things, to:

@begin{Itemize}
Promote @i{@b{discipline and consistency}} during development, increasing
productivity and reducing unintended variation.

Provide empirical evidence and @i{@b{metrics}} for process monitoring and
improvement.

Supplement code @i{@b{inspection and review}}, diversifying beyond the
limitations of testing or manual checking.

Preserve @i{@b{architectural integrity}} in the software as compromises are
made during development.

Avoid violations of @i{@b{coding standards}}, such as the use of inefficient
language constructs.

Increase the @i{@b{correctness and quality}} of delivered software, reducing
defects via comprehensive assessment.

Enhance @i{@b{safety and security}} by applying formal methods to verify
assertions in program code.

Expedite @i{@b{program comprehension}} during maintenance, for engineers new to
the code.

Support @i{@b{reengineering and reuse}} of legacy code, reducing costs.

Result in @i{@b{reduced risk}} to budget and schedule.
@end{Itemize}

The software development life-cycle phases where code analysis can be
beneficially applied include all those in which source code exists: preliminary
design (software architecture and interface definition), through testing and
system integration, to maintenance and reengineering. Hence, automated code
analysis is a technology that primarily supports the back end of the software
life cycle.


@LabeledSubClause{Motivation}

@leading@;Over the years, a wide-ranging set of commercial code analysis tools
has become increasingly available [11]; examples of such tools include:
@begin{Itemize}
Data flow analysis and usage metrics

Invocation (call) trees and cross-reference

Dependency trees and impact analysis

Timing and sizing estimation

Test-case generation and coverage analysis

Usage counts of language constructs

Quality assessment metrics

Coding style and standards compliance

Safety and security verification

Code browsing and navigation

Documentation generation

Reverse engineering and re-engineering

Language translation and code restructuring
@end{Itemize}

Unfortunately, the current state-of-the-practice in software development either
omits code analysis support altogether or only incorporates it as an ancillary,
undisciplined, ad hoc resource. For example, it is not uncommon to find within
a given project various home-grown tools that support the above goals but which
are not recognized as overtly participating in the development process. Such
tools can be quite obtuse (very indirect extraction of information) and are
typically incomplete (handling only a subset of the development language).
Further, they tend to be project-specific (or even person-specific), and cannot
be reused in another project: they are later redeveloped from scratch.

These observations corroborate that the need for code analysis is genuine, and
that a common set of uniform tools could provide significant benefits to
projects. But in the case of Ada software, commercial code analysis tools have
historically proven to be barely adequate, manifesting a variety of problems
whose nature and origin are described below.


@LabeledClause{Technology for code analysis}

For Ada, why is ASIS the best approach? Code analysis tools are not new, having
been available for decades; but the advent of the Ada language has exposed a
variety of analysis limitations and has consequently demanded more
comprehensive technology. The following text articulates various Ada-specific
issues from a historical perspective: it reviews several technologies that have
historically been applied @em with varying degrees of success @em to code
analysis specifically targeted to Ada software. The review is not
comprehensive, but it sketches the evolution of issues that have propelled the
development of the ASIS concept.


@LabeledSubClause{Code parsers}

@leading@;Historically, many commercial code analysis tools have been supplied by
compiler vendors in conjunction with their compiler products. But as the
community of CASE tool vendors has grown, such tools are often available
independent of any compiler. Tool developers have found that conventional
parser@Defn{Parser} technology is sufficient for most traditional languages;
thus when Ada came along, most vendors expected it would suffice to simply
adapt their parsers to handle Ada syntax. But for Ada, the result has held many
disappointments:

@begin{Itemize}
Textual code editors are often sensitive to Ada syntax but not to Ada
semantics.

Graphical design editors yield valid graphics, but invalid Ada designs.

Source-level tools such as debuggers are forced to understand and traverse the
internal data structures of program libraries rather than the text of original
source files.

Reverse engineering and test tools manifest difficulties when trying to resolve
overloaded subprogram names or renamings.

Except for compilers, Ada tools do not require Ada Compiler Validation
Capability (ACVC) certification; hence, such tools typically fail to handle the
complete repertoire of Ada language features.
@end{Itemize}

Consider the case of a toolsmith who wants to develop a call-tree analyzer. For
such a tool to accurately process Ada source files, the toolsmith would be
forced to build almost the entire front end of an Ada compiler @em a decidedly
major undertaking that far out-scopes the original tool building effort. But
CASE tool vendors are not in the compiler business; most are reluctant to make
this major investment, or have tried and failed. Yet tools built on parser
technology alone are not able to fully support the semantic richness of Ada.


@LabeledSubClause{DIANA}

Many Ada compilers store program units into libraries. They typically structure
the information according to some proprietary internal form, such as trees of
DIANA (Descriptive Intermediate Attributed Notation for Ada @em note that the
following discussion applies to all internal forms, but that DIANA is singled
out due to its public documentation [5, 6]: DIANA had been intended for
standardization, but failed due to the unexpectedly wide variation in internal
forms).@Defn{DIANA} Such trees thus encode both syntactic and semantic
information about Ada programs. The root of a DIANA tree corresponds to a
compilation unit; the nodes correspond to smaller Ada structures, such as
declarations and statements. Node attributes contain descriptive information
and references to other nodes.

DIANA trees offer great convenience and power to toolsmiths, and are sufficient
to support the implementation of a large variety of tools (including code
generators in compilers). For example, with access to DIANA, the toolsmith who
wanted to develop the call-tree analyzer would have a fairly straightforward
project. Furthermore, the tool would exhibit better performance, bypassing the
needless regeneration (and redundant storage) of intermediate compilation
results that are already available in the Ada libraries.

@leading@;The power of DIANA is sufficient to support the implementation of a
virtually unlimited variety of tools. In general, any tool that requires the
semantic information generated by an Ada compiler can benefit from access to
DIANA. But as with any technology, the use of DIANA also has drawbacks:

@begin{Itemize}
A given implementation of DIANA by a vendor is @i{@b{subject to change}}:
upgrades can obsolete tools written against previous versions, hampering
maintenance.

Similarly, DIANA @i{@b{implementations}} vary from vendor to vendor: porting a
tool across platforms is a risky endeavor.

DIANA is @i{@b{hard to use}}: the trees are quite complex, making it difficult
to write and debug tools written against a DIANA specification.

The lack of a simple mapping to Ada makes DIANA @i{@b{hard to understand}}: as
an abstracted representation of an Ada program, it does not map intuitively to
concrete Ada structures.

DIANA is @i{@b{not extensible}}; but tools may need to add attributes for
storing graphical or other tool-specific information.

@end{Itemize}


@LabeledSubClause{LRM-interface}

Thus a growing need arose to make tool development possible at the Ada level
rather than at the internal representation level. It was these issues that
drove some Ada compiler vendors to independently develop proprietary
higher-level interfaces to encapsulate their Ada program libraries.

In particular, to overcome the drawbacks of DIANA while retaining all of its
advantages, Rational Software Corporation developed their
LRM-Interface@Defn{LRM-Interface} product
[8] in the late 1980's. It provided nearly the same power as DIANA, through
services that extracted a variety of information from the internal DIANA trees.
The LRM-Interface was also considerably easier to understand than DIANA,
because it used the already-familiar terminology defined in the Ada LRM (the
original @i{Reference Manual for the Ada Programming Language} [12], or its more
recent version, the @i{Ada 95 Reference Manual} [7]). Furthermore, the
LRM-Interface was not subject to change (or at least much less so than was the
underlying DIANA), so tools written against it were easily migrated to updated
implementations.@Chg{Version=[1],New=[@i{@b{We're not allowed to reference other
standards this way, in particular "Ada Reference Manual" is verboten. That
happens a lot in this Standard (I'm surprised that ITTF didn't complain).
We need to fix that in a variety of places. - RLB}}],Old=[]}

@leading@;Regardless of LRM-Interface specifics, this and other similar
approaches generally provide great flexibility: for example, an ad hoc tool can
be easily and quickly built by in-house engineers, without funding the
development or specialization of a commercial tool. But as expected, this
approach also has shortcomings:

@begin{Itemize}
While DIANA as a data structure is not extensible, the above interfaces can be
extended in the sense of user-supplied secondary functions built on the
functions already provided; even so, all the functions are @i{@b{read-only}}
and cannot modify any state.

Importing the subject source code into the tool environment can require edits
that necessarily result in @i{@b{code distortion}}, such that original code
attributes might not be preserved (e.g., line numbers or the byte sizing of
data).

Tools are @i{@b{vendor dependent}}, such that a given tool cannot access Ada
libraries from multiple vendors, or equivalent tools from multiple vendors
cannot access a given Ada library.

Data interchange is @i{@b{not standardized}} among tools, so users can't
configure their own integrated toolsets by choosing from competing or
complementary vendors.

Within a software engineering environment (SEE), Ada semantic information
remains isolated from, and @i{@b{not integrated}} with, other engineering data
present in the environment.
@end{Itemize}


@LabeledSubClause{ASIS}

Historically, only a few Ada vendors provided access to the information
contained in their proprietary Ada program libraries, and each such interface
was unique. Thus began to emerge the need for an open standard that would allow
uniform, vendor-independent access to that information.

Leveraging some informal efforts, the STARS program initiated the development
of the Ada Semantic Interface Specification (ASIS) in 1990; but shortly
thereafter, the activity became unfunded due to the STARS decision to no longer
support standardization efforts. Despite this, several of the involved vendors
(primarily TeleSoft) continued the ASIS work on a volunteer basis. Some time
later, Rational also became an active participant and seeded the draft standard
by contributing their LRM-Interface specification to ASIS.

In 1992, the Ada Board recognized the potential benefits to the Ada community
of an ASIS standard, and recommended that the Ada Joint Program Office (AJPO)
director support "by whatever means possible the development of an ASIS
standard and its submission to ISO/WG9 for publication." The Association for
Computing Machinery (ACM) Special Interest Group on Ada (SIGAda) took on this
important work though volunteer effort in the ASIS Working Group (ASISWG) [2].
The ASISWG developed the interface to ISO/IEC 8652:1987. In December 1993, ASIS
was viable and the director of the AJPO recommended this interface be used by
tools needing information from the Ada program library. The ASISWG then became
focused towards developing ASIS for ISO/IEC 8652:1995. As the ASISWG has no
standardization authority, an ASIS Rapporteur Group (ASISRG) was established on
28 April 1995 by the ISO/IEC JTC1/SC22 WG9 to standardize ASIS as an
International Standard for Ada. ASISWG and ASISRG jointly cooperated to evolve
ASIS as an important interface to the Ada compilation environment.

Like its LRM-Interface predecessor, ASIS defines a set of queries and services
that provide uniform programmatic access to the syntactic and semantic
information contained within Ada library units (i.e., vendor independence). In
addition, for each Ada vendor, ASIS clients are shielded from the evolving
proprietary details that implement the vendor's library representations and
internal forms (i.e., version independence). ASIS is designed for
implementation on a variety of machines and operating systems, while also
supporting the Ada semantic requirements of a wide range of client tools.

ASIS services are essentially primitive, intended to support higher level, more
sophisticated services that address the varied needs of specialized tools.
While ASIS currently operates in a read-only mode, it could eventually be
extended to support some (probably limited) update capability, enabling client
tools to save application-dependent data (e.g., graphical information) within
an existing Ada library. Although an ASIS implementor could readily support
read-write features, members of the safety-critical community have emphasized
the danger of providing a generalized write capability, since this could enable
editing of the internal representation to differ from the original source code.

@leading@;The long-term key is to achieve a critical mass of ASIS
implementations. This will promote a new generation of semantically integrated
Ada tools, which in turn will increase programmer productivity and decrease Ada
development costs. In summary, the availability of ASIS implementations
promises to:

@begin{Itemize}
Stimulate @i{@b{improved quality}} within existing Ada CASE tools; currently,
these tend to be weak in supporting full Ada semantics (e.g., in preserving
renamed entities, resolving overloaded subprogram names, etc.).

Enhance @i{@b{safety and security}} by providing for a new class of powerful
analysis tools that apply formal methods to verify assertions in program code
(e.g., using Pragma Annotate).

Eliminate the need to import Ada source code into secondary Ada compilation
environments, resulting in @i{@b{no distortion}} or loss in the subject code
(e.g., preserving original line numbers and the byte sizing of data).

Maximize @i{@b{interoperability}} between Ada CASE tools and Ada compilation
environments, thus maximizing tool availability.

Enable the @i{@b{data interchange}} of Ada semantic information between
complementary or competing Ada CASE tools, thus maximizing user choices for the
best capabilities of each.

Improve the overall @i{@b{performance}} of Ada CASE tools, by eliminating the
regeneration (and redundant storage) of Ada semantic information that already
exists in Ada libraries.

Facilitate in-house development of informal but powerful ad hoc Ada tools,
providing @i{@b{flexibility}} as needed without funding Ada CASE vendor
specializations.

Promote standardization in software engineering environments (SEEs), enabling
data @i{@b{integration}} of Ada semantic information with other engineering
data present in the environment.

Establish @i{@b{enabling technology}} for new Ada CASE tools, by eliminating
the need for tool vendor investment in proprietary Ada compiler technology;
this will have a major impact on stimulating the development of new code
analysis capabilities.

@end{Itemize}


@LabeledSubClause{Benefits of ASIS standard}

As an International Standard, ASIS benefits the Information Technology
community by facilitating the development of powerful CASE tools portable
amongst the various environments provided by Ada implementors. This portability
can only be achieved through the standardization of ASIS at the international
level. A standardized ASIS promotes the development of powerful tools for the
software engineering environment by providing access to important semantic
information otherwise available only through proprietary interfaces. Further,
ASIS benefits the Information Technology community as a valuable resource for
application development (e.g., decoupling system to system interfaces). The
international standardization of ASIS facilitates the use of this important
capability in the development of system software applications.


@LabeledClause{Design considerations for ASIS}

Why has ASIS taken the form that it has? To guide the assessment of design
decisions for ASIS, the ASIS Working Group has defined a number of design
goals. Most of these embody sound software engineering principles, while others
represent a consensus approach regarding controversial issues. The following
text discusses some of these.

@LabeledSubClause{Design goals}

@leading@;ASIS for ISO 8652:1987, known as ASIS version 1.1.1 [1], was intended
to be used as a de facto standard. For ISO/IEC 8652:1995, it was necessary to
revise ASIS to take into account the changes in the Ada language definition.
When starting the revision of ASIS, the ASIS Working Group agreed upon the
following overall design goals@Defn{Design goals}: the ASIS design should

@begin{Itemize}
Minimize the cost of converting existing ASIS-based applications to the new Ada
standard.

Be simple and uniform with minimal concepts and minimal operations.

Allow for standardization at the international level.
@end{Itemize}

The identification of design goals for ASIS was the major accomplishment of the
19-22 April 1994 ASISWG meeting in Boulder, Colorado. These goals were updated
at the 27-28 June 1994 ASISWG meeting in Reston, Virginia.

The ASIS design goals have been subdivided into the categories shown below. At
a lower level are several design rules identified as important for the
interface specification. Also discussed were several potential goals which were
felt should not be part of the ASIS interface; these are very useful in
providing scope. Several administrative goals were also included.

@leading@b{Design goals (functionality):}
@begin{Itemize}
ASIS should provide all semantic information described by the Ada Reference
Manual.

ASIS should not invent new functionality.

ASIS should provide a read-only interface to the Ada compilation environment.

ASIS should be able to support Ada semantics for a wide range of tools and
application requirements.

Each ASIS interface should be fully defined and unambiguous.

ASIS should be easy to learn and easy to use.

ASIS should use the Ada Reference Manual terminology and style.

ASIS should have a callable Ada interface.

ASIS should provide the ability to traverse the full syntactic structure
represented in the Ada compilation environment for the original text to the
point where the syntax has semantic relevance.

ASIS should not be misleading, be complete as possible, and provide all
information queried (or make obvious to users that they are not getting all the
information, and provide an interface to get that additional information).

@end{Itemize}

@leading@b{Design goals (open systems):}
@begin{Itemize}
ASIS should be implementor independent.

ASIS should be non-proprietary.

ASIS should be designed in such a way that it is implementable by all
implementors.

ASIS should produce semantically consistent results regardless of the
implementor.

ASIS should increase portability of tools across Ada compilers.

ASIS should make Ada compilation environments Open Systems and Reusable
Components for use by other tools.

ASIS should simplify semantic information retrieval by hiding implementation
dependent aspects of Ada environments.
@end{Itemize}

@leading@b{Design goals (migration of ASIS for ISO 8652:1987 to ASIS for
ISO/IEC 8652:1995):}

@begin{Itemize}
ASIS should ensure that ASIS-based tools are convertible to new ASIS.

ASIS should minimize the cost of converting ASIS tools.
@end{Itemize}

@leading@b{Design goals (miscellaneous):}
@begin{Itemize}
ASIS should be efficient.

ASIS should be well engineered.

ASIS documentation should be in ISO format.

ASIS should allow for standardization at the international level.

ASIS should be written to not preclude future extensions.

ASIS should be designed to be simple and uniform with minimal concepts and
minimal operations.
@end{Itemize}

@leading@b{Design rules:}
@begin{Itemize}
ASIS should be as strongly typed as possible.

ASIS should declare its own basic types, and rename (or hide) all implementor
types and operations, for tool portability.

ASIS should use exceptions to report failures and errors; ASIS should
supplement exceptions with status functions.

ASIS should allow only ASIS exceptions to escape the interface.

ASIS should supplement weak compile time type checking of element values by
run-time use of ASIS_Inappropriate_Element Exception.

ASIS should have an existence query for all optional syntax.

ASIS should be able to report a span for every explicit element.
@end{Itemize}

@leading@b{NOT design goals:}
@begin{Itemize}
It is not a goal to support tools having dynamic run-time requirements (e.g.,
symbolic debugger).

It is not a goal to define the semantics of the interface in the face of
simultaneous library updates.
@end{Itemize}

@leading@b{Administrative goals:}

@begin{Itemize}
Resolve issues by consensus rather than simple majority.

Keep the effort scaled down so it is possible and achievable.
@end{Itemize}


@LabeledSubClause{Major changes from ASIS for ISO 8652:1987}

@leading@;The main changes in the ASIS definition are as follows:

@begin{Itemize}
New queries were added for the new language features, such as child units and
protected types, and old ones were revised to take into account language
changes.

Some queries of ASIS were aggregated, in order to minimize their total number.

Whereas ASIS for ISO 8652:1987 was defined as an interface to a program
library, ASIS is now an interface to an Ada compilation environment, following
the new ideas of Ada about the program structure and the compilation process.

The set of package specifications defining ASIS was reorganized into a
hierarchy of child units.

Terminology was revised and comments in the ASIS package specifications were
reworded to make them consistent with Ada.
@end{Itemize}


@LabeledSubClause{Essence of Ada and ASIS}

There are some fundamental differences between a language definition (i.e., the
definition of Ada) and the interface definition of a tool, such as ASIS.
Clearly the Ada language definition, as described in the Ada Reference Manual,
and ASIS are different in nature and have different aims.

Ada defines the notion of a legal compilation unit, the notion of a legal
complete partition and the dynamic semantics of a complete partition. It also
defines some aspects of the process of compiling a compilation unit and some
properties of the results of this process.

On the other hand, ASIS is more like the requirements definition for a tool
which provides information about a set of Ada software components, which may or
may not make up a complete partition. Moreover, as a tool, ASIS provides
management facilities which go beyond what is needed in a language definition
(e.g., ASIS needs terminology for designating the then-part and the else-part
of an if statement).

Despite these differences in nature, the terminology used in Ada and ASIS
should be the same whenever possible. Clearly, there should be no
contradictions. Closeness in terminology eases understanding of ASIS by those
who already know Ada, saves documentation effort, and is justified by improved
maintenance, since every correction in Ada can impact ASIS.

@leading@;Nevertheless, ASIS needs some new terms. Here are some cases:

@begin{Itemize}
To cut down the number of queries, ASIS sometimes needs to aggregate
Ada-defined syntactic categories.

To walk through a heterogeneous data structure, such as a list of compilation
units and configuration pragmas, ASIS needs to introduce a unifying concept for
item types in the structure.

Finally, some concepts in Ada are vaguely defined compared to the needs of
ASIS. It can then be better to introduce a new name rather than restrict the
meaning of the Ada concept. The Ada compilation environment and ASIS context
concepts can serve as an example here.
@end{Itemize}


@LabeledClause{Major issues regarding ASIS}

How has the Ada language revision affected ASIS? The following text presents a
discussion on a variety of issues that have arisen in making this transition.

@LabeledSubClause{Ada environment and compilation units}

One of the major points in the revision of ASIS was the replacement of the idea
of an interface to an Ada program library in ASIS 1.1.1 by an interface to an
Ada compilation environment. This change has in fact some farther-reaching
consequences than it seems at a first look.

The Ada Reference Manual 10.1.4(1) says: "Each compilation unit submitted to
the compiler is compiled in the context of an environment declarative_part (or
simply, an environment@Defn{Environment}), which is a conceptual
declarative_part that forms the outermost declarative region of the context of
any compilation. At run time, an environment forms the declarative_part of the
body of the environment task of a partition". And the next paragraph says: "The
declarative_items of the environment are library_items appearing in an order
such that there are no forward semantic dependencies." Reference Manual
10.1.4(5) then says: "When a compilation unit is compiled, all compilation
units upon which it depends semantically shall already exist in the
environment;..."

At a first look, there seems to be a contradiction between Reference Manual
10.1.4(1,2) which defines an environment as a declarative_part and says that an
environment contains library_items, but not compilation_units, and Reference
Manual 10.1.4(5) which speaks about the existence of compilation units in the
environment.

But this apparent "contradiction" is resolved in Reference Manual 10.1.1(9),
which says: "The term compilation unit is used to refer to a compilation_unit.
When the meaning is clear from context, the term is also used to refer to the
library_item of a compilation_unit or to the proper_body of a subunit (that is,
the compilation_unit without the context_clause and without the separate
(parent_unit_name))."

This means the Ada Reference Manual defines compilation_unit as a syntactical
category, and at the same time it defines "compilation unit" (without
underscore and typed in regular font) as another technical term, which can
denote either one of the syntactical categories "compilation_unit",
"library_item" or "proper_body". ASIS is aware of this subtle difference and
consistently use the term "compilation unit". In ASIS "Compilation_Unit" is the
name of a type. Objects of this type are always compilation_units (except if
they are nonexistent, unknown, or null).

The purpose of the Ada concept of an environment is threefold. It allows Ada to
define what is a legal compilation unit, what is a legal partition and what is
the behavior of a partition at run-time. Strictly speaking, an Ada environment
contains only library_items; indeed, context clauses do not make sense inside
of a declarative part, and subunits are enclosed in their parent unit in place
of the corresponding stub.


@LabeledSubClause{ASIS context and inconsistency}

ASIS has to deal with sets of compilation units. To avoid any confusion, it
therefore introduces a new term, the concept of a context@Defn{Context}, which
stands for a set of compilation_units maintained by an Ada compilation system.
A context has a state, consisting of the compilation units belonging to it, and
the configuration pragmas applying to them, but also depending on the history
of compiler runs, or other actions taken by the compilation system. Thus, the
ASIS concept of a context resembles closely an Ada environment when used as a
dynamic concept like in the Ada Reference Manual 10.1.4(3), which says: "The
mechanisms for creating an environment and for adding and replacing compilation
units within an environment are implementation-defined."

Ada does not deal with arbitrary sets of compilation units as a whole, but ASIS
does.

@leading@Chg{Version=[2],New=[As an example, suppose],Old=[EXAMPLE Suppose]} we
first compile a package P, then a procedure A withing P, then we modify P,
recompile it, and finally compile the procedure B which also is withing P; we
also suppose that all compiler runs are successful (i.e., do not detect
compilation errors):@ChgNote{This is not an example, in the sense that the
important text of the subclause depends on the example. Examples aren't
supposed to have any effect on the normative wording.}

@begin{Example}
compile
   @key[package] P @key[is] ...
compile
   @key[with] P;
   @key[procedure] A ...

edit
   @key[package] P and change it to P'

compile  -- reference to P'
   @key[package] P @key[is] ...
compile
   @key[with] P;  -- reference to P'
   @key[procedure] B ...
@end{Example}

As far as the legality of compilation units is the concern, the environments
can be distinguished as: P alone, P together with A, P' alone, and P' together
with B. It could well be that compiling A with P' yields a compilation error.
Nevertheless, all these environments contain only consistent unit sets
following Reference Manual 10.1.4 (5), which says: "When a compilation unit is
compiled, all compilation units upon which it depends semantically shall
already exist in the environment; the set of these compilation units shall be
consistent in the sense that the new compilation unit shall not semantically
depend (directly or indirectly) on two different versions of the same
compilation unit, nor on an earlier version of itself."

Only when the results of these compilations are used to build a partition, the
inconsistency of depending on two versions for P can be detected by the
implementation following Reference Manual 10.2 (27), which says: "The
implementation shall ensure that all compilation units included in a partition
are consistent with one another, and are legal according to the rules of the
language."

As a conclusion, we can say that Ada does not deal with the above sequence of
compiler runs as a whole, and especially does not bother about possible
"inconsistencies".

On the contrary, it is clearly desirable for ASIS to deal with the previously
described situation as a whole, so that tools can be built capable of analyzing
dependencies between compilation units and possible inconsistencies in the
state of the "context". It is the idea that an ASIS context can refer to such a
situation in some implementation-dependent way. As defined now, in no case ASIS
will provide access to both versions of P, even indirectly by references from A
and B. Depending on the ASIS implementation, the compilation unit A may or may
not be part of a context. If it is, then ASIS reports that A is obsolete (e.g.,
by saying that A is inconsistent with P). If issued, queries about A can raise
exceptions or produce wrong results. This does not preclude some ASIS
implementation providing correct answers to structural queries about A (which
are about syntax only).

As we have just seen, ASIS introduces the idea of inconsistencies within a
context. An inconsistency is always expressed in terms of pairs of compilation
units, the second depending on the first: (A depends on P). An inconsistency is
reported if the latest compiler runs for A and P used different versions of P,
or different versions of any other unit upon which P depends.

This is perhaps the place to speak about the concept of a version of a
compilation unit. Even though Ada uses the term in Reference Manual 10.1.4 (5),
it does not define it. On the contrary, ASIS does not use it at all. ASIS
provides queries for retrieving a library unit having a given name; it is
stated that at most one unit can be retrieved, which means that library units,
but also compilation units, are uniquely identified by their fully expanded
names, and that the concept of a version does not make sense in ASIS.

The Ada environment and ASIS context concepts, though similar, are nevertheless
different, and the same holds for the concept of consistency. However, and even
if technically perhaps not entirely correct, the idea of ASIS being an
interface to an Ada compilation environment reflects intuitively quite well the
very nature of ASIS.


@LabeledSubClause{Implicit declarations}

Sometimes a concept is missing as such in ASIS, but it can be rebuilt
indirectly (e.g., implicit declarations of a declarative region can only be
retrieved indirectly). Ada distinguishes between explicit and implicit
declarations (Reference Manual 3.1 (5)): "A declaration is a language construct
that associates a name with (a view of) an entity. A declaration can appear
explicitly in the program text (an explicit
declaration@Defn{Explicit declaration}), or can be supposed to
occur at a given place in the text as consequence of the semantics of another
construct (an implicit declaration@Defn{Implicit declaration})."
In many ways, Ada deals in an uniform way with explicit and implicit
declarations. We will see that this is not true for ASIS.

ASIS defines several queries returning lists of explicit declarations from
different kinds of declarative regions (i.e., Body_Declarative_Items,
Visible_Part_Declarative_Items, Private_Part_Declarative_Items, and
Block_Declarative_Items).

@leading@;ASIS has several approaches to deal with implicit declarations:
@begin{Enumerate}
A first case is @b{type declarations}. To retrieve the implicit declarations
associated with a type, ASIS provides queries to get all the predefined and
inherited subprograms associated with it, and also its implicit components,
when it is a record.

A second case is @b{statement_identifiers}. Labels, block names and loop names
are statement_identifiers. Reference Manual 5.1(12) says that "For each
statement_identifier, there is an implicit declaration (with the specified
identifier) at the end of the declarative_part of the innermost block_statement
or body that encloses the Statement_Identifier." ASIS does not provide a query
listing the statement_identifiers implicitly declared at the end of a
declarative_part. The only way to find out about statement_identifiers is to
access the statements and then to retrieve the identifiers attached to them by
means of the queries Label_Names and Statement_Identifier.
@end{Enumerate}

The two cases show that it is not easy for an ASIS-based application to
retrieve all implicit declarations of a given declarative region.

Nevertheless, the approach taken by ASIS is justifiable because compilers
usually do not store the implicit statement_identifier declarations in their
intermediate representations, and it would therefore be a major burden for an
ASIS implementation to yield them as a result of a query.


@LabeledSubClause{Abstract "=" for private types}

An Is_Equal function has been defined for the private types Context (Clause
@RefSecNum{type Context}), Element (Clause @RefSecNum{type Element}),
Compilation_Unit (Clause @RefSecNum{type Compilation_Unit}),
Container (Clause @RefSecNum{type Container}),
Line (Clause @RefSecNum{type Line}), Id (Clause @RefSecNum{type Id}),
Record_Component (Clause @RefSecNum{type Record_Component}), and
Array_Component (Clause @RefSecNum{type Array_Component}). An abstract
function "=" has been defined for
each private type because the semantics for the Is_Equal function for these
types may differ from the semantics of the default "=" operator. This prevents
improper use of the "=" operator.@Defn2{Term=[Equality],Sec=[Abstract]}


@LabeledSubClause{Usage names and expressions}

To minimize the number of queries, ASIS sometimes unifies several concepts. For
usage names, Ada makes a difference between names which are and which are not
expressions. ASIS considers all usage names as expressions.

For instance, the name of the procedure in a procedure call statement is
considered to be an expression. Only when trying to apply to such an
"expression" a query specific to an expression, yielding for example the type
of the expression, ASIS will say that it is not an expression by returning a
Nil_Element.

This approach allows ASIS to have a smaller element kinds classification. ASIS
for ISO 8652:1987 already worked this way, and compatibility is another reason
for keeping the approach.


@LabeledSubClause{Select alternative}

@leading@;Let's start by recalling the Ada syntax of a select_alternative
(Reference Manual 9.7.1 (4-7)):

@begin{Example}
select_alternative ::= accept_alternative
                     | delay_alternative
                     | terminate_alternative

accept_alternative ::= accept_statement [sequence_of_statements]

delay_alternative ::= delay_statement [sequence_of_statements]

terminate_alternative ::= @key[terminate];
@end{Example}

When analyzing a selective_accept, ASIS sees the select_alternative just as a
sequence_of_statements, and does not distinguish between its three possible
forms. The advantage of this approach shows up when the ASIS-application
programmer needs to deal with statements in a uniform way, for instance in a
recursive descent. The price to pay is that she has to do her own analysis when
the distinction is needed, by examining the first statement in the sequence.


@LabeledSubClause{Attribute definition clauses}

@leading@;In Reference Manual 13.3 (2), Ada says that an
attribute_definition_clause is defined as:
@begin{Example}
attribute_definition_clause ::=
             @key[for] local_name'attribute_designator @key[use] expression;
           | @key[for] local_name'attribute_designator @key[use] name;
@end{Example}

@leading@;In Reference Manual 4.1, Ada says that a name may be an
attribute_reference, which then is formally defined in Reference Manual 4.1.4
(2) by:
@begin{Example}
attribute_reference ::= prefix'attribute_designator
@end{Example}

@leading@;ASIS uses this latter definition to simplify the
attribute_definition_clause definition:
@begin{Example}
attribute_definition_clause ::=
             @key[for] attribute_reference @key[use] expression;
           | @key[for] attribute_reference @key[use] name;
@end{Example}

From an attribute_definition_clause, ASIS therefore retrieves an
attribute_reference, which can be further decomposed into the prefix and the
attribute_designator parts.


@LabeledSubClause{Configuration pragmas}

According to Reference Manual 10.1.5 (8), the purpose of configuration pragmas
is: "They are generally used to select a partition-wide or system-wide option."
The same paragraph says about their placement that "[T]hey shall appear before
the first compilation_unit of a compilation.", and later on about their "scope"
that "The pragma applies to all compilation_units appearing in the compilation,
unless there are none, in which case it applies to all future compilation_units
compiled into the same environment".@Defn{Configuration pragmas}

ASIS does not use the notion of a compilation ordering. The first reason for
this is to keep ASIS as small as possible. The second justification is that
compilers usually do not keep track of compilations, but only of their effects.

However, some ASIS-based applications could need information about
configuration pragmas. ASIS therefore introduces a special kind of compilation
unit, called A_Configuration_Compilation, containing only configuration
pragmas. By retrieving the content of A_Configuration_Compilation, it is
possible to know the configuration pragmas which are in effect for the next
compiler run within this context.

It is important to notice here that ASIS provides also a query to retrieve all
the pragmas which were in effect when a given compilation unit was compiled,
including both configuration pragmas and pragmas specific to the unit; clearly,
some of the former could differ from those found currently in
A_Configuration_Compilation.


@LabeledSubClause{Queries with additional context parameter}

Several queries, including Corresponding_Declaration and Corresponding_Body,
are overloaded with an additional parameter, Context. The additional queries
are there for flexibility and power, if an ASIS implementation can take
advantage of it.

Some ASIS implementations may be able to support multiple open Contexts at the
same time. Tools that make use of more than one Context could be severely
limited if they do not have the control provided by the Context parameter. An
example of such a tool is one that compared two contexts, identifying units
appearing in both and ensuring their semantic dependent units were consistent,
perhaps for some configuration management purpose.

The Corresponding_Declaration in a different context should always be the
declaration upon which the body depends semantically in that context, not just
an unrelated declaration of the same name.

ASIS implementations that only allow one open Context can implement these pairs
of queries in tandem, with one simply calling the other and providing the
current Context as a parameter.


@LabeledSubClause{Ids}

Ids provide a mechanism to refer to the same ASIS element from one tool
analysis to a separate tool analysis. The type Id is a way to identify a
particular element (i.e., a unique reference to an Element) which is efficient
and persistent as long as the environment is not recompiled. Ids allow a tool to
create a database of visited elements to support a variety of tool
requirements. An Id can be passed from one ASIS analysis tool to another,
facilitating the sharing of analysis thus eliminating the requirement to
recompute the analysis for the same element. It could also be used to generate
hyper-linked analysis reports and support the pipelining of analysis amongst
integrated tools. Ids can be written to files. Ids can be read from files and
converted into an Element value with the use of a suitable open Context.@Defn{Ids}


@LabeledSubClause{Data decomposition}

The optional Data_Decomposition package was developed to support the special
needs of applications in decoupling system software interfaces. Such interfaces
could support communications between two platforms or communications between
two subsystems within a system. Typically an interface between two software
systems is accomplished through a rigorously defined interface. This rigorous
interface is beneficial since it defines the expected communications protocol
between the two computers. However, this interface is a serious impediment to
upgrading a system when only part of the system is to be upgraded. The new
system typically requires a new interface to support the new capabilities.
Hence software on both sides of the interface needs to be upgraded to enhance
the system.

Data_Decomposition allows the upgrade of part of a critical system without
changing software in the rest of the system. Hence it provides significant
flexibility for the life-cycle of a system. Decoupling the interface between
two software systems can be accomplished when each system has access to
semantic information in the other system's Ada compilation environment. This
semantic information describes objects sent across the interface and could
serve as a means to identify its operations. Data_Decomposition was
specifically developed to support a one-way interface between a United States
Air Force satellite and the ground station's message analysis (delog)
processing.

The initial delogger implementation required a rigorous interface protocol and
specialized processing for each possible message. This was expensive from a
development perspective. The time required to build the "types dictionary" took
704 hours for each build, of which there were many.

@leading@;Their practical solution was to develop a generalized processor,
capable of interpreting any message type sent across the interface. This
required knowledge of the semantics of data in the message. This @i{Universal
Delogger} was built using ASIS to analyze messages sent for data analysis [13].
The process was:
@begin{Enumerate}
Implement both the satellite software and the ground software in Ada;

Send each message from the satellite to the ground station as a byte-stream of
application data with a header: the data based on an Ada record type definition
of heterogeneous composite types; and the header an identifier for this Ada
record definition.

Use the ASIS interface to obtain the Ada record type information associated
with the message header.

Use the ASIS interface to map (decompose) the byte-stream into the appropriate
Ada record type.

Decode the message for analysts and analytical processing based on the
composite types in the Ada record. The named components with their values are
then presented to an operator.
@end{Enumerate}

Using this approach, the ground station can perform generalized processing for
any message from any satellite providing it includes an ASIS interface to the
Ada compilation environment that developed the satellite software.
Developmental cost benefit: Many, but most notable: each major build had to
rebuild the message dictionary; the time required to build this "types
dictionary" was reduced from 704 hours to 2 hours for each rebuild. Also
notable is the ground station can process messages from newer satellite systems
without any changes in the software on the ground station, even though message
content is changed.


@LabeledClause{Conclusion}

The Ada language definition and ASIS have different aims. When taking this into
account, it can be said that ASIS follows, overall, as much as possible the
syntax structure of Ada; that it uses Ada-defined terms consistently; and it
does not introduce unnecessarily new terms and concepts. Clearly, ASIS has a
need for unifying concepts (e.g., for traversing heterogeneous lists), and it
introduces them quite carefully and advisedly. Considering that ASIS users and
ASIS implementors are both interested in achieving stability of the ASIS
definition as soon as possible, and that ASIS has already proved its
effectiveness for developing useful tools, the ASIS Working Group and the ASIS
Rapporteur Group believe the ASIS definition is now ready for standardization.


@LabeledClause{Acronyms}

@table{Columns=[2],Alignment=[Allleft],FirstColWidth=[1],LastColWidth=[4],
NoBreak=[F],Border=[F],SmallSize=[F],Caption=[],Headers=[],
Body=[@b{ACM}@\Association for Computing Machinery
@b{ACVC}@\Ada Compiler Validation Capability
@b{AJPO}@\Ada Joint Program Office
@b{ANSI}@\American National Standards Institute
@b{API}@\Application Program Interface
@b{APSE}@\Ada Programming Support Environment
@b{ASCII}@\American National Standard Code for Information Interchange
@b{ASIS}@\Ada Semantic Interface Specification
@b{ASISRG}@\ASIS Rapporteur Group
@b{ASISWG}@\ASIS Working Group
@b{CASE}@\Computer-Aided Software Engineering
@b{CORBA}@\Common Object Request Broker Architecture (OMG)
@b{COTS}@\Commercial, Off-The-Shelf
@b{CPU}@\Central Processing Unit
@b{DIANA}@\Descriptive Intermediate Attributed Notation for Ada
@b{DII}@\Dynamic Interface Invocation
@b{IDL}@\Interface Definition Language
@b{IEC}@\International Electrotechnical Commission
@b{ISO}@\International Standards Organization
@b{LRM}@\Language Reference Manual (Ada)
@b{OMG}@\Object Management Group
@b{ORB}@\Object Request Broker
@b{SEE}@\Software Engineering Environment
@b{SIGAda}@\Special Interest Group on Ada
@b{STARS}@\Software Technology for Adaptable, Reliable Systems]}
