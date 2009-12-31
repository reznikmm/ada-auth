@Part(frontmatter, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/intro.mss,v $}
@comment{$Revision: 1.8 $ $Date: 2009/12/23 06:58:59 $}

@UnNumberedSection(Introduction)

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
The Ada Semantic Interface Specification (ASIS) is an interface between an Ada
environment (as defined by @Chg{Version=[2], New=[ISO/IEC 8652:1995(E)], Old=[ISO/IEC 8652:1995]}) and any tool
requiring
information from it. An Ada environment includes valuable semantic and
syntactic information. ASIS is an open and published callable interface which
gives @Chg{Version=[2],New=[],Old=[CASE ]}tool and application
developers access to this information. ASIS has
been designed to be independent of underlying Ada environment implementations,
thus supporting portability of software engineering tools while relieving tool
developers from needing to understand the complexities of an Ada environment's
proprietary internal representation.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Examples of tools that benefit from the ASIS interface include: automated code
monitors, browsers, call tree tools, code
@Chg{Version=[2],New=[reformatters],Old=[reformators]},
coding standards compliance tools, correctness verifiers,
@Chg{Version=[2],New=[],Old=[debuggers, ]} dependency tree analysis tools,
design tools, document generators, metrics tools, quality assessment tools,
reverse engineering tools, re-engineering tools, style checkers, test tools,
timing estimators, and translators.

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[The word "may" as used in this International
Standard consistently means "is allowed to" (or "are allowed to"). It is used
only to express permission, as in the commonly occurring phrase "an
implementation may"; other words (such as "can," "could" or "might") are used to
express ability, possibility, capacity, or consequentiality.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1],ARef=[SI99-0053-1]}
@ChgAdded{Version=[2],Text=[The ASIS interface consists of a set of types,
subtypes, and subprograms which provide a capability to query the Ada
environment for syntactic and semantic
information@Defn{environment}. The ASIS interface can be separated
into two related subsystems, the syntactic subsystem and the
semantic subsystem.@Defn{syntactic subsystem}@Defn2{Term=[subsystem],Sec=[syntactic]}@Defn{semantic subsystem}@Defn2{Term=[subsystem],Sec=[semantic]}
The @i{syntactic subsystem} provides the ability to traverse a program based on the
syntactic (textual) structure of the program, while the @i{semantic subsystem}
provides the ability to traverse the program based on the semantics (meaning) of
the program. Interconnections are provided to allow moving seamlessly between
the subsystems.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[Package @b{Asis} is the root of the ASIS interface. It
contains common types used throughout the ASIS interface. All ASIS subprogram
interfaces are provided using child packages. Some child packages also contain
type and subtype interfaces local to the child package. The various child
packages of ASIS provide queries about the properties of the entities and other
elements that make up an Ada program. Queries are provided to learn about
declarations, expressions, statements, compilation units, and so on.]}


@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1],ARef=[SI99-0053-1]}
@Chg{Version=[2],New=[],Old=[The ASIS interface consists of a set of types,
subtypes, and subprograms which provide a capability to query the Ada compilation
environment@Defn{compilation environment} for syntactic and
semantic information. Package @b{Asis} is the root of the ASIS interface. It
contains common types used throughout the ASIS interface. ]}Important common
abstractions @Chg{Version=[2],New=[in the syntactic subsystem ],Old=[]}include
Context@Defn{Context}, Element@Defn{Element}, and
Compilation_Unit@Defn{Compilation_Unit}. @Chg{Version=[2],New=[The type],Old=[Type]}
Context helps identify the compilation units considered to be analyzable as part
of the Ada @Chg{Version=[2],New=[],Old=[compilation ]}environment.
@Chg{Version=[2],New=[The type],Old=[Type]}
Element is an abstraction of entities within a logical Ada syntax tree.
@Chg{Version=[2],New=[The type],Old=[Type]} Compilation_Unit is an abstraction
for Ada compilation units. In addition, there are two sets of enumeration types
@Chg{Version=[2],New=[covering various kinds of elements and units, such as
Expression_Kinds],Old=[called Element Kinds and Unit Kinds]}.
@Chg{Version=[2],New=[The element kinds],Old=[Element Kinds]} are a set of
enumeration types providing a mapping to the Ada syntax.
@Chg{Version=[2],New=[The unit kinds],Old=[Unit Kinds]} are a set of
enumeration types describing the various kinds of compilation units.

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[All ASIS subprogram interfaces are provided using
child packages. Some child packages also contain type and subtype interfaces
local to the child package.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[A semantic entity in the semantic subsystem is
represented by a View@Defn{View}. Views can represent semantic concepts like Units,
Objects, or Statements, whether or not they exist explicitly in the analyzed
program. Queries are then provided to further decompose or traverse these views
to other related views of the meaning of the program text. In addition, there is
a set of child packages that provide routines to transition back and forth
between the original syntactic element and the semantic view to obtain these
different perspectives on program meaning or content.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[The child package
Asis.Implementation@Defn{Implementation} provides queries to initialize,
finalize, and query the error status of the ASIS implementation. The child
package Asis.Ada_Environments@Defn{Ada_Environments} encapsulates a set of
queries that map physical Ada compilation@Defn{compilation environment} and
program execution environments to logical ASIS environments.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[The child package
Asis.Compilation_Units@Defn{Compilation_Units} defines queries that deal with
compilation units@Defn{compilation units} and serves as the gateway between
Compilation_Units, Elements, and Ada_Environments. The child package
Asis.Compilation_Units.Times@Defn{Compilation_Units.Times} encapsulates the time
related functions used within ASIS. The child package
Asis.Compilation_Units.Relations@Defn{Compilation_Units.Relations} encapsulates
semantic relationship concepts used in ASIS.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[The child package Asis.Elements@Defn{Elements}
defines general Element queries and queries for pragmas. It provides information
on the element kinds for further semantic analysis.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Type=[Leading],Text=[The child package
Asis.Iterator@Defn{Iterator} provides a mechanism to perform an iterative
traversal of a logical syntax tree. During the syntax tree traversal, ASIS can
analyze the various elements contained within the syntax tree. ASIS can provide
the application additional processing via generic procedures, which are
instantiated by the application. These additional processing queries decompose
as ASIS elements from the logical Ada semantic tree. Queries are provided in the
child packages: Clauses, Declarations, Definitions, Expressions, and
Statements.]}
@begin{itemize}

@ChgDeleted{Version=[2],Text=[child package Asis.Clauses@Defn{Clauses} @en
Defines queries dealing with context clauses and representation clauses.]}

@ChgDeleted{Version=[2],Text=[child package Asis.Declarations@Defn{Declarations}
@en Defines queries dealing with Ada declarations.]}

@ChgDeleted{Version=[2],Text=[child package Asis.Definitions@Defn{Definitions}
@en Defines queries dealing with the definition portion of Ada object, type, and
subtype declarations.]}

@ChgDeleted{Version=[2],Text=[child package Asis.Expressions@Defn{Expressions}
@en Defines all queries dealing with Ada expressions.]}

@ChgDeleted{Version=[2],Text=[child package Asis.Statements@Defn{Statements} @en
Defines queries dealing with Ada statements.]}

@end{itemize}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[The child package Asis.Text@Defn{Text}
encapsulates a set of operations to access the text of ASIS elements. It defines
the operations for obtaining compilation@Defn{compilation} text spans, lines,
and images of elements.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[The child package Asis.Ids@Defn{Ids} provides a
mechanism to efficiently reference ASIS elements in a persistent manner.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[To support portability amongst a variety of
implementors' compilation environments@Defn{compilation environment}, certain
types and constants have been identified as
implementation-defined@Defn{implementation-defined}.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[The child package Asis.Errors@Defn{Errors} defines
the kinds of errors. The exceptions that can be raised across the ASIS interface
are defined in the child package Asis.Exceptions@Defn{Exceptions}.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Type=[Leading],Text=[The interface supports one optional
child package and its single child package:]}

@begin{Itemize}

@ChgDeleted{Version=[2],Text=[child package
Asis.Data_Decomposition@Defn{Data_Decomposition} @en The interface also includes
an optional capability to decompose data values using the ASIS type information
and portable data stream, representing a data value of that type.]}

@end{Itemize}

@AddedSubHeading{Version=[2],Changes in this Edition}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[This revised International Standard updates the
edition from 1999.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[The most significant change in this edition was the
addition of the semantic subsystem (see above and Section
@RefSecNum{ASIS Semantic Subsystem}). The new concept of views added by the
semantic subsystem addresses a significant
shortcoming in the 1999 edition of ASIS @en ASIS elements represent entities
that do not have explicit declarations (such as classwide types, inherited
subprograms, and the like) poorly or not at all. The new concept also
simplifies access to properties that are not easily determined from the syntax
of an entity. For instance, a type can become tagged explicitly, by being an
interface, via inheritance in a derived type, or by having a progenitor.
Thus querying Is_Tagged on an appropriate view is much simpler than analyzing
all of those possibilities.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=<A variety of other changes were made
to ASIS to support the 2007 Amendment to Ada [ISO/IEC 8652:1995/AMD 1:2007(E)]. Some of
the more significant changes were:>}

@begin{Itemize}

@ChgAdded{Version=[2],Text=[Added support for newly added constructs,
including tagged incomplete types, null procedures, overriding indicators,
extended returns, exception messages in raise statements, and new predefined
pragmas;]}

@ChgAdded{Version=[2],Text=[Adjusted ASIS to properly support the
object-oriented prefix notation for subprograms;]}

@ChgAdded{Version=[2],Text=[Changed ASIS to be able to support the new aggregate
features such as defaulted elements; and]}

@ChgAdded{Version=[2],Text=[Changed ASIS to use UTF-16 representations, in order
that the full character set of an Ada source program can be represented.]}

@end{Itemize}


@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[In addition, many other changes were
made to clarify and improve the existing standard. These should have minimal
effect on existing ASIS programs, but will be an aid in creating new programs
using the ASIS package interfaces. Important changes include:]}

@begin{Itemize}

@ChgAdded{Version=[2],Text=[Added a query to determine directly if an element
that is a name represents an implicit dereference;]}

@ChgAdded{Version=[2],Text=[Replaced traits with a set of direct query
functions; and]}

@ChgAdded{Version=[2],Text=[Moved obsolescent types and routines to a new annex,
simplifying the presentation to new users and making clear which routines are
not intended to be used in new programs.]}

@end{Itemize}