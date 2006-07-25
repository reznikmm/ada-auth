@Part(frontmatter, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/intro.mss,v $}
@comment{$Revision: 1.1 $ $Date: 2006/07/25 04:56:43 $}

@UnNumberedSection(Introduction)

The Ada Semantic Interface Specification (ASIS) is an interface between an Ada
environment (as defined by ISO/IEC 8652:1995) and any tool requiring
information from it. An Ada environment includes valuable semantic and
syntactic information. ASIS is an open and published callable interface which
gives CASE tool and application developers access to this information. ASIS has
been designed to be independent of underlying Ada environment implementations,
thus supporting portability of software engineering tools while relieving tool
developers from needing to understand the complexities of an Ada environment's
proprietary internal representation.

Examples of tools that benefit from the ASIS interface include: automated code
monitors, browsers, call tree tools, code reformators, coding standards
compliance tools, correctness verifiers, debuggers, dependency tree analysis
tools, design tools, document generators, metrics tools, quality assessment
tools, reverse engineering tools, re-engineering tools, style checkers, test
tools, timing estimators, and translators.

The word "may" as used in this International Standard consistently means "is
allowed to" (or"are allowed to"). It is used only to express permission, as in
the commonly occurring phrase "an implementation may"; other words (such as
"can," "could" or "might") are used to express ability, possibility, capacity,
or consequentiality.

The ASIS interface consists of a set of types, subtypes, and subprograms which
provide a capability to query the Ada compilation
environment@Defn{compilation environment} for syntactic and
semantic information. Package @b{Asis} is the root of the ASIS interface. It
contains common types used throughout the ASIS interface. Important common
abstractions include Context@Defn{Context}, Element@Defn{Element}, and
Compilation_Unit@Defn{Compilation_Unit}. Type Context helps
identify the compilation units considered to be analyzable as part of the Ada
compilation environment. Type Element is an abstraction of entities within a
logical Ada syntax tree. Type Compilation_Unit is an abstraction for Ada
compilation units. In addition, there are two sets of enumeration types called
Element Kinds and Unit Kinds. Element Kinds are a set of enumeration types
providing a mapping to the Ada syntax. Unit Kinds are a set of enumeration
types describing the various kinds of compilation units.

All ASIS subprogram interfaces are provided using child packages. Some child
packages also contain type and subtype interfaces local to the child package.

The child package Asis.Implementation@Defn{Implementation} provides queries
to initialize, finalize,
and query the error status of the ASIS implementation. The child package
Asis.Ada_Environments@Defn{Ada_Environments} encapsulates a set of queries that
map physical Ada compilation@Defn{compilation environment} and program execution
environments to logical ASIS environments.

The child package Asis.Compilation_Units@Defn{Compilation_Units} defines
queries that deal with
compilation units@Defn{compilation units} and serves as the gateway between Compilation_Units,
Elements, and Ada_Environments. The child package
Asis.Compilation_Units.Times@Defn{Compilation_Units.Times}
encapsulates the time related functions used within ASIS. The child package
Asis.Compilation_Units.Relations@Defn{Compilation_Units.Relations} encapsulates
semantic relationship concepts used in ASIS.

The child package Asis.Elements@Defn{Elements} defines general Element queries
and queries for pragmas. It provides information on the element kinds for
further semantic analysis.

@Leading@;The child package Asis.Iterator@Defn{Iterator} provides a mechanism
to perform an iterative traversal of a logical syntax tree. During the syntax
tree traversal, ASIS can analyze the various elements contained within the
syntax tree. ASIS can provide the application additional processing via generic
procedures, which are instantiated by the application. These additional
processing queries decompose as ASIS elements from the logical Ada semantic
tree. Queries are provided in the child packages: Clauses, Declarations,
Definitions, Expressions, and Statements.
@begin{itemize}

child package Asis.Clauses@Defn{Clauses} @en Defines queries dealing with
context clauses and representation clauses.

child package Asis.Declarations@Defn{Declarations} @en Defines queries dealing
with Ada declarations.

child package Asis.Definitions@Defn{Definitions} @en Defines queries dealing
with the definition portion of Ada object, type, and subtype declarations.

child package Asis.Expressions@Defn{Expressions} @en Defines all queries
dealing with Ada expressions.

child package Asis.Statements@Defn{Statements} @en Defines queries dealing with
Ada statements.

@end{itemize}

The child package Asis.Text@Defn{Text} encapsulates a set of operations to
access the text of ASIS elements. It defines the operations for obtaining
compilation@Defn{compilation} text spans, lines, and images of elements.

The child package Asis.Ids@Defn{Ids} provides a mechanism to efficiently
reference ASIS elements in a persistent manner.

To support portability amongst a variety of implementors' compilation
environments@Defn{compilation environment}, certain types and constants have
been identified as implementation-defined@Defn{implementation-defined}.

The child package Asis.Errors@Defn{Errors} defines the kinds of errors. The
exceptions that can be raised across the ASIS interface are defined in the
child package Asis.Exceptions@Defn{Exceptions}.

@Leading@;The interface supports one optional child package and its single
child package:

@begin{Itemize}

child package Asis.Data_Decomposition@Defn{Data_Decomposition} @en The
interface also includes an optional capability to decompose data values using
the ASIS type information and portable data stream, representing a data value
of that type.

@end{Itemize}
