@Part(general, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/general.mss,v $}
@comment{$Revision: 1.23 $ $Date: 2010/03/09 06:46:51 $}

@PrefaceSection{} @Comment{Go to odd page.}

@begin{Comment}
The following stuff is to get the "INTERNATIONAL STANDARD" title between
two horizontal rules at the top of page 1. (RLB-The following is a hack:
I probably ought to add a style just for this purpose)
@end{Comment}

@thickline

@begin{WideAbove}
@noparanum@leading@tabclear()@tabset(P45)
@Comment{Final
@Swiss<@Grow[@B{INTERNATIONAL STANDARD@\ISO/IEC 15291:@Chg{Version=[1],New=[201x(E)],Old=[1999(E)]}}]>
}@Comment{End final}
@Comment{Draft}
@Comment{@Swiss<@Grow[@B{COMMITTEE DRAFT@\ISO/IEC 15291 CD}]>}
@Swiss<@Grow[@B{WORKING DRAFT 0.8@\ISO/IEC 15291 WD}]>
@Comment{End draft}

@end{WideAbove}

@thickline

@begin{Title}@Comment{Should be 16 points, somehow comes out as 20}
@noparanum@ @*@comment{Some vertical whitespace}

@noparanum@Swiss{@Shrink{@Shrink{@b{Information technology @Em}}}}@*
@Swiss{@Shrink{@Shrink{@b{Programming languages @Em}}}}@*
@Swiss{@Shrink{@Shrink{@b{Ada Semantic Interface Specification (ASIS)}}}}

@noparanum@ @*@comment{Some vertical whitespace}
@end{Title}

@LabeledSectionNoBreak{General}

@LabeledClause{Scope}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1],ARef=[SI99-0047-1]}
The Ada Semantic Interface Specification (ASIS) is an interface between an Ada
environment (as defined by @Chg{Version=[2], New=[ISO/IEC 8652:1995(E)],
Old=[ISO/IEC 8652:1995]})
and any tool requiring
information from this environment. An Ada environment includes valuable
semantic and syntactic information. ASIS is an open and published callable
interface which gives @Chg{Version=[2],New=[],Old=[CASE ]}tool and application
developers access to this
information. ASIS has been designed to be independent of underlying Ada
environment implementations, thus supporting portability of software
engineering tools while relieving tool developers from needing to understand
the complexities of an Ada environment's proprietary internal representation.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Examples of tools that benefit from the ASIS interface include: automated code
monitors, browsers, call tree tools, code
@Chg{Version=[2],New=[reformatters],Old=[reformators]},
coding standards compliance tools, correctness verifiers,
@Chg{Version=[2],New=[],Old=[debuggers, ]}dependency tree analysis
tools, design tools, document generators, metrics tools, quality assessment
tools, reverse engineering tools, re-engineering tools, safety and security
tools, style checkers, test tools, timing estimators, and translators.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0053-1]}
This International Standard specifies the form and meaning of the ASIS
interface to the Ada @Chg{Version=[2],New=[],Old=[compilation ]}environment.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1],ARef=[SI99-0053-1]}
This International Standard is applicable to tools and applications needing
syntactic and semantic information @Chg{Version=[2],New=[from],Old=[in]}
the Ada @Chg{Version=[2],New=[],Old=[compilation ]}environment.

@LabeledSubClause{Extent}

This International Standard specifies:
@begin{Itemize}
The form of the ASIS interface;

Sequencing of ASIS calls;

The permissible variations within this International Standard, and the manner
in which they are to be documented;

Those violations of this International Standard that a conforming
implementation is required to detect, and the effect of attempting to execute a
program containing such violations;
@end{Itemize}

This International Standard does not specify:

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0053-1]}
Semantics of the interface in the face of simultaneous updates to the Ada
@Chg{Version=[2],New=[],Old=[compilation ]}environment.

Semantics of the interface for more than one thread of control.
@end{Itemize}

@begin{AARMOnly}
@ChgAdded{Version=[1],Text=[This annotated version of this
International Standard includes additional information useful to
ASIS implementers and standards writers.]}
@end{AARMOnly}


@LabeledSubClause{Structure}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
This International Standard contains twenty-three @Chg{Version=[2],New=[sections],Old=[clauses]}
and @Chg{Version=[2],New=[six],Old=[four]} annexes.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} 1 is general in nature providing the scope of this International
Standard, normative references, and definitions.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1],ARef=[SI99-0053-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} 2 identifies the ASIS technical
concepts. Here the Ada @Chg{Version=[2],New=[],Old=[compilation ]}environment
to which ASIS interfaces is described. The concept of queries is
presented. The ASIS package architecture is presented.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
The packages that comprise the ASIS International Standard are provided in
@Chg{Version=[2],New=[Section],Old=[Clause]}s @RefSecNum{package Asis} through
@RefSecNum{ASIS Semantic Subsystem}.@Chg{Version=[1],New=[],Old=[ These packages are provided
in the correct compilation order and when presented in electronic format are
compilable.]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]}  @RefSecNum{package Asis}   package Asis

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]}  @RefSecNum{package Asis.Errors}   package Asis.Errors

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]}  @RefSecNum{package Asis.Exceptions}   package Asis.Exceptions

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]}  @RefSecNum{package Asis.Implementation}   package Asis.Implementation

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]}  @RefSecNum{package Asis.Implementation.Permissions}   package Asis.Implementation.Permissions

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]}  @RefSecNum{package Asis.Ada_Environments}   package Asis.Ada_Environments

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1],ARef=[SI99-0058-1]}
@ChgDeleted{Version=[2],Text=[Clause  9   package Asis.Ada_Environments.Containers]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} @RefSecNum{package Asis.Compilation_Units}   package Asis.Compilation_Units

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} @RefSecNum{package Asis.Compilation_Units.Times}   package Asis.Compilation_Units.Times

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} @RefSecNum{package Asis.Compilation_Units.Relations}   package Asis.Compilation_Units.Relations

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} @RefSecNum{package Asis.Elements}   package Asis.Elements

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} @RefSecNum{package Asis.Iterator}   package Asis.Iterator

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} @RefSecNum{package Asis.Declarations}   package Asis.Declarations

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} @RefSecNum{package Asis.Definitions}   package Asis.Definitions

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} @RefSecNum{package Asis.Expressions}   package Asis.Expressions

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} @RefSecNum{package Asis.Statements}   package Asis.Statements

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} @RefSecNum{package Asis.Clauses}   package Asis.Clauses

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} @RefSecNum{package Asis.Text}   package Asis.Text

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} @RefSecNum{package Asis.Ids}   package Asis.Ids

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} @RefSecNum{package Asis.Data_Decomposition (optional)}   package Asis.Data_Decomposition (optional package)

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Section],Old=[Clause]} @RefSecNum{ASIS Semantic Subsystem}   package @Chg{Version=[2],New=[Asis.Views,@*
Asis.Program_Units,@*
Asis.Subtype_Views,@*
Asis.Subtype_Views.Elementary,@*
Asis.Subtype_Views.Composite,@*
Asis.Object_Views,@*
Asis.Object_Views.Access_Views,@*
Asis.Profiles,@*
Asis.Callable_Views,@*
Asis.Package_Views,@*
Asis.Generic_Views,@*
Asis.Exception_Views,@*
Asis.Statement_Views,@*
Asis.Declarations.Views,@*
Asis.Definitions.Views, and@*
Asis.Expressions.Views],Old=[Asis.Data_Decomposition.Portable_Transfer]}

@end{Itemize}

The following annexes are informative:

@begin{Itemize}
Annex  A:   Glossary

Annex  B:   ASIS Application Examples

Annex  C:   Miscellaneous ASIS I/O and IDL Approaches

Annex  D:   Rationale

@ChgAdded{Version=[2],Text=[Annex  E:	Summary of ASIS Entities]}
@end{Itemize}

@ChgAdded{Version=[2],Text=[The following annexes are normative:]}

@begin{Itemize}
@ChgAdded{Version=[2],Text=[Annex  F:	Obsolescent Features]}
@end{Itemize}

The major package interfaces visible to ASIS users are identified as clauses
facilitating access from the table of contents.


@Chg{Version=[1],New=[In addition to the basic description of each entity that
makes up the ASIS interface, text of various types is labeled with headers.
],
Old=[The ASIS interface is compilable. Consequently, Sentinels
have been used to mark portions of the ASIS text with comments appropriate to
an ASIS implementor and an ASIS user.]}

@Chg{Version=[1],New=[The
various headers and their meaning are:],Old=[The sentinels and their meanings are:]}

@begin{ImplPerm}
@Chg{Version=[1],New=[These items],Old=[@b{Implementatiom Permissions} - These comments]}
describe permissions given an implementor when implementing the associated type
or query.
@end{ImplPerm}

@begin{ImplReq}
@Chg{Version=[1],New=[These items],Old=[@b{Implementatiom Requirements} - These comments]}
describe additional requirements for conforming implementations.
@end{ImplReq}

@begin{SingleNote}
@Chg{Version=[1],New=[These items],Old=[@b{Application Note} - These comments]}
describe notes of interest to ASIS applications.
@end{SingleNote}

@begin{Examples}
@ChgAdded{Version=[1],Text=[These items give examples of use of ASIS interfaces.]}
@end{Examples}

@begin{AARMOnly}
@ChgAdded{Version=[1],Type=[Leading],Text=[The Annotated ASIS standard also
includes the following headers:]}
@end{AARMOnly}

@begin{ElementRef}
@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
@Chg{Version=[1],New=[These items],Old=[@b{Element Reference} - These comments]}
mark an element kind reference which acts as a header for those queries that
work on this element kind.@Chg{Version=[1],New=[ The reference includes the
name of a query that can produce the appropriate element kind and the
subclause in the Ada Standard @Chg{Version=[2], New=[], Old=[(ISO/IEC 8652:1995)]} where the kind is defined.],Old=[]}
@end{ElementRef}

@begin{ChildRef}
@Chg{Version=[1],New=[These items],Old=[@b{Child Reference} - These sentinel comments]}
follow @Chg{Version=[1],New=[],Old=[sentinel comments marking ]}element
references @Chg{Version=[1],New=[],Old=[(--ER) ]}and reference child
element queries that decompose the element @Chg{Version=[1],New=[kind(s)
of the element reference],Old=[(--ER)]} into its children.@Chg{Version=[1],
New=[ Typically, the definitions of these queries immediately follow these
items.],Old=[]}@Comment{Just trying to explain what these are for; they're
mysterious to me.}
@end{ChildRef}

@begin{UsageNote}
@Chg{Version=[1],New=[These items],Old=[@b{Application Note} - These comments]}
describe suggested uses, further
analysis, or other notes of interest to ASIS applications.
@end{UsageNote}


@LabeledSubClause{Conformity with this International Standard}


@LabeledSubSubClause{Implementation conformance requirements}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0053-1]}
An @i{ASIS implementation}@defn{ASIS implementation} includes all the hardware
and software that implements the ASIS specification @Chg{Version=[2],
New=[],Old=[for a given Ada implementation ]}and that provides the
functionality required by the ASIS
specification. An @i{ASIS implementor}@defn{ASIS implementor} is a company,
institution, or other group (such as a vendor) who develops an ASIS
implementation. A conforming ASIS implementation shall meet all of the
following criteria:

@begin{enumerate}
The system shall support all required interfaces defined within this
International Standard. These interfaces shall support the functional behavior
described herein. All interfaces in the ASIS specification are required unless
the interface is specifically identified as being optional. The ASIS
specification defines one optional package: Asis.Data_Decomposition.
Asis.@!Data_Decomposition has one child package,
Asis.@!Data_Decomposition.@!Portable_Transfer.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0037-1]}
The system may provide additional facilities not required by this International
Standard. @i{Extensions}@defn{Extension} are non-standard facilities (e.g.,
other library units, non-standard children of standard ASIS library units,
subprograms, etc.) which provide additional information from ASIS types, or
modify the behavior of otherwise standard ASIS facilities to provide
alternative or additional functionality. Nonstandard extensions shall be
identified as such in the system documentation. Nonstandard extensions, when
used by an application, may change the behavior of functions or facilities
defined by this International Standard. The conformance document shall define
an environment in which an application can be run with the behavior specified
by this International Standard. In no case except package name conflicts shall
such an environment require modification of a Basic Conforming or Fully
Conforming ASIS Application. An implementation shall not @Chg{Version=[2],
New=[add any declarations to the visible part of logical packages defined in the
following clauses of this International Standard.],Old=[change package
specifications in this International Standard except by:]}

@begin{InnerItemize}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[Adding @key{with} clauses, pragmas, representation
specifications, comments, and allowable pragmas. Allowable pragmas are those
which do not change the semantics of the interface (e.g., List, Optimize, Page).]}

@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[Replacing instances of the words
<implementation-defined> with appropriate value(s).]}

@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[Adding or changing private parts.]}

@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[Making any other changes that are lexically
transparent to Ada compilers.]}
@end{InnerItemize}

An ASIS implementation shall not raise Program_Error on elaboration of an ASIS
package, or on execution of an ASIS subprogram, due to elaboration order
dependencies in the ASIS implementation.

Except as explicitly provided for in this International Standard,
Standard.Storage_Error is the only exception that should be raised by
operations declared in this International Standard.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
When executed, an implementation of this International Standard shall not be
erroneous, as defined by @Chg{Version=[2], New=[the Ada Standard], Old=[ISO/IEC 8652:1995]}.
@end{Enumerate}

@LabeledSubSubClause{Implementation conformance documentation}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0053-1]}
@Chg{Version=[2],New=[Conformance documentation],Old=[A conformance document]}
shall be available for an implementation claiming
conformance to this International Standard.@Chg{Version=[2],New=[],Old=[ The
conformance document shall have the same structure as this International
Standard, with the information presented in the equivalently numbered clauses,
and subclauses. The conformance document shall not contain information about
extended facilities or capabilities outside the scope of this International
Standard.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0053-1]}
The conformance @Chg{Version=[2],New=[documentation],Old=[document]} shall
contain a statement that indicates the full name, number, and date of the
International Standard that applies. The conformance
@Chg{Version=[2],New=[documentation],Old=[document]} may also list software
standards approved by ISO/IEC or any ISO/IEC member body that are available for
use by a Basic or Fully Conforming ASIS Application. Applicable characteristics
whose documentation is required by one of these standards, or by standards of
government bodies, may also be included.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0053-1]}
The conformance @Chg{Version=[2],New=[documentation],Old=[document]} shall
describe the behavior of the implementation for all implementation-defined
features defined in this International Standard. This requirement shall be met
by listing these features and providing either a specific reference to the
system documentation or providing full syntax and semantics of these features.
The conformance @Chg{Version=[2],New=[documentation],Old=[document]} shall
specify the behavior of the implementation for those features where this
International Standard states that implementations may vary.

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0053-1]}
@ChgDeleted{Version=[2],Text=[No specifications other than those described in
this subclause shall be present in the conformance document.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0053-1]}
@ChgDeleted{Version=[2],Text=[The phrase @i{shall be documented} in this
International Standard means that documentation of the feature shall appear in
the conformance document, as described previously, unless the system
documentation is explicitly mentioned.@Defn{shall be documented}]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0053-1]}
@ChgDeleted{Version=[2],Text=[The system documentation should also contain the
information found in the conformance document.]}


@LabeledSubSubClause{Implementation conformance categories}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
An implementation is required to define all of the subprograms for all of the
operations defined in this International Standard, including those whose
implementation is optional. @i{Required functionality} is the subset of ASIS
facilities which are not explicitly identified in the ASIS standard as
optional.@Defn{Required functionality} @i{Optional functionality} is the subset of ASIS facilities which are
explicitly identified in the ASIS standard as optional which may legitimately
be omitted from a Basic Conforming ASIS implementation.@Defn{Optional functionality}
Optional interfaces
shall be included in any Fully Conforming ASIS implementation, unless stated
otherwise in the ASIS specification.@Chg{Version=[2],New=[],Old=[ An
application that accesses an Ada
environment's semantic tree (e.g., Diana Tree) directly using work-arounds is
not considered to be a conformant application. All Conforming Applications
fall within one of the categories defined below.]}

If an unimplemented feature is used, the exception Asis.ASIS_Failed shall be
raised and Asis.Implementation_Status shall return the value for Error_Kinds of
Not_Implemented_Error.@Defn2{Term=[ASIS_Failed],Sec=[cause]}@Defn2{Term=[Not_Implemented_Error],Sec=[cause]}

@leading@;There are four categories of conforming ASIS implementations:

@Subheading{Basic conforming ASIS implementation}

A Basic Conforming ASIS Implementation is an ASIS implementation supporting all
required interfaces defined within this International Standard.@Defn{Basic conforming ASIS implementation}

@Subheading{Fully conforming ASIS implementation}

A Fully Conforming ASIS Implementation is an ASIS implementation supporting all
required and all optional interfaces defined within this International
Standard.@Defn{Fully conforming ASIS implementation}


@Subheading{Basic conforming ASIS implementation using extensions}

A Basic Conforming ASIS Implementation Using Extensions is an ASIS
implementation that differs from a Basic Conforming ASIS Implementation only in
that it uses nonstandard extensions that are consistent with this International
Standard. Such an implementation shall fully document its extended facilities,
in addition to the documentation required for a Basic Conforming ASIS
Implementation.@Defn2{Term=[Basic conforming ASIS implementation],Sec=[using extensions]}


@Subheading{Fully conforming ASIS implementation using extensions}

A Fully Conforming ASIS Implementation Using Extensions is an ASIS
implementation that differs from a Fully Conforming ASIS Implementation only in
that it uses nonstandard extensions that are consistent with this International
Standard. Such an implementation shall fully document its extended facilities,
in addition to the documentation required for a Fully Conforming ASIS
Implementation.@Defn2{Term=[Fully conforming ASIS implementation],Sec=[using extensions]}



@LabeledSubSubClause{Application conformance categories}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
An @i{ASIS application} is any programming system or any set of software
components making use of ASIS queries to obtain information about any set of
Ada components. All ASIS applications claiming conformance to this
International Standard shall use a Conforming ASIS Implementation with or
without extensions.@Defn{ASIS application}@Chg{Version=[2],New=[ In any case,
an application that accesses an Ada environment directly (other than through
ASIS) is not considered to be a conformant application. All Conforming
Applications fall within one of the categories defined below.],Old=[]}

@Subheading{Basic conforming ASIS application}

A Basic Conforming ASIS Application is an application that only uses the
required facilities defined within this International Standard. It shall be
portable to any Conforming ASIS Implementation.@Defn{Basic conforming ASIS application}


@Subheading{Fully conforming ASIS application}

A Fully Conforming ASIS Application is an application that only uses the
required facilities and the optional facilities defined within this
International Standard. It shall be portable to any Fully Conforming ASIS
Implementation.@Defn{Fully conforming ASIS application}


@Subheading{@ChgDeleted{Version=[2],Text=[Basic conforming ASIS application using extensions]}}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0053-1]}
@ChgDeleted{Version=[2],Text=[A Basic Conforming ASIS Application Using Extensions is an application that
differs from a Basic Conforming ASIS Application only in that it uses
nonstandard, implementation provided, extended facilities that are consistent
with this International Standard. Such an application should fully document its
requirements for these extended facilities. A Basic Conforming ASIS Application
Using Extensions may or may not be portable to other Basic or Fully Conforming
ASIS Implementation Using Extensions.@Defn2{Term=[Basic conforming ASIS application],Sec=[using extensions]}]}


@Subheading{@ChgDeleted{Version=[2],Text=[Fully conforming ASIS application using extensions]}}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0053-1]}
@ChgDeleted{Version=[2],Text=[A Fully Conforming ASIS Application Using
Extensions is an application that differs from a Fully Conforming ASIS
Application only in that it uses nonstandard, implementation provided, extended
facilities that are consistent with this International Standard. Such an
application should fully document its requirements for these extended
facilities. A Fully Conforming ASIS Application Using Extensions may or may not
be portable to other Fully Conforming ASIS Implementation Using
Extensions.@Defn2{Term=[Fully conforming ASIS application],Sec=[using
extensions]}]}


@LabeledDeletedSubClause{Version=[2],Name=[Implementation permissions]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[The ASIS Application Program Interface (API) may be implemented through a
variety of approaches. Approaches permitted by this International Standard are
based on the traditional approach and the client /server approach. These
implementation permissions are depicted in Figure 1 and described below:]}


@ChgNote{SI99-0047-1 remove junk}
@LabeledDeletedSubSubClause{Version=[2],Name=[Traditional approach (permission 1)]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[Traditionally, the ASIS API implementation is
intended to execute on the node containing the implementor's Ada software
engineering environment and the desired Ada compilation environment. Because the
ASIS API interfaces directly, ASIS performs at its best. It is expected that
most ASIS implementors will support this approach as it requires little
additional effort when alternative approaches are supported. In Figure 1, the
client tool using Permission 1 uses the ASIS specification exactly as specified
in this International Standard. ASIS tools and applications are compiled in the
implementor's environment.]}


@ChgNote{SI99-0047-1 remove junk}
@LabeledDeletedSubSubClause{Version=[2],Name=[Client / server approach (permission 2)]}


@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[As an alternative, a client / server approach can
be used to implement the ASIS API. Here the ASIS API is supported by a server;
ASIS client tools can request ASIS services within the supported network.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[Figure 1 identifies four ASIS client tools using
permission 2 capable of interfacing with an ASIS Object Request Broker (ORB)
server. One client tool is written in Ada, one in Java, one in C++, and one in
Smalltalk. The ORB serves as a broker between the client and server on a network
consisting of many nodes. Server location and services are registered with the
ORB. A client needing the services interfaces with the ORB, who brokers the
needed server interface information. The interface between a client and server
is written as an interface specification in the Interface Definition Language
(IDL). IDL is very different from most computer languages; when IDL is compiled,
the interface specification is produced in either Ada, Java, C++, or Smalltalk.
In addition, the necessary artifacts are produced to register the client or
server interface with the ORB.]}


@ChgNote{SI99-0047-1 remove junk}
@LabeledDeletedSubSubClause{Version=[2],Name=[Distributed traditional approach (permission 3)]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[The Ada specification created by the compilation of this ASIS API in IDL is
semantically equivalent to this ASIS standard, but not syntactically identical.
An ASIS Client tool written in Ada interfaces to the ASIS API as specified in
this International Standard. As shown in Figure 1, the ASIS API encapsulates
the ASIS ORB client as generated from the compilation of the ASIS IDL into Ada.
Client tools using either permission 1 or permission 3 are, most likely,
identical. Client tools developed using permission 3 can be developed as plug
and play.]}


@ChgNote{SI99-0047-1 remove junk}
@LabeledDeletedSubSubClause{Version=[2],Name=[ASIS dynamic client approach (permission 4)]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[In addition to using traditional compiled tools
through the client / server interface, ORBs can provide a Dynamic Interface
Invocation (DII) capability where rather general purpose tools can access the
interface dynamically. Shown in Figure 1, such a tool behaves more like a
browser. It accesses the ASIS IDL as registered with the server and browses
through the services provided by the ASIS interface. Use of this capability with
ASIS is extremely cumbersome and manually intensive. However, this provides a
user access to information across the interface that had not been preprogrammed
by a tool.]}

@ChgNote{No picture delete available.}
@Comment{@PictureAlone(Alignment=[Center], Border=[None],
         Height=[596], Width=[492],
         Name=[asis_orb.png],
         Descr=[ASIS implementation permissions])}
@Comment{Image dimensions: Height=[674], Width=[556]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[@b{Figure 1 @em ASIS implementation permissions}]}


@LabeledSubClause{Classification of errors}


@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0058-1]}
ASIS reports all operational errors by raising an exception. Whenever an ASIS
implementation raises one of the exceptions declared in package Asis.Exceptions,
it will @Chg{Version=[2],New=[indicate the cause of the error by setting],
Old=[previously have set]} the values returned by the
Status and Diagnosis queries to indicate the cause of the error]}. The possible
values for Status are indicated @Chg{Version=[2],New=[below],Old=[here along
with suggestions for the associated contents of the Diagnosis string]}.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0055-1]}
@leading@;ASIS applications are encouraged to
@Chg{Version=[2],New=[similarly ],Old=[]}follow
@Chg{Version=[2],New=[the],Old=[this same]} convention
@Chg{Version=[2],New=[of recording],Old=[whenever they explicitly raise
any ASIS exception to always record]} a Status and
Diagnosis prior to @Chg{Version=[2],New=[explicitly ],Old=[]}raising
@Chg{Version=[2],New=[any ASIS],Old=[the]} exception. Values of errors
along with their general meanings are:

@ChgNote{@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0055-1]} - doesn't show on text}
@table{Columns=[2],Alignment=[Allleft],FirstColWidth=[2],LastColWidth=[3],
NoBreak=[F],Border=[F],SmallSize=[F],Caption=[],Headers=[],
Body=[Not_An_Error@Defn{Not_An_Error}@\-- No error is presently recorded
Value_Error@Defn{Value_Error}@\-- Routine argument value invalid
Initialization_Error@Defn{Initialization_Error}@\-- ASIS is uninitialized
Environment_Error@Defn{Environment_Error}@\-- ASIS could not initialize
Parameter_Error@Defn{Parameter_Error}@\-- Bad Parameter given to Initialize
Capacity_Error@Defn{Capacity_Error}@\-- @Chg{Version=[2],New=[Capacity exceeded],Old=[Implementation overloaded]}
Name_Error@Defn{Name_Error}@\-- Context/unit not found
Use_Error@Defn{Use_Error}@\-- Context/unit not use/open-able
Data_Error@Defn{Data_Error}@\-- Context/unit bad/invalid/corrupt
Text_Error@Defn{Text_Error}@\-- The program text cannot be located
Storage_Error@Defn{Storage_Error}@\-- Storage_Error @Chg{Version=[2],New=[detected],Old=[suppressed]}
Obsolete_Reference_Error@Defn{Obsolete_Reference_Error}@\-- Semantic reference is obsolete
Unhandled_Exception_Error@Defn{Unhandled_Exception_Error}@\-- Unexpected exception @Chg{Version=[2],New=[detected],Old=[suppressed]}
Not_Implemented_Error@Defn{Not_Implemented_Error}@\-- Functionality not implemented@Last
Internal_Error@Defn{Internal_Error}@\-- Implementation internal failure]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0053-1]}
@ChgDeleted{Version=[2],Text=[Diagnostic messages may be more specific.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0037-1],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Type=[Leading],Text=[A set of
exceptions shall be raised for the following
circumstances:]}@Comment{ somewhat of a fake to a get a
conditional @leading}@Chg{Version=[2],New=[ASIS defines a set of global
exceptions. These exceptions are raised under the circumstances described in
@RefSec{package Asis.Exceptions}],Old=[]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[@b{ASIS_Inappropriate_Context}@Defn{ASIS_Inappropr
iate_Context} @en Raised when ASIS is passed a Context value that is not
appropriate for the operation. This exception typically indicates that a user
error has occurred within the application.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[@b{ASIS_Inappropriate_Compilation_Unit}@Defn{ASIS_
Inappropriate_Compilation_Unit} @en Raised when ASIS is passed a
Compilation_Unit value that is not appropriate. This exception typically
indicates that a user error has occurred within the application.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[@b{ASIS_Inappropriate_Element}@Defn{ASIS_Inappropriate_Element} @en Raised when
ASIS is given an Element value that is not appropriate. This exception
typically indicates that a user error has occurred within the application.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[@b{ASIS_Inappropriate_Line}@Defn{ASIS_Inappropriate_Line} @en Raised when ASIS
is given a Line value that is not appropriate.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[@b{ASIS_Inappropriate_Line_Number}@Defn{ASIS_Inappropriate_Line_Number} @en
Raised when ASIS is given a Line_Number value that is not appropriate. This
exception typically indicates that a user error has occurred within the
application.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[@b{ASIS_Failed}@Defn{ASIS_Failed} @en All ASIS routines may raise ASIS_Failed
whenever they cannot normally complete their operation. This exception
typically indicates a failure of the underlying ASIS implementation. This is a
catch-all exception that is raised for different reasons in different ASIS
implementations.]}
@end{Itemize}


@LabeledClause{Normative references}

@ChgRef{Version=[2],Kind=[revised],ARef=[SI99-0055-1]}
The following @Chg{Version=[2],New=[standards],Old=[standard]} contains
provisions which, through reference in this
text, constitute provisions of this International Standard. At the time of
publication, the edition indicated was valid. All standards are subject to
revision, and parties to agreements based on this standard are encouraged to
investigate the possibility of applying the most recent edition of the
International @Chg{Version=[2],New=[Standards],Old=[Standard]} indicated below.
Members of IEC and ISO maintain registers of currently valid
International Standards.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
@Chg{Version=[2], New=[ISO/IEC 8652:1995(E)], Old=[ISO/IEC 8652:1995]}, @i{Information technology @em Programming languages @em Ada}.

@Comment{I checked all of these citations on the ISO website on Nov. 10, 2008.
The oddity of the '.' in the Corrigendum but not in the Amendment is how
they write them. RLB.}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0037-1]}
@ChgAdded{Version=[2],Text=[ISO/IEC 8652:1995/Cor.1:2001(E), @i{Information technology @em Programming languages @em Ada} @em Technical Corrigendum 1.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0037-1]}
@ChgAdded{Version=[2],Text=[ISO/IEC 8652:1995/AMD 1:2007(E), @i{Information technology @em Programming languages @em Ada} @em Amendment 1.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0041-1]}
@ChgAdded{Version=[2],Text=[ISO/IEC 10646:2003, @i{Information technology @em Universal Multiple-Octet Coded Character Set (UCS)}.]}


@LabeledClause{Terms and definitions}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0030-1]}
@ChgAdded{Version=[2],Text=[The Ada Programming Language is defined by International Standard ISO/IEC 8652:1995(E) as
corrected by ISO/IEC 8652:1995/COR1:2001(E) and amended by ISO/IEC 8652:1995/AMD.1:2007(E). This
set of documents is collectively known as the @i{Ada Standard}.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
For the purposes of this International Standard, the terms and definitions
given in @Chg{Version=[2], New=[the Ada Standard], Old=[ISO/IEC 8652:1995]} and the following apply.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
Additional terms are defined throughout this International Standard, indicated
by @i{italic} type. Terms explicitly defined in this International Standard are not
to be presumed to refer implicitly to similar terms defined elsewhere. Terms
not defined in this International Standard
and @Chg{Version=[2], New=[the Ada Standard], Old=[ISO/IEC 8652:1995]} are to be
interpreted according to the @i{Webster@lquote@;s Third New International
Dictionary of
the English Language}. Informal descriptions of some terms are also given in
@RefSec{Glossary}.

@ldquote@;ASIS@rdquote is used in reference to the acronym @ldquote@;Ada
Semantic Interface Specification.@rdquote@;
@ldquote@;Asis@rdquote is used in reference to the package Asis.

@begin{Discussion}
@ChgNote{Moved this from the Introduction, where it surely does not belong}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[The word "may" as used in this International
Standard consistently means "is allowed to" (or "are allowed to"). It is used
only to express permission, as in the commonly occurring phrase "an
implementation may"; other words (such as "can," "could" or "might") are used to
express ability, possibility, capacity, or consequentiality.]}
@end{Discussion}

