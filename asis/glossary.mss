@Part(glossary, Root="asis.msm")

@Comment{$Date: 2009/12/23 06:58:59 $}

@comment{$Source: e:\\cvsroot/ARM/ASIS/glossary.mss,v $}
@comment{$Revision: 1.5 $}

@LabeledInformativeAnnex{Glossary}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
All terms which are defined in @Chg{Version=[2], New=[the Ada Standard],
Old=[ISO/IEC 8652:1995]} as Ada technical terms are used in the ASIS
specification in full accordance with those definitions. See the Ada
International Standard for definitions.

@ToGlossary{Term=<Ada Standard>,Text=<Collectively, the International Standard
ISO/IEC 8652:1995(E) as corrected by ISO/IEC 8652:1995/COR1:2001(E)@Chg{Version=[2],New=[ and amended
by ISO/IEC 8652:1995/AMD.1:2007(E)],Old=[]}.>}

@ToGlossary{Term=<Ancestor>,Text=<Ancestors of a library unit are itself, its
parent, its parent's parent, and so on. (Standard is an ancestor of every
library unit).>}

@ToGlossary{Term=<ASIS>,Text=<@Chg{Version=[2],New=[is used in reference to the
acronym Ada Semantic Interface Specification. It is also used in reference to
the Ada package Asis],Old=[Any programming system or any set of software
components making use of ASIS queries to obtain information about any set of Ada
components]}.>}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0055-1]}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<ASIS implementation>,
Text=<All the hardware and software that
implement the ASIS specification @Chg{Version=[2],New=[],Old=[for a given
Ada implementation ]}and that
provide the functionality required by the ASIS specification.>}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0055-1]}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<ASIS queries>,Text=<Those
subprogram interfaces (and only
those) defined in the ASIS standard; these are supported by types, subtypes,
and exceptions also defined in the ASIS standard. Thus, ASIS queries and
supporting entities are together the ASIS interface. The following informal
query classification is used by the ASIS community: "black-box" queries are
those ASIS queries which produce information about compilation units and
"white-box" queries are those ASIS queries which produce information about
Elements; semantic queries are those ASIS queries which express semantic
properties of ASIS Elements in terms of other Elements; structural queries are
those ASIS queries which provide @Chg{Version=[2],New=[information about the
syntactic],Old=[the top-down decomposition and reverse
bottom-up composition of the compilation unit according to its syntax]}
structure@Chg{Version=[2],New=[ of the compilation unit],Old=[]}.
(Note that semantic queries are generally named "Corresponding_..."
or "Implicit_..." in the ASIS specification.)>}

@ToGlossary{Term=<Closure>,Text=<A term commonly used instead of needed units.>}

@ToGlossary{Term=<Compilation unit>,Text=<@ldquote@;The term @i{compilation unit} is used to
refer to a compilation_unit. When the meaning is clear from context, the term
is also used to refer to the library_item of a compilation_unit or to the
proper_body of a subunit@rdquote;. [@Chg{Version=[2], New=[The Ada Standard], Old=[ISO/IEC 8652:1995]}, 10.1.1(9)]. ASIS says "ASIS
compilation unit" when the intent is to stress that the ASIS viewpoint on an
Ada compilation unit is described in the ASIS standard. Note that the term
"compilation unit" can refer to either syntactical category "compilation_unit"
or to the library_item of a compilation_unit or to the proper_body of a subunit
(that is, the compilation_unit without the context_clause and the separate
(parent_unit_name)).>}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Compilation_Unit [type]>,Text=<An
ASIS private type @Chg{Version=[2],New=[whose],Old=[for which]}
values denote an Ada compilation unit or configuration pragma from the
environment denoted by some open ASIS context. A non-nil value of the
Compilation_Unit type also contains information about some physical object from
the "external world" treated by the underlying Ada implementation as the
corresponding Ada compilation unit or as a result of compiling a configuration
pragma.>}

@ToGlossary{Term=<Container>,Text=<Logical collection of ASIS compilation
units. For example, some container can hold compilation units which include Ada
predefined types, another container can hold implementation-defined packages.
Containers provide the implementation-defined way of grouping the compilation
units accessible for an ASIS application through the ASIS queries.>}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Container [type]>,Text=<An ASIS
private type @Chg{Version=[2],New=[whose],Old=[for which]} values denote a set
of compilation units being a subset of the set of compilation units making up a
context.>}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Context>,Text=<@Chg{Version=[2],New=[A],Old=[Defines a]}
set of compilation units and configuration pragmas processed by an ASIS
application. ASIS provides any information from a context by treating this set
as if its elements make up an environment declarative part by modeling some view
(most likely one of the views of the underlying Ada implementation) on the
environment. ASIS may process several different contexts at a time.>}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Context [type]>,Text=<An ASIS private
type @Chg{Version=[2],New=[whose],Old=[for which]} values
denote a set of compilation units considered by ASIS as making up an Ada
environment declarative part from which to provide information.>}

@ToGlossary{Term=<Dependent>,Text=<Dependents of a compilation unit are all the
compilation units that depend semantically on it, either directly or
indirectly. A is a dependent of B, if B is a supporter of A.>}

@ToGlossary{Term=<Descendants>,Text=<Descendants of a library unit relation are
the inverse of the ancestor relation.>}

@ToGlossary{Term=<Element>,Text=<A common abstraction used by ASIS to denote
the syntax components (both explicit and implicit) of ASIS compilation units.
The term Element is also used as the synonym for "the value of the ASIS Element
type". See also "Explicit element" and "Implicit element".>}

@ToGlossary{Term=<Element [type]>,Text=<An ASIS private type, whose values
represent the syntax components (both explicit and implicit) of ASIS
compilation units.>}

@ToGlossary{Term=<Environment>,Text=<@ldquote@;Each compilation unit submitted
to the compiler is compiled in the context of an environment declarative_part
(or simply environment), which is a conceptual declarative_part that forms the
outermost declarative region of the context of any compilation. At run time, an
environment forms the declarative_part of the body of the environment task of a
partition.@rdquote [@Chg{Version=[2], New=[ISO/IEC 8652:1995(E)], Old=[ISO/IEC 8652:1995]}, 10.1.4(1)]. Note that the mechanisms
for creating an environment and for adding and replacing compilation units
within an environment are implementation-defined.>}

@ToGlossary{Term=<Explicit element>,Text=<An ASIS Element, representing a
language construct that appears explicitly in the program text for the
compilation unit (e.g., an explicit declaration).>}

@ToGlossary{Term=<Extension>,Text=<Non-standard facilities (other library
units, non-standard children of standard ASIS library units, subprograms, etc.)
which provide additional information from ASIS types, or modify the behavior of
otherwise standard ASIS facilities to provide alternative or additional
functionality.>}

@ToGlossary{Term=<Family>,Text=<The family of a given unit is defined as the
set of compilation units that comprise the given unit's declaration, body,
descendants, and subunits (and subunits of subunits and descendants, etc.).>}

@ToGlossary{Term=<Id>,Text=<A way of identifying a particular element, from a
particular compilation unit, from a particular context.>}

@ToGlossary{Term=<Id [type]>,Text=<An ASIS private type implementing the Id
abstraction. The values of this type can be written to files. These values can
be read back from files and converted into values of the Element type with the
use of a suitable open context.>}

@ToGlossary{Term=<Implementor>,Text=<A company, institution, or other group
(such as a vendor) who develops an ASIS implementation; thus an ASIS
implementor. There are also Ada implementors, who provide Ada compilation
systems; and there are ASIS-based tool (or, ASIS Application) implementors, who
develop tools which are based upon the ASIS standard.>}

@ToGlossary{Term=<Implicit element>,Text=<An ASIS Element, representing a
language construct that does not exist in the program text for the compilation
unit, but could occur at a given place in the program text as a consequence of
the semantics of another construct, (e.g., an implicit declaration, a generic
instantiation).>}

@ToGlossary{Term=<Line>,Text=<The logical representation of a line of text from
the source code of the external representation of a compilation unit.>}

@ToGlossary{Term=<Line [type]>,Text=<An ASIS private type for the ASIS Line
abstraction. The values of the Line type represent the lines of text from the
source code of the external representation of compilation units.>}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Needed Units>,Text=<The
@Chg{Version=[2],New=[],Old=[needed units of a given compilation
unit is a ]}set of compilation units ultimately needed by the given compilation
unit @Chg{Version=[2],New=[],Old=[to make up or ]}to be included in a
completed partition.>}

@ToGlossary{Term=<Optional functionality>,Text=<The subset of ASIS facilities
that are explicitly identified in the ASIS standard as optional which may
legitimately be omitted from a Basic Conforming ASIS implementation, but shall
be included in any Fully Conforming ASIS implementation, unless stated
otherwise in the ASIS specification.>}

@ToGlossary{Term=<Queries>,Text=<See ASIS queries.>}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Relation (between ASIS Compilation Units)>,Text=<Semantic
relationships between compilation units (as discussed in chapter 10 of
@Chg{Version=[2],New=[the Ada Standard],Old=[ISO/IEC 8652:1995]}). The
Relation_Kinds type enumerates the kinds of relations that can
exist between compilation units. See also Dependent, Extended Family, and
Supporter.>}

@ToGlossary{Term=<Required functionality>,Text=<The subset of ASIS facilities
which are not explicitly identified in the ASIS standard as optional which
shall be included in a Basic or Fully Conforming ASIS implementation, unless
stated otherwise in the ASIS specification.>}

@ToGlossary{Term=<Semantic queries>,Text=<See ASIS queries.>}

@ToGlossary{Term=<Structural queries>,Text=<See ASIS queries.>}

@ToGlossary{Term=<Supporter>,Text=<Supporters of a compilation unit are units
on which it semantically depends, either directly or indirectly. B is a
supporter of A, if A is a dependent of B.>}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0030-1]}
@ChgToGlossary{Version=[2],Kind=[AddedNormal],Term=<CASE>,Text=<@ChgAdded{Version=[2],Text=[Computer
Assisted Software/System
Engineering refers to the methods dedicated to an engineering discipline for the
development of information systems together with automated tools that can be used
in this process.]}>}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0030-1]}
@ChgToGlossary{Version=[2],Kind=[AddedNormal],Term=<DII>,Text=<@ChgAdded{Version=[2],Text=[Dynamic
Invocation Interface is an API which allows dynamic construction of CORBA object
invocations. It is used at compile time when a client does not have knowledge
about the object it wants to invoke. With this interface an argument list is
marshalled, a function is named, and a request for service is sent to the object
server. DII implementations will usually have an asynchronous mode of operation.]}>}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0030-1]}
@ChgToGlossary{Version=[2],Kind=[AddedNormal],Term=<IDL>,Text=<@ChgAdded{Version=[2],Text=[Interface
Definition Language is used
by CORBA to specify the interfaces that objects will present to the outside world.
CORBA then specifies a mapping from IDL to a specific implementation language like
C++ or Java. Standard mappings exist for Ada, C, C++, Lisp, Smalltalk, Java, COBOL,
PL/I and Python. There are also non-standard mappings for Perl, Visual Basic, Ruby,
Erlang, and Tcl implemented by object request brokers (ORBs) written for those languages.]}>}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0030-1]}
@ChgToGlossary{Version=[2],Kind=[AddedNormal],Term=<IEC>,Text=<@ChgAdded{Version=[2],Text=[International
Electrotechnical Commission
is a not-for-profit, non-governmental international standards organization that prepares
and publishes International Standards for all electrical, electronic and related
technologies collectively known as "electrotechnology".]}>}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0030-1]}
@ChgToGlossary{Version=[2],Kind=[AddedNormal],Term=<ISO>,Text=<@ChgAdded{Version=[2],Text=[International
Organization for
Standardization (Organisation internationale de normalisation),  widely known as ISO,
is an international standard-setting body composed of representatives from various
national standards organizations.]}>}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0030-1]}
@ChgToGlossary{Version=[2],Kind=[AddedNormal],Term=[JTC],Text=<@ChgAdded{Version=[2],Text=[ISO/IEC
JTC is the Joint Technical
Committee of the International Organization for Standardization (ISO) and the
International Electrotechnical Commission (IEC). It deals with all matters of
information technology.]}>}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0030-1]}
@ChgToGlossary{Version=[2],Kind=[AddedNormal],Term=<ORB>,Text=<@ChgAdded{Version=[2],Text=[A CORBA
Object Request Broker
is a piece of middleware software that allows programmers to make program
calls from one computer to another via a network.]}>}

@Comment{Display the glossary}
@GlossaryList

