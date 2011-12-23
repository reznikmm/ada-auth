@comment{ $Source: e:\\cvsroot/ARM/Source/ds.mss,v $ }
@comment{ $Revision: 1.62 $ $Date: 2011/11/01 23:14:15 $ $Author: randy $ }
@Part(dist, Root="ada.mss")
@Comment{$Date: 2011/11/01 23:14:15 $}

@LabeledNormativeAnnex{Distributed Systems}

@begin{Intro}
@redundant[This Annex defines facilities for supporting the implementation
of distributed systems using multiple partitions working
cooperatively as part of a single Ada program.]
@end{Intro}

@begin{Extend83}
@Defn{extensions to Ada 83}
This Annex is new to Ada 95.
@end{Extend83}

@begin{LinkTime}

@Defn{processing node}
@Defn{storage node}
@Defn{distributed system}
A @i{distributed system} is an interconnection of one or more
@i{processing nodes} (a system resource that has both computational
and storage capabilities), and zero or more @i{storage nodes}
(a system resource that has only storage capabilities, with the storage
addressable by one or more processing nodes).

@Defn{distributed program}
A @i{distributed program} comprises one or more partitions that
execute independently (except when they communicate) in a distributed system.

@Defn2{Term=[configuration], Sec=(of the partitions of a program)}
The process of mapping the partitions of a program
to the nodes in a distributed system is
called @i{configuring the partitions of the program}.
@end{LinkTime}

@begin{ImplReq}

The implementation shall provide means for explicitly assigning library
units to a partition and for the configuring and execution of a program
consisting of multiple partitions on a distributed system;
the means are implementation defined.
@ImplDef{The means for creating and executing distributed programs.}

@end{ImplReq}

@begin{ImplPerm}

An implementation may require that the set of processing nodes of
a distributed system be homogeneous.

@end{ImplPerm}

@begin{Notes}


The partitions comprising a program may be executed on differently
configured distributed systems or on a non-distributed system without
requiring recompilation.
A distributed program may be partitioned differently from the same set of
library units without recompilation.
The resulting execution is semantically equivalent.

A distributed program retains the same type safety
as the equivalent single partition program.



@end{Notes}

@LabeledClause{Partitions}

@begin{Intro}
@redundant[The partitions of a distributed program are classified as either
active or passive.]
@end{Intro}

@begin{LinkTime}

@Defn{active partition}
@Defn{passive partition}
An @i{active partition} is a partition as defined in
@RefSecNum{Program Execution}. A @i{passive partition} is a partition
that has no thread of control of its own, whose
library units are all preelaborated, and whose data and subprograms are
accessible to one or more active partitions.
@begin{Discussion}
In most situations, a passive partition does not execute, and does not have
a @lquotes@;real@rquotes@; environment task. Any execution involved in
its elaboration and initialization occurs before it comes into existence in a
distributed program (like most preelaborated entities). Likewise,
there is no concrete meaning to passive partition termination.
@end{Discussion}

A passive partition shall include only @nt{library_item}s that either are
declared pure or are shared
passive (see @RefSecNum{Elaboration Control} and
@RefSecNum{Shared Passive Library Units}).

An active partition shall be configured on a processing node.
A passive partition shall be configured either on a storage node
or on a processing node.

The configuration of the partitions of a program onto a
distributed system shall
be consistent with the possibility for data references or calls
between the partitions implied by their semantic dependences.
@Defn{remote access}
Any reference to data or call of
a subprogram across partitions is called a @i{remote access}.
@begin{Discussion}
For example, an active partition that includes a unit with a semantic
dependence on the declaration of another RCI package of some other active
partition has to be connected to that other partition by some sort
of a message passing mechanism.

A passive partition that is accessible to an active partition should have
its storage addressable to the processor(s) of the active partition. The
processor(s) should be able to read and write from/to that
storage, as well as
to perform @lquotes@;read-modify-write@rquotes@; operations (in order to support entry-less
protected objects).

@end{Discussion}

@end{LinkTime}

@begin{RunTime}

@Defn2{Term=[elaboration], Sec=(partition)}
A @nt{library_item} is elaborated as part of the elaboration
of each partition that includes it.
If a normal library unit
(see @RefSecNum{Categorization of Library Units}) has state, then a separate
copy of the state exists in each active partition that elaborates it.
@Redundant[The state evolves independently in each such partition.]


@begin{Ramification}
Normal library units cannot be included in passive partitions.
@end{Ramification}

@Defn2{Term=[termination], Sec=(of a partition)}
@Defn2{Term=[abort], Sec=(of a partition)}
@Defn{inaccessible partition}
@Defn{accessible partition}
@Redundant[An active partition @i{terminates} when its environment task
terminates.] A partition becomes @i{inaccessible} if it terminates or if
it is @i{aborted}. An active partition is aborted when its environment
task is aborted. In addition, if a partition fails during its
elaboration, it becomes inaccessible to other partitions. Other
implementation-defined events can also result in a partition becoming
inaccessible.
@ImplDef{Any events that can result in a partition becoming
inaccessible.}

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
@Leading@;For @ChgPrefixType{Version=[1],Kind=[Revised],Text=[a
@Chg{New=[@nt{prefix}],Old=[prefix]} D that denotes a
library-level declaration,
excepting a declaration of or within a declared-pure library unit]},
the following attribute is defined:
@begin{Description}
@Attribute{Prefix=<D>, AttrName=<Partition_Id>,
  Text=[Denotes a value of the type @i{universal_integer} that
         identifies the partition in which D was elaborated.
         If D denotes the declaration of a remote call interface
         library unit
         (see @RefSecNum{Remote Call Interface Library Units}) the
         given partition is the one where the body of D was elaborated.]}
@end{Description}

@end{RunTime}

@begin{Bounded}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00226-01]}
@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error for there to be cyclic elaboration dependences
between the active partitions of a single distributed
program.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
The possible effects@Chg{Version=[2],New=[, in each of the partitions involved,],Old=[]}
are deadlock during elaboration, or the raising of
@Chg{Version=[2],New=[Communication_Error or ],Old=[]}Program_Error@Chg{Version=[2],
New=[],Old=[ in one or all of the active partitions involved]}.
@end{Bounded}

@begin{ImplPerm}

An implementation may allow multiple active or passive partitions
to be configured on
a single processing node, and multiple passive partitions to be
configured on a single storage node.
In these cases, the scheduling policies, treatment
of priorities, and management of shared resources between
these partitions are implementation defined.
@ImplDef{The scheduling policies, treatment
of priorities, and management of shared resources between
partitions in certain cases.}

An implementation may allow separate copies of an active partition to be
configured on different processing nodes, and to provide
appropriate interactions between the copies to present
a consistent state of the partition to other active partitions.
@begin{Ramification}
  The language does not specify the nature of these interactions,
  nor the actual level of consistency preserved.
@end{Ramification}

In an implementation, the partitions of a distributed
program need not be loaded and elaborated all at the same time;
they may be loaded and elaborated one at a time over an extended
period of time. An implementation may provide facilities to abort
and reload a partition during the execution of a distributed program.

An implementation may allow the state of some of the partitions of
a distributed program to persist while other partitions of the program
terminate and are later reinvoked.
@end{ImplPerm}

@begin{Notes}

Library units are grouped into partitions after compile time,
but before run time.
At compile time, only the relevant library unit properties are
identified using categorization pragmas.


The value returned by the Partition_Id attribute can be used as a
parameter to implementation-provided subprograms in order
to query information about the partition.

@end{Notes}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00226-01]}
  @ChgAdded{Version=[2],Text=[Corrected wording so that a partition that
  has an elaboration problem will either deadlock or raise an exception.
  While an Ada 95 implementation could allow some partitions to continue to
  execute, they could be accessing unelaborated data, which is very bad
  (and erroneous in a practical sense). Therefore, this isn't listed as an
  inconsistency.]}
@end{DiffWord95}


@RMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledClause{Categorization of Library Units}

@begin{Intro}
@redundant[Library units can be categorized according to the role
they play in a distributed program. Certain restrictions
are associated with each category to ensure that the semantics
of a distributed program remain close to the semantics for a nondistributed
program.]

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0243-1]}
@RootDefn{categorization pragma}
@RootDefn{pragma, categorization}
@PDefn2{Term=[library unit pragma], Sec=(categorization pragmas)}
@PDefn2{Term=[pragma, library unit], Sec=(categorization pragmas)}
@Chg{Version=[3],New=[@Defn{categorization aspect}],Old=[]}@Defn{categorized library unit}
A @i{categorization pragma} is a library unit pragma
(see @RefSecNum{Pragmas and Program Units})
that @Chg{Version=[3],New=[specifies a corresponding @i{categorization aspect}.
A categorization aspect ],Old=[]}restricts the declarations, child units, or
semantic dependences of the library unit to which it applies. A
@i{categorized library unit} is a library unit @Chg{Version=[3],New=[that has a
categorization aspect that is True],Old=[to which a categorization pragma
applies]}.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0243-1]}
The pragmas Shared_Passive, Remote_Types, and Remote_Call_Interface
are categorization pragmas@Chg{Version=[3],New=[,
and the associated aspects are categorization aspects],Old=[]}.
In addition, for the purposes of this Annex, the @Chg{Version=[3],New=[aspect
Pure (see @RefSecNum{Elaboration Control}) is
considered a categorization aspect and the],Old=[]}
pragma Pure @Chg{Version=[3],New=[],Old=[(see @RefSecNum{Elaboration Control}) ]}is
considered a categorization pragma.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0078],ARef=[AI95-00048-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0243-1]}
@Redundant[@Defn{shared passive library unit}
A library package or generic library package is called a
@i{shared passive} library unit if @Chg{Version=[3],New=[the],Old=[a]}
Shared_Passive @Chg{Version=[3],New=[aspect of the unit is True],Old=[pragma
applies to it]}. @Defn{remote types library unit} A library package or generic
library package is called a @i{remote types} library unit if
@Chg{Version=[3],New=[the],Old=[a]} Remote_Types @Chg{Version=[3],New=[aspect of
the unit is True],Old=[pragma applies to it]}. @Defn{remote call interface} A
library @Chg{New=[unit],Old=[package or generic library package]} is called a
@i{remote call interface} if @Chg{Version=[3],New=[the],Old=[a]}
Remote_Call_Interface @Chg{Version=[3],New=[aspect of the unit is
True],Old=[pragma applies to it]}.]
@Defn{normal library unit}
A @i{normal library unit} is one @Chg{Version=[3],New=[for],Old=[to]}
which no categorization
@Chg{Version=[3],New=[aspect is True],Old=[pragma applies]}.
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0243-1]}
  @ChgAdded{Version=[3],Text=[These terms (other than @ldquote@;normal
  library unit@rdquote) are really defined in the following clauses.]}
@end{TheProof}
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0078],ARef=[AI95-00048-01]}
@ChgAdded{Version=[1],Text=[A library subprogram can be a remote call
interface, but it cannot be a remote types or shared passive library unit.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0206-1],ARef=[AI05-0243-1]}
@redundant[The various categories of library units and the associated
restrictions are described in this clause and its subclauses. The categories are
related hierarchically in that the library units of one category can depend
semantically only on library units of that category or an earlier one, except
that the body of a remote types or remote call interface library unit is
unrestricted@Chg{Version=[3],New=[, the declaration of a remote types or
remote call interface library unit may depend on preelaborated normal library
units that are mentioned only in private with clauses, and all categories can
depend on limited views],Old=[]}.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0243-1]}
@Leading@;The overall hierarchy (including declared pure) is as
follows@Chg{Version=[3],New=[ (with a lower-numbered category being earlier in
the sense of the previous paragraph)],Old=[]}:
@begin{Enumerate}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Declared Pure]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Shared Passive]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Remote Types]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Remote Call Interface]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Normal (no restrictions)]}
@end{Enumerate}
@begin{NotIso}
@ChgAdded{Version=[3],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 7 through 11
were deleted.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}
@begin{Description}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0243-1]}
@ChgDeleted{Version=[3],Text=[Declared Pure @\Can depend only on other declared
pure library units;]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0243-1]}
@ChgDeleted{Version=[3],Text=[Shared Passive @\Can depend only on other shared passive
    or declared pure library units;]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0206-1],ARef=[AI05-0243-1]}
@ChgDeleted{Version=[3],Text=[Remote Types @\The declaration of the library unit can depend only
    on other remote types library units,
    or one of the
    above library unit categories, or limited views,
    or preelaborated normal library units that are mentioned
    only in private with clauses;
    the body of the library unit is unrestricted;]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0206-1],ARef=[AI05-0243-1]}
@ChgDeleted{Version=[3],Text=[Remote Call Interface @\The declaration of the library unit can depend only
    on other remote call interfaces, or one
    of the above;
    the body of the library unit is unrestricted;]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0243-1]}
@ChgDeleted{Version=[3],Text=[Normal @\Unrestricted.]}
@end{Description}

Declared pure and shared passive library units are preelaborated.
The declaration of a remote types or remote call interface library unit
is required to be preelaborable.
]

@end{Intro}

@begin{ImplReq}
@ChgRef{Version=[1],Kind=[Deleted],Ref=[8652/0079],ARef=[AI95-00208-01]}
@ChgDeleted{Version=[1],Text=[For a given library-level type declared in
a preelaborated library unit or in the declaration of a remote types or remote
call interface library unit, the implementation shall choose the same
representation for the type upon each elaboration of the type's declaration for
different partitions of the same program.]}
@end{ImplReq}

@begin{ImplPerm}

Implementations are allowed to define other categorization pragmas.

@end{ImplPerm}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0078],ARef=[AI95-00048-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that a library
  subprogram can be a remote call interface unit.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0079],ARef=[AI95-00208-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Removed the requirement that
  types be represented the same in all partitions, because it prevents the
  definition of heterogeneous distributed systems and goes much further than
  required.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0206-1]}
  @ChgAdded{Version=[3],Text=[We now allow private withs of preelaborated
  units in Remote Types and Remote Call Interface units; this is documented
  as an extension in the sections where this is defined normatively.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0243-1]}
  @ChgAdded{Version=[3],Text=[We have introduced categorization aspects; these
  are documented as extensions in the clauses where they actually are defined.]}
@end{DiffWord2005}


@LabeledSubClause{Shared Passive Library Units}
@begin{Intro}
@redundant[A shared passive library unit is used for managing global data
shared between active partitions. The restrictions on shared passive
library units prevent the data or tasks of one active partition
from being accessible to another active partition through
references implicit in objects declared in the shared passive library
unit.]
@end{Intro}

@begin{MetaRules}
The restrictions governing a shared
passive library unit are
designed to ensure that objects and
subprograms declared in the package can be used safely from
multiple active partitions, even though the active partitions
live in different address spaces, and have separate run-time systems.
@end{MetaRules}

@begin{Syntax}
@begin{SyntaxText}
@Leading@PDefn2{Term=[categorization pragma], Sec=(Shared_Passive)}
@PDefn2{Term=[pragma, categorization], Sec=(Shared_Passive)}
The form of a @nt{pragma} Shared_Passive is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Shared_Passive)[(@SynI{library_unit_}@Syn2{name})];'

@end{Syntax}

@begin{Legality}

@Leading@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0243-1]}
@Defn{shared passive library unit}
A@Chg{Version=[3],New=[ @nt{pragma} Shared_Passive is used to specify that a
library unit is a],Old=[]} @i{shared passive library unit}@Chg{Version=[3],New=[,
namely that the],Old=[ is
a library unit to which a]}
Shared_Passive
@Chg{Version=[3],New=[aspect@AspectDefn{Shared_Passive} of the library unit is
True],Old=[pragma applies]}. The following restrictions apply to such a library
unit:

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Shared_Passive],
    Text=[@ChgAdded{Version=[3],Text=[A given package is used to represent
      shared memory in a distributed system.]}]}

@begin{itemize}
@Redundant[it shall be preelaborable (see @RefSecNum{Elaboration Control});]
@begin{Ramification}
  It cannot contain library-level declarations of protected objects
  with entries, nor of task objects. Task objects are disallowed
  because passive partitions don't have any threads of control of their
  own, nor any run-time system of their own.
  Protected objects with entries are disallowed because an entry queue
  contains references to calling tasks, and that would require in
  effect a pointer from a passive partition back to a task in
  some active partition.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0243-1]}
it shall depend semantically only upon declared pure or shared passive
@Chg{Version=[3],New=[@nt{library_item}s],Old=[library units]};
@begin{Reason}
  Shared passive packages cannot depend semantically upon remote types packages
  because the values of an access type declared in a remote types package
  refer to the local heap of the active partition including the
  remote types package.
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0243-1]}
  @ChgAdded{Version=[3],Text=[We say @nt{library_item} here, so that
  limited views are allowed; those are not library units, but they are
  @nt{library_item}.]}
@end{Ramification}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0080],ARef=[AI95-00003-01]}
it shall not contain a library-level declaration of an access type
that designates a class-wide type,
task type, or protected type with @nt{entry_declaration}s@Chg{New=[],
Old=[; if the shared passive library unit is generic, it shall
not contain a declaration for such an access type unless the
declaration is nested within a body other than
a @nt<package_body>]}.
@begin{Reason}
  These kinds of access types are disallowed because the object
  designated by an access value of
  such a type could contain an implicit reference back to
  the active partition on whose behalf the designated object was
  created.
@end{Reason}

@end{itemize}

@PDefn2{Term=[accessibility], Sec=(from shared passive library units)}
@Defn{notwithstanding}
Notwithstanding the definition of accessibility given in
@RefSecNum(Operations of Access Types), the declaration
of a library unit P1 is not accessible from within the declarative
region of a shared passive library unit P2,
unless the shared passive library unit
P2 depends semantically on P1.
@begin{Discussion}
  We considered a more complex rule, but dropped it. This is
  the simplest rule that recognizes that a shared passive
  package may outlive some other library package, unless it
  depends semantically on that package. In a nondistributed
  program, all library packages are presumed to have the same lifetime.

  Implementations may define additional pragmas that
  force two library packages to be in the same partition, or to have
  the same lifetime, which would allow this rule to be relaxed
  in the presence of such pragmas.
@end{Discussion}

@end{Legality}

@begin{StaticSem}

@PDefn{preelaborated}
A shared passive library unit is
preelaborated.

@end{StaticSem}

@begin{LinkTime}

A shared passive library unit shall be
assigned to at most one partition within a given program.

@PDefn2{Term=[compilation units needed], Sec=(shared passive library unit)}
@PDefn2{Term=[needed], Sec=(shared passive library unit)}
@Defn{notwithstanding}
Notwithstanding the rule given in @RefSecNum{Program Execution},
a compilation unit in a given partition
does not @i{need} (in the
sense of @RefSecNum{Program Execution})
the shared passive library units
on which it depends semantically to be included in that same
partition; they will typically reside in separate passive
partitions.

@end{LinkTime}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0080],ARef=[AI95-00003-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the wording to allow
  access types in blocks in shared passive generic packages.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0243-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}Shared_Passive
  is now a categorization aspect,
  so it can be specified by an @nt{aspect_specification} @em
  although the pragma is still preferred by the Standard.]}
@end{Extend2005}


@LabeledSubClause{Remote Types Library Units}

@begin{Intro}
@redundant[A remote types library unit supports the definition of types
intended for use in communication between active
partitions.]
@end{Intro}
@begin{MetaRules}
The restrictions governing a remote types package are similar to those for
a declared pure package. However, the restrictions are relaxed
deliberately to allow such a package to contain declarations that violate
the stateless property of pure packages, though it is presumed
that any state-dependent properties are essentially invisible
outside the package.
@end{MetaRules}

@begin{Syntax}
@begin{SyntaxText}
@Leading@PDefn2{Term=[categorization pragma], Sec=(Remote_Types)}
@PDefn2{Term=[pragma, categorization], Sec=(Remote_Types)}
The form of a @nt{pragma} Remote_Types is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Remote_Types)[(@SynI{library_unit_}@Syn2{name})];'

@end{Syntax}

@begin{Legality}

@Leading@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0243-1]}
@Defn{remote types library unit}
A@Chg{Version=[3],New=[ @nt{pragma} Remote_Types is used to specify that a library unit is a],Old=[]}
@i{remote types library unit}@Chg{Version=[3],New=[,
namely that the],Old=[ is a library unit to which the pragma]}
Remote_Types
@Chg{Version=[3],New=[aspect@AspectDefn{Remote_Types} of the library unit is
True],Old=[applies]}. The following restrictions apply to the declaration of
such a library unit:

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Remote_Types],
    Text=[@ChgAdded{Version=[3],Text=[Types in a given package may be used in
      remote procedure calls.]}]}

@begin{itemize}
@Redundant[it shall be preelaborable;]

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0206-1],ARef=[AI05-0243-1]}
it shall depend semantically only on declared pure@Chg{Version=[3],New=[
@nt{library_item}s],Old=[]}, shared passive@Chg{Version=[3],New=[ library
units],Old=[]}, @Chg{Version=[3],New=[],Old=[or ]}other remote types library
units@Chg{Version=[3],New=[, or preelaborated normal library units that are
mentioned only in private with clauses],Old=[]};

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0243-1]}
  @ChgAdded{Version=[3],Text=[We say declared pure @nt{library_item} here,
  so that (all) limited views are allowed; those are not library units, but
  they are declared pure @nt{library_item}s.]}
@end{Ramification}

it shall not contain the declaration of any variable
within the visible part of the library unit;
@begin{Reason}
  This is essentially a @lquotes@;methodological@rquotes@; restriction.
  A separate copy of a remote types package is included
  in each partition that references it, just like a normal package.
  Nevertheless, a remote types package is thought of as
  an @lquotes@;essentially pure@rquotes@; package for defining types to be used
  for interpartition communication,
  and it could be misleading to declare visible objects
  when no remote data access is actually being provided.
@end{Reason}


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00240-01],ARef=[AI95-00366-01]}
@Chg{Version=[2],New=[],Old=[if ]}the full view of @Chg{Version=[2],New=[each],Old=[a]}
type declared in the visible part of the
library unit @Chg{Version=[2],New=[that has any available stream attributes
shall support external streaming (see
@RefSecNum{Stream-Oriented Attributes})], Old=[has a part that is of a
non-remote access type, then that access type, or the type of some
part that includes the access type subcomponent,
shall have user-specified Read and Write attributes]}.
@begin{Reason}
  This is to prevent the use of the predefined Read and Write
  attributes of an access type as part of the Read and Write
  attributes of a visible type.
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00366-01]}
  @ChgAdded{Version=[2],Text=[Types that do not have available stream
  attributes are excluded from this rule; that means that attributes do not
  need to be specified for most limited types. It is only necessary to specify
  attributes for nonlimited types that have a part that is of any access type,
  and for extensions of limited types with available stream attributes where
  the @nt{record_extension_part} includes a subcomponent of an access type,
  where the access type does not have specified attributes.]}
@end{Ramification}
@end{itemize}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0082],ARef=[AI95-00164-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0060-1]}
@ChgAdded{Version=[1],Type=[Leading],Text=[]}@ChgNote{Use ChgAdded to get conditional Leading}@Defn{remote access type}
@Chg{Version=[3],New=[A named],Old=[An]} access type declared in the
visible part of a remote types or remote
call interface library unit is called a @i{remote access type}.
@Defn{remote access-to-subprogram type}
@Defn{remote access-to-class-wide type}
Such a type shall be@Chg{New=[:],Old=[ either an access-to-subprogram type
or a general access type that designates a class-wide limited private
type.]}

@begin{Itemize}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0082],ARef=[AI95-00164-01]}
@ChgAdded{Version=[1],Text=[an access-to-subprogram type, or]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0082],ARef=[AI95-00164-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0060-1]}
@ChgAdded{Version=[1],Text=[a general access type that designates a class-wide
limited private type@Chg{Version=[3],New=[, a class-wide limited interface
type,],Old=[]} or a class-wide
private @Chg{Version=[3],New=[],Old=[type ]}extension all of whose
ancestors are either
private @Chg{Version=[3],New=[],Old=[type ]}extensions@Chg{Version=[3],New=[,
limited interface types,],Old=[]} or limited private types.]}
@end{Itemize}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0081],ARef=[AI95-00004-01]}
@ChgAdded{Version=[1],Text=[A type that is derived from a remote access type
is also a remote access type.]}

@Leading@;The following restrictions apply to the use of a
remote access-to-subprogram type:
@begin{Itemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00431-01]}
A value of a remote access-to-subprogram type
shall be converted only to @Chg{Version=[2],New=[or from ],Old=[]}another
(subtype-conformant) remote access-to-subprogram type;

The @nt<prefix> of an Access @nt<attribute_reference> that yields
a value of a remote access-to-subprogram type shall statically denote a
(subtype-conformant) remote subprogram.
@end{Itemize}

@Leading@;The following restrictions apply to the use of a
remote access-to-class-wide type:
@begin{Itemize}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0083],ARef=[AI95-00047-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00240-01],ARef=[AI95-00366-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0060-1],ARef=[AI05-0101-1]}
The primitive subprograms of the corresponding specific
@Chg{Version=[3],New=[],Old=[limited private ]}type
shall only have access parameters if they are controlling formal
parameters@Chg{Version=[3],New=[. The primitive functions of the corresponding
specific type shall only have an access result if it is a controlling access
result.],Old=[;]} @Chg{New=[@Chg{Version=[3],New=[Each],Old=[each]}
non-controlling formal parameter],Old=[the types of all the non-controlling
formal parameters]} @Chg{Version=[3],New=[and non-controlling result
type ],Old=[]}shall @Chg{Version=[2],New=[support external streaming (see
@RefSecNum{Stream-Oriented Attributes});], Old=[have @Chg{New=[either a
nonlimited type or a type with],Old=[]} Read and Write attributes@Chg{New=[
specified via an @nt{attribute_definition_clause};],Old=[.]}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0060-1],ARef=[AI05-0215-1]}
@ChgAdded{Version=[3],Text=[The corresponding specific type shall not have
a primitive procedure with the Synchronization aspect specified unless the
@nt{synchronization_kind} is Optional;]}

A value of a remote access-to-class-wide type shall be
explicitly converted only to another remote access-to-class-wide type;

A value of a remote access-to-class-wide type shall be dereferenced
(or implicitly converted to an anonymous access type)
only as part of a dispatching call where the value designates
a controlling operand of the call (see @RefSec{Remote Subprogram Calls});

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0101-1]}
@ChgAdded{Version=[3],Text=[A controlling access result value for a primitive
function with any controlling operands of the corresponding specific type shall
either be explicitly converted to a remote access-to-class-wide type or be part
of a dispatching call where the value designates a controlling operand of the
call;]}

@ChgRef{Version=[1],Kind=[Revised]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00366-01]}
The Storage_Pool @Chg{Version=[2],New=[attribute is],Old=[and
Storage_Size attributes are]} not defined for@Chg{Version=[2],New=[ a],Old=[]}
remote access-to-class-wide @Chg{Version=[2],New=[type],Old=[types]};
the expected type for an @nt{allocator} shall not be a remote
access-to-class-wide type@Chg{Version=[2],New=[. A],Old=[; a]} remote
access-to-class-wide type shall not be an actual parameter for a generic
formal access type@Chg{New=[.],Old=[;]}@Chg{Version=[2],New=[ The Storage_Size
attribute of a remote access-to-class-wide type yields 0; it is not allowed in
an @nt{attribute_definition_clause}.],Old=[]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  All @Chg{Version=[3],New=[three ],Old=[]}@ChgNote{There are more than three!}of
  these restrictions are because
  there is no storage pool associated with a remote
  access-to-class-wide type.@Chg{Version=[2],New=[ The Storage_Size is defined
  to be 0 so that there is no conflict with the rules for pure units.],Old=[]}
@end{Reason}
@end{Itemize}

@end{Legality}

@begin{Notes}
A remote types library unit
need not be pure, and the types it defines may
include levels of indirection implemented by using access types.
User-specified Read and Write attributes
(see @RefSecNum{Stream-Oriented Attributes})
provide for sending values of such a type
between active partitions, with Write marshalling the
representation, and Read unmarshalling any levels of
indirection.

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0060-1]}
@ChgAdded{Version=[3],Text=[The value of a remote access-to-class-wide limited
interface can designate an object of a nonlimited type derived from the
interface.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0060-1]}
@ChgAdded{Version=[3],Text=[A remote access type may designate a class-wide
synchronized, protected, or task interface type.]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Synchronized, protected, and task interfaces
  are all considered limited interfaces, see @RefSecNum{Interface types}.]}
@end{TheProof}

@end{Notes}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00240-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @b[Amendment Correction:] The wording was changed from
  @lquotes@;user-specified@rquotes to @lquotes@;available@rquotes
  @Chg{Version=[3],New=[read and write ],Old=[]}attributes. (This was then
  further changed, see below.) This means that
  an access type with the attributes specified in the private part would
  originally have been sufficient to allow the access type to be used in
  a remote type, but that is no longer allowed. Similarly, the attributes
  of a remote type that has access components have to be specified in the
  visible part. These changes were made so that the rules were consistent with
  the rules introduced for the Corrigendum for stream attributes; moreover,
  legality should not depend on the contents of the private part.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00366-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Remote types that cannot be streamed (that is, have no available stream
  attributes) do not require the specification of stream attributes.
  This is necessary so that most extensions of Limited_Controlled do not
  need stream attributes defined (otherwise there would be a
  @Chg{Version=[3],New=[significant],Old=[signficant]}
  incompatibility, as Limited_Controlled would need stream attributes, and then
  all extensions of it also would need stream attributes).]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0081],ARef=[AI95-00004-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added missing wording so that
  a type derived from a remote access type is also a remote access type.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0083],ARef=[AI95-00047-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that user-defined
  Read and Write attributes are required for the primitive subprograms
  corresponding to a remote access-to-class-wide type.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0082],ARef=[AI95-00164-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added missing wording so that
  a remote access type can designate an appropriate private extension.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00366-01]}
  @ChgAdded{Version=[2],Text=[Changed the wording to use the newly defined
  term @i<type that supports external streaming>, so that various issues
  with access types in pure units and implicitly declared attributes for
  type extensions are properly handled.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00366-01]}
  @ChgAdded{Version=[2],Text=[Defined Storage_Size to be 0 for
  remote access-to-class-wide types, rather than having it undefined. This
  eliminates issues with pure units requiring a defined storage size.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00431-01]}
  @ChgAdded{Version=[2],Text=[Corrected the wording so that a value of a local
  access-to-subprogram type cannot be converted to a remote
  access-to-subprogram type, as intended (and required by the ACATS).]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0101-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Added rules for returning of remote access-to-classwide types; this had been
  missed in the past. While programs that returned unstreamable types from RCI
  functions were legal, it is not clear what they could have done (as the
  results could not be marshalled). Similarly, RCI functions that return remote
  controlling access types could try to save those values, but it is unlikely
  that a compiler would know how to do that usefully. Thus, it seems unlikely
  that any real programs will be impacted by these changes.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0060-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}@b<Correction:>
  Clarified that anonymous access types are never remote access types (and can
  be used in remote types units subject to the normal restrictions). Added
  wording to allow limited class-wide interfaces to be designated by remote
  access types.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0206-1]}
  @ChgAdded{Version=[3],Text=[Added wording to allow private withs of
  preelaborated normal units in the specification of a remote types unit.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0243-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}Remote_Types
  is now a categorization aspect,
  so it can be specified by an @nt{aspect_specification} @em
  although the pragma is still preferred by the Standard.]}
@end{Extend2005}



@LabeledSubClause{Remote Call Interface Library Units}

@begin{Intro}
@redundant[A remote call interface library unit can be
used as an interface for remote procedure calls (RPCs) (or remote
function calls) between active partitions.]
@end{Intro}

@begin{MetaRules}
The restrictions governing a remote call
interface library unit are intended to ensure that
the values of the actual parameters in a remote call can be meaningfully
sent between two active partitions.
@end{MetaRules}

@begin{Syntax}
@begin{SyntaxText}
@Leading@PDefn2{Term=[categorization pragma], Sec=(Remote_Call_Interface)}
@PDefn2{Term=[pragma, categorization], Sec=(Remote_Call_Interface)}
The form of a @nt{pragma} Remote_Call_Interface is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Remote_Call_Interface)[(@SynI{library_unit_}@Syn2{name})];'

@begin{SyntaxText}
@Leading@;The form of a @nt{pragma} All_Calls_Remote is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(All_Calls_Remote)[(@SynI{library_unit_}@Syn2{name})];'

@begin{SyntaxText}
@PDefn2{Term=[library unit pragma], Sec=(All_Calls_Remote)}
@PDefn2{Term=[pragma, library unit], Sec=(All_Calls_Remote)}
A @nt{pragma} All_Calls_Remote is a library unit pragma.
@end{SyntaxText}

@end{Syntax}

@begin{Legality}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0078],ARef=[AI95-00048-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0243-1]}
@Defn{remote call interface}
@Defn2{Term=[RCI],Sec=(library unit)}
@Defn2{Term=[RCI],Sec=(package)}
@Defn2{Term=[RCI],Sec=(generic)}
@Defn{remote subprogram}
A@Chg{Version=[3],New=[ @nt{pragma} Remote_Call_Interface is used to specify that a
library unit is a],Old=[]} @i{remote call interface (RCI)}@Chg{Version=[3],New=[,
namely that the],Old=[ is a library unit to which the pragma]}
Remote_Call_Interface
@Chg{Version=[3],New=[aspect@AspectDefn{Remote_Call_Interface} of the library
unit is True],Old=[applies]}. A subprogram declared in the visible part of such
a library unit@Chg{New=[, or declared by such a library unit,],Old=[]} is called
a @i{remote subprogram}.

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Remote_Call_Interface],
    Text=[@ChgAdded{Version=[3],Text=[Subprograms in a given package may be
      used in remote procedure calls.]}]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0206-1],ARef=[AI05-0243-1]}
The declaration of an RCI library unit shall be preelaborable
(see @RefSecNum{Elaboration Control}), and shall depend semantically
only upon declared pure@Chg{Version=[3],New=[ @nt{library_item}s],Old=[]},
shared passive@Chg{Version=[3],New=[ library units],Old=[]},
remote types@Chg{Version=[3],New=[ library units],Old=[]},
@Chg{Version=[3],New=[],Old=[or ]}other remote call interface library
units@Chg{Version=[3],New=[, or preelaborated normal library units that are
mentioned only in private with clauses],Old=[]}.

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0243-1]}
  @ChgAdded{Version=[3],Text=[We say declared pure @nt{library_item} here,
  so that (all) limited views are allowed; those are not library units, but
  they are declared pure @nt{library_item}s.]}
@end{Ramification}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0078],ARef=[AI95-00048-01]}
@Leading@;In addition, the following restrictions apply to @Chg{New=[],Old=[the
visible part of ]}an RCI library unit:
@begin{itemize}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0078],ARef=[AI95-00048-01]}
it@Chg{New=[s visible part],Old=[]} shall not contain the declaration of a
variable;
@begin{Reason}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0078],ARef=[AI95-00048-01]}
  Remote call interface @Chg{New=[units],Old=[packages]} do not provide remote
  data access. A shared passive package has to be used for that.
@end{Reason}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0078],ARef=[AI95-00048-01]}
it@Chg{New=[s visible part],Old=[]} shall not contain the declaration of a
limited type;
@begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00240-01],ARef=[AI95-00366-01]}
  We disallow the declaration of task and protected types,
  since calling an entry or a protected subprogram implicitly passes
  an object of a limited type (the target task or protected object).
  We disallow other limited types since we require that such types
  have @Chg{Version=[2],New=[available],Old=[user-defined]} Read and Write
  attributes, but we certainly
  don't want the Read and Write attributes themselves to involve remote
  calls (thereby defeating their purpose of marshalling the
  value for remote calls).
@end{Reason}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0078],ARef=[AI95-00048-01]}
it@Chg{New=[s visible part],Old=[]} shall not contain a nested
@nt{generic_declaration};
@begin{Reason}
  This is disallowed because the body of the nested generic
  would presumably have access to data inside the body of the
  RCI package, and if instantiated in a different partition, remote
  data access might result, which is not supported.
@end{Reason}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0078],ARef=[AI95-00048-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
it shall not @Chg{New=[be, nor shall its visible part],Old=[]} contain@Chg{New=[,],Old=[]} the
declaration of a subprogram @Chg{Version=[3],New=[for],Old=[to]} which
@Chg{Version=[3],New=[aspect],Old=[a pragma]} Inline @Chg{Version=[3],New=[is True],
Old=[applies]};

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0078],ARef=[AI95-00048-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00240-01],ARef=[AI95-00366-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0101-1]}
it shall not @Chg{New=[be, nor shall its visible part],
Old=[]} contain@Chg{New=[,],Old=[]} a
subprogram (or access-to-subprogram) declaration
whose profile has @Chg{Version=[2],New=[@Chg{Version=[3],New=[],
Old=[@Redundant[an access parameter or]]}
a parameter @Chg{Version=[3],New=[or result ],Old=[]}of a type that
does not support external streaming
(see @RefSecNum{Stream-Oriented Attributes})],
Old=[an access parameter, or a formal parameter of a
limited type unless that limited type has user-specified Read and Write
attributes]};

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0101-1]}
@ChgAdded{Version=[3],Text=[No anonymous access types support external
streaming, so they are never allowed as parameters or results of
RCI subprograms.]}
@end{Ramification}

any public child of the library unit shall
be a remote call interface library unit.
@begin{Reason}
  No restrictions apply to the private part of an RCI package,
  and since a public child can @lquotes@;see@rquotes@; the private part
  of its parent, such a child must itself have a
  Remote_Call_Interface pragma, and be assigned to the same partition
  (see below).
@end{Reason}
@begin{Discussion}
  We considered making the public child of an RCI package
  implicitly RCI, but it seemed better to require an explicit
  pragma to avoid any confusion.

  Note that there is no need for a private child to be an RCI package,
  since it can only be seen from the body of its parent
  or its siblings, all of which are required to be in the
  same active partition.
@end{Discussion}
@end{itemize}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[A],Old=[If a]} pragma All_Calls_Remote
@Chg{Version=[3],New=[sets the All_Calls_Remote representation aspect of
@AspectDefn{All_Calls_Remote}the],Old=[applies to a]} library unit@Chg{Version=[3],New=[ to which the pragma
applies to the value True. If the All_Calls_Remote aspect of a library unit is True],
Old=[]}, the library unit shall be a remote call interface.

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[All_Calls_Remote],
    Text=[@ChgAdded{Version=[3],Text=[All remote procedure calls should use the
      Partition Communication Subsystem, even if they are local.]}]}

@end{Legality}

@begin{LinkTime}

A remote call interface library unit shall be
assigned to at most one partition of a given program.
A remote call
interface library unit whose parent is also an RCI library unit
shall be
assigned only to the same partition as its parent.
@begin{ImplNote}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0078],ARef=[AI95-00048-01]}
  The declaration of an RCI @Chg{New=[unit],Old=[package]}, with a calling-stub
  body, is automatically included in all active
  partitions with compilation units that depend on it.
  However the whole RCI library unit, including its
  (non-stub) body, will only be in one of the active partitions.
@end{ImplNote}

@PDefn2{Term=[compilation units needed], Sec=(remote call interface)}
@PDefn2{Term=[needed], Sec=(remote call interface)}
@Defn{notwithstanding}
Notwithstanding the rule given in @RefSecNum{Program Execution},
a compilation unit in a given partition that semantically depends on
the declaration of an RCI library unit,
@i{needs} (in the sense of @RefSecNum{Program Execution})
only the declaration of the
RCI library unit, not the body, to be included in that same partition.
@Redundant[Therefore, the body of an RCI library unit is included
only in the partition to which the RCI library unit is
explicitly assigned.]

@end{LinkTime}

@begin{ImplReq}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0078],ARef=[AI95-00048-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
If @Chg{Version=[3],New=[aspect],Old=[a pragma]} All_Calls_Remote
@Chg{Version=[3],New=[is True for],Old=[applies to]} a
given RCI library @Chg{New=[unit],Old=[package]}, then the
implementation shall route any call to a subprogram of the RCI
@Chg{New=[unit],Old=[package]} from outside the declarative region of the
@Chg{New=[unit],Old=[package]} through the Partition Communication Subsystem
(PCS); see @RefSecNum{Partition Communication Subsystem}.
Calls to such subprograms from within the declarative region of
the @Chg{New=[unit],Old=[package]} are defined to be local and shall
not go through the PCS.
@begin{Discussion}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0078],ARef=[AI95-00048-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[When this aspect is False (or not used)],Old=[Without
this pragma]}, it is presumed
that most implementations will
make direct calls if the call originates in the same partition
as that of the RCI @Chg{New=[unit],Old=[package]}. @Chg{Version=[3],New=[When
this aspect is True],Old=[With this pragma]}, all calls
from outside the subsystem rooted at the RCI @Chg{New=[unit],Old=[package]} are
treated like calls from outside the
partition, ensuring that the PCS is involved in all such calls
(for debugging, redundancy, etc.).
@end{Discussion}
@begin{Reason}
There is no point to force local calls (or calls from children) to go through
the PCS, since on the target system, these calls are always local,
and all the units are in the same active partition.
@end{Reason}
@end{ImplReq}

@begin{ImplPerm}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0243-1]}
An implementation need not support the
Remote_Call_Interface pragma @Chg{Version=[3],New=[or aspect ],Old=[]}nor
the All_Calls_Remote pragma.
@Redundant[Explicit message-based communication between active partitions can
be supported as an alternative to RPC.]
@begin{Ramification}
  Of course, it is pointless to support the All_Calls_Remote
  pragma if the Remote_Call_Interface pragma (or some
  approximate equivalent) is not supported.
@end{Ramification}

@end{ImplPerm}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00240-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @b[Amendment Correction:] The wording was changed from
  @lquotes@;user-specified@rquotes to @lquotes@;available@rquotes
  @Chg{Version=[3],New=[read and write ],Old=[]}attributes. (This was then
  further changed, see below.) This means that
  a type with the attributes specified in the private part would
  originally have been allowed as a formal parameter of an RCI subprogram,
  but that is no longer allowed.
  This change was made so that the rules were consistent with the rules
  introduced for the Corrigendum for stream attributes; moreover, legality
  should not depend on the contents of the private part.]}
@end{Incompatible95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0078],ARef=[AI95-00048-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Changed the wording to allow
  a library subprogram to be a remote call interface unit.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00366-01]}
  @ChgAdded{Version=[2],Text=[Changed the wording to use the newly defined
  term @i<type that supports external streaming>, so that various issues
  with access types in pure units and implicitly declared attributes for
  type extensions are properly handled.]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0101-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Added a rule to ensure that function results are streamable; this was
  missing in previous versions of Ada. While programs that returned
  unstreamable types from RCI functions were legal, it is not clear what
  they could have done (as the results could not be marshalled). Thus, it seems
  unlikely that any real programs will be impacted by this change.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0206-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}Added wording to
  allow private withs of preelaborated normal units in the specification of a
  remote call interface unit.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[All_Calls_Remote
  is now a representation aspect, so it can be specified by
  an @nt{aspect_specification} @em
  although the pragma is still preferred by the Standard.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0243-1]}
  @ChgAdded{Version=[3],Text=[Remote_Call_Interface
  is now a categorization aspect, so it can be specified by
  an @nt{aspect_specification} @em
  although the pragma is still preferred by the Standard.]}
@end{Extend2005}



@LabeledClause{Consistency of a Distributed System}

@begin{Intro}
@redundant[This clause defines attributes and rules associated with verifying
the consistency of a distributed program.
]
@end{Intro}

@begin{MetaRules}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0248-1]}
The rules guarantee that remote call interface
and shared passive @Chg{Version=[3],New=[library units],Old=[packages]}
are consistent among all partitions
prior to the execution of a distributed program, so
that the semantics of the distributed program are well defined.

@end{MetaRules}

@begin{StaticSem}

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
@Leading@;For @ChgPrefixType{Version=[1],Kind=[Revised],Text=[a
@Chg{New=[@nt{prefix}],Old=[prefix]} P that statically denotes a
program unit]}, the following attributes are defined:
@begin{Description}
@Attribute{Prefix=<P>, AttrName=<Version>,
  Text=[Yields a value of the predefined type String
     that identifies the version of the compilation unit that
     contains the declaration of the program unit.]}

@Attribute{Prefix=<P>, AttrName=<Body_Version>,
  Text=[Yields a value of the predefined type String
     that identifies the version of the compilation unit that contains
     the body (but not any subunits) of the program unit.]}
@end{Description}
@EndPrefixType{}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0084],ARef=[AI95-00104-01]}
@Defn2{Term=[version], Sec=(of a compilation unit)}
The @i{version} of a compilation unit changes whenever the
@Chg{New=[],Old=[version changes for any ]}compilation unit
@Chg{New=[changes in a semantically significant way. This International
Standard does not define the exact meaning of "semantically significant"],
Old=[on which it depends semantically. The version also changes whenever the
compilation unit itself changes in a semantically significant way]}.
It is @Chg{New=[unspecified],Old=[implementation defined]}
whether there are other events (such as recompilation) that
result in the version of a compilation unit changing.
@Chg{New=[@PDefn{unspecified}],Old=[]}
@ChgImplDef{Version=[1],Kind=[Deleted],InitialVersion=[0],
Text=[@Chg{New=[],Old=[Events that
cause the version of a compilation unit to change.]}]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0084],ARef=[AI95-00104-01]}
@ChgAdded{Version=[1],Text=[If P is not a library unit, and P has no completion,
then P'Body_Version returns the Body_Version of the innermost program unit
enclosing the declaration of P. If P is a library unit, and P has no
completion, then P'Body_Version returns a value that is different from
Body_Version of any version of P that has a completion.]}
@end{StaticSem}

@begin{Bounded}
@PDefn2{Term=(bounded error),Sec=(cause)}
@Defn{unit consistency}
In a distributed program, a library unit is @i{consistent} if the same
version of its declaration is used throughout.
It is a bounded error to elaborate a partition of a distributed
program that contains a compilation unit that depends on a different
version of the declaration of a shared passive or RCI library
unit than
that included in the partition to which the shared passive or
RCI library unit
was assigned.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
As a result of this error, Program_Error
can be raised in one or both partitions during elaboration;
in any case, the partitions
become inaccessible to one another.
@begin{Ramification}
  Because a version changes if anything on which it depends undergoes
  a version change, requiring consistency for shared passive
  and remote call interface library units is
  sufficient to ensure
  consistency for the declared pure and remote types library
  units that
  define the types used for the objects and parameters
  through which interpartition communication takes place.

  Note that we do not require matching Body_Versions; it is
  irrelevant for shared passive and remote call interface
  packages, since only one copy of their body exists in a
  distributed program (in the absence of implicit replication),
  and we allow the bodies to differ for declared pure and
  remote types packages from partition to partition, presuming
  that the differences are due to required error corrections
  that took place during the execution of a long-running distributed
  program. The Body_Version attribute provides a means
  for performing stricter consistency checks.
@end{Ramification}
@end{Bounded}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0084],ARef=[AI95-00104-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified the meaning of
  'Version and 'Body_Version.]}
@end{DiffWord95}


@LabeledClause{Remote Subprogram Calls}

@begin{Intro}
@Defn{remote subprogram call}
@RootDefn{asynchronous remote procedure call}
@Defn{calling partition}
@Defn{called partition}
@Defn{remote subprogram binding}
A @i{remote subprogram call} is a subprogram call that invokes the
execution of a subprogram in another partition.
The partition that
originates the remote subprogram call is the @i{calling partition}, and the
partition that executes the corresponding subprogram body is the @i{called
partition}.
Some remote procedure calls are allowed to return prior to the completion of
subprogram execution. These are called @i{asynchronous remote procedure
calls}.

@Leading@;There are three different ways of performing a remote subprogram call:
@begin{Itemize}
  As a direct call on a (remote) subprogram explicitly declared in a
  remote call interface;

  As an indirect call through a value of a remote access-to-subprogram type;

  As a dispatching call with a controlling operand designated by
  a value of a remote access-to-class-wide type.
@end{Itemize}

The first way of calling corresponds to a @i(static) binding between
the calling and the called partition.
The latter two ways correspond to a @i(dynamic) binding between
the calling and the called partition.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0101-1]}
@Chg{Version=[3],New=[Remote types library units (see
@RefSecNum{Remote Types Library Units}) and],Old=[A]} remote
call interface library @Chg{Version=[3],New=[units],Old=[unit]}
(see @RefSecNum{Remote Call Interface Library Units})
@Chg{Version=[3],New=[define],Old=[defines]}
the remote subprograms or remote access types used for
remote subprogram calls.
@end{Intro}

@begin{MetaRules}

Remote subprogram calls are standardized since the RPC paradigm is
widely-used, and establishing an interface to it in the annex will
increase the portability and reusability of distributed
programs.

@end{MetaRules}

@begin{Legality}
In a dispatching call with two or more controlling operands,
if one controlling operand is designated by a value of a
remote access-to-class-wide type, then all shall be.
@end{Legality}

@begin{RunTime}

@Defn{marshalling}
@Defn{unmarshalling}
@PDefn2{Term=[execution], Sec=(remote subprogram call)}
For the execution of a remote subprogram call, subprogram parameters
(and later the results, if any) are passed using a stream-oriented
representation
(see @RefSecNum{The Package Streams}) @Redundant[which is
suitable for transmission between partitions]. This action is called
@i{marshalling}. @i{Unmarshalling} is the reverse action of
reconstructing the parameters or results from the stream-oriented
representation.
@redundant[Marshalling is performed initially as part of the remote subprogram call in
the calling partition; unmarshalling is done in the called partition.
After the remote subprogram completes, marshalling is performed in the
called partition, and finally unmarshalling is done in the calling
partition.]

@Defn{calling stub}
@Defn{receiving stub}
A @i{calling stub} is the sequence of code that replaces the subprogram
body of a remotely called subprogram in the calling partition. A
@i{receiving stub} is the sequence of code (the @lquotes@;wrapper@rquotes@;) that
receives a remote subprogram call on the called partition and invokes
the appropriate subprogram body.
@begin{Discussion}
The use of the term @i{stub} in this annex should not be confused with
@nt{body_stub} as defined in @RefSecNum{Subunits of Compilation Units}. The
term @i{stub} is used here because it is a commonly understood term when
talking about the RPC paradigm.
@end{Discussion}

@Defn{at-most-once execution}
Remote subprogram calls are executed at most once, that is,
if the subprogram call returns normally, then the called
subprogram's body was executed exactly once.

The task executing a remote subprogram call blocks until the subprogram
in the called partition returns, unless the call is asynchronous. For an
asynchronous remote procedure call, the calling task can
become ready before the
procedure in the called partition returns.

@Defn{cancellation of a remote subprogram call}
If a construct containing a remote call is aborted, the
remote subprogram call is @i{cancelled}.
Whether the execution of the remote subprogram is immediately aborted
as a result of the cancellation is implemen@!tation defined.
@ImplDef{Whether the execution of the remote subprogram is immediately
aborted as a result of cancellation.}

If a remote subprogram call is received by a called partition before the
partition has
completed its elaboration, the call is kept pending until the called
partition completes its elaboration (unless the call is
cancelled by the calling partition prior to that).

If an exception is propagated by a remotely called subprogram,
and the call is not an asynchronous call,
the corresponding exception is reraised at the point
of the remote subprogram call.
For an asynchronous call, if the remote procedure call returns prior to
the completion of the remotely called subprogram, any exception is lost.

The exception Communication_Error
(see @RefSecNum{Partition Communication Subsystem}) is raised if a remote call
cannot be completed due to difficulties in communicating with the called
partition.

@PDefn2{Term=[potentially blocking operation],Sec=(remote subprogram call)}
@PDefn2{Term=[blocking, potentially],Sec=(remote subprogram call)}
All forms of remote subprogram calls are potentially blocking operations
(see @RefSecNum{Protected Subprograms and Protected Actions}).
@begin{Reason}
Asynchronous remote procedure calls are potentially blocking since the
implementation may require waiting for the availability of shared resources
to initiate the remote call.
@end{Reason}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0085],ARef=[AI95-00215-01]}
@IndexCheck{Accessibility_Check}
In a remote subprogram call with a formal parameter of a class-wide
type, a check is made that the tag of the actual parameter identifies
a tagged type declared in a declared-pure or shared passive library
unit, or in the visible part of a remote types or remote call interface
library unit.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
Program_Error is raised if this check fails.
@Chg{New=[In a remote function call which returns a class-wide type, the same
check is made on the function result.],Old=[]}
@begin{Discussion}
  @ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0085],ARef=[AI95-00215-01]}
  This check makes certain that the specific type passed@Chg{New=[ or returned],
  Old=[]} in an RPC
  satisfies the rules for a "communicable" type. Normally this
  is guaranteed by the compile-time restrictions on remote call interfaces.
  However, with class-wide types, it is possible to pass an object
  whose tag identifies a type declared outside the "safe" packages.

  This is considered an accessibility_check since only the types
  declared in "safe" packages are considered truly "global" (cross-partition).
  Other types are local to a single partition. This is analogous
  to the "accessibility" of global vs. local declarations in
  a single-partition program.

  This rule replaces a rule from an early version of Ada 9X
  which was given in the subclause on Remote Types Library Units
  (now @RefSec{Remote Types Library Units}). That rule tried
  to prevent "bad" types from being sent by arranging for their
  tags to mismatch between partitions. However, that interfered
  with other uses of tags. The new rule allows tags to agree
  in all partitions, even for those types which are not "safe"
  to pass in an RPC.
@end{Discussion}

@IndexCheck{Partition_Check}
In a dispatching call with two or more controlling operands that are
designated by values of a remote access-to-class-wide type,
a check is made @Redundant[(in addition to the normal Tag_Check
@em see @RefSecNum{Suppressing Checks})] that all the remote
access-to-class-wide values originated from Access
@nt<attribute_reference>s that were evaluated by tasks of the
same active partition.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
Constraint_Error is raised if this check fails.
@begin{ImplNote}
  When a remote access-to-class-wide value is created by
  an Access @nt<attribute_reference>, the identity of the
  active partition that evaluated the @nt<attribute_reference>
  should be recorded in the representation of the remote access value.
@end{ImplNote}

@end{RunTime}

@begin{ImplReq}

The implementation of remote subprogram calls shall conform
to the PCS interface as defined by the specification of the
language-defined package System.RPC
(see @RefSecNum{Partition Communication Subsystem}).
The calling stub shall use the Do_RPC procedure unless the remote
procedure call is asynchronous in which case Do_APC shall be used.
On the receiving side, the corresponding receiving stub shall be
invoked by the RPC-receiver.
@begin{ImplNote}
One possible implementation model is as follows:

The code for calls to subprograms declared in an RCI
package is generated
normally, that is, the call-site is the same as for a
local subprogram call.
The code for the remotely callable subprogram bodies is also generated
normally. Subprogram's prologue and epilogue are the same as for a local call.

When compiling the specification of an RCI package, the compiler
generates calling stubs for each visible subprogram. Similarly, when
compiling the body of an RCI package, the compiler generates
receiving stubs for each visible subprogram together with the appropriate
tables to allow the RPC-receiver to locate the correct receiving stub.

For the statically bound remote calls, the identity of the remote
partition is statically determined (it is resolved at configuration/link time).

@Leading@;The calling stub operates as follows:
@begin(Itemize)
It allocates (or reuses) a stream of Params_Stream_Type of Initial_Size,
and initializes it by repeatedly calling Write operations,
first to identify which remote subprogram in the receiving partition
is being called, and then to pass the incoming value of each of the
@b(in) and @b(in out) parameters of the call.


@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
It allocates (or reuses) a stream for the Result,
unless @Chg{Version=[3],New=[an aspect],Old=[a pragma]} Asynchronous is
@Chg{Version=[3],New=[specified as True for],Old=[applied to]} the procedure.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
It calls Do_RPC unless @Chg{Version=[3],New=[an aspect],Old=[a pragma]}
Asynchronous @Chg{Version=[3],New=[is specified as True for],Old=[applied to]}
the procedure
in which case it calls Do_APC. An access value designating the message
stream allocated and initialized above is passed as the Params parameter.
An access value designating the Result stream is passed as the Result
parameter.


@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
If the @Chg{Version=[3],New=[aspect],Old=[pragma]} Asynchronous is not specified for the procedure,
Do_RPC blocks until a reply message arrives,
and then returns to the calling stub.
The stub returns after extracting
from the Result stream, using Read operations, the @b(in out) and @b(out)
parameters or the function result. If the reply message
indicates that the execution of the remote subprogram propagated
an exception, the exception is propagated from Do_RPC to the
calling stub, and thence to the point of the original remote subprogram call.
If Do_RPC detects that communication with the remote partition has
failed, it propagates Communication_Error.

@end(Itemize)

@Leading@;On the receiving side, the RPC-receiver procedure operates as follows:
@begin(Itemize)
It is called from the PCS when a remote-subprogram-call message is received.
The call originates in some remote call receiver task executed and
managed in the context of the PCS.

It extracts information from the stream to identify the appropriate
receiving stub.

The receiving stub extracts the @b(in) and @b(in out) parameters using
Read from the stream designated by the Params parameter.

The receiving stub calls the actual subprogram body and, upon completion of
the subprogram, uses Write to insert the results into the
stream pointed to by the Result parameter. The receiving stub returns to
the RPC-receiver procedure which in turn returns to the PCS.
If the actual subprogram body propagates an exception, it is propagated
by the RPC-receiver to the PCS, which handles the exception,
and indicates in the reply message that the execution of the
subprogram body propagated an exception. The exception occurrence
can be represented in the reply message using the Write attribute
of Ada.Exceptions.Exception_Occurrence.
@end(Itemize)

For remote access-to-subprogram types:

A value of a remote access-to-subprogram type can be
represented by the following components: a reference to the remote partition,
an index to the package containing the remote subprogram, and an index to
the subprogram within the package. The values of these components are
determined at run time when the remote access value is created.
These three components serve the same purpose when calling Do_APC/RPC,
as in the statically bound remote calls; the only difference is that they
are evaluated dynamically.

For remote access-to-class-wide types:

For each remote access-to-class-wide type,
a calling stub is generated for each dispatching operation of
the designated type. In addition, receiving stubs are generated to perform
the remote dispatching operations in the called partition. The appropriate
@nt{subprogram_body} is determined as for a local
dispatching call once the receiving stub has been reached.

A value of a remote access-to-class-wide type
can be represented with the following components:
a reference to the remote partition, an index to a table (created one per
each such access type) containing addresses of all the dispatching operations
of the designated type, and an access value designating the actual remote
object.

Alternatively, a remote access-to-class-wide value can be represented as
a normal access value, pointing to a "stub" object which in turn
contains the information mentioned above. A call on any
dispatching operation of such a stub object does the remote call,
if necessary, using the information in the stub object to locate
the target partition, etc. This approach has the advantage that
less special-casing is required in the compiler. All access values
can remain just a simple address.

@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
For a call to Do_RPC or Do_APC: The partition ID of all controlling
operands are checked for equality (a Constraint_Error is raised if this
check fails). The partition ID value is used for the Partition parameter.
An index into the @i{tagged-type-descriptor} is created. This index
points to the receiving stub of the class-wide operation. This index and
the index to the table (described above) are
written to the stream.
Then, the actual parameters are marshalled into the message
stream. For a controlling operand, only the access value
designating the remote object is required (the other two
components are already present in the other parameters).

On the called partition (after the RPC-receiver has transferred control
to the appropriate receiving stub) the parameters are first unmarshalled.
Then, the tags of the controlling operands (obtained by dereferencing the
pointer to the object) are checked for equality.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
If the check fails Constraint_Error is raised and propagated back to the
calling partition, unless it is a result of an asynchronous call.
Finally, a dispatching call to the specific subprogram (based on the
controlling object's tag) is made. Note that since this subprogram is not
in an RCI package, no specific stub is generated for it, it is called
normally from the @i{dispatching stub}.

@end{ImplNote}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0086],ARef=[AI95-00159-01]}
@ChgAdded{Version=[1],Text=[With respect to shared variables in shared
passive library units, the
execution of the corresponding subprogram body of a synchronous remote
procedure call is considered to be part of the execution of the calling task.
The execution of the corresponding subprogram body of an asynchronous remote
procedure call proceeds in parallel with the calling task and does not signal
the next action of the calling task (see @RefSecNum{Shared Variables}).]}
@end{ImplReq}

@begin{Notes}
A given active partition can both make and receive remote subprogram calls.
Thus, an active partition can act as both a client and a server.

If a given exception is propagated by a remote
subprogram call, but the exception does not exist in the calling
partition, the exception can be handled by an @key(others) choice
or be propagated to and handled by a third partition.
@begin{Discussion}
This situation can happen
in a case of dynamically nested remote subprogram calls, where an
intermediate call executes in a partition that does
not include the library unit that defines the exception.
@end{Discussion}
@end{Notes}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0086],ARef=[AI95-00159-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added rules so that tasks can
  safely access shared passive objects.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0085],ARef=[AI95-00215-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that the check on
  class-wide types also applies to values returned from remote subprogram call
  functions.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0101-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected the text to
  note that remote access types can be defined in remote types units.]}
@end{DiffWord2005}


@LabeledRevisedSubClause{Version=[3],New=[Asynchronous Remote Calls],Old=[Pragma Asynchronous]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@redundant[This subclause introduces the
@Chg{Version=[3],New=[aspect],Old=[pragma]} Asynchronous which
@Chg{Version=[3],New=[can be specified to allow],Old=[allows]} a
remote subprogram call to return prior to completion of the execution
of the corresponding remote subprogram body.]
@end{Intro}

@begin{NotIso}
@ChgAdded{Version=[3],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 2
through 7 were deleted.>}]}@Comment{This message
should be deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@begin{Syntax}
@begin{SyntaxText}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Type=[Leading],Text=[The form of a @nt{pragma}
Asynchronous is as follows:]}
@end{SyntaxText}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@DeletedPragmaSyn<Version=[3],InitialVersion=[0],@ChgDeleted{Version=[3],
Text=[@key{pragma} @prag(Asynchronous)(@Syn2{local_name});]}>
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Type=[Leading],Text=[The @nt<local_name> of a pragma
Asynchronous shall denote either:]}
@begin{Itemize}
   @ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
   @ChgDeleted{Version=[3],Text=[One or more remote procedures;
   the formal parameters of the procedure(s) shall all be of
   mode @key{in};]}

   @ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
   @ChgDeleted{Version=[3],Text=[The first subtype of a remote
   access-to-procedure type; the formal parameters
   of the designated profile of
   the type shall all be of mode @key{in};]}

   @ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
   @ChgDeleted{Version=[3],Text=[The first subtype of a remote
   access-to-class-wide type.]}
@end{Itemize}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[]}@Comment{Fake to get conditional Leading}
@Chg{Version=[3],New=[For a remote procedure, the following language-defined
representation aspect may be specified:],
Old=[@PDefn2{Term=[representation pragma], Sec=(Asynchronous)}
@PDefn2{Term=[pragma, representation], Sec=(Asynchronous)}
A pragma Asynchronous is a representation pragma.
When applied to a type, it specifies the type-related @i{asynchronous}
aspect of the type.]}
@begin{Description}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Asynchronous@\The type of aspect Asynchronous is
Boolean. If directly specified, the @nt{aspect_definition} shall be a static
expression. If not specified, the aspect is False.@AspectDefn{Asynchronous}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Asynchronous],
    Text=[@ChgAdded{Version=[3],Text=[Remote procedure calls are asynchronous;
      the caller continues without waiting for the call to return.]}]}

@end{Description}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For a remote access type, the
following language-defined representation aspect may be specified:]}
@begin{Description}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Asynchronous@\The type of aspect Asynchronous is
Boolean. If directly specified, the @nt{aspect_definition} shall be a static
expression. If not specified (including by inheritance), the aspect is False.]}
@end{Description}
@end{StaticSem}

@begin{Legality}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[If aspect Asynchronous is specified for a remote
procedure, the formal parameters of the procedure shall all be of mode
@key[in].]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[If aspect Asynchronous is specified for a remote
access type, the type shall be a remote access-to-class-wide type, or the type
shall be a remote access-to-procedure type with the formal parameters of the
designated profile of the type all of mode @key[in].]}
@end{Legality}

@begin{RunTime}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Defn2{Term=[remote procedure call],Sec=(asynchronous)}
@Defn2{Term=[asynchronous], Sec=(remote procedure call)}
A remote call is @i{asynchronous} if it is a call to a procedure,
or a call through a value of an access-to-procedure type,
@Chg{Version=[3],New=[for],Old=[to]} which
@Chg{Version=[3],New=[aspect],Old=[a pragma]} Asynchronous
@Chg{Version=[3],New=[is True],Old=[applies]}.
In addition, if @Chg{Version=[3],New=[aspect],Old=[a pragma]} Asynchronous
@Chg{Version=[3],New=[is True for],Old=[applies to]} a remote
access-to-class-wide type, then
a dispatching call on a procedure
with a controlling operand designated by a value of the type
is asynchronous if the
formal parameters of the procedure are all of mode @key{in}.
@end{RunTime}

@begin{ImplReq}

Asynchronous remote procedure calls shall be implemented such that the
corresponding body executes at most once as a result of the call.
@begin{Honest}
It is not clear that this rule can be tested or even defined formally.
@end{Honest}
@end{ImplReq}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Aspect Asynchronous is new; @nt{pragma} Asynchronous is now obsolescent.]}
@end{Extend2005}


@LabeledSubClause{Example of Use of a Remote Access-to-Class-Wide Type}

@begin{Examples}
@Leading@i{Example of using a remote access-to-class-wide type to achieve dynamic binding
across active partitions:}
@begin{example}
@key{package} Tapes @key{is}
   @key{pragma} Pure(Tapes);
   @key{type} Tape @key{is abstract tagged limited private};
   @RI{-- Primitive dispatching operations where}
   @RI{-- Tape is controlling operand}
   @key{procedure} Copy (From, To : @key{access} Tape; Num_Recs : @key[in] Natural) @key{is} @key{abstract};
   @key{procedure} Rewind (T : @key{access} Tape) @key{is} @key{abstract};
   @RI{-- More operations}
@key{private}
   @key{type} Tape @key{is} ...
@key{end} Tapes;

@key{with} Tapes;
@key{package} Name_Server @key{is}
   @key{pragma} Remote_Call_Interface;
   @RI{-- Dynamic binding to remote operations is achieved}
   @RI{-- using the access-to-limited-class-wide type Tape_Ptr}
   @key{type} Tape_Ptr @key{is access all} Tapes.Tape'Class;
   @RI{-- The following statically bound remote operations}
   @RI{-- allow for a name-server capability in this example}
   @key{function}  Find     (Name : String) @key{return} Tape_Ptr;
   @key{procedure} Register (Name : @key[in] String; T : @key[in] Tape_Ptr);
   @key{procedure} Remove   (T : @key[in] Tape_Ptr);
   @RI{-- More operations}
@key{end} Name_Server;

@key{package} Tape_Driver @key{is}
  @RI{-- Declarations are not shown, they are irrelevant here}
@key{end} Tape_Driver;

@key{with} Tapes, Name_Server;
@key{package body} Tape_Driver @key{is}
   @key{type} New_Tape @key{is new} Tapes.Tape @key{with} ...
   @key{procedure} Copy
    (From, To : @key{access} New_Tape; Num_Recs: @key[in] Natural) @key{is}
   @key{begin}
     . . .
   @key{end} Copy;
   @key{procedure} Rewind (T : @key{access} New_Tape) @key{is}
   @key{begin}
      . . .
   @key{end} Rewind;
   @RI{-- Objects remotely accessible through use}
   @RI{-- of Name_Server operations}
   Tape1, Tape2 : @key[aliased] New_Tape;
@key{begin}
   Name_Server.Register ("NINE-TRACK",  Tape1'Access);
   Name_Server.Register ("SEVEN-TRACK", Tape2'Access);
@key{end} Tape_Driver;

@key{with} Tapes, Name_Server;
@RI{-- Tape_Driver is not needed and thus not mentioned in the @nt{with_clause}}
@key{procedure} Tape_Client @key{is}
   T1, T2 : Name_Server.Tape_Ptr;
@key{begin}
   T1 := Name_Server.Find ("NINE-TRACK");
   T2 := Name_Server.Find ("SEVEN-TRACK");
   Tapes.Rewind (T1);
   Tapes.Rewind (T2);
   Tapes.Copy (T1, T2, 3);
@key{end} Tape_Client;
@end{example}

@leading@keepnext@i{Notes on the example}:
@begin{Discussion}
The example does not show the case where tapes are removed from or added to
the system. In the former case, an appropriate exception needs to be defined
to instruct the client to use another tape. In the latter, the Name_Server
should have a query function visible to the clients to inform them about the
availability of the tapes in the system.
@end{Discussion}

@ChgRef{Version=[1], Kind=[Deleted]}
@Chg[New=<>,Old=<@ @;@comment{Empty paragraph to hang junk paragraph number from original RM}>]

@begin{itemize}
The package Tapes provides the necessary declarations of the type and its
primitive operations.

Name_Server is a remote call interface package and is elaborated in a separate
active partition to provide the necessary naming services (such as Register
and Find) to the entire distributed program
through remote subprogram calls.

Tape_Driver is a normal package that is elaborated in a partition configured
on the processing node that is connected to the tape device(s). The abstract
operations are overridden to support
the locally declared tape devices (Tape1, Tape2). The package is not visible
to its clients, but it exports the tape devices (as remote objects)
through the services of the Name_Server. This allows for tape devices to be
dynamically added, removed or replaced without requiring the modification of
the clients' code.

The Tape_Client procedure references only declarations in the Tapes and
Name_Server packages. Before using a tape for the first time, it needs to
query the Name_Server for a system-wide identity for that tape. From then on,
it can use that identity to access the tape device.

Values of remote access type Tape_Ptr include the necessary information to
complete the remote dispatching operations that result from
dereferencing the controlling operands T1 and T2.

@end{itemize}
@end{Examples}


@LabeledClause{Partition Communication Subsystem}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00273-01]}
@Defn{partition communication subsystem (PCS)}
@Defn{PCS (partition communication subsystem)}
@redundant[The @i{Partition Communication Subsystem} (PCS) provides facilities for
supporting communication between the active partitions of a distributed
program. The package System.RPC is a language-defined interface to the
PCS.]@Chg{Version=[2],New=[],Old=[ An implementation conforming to this
Annex shall use the RPC interface to implement remote subprogram calls.]}
@begin{Reason}
The prefix RPC is used rather than RSC because the term remote procedure call
and its acronym are more familiar.
@end{Reason}
@end{Intro}

@begin{StaticSem}

@leading@keepnext@;The following language-defined library package exists:
@begin{example}
@b(with) Ada.Streams; @RI{-- see @RefSecNum[The Package Streams]}
@key(package) System.RPC @key(is)@ChildUnit{Parent=[System],Child=[RPC]}

   @key(type) @AdaTypeDefn{Partition_Id} @key(is range) 0 .. @RI(implementation-defined);

   @AdaExcDefn{Communication_Error} : @key(exception);

   @key(type) @AdaTypeDefn{Params_Stream_Type} (
      Initial_Size : Ada.Streams.Stream_Element_Count) @key(is) @key(new)
      Ada.Streams.Root_Stream_Type @key(with) @key(private);

   @key(procedure) @AdaSubDefn{Read}(
      Stream : @key(in out) Params_Stream_Type;
      Item : @key(out) Ada.Streams.Stream_Element_Array;
      Last : @key(out) Ada.Streams.Stream_Element_Offset);

   @key(procedure) @AdaSubDefn{Write}(
      Stream : @key(in out) Params_Stream_Type;
      Item : @key(in) Ada.Streams.Stream_Element_Array);


   @RI(-- Synchronous call)
   @key(procedure) @AdaSubDefn{Do_RPC}(
      Partition  : @key(in) Partition_Id;
      Params     : @key(access) Params_Stream_Type;
      Result     : @key(access) Params_Stream_Type);

   @RI(-- Asynchronous call)
   @key(procedure) @AdaSubDefn{Do_APC}(
      Partition  : @key(in) Partition_Id;
      Params     : @key(access) Params_Stream_Type);

   @RI(-- The handler for incoming RPCs)
   @key(type) @AdaTypeDefn{RPC_Receiver} @key(is access procedure)(
      Params     : @key(access) Params_Stream_Type;
      Result     : @key(access) Params_Stream_Type);

   @key(procedure) @AdaSubDefn{Establish_RPC_Receiver}(
      Partition : @key(in) Partition_Id;
      Receiver  : @key(in) RPC_Receiver);

@key[private]
   ... -- @RI{not specified by the language}
@b(end) System.RPC;
@end{example}

A value of the type Partition_Id is used to identify a partition.
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[
The range of type System.RPC.Partition_Id.]}]}

An object of the type Params_Stream_Type is used for identifying
the particular remote subprogram that is being called, as
well as marshalling and
unmarshalling the parameters or result of a remote
subprogram call, as part of sending them between partitions.

@redundant[The Read and Write procedures override the corresponding abstract operations
for the type Params_Stream_Type.]

@end{StaticSem}

@begin{RunTime}

The Do_RPC and Do_APC procedures send a message to the active partition
identified by the Partition parameter.
@begin{ImplNote}
It is assumed that the RPC interface is above the message-passing
layer of the network protocol stack and is implemented in terms of it.
@end{ImplNote}

After sending the message, Do_RPC blocks the calling task until a reply
message comes back from the called partition or some error is detected by
the underlying communication system in which case Communication_Error is
raised at the point of the call to Do_RPC.
@begin{Reason}
Only one exception is defined in System.RPC, although many sources of errors
might exist. This is so because it is not always possible
to distinguish among these errors. In particular, it is often impossible to
tell the difference between a failing communication link and
a failing processing node. Additional information might be associated with
a particular Exception_Occurrence for a Communication_Error.
@end{Reason}

Do_APC operates in the same way as Do_RPC except that it
is allowed to return immediately after sending the message.

Upon normal return, the stream designated by the Result parameter
of Do_RPC contains the reply message.

@PDefn2{Term=[elaboration], Sec=(partition)}
The procedure System.RPC.Establish_RPC_Receiver is called
once, immediately after elaborating
the library units of an active partition
(that is, right after the @i{elaboration of the partition}) if the partition
includes an RCI library unit, but prior to invoking the main
subprogram, if any.
The Partition parameter is the Partition_Id of the active partition
being elaborated.
@Defn{RPC-receiver}
The Receiver parameter designates an
implementation-provided procedure called the
@i{RPC-receiver} which will handle all RPCs received by the
partition from the PCS.
Establish_RPC_Receiver saves a reference to the RPC-receiver;
when a message is received at the called partition, the RPC-receiver
is called with the Params stream containing
the message. When the RPC-receiver
returns, the contents of the stream designated by Result is
placed in a message and sent back to the calling partition.
@begin{ImplNote}
It is defined by the PCS implementation whether one or more threads of control
should be available to process incoming messages and to wait for their
completion.
@end{ImplNote}
@begin{ImplNote}
At link-time, the linker provides the RPC-receiver and the necessary tables
to support it. A call on Establish_RPC_Receiver is inserted just before
the call on the main subprogram.
@end{ImplNote}
@begin{Reason}
The interface between the PCS (the System.RPC package) and the
RPC-receiver is defined to be dynamic in order to allow the elaboration
sequence to notify the PCS that all packages have been
elaborated and that it is safe to call the receiving stubs. It is not
guaranteed that the PCS units will be the last to be elaborated, so some
other indication that elaboration is complete is needed.
@end{Reason}

If a call on Do_RPC is aborted, a cancellation message is sent
to the called partition, to request that the execution of the
remotely called subprogram be aborted.
@begin{Honest}
The full effects of this message are dependent on the implementation of
the PCS.
@end{Honest}

@PDefn2{Term=[potentially blocking operation],Sec=(RPC operations)}
@PDefn2{Term=[blocking, potentially],Sec=(RPC operations)}
The subprograms declared in System.RPC are potentially
blocking operations.
@end{RunTime}

@begin{ImplReq}
The implementation of the RPC-receiver shall be reentrant@Redundant[,
thereby allowing concurrent calls on it from the PCS to service
concurrent remote subprogram calls into the partition].
@begin{Reason}
  There seems no reason to allow the implementation of RPC-receiver
  to be nonreentrant, even though we don't require that every
  implementation of the PCS actually perform concurrent calls
  on the RPC-receiver.
@end{Reason}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0087],ARef=[AI95-00082-01]}
@ChgAdded{Version=[1],Text=[An implementation shall not restrict the
replacement of the body of
System.RPC. An implementation shall not restrict children of System.RPC.
@Redundant[The related implementation permissions in the introduction to
Annex A do not apply.]]}
@begin{Reason}
@ChgRef{Version=[1],Kind=[Added]}
@ChgAdded{Version=[1],Text=[The point of System.RPC is to let the user tailor
the communications
mechanism without requiring changes to or other cooperation from the compiler.
However, implementations can restrict the replacement of language-defined units.
This requirement overrides that permission for System.RPC.]}
@end{Reason}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0087],ARef=[AI95-00082-01]}
@ChgAdded{Version=[1],Text=[If the implementation of System.RPC is provided by
the user, an implementation shall support remote subprogram calls as specified.]}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00273-01]}
@ChgAdded{Version=[2],Text=[If the implementation takes advantage of the
implementation permission to use a different specification for System.RPC,
it still needs to use it for remote subprogram calls, and allow the user
to replace the body of System.RPC. It just isn't guaranteed to be portable
to do so in Ada 2005 - an advantage which was more theoretical than real
anyway.]}
@end{Discussion}
@end{ImplReq}

@begin{DocReq}

The implementation of the PCS shall document whether
the RPC-receiver is invoked from concurrent tasks. If there is an
upper limit on the number of such tasks, this limit shall be documented as
well, together with the mechanisms to configure it (if this is supported).
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Implementation-defined aspects of the PCS.]}]}
@ChgDocReq{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Whether the RPC-receiver is invoked from concurrent tasks, and if so,
the number of such tasks.]}]}

@end{DocReq}

@begin{ImplPerm}

The PCS is allowed to contain implementation-defined interfaces for
explicit message passing, broadcasting, etc. Similarly, it is allowed
to provide additional
interfaces to query the state of some remote partition (given its partition
ID) or of the PCS itself, to set timeouts and retry parameters, to get more
detailed error status, etc. These
additional interfaces should be provided in child packages of System.RPC.
@ImplDef{Implementation-defined interfaces in the PCS.}

A body for the package System.RPC need not be supplied by the
implementation.
@begin{Reason}
  It is presumed that a body for the package System.RPC might be
  extremely environment specific. Therefore, we do not require
  that a body be provided by the (compiler) implementation. The user
  will have to write a body, or acquire one, appropriate for the
  target environment.
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00273-01]}
@ChgAdded{Version=[2],Text=[An alternative declaration is allowed for
package System.RPC as long as it provides a set of operations that is
substantially equivalent to the specification defined in this clause.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Experience has proved that the definition
  of System.RPC given here is inadequate for interfacing to existing
  distribution mechanisms (such as CORBA), especially on heterogeneous
  systems. Rather than mandate a change in the mechanism (which would
  break existing systems), require implementations to support multiple
  mechanisms (which is impractical), or prevent the use of Annex E facilities
  with existing systems (which would be silly), we simply make this facility
  optional.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[One of the purposes behind System.RPC was
  that knowledgeable users, rather than compiler vendors, could create
  this package tailored to their networks. Experience has shown that
  users get their RPC from vendors anyway; users have not taken advantage
  of the flexibility provided by this defined interface. Moreover, one
  could compare this defined interface to requiring Ada compilers to use
  a defined interface to implement tasking. No one thinks that the latter is
  a good idea, why should anyone believe that the former is?]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Therefore, this clause is made optional.
  We considered deleting the clause outright, but we still require that
  users may replace the package (whatever its interface). Also, it still
  provides a useful guide to the implementation of this feature.]}
@end{Reason}
@end{ImplPerm}

@begin{ImplAdvice}

Whenever possible, the PCS on the called partition should
allow for multiple tasks to call the RPC-receiver with
different messages and should allow them to block until the corresponding
subprogram body returns.
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The PCS should allow for multiple tasks to call the RPC-receiver.]}]}

The Write operation on a stream of type Params_Stream_Type should raise
Storage_Error if it runs out of space trying to write the Item
into the stream.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[The System.RPC.Write operation should raise Storage_Error if it runs out
of space when writing an item.]}]}
@begin{ImplNote}
  An implementation could also dynamically allocate more space
  as needed, only propagating Storage_Error if the @nt<allocator>
  it calls raises Storage_Error. This storage could be managed
  through a controlled component of the stream object, to
  ensure that it is reclaimed when the stream object
  is finalized.
@end{ImplNote}

@end{ImplAdvice}

@begin{Notes}
  The package System.RPC is not designed for direct calls by user
  programs. It is instead designed for use in the implementation of remote
  subprograms calls, being called by the calling stubs generated for a
  remote call interface library unit to initiate a remote call,
  and in turn calling back to an
  RPC-receiver that dispatches to the receiving stubs generated
  for the body of a remote call interface, to handle a remote call
  received from elsewhere.
@end{Notes}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00273-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  The specification of System.RPC can now be
  tailored for an implementation. If a program replaces the body of System.RPC
  with a user-defined body, it might not compile in a given implementation
  of Ada 2005 (if the specification of System.RPC has been changed).]}
@end{Incompatible95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0087],ARef=[AI95-00082-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that the user can
  replace System.RPC.]}
@end{DiffWord95}
