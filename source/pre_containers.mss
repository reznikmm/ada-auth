@comment{ $Source: e:\\cvsroot/ARM/Source/pre_containers.mss,v $ }
@comment{ $Revision: 1.111 $ $Date: 2020/06/03 00:09:01 $ $Author: randy $ }
@Part(precontainers, Root="ada.mss")

@Comment{$Date: 2020/06/03 00:09:01 $}

@RMNewPage
@LabeledAddedClause{Version=[2],Name=[Containers]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[This clause presents the specifications of the package
Containers and several child packages, which provide facilities for storing
collections of elements.]}

@ChgToGlossary{Version=[3],Kind=[Added],Term=<Container>,
Text=<@ChgAdded{Version=[3],Text=[A container is an object that contain other
objects all of the same type, which could be class-wide.
Several predefined container types are provided by the children
of package Ada.Containers (see @RefSecNum{The Package Containers}).]}>}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0196-1]}
@ChgAdded{Version=[2],Text=[A variety of sequence and associative containers are
provided. Each container includes a @i{cursor} type. A cursor is a reference
to an element within a container. Many operations on cursors are common to
all of the containers. A cursor referencing
an element in a container is considered to be overlapping
@Chg{Version=[5],New=[only ],Old=[]}with the
@Chg{Version=[5],New=[element],Old=[container object]}
itself.@PDefn2{Term=[cursor],Sec=[for a container]}
@Defn2{Term=[container],Sec=[cursor]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0196-1]}
  @ChgAdded{Version=[2],Text=[The last sentence is intended to clarify that
  operations that just use a cursor @Chg{Version=[5],New=[do not interfere if
  the cursor objects designated diferent elements of the container],Old=[are
  on the same footing as operations that
  use a container]} in terms of
  the @Chg{Version=[5],New=[concurrent call],Old=[reentrancy]} rules of
  @RefSecNum{Predefined Language Environment}.]}
@end{Reason}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0196-1]}
  @ChgAdded{Version=[5],Text=[A cursor is not considered to overlap with other
  elements of the associated container, thus parallel operations involving a set
  of cursors each operating on mutually exclusive sets of elements from the same
  container are expected to work.]}
@end{Ramification}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[Some operations of the language-defined child units
of Ada.Containers have access-to-subprogram parameters. To ensure such
operations are well-defined, they guard against certain actions by the
designated subprogram. An action on a container that might add or remove an
element is considered to @i{tamper with cursors},@PDefn{tamper with cursors} and
these are prohibited during all such operations. An action on a container that
might replace an element with one of a different size is considered to @i{tamper
with elements}, and these are prohibited during certain of such
operations.@PDefn{tamper with elements} The details of the specific actions that
are considered to tamper with cursors or elements are defined for each child
unit of Ada.Containers.]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[Several of the language-defined child units of
Ada.Containers include a nested package named Stable, which provides a view of a
container that prohibits any operations that would tamper with elements. By
using a Stable view for manipulating a container, the number of tampering checks
performed while performing the operations can be reduced. The details of the
Stable subpackage are defined separately for each child unit of Ada.Containers
that includes such a nested package.]}


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Within this clause we provide Implementation Advice
for the desired average or worst case time complexity of certain operations
on a container. This advice is expressed using the Landau symbol @i{O}(X).
Presuming f is some function of a length parameter N and t(N) is the time the
operation takes (on average or worst case, as specified) for the length N, a
complexity of @i{O}(f(N)) means that there exists a finite A such that for any N,
t(N)/f(N) < A. @Defn{Landau symbol @i{O}(X)}@Defn{@i{O}(f(N))}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Of course, an implementation can do better than a
  specified @i{O}(f(N)): for example, @i{O}(1) meets the requirements for
  @i{O}(log N).]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This concept seems to have
  as many names as there are authors. We used @lquotes@;Landau symbol@rquotes
  because that's what our reference does. But we'd also seen this referred as
  big-O notation@Defn{big-O notation} (sometimes written as @i<big-oh>), and
  as Bachmann notation. Whatever the name, it always has the above definition.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[If the advice suggests that the complexity should be
less than @i{O}(f(N)), then for any arbitrarily small positive real D, there
should exist a positive integer M such that for all N > M,
t(N)/f(N) < D.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0044-1]}
@ChgAdded{Version=[3],Text=[When a formal function is used to provide an
ordering for a container, it is generally required to define
a strict weak ordering. A function "<" defines
a @i<strict weak ordering>@Defn{strict weak ordering} if it is irreflexive,
asymmetric, transitive, and in addition, if @i<x> < @i<y> for any values
@i<x> and @i<y>, then for all other
values @i<z>, (@i<x> < @i<z>) or (@i<z> < @i<y>).]}

@end{Intro}

@begin{Metarules}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@ChgAdded{Version=[2],Text=[This @Chg{Version=[3],New=[subclause],Old=[clause]}
provides a number of useful containers
for Ada. Only the most useful containers are provided. Ones that are relatively
easy to code, redundant, or rarely used are omitted from this set, even if they
are generally included in containers libraries.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The containers packages are modeled on the Standard Template Library (STL), an
algorithms and data structure library popularized by Alexander Stepanov, and
included in the C++ standard library. The structure and terminology differ from
the STL where that better maps to common Ada usage. For instance, what the STL
calls @lquotes@;iterators@rquotes@; are
called @lquotes@;cursors@rquotes@; here.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The following major nonlimited
containers are provided:]}

@begin{Itemize}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[(Expandable) Vectors of any nonlimited type;]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Doubly-linked Lists of any nonlimited type;]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Hashed Maps keyed by any nonlimited hashable type,
  and containing any nonlimited type;]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Ordered Maps keyed by any nonlimited ordered type,
  and containing any nonlimited type;]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[2],Text=[Hashed Sets of any nonlimited hashable type;@Chg{Version=[3],New=[],Old=[ and]}]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[2],Text=[Ordered Sets of any nonlimited ordered type@Chg{Version=[3],New=[;],Old=[.]}]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Text=[Multiway Trees of any nonlimited type;]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0069-1]}
  @ChgAdded{Version=[3],Text=[Holders of any (indefinite) nonlimited type;]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0159-1]}
  @ChgAdded{Version=[3],Text=[Synchronized queues of any definite nonlimited type; and]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0159-1]}
  @ChgAdded{Version=[3],Text=[Priority queues of any definite nonlimited type.]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0001-1]}
@ChgAdded{Version=[2],Text=[Separate versions for definite and indefinite
element types are provided, as those for definite types can be implemented more
efficiently.@Chg{Version=[3],New=[ Similarly, a separate bounded version is
provided in order to give more predictable memory usage.],Old=[]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Each container includes a cursor, which is a
reference to an element within a container. Cursors generally remain valid as
long as the container exists and the element referenced is not deleted. Many
operations on cursors are common to all of the containers. This makes it
possible to write generic algorithms that work on any kind of container.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The containers packages are structured so that
additional packages can be added in the future. Indeed, we hope that these
packages provide the basis for a more extensive secondary standard for
containers.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[If containers with similar functionality (but
different performance characteristics) are provided (by the implementation or
by a secondary standard), we suggest that a prefix be used to identify the
class of the functionality: "Ada.Containers.Bounded_Sets" (for a set with a
maximum number of elements); "Ada.Containers.Protected_Maps" (for a map which
can be accessed by multiple tasks at one time);
"Ada.Containers.Persistent_Vectors" (for a persistent vector which continues to
exist between executions of a program) and so on.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[Note that the language already
includes several requirements that are important to the use of containers.
These include:]}

@begin{Itemize}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0196-1]}
  @ChgAdded{Version=[2],Text=[Library packages must @Chg{Version=[5],New=[allow
  concurrent calls],Old=[be
  reentrant]} @en multiple tasks can use the packages as long as they operate on
  separate containers. Thus, it is only necessary for a user to protect a
  container if a single container needs to be used by multiple tasks@Chg{Version=[5],New=[ and
  concurrent calls to operations of the container have overlapping
  parameters],Old=[]}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Language-defined types must stream "properly".
  That means that the stream attributes can be used to implement persistence
  of containers when necessary, and containers can be passed between
  partitions of a program.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Equality of language-defined types must compose
  @lquotes@;properly@rquotes@;. This means that the version of "=" directly
  used by users is the same one that will be used in generics and in predefined
  equality operators of types with components of the containers and/or cursors.
  This prevents the abstraction from breaking unexpectedly.]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0048-1]}
  @ChgAdded{Version=[3],Text=[Redispatching is not allowed (unless it is
  required). That means that overriding a container operation will not change
  the behavior of any other predefined container operation. This provides
  a stable base for extensions.]}
@end{Itemize}


@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0258-1]}
@ChgAdded{Version=[2],Text=[If a container's element type is controlled, the
point at which the element is finalized will depend on the implementation of
the container. @Chg{Version=[5],New=[For certain
kinds of containers, we require finalization behavior based on the canonical
implementation of the container (see the @ImplReqTitle below).
For the "normal" containers, we],Old=[We]}
do not specify precisely where this will happen (it will happen no later than
the finalization of the container, of course) in order to give implementations
flexibility to cache, block, @Chg{Version=[5],New=[],Old=[or ]}split
@Chg{Version=[5],New=[, or reuse],Old=[]}the nodes of the
container.@Chg{Version=[5],New=[],Old=[ In particular, Delete does not
necessarily finalize the element; the implementation may (or may not) hold the
space for reuse.]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Deleted],ARef=[AI12-0258-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[This is not likely to
be a hardship, as the element type has to be nonlimited. Types used to manage
scarce resources generally need to be limited. Otherwise, the amount of
resources needed is hard to control, as the language allows a lot of variation
in the number or order of adjusts/finalizations. For common uses of nonlimited
controlled types such as managing storage, the types already have to manage
arbitrary copies.]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The use of controlled types also brings up the
possibility of failure of finalization (and thus deallocation) of an element.
This is a @ldquote@;serious bug@rdquote@;, as AI95-179 puts it, so we don't try
to specify what happens in that case. The implementation should propagate
the exception.]}
@end{Metarules}

@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[It is expected that exceptions propagated from
these operations do not
damage containers. That is, if Storage_Error is propagated because of an
allocation failure, or Constraint_Error is propagated by the assignment of
elements, the container can continue to be used without further exceptions.
The intent is that it should be possible to recover from errors without
losing data. We don't try to state this formally in most cases, because it is
hard to define precisely what is and is not allowed behavior.]}
@end{ImplNote}

@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[When this clause says that the behavior of
something is unspecified@PDefn{unspecified}, we
really mean that any result of executing Ada code short of erroneous
execution is allowed. We do not mean that memory not belonging to the
parameters of the operation can be trashed. When we mean to allow erroneous
behavior, we specifically say that execution is erroneous. All this means
if the containers are written in Ada is that checks should not be suppressed
or removed assuming some behavior of other code, and that the implementation
should take care to avoid creating internal dangling accesses by assuming
behavior from generic formals that can't be guaranteed. We don't
try to say this normatively because it would be fairly complex, and
implementers are unlikely to increase their support costs by fielding
implementations that are unstable if given buggy hash functions, et al.]}
@end{ImplNote}

@begin{StaticSem}
@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
@ChgAdded{Version=[4],Text=[Certain subprograms declared within instances of
some of the generic packages presented in this clause are said to @i<perform
indefinite insertion>. These subprograms are those corresponding (in the sense
of the copying described in subclause @RefSecNum{Generic Instantiation}) to
subprograms that have formal parameters of a generic formal indefinite type and
that are identified as performing indefinite insertion in the subclause defining
the generic package.@Defn{perform indefinite insertion}]}

@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
@ChgAdded{Version=[4],Type=[Leading],Text=[If a subprogram performs indefinite
insertion, then certain run-time checks are performed as part of a call to the
subprogram; if any of these checks fail, then the resulting exception is
propagated to the caller and the container is not modified by the call. These
checks are performed for each parameter corresponding (in the sense of the
copying described in @RefSecNum{Generic Instantiation}) to a parameter in the
corresponding generic whose type is a generic formal indefinite type. The checks
performed for a given parameter are those checks explicitly specified in
subclause @RefSecNum{Allocators} that would be performed as part of the
evaluation of an initialized allocator whose access type is declared immediately
within the instance, where:]}

@begin{Itemize}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[the value of the @nt{qualified_expression} is
  that of the parameter; and]}

  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[the designated subtype of the access type is the
  subtype of the parameter; and]}

  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[finalization of the collection of the access type
  has started if and only if the finalization of the instance has started.]}
@end{Itemize}

@begin{Discussion}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[The phrase "explicitly specified" means those
  checks for which subclause @RefSecNum{Allocators} includes the phrase "<some
  exception> is raised if ...". It does not refer, for example, to any checks
  performed as part of any subtype conversion. In particular, this wording
  includes the checks described in subclause @RefSecNum{Allocators} to be
  performed in the case of a class-wide designated type, and of a designated
  subtype that has access discriminant parts. These checks are needed to prevent
  containers from outliving their contained (Element_Type or Key_Type) values.]}
@end{Discussion}
@begin{ImplNote}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[These rules have a dual purpose. Mainly, we are
  @i{requiring} checks needed to prevent dangling references. As a side effect, we
  are also @i{allowing} checks needed to permit an implementation of a container
  generic to make use of access types in a straightforward way. As an example of
  the second purpose, suppose that an implementation does declare such an access
  type and suppose further that the finalization of the collection of the
  access type has started. These rules allow Program_Error to be propagated
  in this case (as specified in @RefSecNum{Allocators}); this is necessary to
  allow an all-Ada implementation of these packages.]}
@end{ImplNote}
@end{StaticSem}

@begin{ImplReq}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0258-1]}
@ChgAdded{Version=[5],Text=[For an indefinite container (one whose type is
defined in an instance of a child package of Containers whose
@nt{defining_identifier} contains "Indefinite"), each element of the container
shall be created when it is inserted into the container and finalized when it is
deleted from the container (or when the container object is finalized if the
element has not been deleted). For a bounded container (one whose type is
defined in an instance of a child package of Containers whose
@nt{defining_identifier} starts with "Bounded") that is not an indefinite
container, all of the elements of the capacity of the container shall be created
and default initialized when the container object is created; the elements shall
be finalized when the container object is finalized. @Redundant[For other kinds
of containers, when elements are created and finalized is unspecified.]]}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[This allows a user to be able to reason about the
  behavior of elements that have controlled parts. In most cases, such elements
  need to be stored in an indefinite container.]}
@end{Ramification}

@begin{ImplNote}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[If the containers are implemented in Ada, this
  implies that elements for an indefinite container are allocated individually,
  and that a bounded container contains an array of elements or other data
  structure that is initialized for the entire capacity of the container when it
  is created. There is no such restriction on the implementation of the "normal"
  containers; these can be handled in any way convenient to the implementation
  @em in particular, node reuse is allowed.]}
@end{ImplNote}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[For an instance @i<I> of a container package with a
container type @i<C>, the specific type @i<T> of the object returned from a
function that returns an object of an iterator interface, as well as the
primitive operations of @i<T>, shall be nonblocking. The Global aspect specified
for @i<T> and the primitive operations of @i<T> shall be @exam[(@key{in all},
@key{out synchronized})] or a specification that allows
access to fewer global objects.]}

@begin{ImplNote}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[This requires that the traversal and iteration
  operations of a container do not create, destroy, or assign any objects of
  a formal type of @i<I>, nor call any formal subprograms of @i<I>. Those
  objects and subprograms might be blocking (depending on the actual parameters).
  We put similar requirements on the individual traversal operations in the
  container package definitions.]}
@end{ImplNote}

@begin{Reason}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[These requirements allows users to use container
  iterators inside of parallel constructs, regardless of the actual parameters
  to the instantiation. If such an iterator allowed blocking,
  it would be illegal inside of a parallel construct (see
  @RefSecNum{Intertask Communication}). If such an iterator allowed writing of
  unsynchronized global objects, it would be illegal when the default conflict
  checking policy is in effect (see @RefSecNum{Conflict Check Policies}).
  These requirements include sequential iterators; the iterator does not need
  to appear in a parallel loop to trigger these requirements.]}
@end{Reason}

@begin{Discussion}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[We have to give these requirements as a text rule,
  as there is no place to declare suitable aspects. The specific type of a
  container iterator is declared by the implementation and is not part of the
  visible specification (iterator functions just return a value of a class-wide
  type). The iterator interface itself cannot impose such a requirement since
  it needs to be able to work with user-defined types that do need to
  allow blocking. We give this as a global requirement to avoid duplication.]}
@end{Discussion}

@end{ImplReq}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  This @Chg{Version=[3],New=[subclause],Old=[clause]} is new. It just
  provides an introduction to the following subclauses.]}
@end{Extend95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0044-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a definition of
  strict weak ordering.]}
@end{DiffWord2005}

@begin{Incompatible2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1],ARef=[AI12-0112-1],ARef=[AI12-0339-1]}
  @ChgAdded{Version=[5],Text=[@Defn{incompatibilities with Ada 2012}A number of
  new subprograms, types, and even a nested package were added to
  Containers.Hashed_Maps to better support contracts and stable views. If an
  instance of Containers.Hashed_Maps
  is referenced in a @nt{use_clause}, and an entity @i<E> with the same
  @nt{defining_identifier} as a new entity in Containers.Hashed_Maps is
  defined in a package that is also referenced in a @nt{use_clause}, the
  entity @i<E> may no longer be use-visible, resulting in errors. This should
  be rare and is easily fixed if it does occur.]}
@end{Incompatible2012}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0196-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}@b{Correction:}
  We now say that a cursor only overlaps with the element it designates,
  rather than with the whole container. This allows some reading operations
  to operate on the container in parallel without separate synchronization.]}
@end{Extend2012}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI05-0035-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Added a definition of
  @ldquote@;performs indefinite insertion@rdquote. This is used in
  other subclauses and any resulting inconsistencies are documented
  there.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1]}
  @ChgAdded{Version=[5],Text=[Moved the basic description of tampering
  checks here, to cut duplication in description of the individual containers.
  Added a description of stable views of containers.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[Added a global requirement that iterators
  returned from containers are nonblocking if the instance is nonblocking.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0258-1]}
  @ChgAdded{Version=[5],Text=[@b<Correction:> Defined when objects
  are created and finalized for Bounded and Indefinite containers,
  so that these can be used reliably with controlled element types.
  This is not incompatible as this behavior was previously unspecified;
  code depending on specific behavior was wrong.]}
@end{DiffWord2012}


@LabeledAddedSubclause{Version=[2],Name=[The Package Containers]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The package Containers is the root of
the containers subsystem.]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The library package
Containers has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} Ada.Containers @key{is}@ChildUnit{Parent=[Ada],Child=[Containers]}
   @key{pragma} Pure(Containers);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Hash_Type} @key{is mod} @RI<implementation-defined>;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Count_Type} @key{is range} 0 .. @RI<implementation-defined>;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @AdaExcDefn{Capacity_Error} : @key[exception];]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{end} Ada.Containers;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Hash_Type represents the range of the result of a
hash function. Count_Type represents the (potential or actual) number of
elements of a container.]}
@ChgImpldef{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The value of Containers.Hash_Type'Modulus. The value of
Containers.Count_Type'Last.]}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[Capacity_Error is raised when the capacity of a
container is exceeded.]}

@end{StaticSem}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Hash_Type'Modulus should be at least 2**32.
Count_Type'Last should be at least 2**31@en@;1.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Containers.Hash_Type'Modulus should be at least 2**32.
Containers.Count_Type'Last should be at least 2**31@en@;1.]}]}

@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This is not a requirement so that these types
can be declared
properly on machines with native sizes that are not 32 bits. For instance, a
24-bit target could use 2**24 for Hash_Type'Modulus.]}
@end{Discussion}
@end{ImplAdvice}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The package Containers is new.]}
@end{Extend95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}Exception Capacity_Error
  is added to Containers. If Containers is referenced in a @nt{use_clause},
  and an entity with the name Capacity_Error is defined in a package that is
  also referenced in a @nt{use_clause}, the entity Capacity_Error may no
  longer be use-visible, resulting in errors. This should be rare and is
  easily fixed if it does occur.]}
@end{Incompatible2005}



@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Vectors]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The language-defined generic package
Containers.Vectors provides private types Vector and Cursor, and a set of
operations for each type. A vector container allows insertion and deletion at
any position, but it is specifically optimized for insertion and deletion at
the high end (the end with the higher index) of the container. A vector
container also provides random access to its elements.@Defn{vector container}
@Defn2{Term=[container],Sec=[vector]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@Pdefn2{Term=[length], Sec=(of a vector container)}
@Pdefn2{Term=[capacity], Sec=(of a vector)}
A vector container behaves conceptually as an array that expands as necessary
as items are inserted. The @i{length} of a vector is the number of elements that
the vector contains. The @i{capacity} of a vector is the maximum number of
elements that can be inserted into the vector prior to it being automatically
expanded.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Elements in a vector container can be referred to by
an index value of a generic formal type. The first element of a vector always
has its index value equal to the lower bound of the formal type.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@Pdefn2{Term=[empty element], Sec=(of a vector)}
A vector container may contain @i{empty elements}. Empty elements do not have a
specified value.]}

@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[
Vectors are not intended to be sparse (that is, there are elements at all
defined positions). Users are expected to use other containers (like a Map)
when they need sparse structures (there is a Note to this effect at the
end of this subclause).]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[
The internal array is a conceptual model of a vector. There is no requirement
for an implementation to be a single contiguous array.]}
@end{Implnote}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
package Containers.Vectors has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0084-1],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[@key[with] Ada.Iterator_Interfaces;
],Old=[]}@key{generic}
   @key{type} Index_Type @key{is range} <>;
   @key{type} Element_Type @key{is private};
   @key{with function} "=" (Left, Right : Element_Type)
      @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Vectors@Chg{Version=[5],New=[],Old=[ @key{is}]}@ChildUnit{Parent=[Ada.Containers],Child=[Vectors]}@Chg{Version=[5],New=[
   @key[with] Preelaborate, Remote_Types,
        Nonblocking, Global => @key[null] @key[is]],Old=[
   @key{pragma} Preelaborate(Vectors);@Chg{Version=[3],New=[
   @key{pragma} Remote_Types(Vectors);],Old=[]}]}]}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
   @ChgAdded{Version=[5],Text=[For the Global aspect, any side-effects of the
   actual parameters of an instance are ignored. So Global => @key[null] means
   that the only global side-effects allowed are associated with the actual
   generic parameters. Package state is still allowed for packages that are not
   Pure, but it must be task-safe (that is, @key[synchronized]) and cannot 
   affect the result for correct code.]}

   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
   @ChgAdded{Version=[5],Text=[Similarly, when Nonblocking is set to True for
   a generic unit, it still includes the blocking affects of the actual
   parameters to the instance. Thus, the only blocking allowed is that
   associated with the actual generic parameters. If none of the actual
   paramerters allow blocking, then no operation of the generic may block.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{subtype} @AdaSubtypeDefn{Name=[Extended_Index],Of=[Index_Type'Base]} @key{is}
      Index_Type'Base @key{range}
         Index_Type'First-1 ..
         Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;
   @AdaObjDefn{No_Index} : @key{constant} Extended_Index := Extended_Index'First;]}

@begin{Ramification}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
   @ChgAdded{Version=[5],Text=[The base type of a scalar type always is 
   nonblocking and has Global => @key[null]. Therefore, so long as this type
   is used in the implementation, whether or not the actual type for 
   Index_Type allows blocking or side-effects does not matter. Therefore,
   we require that operations that only operate on the container 
   implementation by nonblocking and have Global => @key[null] regardless
   of the actual parameters.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0111-1],ARef=[AI12-0112-1],ARef=[AI12-0212-1],ARef=[AI12-0339-1]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Vector} @key{is tagged private}@Chg{Version=[3],New=[
      @key[with] Constant_Indexing => Constant_Reference,
           Variable_Indexing => Reference,
           Default_Iterator  => Iterate,
           Iterator_Element  => Element_Type],Old=[]}@Chg{Version=[5],New=[,
           Iterator_View     => Stable.Vector,
           Aggregate         => (Empty          => Empty,
                                 Add_Unnamed    => Append_One,
                                 New_Indexed    => New_Vector,
                                 Assign_Indexed => Replace_Element),
           Stable_Properties => (Length, Capacity,
                                 Tampering_With_Cursors_Prohibited,
                                 Tampering_With_Elements_Prohibited),
           Default_Initial_Condition =>
              Length (Vector) = 0 @key{and then}
              (@key{not} Tampering_With_Cursors_Prohibited (Vector)) @key{and then}
              (@key{not} Tampering_With_Elements_Prohibited (Vector))],Old=[]};
   @key{pragma} Preelaborable_Initialization(Vector);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Cursor} @key{is private};
   @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Empty_Vector} : @key{constant} Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[in all], @key[use null])],Old=[]};]}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
   @ChgAdded{Version=[5],Text=[Any operation that takes a cursor but no vector
   can read the vector associated with the cursor. We only know that that is
   some object of type Vector. Since we don't have a global specification
   that describes all objects of a specific type, we have to allow reading
   any object by specifying @key[in all]. For such functions, we don't allow 
   writing any object, even those associated with generic formal parameters, 
   thus we also specify @key[use null].]}
@end{Discussion}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Has_Element} (Container : Vector; Position : Cursor)
      @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
   @ChgAdded{Version=[5],Text=[For operations that do not depend on any of
   the operations of the generic formal parameters (including those of formal 
   types), we specify that the operation has no side-effects of any kind.
   This requires specifying that there is no dependence on the generic formal
   parameters with @key[use null] in addition to no usual side-effects with
   @key[null]. We also specify Nonblocking on such operations in order that
   the operation never blocks even if some of the actual parameters allow
   blocking.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}@ChgNote{Just a paragraph number change}
@ChgAdded{Version=[3],Text=[   @key[package] @AdaPackDefn{Vector_Iterator_Interfaces} @key[is new]
       Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "=" (Left, Right : Vector) @key{return} Boolean;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Tampering_With_Cursors_Prohibited}
      (Container : Vector) @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Tampering_With_Elements_Prohibited}
      (Container : Vector) @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Maximum_Length} @key{return} Count_Type
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0339-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Empty} (Capacity : Count_Type := @RI<implementation-defined>)
      @key{return} Vector
      @key[with] Pre  => Capacity <= Maximum_Length @key[or else raise] Constraint_Error,
           Post =>
              Capacity (Empty'Result) >= Capacity @key[and then]
              @key[not] Tampering_With_Elements_Prohibited (Empty'Result) @key[and then]
              @key[not] Tampering_With_Cursors_Prohibited (Empty'Result) @key[and then]
              Length (Empty'Result) = 0;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{To_Vector} (Length : Count_Type) @key{return} Vector@Chg{Version=[5],New=[
      @key[with] Pre  => Length <= Maximum_Length @key[or else raise] Constraint_Error,
           Post =>
              To_Vector'Result.Length = Length @key[and then]
              @key[not] Tampering_With_Elements_Prohibited (To_Vector'Result) @key[and then]
              @key[not] Tampering_With_Cursors_Prohibited (To_Vector'Result) @key[and then]
              To_Vector'Result.Capacity >= Length],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{To_Vector}
     (New_Item : Element_Type;
      Length   : Count_Type) @key{return} Vector@Chg{Version=[5],New=[
      @key[with] Pre  => Length <= Maximum_Length @key[or else raise] Constraint_Error,
           Post =>
              To_Vector'Result.Length = Length @key[and then]
              @key[not] Tampering_With_Elements_Prohibited (To_Vector'Result) @key[and then]
              @key[not] Tampering_With_Cursors_Prohibited (To_Vector'Result) @key[and then]
              To_Vector'Result.Capacity >= Length],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0212-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{New_Vector} (First, Last : Index_Type) @key{return} Vector @key{is}
        (To_Vector (Count_Type (Last - First + 1)))
     @key{with} Pre => First = Index_Type'First;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} "&" (Left, Right : Vector) @key{return} Vector@Chg{Version=[5],New=[
      @key{with} Pre  => Length (Left) <= Maximum_Length - Length (Right)
                    @key[or else raise] Constraint_Error,
           Post => Length (Vectors."&"'Result) = Length (Left) + Length (Right) @key[and then]
                   @key[not] Tampering_With_Elements_Prohibited (Vectors."&"'Result) @key[and then]
                   @key[not] Tampering_With_Cursors_Prohibited (Vectors."&"'Result) @key[and then]
                   Vectors."&"'Result.Capacity >= Length (Left) + Length (Right)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} "&" (Left  : Vector;
                 Right : Element_Type) @key{return} Vector@Chg{Version=[5],New=[
      @key{with} Pre  => Length (Left) <= Maximum_Length - 1
                    @key[or else raise] Constraint_Error,
           Post => Vectors."&"'Result.Length = Length (Left) + 1 @key[and then]
                   @key[not] Tampering_With_Elements_Prohibited (Vectors."&"'Result) @key[and then]
                   @key[not] Tampering_With_Cursors_Prohibited (Vectors."&"'Result) @key[and then]
                   Vectors."&"'Result.Capacity >= Length (Left) + 1],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} "&" (Left  : Element_Type;
                 Right : Vector) @key{return} Vector@Chg{Version=[5],New=[
      @key{with} Pre  => Length (Right) <= Maximum_Length - 1
                    @key[or else raise] Constraint_Error,
           Post => Length (Vectors."&"'Result) = Length (Right) + 1 @key[and then]
                   @key[not] Tampering_With_Elements_Prohibited (Vectors."&"'Result) @key[and then]
                   @key[not] Tampering_With_Cursors_Prohibited (Vectors."&"'Result) @key[and then]
                   Vectors."&"'Result.Capacity >= Length (Right) + 1],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} "&" (Left, Right  : Element_Type) @key{return} Vector@Chg{Version=[5],New=[
      @key{with} Pre  => Maximum_Length >= 2 @key[or else raise] Constraint_Error,
           Post => Length ("&"'Result) = 2 @key[and then]
                   @key[not] Tampering_With_Elements_Prohibited (Vectors."&"'Result) @key[and then]
                   @key[not] Tampering_With_Cursors_Prohibited (Vectors."&"'Result) @key[and then]
                   Vectors."&"'Result.Capacity >= 2],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Capacity} (Container : Vector) @key{return} Count_Type@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key{null}, @key{use null})],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reserve_Capacity} (Container : @key{in out} Vector;
                               Capacity  : @key{in}     Count_Type)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                    @key[or else raise] Program_Error,
           Post => Container.Capacity >= Capacity],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Length} (Container : Vector) @key{return} Count_Type@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key{null}, @key{use null})],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Set_Length} (Container : @key{in out} Vector;
                         Length    : @key{in}     Count_Type)@Chg{Version=[5],New=[
      @key[with] Pre  => (@key[not] Tampering_With_Cursors_Prohibited (Container)
                    @key[or else raise] Program_Error) @key[and then]
                   (Length <= Maximum_Length @key[or else raise] Constraint_Error),
           Post => Container.Length = Length @key[and then]
                   Capacity (Container) >= Length],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Empty} (Container : Vector) @key{return} Boolean@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key{null}, @key{use null}),
           Post => Is_Empty'Result = (Length (Container) = 0)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Clear} (Container : @key{in out} Vector)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                       @key[or else raise] Program_Error,
           Post => Length (Container) = 0],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{To_Cursor} (Container : Vector;
                       Index     : Extended_Index) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Post => (@key{if} Index @key{in} First_Index (Container) .. Last_Index (Container)
                    @key{then} Has_Element (Container, To_Cursor'Result)
                    @key{else} To_Cursor'Result = No_Element),
           Nonblocking, Global => (@key{null}, @key{use null})],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{To_Index} (Position  : Cursor) @key{return} Extended_Index@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_Index} (Container : Vector;
                      Position  : Cursor) @key{return} Extended_Index
      @key{with} Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position) @key{or else}
                      @key{raise} Program_Error,
           Post => (@key{if} Position = No_Element @key{then} To_Index'Result = No_Index
                    @key{else} To_Index'Result @key{in} First_Index (Container) ..
                           Last_Index (Container)),
           Nonblocking, Global => (@key{null}, @key{use null});]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Container : Vector;
                     Index     : Index_Type)
      @key{return} Element_Type@Chg{Version=[5],New=[
      @key{with} Pre  => Index @key{in} First_Index (Container) .. Last_Index (Container)
                      @key{or else raise} Constraint_Error,
           Nonblocking => Element_Type'Nonblocking,
           Global => (@key{null}, @key{use} Element_Type)],Old=[]};]}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
   @ChgAdded{Version=[5],Text=[Here the Nonblocking and Global contracts are
   saying that Element depends on the properties of the actual for 
   Element_Type, but not on the properties of the actuals for Index_Type or 
   "=". This is necessary as copying the element may require calling
   Adjust and Finalize for the actual Element_Type, and those may have
   side-effects or block.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Position : Cursor) @key{return} Element_Type@Chg{Version=[5],New=[
      @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
           Nonblocking => Element_Type'Nonblocking,
           Global => (@key{in all}, @key{use} Element_Type)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Element} (Container : Vector;
                     Position  : Cursor) @key{return} Element_Type
      @key{with} Pre => (Position /= No_Element @key{or else}
                      @key{raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position) @key{or else raise} Program_Error),
           Nonblocking => Element_Type'Nonblocking,
           Global => (@key{null}, @key{use} Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} Vector;
                              Index     : @key{in}     Index_Type;
                              New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre => (@key{not} Tampering_With_Elements_Prohibited (Container)
                     @key{or else raise} Program_Error) @key{and then}
                  (Index @key{in} First_Index (Container) .. Last_Index (Container)
                     @key{or else raise} Constraint_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} Vector;
                              Position  : @key{in}     Cursor;
                              New_item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Elements_Prohibited (Container)
                      @key{or else raise} Program_Error) @key{and then}
                   (Position /= No_Element 
                      @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position) @key{or else raise} Program_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Container : @key{in} Vector;
      Index     : @key{in} Index_Type;
      Process   : @key{not null access procedure} (Element : @key{in} Element_Type))@Chg{Version=[5],New=[
      @key{with} Pre  => Index @key{in} First_Index (Container) .. Last_Index (Container)
                      @key{or else raise} Constraint_Error],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Element : @key{in} Element_Type))@Chg{Version=[5],New=[
      @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Container : @key{in} Vector;
      Position  : @key{in} Cursor;
      Process   : @key{not null access procedure} (Element : @key{in} Element_Type))
      @key{with} Pre  => (Position /= No_Element 
                       @key{or else raise} Constraint_Error) @key{and then}
                    (Has_Element (Container, Position) 
                       @key{or else raise} Program_Error);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Update_Element}
     (Container : @key{in out} Vector;
      Index     : @key{in}     Index_Type;
      Process   : @key{not null access procedure}
                      (Element : @key{in out} Element_Type))@Chg{Version=[5],New=[
      @key{with} Pre  => Index @key{in} First_Index (Container) .. Last_Index (Container)
                      @key{or else raise} Constraint_Error],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Update_Element}
     (Container : @key{in out} Vector;
      Position  : @key{in}     Cursor;
      Process   : @key{not null access procedure}
                      (Element : @key{in out} Element_Type))@Chg{Version=[5],New=[
      @key{with} Pre  => (Position /= No_Element @key{or else raise} Constraint_Error) @key{and then}
                    (Has_Element (Container, Position) @key{or else raise} Program_Error)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[type] Constant_Reference_Type
         (Element : @key[not null access constant] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
           Nonblocking, Global => @key[in out synchronized],
           Default_Initial_Condition => (@key[raise] Program_Error)],Old=[]};]}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
   @ChgAdded{Version=[5],Text=[Finalization of this type will update the
   tampering counter of an associated container. We know this has to be an
   object of type Vector, but we don't have a way to specify that. We need
   this separate Global in case an object of this type is declared to exist
   separately from the short-lived object associated with a call of the 
   Constant_Reference function.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Reference_Type} (Element : @key[not null access] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
           Nonblocking, Global => @key[in out synchronized],
           Default_Initial_Condition => (@key[raise] Program_Error)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Vector;
                                Index     : @key[in] Index_Type)
      @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre    => Index @key[in] First_Index (Container) .. Last_Index (Container)
                        @key[or else raise] Constraint_Error,
           Post   => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] Vector;
                       Index     : @key[in] Index_Type)
      @key[return] Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre    => Index @key[in] First_Index (Container) .. Last_Index (Container)
                        @key[or else raise] Constraint_Error,
           Post   => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Vector;
                                Position  : @key[in] Cursor)
      @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre  => (Position /= No_Element @key[or else]
                       @key[raise] Constraint_Error) @key[and then]
                    (Has_Element (Container, Position) @key[or else raise] Program_Error),
           Post   => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] Vector;
                       Position  : @key[in] Cursor)
      @key[return] Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre  => (Position /= No_Element @key[or else]
                       @key[raise] Constraint_Error) @key[and then]
                    (Has_Element (Container, Position) @key[or else raise] Program_Error),
           Post   => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} Vector; Source : @key{in} Vector)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Target)
                      @key[or else raise] Program_Error,
           Post => Length (Source) = Length (Target) @key{and then}
                   Capacity (Target) >= Length (Target)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Copy} (Source : Vector; Capacity : Count_Type := 0)
      @key[return] Vector@Chg{Version=[5],New=[
      @key[with] Pre => Capacity = 0 @key[or else] Capacity >= Length (Source)
                      @key[or else raise] Capacity_Error,
           Post => Length (Copy'Result) = Length (Source) @key[and then]
                   @key[not] Tampering_With_Elements_Prohibited (Copy'Result) @key[and then]
                   @key[not] Tampering_With_Cursors_Prohibited (Copy'Result) @key[and then]
                   Copy'Result.Capacity >= (@key[if] Capacity = 0 @key[then]
                      Length (Source) @key[else] Capacity)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Move} (Target : @key{in out} Vector;
                   Source : @key{in out} Vector)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Target)
                       @key{or else raise} Program_Error) @key{and then}
                   (@key{not} Tampering_With_Cursors_Prohibited (Source)
                       @key{or else raise} Program_Error),
           Post => (@key{if} Target = Source @key{then} True
                    @key{else}
                       Length (Target) = Length (Source'Old) @key{and then}
                       Length (Source) = 0 @key{and then}
                       Capacity (Target) >= Length (Source'Old))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Extended_Index;
                     New_Item  : @key{in}     Vector)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before @key{in} First_Index (Container) .. Last_Index (Container) + 1
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - Length (New_Item)
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Length (New_Item) = Length (Container) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Vector)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before = No_Element @key{or else}
                    Has_Element (Container, Before)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - Length (New_Item)
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Length (New_Item) = Length (Container) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Vector;
                     Position  :    @key{out} Cursor)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before = No_Element @key{or else}
                    Has_Element (Container, Before)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - Length (New_Item)
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Length (New_Item) = Length (Container) @key{and then}
                   Has_Element (Container, Position) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Extended_Index;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before @key{in} First_Index (Container) .. Last_Index (Container) + 1
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - Count
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before = No_Element @key{or else}
                    Has_Element (Container, Before)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - Count
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before = No_Element @key{or else}
                    Has_Element (Container, Before)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - Count
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                   Has_Element (Container, Position) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Extended_Index;
                     Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before @key{in} First_Index (Container) .. Last_Index (Container) + 1
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - Count
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Cursor;
                     Position  :    @key{out} Cursor;
                     Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before = No_Element @key{or else}
                    Has_Element (Container, Before)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - Count
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Count = Length (Container)
                   @key{and then} as_Element (Container, Position) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Prepend} (Container : @key{in out} Vector;
                      New_Item  : @key{in}     Vector)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - Length (New_Item)
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Length (New_Item) = Length (Container) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Prepend} (Container : @key{in out} Vector;
                      New_Item  : @key{in}     Element_Type;
                      Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - Count
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Append} (Container : @key{in out} Vector;
                     New_Item  : @key{in}     Vector)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - Length (New_Item)
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Length (New_Item) = Length (Container) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Append} (Container : @key{in out} Vector;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - Count
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1],ARef=[AI12-0212-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Append_One} (Container : @key{in out} Vector;
                         New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - 1
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + 1 = Length (Container) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert_Space} (Container : @key{in out} Vector;
                           Before    : @key{in}     Extended_Index;
                           Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                    (Before @key{in} First_Index (Container) .. Last_Index (Container) + 1
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - Count
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert_Space} (Container : @key{in out} Vector;
                           Before    : @key{in}     Cursor;
                           Position  :    @key{out} Cursor;
                           Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before = No_Element @key{or else}
                       Has_Element (Container, Before)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Maximum_Length - Count
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                   Has_Element (Container, Position) @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Vector;
                     Index     : @key{in}     Extended_Index;
                     Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Index @key{in} First_Index (Container) .. Last_Index (Container) + 1
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old - Count <= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Vector;
                     Position  : @key{in out} Cursor;
                     Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Position /= No_Element 
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position)
                       @key{or else raise} Program_Error),
           Post => Length (Container)'Old - Count <= Length (Container) @key{and then}
                   Position = No_Element],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_First} (Container : @key{in out} Vector;
                           Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error,
           Post => Length (Container)'Old - Count <= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_Last} (Container : @key{in out} Vector;
                          Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error,
           Post => Length (Container)'Old - Count <= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reverse_Elements} (Container : @key{in out} Vector)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Swap} (Container : @key{in out} Vector;
                   I, J      : @key{in}     Index_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Elements_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                  (I @key{in} First_Index (Container) .. Last_Index (Container)
                       @key{or else raise} Constraint_Error) @key{and then}
                  (J @key{in} First_Index (Container) .. Last_Index (Container)
                       @key{or else raise} Constraint_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Swap} (Container : @key{in out} Vector;
                   I, J      : @key{in}     Cursor)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Elements_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (I /= No_Element @key{or else} Constraint_Error) @key{and then}
                   (J /= No_Element @key{or else} Constraint_Error) @key{and then}
                   (Has_Element (Container, I)
                       @key{or else raise} Program_Error) @key{and then}
                   (Has_Element (Container, J)
                       @key{or else raise} Program_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First_Index} (Container : Vector) @key{return} Index_Type@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Post => First_Index'Result = Index_Type'First],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First} (Container : Vector) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Post => (@key{if not} Is_Empty (Container)
                    @key{then} Has_Element (Container, First'Result)
                    @key{else} First'Result = No_Element)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First_Element} (Container : Vector)
      @key{return} Element_Type@Chg{Version=[5],New=[
      @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last_Index} (Container : Vector) @key{return} Extended_Index@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Post => (@key{if} Length (Container) = 0 @key{then} Last_Index'Result = No_Index
                    @key{else} Count_Type(Last_Index'Result - Index_Type'First) =
                         Length (Container) - 1)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last} (Container : Vector) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Post => (@key{if} not Is_Empty (Container)
                    @key{then} Has_Element (Container, Last'Result)
                    @key{else} Last'Result = No_Element)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last_Element} (Container : Vector)
      @key{return} Element_Type@Chg{Version=[5],New=[
      @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Next} (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Global => (@key{in all}, @key{use null}),
           Nonblocking => True,
           Post => (@key{if} Position = No_Element @key{then} Next'Result = No_Element
                    @key{else} True)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Next} (Container : Vector; Position : Cursor) @key{return} Cursor
      @key{with} Nonblocking, Global =>(@key{null}, @key{use null}),
           Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Position = No_Element @key{then} Next'Result = No_Element
                    @key{elsif} Has_Element (Container, Next'Result) @key{then}
                       To_Index (Container, Next'Result) =
                       To_Index (Container, Position) + 1
                    @key{elsif} Next'Result = No_Element @key{then}
                       Position = Last (Container)
                    @key{else} False);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Next} (Position : @key{in out} Cursor)@Chg{Version=[5],New=[
      @key{with} Global => (@key{in all}, @key{use null}),
           Nonblocking => True],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Next} (Container : @key{in}     Vector;
                   Position  : @key{in out} Cursor)
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Position /= No_Element
                    @key{then} Has_Element (Container, Position));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Previous} (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Global => (@key{in all}, @key{use null}),
           Nonblocking => True,
           Post => (@key{if} Position = No_Element @key{then} Previous'Result = No_Element
                    @key{else} True)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Previous} (Container : Vector;
                      Position  : Cursor) @key{return} Cursor
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Position = No_Element @key{then} Previous'Result = No_Element
                    @key{elsif} Has_Element (Container, Previous'Result) @key{then}
                       To_Index (Container, Previous'Result) =
                       To_Index (Container, Position) - 1
                    @key{elsif} Previous'Result = No_Element @key{then}
                       Position = First (Container)
                    @key{else} False);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Previous} (Position : @key{in out} Cursor)@Chg{Version=[5],New=[
      @key{with} Global => (@key{in all}, @key{use null}),
           Nonblocking => True],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Previous} (Container : @key{in}     Vector;
                       Position  : @key{in out} Cursor)
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Position /= No_Element
                    @key{then} Has_Element (Container, Position));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Find_Index} (Container : Vector;
                        Item      : Element_Type;
                        Index     : Index_Type := Index_Type'First)
      @key{return} Extended_Index;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Find} (Container : Vector;
                  Item      : Element_Type;
                  Position  : Cursor := No_Element)
      @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Find'Result = No_Element @key{then} True
                    @key{else} Has_Element (Container, Find'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Reverse_Find_Index} (Container : Vector;
                                Item      : Element_Type;
                                Index     : Index_Type := Index_Type'Last)
      @key{return} Extended_Index;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Reverse_Find} (Container : Vector;
                          Item      : Element_Type;
                          Position  : Cursor := No_Element)
      @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Reverse_Find'Result = No_Element @key{then} True
                    @key{else} Has_Element (Container, Reverse_Find'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Contains} (Container : Vector;
                      Item      : Element_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Iterate}
     (Container : @key{in} Vector;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
      @key{with} Allows_Exit],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reverse_Iterate}
     (Container : @key{in} Vector;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
      @key{with} Allows_Exit],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Iterate} (Container : @key[in] Vector)
      @key[return] Vector_Iterator_Interfaces.@Chg{Version=[5],New=[Parallel_Reversible_Iterator],Old=[Reversible_Iterator]}'Class@Chg{Version=[5],New=[
      @key[with] Post   => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Iterate} (Container : @key[in] Vector; Start : @key[in] Cursor)
      @key[return] Vector_Iterator_Interfaces.Reversible_Iterator'Class@Chg{Version=[5],New=[
      @key[with] Pre    => (Start /= No_Element
                            @key[or else raise] Constraint_Error) @key[and then]
                        (Has_Element (Container, Start)
                            @key[or else raise] Program_Error),
           Post   => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{generic}
      @key{with function} "<" (Left, Right : Element_Type)
         @key{return} Boolean is <>;
   @key{package} @AdaPackDefn{Generic_Sorting}@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => @key{null}],Old=[]} @key{is}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{function} @AdaSubDefn{Is_Sorted} (Container : Vector) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Sort} (Container : @key{in out} Vector)@Chg{Version=[5],New=[
         @key{with} Pre  => (@key{if} Tampering_With_Elements_Prohibited (Container)
                       @key{then raise} Program_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Merge} (Target  : @key{in out} Vector;
                       Source  : @key{in out} Vector)@Chg{Version=[5],New=[
         @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Target)
                          @key{or else raise} Program_Error) @key{and then}
                      (@key{not} Tampering_With_Cursors_Prohibited (Source)
                          @key{or else raise} Program_Error) @key{and then}
                      (Length (Target) <= Maximum_Length - Length (Source)
                          @key{or else raise} Constraint_Error),
              Post => (@key{declare}
                         Result_Length : @key{constant} Count_Type :=
                            Length (Source)'Old + Length (Target)'Old;
                       @key{begin}
                          (@key{if} Target = Source @key{then} True
                           @key{else} Length (Source) = 0 @key{and then}
                              Length (Target) = Result_Length @key{and then}
                              Capacity (Target) >= Result_Length))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{end} Generic_Sorting;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[   @key{package} @AdaPackDefn{Stable} @key{is}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1],ARef=[AI12-0339-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Vector} (Base : @key{not null access} Vectors.Vector) @key{is}
         @key{tagged limited private}
         @key{with} Constant_Indexing => Constant_Reference,
              Variable_Indexing => Reference,
              Default_Iterator  => Iterate,
              Iterator_Element  => Element_Type,
              Aggregate         => (Empty          => Empty,
                                    Add_Unnamed    => Append_One,
                                    New_Indexed    => New_Vector,
                                    Assign_Indexed => Replace_Element),
              Stable_Properties => (Length, Capacity),
              Global            => @key{null},
              Default_Initial_Condition => Length (Vector) = 0;
      @key{pragma} Preelaborable_Initialization(Vector);]}

@begin{Discussion}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI05-0112-1]}
  @ChgAdded{Version=[5],Text=[The Global of @key{null} assumes that the
  user of a stable object is including effects associated with the
  access discriminant. For operations with @key{in} parameters (after
  any overriding), the object designated by the access discriminant is
  assumed to be read, and for other operations (including initialization
  and finalization) the object designated by the access discriminant is
  assumed to be read and updated.]}
@end{Discussion}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Cursor} @key{is private};
      @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @AdaObjDefn{Empty_Vector} : @key{constant} Vector;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean
         @key{with} Nonblocking, Global => (@key{in all}, @key{use null});]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{package} @AdaPackDefn{Vector_Iterator_Interfaces} @key[is new]
         Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} Vectors.Vector;
                        Source : @key{in} Vector)
         @key{with} Post => Length (Source) = Length (Target) @key{and then}
                     Capacity (Target) >= Length (Target);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Copy} (Source : Vectors.Vector) @key{return} Vector
         @key{with} Post => Length (Copy'Result) = Length (Source);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Constant_Reference_Type}
            (Element : @key{not null access constant} Element_Type) @key{is private}
         @key{with} Implicit_Dereference => Element,
              Nonblocking, Global => (@key{null}, @key{use null}),
              Default_Initial_Condition => (@key{raise} Program_Error);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Reference_Type}
            (Element : @key{not null access} Element_Type) @key{is private}
         @key{with} Implicit_Dereference => Element,
              Nonblocking, Global => (@key{null}, @key{use null}),
              Default_Initial_Condition => (@key{raise} Program_Error);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      -- @examcom{Additional subprograms as described in the text}
      -- @examcom{are declared here.}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[   @key{private}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      ... -- @Examcom{not specified by the language}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[   @key{end} Stable;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{private}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   ... -- @RI[not specified by the language]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{end} Ada.Containers.Vectors;]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
"=" on Element_Type values is expected to define a reflexive and symmetric
relationship and return the same result value each time it is called with a
particular pair of values. If it behaves in some other manner, the functions
defined to use it return an unspecified value. The exact arguments and number
of calls of this generic formal function by the functions defined to use it are
unspecified.@PDefn{unspecified}]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The @lquotes@;functions defined to use it@rquotes
  are Find, Find_Index, Reverse_Find, Reverse_Find_Index, and
  "=" for Vectors. This list is a bit too long to give explicitly.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If the actual function for "=" is not symmetric
  and consistent, the result returned by any of the functions defined to use
  "=" cannot be predicted. The implementation is not required to protect
  against "=" raising an exception, or returning random results, or any
  other @lquotes@;bad@rquotes behavior. And it can call "=" in whatever
  manner makes sense. But note that only the results of the functions defined
  to use "=" are unspecified; other subprograms are not allowed to break if
  "=" is bad.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The type Vector is used to represent vectors.
The type Vector needs finalization@PDefn2{Term=<needs finalization>,
Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Empty_Vector represents the empty vector object. It
has a length of 0. If an object of type Vector is not otherwise initialized, it
is initialized to the same value as Empty_Vector.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[No_Element represents a cursor that designates no
element. If an object of type Cursor is not otherwise initialized, it is
initialized to the same value as No_Element.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The predefined "=" operator for type Cursor returns
True if both cursors are No_Element, or designate the same element in the same
container.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Execution of the default implementation of the
Input, Output, Read, or Write attribute of type Cursor raises Program_Error.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[A cursor will probably be implemented in terms
  of one or more access values, and the effects of streaming access values is
  unspecified. Rather than letting the user stream junk by accident, we mandate
  that streaming of cursors raise Program_Error by default. The attributes
  can always be specified if there is a need to support streaming.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[Vector'Write for a Vector object @i<V> writes
Length(@i<V>) elements of the vector to the stream. It also may write
additional information about the vector.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[Vector'Read reads the representation of a vector
from the stream, and assigns to @i<Item> a vector with the same length and
elements as was written by Vector'Write.]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The Standard requires streaming of all
  language-defined nonlimited types (including containers) to "work" (see
  @RefSecNum{Stream-Oriented Attributes}). In addition, we do not want all
  of the elements that make up the
  capacity of the vector streamed, as those beyond the length of the container
  have undefined contents (and might cause bad things when read back in).
  This will require a custom stream attribute
  implementation; the language-defined default implementation will not work
  (even for a bounded form, as that would most likely stream the entire
  capacity of the vector). There is a separate requirement that the unbounded
  and Bounded form use the same streaming representation for the same element
  type, see @RefSecNum{The Generic Package Containers.Bounded_Vectors}.]}
@end{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[No_Index represents a position that does not
correspond to any element. The subtype Extended_Index includes the indices
covered by Index_Type plus the value No_Index and, if it exists, the successor
to the Index_Type'Last.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We require the existence of Index_Type'First @en 1,
  so that No_Index and Last_Index of an empty vector is well-defined. We don't
  require the existence of Index_Type'Last + 1, as it is only used as the
  position of insertions (and needs to be allowed only when inserting an empty
  vector).]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[If an operation attempts to modify the vector such
that the position of the last element would be greater than Index_Type'Last,
then the operation propagates Constraint_Error.]}]}@ChgNote{Covered by preconditions.}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[We don't want to
  require an implementation to go to heroic efforts to handle index values
  larger than the base type of the index subtype.]}]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Redundant[Some
operations @Chg{Version=[5],New=[@Defn2{Term=[tamper with cursors],Sec=[of a vector]}@Defn2{Term=[tamper with elements],Sec=[of a vector]}],Old=[of this generic package have
access-to-subprogram parameters. To
ensure such operations are well-defined, they guard against certain actions by
the designated subprogram. In particular, some operations]} check for
@lquotes@;tampering with cursors@rquotes of a container because they depend on
the set of elements of the container remaining constant, and others check for
@lquotes@;tampering with elements@rquotes of a container because they depend on
elements of the container not being replaced.]@Chg{Version=[5],New=[ When
tampering with cursors is @i<prohibited>@Defn2{Term=[prohibited],Sec=[tampering with a vector]}
@Defn2{Term=[tampering],Sec=[prohibited for a vector]}for a particular
vector object @i<V>, Program_Error is propagated by the finalization
of @i<V>@Redundant[, as well as by a call that passes @i<V> to
certain of the operations of this package, as indicated by the precondition
of such an operation]. Similarly, when tampering with elements is @i<prohibited>
for @i<V>, Program_Error is propagated by a call that passes @i<V> to
certain of other operations of this package, as indicated by the precondition
of such an operation.],Old=[]}]}

@begin{NotIso}
@ChgAdded{Version=[5],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 91 through 97
are removed as preconditions now describe these rules.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgDeleted{Version=[5],Type=[Leading],Text=[@Chg{Version=[2],New=[@Defn2{Term=[tamper with cursors],Sec=[of a vector]}
A subprogram is said to
@i{tamper with cursors} of a vector object @i<V> if:],Old=[]}]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it inserts or deletes
elements of @i<V>, that is,
it calls the Insert, Insert_Space, Clear, Delete, or Set_Length
procedures with @i<V> as a parameter; or]}]}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[Operations which
  are defined to be equivalent to
  a call on one of these operations also are included. Similarly, operations
  which call one of these as part of their definition are included.]}]}
@end{Honest}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it finalizes @i<V>;
or]}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[it calls the Assign
procedure with @i<V> as the Target parameter; or]}]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[We don't need to explicitly mention
  @nt{assignment_statement}, because that finalizes the target object
  as part of the operation, and finalization of an object is already defined
  as tampering with cursors.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it calls the Move
procedure with @i<V> as a parameter.]}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0111-1]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[Swap,
  Sort, and Merge copy elements rather than reordering them, so they don't
  tamper with cursors.]}]}
@end{Discussion}

@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgDeleted{Version=[5],Type=[Leading],Text=[@Chg{Version=[2],New=[@Defn2{Term=[tamper with elements],Sec=[of a vector]}
A subprogram is said to @i{tamper with elements} of a vector object @i<V>
if:],Old=[]}]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it tampers with
cursors of @i<V>; or]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it replaces one or more
elements of @i<V>, that is, it calls the Replace_Element, Reverse_Elements, or
Swap procedures or the Sort or Merge procedures of an instance of
Generic_Sorting with @i<V> as a parameter.]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[Complete replacement
  of an element can cause its memory to be deallocated while another operation
  is holding onto a reference to it. That can't be allowed. However, a simple
  modification of (part of) an element is not a problem, so Update_Element does
  not cause a problem.]}]}
@end{Reason}

@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0265-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0110-1]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[@Defn2{Term=[prohibited],Sec=[tampering with a vector]}
@Defn2{Term=[tampering],Sec=[prohibited for a vector]}
When tampering with cursors is @i<prohibited> for a particular vector object
@i<V>, Program_Error is propagated by a call of any language-defined subprogram
that is defined to tamper with the cursors of @i<V>, leaving @i<V> unmodified.
Similarly, when tampering with elements is @i<prohibited> for a particular vector
object @i<V>, Program_Error is propagated by a call of any language-defined
subprogram that is defined to tamper with the elements of @i<V> @Redundant[(or
tamper with the cursors of @i<V>)], leaving @i<V>
unmodified.@Chg{Version=[4],New=[ These checks are made before any other
defined behavior of the body of the language-defined subprogram.],Old=[]}]}]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[Tampering with
  elements includes tampering with cursors, so we mention it only from
  completeness in the second sentence.]}]}
@end{TheProof}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Has_Element (Position : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => (@key[in all], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if Position designates
an element, and returns False otherwise.]}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[3],Text=[This function might not detect cursors that
  designate deleted elements; such cursors are invalid (see below) and the
  result of calling Has_Element with an invalid cursor is unspecified (but
  not erroneous).]}
@end{Honest}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} @AdaSubDefn{Has_Element} (Container : Vector; Position : Cursor)
   @key{return} Boolean
   @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns True if Position designates
an element in Container, and returns False otherwise.]}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[If Position is No_Element, Has_Element returns False.]}
@end{Ramification}


@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "=" (Left, Right : Vector) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Left and Right denote the same
vector object, then the function returns True. If Left and Right have different
lengths, then the function returns False.
Otherwise, it compares each element in Left to
the corresponding element in Right using the generic formal equality operator.
If any such comparison returns False, the function returns False;
otherwise@Chg{Version=[3],New=[,],Old=[]} it
returns True. Any exception raised during
evaluation of element equality is propagated.]}

@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This wording describes the canonical semantics.
However, the order and number of calls on the formal equality function is
unspecified for all of the operations that use it in this package, so an
implementation can call it as many or as few times as it needs to get the
correct answer. Specifically, there is no requirement to call the formal
equality additional times once the answer has been determined.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Tampering_With_Cursors_Prohibited
   (Container : Vector) @key{return} Boolean
   @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns True if tampering with
cursors or tampering with elements is currently prohibited for Container, and
returns False otherwise.]}

@begin{Reason}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[Prohibiting tampering with elements also
  needs to prohibit tampering with cursors, as deleting an element is similar
  to replacing it.]}
@end{Reason}

@begin{ImplNote}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[Various contracts elsewhere in this specification
    require that this function is implemented with synchronized data. Moreover,
    it is possible for tampering to be prohibited by multiple operations
    (sequentiually or in parallel). Therefore, tampering needs to be
    implemented with an atomic or protected counter. The counter is initialized
    to zero, and is incremented when tampering is prohibited, and decremented
    when leaving an area that prohibited tampering. Function
    Tampering_With_Cursors_Prohibited returns True if the counter is nonzero.
    (Note that any case where the result is not well-defined for one task
    is incorrect use of shared variables and would be erroneous by the rules
    of @RefSecNum{Shared Variables}, so no special protection is needed to
    read the counter.)]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Tampering_With_Elements_Prohibited
   (Container : Vector) @key{return} Boolean
   @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Always returns False@Redundant[,
regardless of whether tampering with elements is prohibited].]}

@begin{Reason}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1]}
  @ChgAdded{Version=[5],Text=[A definite element cannot
  change size, so we allow operations that tamper with elements even when
  tampering with elements is prohibited. That's not true for the indefinite
  containers, which is why this kind of tampering exists.]}
@end{Reason}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Maximum_Length @key{return} Count_Type
   @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns the maximum Length of a Vector, based on the
index type.]}

@begin{ImplNote}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Type=[Leading],Text=[This is just:]}

@begin{Example}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[Count_Type (Index_Type'Last - Index_Type'First + 1)]}
@end{Example}

  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Type=[Trailing],Text=[but since the inner calculation
  can overflow or the type conversion can fail, this can't be evaluated in
  general with an expression function. Note that if this expression raises
  Constraint_Error, then the result is Count_Type'Last, since the Capacity of
  a Vector cannot exceed Count_Type'Last.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Empty (Capacity : Count_Type := @RI<implementation-defined>)
   @key{return} Vector
   @key[with] Pre  => Capacity <= Maximum_Length @key[or else raise] Constraint_Error,
        Post => Capacity (Empty'Result) >= Capacity @key[and then]
                @key[not] Tampering_With_Elements_Prohibited (Empty'Result) @key[and then]
                @key[not] Tampering_With_Cursors_Prohibited (Empty'Result) @key[and then]
                Length (Empty'Result) = 0;]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0339-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns an empty vector.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} To_Vector (Length : Count_Type) @key{return} Vector@Chg{Version=[5],New=[
   @key[with] Pre  => Length <= Maximum_Length @key[or else raise] Constraint_Error,
        Post =>
           To_Vector'Result.Length = Length @key[and then]
           @key[not] Tampering_With_Elements_Prohibited (To_Vector'Result) @key[and then]
           @key[not] Tampering_With_Cursors_Prohibited (To_Vector'Result) @key[and then]
           To_Vector'Result.Capacity >= Length],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a vector with a length of
Length, filled with empty elements.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} To_Vector
  (New_Item : Element_Type;
   Length   : Count_Type) @key{return} Vector@Chg{Version=[5],New=[
   @key[with] Pre  => Length <= Maximum_Length @key[or else raise] Constraint_Error,
        Post =>
           To_Vector'Result.Length = Length @key[and then]
           @key[not] Tampering_With_Elements_Prohibited (To_Vector'Result) @key[and then]
           @key[not] Tampering_With_Cursors_Prohibited (To_Vector'Result) @key[and then]
        To_Vector'Result.Capacity >= Length],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a vector with a length of
Length, filled with elements initialized to the value New_Item.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "&" (Left, Right : Vector) @key{return} Vector@Chg{Version=[5],New=[
   @key{with} Pre  => Length (Left) <= Maximum_Length - Length (Right)
                 @key[or else raise] Constraint_Error,
        Post => Length (Vectors."&"'Result) = Length (Left) + Length (Right) @key[and then]
                @key[not] Tampering_With_Elements_Prohibited (Vectors."&"'Result) @key[and then]
                @key[not] Tampering_With_Cursors_Prohibited (Vectors."&"'Result) @key[and then]
                Vectors."&"'Result.Capacity >= Length (Left) + Length (Right)],Old=[]};]}@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a vector comprising the
elements of Left followed by the elements of Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "&" (Left  : Vector;
              Right : Element_Type) @key{return} Vector@Chg{Version=[5],New=[
   @key{with} Pre  => Length (Left) <= Maximum_Length - 1 
                @key[or else raise] Constraint_Error,
        Post => Vectors."&"'Result.Length = Length (Left) + 1 @key[and then]
                @key[not] Tampering_With_Elements_Prohibited (Vectors."&"'Result) @key[and then]
                @key[not] Tampering_With_Cursors_Prohibited (Vectors."&"'Result) @key[and then]
                Vectors."&"'Result.Capacity >= Length (Left) + 1],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a vector comprising the
elements of Left followed by the element Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "&" (Left  : Element_Type;
              Right : Vector) @key{return} Vector@Chg{Version=[5],New=[
   @key{with} Pre  => Length (Right) <= Maximum_Length - 1
                 @key[or else raise] Constraint_Error,
        Post => Length (Vectors."&"'Result) = Length (Right) + 1 @key[and then]
                @key[not] Tampering_With_Elements_Prohibited (Vectors."&"'Result) @key[and then]
                @key[not] Tampering_With_Cursors_Prohibited (Vectors."&"'Result) @key[and then]
                Vectors."&"'Result.Capacity >= Length (Right) + 1],Old=[]};]}@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a vector comprising the
element Left followed by the elements of Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "&" (Left, Right  : Element_Type) @key{return} Vector@Chg{Version=[5],New=[
   @key{with} Pre  => Maximum_Length >= 2 @key[or else raise] Constraint_Error,
        Post => Length ("&"'Result) = 2 @key[and then]
                @key[not] Tampering_With_Elements_Prohibited (Vectors."&"'Result) @key[and then]
                @key[not] Tampering_With_Cursors_Prohibited (Vectors."&"'Result) @key[and then]
                Vectors."&"'Result.Capacity >= 2],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a vector comprising the
element Left followed by the element Right.]}


@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Capacity (Container : Vector) @key{return} Count_Type@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the capacity of Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reserve_Capacity (Container : @key{in out} Vector;
                            Capacity  : @key{in}     Count_Type)@Chg{Version=[5],New=[
   @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                 @key[or else raise] Program_Error,
        Post => Container.Capacity >= Capacity],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0001-1],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[3],New=[If the
capacity of Container is already greater than or equal to Capacity, then
Reserve_Capacity has no effect. Otherwise, ],Old=[]}Reserve_Capacity allocates
@Chg{Version=[3],New=[additional storage as necessary to
ensure],Old=[new internal data structures such]} that the length of the
resulting vector can become at least the value Capacity without requiring an additional call to
Reserve_Capacity, and is large enough to hold the current length of Container.
Reserve_Capacity then@Chg{Version=[3],New=[, as necessary, moves],Old=[ copies the]}
elements into the new @Chg{Version=[3],New=[storage],Old=[data structures]} and
deallocates @Chg{Version=[3],New=[any storage no longer needed],Old=[the old
data structures]}. Any exception raised during allocation is
propagated and Container is not modified.]}

@begin{Discussion}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Expanding the internal array can be done by
  allocating a new, longer array, copying the elements, and deallocating the
  original array. This may raise Storage_Error, or cause an exception from a
  controlled subprogram. We require that a failed Reserve_Capacity does not
  lose any elements if an exception occurs, but we do not require a specific
  order of evaluations or copying.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This routine is used to preallocate the internal
  array to the specified capacity such that future Inserts do not require
  memory allocation overhead. Therefore, the implementation should allocate the
  needed memory to make that true at this point, even though the visible
  semantics could be preserved by waiting until the memory is needed. This
  doesn't apply to the indefinite element container, because elements will have
  to be allocated individually.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The implementation does not have to contract the
  internal array if the capacity is reduced, as any capacity greater than or
  equal to the specified capacity is allowed.]}

@end{Discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Length (Container : Vector) @key{return} Count_Type@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the number of elements in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Set_Length (Container : @key{in out} Vector;
                      Length    : @key{in}     Count_Type)@Chg{Version=[5],New=[
   @key[with] Pre  => (@key[not] Tampering_With_Cursors_Prohibited (Container)
                 @key[or else raise] Program_Error) @key[and then]
                (Length <= Maximum_Length @key[or else raise] Constraint_Error),
        Post => Container.Length = Length @key[and then]
                Capacity (Container) >= Length],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Length is larger than the
capacity of Container, Set_Length calls Reserve_Capacity (Container, Length),
then sets the length of the Container to Length. If Length is greater than the
original length of Container, empty elements are added to Container;
otherwise@Chg{Version=[3],New=[,],Old=[]}
elements are removed from Container.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[No elements are moved by this operation; any new
  empty elements are added at the end. This follows from the rules that a
  cursor continues to designate the same element unless the routine is
  defined to make the cursor ambiguous or invalid; this operation does not do
  that.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Is_Empty (Container : Vector) @key{return} Boolean@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => (@key[null], @key[use null]),
        Post => Is_Empty'Result = (Length (Container) = 0)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[Returns True
if Container is empty],Old=[Equivalent to Length (Container) = 0]}.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Clear (Container : @key{in out} Vector)@Chg{Version=[5],New=[
   @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                    @key[or else raise] Program_Error,
        Post => Length (Container) = 0],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Removes all the elements from
Container. The capacity of Container does not change.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} To_Cursor (Container : Vector;
                    Index     : Extended_Index) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Post => (@key{if} Index @key{in} First_Index (Container) .. Last_Index (Container)
                 @key{then} Has_Element (Container, To_Cursor'Result)
                 @key{else} To_Cursor'Result = No_Element),
        Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1],ARef=[AI12-0196-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[Returns],Old=[If
Index is not in the range
First_Index (Container) .. Last_Index (Container), then No_Element is returned.
Otherwise,]} a cursor designating the element at position Index
in Container@Chg{Version=[5],New=[; returns No_Element if Index does not
designate an element],Old=[ is
returned]}.@Chg{Version=[5],New=[ For the purposes of
determining whether the parameters overlap in a call to To_Cursor, the
Container parameter is not considered to overlap with any object
@Redundant[(including itself)].],Old=[]}]}

  @begin{Reason}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0196-1]}
  @ChgAdded{Version=[5],Text=[Without the preceding rule, concurrent calls to To_Cursor
   on the same container would interfere by the concurrent call rules in
   @RefSecNum{Predefined Language Environment},
   since the container object of the concurrent calls would overlap with itself.
   We want these to not interfere, for example to allow the Vector elements to
   be split into separate @ldquote@;chunks@rdquote for parallel processing.]}
  @end{Reason}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} To_Index (Position  : Cursor) @key{return} Extended_Index@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{in all}, @key{use null})],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position is No_Element, No_Index
is returned. Otherwise, the index (within its containing vector) of the element
designated by Position is returned.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This implies that the index is determinable from
  a bare cursor alone. The basic model is that a vector cursor is implemented
  as a record containing an access to the vector container and an index value.
  This does constrain implementations, but it also allows all of the cursor
  operations to be defined in terms of the corresponding index operation (which
  should be primary for a vector).]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} To_Index (Container : Vector;
                   Position  : Cursor) @key{return} Extended_Index
   @key{with} Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)  @key{or else}
                   @key{raise} Program_Error,
        Post => (@key{if} Position = No_Element @key{then} To_Index'Result = No_Index
                 @key{else} To_Index'Result @key{in} First_Index (Container) ..
                        Last_Index (Container)),
        Nonblocking, Global => (@key{null}, @key{use null});]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns the index (within
Container) of the element designated by Position; returns No_Index if Position
does not designate an element. For the purposes of
determining whether the parameters overlap in a call to To_Index, the
Container parameter is not considered to overlap with any object
@Redundant[(including itself)].]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Element (Container : Vector;
                  Index     : Index_Type)
   @key{return} Element_Type@Chg{Version=[5],New=[
   @key{with} Pre  => Index @key{in} First_Index (Container) .. Last_Index (Container)
                   @key{or else raise} Constraint_Error,
        Nonblocking => Element_Type'Nonblocking,
        Global => (@key{null}, @key{use} Element_Type)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Index is not in the range First_Index (Container) .. Last_Index (Container),
then  Constraint_Error is propagated. Otherwise, ]}Element returns the
element at position Index.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Element (Position  : Cursor) @key{return} Element_Type@Chg{Version=[5],New=[
   @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
        Nonblocking => Element_Type'Nonblocking,
        Global => (@key{in all}, @key{use} Element_Type)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
If Position equals No_Element, then Constraint_Error is propagated.
Otherwise, ]}Element returns the element
designated by Position.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Element (Container : Vector;
                  Position  : Cursor) @key{return} Element_Type
   @key{with} Pre => (Position /= No_Element @key{or else}
                   @key{raise} Constraint_Error) @key{and then}
                (Has_Element (Container, Position) @key{or else raise} Program_Error),
        Nonblocking => Element_Type'Nonblocking,
        Global => (@key{null}, @key{use} Element_Type);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Element returns the element
designated by Position in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace_Element (Container : @key{in out} Vector;
                           Index     : @key{in}     Index_Type;
                           New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
   @key{with} Pre => (@key{not} Tampering_With_Elements_Prohibited (Container)
                  @key{or else raise} Program_Error) @key{and then}
               (Index @key{in} First_Index (Container) .. Last_Index (Container)
                  @key{or else raise} Constraint_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1],ARef=[AI12-0196-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Index is not in the range
First_Index (Container) .. Last_Index (Container), then Constraint_Error is
propagated. Otherwise@Chg{Version=[3],New=[,],Old=[]}]}
Replace_Element assigns the value New_Item to the element at
position Index. Any exception raised during the assignment is propagated. The
element at position Index is not an empty element after successful call to
Replace_Element.@Chg{Version=[5],New=[ For the purposes of
determining whether the parameters overlap in a call to Replace_Element, the
Container parameter is not considered to overlap with any object
@Redundant[(including itself)], and the Index parameter is
considered to overlap with the element at position Index.],Old=[]}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace_Element (Container : @key{in out} Vector;
                           Position  : @key{in}     Cursor;
                           New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Elements_Prohibited (Container)
                   @key{or else raise} Program_Error) @key{and then}
                (Position /= No_Element 
                   @key{or else raise} Constraint_Error) @key{and then}
                (Has_Element (Container, Position) @key{or else raise} Program_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1],ARef=[AI12-0196-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element,
then Constraint_Error is propagated; if Position does not designate an element
in Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]} ]}Replace_Element
assigns New_Item to the element designated by Position. Any exception raised
during the assignment is propagated. The element at Position is not an empty
element after successful call to
Replace_Element.@Chg{Version=[5],New=[ For the purposes of
determining whether the parameters overlap in a call to Replace_Element, the
Container parameter is not considered to overlap with any object
@Redundant[(including itself)].],Old=[]}]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[2],Text=[Replace_Element@Chg{Version=[3],New=[,],Old=[ and]}
  Update_Element@Chg{Version=[3],New=[, and Reference],Old=[]} are the only
  ways that an element can change from empty to nonempty. Also see the note
  following Update_Element.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Query_Element
  (Container : @key{in} Vector;
   Index     : @key{in} Index_Type;
   Process   : @key{not null access procedure} (Element : @key{in} Element_Type))@Chg{Version=[5],New=[
   @key{with} Pre  => Index @key{in} First_Index (Container) .. Last_Index (Container)
                    @key{or else raise} Constraint_Error],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Index is not in the range
First_Index (Container) .. Last_Index (Container), then Constraint_Error is
propagated. Otherwise, ]}Query_Element calls Process.@key{all} with the element at
position Index as the argument.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of Container@Chg{Version=[3],New=[ is prohibited during the
execution of the call on Process.@key{all}],Old=[]}. Any exception raised by
Process.@key{all} is propagated.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[The @lquotes@;tamper with the elements@rquotes@;
  check is intended to prevent the Element parameter of Process from being
  @Chg{Version=[3],New=[replaced],Old=[modified]} or deleted
  outside of Process. The check prevents data loss (if Element_Type is passed by
  copy) or erroneous execution (if Element_Type is an unconstrained type in an
  indefinite container).]}
@end{Reason}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Query_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access procedure} (Element : @key{in} Element_Type))@Chg{Version=[5],New=[
   @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error],Old=[]};]}
        Global => @key{in all};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0021-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then
Constraint_Error is propagated. Otherwise, ]}Query_Element calls
Process.@key{all} with the element designated by Position as the argument.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of @Chg{Version=[3],New=[the vector that contains the
element designated by Position is prohibited during the
execution of the call on Process.@key{all}],Old=[Container]}. Any
exception raised by Process.@key{all} is propagated.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],Text=[@key{procedure} Query_Element
  (Container : @key{in} Vector;
   Position  : @key{in} Cursor;
   Process   : @key{not null access procedure} (Element : @key{in} Element_Type))
   @key{with} Pre  => (Position /= No_Element 
                    @key{or else raise} Constraint_Error) @key{and then}
                 (Has_Element (Container, Position) 
                    @key{or else raise} Program_Error);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Query_Element calls Process.@key{all}
with the element designated by Position as the
argument. Tampering with the elements of Container
is prohibited during the execution of the call on
Process.@key{all}. Any exception raised by Process.@key{all} is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Update_Element
  (Container : @key{in out} Vector;
   Index     : @key{in}     Index_Type;
   Process   : @key{not null access procedure}
                   (Element : @key{in out} Element_Type))@Chg{Version=[5],New=[
   @key{with} Pre  => Index @key{in} First_Index (Container) .. Last_Index (Container)
                    @key{or else raise} Constraint_Error],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Index is not in the range
First_Index (Container) .. Last_Index (Container), then Constraint_Error is
propagated. Otherwise, ]}Update_Element calls Process.@key{all} with the element
at position Index as the argument.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of Container@Chg{Version=[3],New=[ is prohibited during the
execution of the call on Process.@key{all}],Old=[]}. Any exception raised by
Process.@key{all} is propagated.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[If Element_Type is unconstrained and definite, then
the actual Element parameter of Process.@key{all} shall be unconstrained.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This means that the elements cannot be directly
  allocated from the heap; it must be possible to change the discriminants of
  the element in place.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[The element at position Index is
not an empty element after successful completion of this operation.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Since reading an empty element is a bounded
  error, attempting to use this procedure to replace empty elements may fail.
  Use Replace_Element to do that reliably.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Update_Element
  (Container : @key{in out} Vector;
   Position  : @key{in}     Cursor;
   Process   : @key{not null access procedure}
                   (Element : @key{in out} Element_Type))@Chg{Version=[5],New=[
   @key{with} Pre  => (Position /= No_Element @key{or else raise} Constraint_Error) @key{and then}
                 (Has_Element (Container, Position) @key{or else raise} Program_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then
Constraint_Error is propagated; if Position does not designate an element in
Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]} ]}Update_Element calls
Process.@key{all} with the element designated by Position as the argument.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of Container@Chg{Version=[3],New=[ is prohibited during the
execution of the call on Process.@key{all}],Old=[]}. Any exception raised by
Process.@key{all} is propagated.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[If Element_Type is unconstrained and definite, then
the actual Element parameter of Process.@key{all} shall be unconstrained.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[The element designated by Position
is not an empty element after successful completion of this operation.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@key[type] Constant_Reference_Type
      (Element : @key[not null access constant] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
        Nonblocking, Global => @key[in out synchronized],
        Default_Initial_Condition => (@key[raise] Program_Error)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[type] Reference_Type (Element : @key[not null access] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
        Nonblocking, Global => @key[in out synchronized],
        Default_Initial_Condition => (@key[raise] Program_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[The types Constant_Reference_Type
and Reference_Type need finalization.@PDefn2{Term=<needs finalization>,
Sec=<language-defined type>}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[DeletedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[The default
initialization of an object of type Constant_Reference_Type or Reference_Type
propagates Program_Error.]}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[It is expected that Reference_Type (and
  Constant_Reference_Type) will be a controlled type, for which finalization
  will have some action to terminate the tampering check for the associated
  container. If the object is created by default, however, there is no
  associated container. Since this is useless, and supporting this case would
  take extra work, we define it to raise an exception.]}
@end{Reason}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] Vector;
                             Index     : @key[in] Index_Type)
   @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
   @key[with] Pre    => Index @key[in] First_Index (Container) .. Last_Index (Container)
                     @key[or else raise] Constraint_Error,
        Post   => Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read access to an individual element of a vector given an
index value.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[If
Index is not in the range First_Index (Container) .. Last_Index (Container),
then Constraint_Error is propagated. Otherwise, ]}Constant_Reference returns an
object whose discriminant is an access value that designates the element at
position Index. Tampering with the elements of Container is prohibited while the
object returned by Constant_Reference exists and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference (Container : @key[aliased in out] Vector;
                    Index     : @key[in] Index_Type)
   @key[return] Reference_Type@Chg{Version=[5],New=[
   @key[with] Pre    => Index @key[in] First_Index (Container) .. Last_Index (Container)
                    @key[or else raise] Constraint_Error,
        Post   => Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read and write access to an individual element of a vector given
an index value.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[If
Index is not in the range First_Index (Container) .. Last_Index (Container),
then Constraint_Error is propagated. Otherwise, ]}Reference returns an object
whose discriminant is an access value that designates the element at position
Index. Tampering with the elements of Container is prohibited while the object
returned by Reference exists and has not been finalized.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The element at position Index is not an empty
element after successful completion of this operation.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] Vector;
                             Position  : @key[in] Cursor)
   @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
   @key[with] Pre  => (Position /= No_Element @key[or else]
                    @key[raise] Constraint_Error) @key[and then]
                 (Has_Element (Container, Position) @key[or else raise] Program_Error),
        Post   => Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read access to an individual element of a vector given a cursor.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then Constraint_Error
is propagated; if Position does not designate an element in Container, then
Program_Error is propagated. Otherwise, ]}Constant_Reference returns an object
whose discriminant is an access value that designates the element designated by
Position. Tampering with the elements of Container is prohibited while the
object returned by Constant_Reference exists and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference (Container : @key[aliased in out] Vector;
                    Position  : @key[in] Cursor)
   @key[return] Reference_Type@Chg{Version=[5],New=[
   @key[with] Pre  => (Position /= No_Element @key[or else]
                    @key[raise] Constraint_Error) @key[and then]
                 (Has_Element (Container, Position) @key[or else raise] Program_Error),
        Post   => Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read and write access to an individual element of a vector given
a cursor.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then Constraint_Error
is propagated; if Position does not designate an element in Container, then
Program_Error is propagated. Otherwise, ]}Reference returns an object whose
discriminant is an access value that designates the element designated by
Position. Tampering with the elements of Container is prohibited while the
object returned by Reference exists and has not been finalized.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The element designated by Position is not an empty
element after successful completion of this operation.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Assign (Target : @key{in out} Vector; Source : @key{in} Vector)@Chg{Version=[5],New=[
   @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Target)
                   @key[or else raise] Program_Error,
        Post => Length (Source) = Length (Target) @key{and then}
                Capacity (Target) >= Length (Target)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0248-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Target denotes the same object as
Source, the operation has no effect. If the length of Source is greater than the
capacity of Target, Reserve_Capacity (Target, Length (Source)) is called.
The elements of Source are then copied to Target as for an
@nt{assignment_statement} assigning Source to Target (this includes
setting the length of Target to be that of Source).]}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[3],Text=[This routine exists for compatibility with the
  bounded vector container. For an unbounded vector, @exam{Assign(A, B)} and
  @exam{A := B} behave identically. For a bounded vector, := will raise an
  exception if the container capacities are different, while Assign will
  not raise an exception if there is enough room in the target.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Copy (Source : Vector; Capacity : Count_Type := 0)
   @key[return] Vector@Chg{Version=[5],New=[
   @key[with] Pre => Capacity = 0 @key[or else] Capacity >= Length (Source)
                   @key[or else raise] Capacity_Error,
        Post => Length (Copy'Result) = Length (Source) @key[and then]
                @key[not] Tampering_With_Elements_Prohibited (Copy'Result) @key[and then]
                @key[not] Tampering_With_Cursors_Prohibited (Copy'Result) @key[and then]
                Copy'Result.Capacity >= (@key[if] Capacity = 0 @key[then]
                   Length (Source) @key[else] Capacity)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a vector whose elements are
initialized from the corresponding elements of Source.@Chg{Version=[5],New=[],Old=[ If
Capacity is 0, then the vector capacity is the length of Source; if Capacity is
equal to or greater than the length of Source, the vector capacity is at least
the specified value. Otherwise, the operation propagates Capacity_Error.]}]}


@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Move (Target : @key{in out} Vector;
                Source : @key{in out} Vector)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Target)
                    @key{or else raise} Program_Error) @key{and then}
                (@key{not} Tampering_With_Cursors_Prohibited (Source)
                    @key{or else raise} Program_Error),
        Post => (@key{if} Target = Source @key{then} True
                 @key{else}
                    Length (Target) = Length (Source'Old) @key{and then}
                    Length (Source) = 0 @key{and then}
                    Capacity (Target) >= Length (Source'Old))],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Target denotes the same object
as Source, then @Chg{Version=[3],New=[the operation],Old=[Move]}
has no effect. Otherwise, Move first calls
@Chg{Version=[3],New=[Reserve_Capacity (Target, Length (Source))
and then ],Old=[]}Clear (Target);
then, each element from Source is removed from Source and inserted into Target
in the original order.@Chg{Version=[5],New=[],Old=[ The length of Source is
0 after a successful call to Move.]}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The idea is that the internal array is removed
  from Source and moved to Target. (See the @ImplAdviceName for Move). If
  Capacity (Target) /= 0, the previous internal array may need to be
  deallocated. We don't mention this explicitly, because it is covered by the
  "no memory loss" @ImplReqName@;.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Extended_Index;
                  New_Item  : @key{in}     Vector)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Before @key{in} First_Index (Container) .. Last_Index (Container) + 1
                    @key{then raise} Constraint_Error) @key{and then}
                (Length (Container) <= Maximum_Length - Length (New_Item)
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Length (New_Item) = Length (Container) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[If
Before is not in the range First_Index (Container) .. Last_Index (Container) +
1, then Constraint_Error is propagated. ]}If Length(New_Item) is 0, then Insert
does nothing. Otherwise, it computes the new length @i<NL> as the sum of the
current length and Length (New_Item); if the value of Last appropriate for
length @i<NL> would be greater than
Index_Type'Last@Chg{Version=[3],New=[,],Old=[]} then Constraint_Error is
propagated.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If the current vector capacity is
less than @i<NL>, Reserve_Capacity (Container, @i<NL>) is called to
increase the vector capacity. Then Insert slides the elements in the range
Before .. Last_Index (Container) up by Length(New_Item) positions, and then
copies the elements of New_Item to the positions starting at Before. Any
exception raised during the copying is propagated.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Moving the elements does not necessarily involve
  copying. Similarly, since Reserve_Capacity does not require the copying of
  elements, it does not need to be explicitly called (the implementation can
  combine the operations if it wishes to).]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Vector)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Before = No_Element @key{or else}
                 Has_Element (Container, Before)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Maximum_Length - Length (New_Item)
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Length (New_Item) = Length (Container) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If @Chg{Version=[5],New=[],Old=[Before
is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
Otherwise, if]}
Length(New_Item) is 0, then Insert does nothing. If Before is No_Element, then
the call is equivalent to Insert (Container, Last_Index (Container) + 1,
New_Item); otherwise@Chg{Version=[3],New=[,],Old=[]}
the call is equivalent to Insert (Container, To_Index
(Before), New_Item);]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The check on Before checks that the cursor does
  not belong to some other Container. This check implies that a reference to
  the container is included in the cursor value. This wording is not meant to
  require detection of dangling cursors; such cursors are defined to be
  invalid, which means that execution is erroneous, and any result is allowed
  (including not raising an exception).]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Vector;
                  Position  :    @key{out} Cursor)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Before = No_Element @key{or else}
                 Has_Element (Container, Before)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Maximum_Length - Length (New_Item)
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Length (New_Item) = Length (Container) @key{and then}
                Has_Element (Container, Position) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Before is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
]}If Before
equals No_Element, then let @i<T> be Last_Index (Container) + 1; otherwise, let
@i<T> be To_Index (Before). Insert (Container, @i<T>, New_Item) is called, and
then Position is set to To_Cursor (Container, @i<T>).]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The messy wording is needed because Before is
  invalidated by Insert, and we don't want Position to be invalid after this
  call. An implementation probably only needs to copy Before to Position.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Extended_Index;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Before @key{in} First_Index (Container) .. Last_Index (Container) + 1
                    @key{or else raise} Constraint_Error) @key{and then}
                (Length (Container) <= Maximum_Length - Count
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container, Before, To_Vector (New_Item, Count));]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Before = No_Element @key{or else}
                 Has_Element (Container, Before)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Maximum_Length - Count
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container, Before, To_Vector (New_Item, Count));]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Before = No_Element @key{or else}
                 Has_Element (Container, Before)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Maximum_Length - Count
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                Has_Element (Container, Position) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container, Before, To_Vector (New_Item, Count), Position);]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0257-1]}
  @ChgAdded{Version=[3],Text=[If Count equals 0, Position will designate the
  element designated by Before, rather than a newly inserted element. Otherwise,
  Position will designate the first newly inserted element.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Extended_Index;
                  Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Before @key{in} First_Index (Container) .. Last_Index (Container) + 1
                    @key{or else raise} Constraint_Error) @key{and then}
                (Length (Container) <= Maximum_Length - Count
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[If
Before is not in the range
First_Index (Container) .. Last_Index (Container) + 1, then Constraint_Error is
propagated. ]}If Count is 0, then Insert does nothing. Otherwise, it
computes the new length @i<NL> as the sum of the current length and Count; if
the value of Last appropriate for length @i<NL> would be greater than
Index_Type'Last@Chg{Version=[3],New=[,],Old=[]} then
Constraint_Error is propagated.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If the current vector capacity is
less than @i<NL>, Reserve_Capacity (Container, @i<NL>) is called to
increase the vector capacity. Then Insert slides the elements in the
range Before .. Last_Index (Container) up by Count positions, and then inserts
elements that are initialized by default (see @RefSecNum{Object Declarations})
in the positions starting at Before.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Before = No_Element @key{or else}
                 Has_Element (Container, Before)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Maximum_Length - Count
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                Has_Element (Container, Position) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Before is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
]}If Before
equals No_Element, then let @i<T> be Last_Index (Container) + 1; otherwise, let
@i<T> be To_Index (Before). Insert (Container, @i<T>, Count) is called,
and then Position is set to To_Cursor (Container, @i<T>).]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This routine exists mainly to ease conversion
  between Vector and List containers. Unlike Insert_Space, this routine
  default initializes the elements it inserts, which can be more expensive
  for some element types.]}
@end{Reason}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0080-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Prepend (Container : @key{in out} Vector;
                   New_Item  : @key{in}     Vector@Chg{Version=[4],New=[],Old=[;
                   Count     : @key{in}     Count_Type := 1]})@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Maximum_Length - Length (New_Item)
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Length (New_Item) = Length (Container) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container, First_Index (Container), New_Item).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Prepend (Container : @key{in out} Vector;
                   New_Item  : @key{in}     Element_Type;
                   Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Maximum_Length - Count
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container, First_Index (Container), New_Item, Count).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Append (Container : @key{in out} Vector;
                  New_Item  : @key{in}     Vector)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Maximum_Length - Length (New_Item)
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Length (New_Item) = Length (Container) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container, Last_Index (Container) + 1, New_Item).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Append (Container : @key{in out} Vector;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Maximum_Length - Count
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container, Last_Index (Container) + 1, New_Item, Count).]}


@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{procedure} Append_One (Container : @key{in out} Vector;
                      New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Maximum_Length - 1
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + 1 = Length (Container) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1],ARef=[AI12-0212-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Equivalent to Insert (Container, Last_Index (Container) + 1, New_Item, 1).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert_Space (Container : @key{in out} Vector;
                        Before    : @key{in}     Extended_Index;
                        Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                 (Before @key{in} First_Index (Container) .. Last_Index (Container) + 1
                    @key{or else raise} Constraint_Error) @key{and then}
                 (Length (Container) <= Maximum_Length - Count
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[If
Before is not in the range
First_Index (Container) .. Last_Index (Container) + 1, then Constraint_Error is
propagated. ]}If Count is 0, then Insert_Space does nothing. Otherwise, it
computes the new length @i<NL> as the sum of the current length and Count; if
the value of Last appropriate for length @i<NL> would be greater than
Index_Type'Last@Chg{Version=[3],New=[,],Old=[]} then
Constraint_Error is propagated.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If the current vector capacity is
less than @i<NL>, Reserve_Capacity (Container, @i<NL>) is called to
increase the vector capacity. Then Insert_Space slides the elements in the
range Before .. Last_Index (Container) up by Count positions, and then inserts
empty elements in the positions starting at Before.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert_Space (Container : @key{in out} Vector;
                        Before    : @key{in}     Cursor;
                        Position  :    @key{out} Cursor;
                        Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                 (Before = No_Element @key{or else}
                    Has_Element (Container, Before)
                    @key{or else raise} Program_Error) @key{and then}
                 (Length (Container) <= Maximum_Length - Count
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Count = Length (Container) @key{and then}
                Has_Element (Container, Position) @key{and then}
                Capacity (Container) >= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Before is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
]}If Before
equals No_Element, then let @i<T> be Last_Index (Container) + 1; otherwise, let
@i<T> be To_Index (Before). Insert_Space (Container, @i<T>, Count) is called,
and then Position is set to To_Cursor (Container, @i<T>).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} Vector;
                  Index     : @key{in}     Extended_Index;
                  Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Index @key{in} First_Index (Container) .. Last_Index (Container) + 1
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old - Count <= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Index is not in the range
First_Index (Container) .. Last_Index (Container) + 1, then Constraint_Error is
propagated. ]}If Count is 0, Delete has no effect.
Otherwise@Chg{Version=[3],New=[,],Old=[]} Delete slides the
elements (if any) starting at position Index + Count down to Index. Any
exception raised during element assignment is propagated.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If Index + Count >= Last_Index(Container), this
  effectively truncates the vector (setting Last_Index to Index @en 1 and
  consequently sets Length to Index @en Index_Type'First).]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} Vector;
                  Position  : @key{in out} Cursor;
                  Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Position /= No_Element 
                    @key{or else raise} Constraint_Error) @key{and then}
                (Has_Element (Container, Position)
                    @key{or else raise} Program_Error),
        Post => Length (Container)'Old - Count <= Length (Container) @key{and then}
                Position = No_Element],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then
Constraint_Error is propagated. If Position does not designate an element in
Container, then Program_Error is propagated. Otherwise, ]}Delete (Container,
To_Index (Position), Count) is called, and then Position is set to No_Element.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_First (Container : @key{in out} Vector;
                        Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => Length (Container)'Old - Count <= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Delete (Container, First_Index (Container), Count).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_Last (Container : @key{in out} Vector;
                       Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => Length (Container)'Old - Count <= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If
Length (Container) <= Count@Chg{Version=[3],New=[,],Old=[]} then
Delete_Last is equivalent to Clear (Container).
Otherwise@Chg{Version=[3],New=[,],Old=[]} it is equivalent to
Delete (Container, Index_Type'Val(Index_Type'Pos(Last_Index (Container)) @en
Count + 1), Count).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reverse_Elements (Container : @key{in out} @Chg{Version=[3],New=[Vector],Old=[List]})@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error],Old=[]};]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Reorders the elements of Container
in reverse order.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This can copy the elements of the vector @em
  all cursors referencing the vector are ambiguous afterwards and may
  designate different elements afterwards.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Swap (Container : @key{in out} Vector;
                I, J      : @key{in}     Index_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Elements_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
               (I @key{in} First_Index (Container) .. Last_Index (Container)
                    @key{or else raise} Constraint_Error) @key{and then}
               (J @key{in} First_Index (Container) .. Last_Index (Container)
                    @key{or else raise} Constraint_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
either I or J is not in the range First_Index (Container) .. Last_Index
(Container), then Constraint_Error is propagated. Otherwise, ]}Swap exchanges
the values of the elements at positions I and J.]}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The implementation is not required to actually
  copy the elements if it can do the swap some other way. But it is allowed
  to copy the elements if needed.]}
@end{Honest}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Swap (Container : @key{in out} Vector;
                I, J      : @key{in}     Cursor)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Elements_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (I /= No_Element @key{or else} Constraint_Error) @key{and then}
                (J /= No_Element @key{or else} Constraint_Error) @key{and then}
                (Has_Element (Container, I)
                    @key{or else raise} Program_Error) @key{and then}
                (Has_Element (Container, J)
                    @key{or else raise} Program_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
either I or J is No_Element, then Constraint_Error is propagated. If either I or
J do not designate an element in Container, then Program_Error is propagated.
Otherwise, ]}Swap exchanges the values of the elements designated by I and J.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[After a call to Swap, I designates the element
  value previously designated by J, and J designates the element value
  previously designated by I. The cursors do not become ambiguous from this
  operation.]}
@end{Ramification}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The implementation is not required to actually
  copy the elements if it can do the swap some other way. But it is allowed
  to copy the elements if needed.]}
@end{Honest}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First_Index (Container : Vector) @key{return} Index_Type@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Post => First_Index'Result = Index_Type'First],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the value Index_Type'First.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We'd rather call this @lquotes@;First@rquotes@;,
  but then calling most routines in here with First (Some_Vect) would be
  ambiguous.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First (Container : Vector) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Post => (@key{if not} Is_Empty (Container)
                 @key{then} Has_Element (Container, First'Result)
                 @key{else} First'Result = No_Element)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, First
returns No_Element. Otherwise, it returns a cursor that designates the first
element in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First_Element (Container : Vector)
   @key{return} Element_Type@Chg{Version=[5],New=[
   @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Container, First_Index (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last_Index (Container : Vector) @key{return} Extended_Index@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Post => (@key{if} Length (Container) = 0 @key{then} Last_Index'Result = No_Index
                 @key{else} Count_Type(Last_Index'Result - Index_Type'First) =
                      Length (Container) - 1)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, Last_Index
returns No_Index. Otherwise, it returns the position of the last element in
Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last (Container : Vector) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Post => (@key{if} not Is_Empty (Container)
                 @key{then} Has_Element (Container, Last'Result)
                 @key{else} Last'Result = No_Element)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, Last returns
No_Element. Otherwise, it returns a cursor that designates the last element in
Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last_Element (Container : Vector)
   @key{return} Element_Type@Chg{Version=[5],New=[
   @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Container,
Last_Index (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Next (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Global => (@key{in all}, @key{use null}),
        Nonblocking => True,
        Post => (@key{if} Position = No_Element @key{then} Next'Result = No_Element
                 @key{else} True)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element or
designates the last element of the container, then Next returns the value
No_Element. Otherwise, it returns a cursor that designates the element with index
To_Index (Position) + 1 in the same vector as Position.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Next (Container : Vector;
               Position : Cursor) @key{return} Cursor
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)
                    @key{or else raise} Program_Error,
        Post => (@key{if} Position = No_Element @key{then} Next'Result = No_Element
                 @key{elsif} Has_Element (Container, Next'Result) @key{then}
                    To_Index (Container, Next'Result) =
                    To_Index (Container, Position) + 1
                 @key{elsif} Next'Result = No_Element @key{then}
                    Position = Last (Container)
                 @key{else} False);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns a cursor designating the
next element in Container, if any.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Next (Position : @key{in out} Cursor)@Chg{Version=[5],New=[
   @key{with} Global => (@key{in all}, @key{use null}),
        Nonblocking => True],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Next (Position).]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{procedure} Next (Container : @key{in}     Vector;
                Position  : @key{in out} Cursor)
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)
                    @key{or else raise} Program_Error,
        Post => (@key{if} Position /= No_Element
                 @key{then} Has_Element (Container, Position));]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Equivalent to Position := Next
(Container, Position).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Previous (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Global => (@key{in all}, @key{use null}),
        Nonblocking => True,
        Post => (@key{if} Position = No_Element @key{then} Previous'Result = No_Element
                 @key{else} True)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element or
designates the first element of the container, then Previous returns the value
No_Element. Otherwise, it returns a cursor that designates the element with index
To_Index (Position) @en 1 in the same vector as Position.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Previous (Container : Vector;
                   Position  : Cursor) @key{return} Cursor
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)
                    @key{or else raise} Program_Error,
        Post => (@key{if} Position = No_Element @key{then} Previous'Result = No_Element
                 @key{elsif} Has_Element (Container, Previous'Result) @key{then}
                    To_Index (Container, Previous'Result) =
                    To_Index (Container, Position) - 1
                 @key{elsif} Previous'Result = No_Element @key{then}
                    Position = First (Container)
                 @key{else} False);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns a cursor designating the
previous element in Container, if any.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Previous (Position : @key{in out} Cursor)@Chg{Version=[5],New=[
   @key{with} Global => (@key{in all}, @key{use null}),
        Nonblocking => True],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Previous (Position).]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{procedure} Previous (Container : @key{in}     Vector;
                    Position  : @key{in out} Cursor)
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)
                    @key{or else raise} Program_Error,
        Post => (@key{if} Position /= No_Element
                 @key{then} Has_Element (Container, Position));]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Equivalent to Position :=
Previous (Container, Position).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Find_Index (Container : Vector;
                     Item      : Element_Type;
                     Index     : Index_Type := Index_Type'First)
   @key{return} Extended_Index;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Searches the elements of Container
for an element equal to Item (using the generic formal equality
operator). The search starts at position Index and proceeds towards Last_Index
(Container). If no equal element is found, then Find_Index returns No_Index.
Otherwise, it returns the index of the first equal element encountered.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Find (Container : Vector;
               Item      : Element_Type;
               Position  : Cursor := No_Element)
   @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)
                    @key{or else raise} Program_Error,
              Post => (@key{if} Find'Result = No_Element @key{then} True
                 @key{else} Has_Element (Container, Find'Result))],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]}
]}Find searches the elements of Container for an element equal to Item
(using the generic formal equality operator). The search starts at
the first element if Position equals No_Element, and at the element designated by
Position otherwise. It proceeds towards the last element of Container. If no
equal element is found, then Find returns No_Element. Otherwise, it returns a
cursor designating the first equal element encountered.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Reverse_Find_Index (Container : Vector;
                             Item      : Element_Type;
                             Index     : Index_Type := Index_Type'Last)
   @key{return} Extended_Index;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Searches the elements of Container
for an element equal to Item (using the generic formal equality
operator). The search starts at position Index or, if Index is greater than
Last_Index (Container), at position Last_Index (Container). It proceeds towards
First_Index (Container). If no equal element is found, then Reverse_Find_Index
returns No_Index. Otherwise, it returns the index of the first equal element
encountered.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Reverse_Find (Container : Vector;
                       Item      : Element_Type;
                       Position  : Cursor := No_Element)
   @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)
                    @key{or else raise} Program_Error,
              Post => (@key{if} Reverse_Find'Result = No_Element @key{then} True
                 @key{else} Has_Element (Container, Reverse_Find'Result))],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]}
]}Reverse_Find searches the elements of Container for an element equal
to Item (using the generic formal equality operator). The search
starts at the last element if Position equals No_Element, and at the element
designated by Position otherwise. It proceeds towards the first element of
Container. If no equal element is found, then Reverse_Find returns No_Element.
Otherwise, it returns a cursor designating the first equal element
encountered.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Contains (Container : Vector;
                   Item      : Element_Type) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Has_Element (Find (Container, Item)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@Chg{Version=[3],New=[],Old=[@key{function} Has_Element (Position : Cursor) @key{return} Boolean;]}]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[3],New=[],Old=[Returns True if Position designates
an element, and returns False otherwise.]}]}
@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[This function may not detect cursors that
  designate deleted elements; such cursors are invalid (see below) and the
  result of calling Has_Element with an invalid cursor is unspecified (but
  not erroneous).]}]}
@end{Honest}
@begin{NotIso}
@ChgAdded{Version=[3],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 225 and 226
were moved above.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}


@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Iterate
  (Container : @key{in} Vector;
   Process   : @key{not null access} @key{procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
   @key{with} Allows_Exit],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Invokes Process.@key{all} with a
cursor that designates each element in Container, in index order.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the cursors of Container@Chg{Version=[3],New=[ is prohibited during the
execution of a call on Process.@key{all}],Old=[]}. Any exception raised by
Process.@key{all} is propagated.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The purpose of the @lquotes@;tamper with the
  cursors@rquotes@; check is
  to prevent erroneous execution from the Position parameter of Process.@key{all}
  becoming invalid. This check takes place when the operations that tamper with
  the cursors of the container are called. The check cannot be made later (say
  in the body of Iterate), because that could cause the Position cursor to be
  invalid and potentially cause execution to become erroneous -- defeating the
  purpose of the check.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[ There is no check needed if an attempt is made
  to insert or delete nothing (that is, Count = 0 or Length(Item) = 0).]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The check is easy to implement: each container
  needs a counter. The counter is incremented when Iterate is called, and
  decremented when Iterate completes. If the counter is nonzero when an
  operation that inserts or deletes is called, Finalize is called, or one of
  the other operations in the list occurs, Program_Error is raised.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reverse_Iterate
  (Container : @key{in} Vector;
   Process   : @key{not null access} @key{procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
   @key{with} Allows_Exit],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Iterates over the elements in
Container as per @Chg{Version=[3],New=[procedure ],Old=[]}Iterate,
except that elements are traversed in reverse index order.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0212-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] Vector)
   @key[return] Vector_Iterator_Interfaces.@Chg{Version=[5],New=[Parallel_Reversible_Iterator],Old=[Reversible_Iterator]}'Class@Chg{Version=[5],New=[
   @key[with] Post   => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1],ARef=[AI05-0269-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate returns
@Chg{Version=[5],New=[an],Old=[a reversible]}
iterator object (see @RefSecNum{User-Defined Iterator Types}) that
will generate a value for a loop parameter (see
@RefSecNum{Generalized Loop Iteration}) designating
each node in Container, starting with the first node and moving the cursor as
per the Next function when used as a forward iterator, and starting with the
last node and moving the cursor as per the Previous function when used as a
reverse iterator@Chg{Version=[5],New=[, and processing all nodes
concurrently when used as a parallel iterator],Old=[]}. Tampering with the
cursors of Container is prohibited while
the iterator object exists (in particular, in
the @nt{sequence_of_statements} of the @nt{loop_statement} whose
@nt{iterator_specification} denotes this object). The iterator object needs
finalization.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] Vector; Start : @key[in] Cursor)
   @key[return] Vector_Iterator_Interfaces.Reversible_Iterator'Class@Chg{Version=[5],New=[
   @key[with] Pre    => (Start /= No_Element
                         @key[or else raise] Constraint_Error) @key[and then]
                     (Has_Element (Container, Start)
                         @key[or else raise] Program_Error),
        Post   => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0262-1],ARef=[AI05-0265-1],ARef=[AI05-0269-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Start is not No_Element and does
not designate an item in Container, then Program_Error is propagated. If Start
is No_Element, then Constraint_Error is propagated. Otherwise, ]}Iterate
returns a reversible iterator object
(see @RefSecNum{User-Defined Iterator Types}) that will generate
a value for a loop parameter (see @RefSecNum{Generalized Loop Iteration})
designating each node in Container, starting with the node
designated by Start and moving the cursor as per the Next function when used as
a forward iterator, or moving the cursor as per the Previous function when used
as a reverse iterator. Tampering with the
cursors of Container is prohibited while the iterator object exists (in
particular, in the @nt{sequence_of_statements} of the @nt{loop_statement} whose
@nt{iterator_specification} denotes this object). The iterator object needs
finalization.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[Exits are allowed from the loops
  created using the iterator objects. In particular, to stop the iteration at a
  particular cursor, just add]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[exit when] Cur = Stop;]}
@end{Example}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[in the body of the loop (assuming
  that @exam{Cur} is the loop parameter and @exam{Stop} is the cursor that you
  want to stop at).]}
@end{Discussion}


@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0044-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
"<" of Generic_Sorting is expected to return the same value each time it is
called with a particular pair of element values. It should define a strict
@Chg{Version=[3],New=[weak ],Old=[]}ordering relationship@Chg{Version=[3],
New=[ (see @RefSecNum{Containers})],Old=[, that is, be irreflexive, asymmetric,
and transitive]};
it should not modify Container. If the actual for "<" behaves in some other
manner, the behavior of the subprograms of Generic_Sorting are unspecified.
@Chg{Version=[3],New=[The number of],Old=[How many]} times the subprograms of
Generic_Sorting call "<" is unspecified.@PDefn{unspecified}]}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Is_Sorted (Container : Vector) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns True if the elements are
sorted smallest first as determined by the
generic formal "<" operator; otherwise, Is_Sorted returns False.
Any exception raised during evaluation of "<" is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Sort (Container : @key{in out} Vector)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{if} Tampering_With_Elements_Prohibited (Container)
                 @key{then raise} Program_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Reorders the elements of Container
such that the elements are
sorted smallest first as determined by the generic formal "<" operator
provided. Any exception raised during evaluation of "<" is propagated.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This implies swapping the elements, usually
  including an intermediate copy. This means that the elements will usually be
  copied. (As with Swap, if the implementation can do this some other way, it
  is allowed to.) Since the elements are nonlimited, this usually will not be
  a problem. Note that there is @ImplAdviceName below that the implementation
  should use a sort that minimizes copying of elements.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The sort is not required to be stable (and the
  fast algorithm required will not be stable). If a stable sort is needed, the
  user can include the original location of the element as an extra "sort key".
  We considered requiring the implementation to do that, but it is mostly extra
  overhead -- usually there is something already in the element that provides
  the needed stability.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Merge (Target  : @key{in out} Vector;
                    Source  : @key{in out} Vector)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Target)
                    @key{or else raise} Program_Error) @key{and then}
                (@key{not} Tampering_With_Cursors_Prohibited (Source)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Target) <= Maximum_Length - Length (Source)
                    @key{or else raise} Constraint_Error),
        Post => (@key{declare}
                   Result_Length : @key{constant} Count_Type :=
                      Length (Source)'Old + Length (Target)'Old;
                 @key{begin}
                    (@key{if} Target = Source @key{then} True
                     @key{else} Length (Source) = 0 @key{and then}
                        Length (Target) = Result_Length @key{and then}
                        Capacity (Target) >= Result_Length))],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0021-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[3],New=[If Source is
empty, then Merge does nothing. If Source and Target are the same nonempty
container object, then Program_Error is propagated. Otherwise, ],Old=[]}Merge
removes elements from Source and inserts them into Target; afterwards, Target
contains the union of the elements that were initially in Source and Target;
Source is left empty. If Target and Source are initially sorted smallest first,
then Target is ordered smallest first as determined by the generic formal "<"
operator; otherwise, the order of elements in Target is unspecified. Any
exception raised during evaluation of "<" is propagated.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[It is a bounded error if either of the vectors is
  unsorted, see below. The bounded error can be recovered by sorting Target
  after the merge call, or the vectors can be pretested with Is_Sorted.]}
@end{Discussion}
@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The Merge operation will usually require copying
  almost all of the elements. One implementation strategy would be to
  extend Target to the appropriate length, then copying elements from the back
  of the vectors working towards the front. An alternative approach would be
  to allocate a new internal data array of the appropriate length, copy the
  elements into it in an appropriate order, and then replacing the data array
  in Target with the temporary.]}
@end{ImplNote}

@end{DescribeCode}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[The nested package Vectors.Stable
provides a type Stable.Vector that represents
a @i<stable> vector,@Defn2{Term=(stable),Sec=(vector)} which is one that
cannot grow and shrink. Such a vector can be created by calling the
To_Vector or Copy functions, or by establishing a
@i<stabilized view> of a regular Vector.@Defn2{Term=[stabilized view],Sec=[vector]}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Type=[Leading],Text=[The subprograms of package
Containers.Vectors that have a parameter or result of type Vector are included
in the nested package Stable with the same specification, except that the
following are omitted:]}

@begin{Indent}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],Text=[Tampering_With_Cursors_Prohibited,
Tampering_With_Elements_Prohibited, Reserve_Capacity, Assign, Move,
Insert, Insert_Space, Clear, Delete, Delete_First, Delete_Last, and
Set_Length]}
@end{Indent}

@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Generic package Generic_Sorting is
also included with the same specification, except that Merge is omitted.]}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[Added]}
  @ChgAdded{Version=[5],Text=[The names Vector and Cursor mean the types
    declared in the nested package in these subprogram specifications.]}
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[5],Kind=[Added]}
  @ChgAdded{Version=[5],Text=[The omitted routines are those that tamper with
    cursors or elements (or test that state). The model is that it is
    impossible to tamper with cursors or elements of a stable view since no
    such operations are included. Thus tampering checks are not needed for
    a stable view, and we omit the operations associated with those checks.]}
@end{Reason}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[The operations of this package are equivalent
to those for regular Vectors, except that the calls to
Tampering_With_Cursors_Prohibited and
Tampering_With_Elements_Prohibited that occur in preconditions are replaced
by False, and any that occur in postconditions are replaced by True.]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[If a stable vector is declared with the Base
discriminant designating a pre-existing regular vector, the stable vector
represents a stabilized view of the underlying regular vector, and any operation
on the stable vector is reflected on the underlying regular vector. While a
stabilized view exists, any operation that tampers with elements performed on
the underlying vector is prohibited. The finalization of a stable vector that
provides such a view removes this restriction on the underlying regular vector
@Redundant[(though some other restriction might exist due to other concurrent
iterations or stabilized views)].]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[If a stable vector is declared without specifying
Base, the object must be initialized. The initializing expression of the stable
vector, @Redundant[typically a call on To_Vector or Copy], determines the Length
of the vector. The Length of a stable vector never changes after
initialization.]}

@end{StaticSem}

@begin{Bounded}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
Reading the value of an empty element by calling
Element, Query_Element, Update_Element,@Chg{Version=[3],New=[
Constant_Reference, Reference,],Old=[]} Swap, Is_Sorted, Sort, Merge,
"=", Find, or Reverse_Find is a bounded error. The implementation may treat
the element as having any normal value (see @RefSecNum{Data Validity}) of the
element type, or raise
Constraint_Error@Defn2{Term=[Constraint_Error],Sec=(raised by detection of a bounded error)}
or Program_Error@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}
before modifying the vector.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[For instance, a default initialized element could
  be returned. Or some previous value of an element. But returning random junk
  is not allowed if the type has default initial value(s).]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Assignment and streaming of empty elements are
  @b<not> bounded errors. This is consistent with regular composite types, for
  which assignment and streaming of uninitialized components do not cause a
  bounded error, but reading the uninitialized component does cause a bounded
  error.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[There are other operations which are defined in
  terms of the operations listed above.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
Calling Merge in an instance of Generic_Sorting
with either Source or Target not ordered smallest first using the provided
generic formal "<" operator is a bounded error. Either Program_Error is raised
after Target is updated as described for Merge, or the operation works as
defined.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0022-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}It
is a bounded error for the actual function
associated with a generic formal subprogram, when called as part of an
operation of this package, to tamper with elements of any Vector parameter of
the operation. Either Program_Error is raised, or the operation works as
defined on the value of the Vector either prior to, or subsequent to, some or
all of the modifications to the Vector.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0027-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}It
is a bounded error to call any subprogram
declared in the visible part of Containers.Vectors
when the associated container has been finalized. If the operation takes
Container as an @key[in out] parameter, then it raises Constraint_Error or
Program_Error. Otherwise, the operation either proceeds as it would
for an empty container, or it raises
Constraint_Error@Defn2{Term=[Constraint_Error],Sec=(raised by detection of a bounded error)}
or Program_Error.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[
@Defn2{Term=[ambiguous cursor],Sec=[of a vector]}
@Defn2{Term=[cursor],Sec=[ambiguous]}
A Cursor value is @i{ambiguous} if any of the following have occurred since it
was created:]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Insert, Insert_Space, or Delete has been called on
the vector that contains the element the cursor designates with an index value
(or a cursor designating an element at such an index value) less than or equal
to the index value of the element designated by the cursor; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The vector that contains the element it designates
has been passed to the Sort or Merge procedures of an instance of
Generic_Sorting, or to the Reverse_Elements procedure.]}

@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to call any subprogram other than "="
or Has_Element declared in Containers.Vectors with an ambiguous (but not
invalid, see below) cursor parameter. Possible results are:]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The cursor may be treated as if it were No_Element;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The cursor may designate some element in the vector
(but not necessarily the element that it originally designated);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Constraint_Error may be raised; or@Defn2{Term=[Constraint_Error],Sec=(raised by detection of a bounded error)}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Program_Error may be raised.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}

@end{Itemize}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Cursors are made ambiguous if an Insert or Delete
  occurs that moves the elements in the internal array including the designated
  ones. After such an operation, the cursor probably still designates an
  element (although it might not after a deletion), but it is a @i<different>
  element. That violates the definition of cursor @em it designates a particular
  element.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[For "=" or Has_Element, the cursor works normally
  (it would not be No_Element). We don't want to trigger an exception simply
  for comparing a bad cursor.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[While it is possible to check for these cases
  or ensure that cursors survive such operations, in many cases the overhead
  necessary to make the check (or ensure cursors continue to designate the
  same element) is substantial in time or space.]}
@end{Reason}

@end{Bounded}

@begin{Erron}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[
A Cursor value is @i{invalid} if any of the following have occurred since it
was created:@Defn2{Term=[invalid cursor],Sec=[of a vector]}
@PDefn2{Term=[cursor],Sec=[invalid]}]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The vector that contains the element it designates
has been finalized;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0160-1]}
@ChgAdded{Version=[3],Text=[The vector that contains the element it designates
has been used as the Target of a call to Assign, or as the target of an
@nt{assignment_statement};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@Redundant[The vector that contains the element it
designates has been used as the Source or Target of a call to Move;] or]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[Move has been reworded in terms of Assign and
  Clear, which are covered by other bullets, so this text is redundant.]}
@end{TheProof}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0160-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[The element it designates has been
deleted@Chg{Version=[3],New=[ or removed from the vector that previously
contained the element],Old=[]}.]}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
  @ChgAdded{Version=[3],Text=[An element can be removed via calls to Set_Length,
  Clear, and Merge; and indirectly via calls to Assign and Move.]}
@end{Ramification}

@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The result of "=" or Has_Element is unspecified if
it is called with an invalid cursor parameter.@PDefn{unspecified} Execution
is erroneous if any other subprogram declared in Containers.Vectors is called
with an invalid cursor parameter.@PDefn2{Term=(erroneous execution),
Sec=(cause)}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The list above (combined with the bounded error
  cases) is intended to be exhaustive. In other cases, a cursor value continues
  to designate its original element. For instance, cursor values survive the
  appending of new elements.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[Execution is erroneous if the vector associated with the result of a call to
Reference or Constant_Reference is finalized before the result object returned
by the call to Reference or Constant_Reference is finalized.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Each object of Reference_Type and
  Constant_Reference_Type probably contains some reference to the originating
  container. If that container is prematurely finalized (which is only possible
  via Unchecked_Deallocation, as accessibility checks prevent passing a
  container to Reference that will not live as long as the result), the
  finalization of the object of Reference_Type will try to access a nonexistent
  object. This is a normal case of a dangling pointer created by
  Unchecked_Deallocation; we have to explicitly mention it here as the pointer
  in question is not visible in the specification of the type. (This is the same
  reason we have to say this for invalid cursors.)]}
@end{Reason}
@end{Erron}

@begin{ImplReq}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[No storage associated with a vector object shall be
lost upon assignment or scope exit.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[The execution of an @nt{assignment_statement} for
a vector shall have the effect of copying the elements from the source vector
object to the target vector object@Chg{Version=[3],New=[ and changing the length
of the target object to that of the source object],Old=[]}.]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0298-1]}
  @ChgAdded{Version=[2],Text=[An assignment of a Vector is a @lquotes@;deep@rquotes
  copy; that is the elements are copied as well as the data structures.
  We say @lquotes@;effect of@rquotes in order to allow the implementation to
  avoid copying elements immediately if it wishes. For instance, an
  implementation that avoided copying until one of the containers is modified
  would be allowed.@Chg{Version=[3],New=[ (Note that such an implementation
  would be require care, as Query_Element and Constant_Reference both could be
  used to access an element which later needs to be reallocated while
  the parameter or reference still exists, potentially leaving the
  parameter or reference pointing at the wrong element.)],Old=[]}]}
@end{ImplNote}
@end{ImplReq}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Containers.Vectors should be implemented similarly
to an array. In particular, if the length of a vector is @i{N}, then]}

@begin{Itemize}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[the worst-case time complexity of Element should
  be @i{O}(log @i{N});]}
  @ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
  Text=[The worst-case time complexity of Element
  for Containers.Vector should be @i{O}(log @i{N}).]}]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[the worst-case time complexity of Append with
  Count=1 when @i{N} is less than the capacity of the vector should be
  @i{O}(log @i{N}); and]}
  @ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
  Text=[The worst-case time complexity of Append with Count = 1 when
  @i{N} is less than the capacity for Containers.Vector should be @i{O}(log @i{N}).]}]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[the worst-case time complexity of Prepend with
  Count=1 and Delete_First with Count=1 should be @i{O}(@i{N} log @i{N}).]}
  @ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
  Text=[The worst-case time complexity of Prepend with Count = 1 and
  Delete_First with Count=1 for Containers.Vectors should be @i{O}(@i{N} log @i{N}).]}]}

@end{Itemize}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We do not mean to overly constrain implementation
  strategies here. However, it is important for portability that the
  performance of large containers has roughly the same factors on different
  implementations. If a program is moved to an implementation that takes
  @i{O}(@i{N}) time to access elements, that program could be unusable when the
  vectors are large. We allow @i{O}(log @i{N}) access because the proportionality
  constant and caching effects are likely to be larger than the log factor, and
  we don't want to discourage innovative implementations.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The worst-case time complexity of a call on procedure
Sort of an instance of Containers.Vectors.Generic_Sorting should be @i{O}(@i{N}**2), and the
average time complexity should be better than @i{O}(@i{N}**2).]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The worst-case time complexity of a call on procedure Sort of an
instance of Containers.Vectors.Generic_Sorting should be @i{O}(@i{N}**2), and the
average time complexity should be better than @i{O}(@i{N}**2).]}]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[In other words, we're requiring the use of a
  better than @i{O}(@i{N}**2) sorting algorithm, such as Quicksort. No bubble sorts
  allowed!]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Containers.Vectors.Generic_Sorting.Sort and
Containers.Vectors.Generic_Sorting.Merge should minimize
copying of elements.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Containers.Vectors.Generic_Sorting.Sort and
Containers.Vectors.Generic_Sorting.Merge should minimize copying of elements.]}]}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We do not mean @lquotes@;absolutely minimize@rquotes
  here; we're not intending to require a single copy for each element.
  Rather, we want to suggest that the sorting algorithm chosen is one that
  does not copy items unnecessarily. Bubble sort would not meet this advice,
  for instance.]}
@end{Honest}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Move should not copy elements, and should minimize
copying of internal data structures.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Containers.Vectors.Move should not copy elements, and should minimize
copying of internal data structures.]}]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Usually that can be accomplished simply by
  moving the pointer(s) to the internal data structures from the Source vector
  to the Target vector.]}
@end{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[If an exception is propagated from a vector
operation, no storage should be lost, nor any elements removed from a vector
unless specified by the operation.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[If an exception is propagated from a vector
operation, no storage should be lost, nor any elements removed from a vector
unless specified by the operation.]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is important so that programs can recover
  from errors. But we don't want to require heroic efforts, so we just require
  documentation of cases where this can't be accomplished.]}
@end{Reason}

@end{ImplAdvice}

@begin{Notes}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[All elements of a vector occupy locations in the
internal array. If a sparse container is required, a Hashed_Map should be used
rather than a vector.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[If Index_Type'Base'First = Index_Type'First an
instance of Ada.Containers.Vectors will raise Constraint_Error. A value
below Index_Type'First is required so that an empty vector has a meaningful
value of Last_Index.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This property is the main reason why only integer
  types (as opposed to any discrete type) are allowed as the index type of a
  vector. An enumeration or modular type would require a subtype in order to
  meet this requirement.]}
@end{Discussion}

@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The package Containers.Vectors is new.]}
@end{Extend95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}Subprograms Assign and Copy
  are added to Containers.Vectors. If an instance of Containers.Vectors
  is referenced in a @nt{use_clause}, and an entity @i<E> with the same
  @nt{defining_identifier} as a new entity in Containers.Vectors is
  defined in a package that is also referenced in a @nt{use_clause}, the
  entity @i<E> may no longer be use-visible, resulting in errors. This should
  be rare and is easily fixed if it does occur.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Added iterator, reference, and indexing support to make vector containers more
  convenient to use.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[Generalized the definition
  of Reserve_Capacity and Move. Specified which elements are read/written
  by stream attributes.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0022-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a @BoundedName
  to cover tampering by generic actual subprograms.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0027-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a @BoundedName
  to cover access to finalized vector containers.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0044-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Redefined "<" actuals
  to require a strict weak ordering; the old definition allowed
  indeterminant comparisons that would not have worked in a container.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0084-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a pragma
  Remote_Types so that containers can be used in distributed programs.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Revised the definition
  of invalid cursors to cover missing (and new) cases.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Defined when a container
  prohibits tampering in order to more clearly define where the check is
  made and the exception raised.]}
@end{DiffWord2005}

@begin{Inconsistent2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1]}
  @ChgAdded{Version=[5],Text=[@Defn{inconsistencies with Ada 2012}Tampering
  with elements is now defined to be equivalent to tampering with
  cursors for regular containers. If a program requires tampering detection
  to work, it might fail in Ada 202x. Specifically, if a program requires
  Program_Error to be raised by a routine that (only) tampers with elements
  in Ada 2012 (such as Replace_Element) when called in a context that does not
  allow tampering with elements (such as Update_Element), the routine will
  work as defined instead of raising Program_Error in Ada 202x. Needless to say,
  this shouldn't happen outside of test programs. Note that such contexts still
  prohibit tampering with cursors, so routines like Insert and Delete will
  still raise Program_Error in this case.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[Trying to insert or concatenate more than
  Count_Type'Last elements will now raise Constraint_Error rather than
  Capacity_Error. This is extremely unlikely to happen, as Count_Type'Last
  is typically at least 2**31-1, so most such vectors will exceed memory
  before reaching this error.]}
@end{Inconsistent2012}

@begin{Incompatible2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1],ARef=[AI12-0112-1],ARef=[AI12-0339-1]}
  @ChgAdded{Version=[5],Text=[@Defn{incompatibilities with Ada 2012}A number of
  new subprograms, types, and even a nested package were added to
  Containers.Vectors to better support contracts and stable views. If an
  instance of Containers.Vectors
  is referenced in a @nt{use_clause}, and an entity @i<E> with the same
  @nt{defining_identifier} as a new entity in Containers.Hashed_Maps is
  defined in a package that is also referenced in a @nt{use_clause}, the
  entity @i<E> may no longer be use-visible, resulting in errors. This should
  be rare and is easily fixed if it does occur.]}
@end{Incompatible2012}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0196-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}@b{Correction:}
  To_Cursor and Replace_Element are now defined such that they can be used
  concurrently so long as they operate on different elements. This allows
  some container operations to be used in parallel without separate
  synchronization.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0212-1]}
  @ChgAdded{Version=[5],Text=[Vectors now support indexed container
  aggregates, so @nt{aggregate} syntax can be used to create Vectors.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0266-1]}
  @ChgAdded{Version=[5],Text=[The iterator for the
  entire container now can return a parallel iterator which can be used to
  process the container in parallel.]}
@end{Extend2012}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0110-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Clarified that tampering checks
  precede all other checks made by a subprogram (but come after those associated
  with the call).]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[Added contracts to this container. This includes
  describing some of the semantics with pre- and postconditions, rather than
  English text. Note that the preconditions can be Suppressed (see
  @RefSecNum{Suppressing Checks}).]}
@end{DiffWord2012}


@LabeledAddedSubclause{Version=[2],
Name=[The Generic Package Containers.Doubly_Linked_Lists]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The language-defined generic package
Containers.Doubly_Linked_Lists provides private types List and Cursor, and a
set of operations for each type. A list container is optimized for insertion
and deletion at any position. @Defn{list container}@Defn2{Term=[container],Sec=[list]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[node],Sec=[of a list]}A
doubly-linked list container object manages a linked list of internal
@i{nodes}, each of which contains an element and pointers to the
next (successor) and previous (predecessor) internal nodes. A cursor
designates a particular node within a list (and by extension the element
contained in that node). A cursor keeps designating the same node (and element)
as long as the node is part of the container, even if the node is moved in the
container.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The @i{length} of a list is the number of elements
it contains.@Defn2{Term=[length],Sec=(of a list container)}]}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
package Containers.Doubly_Linked_Lists has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0084-1],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[@key[with] Ada.Iterator_Interfaces;
],Old=[]}@key{generic}
   @key{type} Element_Type @key{is private};
   @key{with function} "=" (Left, Right : Element_Type)
      @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Doubly_Linked_Lists@Chg{Version=[5],New=[],Old=[ @key{is}]}@ChildUnit{Parent=[Ada.Containers],Child=[Doubly_@!Linked_@!Lists]}@Chg{Version=[5],New=[
   @key[with] Preelaborate, Remote_Types,
        Nonblocking, Global => @key[null] @key[is]],Old=[
   @key{pragma} Preelaborate(Doubly_Linked_Lists);@Chg{Version=[3],New=[
   @key{pragma} Remote_Types(Doubly_Linked_Lists);],Old=[]}]}]}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
   @ChgAdded{Version=[5],Text=[For discussion on the reasons and meaning of
   the specifications of the Global and Nonblocking aspects in this generic
   package, see the notes on the equivalent operations in the specification
   of the Containers.Vectors package (see 
   @RefSecNum{The Generic Package Containers.Vectors}).]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0111-1],ARef=[AI12-0112-1],ARef=[AI12-0212-1],ARef=[AI12-0339-1]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{List} @key{is tagged private}@Chg{Version=[3],New=[
      @key[with] Constant_Indexing => Constant_Reference,
           Variable_Indexing => Reference,
           Default_Iterator  => Iterate,
           Iterator_Element  => Element_Type],Old=[]}@Chg{Version=[5],New=[,
           Iterator_View     => Stable.List,
           Aggregate         => (Empty       => Empty,
                                 Add_Unnamed => Append),
           Stable_Properties => (Length,
                                 Tampering_With_Cursors_Prohibited,
                                 Tampering_With_Elements_Prohibited),
           Default_Initial_Condition =>
              Length (List) = 0 @key{and then}
              (@key{not} Tampering_With_Cursors_Prohibited (List)) @key{and then}
              (@key{not} Tampering_With_Elements_Prohibited (List))],Old=[]};
   @key{pragma} Preelaborable_Initialization(List);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Cursor} @key{is private};
   @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Empty_List} : @key{constant} List;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[in all], @key[use null])],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Has_Element} (Container : List; Position : Cursor)
      @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[package] @AdaPackDefn{List_Iterator_Interfaces} @key[is new]
       Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "=" (Left, Right : List) @key{return} Boolean;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} Same_Object (Left, Right : List) @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Tampering_With_Cursors_Prohibited}
      (Container : List) @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Tampering_With_Elements_Prohibited}
      (Container : List) @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0339-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Empty} @key{return} List
      @key[is] (Empty_List)
      @key[with] Post =>
             @key[not] Tampering_With_Elements_Prohibited (Empty'Result) @key[and then]
             @key[not] Tampering_With_Cursors_Prohibited (Empty'Result) @key[and then]
             Length (Empty'Result) = 0;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Length} (Container : List) @key{return} Count_Type@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Empty} (Container : List) @key{return} Boolean@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[null], @key[use null]),
           Post => Is_Empty'Result = (Length (Container) = 0)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Clear} (Container : @key{in out} List)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                       @key[or else raise] Program_Error,
           Post => Length (Container) = 0],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Position : Cursor) @key{return} Element_Type@Chg{Version=[5],New=[
      @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
           Nonblocking => Element_Type'Nonblocking,
           Global => (@key{in all}, @key{use} Element_Type)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Element} (Container : List;
                     Position  : Cursor) @key{return} Element_Type
      @key{with} Pre => (Position /= No_Element @key{or else}
                      @key{raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position) @key{or else raise} Program_Error),
           Nonblocking => Element_Type'Nonblocking,
           Global => (@key{null}, @key{use} Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} List;
                              Position  : @key{in}     Cursor;
                              New_item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Elements_Prohibited (Container)
                      @key{or else raise} Program_Error) @key{and then}
                   (Position /= No_Element 
                      @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position) @key{or else raise} Program_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Element : @key{in} Element_Type))@Chg{Version=[5],New=[
      @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Container : @key{in} List;
      Position  : @key{in} Cursor;
      Process   : @key{not null access procedure} (Element : @key{in} Element_Type))
      @key{with} Pre  => (Position /= No_Element 
                       @key{or else raise} Constraint_Error) @key{and then}
                    (Has_Element (Container, Position) 
                       @key{or else raise} Program_Error);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Update_Element}
     (Container : @key{in out} List;
      Position  : @key{in}     Cursor;
      Process   : @key{not null access procedure}
                      (Element : @key{in out} Element_Type))@Chg{Version=[5],New=[
      @key{with} Pre  => (Position /= No_Element @key{or else raise} Constraint_Error) @key{and then}
                    (Has_Element (Container, Position) @key{or else raise} Program_Error)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[type] Constant_Reference_Type
         (Element : @key[not null access constant] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
           Nonblocking, Global => @key[in out synchronized],
           Default_Initial_Condition => (@key[raise] Program_Error)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Reference_Type} (Element : @key[not null access] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
           Nonblocking, Global => @key[in out synchronized],
           Default_Initial_Condition => (@key[raise] Program_Error)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] List;
                                Position  : @key[in] Cursor)
      @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre  => (Position /= No_Element @key[or else]
                       @key[raise] Constraint_Error) @key[and then]
                    (Has_Element (Container, Position) @key[or else raise] Program_Error),
           Post   => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] List;
                       Position  : @key[in] Cursor)
      @key[return] Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre  => (Position /= No_Element @key[or else]
                       @key[raise] Constraint_Error) @key[and then]
                    (Has_Element (Container, Position) @key[or else raise] Program_Error),
           Post   => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} List; Source : @key{in} List)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Target)
                      @key[or else raise] Program_Error,
           Post => Length (Source) = Length (Target)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Copy} (Source : List)
      @key[return] List@Chg{Version=[5],New=[
      @key[with] Post => Length (Copy'Result) = Length (Source) @key[and then]
                   @key[not] Tampering_With_Elements_Prohibited (Copy'Result) @key[and then]
                   @key[not] Tampering_With_Cursors_Prohibited (Copy'Result)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Move} (Target : @key{in out} List;
                   Source : @key{in out} List)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Target)
                       @key{or else raise} Program_Error) @key{and then}
                   (@key{not} Tampering_With_Cursors_Prohibited (Source)
                       @key{or else raise} Program_Error),
           Post => (@key{if} Target = Source @key{then} True
                    @key{else}
                       Length (Target) = Length (Source'Old) @key{and then}
                       Length (Source) = 0)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} List;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before = No_Element @key{or else}
                    Has_Element (Container, Before)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - Count
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Count = Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} List;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before = No_Element @key{or else}
                    Has_Element (Container, Before)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - Count
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Count = Length (Container)
                   @key{and then} Has_Element (Container, Position)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} List;
                     Before    : @key{in}     Cursor;
                     Position  :    @key{out} Cursor;
                     Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before = No_Element @key{or else}
                    Has_Element (Container, Before)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - Count
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Count = Length (Container)
                   @key{and then} Has_Element (Container, Position)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Prepend} (Container : @key{in out} List;
                      New_Item  : @key{in}     Element_Type;
                      Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                    (Length (Container) <= Count_Type'Last - Count
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Count = Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Append} (Container : @key{in out} List;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - Count
                       @key{or else raise} Constraint_Error),
           Post => Length (Container)'Old + Count = Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} List;
                     Position  : @key{in out} Cursor;
                     Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Position /= No_Element 
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position)
                       @key{or else raise} Program_Error),
           Post => Length (Container)'Old - Count <= Length (Container)
                   @key{and then} Position = No_Element],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_First} (Container : @key{in out} List;
                           Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error,
           Post => Length (Container)'Old - Count <= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_Last} (Container : @key{in out} List;
                          Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error,
           Post => Length (Container)'Old - Count <= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reverse_Elements} (Container : @key{in out} List)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Swap} (Container : @key{in out} List;
                   I, J      : @key{in}     Cursor)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Elements_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (I /= No_Element @key{or else} Constraint_Error) @key{and then}
                   (J /= No_Element @key{or else} Constraint_Error) @key{and then}
                   (Has_Element (Container, I)
                       @key{or else raise} Program_Error) @key{and then}
                   (Has_Element (Container, J)
                       @key{or else raise} Program_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Swap_Links} (Container : @key{in out} List;
                         I, J      : @key{in}     Cursor)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Elements_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (I /= No_Element @key{or else} Constraint_Error) @key{and then}
                   (J /= No_Element @key{or else} Constraint_Error) @key{and then}
                   (Has_Element (Container, I)
                       @key{or else raise} Program_Error) @key{and then}
                   (Has_Element (Container, J)
                       @key{or else raise} Program_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Splice} (Target   : @key{in out} List;
                     Before   : @key{in}     Cursor;
                     Source   : @key{in out} List)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Target)
                       @key{or else raise} Program_Error) @key{and then}
                   (@key{not} Tampering_With_Cursors_Prohibited (Source)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before = No_Element @key{or else}
                    Has_Element (Target, Before)
                       @key{or else raise} Program_Error) @key{and then}
                   (Same_Object (Target, Source) @key{or else}
                    Length (Target) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => (@key{declare}
                      Result_Length : @key{constant} Count_Type :=
                         Length (Source)'Old + Length (Target)'Old;
                    @key{begin}
                       (@key{if} Target = Source @key{then} True
                        @key{else} Length (Source) = 0 @key{and then}
                           Length (Target) = Result_Length))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Splice} (Target   : @key{in out} List;
                     Before   : @key{in}     Cursor;
                     Source   : @key{in out} List;
                     Position : @key{in out} Cursor)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Target)
                       @key{or else raise} Program_Error) @key{and then}
                   (@key{not} Tampering_With_Cursors_Prohibited (Source)
                       @key{or else raise} Program_Error) @key{and then}
                   (Position /= No_Element
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Source, Position)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before = No_Element @key{or else}
                    Has_Element (Target, Before)
                       @key{or else raise} Program_Error) @key{and then}
                   (Same_Object (Target, Source) @key{or else}
                    Length (Target) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => (@key{declare}
                      Org_Target_Length : @key{constant} Count_Type :=
                         Length (Target)'Old;
                      Org_Source_Length : @key{constant} Count_Type :=
                         Length (Source)'Old;
                    @key{begin}
                       (@key{if} Target = Source @key{then}
                           Position = Position'Old
                        @key{else} Length (Source) = Org_Source_Length - 1 @key{and then}
                           Length (Target) = Org_Target_Length + 1 @key{and then}
                           Has_Element (Target, Position)))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Splice} (Container: @key{in out} List;
                     Before   : @key{in}     Cursor;
                     Position : @key{in}     Cursor)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Position /= No_Element
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position)
                       @key{or else raise} Program_Error) @key{and then}
                   (Before = No_Element @key{or else}
                    Has_Element (Container, Before)
                       @key{or else raise} Program_Error),
           Post =>  Length (Container) = Length (Container)'Old],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First} (Container : List) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Post => (@key{if not} Is_Empty (Container)
                    @key{then} Has_Element (Container, First'Result)
                    @key{else} First'Result = No_Element)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First_Element} (Container : List)
      @key{return} Element_Type@Chg{Version=[5],New=[
      @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last} (Container : List) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Post => (@key{if} not Is_Empty (Container)
                    @key{then} Has_Element (Container, Last'Result)
                    @key{else} Last'Result = No_Element)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last_Element} (Container : List)
      @key{return} Element_Type@Chg{Version=[5],New=[
      @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Next} (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Global => (@key{in all}, @key{use null}),
           Nonblocking => True,
           Post => (@key{if} Position = No_Element @key{then} Next'Result = No_Element
                    @key{else} True)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Next} (Container : List;
                  Position : Cursor) @key{return} Cursor
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Position = No_Element @key{then} Next'Result = No_Element
                    @key{elsif} Next'Result = No_Element @key{then}
                       Position = Last (Container)
                    @key{else} Has_Element (Container, Next'Result));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Next} (Position : @key{in out} Cursor)@Chg{Version=[5],New=[
      @key{with} Global => (@key{in all}, @key{use null}),
           Nonblocking => True],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Next} (Container : @key{in}     List;
                   Position  : @key{in out} Cursor)
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Position /= No_Element
                    @key{then} Has_Element (Container, Position));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Previous} (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Global => (@key{in all}, @key{use null}),
           Nonblocking => True,
           Post => (@key{if} Position = No_Element @key{then} Previous'Result = No_Element
                    @key{else} True)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Previous} (Container : List;
                      Position  : Cursor) @key{return} Cursor
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Position = No_Element @key{then} Previous'Result = No_Element
                    @key{elsif} Previous'Result = No_Element @key{then}
                       Position = First (Container)
                    @key{else} Has_Element (Container, Previous'Result));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Previous} (Position : @key{in out} Cursor)@Chg{Version=[5],New=[
      @key{with} Global => (@key{in all}, @key{use null}),
           Nonblocking => True],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Previous} (Container : @key{in}     List;
                       Position  : @key{in out} Cursor)
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Position /= No_Element
                    @key{then} Has_Element (Container, Position));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Find} (Container : List;
                  Item      : Element_Type;
                  Position  : Cursor := No_Element)
      @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Find'Result = No_Element @key{then} True
                    @key{else} Has_Element (Container, Find'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Reverse_Find} (Container : List;
                          Item      : Element_Type;
                          Position  : Cursor := No_Element)
      @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Reverse_Find'Result = No_Element @key{then} True
                    @key{else} Has_Element (Container, Reverse_Find'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Contains} (Container : List;
                      Item      : Element_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure}  @AdaSubDefn{Iterate}
     (Container : @key{in} List;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
      @key{with} Allows_Exit],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reverse_Iterate}
     (Container : @key{in} List;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
      @key{with} Allows_Exit],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Iterate} (Container : @key[in] List)
      @key[return] List_Iterator_Interfaces.@Chg{Version=[5],New=[Parallel_Reversible_Iterator],Old=[Reversible_Iterator]}'Class@Chg{Version=[5],New=[
      @key[with] Post   => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Iterate} (Container : @key[in] List; Start : @key[in] Cursor)
      @key[return] List_Iterator_Interfaces.Reversible_Iterator'Class@Chg{Version=[5],New=[
      @key[with] Pre    => (Start /= No_Element
                               @key[or else raise] Constraint_Error) @key[and then]
                        (Has_Element (Container, Start)
                            @key[or else raise] Program_Error),
           Post   => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{generic}
      @key{with function} "<" (Left, Right : Element_Type)
         @key{return} Boolean is <>;
   @key{package} @AdaPackDefn{Generic_Sorting}@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => @key{null}],Old=[]} @key{is}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Less_Element} (Left, Right : Element_Type) @key{return} Boolean
         @key{renames} "<";]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{function} @AdaSubDefn{Is_Sorted} (Container : List) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Sort} (Container : @key{in out} List)@Chg{Version=[5],New=[
         @key{with} Pre  => (@key{if} Tampering_With_Cursors_Prohibited (Container)
                       @key{then raise} Program_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Merge} (Target  : @key{in out} List;
                       Source  : @key{in out} List)@Chg{Version=[5],New=[
         @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Target)
                          @key{or else raise} Program_Error) @key{and then}
                      (@key{not} Tampering_With_Elements_Prohibited (Source)
                          @key{or else raise} Program_Error) @key{and then}
                      (Length (Target) <= Maximum_Length - Length (Source)
                          @key{or else raise} Constraint_Error),
              Post => (@key{declare}
                         Result_Length : @key{constant} Count_Type :=
                            Length (Source)'Old + Length (Target)'Old;
                       @key{begin}
                          (@key{if} Target = Source @key{then} True
                           @key{else} Length (Source) = 0 @key{and then}
                              Length (Target) = Result_Length))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{end} Generic_Sorting;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[   @key{package} @AdaPackDefn{Stable} @key{is}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1],ARef=[AI12-0339-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{List} (Base : @key{not null access} Doubly_Linked_Lists.List) @key{is}
         @key{tagged limited private}
         @key{with} Constant_Indexing => Constant_Reference,
              Variable_Indexing => Reference,
              Default_Iterator  => Iterate,
              Iterator_Element  => Element_Type,
              Aggregate         => (Empty          => Empty,
                                    Add_Unnamed    => Append),
              Stable_Properties => (Length),
              Global => @key{null},
              Default_Initial_Condition => Length (List) = 0;
      @key{pragma} Preelaborable_Initialization(List);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Cursor} @key{is private};
      @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @AdaObjDefn{Empty_List} : @key{constant} List;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean
         @key{with} Nonblocking, Global => (@key{in all}, @key{use null});]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{package} @AdaPackDefn{List_Iterator_Interfaces} @key[is new]
         Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} Doubly_Linked_Lists.List;
                        Source : @key{in} List)
         @key{with} Post => Length (Source) = Length (Target);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Copy} (Source : Doubly_Linked_Lists.List) @key{return} List
         @key{with} Post => Length (Copy'Result) = Length (Source);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Constant_Reference_Type}
            (Element : @key{not null access constant} Element_Type) @key{is private}
         @key{with} Implicit_Dereference => Element,
              Nonblocking, Global => (@key{null}, @key{use null}),
              Default_Initial_Condition => (@key{raise} Program_Error);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Reference_Type}
            (Element : @key{not null access} Element_Type) @key{is private}
         @key{with} Implicit_Dereference => Element,
              Nonblocking, Global => (@key{null}, @key{use null}),
              Default_Initial_Condition => (@key{raise} Program_Error);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      -- @examcom{Additional subprograms as described in the text}
      -- @examcom{are declared here.}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[   @key{private}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      ... -- @Examcom{not specified by the language}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[   @key{end} Stable;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{private}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   ... -- @RI[not specified by the language]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{end} Ada.Containers.Doubly_Linked_Lists;]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
"=" on Element_Type values is expected to define a reflexive and symmetric
relationship and return the same result value each time it is called with a
particular pair of values. If it behaves in some other manner, the functions
Find, Reverse_Find, and "=" on list values return an unspecified value. The
exact arguments and number of calls of this generic formal function by the
functions Find, Reverse_Find, and "=" on list values are
unspecified.@PDefn{unspecified}]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If the actual function for "=" is not symmetric
  and consistent, the result returned by the listed functions cannot be predicted.
  The implementation is not required to protect
  against "=" raising an exception, or returning random results, or any
  other @lquotes@;bad@rquotes behavior. And it can call "=" in whatever
  manner makes sense. But note that only the results of Find, Reverse_Find, and
  List "=" are unspecified; other subprograms are not allowed to break if "="
  is bad (they aren't expected to use "=").]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The type List is used to represent lists. The type
List needs finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Empty_List represents the empty List object. It has
a length of 0. If an object of type List is not otherwise initialized, it is
initialized to the same value as Empty_List.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[No_Element represents a cursor that designates no
element. If an object of type Cursor is not otherwise initialized, it is
initialized to the same value as No_Element.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The predefined "=" operator for type Cursor returns
True if both cursors are No_Element, or designate the same element in the same
container.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Execution of the default implementation of the
Input, Output, Read, or Write attribute of type Cursor raises Program_Error.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[A cursor will probably be implemented in terms
  of one or more access values, and the effects of streaming access values is
  unspecified. Rather than letting the user stream junk by accident, we mandate
  that streaming of cursors raise Program_Error by default. The attributes
  can always be specified if there is a need to support streaming.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[List'Write for a List object @i<L> writes
Length(@i<L>) elements of the list to the stream. It also may write
additional information about the list.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[List'Read reads the representation of a list
from the stream, and assigns to @i<Item> a list with the same length and
elements as was written by List'Write.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Streaming more elements than the container
  length is wrong. For implementation implications of this rule, see the Implementation Note in
  @RefSecNum{The Generic Package Containers.Vectors}.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Redundant[Some
operations @Chg{Version=[5],New=[@Defn2{Term=[tamper with cursors],Sec=[of a list]}@Defn2{Term=[tamper with elements],Sec=[of a list]}],Old=[of
this generic package have access-to-subprogram parameters. To
ensure such operations are well-defined, they guard against certain actions by
the designated subprogram. In particular, some operations]} check for
@lquotes@;tampering with cursors@rquotes of a container because they depend on
the set of elements of the container remaining constant, and others check for
@lquotes@;tampering with elements@rquotes of a container because they depend on
elements of the container not being replaced.]@Chg{Version=[5],New=[ When
tampering with cursors is @i<prohibited>@Defn2{Term=[prohibited],Sec=[tampering with a list]}
@Defn2{Term=[tampering],Sec=[prohibited for a list]}for a particular
list object @i<L>, Program_Error is propagated by the finalization
of @i<L>@Redundant[, as well as by a call that passes @i<L> to
certain of the operations of this package, as indicated by the precondition
of such an operation]. Similarly, when tampering with elements is @i<prohibited>
for @i<L>, Program_Error is propagated by a call that passes @i<L> to
certain of other operations of this package, as indicated by the precondition
of such an operation.],Old=[]}]}

@begin{NotIso}
@ChgAdded{Version=[5],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 62 through 69
are removed as preconditions now describe these rules.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgDeleted{Version=[5],Type=[Leading],Text=[@Chg{Version=[2],New=[@Defn2{Term=[tamper with cursors],Sec=[of a list]}
A subprogram is said to
@i{tamper with cursors} of a list object @i<L> if:],Old=[]}]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
inserts or deletes elements of @i<L>, that is,
it calls the Insert, Clear, Delete, or Delete_Last procedures with @i<L> as a
parameter; or]}]}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[Operations which
  are defined to be equivalent to
  a call on one of these operations also are included. Similarly, operations
  which call one of these as part of their definition are included.]}]}
@end{Honest}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
reorders the elements of @i<L>, that is, it
calls the Splice, Swap_Links, or Reverse_Elements procedures or the Sort or
Merge procedures of an instance of Generic_Sorting with @i<L> as a parameter;
or]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
finalizes @i<L>; or]}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[it
calls the Assign procedure with @i<L> as the Target parameter; or]}]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[We don't need to explicitly mention
  @nt{assignment_statement}, because that finalizes the target object
  as part of the operation, and finalization of an object is already defined
  as tampering with cursors.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
calls the Move procedure with @i<L> as a parameter.]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[Swap copies
  elements rather than reordering them, so it doesn't tamper with cursors.]}]}
@end{Reason}
@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgDeleted{Version=[5],Type=[Leading],Text=[@Chg{Version=[2],New=[@Defn2{Term=[tamper with elements],Sec=[of a list]}
A subprogram is said to @i{tamper with elements} of a list
object @i<L> if:],Old=[]}]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
tampers with cursors of @i<L>; or]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
replaces one or more elements of @i<L>, that is, it calls the Replace_Element or
Swap procedures with @i<L> as a parameter.]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[Complete
  replacement of an element can cause its memory to be deallocated while another
  operation is holding onto a reference to it. That can't be allowed. However, a
  simple modification of (part of) an element is not a problem, so
  Update_Element does not cause a problem.]}]}
@end{Reason}

@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0265-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0110-1]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[@Defn2{Term=[prohibited],Sec=[tampering with a list]}
@Defn2{Term=[tampering],Sec=[prohibited for a list]}
When tampering with cursors is @i<prohibited> for a particular list object
@i<L>, Program_Error is propagated by a call of any language-defined subprogram
that is defined to tamper with the cursors of @i<L>, leaving @i<L> unmodified.
Similarly, when tampering with elements is @i<prohibited> for a particular list
object @i<L>, Program_Error is propagated by a call of any language-defined
subprogram that is defined to tamper with the elements of @i<L> @Redundant[(or
tamper with the cursors of @i<L>)], leaving @i<L>
unmodified.@Chg{Version=[4],New=[ These checks are made before any other
defined behavior of the body of the language-defined subprogram.],Old=[]}]}]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[Tampering
  with elements includes tampering with
  cursors, so we mention it only from completeness in the second sentence.]}]}
@end{TheProof}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Has_Element (Position : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => (@key{in all}, @key{use null})],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if Position designates
an element, and returns False otherwise.]}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[3],Text=[This function might not detect cursors that
  designate deleted elements; such cursors are invalid (see below) and the
  result of calling Has_Element with an invalid cursor is unspecified (but
  not erroneous).]}
@end{Honest}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} @AdaSubDefn{Has_Element} (Container : List; Position : Cursor)
   @key{return} Boolean
   @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns True if Position designates
an element in Container, and returns False otherwise.]}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[If Position is No_Element, Has_Element returns False.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "=" (Left, Right : List) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Left and Right denote the same
list object, then the function returns True. If Left and Right have different
lengths, then the function returns False.
Otherwise, it compares each element in Left to
the corresponding element in Right using the generic formal equality operator.
If any such comparison returns False, the function returns False;
otherwise@Chg{Version=[3],New=[,],Old=[]} it
returns True. Any exception raised during
evaluation of element equality is propagated.]}
@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This wording describes the canonical semantics.
However, the order and number of calls on the formal equality function is
unspecified for all of the operations that use it in this package, so an
implementation can call it as many or as few times as it needs to get the
correct answer. Specifically, there is no requirement to call the formal
equality additional times once the answer has been determined.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[@key{function} Same_Object (Left, Right : List) @key{return} Boolean
   @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Type=[Trailing],Text=[Returns True if Left and Right
  denote the same list object, and returns False otherwise.]}

@begin{ImplNote}
@ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Text=[This can be implemented by comparing object
    addresses or identifiers (if available). The elements stored in the
    containers should not be compared.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Tampering_With_Cursors_Prohibited
   (Container : List) @key{return} Boolean
   @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns True if tampering with
cursors or tampering with elements is currently prohibited for Container, and
returns False otherwise.]}

@begin{Reason}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[Prohibiting tampering with elements also
  needs to prohibit tampering with cursors, as deleting an element is similar
  to replacing it.]}
@end{Reason}

@begin{ImplNote}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[Various contracts elsewhere in this specification
    require that this function is implemented with synchronized data. Moreover,
    it is possible for tampering to be prohibited by multiple operations
    (sequentiually or in parallel). Therefore, tampering needs to be
    implemented with an atomic or protected counter. The counter is initialized
    to zero, and is incremented when tampering is prohibited, and decremented
    when leaving an area that prohibited tampering. Function
    Tampering_With_Cursors_Prohibited returns True if the counter is nonzero.
    (Note that any case where the result is not well-defined for one task
    is incorrect use of shared variables and would be erroneous by the rules
    of @RefSecNum{Shared Variables}, so no special protection is needed to
    read the counter.)]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Tampering_With_Elements_Prohibited
   (Container : List) @key{return} Boolean
   @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Always returns False@Redundant[,
regardless of whether tampering with elements is prohibited].]}

@begin{Reason}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1]}
  @ChgAdded{Version=[5],Text=[A definite element cannot
  change size, so we allow operations that tamper with elements even when
  tampering with elements is prohibited. That's not true for the indefinite
  containers, which is why this kind of tampering exists.]}
@end{Reason}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Length (Container : List) @key{return} Count_Type@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the number of elements in
Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Is_Empty (Container : List) @key{return} Boolean@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => (@key[null], @key[use null]),
        Post => Is_Empty'Result = (Length (Container) = 0)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[Returns True
if Container is empty],Old=[Equivalent to Length (Container) = 0]}.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Clear (Container : @key{in out} List)@Chg{Version=[5],New=[
   @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                    @key[or else raise] Program_Error,
        Post => Length (Container) = 0],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Removes all the elements from
Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Element (Position : Cursor) @key{return} Element_Type@Chg{Version=[5],New=[
   @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
        Nonblocking => Element_Type'Nonblocking,
        Global => (@key{in all}, @key{use} Element_Type)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then Constraint_Error is propagated. Otherwise,
]}Element returns the element designated by Position.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Element (Container : List;
                  Position  : Cursor) @key{return} Element_Type
   @key{with} Pre => (Position /= No_Element @key{or else}
                   @key{raise} Constraint_Error) @key{and then}
                (Has_Element (Container, Position) @key{or else raise} Program_Error),
        Nonblocking => Element_Type'Nonblocking,
        Global => (@key{null}, @key{use} Element_Type);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Element returns the element
designated by Position in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace_Element (Container : @key{in out} List;
                           Position  : @key{in}     Cursor;
                           New_item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Elements_Prohibited (Container)
                   @key{or else raise} Program_Error) @key{and then}
                (Position /= No_Element 
                   @key{or else raise} Constraint_Error) @key{and then}
                (Has_Element (Container, Position) @key{or else raise} Program_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1],ARef=[AI12-0196-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element,
then Constraint_Error is propagated; if Position does not designate an element
in Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]} ]}Replace_Element
assigns the value New_Item to the element designated by
Position.@Chg{Version=[5],New=[ For the purposes of
determining whether the parameters overlap in a call to Replace_Element, the
Container parameter is not considered to overlap with any object
@Redundant[(including itself)].],Old=[]}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Query_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access procedure} (Element : @key{in} Element_Type))@Chg{Version=[5],New=[
   @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0021-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then
Constraint_Error is propagated. Otherwise, ]}Query_Element calls
Process.@key{all} with the element designated by Position as the argument.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of @Chg{Version=[3],New=[the list that contains the
element designated by Position is prohibited during the
execution of the call on Process.@key{all}],Old=[Container]}. Any exception
raised by Process.@key{all} is propagated.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],Text=[@key{procedure} Query_Element
  (Container : @key{in} List;
   Position  : @key{in} Cursor;
   Process   : @key{not null access procedure} (Element : @key{in} Element_Type))
   @key{with} Pre  => (Position /= No_Element 
                    @key{or else raise} Constraint_Error) @key{and then}
                 (Has_Element (Container, Position) 
                    @key{or else raise} Program_Error);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Query_Element calls Process.@key{all}
with the element designated by Position as the
argument. Tampering with the elements of Container
is prohibited during the execution of the call on
Process.@key{all}. Any exception raised by Process.@key{all} is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Update_Element
  (Container : @key{in out} List;
   Position  : @key{in}     Cursor;
   Process   : @key{not null access procedure}
                   (Element : @key{in out} Element_Type))@Chg{Version=[5],New=[
   @key{with} Pre  => (Position /= No_Element @key{or else raise} Constraint_Error) @key{and then}
                 (Has_Element (Container, Position) @key{or else raise} Program_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then
Constraint_Error is propagated; if Position does not designate an element in
Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]} ]}Update_Element calls
Process.@key{all} with the element designated by Position as the
argument. @Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of Container@Chg{Version=[3],New=[ is prohibited during the
execution of the call on Process.@key{all}],Old=[]}. Any exception raised by
Process.@key{all} is propagated.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Element_Type is unconstrained
and definite, then the actual Element parameter of Process.@key{all} shall be
unconstrained.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This means that the elements cannot be directly
  allocated from the heap; it must be possible to change the discriminants of
  the element in place.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@key[type] Constant_Reference_Type
      (Element : @key[not null access constant] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
        Nonblocking, Global => @key[in out synchronized],
        Default_Initial_Condition => (@key[raise] Program_Error)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[type] Reference_Type (Element : @key[not null access] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
        Nonblocking, Global => @key[in out synchronized],
        Default_Initial_Condition => (@key[raise] Program_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[The types Constant_Reference_Type
and Reference_Type need finalization.@PDefn2{Term=<needs finalization>,
Sec=<language-defined type>}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[DeletedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[The default
initialization of an object of type
Constant_Reference_Type or Reference_Type propagates Program_Error.]}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[It is expected that Reference_Type (and
  Constant_Reference_Type) will be a controlled type, for which finalization
  will have some action to terminate the tampering check for the associated
  container. If the object is created by default, however, there is no
  associated container. Since this is useless, and supporting this case would
  take extra work, we define it to raise an exception.]}
@end{Reason}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] List;
                             Position  : @key[in] Cursor)
   @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
   @key[with] Pre  => (Position /= No_Element @key[or else]
                    @key[raise] Constraint_Error) @key[and then]
                 (Has_Element (Container, Position) @key[or else raise] Program_Error),
        Post   => Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read access to an individual element of a list given a
cursor.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then Constraint_Error
is propagated; if Position does not designate an element in Container, then
Program_Error is propagated. Otherwise, ]}Constant_Reference returns an object
whose discriminant is an access value that designates the element designated by
Position. Tampering with the elements of Container is prohibited while the
object returned by Constant_Reference exists and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference (Container : @key[aliased in out] List;
                    Position  : @key[in] Cursor)
   @key[return] Reference_Type@Chg{Version=[5],New=[
   @key[with] Pre  => (Position /= No_Element @key[or else]
                    @key[raise] Constraint_Error) @key[and then]
                 (Has_Element (Container, Position) @key[or else raise] Program_Error),
        Post   => Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read and write access to an individual element of a list given
a cursor.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then Constraint_Error
is propagated; if Position does not designate an element in Container, then
Program_Error is propagated. Otherwise, ]}Reference returns an object whose
discriminant is an access value that designates the element designated by
Position. Tampering with the elements of Container is prohibited while the
object returned by Reference exists and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Assign (Target : @key{in out} List; Source : @key{in} List)@Chg{Version=[5],New=[
   @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Target)
                   @key[or else raise] Program_Error,
        Post => Length (Source) = Length (Target)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Target denotes the same object as
Source, the operation has no effect. Otherwise, the elements of Source are
copied to Target as for an @nt{assignment_statement} assigning Source to
Target.]}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[3],Text=[This routine exists for compatibility with the
  bounded list container. For an unbounded list, @exam{Assign(A, B)} and
  @exam{A := B} behave identically. For a bounded list, := will raise an
  exception if the container capacities are different, while Assign will
  not raise an exception if there is enough room in the target.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Copy (Source : List)
   @key[return] List@Chg{Version=[5],New=[
   @key[with] Post => Length (Copy'Result) = Length (Source) @key[and then]
                @key[not] Tampering_With_Elements_Prohibited (Copy'Result) @key[and then]
                @key[not] Tampering_With_Cursors_Prohibited (Copy'Result)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a list whose elements match
the elements of Source.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Move (Target : @key{in out} List;
                Source : @key{in out} List)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Target)
                    @key{or else raise} Program_Error) @key{and then}
                (@key{not} Tampering_With_Cursors_Prohibited (Source)
                    @key{or else raise} Program_Error),
        Post => (@key{if} Same_Object (Target, Source) @key{then} True
                 @key{else}
                    Length (Target) = Length (Source'Old) @key{and then}
                    Length (Source) = 0)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0001-1],ARef=[AI05-0248-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Target denotes the same object
as Source, then @Chg{Version=[3],New=[the operation],Old=[Move]}
has no effect. Otherwise, @Chg{Version=[3],New=[the operation is equivalent
to Assign (Target, Source) followed by Clear (Source)],Old=[Move first calls Clear
(Target). Then, the nodes in Source are moved to Target (in the original order).
The length of Target is set to the length of Source, and the length of Source is
set to 0]}.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} List;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Before = No_Element @key{or else}
                 Has_Element (Container, Before)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Count_Type'Last - Count
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Count = Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Before is not No_Element, and does not designate an element in Container,
then Program_Error is propagated. Otherwise,
]}Insert inserts Count copies of New_Item prior to the element designated by
Before. If Before equals No_Element, the new elements are inserted after the
last node (if any). Any exception raised during allocation of internal storage
is propagated, and Container is not modified.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The check on Before checks that the cursor does
  not belong to some other Container. This check implies that a reference to
  the container is included in the cursor value. This wording is not meant to
  require detection of dangling cursors; such cursors are defined to be
  invalid, which means that execution is erroneous, and any result is allowed
  (including not raising an exception).]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} List;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Before = No_Element @key{or else}
                 Has_Element (Container, Before)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Count_Type'Last - Count
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Count = Length (Container)
                @key{and then} Has_Element (Container, Position)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0257-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Before is not No_Element, and does not designate an element in Container,
then Program_Error is propagated. Otherwise,
]}Insert allocates Count copies of New_Item, and inserts them prior to the
element designated by Before. If Before equals No_Element, the new elements are
inserted after the last element (if any). Position designates the first
newly-inserted element@Chg{Version=[3],New=[, or if Count equals 0,
then Position is assigned the value of Before],Old=[]}. Any exception raised
during allocation of internal
storage is propagated, and Container is not modified.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} List;
                  Before    : @key{in}     Cursor;
                  Position  :    @key{out} Cursor;
                  Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Before = No_Element @key{or else}
                 Has_Element (Container, Before)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Count_Type'Last - Count
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Count = Length (Container)
                @key{and then} Has_Element (Container, Position)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0257-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Before is not No_Element, and does not designate an element in Container,
then Program_Error is propagated. Otherwise,
]}Insert inserts Count new elements prior to the element designated by Before.
If Before equals No_Element, the new elements are inserted after the last node
(if any). The new elements are initialized by default (see
@RefSecNum{Object Declarations}). @Chg{Version=[3],New=[Position designates the
first newly-inserted element, or if Count equals 0,
then Position is assigned the value of Before],Old=[]}. Any exception raised
during allocation of internal storage is propagated, and Container is not
modified.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Prepend (Container : @key{in out} List;
                   New_Item  : @key{in}     Element_Type;
                   Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Count_Type'Last - Count
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Count = Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container,
First (Container), New_Item, Count).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Append (Container : @key{in out} List;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Count_Type'Last - Count
                    @key{or else raise} Constraint_Error),
        Post => Length (Container)'Old + Count = Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container,
No_Element, New_Item, Count).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} List;
                  Position  : @key{in out} Cursor;
                  Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Position /= No_Element 
                    @key{or else raise} Constraint_Error) @key{and then}
                (Has_Element (Container, Position)
                    @key{or else raise} Program_Error),
        Post => Length (Container)'Old - Count <= Length (Container)
                @key{and then} Position = No_Element],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then
Constraint_Error is propagated. If Position does not designate an element in
Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]} ]}Delete removes (from
Container) Count elements starting at the element designated by Position (or
all of the elements starting at Position if there are fewer than Count elements
starting at Position). Finally, Position is set to No_Element.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_First (Container : @key{in out} List;
                        Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => Length (Container)'Old - Count <= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0021-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[3],New=[If Length
(Container) <= Count, then Delete_First is equivalent to Clear (Container).
Otherwise, it removes the first Count nodes from Container],Old=[Equivelent to
Delete (Container, First (Container), Count)]}.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_Last (Container : @key{in out} List;
                       Count     : @key{in}     Count_Type := 1)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => Length (Container)'Old - Count <= Length (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If
Length (Container) <= Count@Chg{Version=[3],New=[,],Old=[]} then
Delete_Last is equivalent to Clear (Container).
Otherwise@Chg{Version=[3],New=[,],Old=[]} it removes the last
Count nodes from Container.]}


@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reverse_Elements (Container : @key{in out} List)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Reorders the elements of Container
in reverse order.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Unlike the similar routine for a vector,
  elements should not be copied; rather, the nodes should be exchanged. Cursors
  are expected to reference the same elements afterwards.]}
@end{Discussion}


@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Swap (Container : @key{in out} List;
                I, J      : @key{in}     Cursor)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Elements_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (I /= No_Element @key{or else} Constraint_Error) @key{and then}
                (J /= No_Element @key{or else} Constraint_Error) @key{and then}
                (Has_Element (Container, I)
                    @key{or else raise} Program_Error) @key{and then}
                (Has_Element (Container, J)
                    @key{or else raise} Program_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
either I or J is No_Element,
then Constraint_Error is propagated. If either I or J do not designate an
element in Container, then Program_Error is propagated. Otherwise, ]}Swap
exchanges the values of the elements designated by I and J.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[After a call to Swap, I designates the element
  value previously designated by J, and J designates the element value
  previously designated by I. The cursors do not become ambiguous from
  this operation.]}
@end{Ramification}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The implementation is not required to actually
  copy the elements if it can do the swap some other way. But it is allowed
  to copy the elements if needed.]}
@end{Honest}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Swap_Links (Container : @key{in out} List;
                      I, J      : @key{in}     Cursor)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Elements_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (I /= No_Element @key{or else} Constraint_Error) @key{and then}
                (J /= No_Element @key{or else} Constraint_Error) @key{and then}
                (Has_Element (Container, I)
                    @key{or else raise} Program_Error) @key{and then}
                (Has_Element (Container, J)
                    @key{or else raise} Program_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
either I or J is No_Element,
then Constraint_Error is propagated. If either I or J do not designate an
element in Container, then Program_Error is propagated. Otherwise, ]}Swap_Links
exchanges the nodes designated by I and J.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Unlike Swap, this exchanges the nodes, not the
  elements. No copying is performed. I and J designate the same elements after
  this call as they did before it. This operation can provide better performance
  than Swap if the element size is large.]}
@end{Ramification}


@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Splice (Target   : @key{in out} List;
                  Before   : @key{in}     Cursor;
                  Source   : @key{in out} List)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Target)
                    @key{or else raise} Program_Error) @key{and then}
                (@key{not} Tampering_With_Cursors_Prohibited (Source)
                    @key{or else raise} Program_Error) @key{and then}
                (Before = No_Element @key{or else}
                 Has_Element (Target, Before)
                    @key{or else raise} Program_Error) @key{and then}
                (Same_Object (Target, Source) @key{or else}
                 Length (Target) <= Count_Type'Last - 1
                    @key{or else raise} Constraint_Error),
        Post => (@key{declare}
                   Result_Length : @key{constant} Count_Type :=
                      Length (Source)'Old + Length (Target)'Old;
                 @key{begin}
                    (@key{if} Same_Object (Target, Source) @key{then} True
                     @key{else} Length (Source) = 0 @key{and then}
                        Length (Target) = Result_Length))],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If @Chg{Version=[5],New=[],Old=[Before is not
No_Element, and does not designate an element in Target, then Program_Error
is propagated. Otherwise, if
]}Source denotes the same object as Target, the operation has no effect.
Otherwise, Splice reorders elements such that they are removed from Source and
moved to Target, immediately prior to Before. If Before equals No_Element, the
nodes of Source are spliced after the last node of
Target.@Chg{Version=[5],New=[],Old=[ The length of Target
is incremented by the number of nodes in Source, and the length of Source is
set to 0.]}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Splice (Target   : @key{in out} List;
                  Before   : @key{in}     Cursor;
                  Source   : @key{in out} List;
                  Position : @key{in out} Cursor)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Target)
                    @key{or else raise} Program_Error) @key{and then}
                (@key{not} Tampering_With_Cursors_Prohibited (Source)
                    @key{or else raise} Program_Error) @key{and then}
                (Position /= No_Element
                    @key{or else raise} Constraint_Error) @key{and then}
                (Has_Element (Source, Position)
                    @key{or else raise} Program_Error) @key{and then}
                (Before = No_Element @key{or else}
                 Has_Element (Target, Before)
                    @key{or else raise} Program_Error) @key{and then}
                (Same_Object (Target, Source) @key{or else}
                 Length (Target) <= Count_Type'Last - 1
                    @key{or else raise} Constraint_Error),
        Post => (@key{declare}
                   Org_Target_Length : @key{constant} Count_Type :=
                      Length (Target)'Old;
                   Org_Source_Length : @key{constant} Count_Type :=
                      Length (Source)'Old;
                 @key{begin}
                    (@key{if} Same_Object (Target, Source) @key{then}
                        Position = Position'Old
                     @key{else} Length (Source) = Org_Source_Length - 1 @key{and then}
                        Length (Target) = Org_Target_Length + 1 @key{and then}
                        Has_Element (Target, Position)))],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If @Chg{Version=[5],New=[],Old=[Position is
No_Element@Chg{Version=[3],New=[,],Old=[]} then
Constraint_Error is propagated. If Before does not equal No_Element, and does
not designate an element in Target, then Program_Error is propagated. If
Position does not equal No_Element, and does not designate a node in Source,
then Program_Error is propagated. If ]}Source denotes the same object as Target,
then there is no effect if Position equals Before, else the element
designated by Position is moved immediately prior to Before, or, if Before
equals No_Element, after the last element.
@Chg{Version=[5],New=[],Old=[In both cases, Position and the length of Target
are unchanged. ]}Otherwise@Chg{Version=[3],New=[,],Old=[]} the element
designated by Position is removed from Source and moved to Target, immediately
prior to Before, or, if Before equals No_Element, after the last element of
Target.@Chg{Version=[5],New=[],Old=[ The length of Target is incremented, the
length of Source is decremented, and]} Position is updated to represent
an element in Target.]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If Source is the same as Target, and
  Position = Before, or Next(Position) = Before, Splice has no effect, as
  the element does not have to move to meet the postcondition.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Splice (Container: @key{in out} List;
                  Before   : @key{in}     Cursor;
                  Position : @key{in}     Cursor)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Position /= No_Element
                    @key{or else raise} Constraint_Error) @key{and then}
                (Has_Element (Container, Position)
                    @key{or else raise} Program_Error) @key{and then}
                (Before = No_Element @key{or else}
                 Has_Element (Container, Before)
                    @key{or else raise} Program_Error),
        Post =>  Length (Container) = Length (Container)'Old],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If @Chg{Version=[5],New=[],Old=[Position is
No_Element@Chg{Version=[3],New=[,],Old=[]} then
Constraint_Error is propagated. If Before does not equal No_Element, and does
not designate an element in Container, then Program_Error is propagated. If
Position does not equal No_Element, and does not designate a node in Container,
then Program_Error is propagated. If ]}Position equals Before there is no effect.
Otherwise, the element designated by Position is moved immediately prior to
Before, or, if Before equals No_Element, after the last
element.@Chg{Version=[5],New=[],Old=[ The length of Container is unchanged.]}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First (Container : List) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Post => (@key{if not} Is_Empty (Container)
                 @key{then} Has_Element (Container, First'Result)
                 @key{else} First'Result = No_Element)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, First
returns No_Element. Otherwise, it returns a cursor that designates the first
node in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First_Element (Container : List)
   @key{return} Element_Type@Chg{Version=[5],New=[
   @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Container, First_Index (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last (Container : List) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Post => (@key{if} not Is_Empty (Container)
                 @key{then} Has_Element (Container, Last'Result)
                 @key{else} Last'Result = No_Element)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, Last returns
No_Element. Otherwise, it returns a cursor that designates the last node in
Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last_Element (Container : List)
   @key{return} Element_Type@Chg{Version=[5],New=[
   @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Last (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Next (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Global => (@key{in all}, @key{use null}),
        Nonblocking => True,
        Post => (@key{if} Position = No_Element @key{then} Next'Result = No_Element
                 @key{else} True)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element or
designates the last element of the container, then Next returns the value
No_Element. Otherwise, it returns a cursor that designates the successor of the
element designated by Position.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Next (Container : List;
               Position  : Cursor) @key{return} Cursor
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)
                    @key{or else raise} Program_Error,
        Post => (@key{if} Position = No_Element @key{then} Next'Result = No_Element
                 @key{elsif} Next'Result = No_Element @key{then}
                    Position = Last (Container)
                 @key{else} Has_Element (Container, Next'Result));]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns a cursor designating the
successor of the element designated by Position in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Previous (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Global => (@key{in all}, @key{use null}),
        Nonblocking => True,
        Post => (@key{if} Position = No_Element @key{then} Previous'Result = No_Element
                 @key{else} True)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element or
designates the first element of the container, then Previous returns the value
No_Element. Otherwise, it returns a cursor that designates the predecessor of
the element designated by Position.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Previous (Container : List;
                   Position : Cursor) @key{return} Cursor
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)
                    @key{or else raise} Program_Error,
        Post => (@key{if} Position = No_Element @key{then} Previous'Result = No_Element
                 @key{elsif} Previous'Result = No_Element @key{then}
                    Position = First (Container)
                 @key{else} Has_Element (Container, Previous'Result));]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns a cursor designating the
predecessor of the element designated by Position in Container, if any.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Next (Position : @key{in out} Cursor)@Chg{Version=[5],New=[
   @key{with} Global => (@key{in all}, @key{use null}),
        Nonblocking => True],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Next (Position).]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{procedure} Next (Container : @key{in}     List;
                Position  : @key{in out} Cursor)
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)
                    @key{or else raise} Program_Error,
        Post => (@key{if} Position /= No_Element
                 @key{then} Has_Element (Container, Position));]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Equivalent to Position := Next
(Container, Position).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Previous (Position : @key{in out} Cursor)@Chg{Version=[5],New=[
   @key{with} Global => (@key{in all}, @key{use null}),
        Nonblocking => True],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Previous (Position).]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{procedure} Previous (Container : @key{in}     List;
                    Position  : @key{in out} Cursor)
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)
                    @key{or else raise} Program_Error,
        Post => (@key{if} Position /= No_Element
                 @key{then} Has_Element (Container, Position));]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Equivalent to Position :=
Previous (Container, Position).]}



@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Find (Container : List;
               Item      : Element_Type;
               Position  : Cursor := No_Element)
   @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)
                    @key{or else raise} Program_Error,
        Post => (@key{if} Find'Result = No_Element @key{then} True
                 @key{else} Has_Element (Container, Find'Result))],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
]}Find searches the elements of Container for an element equal to Item (using
the generic formal equality operator). The search starts at the
element designated by Position, or at the first element if Position equals
No_Element. It proceeds towards Last (Container). If no equal element is found,
then Find returns No_Element. Otherwise, it returns a cursor designating the
first equal element encountered.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Reverse_Find (Container : List;
                       Item      : Element_Type;
                       Position  : Cursor := No_Element)
   @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)
                    @key{or else raise} Program_Error,
        Post => (@key{if} Reverse_Find'Result = No_Element @key{then} True
                 @key{else} Has_Element (Container, Reverse_Find'Result))],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
]}Find searches the elements of Container for an element equal to Item (using
the generic formal equality operator). The search starts at the
element designated by Position, or at the last element if Position equals
No_Element. It proceeds towards First (Container). If no equal element is
found, then Reverse_Find returns No_Element. Otherwise, it returns a cursor
designating the first equal element encountered.]}


@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Contains (Container : List;
                   Item      : Element_Type) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Find (Container, Item) /= No_Element.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@Chg{Version=[3],New=[],Old=[@key{function} Has_Element (Position : Cursor) @key{return} Boolean;]}]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[3],New=[],Old=[Returns True if Position designates
an element, and returns False otherwise.]}]}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[This function may not detect cursors that
  designate deleted elements; such cursors are invalid (see below) and the
  result of calling Has_Element with an invalid cursor is unspecified (but
  not erroneous).]}]}
@end{Honest}
@begin{NotIso}
@ChgAdded{Version=[3],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 139 and 140
were moved above.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Iterate
  (Container : @key{in} List;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
   @key{with} Allows_Exit],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Iterate calls Process.@key{all}
with a cursor that designates each node in Container, starting with the first
node and moving the cursor as per the Next function.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the cursors of Container@Chg{Version=[3],New=[ is prohibited during the
execution of a call on Process.@key{all}],Old=[]}. Any exception raised by
Process.@key{all} is propagated.]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The purpose of the tamper with cursors check is
  to prevent erroneous execution from the Position parameter of Process.@key{all}
  becoming invalid. This check takes place when the operations that tamper with
  the cursors of the container are called. The check cannot be made later (say
  in the body of Iterate), because that could cause the Position cursor to be
  invalid and potentially cause execution to become erroneous -- defeating the
  purpose of the check.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[See Iterate for vectors
  (@RefSecNum{The Generic Package Containers.Vectors}) for a suggested
  implementation of the check.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reverse_Iterate
  (Container : @key{in} List;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
   @key{with} Allows_Exit],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Iterates over the nodes in
Container as per @Chg{Version=[3],New=[procedure ],Old=[]}Iterate,
except that elements are traversed in reverse order,
starting with the last node and moving the cursor as per the Previous
function.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] List)
   @key[return] List_Iterator_Interfaces.@Chg{Version=[5],New=[Parallel_Reversible_Iterator],Old=[Reversible_Iterator]}'Class@Chg{Version=[5],New=[
   @key[with] Post   => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1],ARef=[AI05-0269-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate returns
@Chg{Version=[5],New=[an],Old=[a reversible]}
iterator object (see @RefSecNum{User-Defined Iterator Types}) that
will generate a value for a loop parameter (see
@RefSecNum{Generalized Loop Iteration}) designating
each node in Container, starting with the first node and moving the cursor as
per the Next function when used as a forward iterator, and starting with the
last node and moving the cursor as per the Previous function when used as a
reverse iterator@Chg{Version=[5],New=[, and processing all nodes concurrently
when used as a parallel iterator],Old=[]}. Tampering with the cursors of
Container is prohibited while the iterator object exists (in particular, in the
@nt{sequence_of_statements} of the @nt{loop_statement} whose
@nt{iterator_specification} denotes this object). The iterator object needs
finalization.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] List; Start : @key[in] Cursor)
   @key[return] List_Iterator_Interfaces.Reversible_Iterator'Class@Chg{Version=[5],New=[
   @key[with] Pre    => (Start /= No_Element
                            @key[or else raise] Constraint_Error) @key[and then]
                     (Has_Element (Container, Start)
                         @key[or else raise] Program_Error),
        Post   => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0262-1],ARef=[AI05-0265-1],ARef=[AI05-0269-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Start is not No_Element and does
not designate an item in Container, then Program_Error is propagated. If Start
is No_Element, then Constraint_Error is propagated. Otherwise, ]}Iterate
returns a reversible iterator object
(see @RefSecNum{User-Defined Iterator Types}) that
will generate a value for a loop parameter (see
@RefSecNum{Generalized Loop Iteration}) designating
each node in Container, starting with the node designated
by Start and moving the cursor as per the Next function when used as a forward
iterator, or moving the cursor as per the Previous function when used as a
reverse iterator. Tampering with the cursors of
Container is prohibited while the iterator object exists (in particular, in the
@nt{sequence_of_statements} of the @nt{loop_statement} whose
@nt{iterator_specification} denotes this object). The iterator object needs
finalization.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[Exits are allowed from the loops
  created using the iterator objects. In particular, to stop the iteration at a
  particular cursor, just add]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[exit when] Cur = Stop;]}
@end{Example}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[in the body of the loop (assuming
  that @exam{Cur} is the loop parameter and @exam{Stop} is the cursor that you
  want to stop at).]}
@end{Discussion}

@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0044-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
"<" of Generic_Sorting is expected to return the same value each time it is
called with a particular pair of element values. It should define a strict
@Chg{Version=[3],New=[weak ],Old=[]}ordering relationship@Chg{Version=[3],
New=[ (see @RefSecNum{Containers})],Old=[, that is, be irreflexive, asymmetric,
and transitive]}; it
should not modify Container. If the actual for "<" behaves in some other
manner, the behavior of the subprograms of Generic_Sorting are unspecified.
@Chg{Version=[3],New=[The number of],Old=[How many]} times the subprograms of
Generic_Sorting call "<" is unspecified.@PDefn{unspecified}]}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Is_Sorted (Container : List) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns True if the elements are
sorted smallest first as determined by the generic formal "<" operator;
otherwise, Is_Sorted returns False. Any exception raised during evaluation of
"<" is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Sort (Container : @key{in out} List)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{if} Tampering_With_Cursors_Prohibited (Container)
                 @key{then raise} Program_Error)],Old=[]};]}
@end{Example}


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Reorders the nodes of Container
such that the elements are sorted smallest first as determined by the generic
formal "<" operator provided. The sort is stable. Any exception raised during
evaluation of "<" is propagated.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Unlike array sorts, we do require stable sorts
  here. That's because algorithms in the merge sort family (as described by
  Knuth) can be both fast and stable. Such sorts use the extra memory as
  offered by the links to provide better performance.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Note that list sorts never copy elements; it is
  the nodes, not the elements, that are reordered.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Merge (Target  : @key{in out} List;
                    Source  : @key{in out} List)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Target)
                    @key{or else raise} Program_Error) @key{and then}
                (@key{not} Tampering_With_Elements_Prohibited (Source)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Target) <= Maximum_Length - Length (Source)
                    @key{or else raise} Constraint_Error),
        Post => (@key{declare}
                   Result_Length : @key{constant} Count_Type :=
                      Length (Source)'Old + Length (Target)'Old;
                 @key{begin}
                    (@key{if} Same_Object (Target, Source) @key{then} True
                     @key{else} Length (Source) = 0 @key{and then}
                        Length (Target) = Result_Length))],Old=[]};]}
@end{Example}




@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0021-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[3],New=[If Source is
empty, then Merge does nothing. If Source and Target are the same nonempty
container object, then Program_Error is propagated. Otherwise, ],Old=[]}Merge
removes elements from Source
and inserts them into Target; afterwards,
Target contains the union of the elements that were initially
in Source and Target; Source is left empty.
If Target and Source are initially sorted smallest first, then Target is
ordered smallest first as determined by the generic formal "<" operator;
otherwise, the order of elements in Target is unspecified.
Any exception raised during evaluation of "<" is propagated.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[It is a bounded error if either of the lists is
  unsorted, see below. The bounded error can be recovered by sorting Target
  after the merge call, or the lists can be pretested with Is_Sorted.]}
@end{Ramification}

@end{DescribeCode}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[The nested package Doubly_Linked_Lists.Stable
provides a type Stable.List that represents
a @i<stable> list,@Defn2{Term=(stable),Sec=(list)} which is one that
cannot grow and shrink. Such a list can be created by calling the
Copy function, or by establishing a
@i<stabilized view> of a regular List.@Defn2{Term=[stabilized view],Sec=[list]}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Type=[Leading],Text=[The subprograms of package
Containers.Doubly_Linked_Lists that have a parameter or result of type List
are included in the nested package Stable with the same specification, except
that the following are omitted:]}

@begin{Indent}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],Text=[Tampering_With_Cursors_Prohibited,
Tampering_With_Elements_Prohibited, Assign, Move,
Insert, Clear, Delete, Delete_First, Delete_Last, Splice, Swap_Links,
and Reverse_Elements]}
@end{Indent}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[Added]}
  @ChgAdded{Version=[5],Text=[The names List and Cursor mean the types
    declared in the nested package in these subprogram specifications.]}
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[5],Kind=[Added]}
  @ChgAdded{Version=[5],Text=[The omitted routines are those that tamper with
    cursors or elements (or test that state). The model is that it is
    impossible to tamper with cursors or elements of a stable view since no
    such operations are included. Thus tampering checks are not needed for
    a stable view, and we omit the operations associated with those checks.]}

  @ChgRef{Version=[5],Kind=[Added]}
  @ChgAdded{Version=[5],Text=[The Generic_Sorting generic is omitted entirely,
    as only function Is_Sorting does not tamper with cursors. It isn't useful
    enough by itself to include.]}
@end{Reason}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[The operations of this package are equivalent
to those for regular lists, except that the calls to
Tampering_With_Cursors_Prohibited and
Tampering_With_Elements_Prohibited that occur in preconditions are replaced
by False, and any that occur in postconditions are replaced by True.]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[If a stable list is declared with the Base
discriminant designating a pre-existing regular list, the stable list
represents a stabilized view of the underlying regular list, and any operation
on the stable list is reflected on the underlying regular list. While a
stabilized view exists, any operation that tampers with elements performed on
the underlying list is prohibited. The finalization of a stable list that
provides such a view removes this restriction on the underlying regular list
@Redundant[(though some other restriction might exist due to other concurrent
iterations or stabilized views)].]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[If a stable list is declared without specifying
Base, the object must be initialized. The initializing expression of the stable
list, @Redundant[typically a call on Copy], determines the Length
of the list. The Length of a stable list never changes after
initialization.]}

@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
Calling Merge in an instance of Generic_Sorting
with either Source or Target not ordered smallest first using the provided
generic formal "<" operator is a bounded error. Either Program_Error is raised
after Target is updated as described for Merge, or the operation works as
defined.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0022-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error for the actual function
associated with a generic formal subprogram, when called as part of an
operation of this package, to tamper with elements of any List parameter of
the operation. Either Program_Error is raised, or the operation works as
defined on the value of the List either prior to, or subsequent to, some or
all of the modifications to the List.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0027-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to call any subprogram
declared in the visible part of Containers.Doubly_Linked_Lists
when the associated container has been finalized. If the operation takes
Container as an @key[in out] parameter, then it raises Constraint_Error or
Program_Error. Otherwise, the operation either proceeds as it would
for an empty container, or it raises Constraint_Error@Defn2{Term=[Constraint_Error],Sec=(raised by detection of a bounded error)}
or Program_Error.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[A Cursor value is
@i<invalid> if any of the following have occurred since it
was created:@Defn2{Term=[invalid cursor],Sec=[of a list container]}
@PDefn2{Term=[cursor],Sec=[invalid]}]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The list that contains the element it designates
has been finalized;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0160-1]}
@ChgAdded{Version=[3],Text=[The list that contains the element it designates
has been used as the Target of a call to Assign, or as the target of an
@nt{assignment_statement};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@Redundant[The list that contains the element it
designates has been used as the Source or Target of a call to Move;] or]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[Move has been reworded in terms of Assign and
  Clear, which are covered by other bullets, so this text is redundant.]}
@end{TheProof}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0160-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[The element it designates has been
@Chg{Version=[3],New=[removed from the list that previously contained the
element],Old=[deleted]}.]}
@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
  @ChgAdded{Version=[3],Text=[The cursor modified by the four parameter Splice
  is not invalid, even though the element it designates has been removed from
  the source list, because that cursor has been modified to designate that
  element in the target list @en the cursor no longer designates an element
  in the source list.]}
@end{Honest}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
  @ChgAdded{Version=[3],Text=[This can happen directly via calls to Delete,
  Delete_Last, Clear, Splice with a Source parameter, and Merge; and indirectly
  via calls to Delete_First, Assign, and Move.]}
@end{Ramification}

@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The result of "=" or Has_Element is unspecified
if it is called with an invalid
cursor parameter. Execution is erroneous if any other subprogram declared in
Containers.Doubly_Linked_Lists is called with an invalid cursor parameter.
@PDefn{unspecified}@PDefn2{Term=(erroneous execution),Sec=(cause)}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The list above is intended to be exhaustive. In
  other cases, a cursor value continues to designate its original element. For
  instance, cursor values survive the insertion and deletion of other nodes.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[While it is possible to check for these cases, in
  many cases the overhead necessary to make the check is substantial in time or
  space. Implementations are encouraged to check for as many of these cases as
  possible and raise Program_Error if detected.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[Execution is erroneous if the list associated with
the result of a call to Reference or Constant_Reference is finalized before the
result object returned by the call to Reference or Constant_Reference is
finalized.]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Each object of Reference_Type and
  Constant_Reference_Type probably contains some reference to the originating
  container. If that container is prematurely finalized (which is only possible
  via Unchecked_Deallocation, as accessibility checks prevent passing a
  container to Reference that will not live as long as the result), the
  finalization of the object of Reference_Type will try to access a nonexistent
  object. This is a normal case of a dangling pointer created by
  Unchecked_Deallocation; we have to explicitly mention it here as the pointer
  in question is not visible in the specification of the type. (This is the same
  reason we have to say this for invalid cursors.)]}
@end{Reason}
@end{Erron}

@begin{ImplReq}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[No storage associated with a doubly-linked List
object shall be lost upon assignment or scope exit.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[The execution of an @nt{assignment_statement} for
a list shall have the effect of copying the elements from the source list
object to the target list object@Chg{Version=[3],New=[ and changing the length
of the target object to that of the source object],Old=[]}.]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0298-1]}
  @ChgAdded{Version=[2],Text=[An assignment of a List is a @lquotes@;deep@rquotes
  copy; that is the elements are copied as well as the data structures.
  We say @lquotes@;effect of@rquotes in order to allow the implementation to
  avoid copying elements immediately if it wishes. For instance, an
  implementation that avoided copying until one of the containers is modified
  would be allowed.@Chg{Version=[3],New=[ (Note that this implementation would
  require care, see @RefSecNum{The Generic Package Containers.Vectors} for more.)],Old=[]}]}
@end{ImplNote}
@end{ImplReq}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Containers.Doubly_Linked_Lists should be
implemented similarly to a linked list. In particular, if @i<N> is the length
of a list, then the worst-case time complexity of Element, Insert with Count=1,
and Delete with Count=1 should be @i{O}(log @i<N>).]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The worst-case time complexity of Element, Insert with Count=1, and
Delete with Count=1 for Containers.Doubly_Linked_Lists should be
@i{O}(log @i<N>).]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We do not mean to overly constrain implementation
  strategies here. However, it is important for portability that the
  performance of large containers has roughly the same factors on different
  implementations. If a program is moved to an implementation that takes
  @i{O}(@i<N>) time to access elements, that program could be unusable when the
  lists are large. We allow @i{O}(log @i<N>) access because the proportionality
  constant and caching effects are likely to be larger than the log factor, and
  we don't want to discourage innovative implementations.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The worst-case time complexity of a call on
procedure Sort of an
instance of Containers.Doubly_Linked_Lists.Generic_Sorting should be
@i{O}(@i<N>**2), and the average time complexity should be better than @i{O}(@i<N>**2).]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[A call on procedure Sort of an
instance of Containers.Doubly_Linked_Lists.Generic_Sorting
should have an average time complexity better than @i{O}(@i{N}**2) and
worst case no worse than @i{O}(@i{N}**2).]}]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[In other words, we're requiring the use of a
  better than @i{O}(@i{N}**2) sorting algorithm, such as Quicksort. No bubble sorts
  allowed!]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Move should not copy elements, and should minimize
copying of internal data structures.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Containers.Doubly_Linked_Lists.Move should not copy elements, and should
minimize copying of internal data structures.]}]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Usually that can be accomplished simply by
  moving the pointer(s) to the internal data structures from the Source
  container to the Target container.]}
@end{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[If an exception is propagated from a list
operation, no storage should be lost, nor any elements removed from a list
unless specified by the operation.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[If an exception is propagated from a list
operation, no storage should be lost, nor any elements removed from a list
unless specified by the operation.]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is important so that programs can recover
  from errors. But we don't want to require heroic efforts, so we just require
  documentation of cases where this can't be accomplished.]}
@end{Reason}

@end{ImplAdvice}

@begin{Notes}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Sorting a list never copies elements, and is a
stable sort (equal elements remain in the original order). This is different
than sorting an array or vector, which may need to copy elements, and is
probably not a stable sort.]}
@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The generic package Containers.Doubly_Linked_Lists is new.]}
@end{Extend95}

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0248-1],ARef=[AI05-0257-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}@b<Correction:>
  The Insert versions that return a Position parameter are now defined to
  return Position = Before if Count = 0. This was unspecified for Ada 2005;
  so this will only be inconsistent if an implementation did something else and
  a program depended on that something else @em this should be very rare.]}
@end{Inconsistent2005}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}Subprograms Assign and Copy
  are added to Containers.Doubly_Linked_Lists. If an instance of Containers.Doubly_Linked_Lists
  is referenced in a @nt{use_clause}, and an entity @i<E> with the same
  @nt{defining_identifier} as a new entity in Containers.Doubly_Linked_Lists is
  defined in a package that is also referenced in a @nt{use_clause}, the
  entity @i<E> may no longer be use-visible, resulting in errors. This should
  be rare and is easily fixed if it does occur.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Added iterator, reference, and indexing support to make list containers more
  convenient to use.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[Generalized the definition
  of Move. Specified which elements are read/written by stream attributes.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0022-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a @BoundedName
  to cover tampering by generic actual subprograms.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0027-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a @BoundedName
  to cover access to finalized list containers.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0044-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Redefined "<" actuals
  to require a strict weak ordering; the old definition allowed
  indeterminant comparisons that would not have worked in a container.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0084-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a pragma
  Remote_Types so that containers can be used in distributed programs.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Revised the definition
  of invalid cursors to cover missing (and new) cases.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0257-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added missing wording to describe
  the Position after Inserting 0 elements.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Defined when a container
  prohibits tampering in order to more clearly define where the check is
  made and the exception raised.]}
@end{DiffWord2005}

@begin{Inconsistent2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1]}
  @ChgAdded{Version=[5],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  Tampering with elements is now defined to be equivalent to tampering with
  cursors for regular containers. If a program requires tampering detection
  to work, it might fail in Ada 202x. Needless to say, this shouldn't happen
  outside of test programs. See @Inconsistent2012Title in
  @RefSecNum{The Generic Package Containers.Vectors} for more details.]}
@end{Inconsistent2012}

@begin{Incompatible2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1],ARef=[AI12-0112-1],ARef=[AI12-0339-1]}
  @ChgAdded{Version=[5],Text=[@Defn{incompatibilities with Ada 2012}A number of
  new subprograms, types, and even a nested package were added to
  Containers.Doubly_Linked_Lists to better support contracts and stable views. If an
  instance of Containers.Doubly_Linked_Lists
  is referenced in a @nt{use_clause}, and an entity @i<E> with the same
  @nt{defining_identifier} as a new entity in Containers.Hashed_Maps is
  defined in a package that is also referenced in a @nt{use_clause}, the
  entity @i<E> may no longer be use-visible, resulting in errors. This should
  be rare and is easily fixed if it does occur.]}
@end{Incompatible2012}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0196-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}
  Replace_Element is now defined such that it can be used
  concurrently so long as it operates on different elements. This allows
  some container operations to be used in parallel without separate
  synchronization.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0212-1]}
  @ChgAdded{Version=[5],Text=[Lists now support positional container
  aggregates, so @nt{aggregate} syntax can be used to create Lists.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0266-1]}
  @ChgAdded{Version=[5],Text=[The iterator for the
  entire container now can return a parallel iterator which can be used to
  process the container in parallel.]}
@end{Extend2012}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0110-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Clarified that tampering checks
  precede all other checks made by a subprogram (but come after those associated
  with the call).]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[Added contracts to this container. This includes
  describing some of the semantics with pre- and postconditions, rather than
  English text. Note that the preconditions can be Suppressed (see
  @RefSecNum{Suppressing Checks}).]}
@end{DiffWord2012}

