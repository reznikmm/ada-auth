@comment{ $Source: e:\\cvsroot/ARM/Source/pre_containers.mss,v $ }
@comment{ $Revision: 1.4 $ $Date: 2004/12/12 05:36:22 $ $Author: Randy $ }
@Part(precontainers, Root="ada.mss")

@Comment{$Date: 2004/12/12 05:36:22 $}

@LabeledAddedClause{Version=[2],Name=[Containers]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[This clause presents the specifications of the package
Containers and several child packages, which provide facilities for storing
collections of elements.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[A variety of sequence and associative containers are
provided. Each container includes a @i{cursor} type. A cursor is a reference
to an element within a container. Many operations on cursors are common to
all of the containers.@PDefn2{Term=[cursor],Sec=[for a container]}
@Defn2{Term=[container],Sec=[cursor]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Within this clause we provide Implementation Advice
for the desired average or worst case time complexity of certain operations
on a container. This advice is expressed using a big-O notation.
A complexity of O(f(N)), presuming f is some function of a
length parameter N and t(N) is the time the operation takes
(on average or worst case, as specified) for the length N,
means that there exists a finite A such that for any N, t(N)/f(N) < A.
@Defn{big-O notation}@Defn{O(f(N))}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Of course, an implementation can do better than a
  specified O(f(N)): for example, O(1) meets the requirements for O(log N).]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[If the advice suggests that the complexity should be
less than O(f(N)), then for any arbitrarily small positive real D, there
should exist a positive integer M such that for all N > M,
t(N)/f(N) < D.]}
@end{Intro}

@begin{Metarules}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[This clause provides a number of useful containers
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
@ChgAdded{Version=[2],Type=[Leading],Text=[The following major non-limited
containers are provided:]}

@begin{Itemize}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[(Expandable) Vectors of any non-limited type;]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Doubly-linked Lists of any non-limited type;]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Hashed Maps keyed by any non-limited type
  containing any non-limited type;]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Ordered Maps keyed by any non-limited type
  containing any non-limited type;]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Hashed Sets of any non-limited type; and]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Ordered Sets of any non-limited type.]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Separate versions for definite and indefinite
element types are provided, as those for definite types can be implemented more
efficiently.]}

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
  @ChgAdded{Version=[2],Text=[Library packages must be
  reentrant @en multiple tasks can use the packages as long as they operate on
  separate containers. Thus, it is only necessary for a user to protect a
  container if a single container needs to be used by multiple tasks.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Language-defined types must stream "properly".
  That means that the stream attributes can be used to implement persistence
  of containers when necessary, and containers can be passed between
  partitions of a program.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Equality of language-defined types
  must compose @lquotes@;properly@rquotes@;. This means that the version of
  "=" directly used by users is the same one that will be used in generics and in
  predefined equality operators of types with components of the containers and/or
  cursors. This prevents the abstraction from breaking unexpectedly.]}

@end{Itemize}


@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[If a container's element type is controlled, the
point at which the element is finalized will depend on the implementation of
the container. We do not specify precisely where this will happen (it will
happen no later than the finalization of the container, of course) in order to
give implementation's flexibility to cache, block, or split the nodes of the
container. In particular, Delete does not necessarily finalize the element; the
implementation may (or may not) hold the space for reuse. (The reference
implementations show this well, as Delete for a Vector does not finalize the
element, while Delete for an Ordered_Set does.)]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This is not likely to be a hardship, as the element
type has to be non-limited. Types used to manage scarce resources generally
need to be limited. Otherwise, the amount of resources needed is hard to
control, as the language allows a lot of variation in the number or order of
adjusts/finalizations. For common uses of non-limited controlled types such as
managing storage, the types already have to manage arbitrary copies.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The use of controlled type also brings up the
possibility of failure of finalization (and thus deallocation) of an element.
This is a @lquotes@;serious bug@rquotes@;, as AI-179 puts it, so we don't try
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
something is unspecified, we
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
implementations that are unstable if given buggy hash functions, et. al.]}
@end{ImplNote}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
This clause is new. It just provides an introduction to the following
subclauses.]}
@end{Extend95}


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
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Hash_Type} @key{is mod} @i<implementation-defined>;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} Count_Type @key{is range} 0 .. @i<implementation-defined>;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{end} Ada.Containers;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-02]}
@ChgAdded{Version=[2],Text=[Hash_Type represents the range of the result of a
hash function. Count_Type represents the (potential or actual) number of
elements of a container.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-02]}
@ChgAdded{Version=[2],Type=[Leading],Text=[Each child unit of Containers
defines a container type. A subprogram is said to @i<tamper with> an object
@i<C> of a container type if:@Defn2{Term=[tamper with], Sec=(a container)}]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it inserts or deletes elements from @i<C>; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it reorders the elements of @i<C>; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it finalizes @i<C>; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it calls the Move procedure (if such a procedure
exists) with @i<C> as a parameter.]}

@end{Itemize}
@end{StaticSem}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Hash_Type'Modulus should be at least 2**32.
Count_Type'Last should be at least 2**31-1.]}

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
The package Ada.Containers is new.]}
@end{Extend95}


@LabeledAddedSubclause{Version=[2],Name=[The Package Containers.Vectors]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[ The language-defined package Containers.Vectors
provides private types Vector and Cursor, and a set of operations for each
type. A vector container allows insertion and deletion at any position, but it
is specifically optimized for insertion and deletion at the high end (the end
with the higher index) of the container. A vector container also provides
random access to its elements.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[
@Pdefn2{Term=[length], Sec=(of a vector container)}
@Pdefn2{Term=[capacity], Sec=(of a vector container)}
A vector container behaves conceptually as an array that expands as necessary
as items are inserted. The @i{length} of a vector is the number of elements that
the vector contains. The @i{capacity} of a vector is the maximum number of
elements that can be inserted into the vector prior to it being automatically
expanded.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[
@Pdefn2{Term=[empty element], Sec=(of a vector container)}
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
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The library package
Containers.Vectors has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{generic}
   @key{type} Index_Type @key{is range} <>;
   @key{type} Element_Type @key{is private};
   @key{with function} "=" (Left, Right : Element_Type)
      @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Vectors @key{is}@ChildUnit{Parent=[Ada.Containers],Child=[Vectors]}
   @key{pragma} Preelaborate(Vectors);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{subtype} @AdaDefn{Extended_Index} @key{is}
      Index_Type'Base @key{range}
         Index_Type'First-1 ..
         Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;
   @AdaDefn{No_Index} : @key{constant} Extended_Index := Extended_Index'First;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{subtype} @AdaDefn{Index_Subtype} @key{is} Index_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Vector} @key{is tagged private};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Cursor} @key{is private};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaDefn{Empty_Vector} : @key{constant} Vector;]}



@end{Example}

**** The rest of this clause (including 14 other subclauses) has yet to be inserted ****

@end{StaticSem}


@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The package Ada.Containers.Vectors is new.]}
@end{Extend95}

