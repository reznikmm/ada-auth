@comment{ $Source: e:\\cvsroot/ARM/Source/pre_containers.mss,v $ }
@comment{ $Revision: 1.86 $ $Date: 2011/11/01 23:14:15 $ $Author: randy $ }
@Part(precontainers, Root="ada.mss")

@Comment{$Date: 2011/11/01 23:14:15 $}

@RMNewPage
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
all of the containers. A cursor referencing
an element in a container is considered to be overlapping
with the container object itself.@PDefn2{Term=[cursor],Sec=[for a container]}
@Defn2{Term=[container],Sec=[cursor]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The last sentence is intended to clarify that
  operations that just use a cursor are on the same footing as operations that
  use a container in terms of the reentrancy rules of Annex A.]}
@end{Reason}

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
@ChgAdded{Version=[2],Text=[If a container's element type is controlled, the
point at which the element is finalized will depend on the implementation of
the container. We do not specify precisely where this will happen (it will
happen no later than the finalization of the container, of course) in order to
give implementation's flexibility to cache, block, or split the nodes of the
container. In particular, Delete does not necessarily finalize the element; the
implementation may (or may not) hold the space for reuse.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This is not likely to be a hardship, as the element
type has to be nonlimited. Types used to manage scarce resources generally
need to be limited. Otherwise, the amount of resources needed is hard to
control, as the language allows a lot of variation in the number or order of
adjusts/finalizations. For common uses of nonlimited controlled types such as
managing storage, the types already have to manage arbitrary copies.]}

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

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  This clause is new. It just provides an introduction to the following
  subclauses.]}
@end{Extend95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0044-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a definition of
  strict weak ordering.]}
@end{DiffWord2005}


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
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Count_Type} @key{is range} 0 .. @i<implementation-defined>;]}

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
  is newly added to Containers. If Containers is referenced in a @nt{use_clause},
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
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[@key[with] Ada.Iterator_Interfaces;
],Old=[]}@key{generic}
   @key{type} Index_Type @key{is range} <>;
   @key{type} Element_Type @key{is private};
   @key{with function} "=" (Left, Right : Element_Type)
      @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Vectors @key{is}@ChildUnit{Parent=[Ada.Containers],Child=[Vectors]}
   @key{pragma} Preelaborate(Vectors);@Chg{Version=[3],New=[
   @key{pragma} Remote_Types(Vectors);],Old=[]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{subtype} @AdaSubtypeDefn{Name=[Extended_Index],Of=[Index_Type'Base]} @key{is}
      Index_Type'Base @key{range}
         Index_Type'First-1 ..
         Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;
   @AdaObjDefn{No_Index} : @key{constant} Extended_Index := Extended_Index'First;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Vector} @key{is tagged private}@Chg{Version=[3],New=[
      @key[with] Constant_Indexing => Constant_Reference,
           Variable_Indexing => Reference,
           Default_Iterator  => Iterate,
           Iterator_Element  => Element_Type],Old=[]};
   @key{pragma} Preelaborable_Initialization(Vector);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Cursor} @key{is private};
   @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Empty_Vector} : @key{constant} Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[package] @AdaPackDefn{Vector_Iterator_Interfaces} @key[is new]
       Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "=" (Left, Right : Vector) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{To_Vector} (Length : Count_Type) @key{return} Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{To_Vector}
     (New_Item : Element_Type;
      Length   : Count_Type) @key{return} Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "&" (Left, Right : Vector) @key{return} Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "&" (Left  : Vector;
                 Right : Element_Type) @key{return} Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "&" (Left  : Element_Type;
                 Right : Vector) @key{return} Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "&" (Left, Right  : Element_Type) @key{return} Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Capacity} (Container : Vector) @key{return} Count_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reserve_Capacity} (Container : @key{in out} Vector;
                               Capacity  : @key{in}     Count_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Length} (Container : Vector) @key{return} Count_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Set_Length} (Container : @key{in out} Vector;
                         Length    : @key{in}     Count_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Empty} (Container : Vector) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Clear} (Container : @key{in out} Vector);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{To_Cursor} (Container : Vector;
                       Index     : Extended_Index) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{To_Index} (Position  : Cursor) @key{return} Extended_Index;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Container : Vector;
                     Index     : Index_Type)
      @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Position : Cursor) @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} Vector;
                              Index     : @key{in}     Index_Type;
                              New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} Vector;
                              Position  : @key{in}     Cursor;
                              New_item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Container : @key{in} Vector;
      Index     : @key{in} Index_Type;
      Process   : @key{not null access procedure} (Element : @key{in} Element_Type));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Element : @key{in} Element_Type));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Update_Element}
     (Container : @key{in out} Vector;
      Index     : @key{in}     Index_Type;
      Process   : @key{not null access procedure}
                      (Element : @key{in out} Element_Type));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Update_Element}
     (Container : @key{in out} Vector;
      Position  : @key{in}     Cursor;
      Process   : @key{not null access procedure}
                      (Element : @key{in out} Element_Type));]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[type] Constant_Reference_Type
         (Element : @key[not null access constant] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Reference_Type} (Element : @key[not null access] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Vector;
                                Index     : @key[in] Index_Type)
      @key[return] Constant_Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] Vector;
                       Index     : @key[in] Index_Type)
      @key[return] Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Vector;
                                Position  : @key[in] Cursor)
      @key[return] Constant_Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] Vector;
                       Position  : @key[in] Cursor)
      @key[return] Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} Vector; Source : @key{in} Vector);]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Copy} (Source : Vector; Capacity : Count_Type := 0)
      @key[return] Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Move} (Target : @key{in out} Vector;
                   Source : @key{in out} Vector);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Extended_Index;
                     New_Item  : @key{in}     Vector);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Vector);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Vector;
                     Position  :    @key{out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Extended_Index;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Extended_Index;
                     Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Vector;
                     Before    : @key{in}     Cursor;
                     Position  :    @key{out} Cursor;
                     Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Prepend} (Container : @key{in out} Vector;
                      New_Item  : @key{in}     Vector);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Prepend} (Container : @key{in out} Vector;
                      New_Item  : @key{in}     Element_Type;
                      Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Append} (Container : @key{in out} Vector;
                     New_Item  : @key{in}     Vector);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Append} (Container : @key{in out} Vector;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert_Space} (Container : @key{in out} Vector;
                           Before    : @key{in}     Extended_Index;
                           Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert_Space} (Container : @key{in out} Vector;
                           Before    : @key{in}     Cursor;
                           Position  :    @key{out} Cursor;
                           Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Vector;
                     Index     : @key{in}     Extended_Index;
                     Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Vector;
                     Position  : @key{in out} Cursor;
                     Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_First} (Container : @key{in out} Vector;
                           Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_Last} (Container : @key{in out} Vector;
                          Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reverse_Elements} (Container : @key{in out} Vector);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Swap} (Container : @key{in out} Vector;
                   I, J      : @key{in}     Index_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Swap} (Container : @key{in out} Vector;
                   I, J      : @key{in}     Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First_Index} (Container : Vector) @key{return} Index_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First} (Container : Vector) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First_Element} (Container : Vector)
      @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last_Index} (Container : Vector) @key{return} Extended_Index;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last} (Container : Vector) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last_Element} (Container : Vector)
      @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Next} (Position : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Next} (Position : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Previous} (Position : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Previous} (Position : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Find_Index} (Container : Vector;
                        Item      : Element_Type;
                        Index     : Index_Type := Index_Type'First)
      @key{return} Extended_Index;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Find} (Container : Vector;
                  Item      : Element_Type;
                  Position  : Cursor := No_Element)
      @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Reverse_Find_Index} (Container : Vector;
                                Item      : Element_Type;
                                Index     : Index_Type := Index_Type'Last)
      @key{return} Extended_Index;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Reverse_Find} (Container : Vector;
                          Item      : Element_Type;
                          Position  : Cursor := No_Element)
      @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Contains} (Container : Vector;
                      Item      : Element_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure}  @AdaSubDefn{Iterate}
     (Container : @key{in} Vector;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reverse_Iterate}
     (Container : @key{in} Vector;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] Iterate (Container : @key[in] Vector)
      @key[return] Vector_Iterator_Interfaces.Reversible_Iterator'Class;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] Iterate (Container : @key[in] Vector; Start : @key[in] Cursor)
      @key[return] Vector_Iterator_Interfaces.Reversible_Iterator'Class;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{generic}
      @key{with function} "<" (Left, Right : Element_Type)
         @key{return} Boolean is <>;
   @key{package} @AdaPackDefn{Generic_Sorting} @key{is}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{function} @AdaSubDefn{Is_Sorted} (Container : Vector) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Sort} (Container : @key{in out} Vector);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Merge} (Target  : @key{in out} Vector;
                       Source  : @key{in out} Vector);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{end} Generic_Sorting;]}

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
@ChgAdded{Version=[3],Text=[Vector'Write writes exactly Length(Vector) elements
of the vector to the stream. It may write additional information about the
vector as well. Vector'Read reads exactly Length(Vector) elements of Vector from
the stream and consumes any additional information written by Vector'Write.]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The Standard requires streaming of all
  language-defined non-limited types (including containers) to "work" (see
  @RefSecNum{Stream-Oriented Attributes}). In addition, we do not want all
  of the elements that make up the
  capacity of the vector streamed, as those beyond the length of the container
  have undefined contents (and might cause bad things when read back in).
  This will require a custom stream attribute
  implementation; the language-defined default implementation will not work
  (even for a bounded form, as that would most likely stream the entire
  capacity of the vector).]}
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
@ChgAdded{Version=[3],Text=[If an operation attempts to modify the vector such
that the position of the last element would be greater than Index_Type'Last,
then the operation propagates Constraint_Error.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We don't want to require an implementation to
  go to heroic efforts to handle index values larger than the base type of
  the index subtype.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Redundant[Some operations of this generic package
have access-to-subprogram parameters. To ensure such operations are
well-defined, they guard against certain actions by the designated
subprogram. In particular, some operations check for @lquotes@;tampering with
cursors@rquotes of a container because they depend on the set of elements of
the container remaining constant, and others check for @lquotes@;tampering with
elements@rquotes of a container because they depend on elements of the
container not being replaced.]]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Defn2{Term=[tamper with cursors],Sec=[of a vector]}
A subprogram is said to
@i{tamper with cursors} of a vector object @i<V> if:]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it inserts or deletes elements of @i<V>, that is,
it calls the Insert, Insert_Space, Clear, Delete, or Set_Length procedures with
@i<V> as a parameter; or]}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Operations which are defined to be equivalent to
  a call on one of these operations also are included. Similarly, operations
  which call one of these as part of their definition are included.]}
@end{Honest}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it finalizes @i<V>; or]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[it calls the Assign procedure with @i<V> as the Target parameter;
or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it calls the Move procedure with @i<V> as
a parameter.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Swap, Sort, and Merge copy elements rather
  than reordering them, so they don't tamper with cursors.]}
@end{Discussion}

@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Defn2{Term=[tamper with elements],Sec=[of a vector]}
A subprogram is said to @i{tamper with elements} of a vector object @i<V> if:]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it tampers with cursors of @i<V>; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it replaces one or more elements of @i<V>, that is,
it calls the Replace_Element, Reverse_Elements, or Swap procedures or the Sort
or Merge procedures of an instance of Generic_Sorting with @i<V> as a
parameter.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Complete replacement of an element can cause its
  memory to be deallocated while another operation is holding onto a reference
  to it. That can't be allowed. However, a simple modification of (part of) an
  element is not a problem, so Update_Element does not cause a problem.]}
@end{Reason}

@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[prohibited],Sec=[tampering with a vector]}
@Defn2{Term=[tampering],Sec=[prohibited for a vector]}
If tampering with cursors is @i<prohibited> for a particular vector
object @i<V>, Program_Error is propagated by any language-defined subprogram
that is defined to tamper with the cursors of @i<V>. Similarly, if tampering with
elements is @i<prohibited> for a particular vector object @i<V>,
Program_Error is propagated by any language-defined subprogram that is defined
to tamper with the elements of @i<V>.]}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Has_Element (Position : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if Position designates
an element, and returns False otherwise.]}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This function may not detect cursors that
  designate deleted elements; such cursors are invalid (see below) and the
  result of calling Has_Element with an invalid cursor is unspecified (but
  not erroneous).]}
@end{Honest}

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
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} To_Vector (Length : Count_Type) @key{return} Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a vector with a length of
Length, filled with empty elements.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} To_Vector
  (New_Item : Element_Type;
   Length   : Count_Type) @key{return} Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a vector with a length of
Length, filled with elements initialized to the value New_Item.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "&" (Left, Right : Vector) @key{return} Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a vector comprising the
elements of Left followed by the elements of Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "&" (Left  : Vector;
              Right : Element_Type) @key{return} Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a vector comprising the
elements of Left followed by the element Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "&" (Left  : Element_Type;
              Right : Vector) @key{return} Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a vector comprising the
element Left followed by the elements of Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "&" (Left, Right  : Element_Type) @key{return} Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a vector comprising the
element Left followed by the element Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Capacity (Container : Vector) @key{return} Count_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the capacity of Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reserve_Capacity (Container : @key{in out} Vector;
                            Capacity  : @key{in}     Count_Type);]}
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Length (Container : Vector) @key{return} Count_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the number of elements in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Set_Length (Container : @key{in out} Vector;
                      Length    : @key{in}     Count_Type);]}
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Is_Empty (Container : Vector) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Length (Container) = 0.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Clear (Container : @key{in out} Vector);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Removes all the elements from
Container. The capacity of Container does not change.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} To_Cursor (Container : Vector;
                    Index     : Extended_Index) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Index is not in the range
First_Index (Container) .. Last_Index (Container), then No_Element is returned.
Otherwise, a cursor designating the element at position Index in Container is
returned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} To_Index (Position  : Cursor) @key{return} Extended_Index;]}
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
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Element (Container : Vector;
                  Index     : Index_Type)
   @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Index is not in the range
First_Index (Container) .. Last_Index (Container), then Constraint_Error is
propagated. Otherwise, Element returns the element at position Index.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Element (Position  : Cursor) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Constraint_Error is propagated. Otherwise, Element returns the element
designated by Position.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace_Element (Container : @key{in out} Vector;
                           Index     : @key{in}     Index_Type;
                           New_Item  : @key{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Index is not in the range
First_Index (Container) .. Last_Index (Container), then Constraint_Error is
propagated. Otherwise@Chg{Version=[3],New=[,],Old=[]}
Replace_Element assigns the value New_Item to the element at
position Index. Any exception raised during the assignment is propagated. The
element at position Index is not an empty element after successful call to
Replace_Element.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace_Element (Container : @key{in out} Vector;
                           Position  : @key{in}     Cursor;
                           New_Item  : @key{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element,
then Constraint_Error is propagated; if Position does not designate an element
in Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]} Replace_Element
assigns New_Item to the element designated by Position. Any exception raised
during the assignment is propagated. The element at Position is not an empty
element after successful call to Replace_Element.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[2],Text=[Replace_Element@Chg{Version=[3],New=[,],Old=[ and]}
  Update_Element@Chg{Version=[3],New=[, and Reference],Old=[]} are the only
  ways that an element can change from empty to non-empty. Also see the note
  following Update_Element.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Query_Element
  (Container : @key{in} Vector;
   Index     : @key{in} Index_Type;
   Process   : @key{not null access} @key{procedure} (Element : @key{in} Element_Type));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Index is not in the range
First_Index (Container) .. Last_Index (Container), then Constraint_Error is
propagated. Otherwise, Query_Element calls Process.@key{all} with the element at
position Index as the argument.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of Container@Chg{Version=[3],New=[ is prohibited during the
execution of Process.@key{all}],Old=[]}. Any exception raised by
Process.@key{all} is propagated.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The @lquotes@;tamper with the elements@rquotes@;
  check is intended to prevent the Element parameter of Process from being
  modified or deleted
  outside of Process. The check prevents data loss (if Element_Type is passed by
  copy) or erroneous execution (if Element_Type is an unconstrained type in an
  indefinite container).]}
@end{Reason}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Query_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access} @key{procedure} (Element : @key{in} Element_Type));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0021-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Constraint_Error is propagated. Otherwise, Query_Element calls
Process.@key{all} with the element designated by Position as the argument.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of @Chg{Version=[3],New=[the vector that contains the
element designated by Position is prohibited during the
execution of Process.@key{all}],Old=[Container]}. Any exception raised by
Process.@key{all} is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Update_Element
  (Container : @key{in out} Vector;
   Index     : @key{in}     Index_Type;
   Process   : @key{not null access} @key{procedure} (Element : @key{in out} Element_Type));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Text=[If Index is not in the range
First_Index (Container) .. Last_Index (Container), then Constraint_Error is
propagated. Otherwise, Update_Element calls Process.@key{all} with the element
at position Index as the argument.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of Container@Chg{Version=[3],New=[ is prohibited during the
execution of Process.@key{all}],Old=[]}. Any exception raised by
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Update_Element
  (Container : @key{in out} Vector;
   Position  : @key{in}     Cursor;
   Process   : @key{not null access} @key{procedure} (Element : @key{in out} Element_Type));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Text=[If Position equals No_Element, then
Constraint_Error is propagated; if Position does not designate an element in
Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]} Update_Element calls
Process.@key{all} with the element designated by Position as the argument.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of Container@Chg{Version=[3],New=[ is prohibited during the
execution of Process.@key{all}],Old=[]}. Any exception raised by
Process.@key{all} is propagated.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[If Element_Type is unconstrained and definite, then
the actual Element parameter of Process.@key{all} shall be unconstrained.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[The element designated by Position
is not an empty element after successful completion of this operation.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key[type] Constant_Reference_Type
      (Element : @key[not null access constant] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[type] Reference_Type (Element : @key[not null access] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[The types Constant_Reference_Type
and Reference_Type need finalization.@PDefn2{Term=<needs finalization>,
Sec=<language-defined type>}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The default initialization of an object of type
Constant_Reference_Type or Reference_Type propagates Program_Error.]}

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
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] Vector;
                             Index     : @key[in] Index_Type)
   @key[return] Constant_Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read access to the individual elements of a container starting with an
index value.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[If Index is not in the range First_Index (Container)
.. Last_Index (Container), then Constraint_Error is propagated. Otherwise,
Constant_Reference returns an object whose discriminant is an access value that
designates the element at position Index. Tampering with the elements of
Container is prohibited while the object returned by Constant_Reference exists
and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference (Container : @key[aliased in out] Vector;
                    Index     : @key[in] Index_Type)
   @key[return] Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read and write access to the individual elements of a container starting
with an index value.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[If Index is not in the range First_Index (Container)
.. Last_Index (Container), then Constraint_Error is propagated. Otherwise,
Reference returns an object whose discriminant is an access value that
designates the element at position Index. Tampering with the elements of
Container is prohibited while the object returned by Reference exists and has
not been finalized.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The element at position Index is not an empty
element after successful completion of this operation.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] Vector;
                             Position  : @key[in] Cursor)
   @key[return] Constant_Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read access to the individual elements of a container starting with a
cursor.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[If Position equals No_Element, then Constraint_Error
is propagated; if Position does not designate an element in Container, then
Program_Error is propagated. Otherwise, Constant_Reference returns an object
whose discriminant is an access value that designates the element designated by
Position. Tampering with the elements of Container is prohibited while the
object returned by Constant_Reference exists and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference (Container : @key[aliased in out] Vector;
                    Position  : @key[in] Cursor)
   @key[return] Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read and write access to the individual elements of a container starting
with a cursor.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[If Position equals No_Element, then Constraint_Error
is propagated; if Position does not designate an element in Container, then
Program_Error is propagated. Otherwise, Reference returns an object whose
discriminant is an access value that designates the element designated by
Position. Tampering with the elements of Container is prohibited while the
object returned by Reference exists and has not been finalized.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The element designated by Position is not an empty
element after successful completion of this operation.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Assign (Target : @key{in out} Vector; Source : @key{in} Vector);]}
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
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Copy (Source : Vector; Capacity : Count_Type := 0)
   @key[return] Vector;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a vector whose elements are
initialized from the corresponding elements of Source. If Capacity is 0, then
the vector capacity is the length of Source; if Capacity is equal to or greater
than the length of Source, the vector capacity is at least the specified value.
Otherwise, the operation propagates Capacity_Error.]}


@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Move (Target : @key{in out} Vector;
                Source : @key{in out} Vector);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Target denotes the same object
as Source, then @Chg{Version=[3],New=[the operation],Old=[Move]}
has no effect. Otherwise, Move first calls
@Chg{Version=[3],New=[Reserve_Capacity (Target, Length (Source))
and then ],Old=[]}Clear (Target);
then, each element from Source is removed from Source and inserted into Target
in the original order. The length of Source is 0 after a successful call to
Move.]}

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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Extended_Index;
                  New_Item  : @key{in}     Vector);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[If Before is not in the range First_Index
(Container) .. Last_Index (Container) + 1, then Constraint_Error is propagated.
If Length(New_Item) is 0, then Insert does nothing. Otherwise, it computes the
new length @i<NL> as the sum of the current length and Length (New_Item); if
the value of Last appropriate for length @i<NL> would be greater than
Index_Type'Last@Chg{Version=[3],New=[,],Old=[]} then
Constraint_Error is propagated.]}

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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Vector);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Before is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
Otherwise, if
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Vector;
                  Position  :    @key{out} Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Before is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
If Before
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Extended_Index;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container, Before, To_Vector (New_Item, Count));]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container, Before, To_Vector (New_Item, Count));]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Count     : @key{in}     Count_Type := 1);]}
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Extended_Index;
                  Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[If Before is not in the range
First_Index (Container) .. Last_Index (Container) + 1, then Constraint_Error is
propagated. If Count is 0, then Insert does nothing. Otherwise, it
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  Position  :    @key{out} Cursor;
                  Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Before is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
If Before
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Prepend (Container : @key{in out} Vector;
                   New_Item  : @key{in}     Vector;
                   Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container, First_Index (Container), New_Item).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Prepend (Container : @key{in out} Vector;
                   New_Item  : @key{in}     Element_Type;
                   Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container, First_Index (Container), New_Item, Count).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Append (Container : @key{in out} Vector;
                  New_Item  : @key{in}     Vector);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container, Last_Index (Container) + 1, New_Item).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Append (Container : @key{in out} Vector;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container, Last_Index (Container) + 1, New_Item, Count).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert_Space (Container : @key{in out} Vector;
                        Before    : @key{in}     Extended_Index;
                        Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[If Before is not in the range
First_Index (Container) .. Last_Index (Container) + 1, then Constraint_Error is
propagated. If Count is 0, then Insert_Space does nothing. Otherwise, it
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert_Space (Container : @key{in out} Vector;
                        Before    : @key{in}     Cursor;
                        Position  :    @key{out} Cursor;
                        Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Before is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
If Before
equals No_Element, then let @i<T> be Last_Index (Container) + 1; otherwise, let
@i<T> be To_Index (Before). Insert_Space (Container, @i<T>, Count) is called,
and then Position is set to To_Cursor (Container, @i<T>).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} Vector;
                  Index     : @key{in}     Extended_Index;
                  Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Index is not in the range
First_Index (Container) .. Last_Index (Container) + 1, then Constraint_Error is
propagated. If Count is 0, Delete has no effect.
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} Vector;
                  Position  : @key{in out} Cursor;
                  Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Constraint_Error is propagated. If Position does not designate an element in
Container, then Program_Error is propagated. Otherwise, Delete (Container,
To_Index (Position), Count) is called, and then Position is set to No_Element.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_First (Container : @key{in out} Vector;
                        Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Delete (Container, First_Index (Container), Count).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_Last (Container : @key{in out} Vector;
                       Count     : @key{in}     Count_Type := 1);]}
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
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reverse_Elements (Container : @key{in out} @Chg{Version=[3],New=[Vector],Old=[List]});]}
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Swap (Container : @key{in out} Vector;
                I, J      : @key{in}     Index_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If either I or J is not in the range
First_Index (Container) .. Last_Index (Container), then Constraint_Error is
propagated. Otherwise, Swap exchanges the values of the elements at positions I
and J.]}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The implementation is not required to actually
  copy the elements if it can do the swap some other way. But it is allowed
  to copy the elements if needed.]}
@end{Honest}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Swap (Container : @key{in out} Vector;
                I, J      : @key{in}     Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If either I or J is No_Element,
then Constraint_Error is propagated. If either I or J do not designate an
element in Container, then Program_Error is propagated. Otherwise, Swap
exchanges the values of the elements designated by I and J.]}

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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First_Index (Container : Vector) @key{return} Index_Type;]}
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First (Container : Vector) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, First
returns No_Element. Otherwise, it returns a cursor that designates the first
element in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First_Element (Container : Vector) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Container, First_Index (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last_Index (Container : Vector) @key{return} Extended_Index;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, Last_Index
returns No_Index. Otherwise, it returns the position of the last element in
Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last (Container : Vector) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, Last returns
No_Element. Otherwise, it returns a cursor that designates the last element in
Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last_Element (Container : Vector) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Container,
Last_Index (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Next (Position : Cursor) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element or
designates the last element of the container, then Next returns the value
No_Element. Otherwise, it returns a cursor that designates the element with index
To_Index (Position) + 1 in the same vector as Position.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Next (Position : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Next (Position).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Previous (Position : Cursor) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element or
designates the first element of the container, then Previous returns the value
No_Element. Otherwise, it returns a cursor that designates the element with index
To_Index (Position) @en 1 in the same vector as Position.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Previous (Position : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Previous (Position).]}

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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Find (Container : Vector;
               Item      : Element_Type;
               Position  : Cursor := No_Element)
   @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]}
Find searches the elements of Container for an element equal to Item
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Reverse_Find (Container : Vector;
                       Item      : Element_Type;
                       Position  : Cursor := No_Element)
   @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]}
Reverse_Find searches the elements of Container for an element equal
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Iterate
  (Container : @key{in} Vector;
   Process   : @key{not null access} @key{procedure} (Position : @key{in} Cursor));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Invokes Process.@key{all} with a
cursor that designates each element in Container, in index order.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the cursors of Container@Chg{Version=[3],New=[ is prohibited during the
execution of Process.@key{all}],Old=[]}. Any exception raised by
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reverse_Iterate
  (Container : @key{in} Vector;
   Process   : @key{not null access} @key{procedure} (Position : @key{in} Cursor));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Iterates over the elements in
Container as per @Chg{Version=[3],New=[procedure ],Old=[]}Iterate,
except that elements are traversed in reverse index order.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] Vector)
   @key[return] Vector_Iterator_Interfaces.Reversible_Iterator'Class;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate returns a reversible
iterator object that will generate a value for the loop parameter designating
each node in Container, starting with the first node and moving the cursor as
per the Next function when used as a forward iterator, and starting with the
last node and moving the cursor as per the Previous function when used as a
reverse iterator. Tampering with the cursors of Container is prohibited while
the iterator object exists (in particular, in
the @nt{sequence_of_statements} of the @nt{loop_statement} whose
@nt{iterator_specification} denotes this object). The iterator object needs
finalization.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] Vector; Start : @key[in] Cursor)
   @key[return] Vector_Iterator_Interfaces.Reversible_Iterator'Class;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Start is not No_Element and does
not designate an item in Container, then Program_Error is propagated. If Start
is No_Element, the call is equivalent to Iterate (Container). Otherwise, Iterate
returns a reversible iterator object that will generate a value for the
loop parameter designating each node in Container, starting with the node
designated by Start and moving the cursor as per the Next function when used as
a forward iterator, or moving the cursor as per the Previous function when used
as a reverse iterator. Tampering with the cursors of Container is prohibited
while the iterator object exists (in particular, in the
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
  the @exam{Cur} is the loop parameter and @exam{Stop} is the cursor that you
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Sort (Container : @key{in out} Vector);]}
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Merge (Target  : @key{in out} Vector;
                 Source  : @key{in out} Vector);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0021-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[3],New=[If Source is
empty, then Merge does nothing. If Source and Target are the same non-empty
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
element type, or raise Constraint_Error or Program_Error before modifying
the vector.]}

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
defined.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0022-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[It is a bounded error for the actual function
associated with a generic formal subprogram, when called as part of an
operation of this package, to tamper with elements of any Vector parameter of
the operation. Either Program_Error is raised, or the operation works as
defined on the value of the Vector either prior to, or subsequent to, some or
all of the modifications to the Vector.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0027-1]}
@ChgAdded{Version=[3],Text=[It is a bounded error to call any subprogram
declared in the visible part of Containers.Vectors
when the associated container has been finalized. If the operation takes
Container as an @key[in out] parameter, then it raises Constraint_Error or
Program_Error. Otherwise, the operation either proceeds as it would
for an empty container, or it raises Constraint_Error or Program_Error.]}

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
@ChgAdded{Version=[2],Text=[Constraint_Error may be raised; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Program_Error may be raised.]}

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
  finalization of the object of Reference_Type will try to access a non-existent
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
  @ChgAdded{Version=[2],Text=[An assignment of a Vector is a @lquotes@;deep@rquotes
  copy; that is the elements are copied as well as the data structures.
  We say @lquotes@;effect of@rquotes in order to allow the implementation to
  avoid copying elements immediately if it wishes. For instance, an
  implementation that avoided copying until one of the containers is modified
  would be allowed.]}
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
  are newly added to Containers.Vectors. If an instance of Containers.Vectors
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
  to cover access to finalized Vector containers.]}

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
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[@key[with] Ada.Iterator_Interfaces;
],Old=[]}@key{generic}
   @key{type} Element_Type @key{is private};
   @key{with function} "=" (Left, Right : Element_Type)
      @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Doubly_Linked_Lists @key{is}@ChildUnit{Parent=[Ada.Containers],Child=[Doubly_@!Linked_@!Lists]}
   @key{pragma} Preelaborate(Doubly_Linked_Lists);@Chg{Version=[3],New=[
   @key{pragma} Remote_Types(Doubly_Linked_Lists);],Old=[]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{List} @key{is tagged private}@Chg{Version=[3],New=[
      @key[with] Constant_Indexing => Constant_Reference,
           Variable_Indexing => Reference,
           Default_Iterator  => Iterate,
           Iterator_Element  => Element_Type],Old=[]};
   @key{pragma} Preelaborable_Initialization(List);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Cursor} @key{is private};
   @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Empty_List} : @key{constant} List;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[package] @AdaPackDefn{List_Iterator_Interfaces} @key[is new]
       Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "=" (Left, Right : List) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Length} (Container : List) @key{return} Count_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Empty} (Container : List) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Clear} (Container : @key{in out} List);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Position : Cursor)
      @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} List;
                              Position  : @key{in}     Cursor;
                              New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Element : @key{in} Element_Type));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Update_Element}
     (Container : @key{in out} List;
      Position  : @key{in}     Cursor;
      Process   : @key{not null access procedure}
                      (Element : @key{in out} Element_Type));]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[type] Constant_Reference_Type
         (Element : @key[not null access constant] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Reference_Type} (Element : @key[not null access] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] List;
                                Position  : @key[in] Cursor)
      @key[return] Constant_Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] List;
                       Position  : @key[in] Cursor)
      @key[return] Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} List; Source : @key{in} List);]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Copy} (Source : List) @key[return] List;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Move} (Target : @key{in out} List;
                   Source : @key{in out} List);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} List;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} List;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} List;
                     Before    : @key{in}     Cursor;
                     Position  :    @key{out} Cursor;
                     Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Prepend} (Container : @key{in out} List;
                      New_Item  : @key{in}     Element_Type;
                      Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Append} (Container : @key{in out} List;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} List;
                     Position  : @key{in out} Cursor;
                     Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_First} (Container : @key{in out} List;
                           Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_Last} (Container : @key{in out} List;
                          Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reverse_Elements} (Container : @key{in out} List);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Swap} (Container : @key{in out} List;
                   I, J      : @key{in}     Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Swap_Links} (Container : @key{in out} List;
                         I, J      : @key{in}     Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Splice} (Target   : @key{in out} List;
                     Before   : @key{in}     Cursor;
                     Source   : @key{in out} List);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Splice} (Target   : @key{in out} List;
                     Before   : @key{in}     Cursor;
                     Source   : @key{in out} List;
                     Position : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Splice} (Container: @key{in out} List;
                     Before   : @key{in}     Cursor;
                     Position : @key{in}     Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First} (Container : List) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First_Element} (Container : List)
      @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last} (Container : List) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last_Element} (Container : List)
      @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Next} (Position : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Previous} (Position : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Next} (Position : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Previous} (Position : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Find} (Container : List;
                  Item      : Element_Type;
                  Position  : Cursor := No_Element)
      @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Reverse_Find} (Container : List;
                          Item      : Element_Type;
                          Position  : Cursor := No_Element)
      @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Contains} (Container : List;
                      Item      : Element_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Iterate}
     (Container : @key{in} List;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reverse_Iterate}
     (Container : @key{in} List;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] Iterate (Container : @key[in] List)
      @key[return] List_Iterator_Interfaces.Reversible_Iterator'Class;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] Iterate (Container : @key[in] List; Start : @key[in] Cursor)
      @key[return] List_Iterator_Interfaces.Reversible_Iterator'Class;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{generic}
      @key{with function} "<" (Left, Right : Element_Type)
         @key{return} Boolean is <>;
   @key{package} @AdaPackDefn{Generic_Sorting} @key{is}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{function} @AdaSubDefn{Is_Sorted} (Container : List) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Sort} (Container : @key{in out} List);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Merge} (Target  : @key{in out} List;
                       Source  : @key{in out} List);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{end} Generic_Sorting;]}

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
@ChgAdded{Version=[3],Text=[List'Write writes exactly Length(List) elements of
the list to the stream. It may write additional information about the list as
well. List'Read reads exactly Length(List) elements of List from the stream and
consumes any additional information written by List'Write.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Streaming more elements than the container
  length is wrong. For implementation implications of this rule, see the Implementation Note in
  @RefSecNum{The Generic Package Containers.Vectors}.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Redundant[Some operations of this generic package
have access-to-subprogram parameters. To ensure such operations are
well-defined, they guard against certain actions by the designated
subprogram. In particular, some operations check for @lquotes@;tampering with
cursors@rquotes of a container because they depend on the set of elements of
the container remaining constant, and others check for @lquotes@;tampering with
elements@rquotes of a container because they depend on elements of the
container not being replaced.]]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Defn2{Term=[tamper with cursors],Sec=[of a list]}
A subprogram is said to @i{tamper with cursors} of a list object
@i<L> if:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it inserts or deletes elements of @i<L>, that is,
it calls the Insert, Clear, Delete, or Delete_Last procedures with @i<L> as a
parameter; or]}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Operations which are defined to be equivalent to
  a call on one of these operations also are included. Similarly, operations
  which call one of these as part of their definition are included.]}
@end{Honest}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it reorders the elements of @i<L>, that is, it
calls the Splice, Swap_Links, or Reverse_Elements procedures or the Sort or
Merge procedures of an instance of Generic_Sorting with @i<L> as a parameter; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it finalizes @i<L>; or]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[it calls the Assign procedure with @i<L> as the
Target parameter; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it calls the Move procedure with @i<L> as a
parameter.]}
@end{Itemize}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Swap copies elements rather than reordering them,
  so it doesn't tamper with cursors.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Defn2{Term=[tamper with elements],Sec=[of a list]}
A subprogram is said to @i{tamper with elements} of a list
object @i<L> if:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it tampers with cursors of @i<L>; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it replaces one or more elements of @i<L>, that is,
it calls the Replace_Element or Swap procedures with @i<L> as
a parameter.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Complete replacement of an element can cause
  its memory to be deallocated while another operation is holding onto a
  reference to it. That can't be allowed. However, a simple modification of
  (part of) an element is not a problem, so Update_Element does not cause a
  problem.]}
@end{Reason}


@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[prohibited],Sec=[tampering with a list]}
@Defn2{Term=[tampering],Sec=[prohibited for a list]}
If tampering with cursors is @i<prohibited> for a particular list
object @i<L>, Program_Error is propagated by any language-defined subprogram
that is defined to tamper with the cursors of @i<L>. Similarly, if tampering with
elements is @i<prohibited> for a particular list object @i<L>,
Program_Error is propagated by any language-defined subprogram that is defined
to tamper with the elements of @i<L>.]}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Has_Element (Position : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if Position designates
an element, and returns False otherwise.]}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This function may not detect cursors that
  designate deleted elements; such cursors are invalid (see below) and the
  result of calling Has_Element with an invalid cursor is unspecified (but
  not erroneous).]}
@end{Honest}

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
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Length (Container : List) @key{return} Count_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the number of elements in
Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Is_Empty (Container : List) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Length (Container) = 0.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Clear (Container : @key{in out} List);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Removes all the elements from Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Element (Position : Cursor) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Constraint_Error is propagated. Otherwise, Element returns the element
designated by Position.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace_Element (Container : @key{in out} List;
                           Position  : @b{in}     Cursor;
                           New_Item  : @b{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element,
then Constraint_Error is propagated; if Position does not designate an element
in Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]} Replace_Element
assigns the value New_Item to the element designated by Position.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Query_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access procedure} (Element : @key{in} Element_Type));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0021-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Constraint_Error is propagated. Otherwise, Query_Element calls
Process.@key{all} with the element designated by Position as the argument.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of @Chg{Version=[3],New=[the list that contains the
element designated by Position is prohibited during the
execution of Process.@key{all}],Old=[Container]}. Any exception raised by
Process.@key{all} is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Update_Element
  (Container : @key{in out} List;
   Position  : @key{in}     Cursor;
   Process   : @key{not null access procedure} (Element : @key{in out} Element_Type));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Text=[If Position equals No_Element, then
Constraint_Error is propagated; if Position does not designate an element in
Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]} Update_Element calls
Process.@key{all} with the element designated by Position as the
argument. @Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of Container@Chg{Version=[3],New=[ is prohibited during the
execution of Process.@key{all}],Old=[]}. Any exception raised by
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
@ChgAdded{Version=[3],Text=[@key[type] Constant_Reference_Type
      (Element : @key[not null access constant] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[type] Reference_Type (Element : @key[not null access] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[The types Constant_Reference_Type
and Reference_Type need finalization.@PDefn2{Term=<needs finalization>,
Sec=<language-defined type>}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The default initialization of an object of type
Constant_Reference_Type or Reference_Type propagates Program_Error.]}

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
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] List;
                             Position  : @key[in] Cursor)
   @key[return] Constant_Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read access to the individual elements of a container starting with a
cursor.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[If Position equals No_Element, then Constraint_Error
is propagated; if Position does not designate an element in Container, then
Program_Error is propagated. Otherwise, Constant_Reference returns an object
whose discriminant is an access value that designates the element designated by
Position. Tampering with the elements of Container is prohibited while the
object returned by Constant_Reference exists and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference (Container : @key[aliased in out] List;
                    Position  : @key[in] Cursor)
   @key[return] Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read and write access to the individual elements of a container starting
with a cursor.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[If Position equals No_Element, then Constraint_Error
is propagated; if Position does not designate an element in Container, then
Program_Error is propagated. Otherwise, Reference returns an object whose
discriminant is an access value that designates the element designated by
Position. Tampering with the elements of Container is prohibited while the
object returned by Reference exists and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Assign (Target : @key{in out} List;
                Source : @key{in} List);]}
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
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Copy (Source : List) @key[return] List;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a list whose elements match
the elements of Source.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Move (Target : @key{in out} List;
                Source : @key{in out} List);]}
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} List;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Before is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
Otherwise,
Insert inserts Count copies of New_Item prior to the element designated by
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} List;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0257-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Before is not No_Element, and
does not designate an element in Container, then Program_Error is
propagated. Otherwise,
Insert allocates Count copies of New_Item, and inserts them prior to the
element designated by Before. If Before equals No_Element, the new elements are
inserted after the last element (if any). Position designates the first
newly-inserted element@Chg{Version=[3],New=[, or if Count equals 0,
then Position is assigned the value of Before],Old=[]}. Any exception raised
during allocation of internal
storage is propagated, and Container is not modified.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} List;
                  Before    : @key{in}     Cursor;
                  Position  :    @key{out} Cursor;
                  Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0257-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Before is not No_Element, and
does not designate an element in Container, then Program_Error is
propagated. Otherwise,
Insert inserts Count new elements prior to the element designated by Before. If
Before equals No_Element, the new elements are inserted after the last node (if
any). The new elements are initialized by default (see
@RefSecNum{Object Declarations}). @Chg{Version=[3],New=[Position designates the
first newly-inserted element, or if Count equals 0,
then Position is assigned the value of Before],Old=[]}. Any exception raised
during allocation of internal storage is propagated, and Container is not
modified.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Prepend (Container : @key{in out} List;
                   New_Item  : @key{in}     Element_Type;
                   Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container,
First (Container), New_Item, Count).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Append (Container : @key{in out} List;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Insert (Container,
No_Element, New_Item, Count).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} List;
                  Position  : @key{in out} Cursor;
                  Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Constraint_Error is propagated. If Position does not designate an element in
Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]} Delete removes (from
Container) Count elements starting at the element designated by Position (or
all of the elements starting at Position if there are fewer than Count elements
starting at Position). Finally, Position is set to No_Element.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_First (Container : @key{in out} List;
                        Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0021-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[3],New=[If Length
(Container) <= Count, then Delete_First is equivalent to Clear (Container).
Otherwise, it removes the first Count nodes from Container],Old=[Equivelent to
Delete (Container, First (Container), Count)]}.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_Last (Container : @key{in out} List;
                       Count     : @key{in}     Count_Type := 1);]}
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reverse_Elements (Container : @key{in out} List);]}
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Swap (Container : @key{in out} List;
                I, J      : @key{in}     Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If either I or J is No_Element,
then Constraint_Error is propagated. If either I or J do not designate an
element in Container, then Program_Error is propagated. Otherwise, Swap
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Swap_Links (Container : @key{in out} List;
                      I, J      : @key{in}     Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If either I or J is No_Element,
then Constraint_Error is propagated. If either I or J do not designate an
element in Container, then Program_Error is propagated. Otherwise, Swap_Links
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Splice (Target   : @key{in out} List;
                  Before   : @key{in}     Cursor;
                  Source   : @key{in out} List);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Before is not No_Element, and
does not designate an element in Target, then Program_Error is propagated.
Otherwise, if
Source denotes the same object as Target, the operation has no effect.
Otherwise, Splice reorders elements such that they are removed from Source and
moved to Target, immediately prior to Before. If Before equals No_Element, the
nodes of Source are spliced after the last node of Target. The length of Target
is incremented by the number of nodes in Source, and the length of Source is
set to 0.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Splice (Target   : @key{in out} List;
                  Before   : @key{in}     Cursor;
                  Source   : @key{in out} List;
                  Position : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position is
No_Element@Chg{Version=[3],New=[,],Old=[]} then
Constraint_Error is propagated. If Before does not equal No_Element, and does
not designate an element in Target, then Program_Error is propagated. If
Position does not equal No_Element, and does not designate a node in Source,
then Program_Error is propagated. If Source denotes the same object as Target,
then there is no effect if Position equals Before, else the element
designated by Position is moved immediately prior to Before, or, if Before
equals No_Element, after the last element.
In both cases, Position and the length of Target are unchanged.
Otherwise@Chg{Version=[3],New=[,],Old=[]} the element
designated by Position is removed from Source and moved to Target, immediately
prior to Before, or, if Before equals No_Element, after the last element of
Target. The length of Target is incremented, the length of Source is
decremented, and Position is updated to represent an element in Target.]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If Source is the same as Target, and
  Position = Before, or Next(Position) = Before, Splice has no effect, as
  the element does not have to move to meet the postcondition.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Splice (Container: @key{in out} List;
                  Before   : @key{in}     Cursor;
                  Position : @key{in}     Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position is
No_Element@Chg{Version=[3],New=[,],Old=[]} then
Constraint_Error is propagated. If Before does not equal No_Element, and does
not designate an element in Container, then Program_Error is propagated. If
Position does not equal No_Element, and does not designate a node in Container,
then Program_Error is propagated. If Position equals Before there is no effect.
Otherwise, the element designated by Position is moved immediately prior to
Before, or, if Before equals No_Element, after the last element. The length of
Container is unchanged.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First (Container : List) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, First
returns the value No_Element.
Otherwise@Chg{Version=[3],New=[,],Old=[]}
it returns a cursor that designates the first node in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First_Element (Container : List) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (First (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last (Container : List) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, Last returns
the value No_Element. Otherwise@Chg{Version=[3],New=[,],Old=[]}
it returns a cursor that designates the last node in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last_Element (Container : List) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Last (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Next (Position : Cursor) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element or
designates the last element of the container, then Next returns the value
No_Element. Otherwise, it returns a cursor that designates the successor of the
element designated by Position.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Previous (Position : Cursor) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element or
designates the first element of the container, then Previous returns the value
No_Element. Otherwise, it returns a cursor that designates the predecessor of
the element designated by Position.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Next (Position : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Next (Position).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Previous (Position : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Previous (Position).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Find (Container : List;
               Item      : Element_Type;
               Position  : Cursor := No_Element)
  @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
Find searches the elements of Container for an element equal to Item (using
the generic formal equality operator). The search starts at the
element designated by Position, or at the first element if Position equals
No_Element. It proceeds towards Last (Container). If no equal element is found,
then Find returns No_Element. Otherwise, it returns a cursor designating the
first equal element encountered.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Reverse_Find (Container : List;
                       Item      : Element_Type;
                       Position  : Cursor := No_Element)
   @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position is not No_Element, and
does not designate an element in Container, then Program_Error is propagated.
Find searches the elements of Container for an element equal to Item (using
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Iterate
  (Container : @key{in} List;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Iterate calls Process.@key{all}
with a cursor that designates each node in Container, starting with the first
node and moving the cursor as per the Next function.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the cursors of Container@Chg{Version=[3],New=[ is prohibited during the
execution of Process.@key{all}],Old=[]}. Any exception raised by
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reverse_Iterate
  (Container : @key{in} List;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}
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
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] List)
   @key[return] List_Iterator_Interfaces.Reversible_Iterator'Class;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate returns a reversible
iterator object that will generate a value for the loop parameter designating
each node in Container, starting with the first node and moving the cursor as
per the Next function when used as a forward iterator, and starting with the
last node and moving the cursor as per the Previous function when used as a
reverse iterator. Tampering with the cursors of Container is prohibited while
the iterator object exists (in particular, in
the @nt{sequence_of_statements} of the @nt{loop_statement} whose
@nt{iterator_specification} denotes this object). The iterator object needs
finalization.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] List; Start : @key[in] Cursor)
   @key[return] List_Iterator_Interfaces.Reversible_Iterator'Class;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Start is not No_Element and does
not designate an item in Container, then Program_Error is propagated. If Start
is No_Element, the call is equivalent to Iterate (Container). Otherwise, Iterate
returns a reversible iterator object that will generate a value for the loop
parameter designating each node in Container, starting with the node designated
by Start and moving the cursor as per the Next function when used as a forward
iterator, or moving the cursor as per the Previous function when used as a
reverse iterator. Tampering with the cursors of Container is prohibited while
the iterator object exists (in particular, in
the @nt{sequence_of_statements} of the @nt{loop_statement} whose
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
  the @exam{Cur} is the loop parameter and @exam{Stop} is the cursor that you
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Sort (Container : @key{in out} List);]}
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Merge (Target  : @key{in out} List;
                 Source  : @key{in out} List);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0021-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[3],New=[If Source is
empty, then Merge does nothing. If Source and Target are the same non-empty
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

@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
Calling Merge in an instance of Generic_Sorting
with either Source or Target not ordered smallest first using the provided
generic formal "<" operator is a bounded error. Either Program_Error is raised
after Target is updated as described for Merge, or the operation works as
defined.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0022-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error for the actual function
associated with a generic formal subprogram, when called as part of an
operation of this package, to tamper with elements of any List parameter of
the operation. Either Program_Error is raised, or the operation works as
defined on the value of the List either prior to, or subsequent to, some or
all of the modifications to the List.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0027-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to call any subprogram
declared in the visible part of Containers.Doubly_Linked_List
when the associated container has been finalized. If the operation takes
Container as an @key[in out] parameter, then it raises Constraint_Error or
Program_Error. Otherwise, the operation either proceeds as it would
for an empty container, or it raises Constraint_Error or Program_Error.]}
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
  finalization of the object of Reference_Type will try to access a non-existent
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
  @ChgAdded{Version=[2],Text=[An assignment of a List is a @lquotes@;deep@rquotes
  copy; that is the elements are copied as well as the data structures.
  We say @lquotes@;effect of@rquotes in order to allow the implementation to
  avoid copying elements immediately if it wishes. For instance, an
  implementation that avoided copying until one of the containers is modified
  would be allowed.]}
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
  are newly added to Containers.Doubly_Linked_Lists. If an instance of Containers.Doubly_Linked_Lists
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


@LabeledAddedSubclause{Version=[2],Name=[Maps]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The language-defined generic packages
Containers.Hashed_Maps and Containers.Ordered_Maps provide private types Map
and Cursor, and a set of operations for each type. A map container allows an
arbitrary type to be used as a key to find the element associated with that
key. A hashed map uses a hash function to organize the keys, while an ordered
map orders the keys per a specified relation.
@Defn{map container}@Defn2{Term=[container],Sec=[map]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[This section describes the declarations that are
common to both kinds of maps. See @RefSecNum{The Generic Package Containers.Hashed_Maps}
for a description of the semantics specific to
Containers.Hashed_Maps and @RefSecNum{The Generic Package Containers.Ordered_Maps} for
a description of the semantics specific to Containers.Ordered_Maps.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
"=" on Element_Type values is expected to define a reflexive and symmetric
relationship and return the same result value each time it is called with a
particular pair of values. If it behaves in some other manner, the function
"=" on map values returns an unspecified value. The
exact arguments and number of calls of this generic formal function by the
function "=" on map values are unspecified.@PDefn{unspecified}]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If the actual function for "=" is not symmetric
  and consistent, the result returned by "=" for Map objects cannot be predicted.
  The implementation is not required to protect
  against "=" raising an exception, or returning random results, or any
  other @lquotes@;bad@rquotes behavior. And it can call "=" in whatever
  manner makes sense. But note that only the result of "=" for Map objects
  is unspecified; other subprograms are not allowed to break if "="
  is bad (they aren't expected to use "=").]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The type Map is used to represent maps. The type
Map needs finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[node],Sec=[of a map]}A map contains
pairs of keys and elements, called @i{nodes}. Map cursors designate nodes, but
also can be thought of as designating an element (the element contained in the
node) for consistency with the other containers. There exists an equivalence
relation on keys, whose definition is different for hashed maps and ordered
maps. A map never contains two or more nodes with equivalent keys. The
@i{length} of a map is the number of nodes it
contains.@Defn2{Term=[length],Sec=[of a map]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[first node],Sec=[of a map]}
@Defn2{Term=[last node],Sec=[of a map]}
@Defn2{Term=[successor node],Sec=[of a map]}Each nonempty map has two
particular nodes called the @i{first node} and the @i{last node} (which may be
the same). Each node except for the last node has a @i{successor node}. If
there are no other intervening operations, starting with the first node and
repeatedly going to the successor node will visit each node in the map exactly
once until the last node is reached. The exact definition of these terms is
different for hashed maps and ordered maps.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Redundant[Some operations of these generic packages
have access-to-subprogram parameters. To ensure such operations are
well-defined, they guard against certain actions by the designated
subprogram. In particular, some operations check for @lquotes@;tampering with
cursors@rquotes of a container because they depend on the set of elements of
the container remaining constant, and others check for @lquotes@;tampering with
elements@rquotes of a container because they depend on elements of the
container not being replaced.]]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[
@Defn2{Term=[tamper with cursors],Sec=[of a map]}
A subprogram is said to @i{tamper with cursors} of a map object @i<M>
if:]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it inserts or deletes elements of @i<M>, that is,
it calls the Insert, Include, Clear, Delete, or Exclude procedures with @i<M>
as a parameter; or]}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Operations which are defined to be equivalent to
  a call on one of these operations also are included. Similarly, operations
  which call one of these as part of their definition are included.]}
@end{Honest}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it finalizes @i<M>; or]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[it calls the Assign procedure with @i<M> as the Target parameter; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it calls the Move procedure with @i<M> as a
parameter; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it calls one of the operations defined to tamper
with the cursors of @i<M>.]}

@end{Itemize}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Replace only modifies a key and element rather
  than rehashing, so it does not tamper with cursors.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[
@Defn2{Term=[tamper with elements],Sec=[of a map]}
A subprogram is said to @i{tamper with elements} of a map object @i<M> if:]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it tampers with cursors of @i<M>; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it replaces one or more elements of @i<M>, that is,
it calls the Replace or Replace_Element procedures with @i<M>
as a parameter.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Complete replacement of an element can cause its
  memory to be deallocated while another operation is holding onto a reference
  to it. That can't be allowed. However, a simple modification of (part of) an
  element is not a problem, so Update_Element does not cause a problem.]}
@end{Reason}

@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[prohibited],Sec=[tampering with a map]}
@Defn2{Term=[tampering],Sec=[prohibited for a map]}
If tampering with cursors is @i<prohibited> for a particular map
object @i<M>, Program_Error is propagated by any language-defined subprogram
that is defined to tamper with the cursors of @i<M>. Similarly, if tampering with
elements is @i<prohibited> for a particular map object @i<M>,
Program_Error is propagated by any language-defined subprogram that is defined
to tamper with the elements of @i<M>.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Empty_Map represents the empty Map object. It has a
length of 0. If an object of type Map is not otherwise initialized, it is
initialized to the same value as Empty_Map.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[No_Element represents a cursor that designates no
node. If an object of type Cursor is not otherwise initialized, it is
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
@ChgAdded{Version=[3],Text=[Map'Write writes exactly Length(Map) elements of the
map to the stream. It may write additional information about the map as well.
Map'Read reads exactly Length(Map) elements of Map from the stream and consumes
any additional information written by Map'Write.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Streaming more elements than the container
  length is wrong. For implementation implications of this rule, see the Implementation Note in
  @RefSecNum{The Generic Package Containers.Vectors}.]}
@end{Ramification}


@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Has_Element (Position : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if Position designates
an element, and returns False otherwise.]}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This function may not detect cursors that
  designate deleted elements; such cursors are invalid (see below) and the
  result of calling Has_Element with an invalid cursor is unspecified (but
  not erroneous).]}
@end{Honest}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "=" (Left, Right : Map) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[If Left and Right denote the same
map object, then the function returns True. If Left and Right have different
lengths, then the function returns False. Otherwise, for each key @i<K> in
Left, the function returns False if:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[a key equivalent to @i<K> is not present
in Right; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[the element associated with @i<K>
in Left is not equal to the element associated with @i<K> in Right (using the
generic formal equality operator for elements).]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If the function has not returned a
result after checking all of the keys, it returns True. Any exception raised
during evaluation of key equivalence or element equality is propagated.]}
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
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Length (Container : Map) @key{return} Count_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the number of nodes in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Is_Empty (Container : Map) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Length (Container) = 0.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Clear (Container : @key{in out} Map);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Removes all the nodes from Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Key (Position : Cursor) @key{return} Key_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Constraint_Error is propagated. Otherwise, Key returns the key component of the
node designated by Position.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Element (Position : Cursor) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Constraint_Error is propagated. Otherwise, Element returns the element
component of the node designated by Position.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace_Element (Container : @key{in out} Map;
                           Position  : @key{in}     Cursor;
                           New_Item  : @key{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element,
then Constraint_Error is propagated; if Position does not designate an element
in Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]} Replace_Element
assigns New_Item to the element of the node designated by Position.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Query_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access procedure} (Key     : @key{in} Key_Type;
                                         Element : @key{in} Element_Type));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0021-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Constraint_Error is propagated. Otherwise, Query_Element calls
Process.@key{all} with the key and element from the node designated by Position
as the arguments.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of @Chg{Version=[3],New=[the map that contains the
element designated by Position is prohibited during the
execution of Process.@key{all}],Old=[Container]}. Any exception raised by
Process.@key{all} is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Update_Element
  (Container : @key{in out} Map;
   Position  : @key{in}     Cursor;
   Process   : @key{not null access procedure} (Key     : @key{in}     Key_Type;
                                          Element : @key{in out} Element_Type));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Text=[If Position equals No_Element, then
Constraint_Error is propagated; if Position does not designate an element in
Container, then Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]} Update_Element calls
Process.@key{all} with the key and element from the node designated by Position
as the arguments. @Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of Container@Chg{Version=[3],New=[ is prohibited during the
execution of Process.@key{all}],Old=[]}. Any exception raised by
Process.@key{all} is propagated.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Type=[Trailing],Text=[If Element_Type is
unconstrained and definite, then the actual Element parameter of Process.@key{all}
shall be unconstrained.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This means that the elements cannot be directly
  allocated from the heap; it must be possible to change the discriminants
  of the element in place.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key[type] Constant_Reference_Type
      (Element : @key[not null access constant] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[type] Reference_Type (Element : @key[not null access] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[The types Constant_Reference_Type
and Reference_Type need finalization.@PDefn2{Term=<needs finalization>,
Sec=<language-defined type>}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The default initialization of an object of type
Constant_Reference_Type or Reference_Type propagates Program_Error.]}

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
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] Map;
                             Position  : @key[in] Cursor)
   @key[return] Constant_Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read access to the individual elements of a container starting with a
cursor.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[If Position equals No_Element, then Constraint_Error
is propagated; if Position does not designate an element in Container, then
Program_Error is propagated. Otherwise, Constant_Reference returns an object
whose discriminant is an access value that designates the element designated by
Position. Tampering with the elements of Container is prohibited while the
object returned by Constant_Reference exists and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference (Container : @key[aliased in out] Map;
                    Position  : @key[in] Cursor)
   @key[return] Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read and write access to the individual elements of a container starting
with a cursor.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[If Position equals No_Element, then Constraint_Error
is propagated; if Position does not designate an element in Container, then
Program_Error is propagated. Otherwise, Reference returns an object whose
discriminant is an access value that designates the element designated by
Position. Tampering with the elements of Container is prohibited while the
object returned by Reference exists and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] Map;
                             Key       : @key[in] Key_Type)
   @key[return] Constant_Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read access to the individual elements of a container starting with a
key value.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Equivalent to
Constant_Reference (Container, Find (Container, Key)).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference (Container : @key[aliased in out] Map;
                    Key       : @key[in] Key_Type)
   @key[return] Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read and write access to the individual elements of a container starting
with a key value.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Equivalent to Reference (Container, Find (Container, Key)).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Assign (Target : @key{in out} Map; Source : @key{in} Map);]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Target denotes the same object as
Source, the operation has no effect. Otherwise, the key/element pairs of Source
are copied to Target as for an @nt{assignment_statement} assigning Source to
Target.]}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[3],Text=[This routine exists for compatibility with the
  bounded map containers. For an unbounded map, @exam{Assign(A, B)} and
  @exam{A := B} behave identically. For a bounded map, := will raise an
  exception if the container capacities are different, while Assign will
  not raise an exception if there is enough room in the target.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Move (Target : @key{in out} Map;
                Source : @key{in out} Map);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0001-1],ARef=[AI05-0248-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Target denotes the same object
as Source, then @Chg{Version=[3],New=[the operation],Old=[Move]}
has no effect. Otherwise, @Chg{Version=[3],New=[the operation is equivalent
to Assign (Target, Source) followed by Clear (Source)],Old=[Move first calls
Clear (Target). Then, each node from Source is removed from Source and inserted
into Target. The length of Source is 0 after a successful call to Move]}.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Insert checks if a node with a key
equivalent to Key is already present in Container. If a match is found,
Inserted is set to False and Position designates the element with the matching
key. Otherwise, Insert allocates a new node, initializes it to Key and
New_Item, and adds it to Container; Inserted is set to True and Position
designates the newly-inserted node. Any exception raised during allocation is
propagated and Container is not modified.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Insert inserts Key into Container
as per the five-parameter Insert, with the difference that an element
initialized by default (see @RefSecNum{Object Declarations}) is inserted.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type;
                  New_Item  : @key{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Insert inserts Key and New_Item
into Container as per the five-parameter Insert, with the difference that if a
node with a key equivalent to Key is already in the map, then Constraint_Error
is propagated.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[This is equivalent to:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{declare}
  Inserted : Boolean; C : Cursor;
@key{begin}
  Insert (Container, Key, New_Item, C, Inserted);
  @key{if not} Inserted @key{then}
     @key{raise} Constraint_Error;
  @key{end if};
@key{end};]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[but doesn't require the hassle of
  @key{out} parameters.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Include (Container : @key{in out} Map;
                   Key       : @key{in}     Key_Type;
                   New_Item  : @key{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Include inserts Key and New_Item
into Container as per the five-parameter Insert, with the difference that if a
node with a key equivalent to Key is already in the map, then this operation
assigns Key and New_Item to the matching node. Any exception raised during
assignment is propagated.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[This is equivalent to:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{declare}
  C : Cursor := Find (Container, Key);
@key{begin}
  @key{if} C = No_Element @key{then}
     Insert (Container, Key, New_Item);
  @key{else}
     Replace (Container, Key, New_Item);
  @key{end if};
@key{end};]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[but this avoids doing the search twice.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace (Container : @key{in out} Map;
                   Key       : @key{in}     Key_Type;
                   New_Item  : @key{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Replace checks if a node with a key
equivalent to Key is present in Container. If a match is found, Replace assigns
Key and New_Item to the matching node; otherwise, Constraint_Error is
propagated.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We update the key as well as the
  element, as the key might include additional information that does not
  participate in equivalence. If only the element needs to be updated, use
  Replace_Element (Find (Container, Key), New_Element).]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Exclude (Container : @key{in out} Map;
                   Key       : @key{in}     Key_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Exclude checks if a node with a key
equivalent to Key is present in Container. If a match is found, Exclude removes
the node from the map.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Exclude should work on an empty map; nothing
  happens in that case.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Delete checks if a node with a key
equivalent to Key is present in Container. If a match is found, Delete removes
the node from the map; otherwise, Constraint_Error is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} Map;
                  Position  : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Constraint_Error is propagated. If Position does not designate an element in
Container, then Program_Error is propagated. Otherwise, Delete removes the node
designated by Position from the map. Position is set to No_Element on return.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The check on Position checks that the cursor does
  not belong to some other map. This check implies that a reference to the map
  is included in the cursor value. This wording is not meant to require
  detection of dangling cursors; such cursors are defined to be invalid, which
  means that execution is erroneous, and any result is allowed (including not
  raising an exception).]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First (Container : Map) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Length (Container) = 0, then
First returns No_Element. Otherwise, First returns a cursor that designates the
first node in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Next (Position  : Cursor) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a cursor that designates
the successor of the node designated by Position. If Position designates the
last node, then No_Element is returned. If Position equals No_Element, then
No_Element is returned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Next (Position  : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Next (Position).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Find (Container : Map;
               Key       : Key_Type) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Length (Container) equals 0,
then Find returns No_Element. Otherwise, Find checks if a node with a key
equivalent to Key is present in Container. If a match is found, a cursor
designating the matching node is returned; otherwise, No_Element is returned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Element (Container : Map;
                  Key       : Key_Type) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Find (Container, Key)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Contains (Container : Map;
                   Key       : Key_Type) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Find (Container, Key) /= No_Element.]}

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
@ChgAdded{Version=[3],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 72 and 73
were moved above.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Iterate
  (Container : @key{in} Map;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Iterate calls Process.@key{all}
with a cursor that designates each node in Container, starting with the first
node and moving the cursor according to the successor relation.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the cursors of Container@Chg{Version=[3],New=[ is prohibited during the
execution of Process.@key{all}],Old=[]}. Any exception raised by
Process.@key{all} is propagated.]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The @lquotes@;tamper with cursors@rquotes@;
  check takes place when the operations that insert or delete elements, and
  so on, are called.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[See Iterate for vectors
  (@RefSecNum{The Generic Package Containers.Vectors}) for a suggested
  implementation of the check.]}
@end{ImplNote}

@end{DescribeCode}
@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0022-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error for the actual function
associated with a generic formal subprogram, when called as part of an
operation of a map package, to tamper with elements of any map parameter of
the operation. Either Program_Error is raised, or the operation works as
defined on the value of the map either prior to, or subsequent to, some or
all of the modifications to the map.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0027-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to call any subprogram
declared in the visible part of a map package
when the associated container has been finalized. If the operation takes
Container as an @key[in out] parameter, then it raises Constraint_Error or
Program_Error. Otherwise, the operation either proceeds as it would
for an empty container, or it raises Constraint_Error or Program_Error.]}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[
A Cursor value is @i{invalid} if any of the following have occurred since it was
created:@Defn2{Term=[invalid cursor],Sec=[of a map]}
@PDefn2{Term=[cursor],Sec=[invalid]}]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The map that contains the node it designates has
been finalized;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0160-1]}
@ChgAdded{Version=[3],Text=[The map that contains the node it designates has
been used as the Target of a call to Assign, or as the target of an
@nt{assignment_statement};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The map that contains the node it designates has
been used as the Source or Target of a call to Move; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0160-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[The node it designates has been
@Chg{Version=[3],New=[removed],Old=[deleted]} from the map@Chg{Version=[3],New=[
that previously contained the node],Old=[]}.]}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
  @ChgAdded{Version=[3],Text=[This can happen directly via calls to Clear,
  Exclude, and Delete.]}
@end{Ramification}
@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The result of "=" or Has_Element is unspecified if
these functions are called with an invalid cursor parameter.@PDefn{unspecified}
Execution is erroneous if any other subprogram declared in
Containers.Hashed_Maps or Containers.Ordered_Maps is called with an invalid
cursor parameter.@PDefn2{Term=(erroneous execution),Sec=(cause)}]}

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
@ChgAdded{Version=[3],Text=[Execution is erroneous if the map associated with
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
  finalization of the object of Reference_Type will try to access a non-existent
  object. This is a normal case of a dangling pointer created by
  Unchecked_Deallocation; we have to explicitly mention it here as the pointer
  in question is not visible in the specification of the type. (This is the same
  reason we have to say this for invalid cursors.)]}
@end{Reason}
@end{Erron}

@begin{ImplReq}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[No storage associated with a Map object shall be
lost upon assignment or scope exit.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[The execution of an @nt{assignment_statement} for
a map shall have the effect of copying the elements from the source map
object to the target map object@Chg{Version=[3],New=[ and changing the length
of the target object to that of the source object],Old=[]}.]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[An assignment of a Map is a @lquotes@;deep@rquotes
  copy; that is the elements are copied as well as the data structures.
  We say @lquotes@;effect of@rquotes in order to allow the implementation to
  avoid copying elements immediately if it wishes. For instance, an
  implementation that avoided copying until one of the containers is modified
  would be allowed.]}
@end{ImplNote}
@end{ImplReq}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Move should not copy elements, and should minimize
copying of internal data structures.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Move for a map should not copy elements, and should
minimize copying of internal data structures.]}]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Usually that can be accomplished simply by
  moving the pointer(s) to the internal data structures from the Source
  container to the Target container.]}
@end{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[If an exception is propagated from a map
operation, no storage should be lost, nor any elements removed from a map
unless specified by the operation.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[If an exception is propagated from a map
operation, no storage should be lost, nor any elements removed from a map
unless specified by the operation.]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is important so that programs can recover
  from errors. But we don't want to require heroic efforts, so we just require
  documentation of cases where this can't be accomplished.]}
@end{Reason}

@end{ImplAdvice}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[This description of maps is new; the
  extensions are documented with the specific packages.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Added reference support to make map containers more
  convenient to use.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[Added procedure Assign;
  the extension and incompatibility is documented with the specific packages.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[Generalized the definition
  of Move. Specified which elements are read/written by stream attributes.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0022-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a @BoundedName
  to cover tampering by generic actual subprograms.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0027-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a @BoundedName
  to cover access to finalized map containers.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Revised the definition
  of invalid cursors to cover missing (and new) cases.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Defined when a container
  prohibits tampering in order to more clearly define where the check is
  made and the exception raised.]}
@end{DiffWord2005}


@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Hashed_Maps]}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
package Containers.Hashed_Maps has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0084-1],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[@key[with] Ada.Iterator_Interfaces;
],Old=[]}@key{generic}
   @key{type} Key_Type @key{is private};
   @key{type} Element_Type @key{is private};
   @key{with function} Hash (Key : Key_Type) @key{return} Hash_Type;
   @key{with function} Equivalent_Keys (Left, Right : Key_Type)
      @key{return} Boolean;
   @key{with function} "=" (Left, Right : Element_Type)
      @key{return} Boolean @Chg{Version=[3],New=[@b<is>],Old=[is]} <>;
@key{package} Ada.Containers.Hashed_Maps @key{is}@ChildUnit{Parent=[Ada.Containers],Child=[Hashed_Maps]}
   @key{pragma} Preelaborate(Hashed_Maps);@Chg{Version=[3],New=[
   @key{pragma} Remote_Types(Hashed_Maps);],Old=[]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Map} @key{is tagged private}@Chg{Version=[3],New=[
      @key[with] Constant_Indexing => Constant_Reference,
           Variable_Indexing => Reference,
           Default_Iterator  => Iterate,
           Iterator_Element  => Element_Type],Old=[]};
   @key{pragma} Preelaborable_Initialization(Map);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Cursor} @key{is private};
   @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Empty_Map} : @key{constant} Map;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[package] @AdaPackDefn{Map_Iterator_Interfaces} @key[is new]
       Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "=" (Left, Right : Map) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Capacity} (Container : Map) @key{return} Count_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reserve_Capacity} (Container : @key{in out} Map;
                               Capacity  : @key{in}     Count_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Length} (Container : Map) @key{return} Count_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Empty} (Container : Map) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Clear} (Container : @key{in out} Map);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Key} (Position : Cursor) @key{return} Key_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Position : Cursor) @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} Map;
                              Position  : @key{in}     Cursor;
                              New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Key     : @key{in} Key_Type;
                                            Element : @key{in} Element_Type));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Update_Element}
     (Container : @key{in out} Map;
      Position  : @key{in}     Cursor;
      Process   : @key{not null access procedure}
                      (Key     : @key{in}     Key_Type;
                       Element : @key{in out} Element_Type));]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[type] Constant_Reference_Type
         (Element : @key[not null access constant] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Reference_Type} (Element : @key[not null access] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Map;
                                Position  : @key[in] Cursor)
      @key[return] Constant_Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] Map;
                       Position  : @key[in] Cursor)
      @key[return] Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Map;
                                Key       : @key[in] Key_Type)
      @key[return] Constant_Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] Map;
                       Key       : @key[in] Key_Type)
      @key[return] Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Assign} (Target : @key[in out] Map; Source : @key[in] Map);]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Copy} (Source : Map; Capacity : Count_Type := 0) @key[return] Map;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Move} (Target : @key{in out} Map;
                   Source : @key{in out} Map);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Include} (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type;
                      New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace} (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type;
                      New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Exclude} (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Map;
                     Position  : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First} (Container : Map)
      @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Next} (Position  : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Next} (Position  : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Find} (Container : Map;
                  Key       : Key_Type)
      @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Container : Map;
                     Key       : Key_Type)
      @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Contains} (Container : Map;
                      Key       : Key_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Keys} (Left, Right : Cursor)
      @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Keys} (Left  : Cursor;
                             Right : Key_Type)
      @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Keys} (Left  : Key_Type;
                             Right : Cursor)
      @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Iterate}
     (Container : @key{in} Map;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] Iterate (Container : @key[in] Map)
      @key[return] Map_Iterator_Interfaces.Forward_Iterator'Class;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{private}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   ... -- @RI[not specified by the language]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{end} Ada.Containers.Hashed_Maps;]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[An object of type Map contains an expandable hash
table, which is used to provide direct access to nodes. The @i<capacity> of an
object of type Map is the maximum number of nodes that can be inserted into the
hash table prior to it being automatically
expanded.@Defn2{Term=[capacity],Sec=[of a hashed map]}]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The expected implementation for a Map uses a
  hash table which is grown when it is too small, with linked lists hanging off
  of each bucket. Note that in that implementation a cursor needs a back
  pointer to the Map object to implement iteration; that could either be in the
  nodes, or in the cursor object. To provide an average @i{O}(1) access time,
  capacity would typically equal the number of buckets in such an
  implementation, so that the average bucket linked list length would be no
  more than 1.0.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[There is no defined relationship between elements
  in a hashed map. Typically, iteration will return elements in the order that
  they are hashed in.]}
@end{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn2{Term={equivalent key},Sec={of a hashed map}}
Two keys @i<K1> and @i<K2> are defined to be @i<equivalent> if
Equivalent_Keys (@i<K1>, @i<K2>) returns True.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
Hash is expected to return the same value each time it is called with a
particular key value. For any two equivalent key values, the actual for Hash is
expected to return the same value. If the actual for Hash behaves in some other
manner, the behavior of this package is unspecified. Which subprograms of this
package call Hash, and how many times they call it, is
unspecified.@PDefn{unspecified}]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The implementation is not required to protect
  against Hash raising an exception, or returning random numbers, or any other
  @lquotes@;bad@rquotes behavior. It's not practical to do so, and a broken
  Hash function makes the container unusable.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The implementation can call Hash whenever it is
  needed; we don't want to specify how often that happens. The result must
  remain the same (this is logically a pure function), or the behavior is
  unspecified.]}
@end{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
Equivalent_Keys on Key_Type values is expected to return the same value each
time it is called with a particular pair of key values. It should define an
equivalence relationship, that is, be reflexive, symmetric, and transitive. If
the actual for Equivalent_Keys behaves in some other manner, the behavior of
this package is unspecified. Which subprograms of this package call
Equivalent_Keys, and how many times they call it, is
unspecified.@PDefn{unspecified}]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[As with Hash, the implementation is not required
  to protect against Equivalent_Keys raising an exception or returning random
  results. Similarly, the implementation can call this operation whenever it is
  needed. The result must remain the same (this is a logically pure function),
  or the behavior is unspecified.]}
@end{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[If the value of a key stored in a node of a map is
changed other than by an operation in this package such that at least one of
Hash or Equivalent_Keys give different results, the behavior of this package is
unspecified.@PDefn{unspecified}]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The implementation is not required to protect
  against changes to key values other than via the operations declared in the
  Hashed_Maps package.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[To see how this could happen,
  imagine an instance of Hashed_Maps where the key type is an
  access-to-variable type and Hash returns a value derived from the components
  of the designated object. Then, any operation that has a key value could
  modify those components and change the hash value:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Key (Map).Some_Component := New_Value;]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is really a design error on the part of the
  user of the map; it shouldn't be possible to modify keys stored in a map. But
  we can't prevent this error anymore than we can prevent someone passing as
  Hash a random number generator.]}
@end{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[first node],Sec=[of a hashed map]}
@Defn2{Term=[last node],Sec=[of a hashed map]}
@Defn2{Term=[successor node],Sec=[of a hashed map]}Which nodes are the first node and the last node of a map, and which node is the
successor of a given node, are unspecified, other than the general semantics
described in @RefSecNum{Maps}.@PDefn{unspecified}]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Typically the first node will be the first node
  in the first bucket, the last node will be the last node in the last bucket,
  and the successor will be obtained by following the collision list, and going
  to the next bucket at the end of each bucket.]}
@end{ImplNote}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Capacity (Container : Map) @key{return} Count_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the capacity of Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reserve_Capacity (Container : @key{in out} Map;
                            Capacity  : @key{in}     Count_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Reserve_Capacity allocates a new hash table such
that the length of the resulting map can become at least the value Capacity
without requiring an additional call to Reserve_Capacity, and is large enough
to hold the current length of Container. Reserve_Capacity then rehashes the
nodes in Container onto the new hash table. It replaces the old hash table with
the new hash table, and then deallocates the old hash table. Any exception
raised during allocation is propagated and Container is not modified.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Reserve_Capacity tampers with the
cursors of Container.]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This routine is used to preallocate the internal
  hash table to the specified capacity such that future Inserts do not require
  expansion of the hash table. Therefore, the implementation should allocate
  the needed memory to make that true at this point, even though the visible
  semantics could be preserved by waiting until enough elements are inserted.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[While Reserve_Capacity can be used to reduce the
  capacity of a map, we do not specify whether an implementation actually
  supports reduction of the capacity. Since the actual capacity can be anything
  greater than or equal to @Chg{Version=[3],New=[Capacity],Old=[Count]},
  an implementation never has to reduce the capacity.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Reserve_Capacity tampers with the cursors, as
  rehashing probably will change the order that elements are stored in the
  map.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Clear (Container : @key{in out} Map);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[In addition to the semantics
described in @RefSecNum{Maps}, Clear does not affect the capacity of
Container.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[procedure] Assign (Target : @key[in out] Map; Source : @key[in] Map);]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[In addition to the semantics
described in @RefSecNum{Maps}, if the length of Source is greater than the
capacity of Target,
Reserve_Capacity (Target, Length (Source)) is called before assigning
any elements.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Copy (Source : Map; Capacity : Count_Type := 0) @key[return] Map;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a map whose keys and
elements are initialized from the keys and elements of Source. If Capacity is 0,
then the map capacity is the length of Source; if Capacity is equal to or
greater than the length of Source, the map capacity is at least the specified
value. Otherwise, the operation propagates Capacity_Error.]}

@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[In:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Move (Target : @key{in out} Map;
                Source : @key{in out} Map);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The intended implementation is that the internal
hash table of Target is first deallocated; then the internal hash table is
removed from Source and moved to Target.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[In addition to the semantics
described in @RefSecNum{Maps}, if Length (Container) equals Capacity
(Container), then Insert first calls Reserve_Capacity to increase the capacity
of Container to some larger value.]}

@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Insert should only compare keys that hash to the
same bucket in the hash table.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[We specify when Reserve_Capacity is called to bound
the overhead of capacity expansion operations (which are potentially
expensive). Moreover, expansion can be predicted by comparing Capacity(Map) to
Length(Map). Since we don't specify by how much the hash table is expanded,
this only can be used to predict the next expansion, not later ones.]}

@end{ImplNote}
@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[In:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Exclude (Container : @key{in out} Map;
                   Key       : @key{in}     Key_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Exclude should only compare keys that hash to the same bucket in the hash
table.]}

@end{ImplNote}
@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[In:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Delete should only compare keys that hash to the
same bucket in the hash table. The node containing the element may be
deallocated now, or it may be saved and reused later.]}

@end{ImplNote}
@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[In:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First (Container : Map) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[In a typical implementation, this will be the first node in the lowest numbered
hash bucket that contains a node.]}

@end{ImplNote}
@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[In:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Next (Position  : Cursor) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[In a typical implementation, this will return the next node in a bucket; if
Position is the last node in a bucket, this will return the first node in the
next non-empty bucket.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A typical implementation will need to a keep a pointer at the map container
in the cursor in order to implement this function.]}
@end{ImplNote}
@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[In:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Find (Container : Map;
               Key       : Key_Type) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Find should only compare keys that hash to the same bucket in the hash table.]}

@end{ImplNote}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Equivalent_Keys (Left, Right : Cursor)
      @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Equivalent_Keys (Key
(Left), Key (Right)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Equivalent_Keys (Left  : Cursor;
                          Right : Key_Type) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Equivalent_Keys (Key
(Left), Right).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Equivalent_Keys (Left  : Key_Type;
                          Right : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Equivalent_Keys
(Left, Key (Right)).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] Map)
   @key[return] Map_Iterator_Interfaces.Forward_Iterator'Class;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate returns an
iterator object that will generate a value for the loop parameter designating
each node in Container, starting with the first node and moving the cursor
according to the successor relation.
Tampering with the cursors of Container is prohibited while
the iterator object exists (in particular, in
the @nt{sequence_of_statements} of the @nt{loop_statement} whose
@nt{iterator_specification} denotes this object). The iterator object needs
finalization.]}

@end{DescribeCode}
@end{StaticSem}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[If @i<N> is the length of a map, the average time
complexity of the subprograms Element, Insert, Include, Replace, Delete,
Exclude and Find that take a key parameter should be @i{O}(log @i<N>). The average
time complexity of the subprograms that take a cursor parameter should be @i{O}(1).
The average time complexity of Reserve_Capacity should be @i{O}(@i<N>).]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The average time complexity of Element, Insert, Include, Replace,
Delete, Exclude and Find operations that
take a key parameter for Containers.Hashed_Maps should be
@i{O}(log @i<N>). The average
time complexity of the subprograms of Containers.Hashed_Maps that take
a cursor parameter should be @i{O}(1). The average time complexity of
Containers.Hashed_Maps.Reserve_Capacity should be @i{O}(@i<N>).]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We do not mean to overly constrain implementation
  strategies here. However, it is important for portability that the
  performance of large containers has roughly the same factors on different
  implementations. If a program is moved to an implementation for which Find is
  @i{O}(@i<N>), that program could be unusable when the maps are large. We allow
  @i{O}(log @i<N>) access because the proportionality constant and caching effects
  are likely to be larger than the log factor, and we don't want to discourage
  innovative implementations.]}
@end{Reason}
@end{ImplAdvice}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The generic package Containers.Hashed_Maps is new.]}
@end{Extend95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}Subprograms Assign and Copy
  are newly added to Containers.Hashed_Maps. If an instance of Containers.Hashed_Maps
  is referenced in a @nt{use_clause}, and an entity @i<E> with the same
  @nt{defining_identifier} as a new entity in Containers.Hashed_Maps is
  defined in a package that is also referenced in a @nt{use_clause}, the
  entity @i<E> may no longer be use-visible, resulting in errors. This should
  be rare and is easily fixed if it does occur.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Added iterator and indexing support to make hashed map containers more
  convenient to use.]}
@end{Extend2005}

@begin{Diffword2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0084-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a pragma
  Remote_Types so that containers can be used in distributed programs.]}
@end{Diffword2005}


@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Ordered_Maps]}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
package Containers.Ordered_Maps has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0084-1],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[@key[with] Ada.Iterator_Interfaces;
],Old=[]}@key{generic}
   @key{type} Key_Type @key{is private};
   @key{type} Element_Type @key{is private};
   @key{with function} "<" (Left, Right : Key_Type) @key{return} Boolean @key{is} <>;
   @key{with function} "=" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Ordered_Maps @key{is}@ChildUnit{Parent=[Ada.Containers],Child=[Ordered_Maps]}
   @key{pragma} Preelaborate(Ordered_Maps);@Chg{Version=[3],New=[
   @key{pragma} Remote_Types(Ordered_Maps);],Old=[]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Keys} (Left, Right : Key_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Map} @key{is tagged private}@Chg{Version=[3],New=[
      @key[with] Constant_Indexing => Constant_Reference,
           Variable_Indexing => Reference,
           Default_Iterator  => Iterate,
           Iterator_Element  => Element_Type],Old=[]};
   @key{pragma} Preelaborable_Initialization(Map);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Cursor} @key{is private};
   @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Empty_Map} : @key{constant} Map;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[package] @AdaPackDefn{Map_Iterator_Interfaces} @key[is new]
       Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "=" (Left, Right : Map) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Length} (Container : Map) @key{return} Count_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Empty} (Container : Map) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Clear} (Container : @key{in out} Map);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Key} (Position : Cursor) @key{return} Key_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Position : Cursor) @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} Map;
                              Position  : @key{in}     Cursor;
                              New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Key     : @key{in} Key_Type;
                                            Element : @key{in} Element_Type));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Update_Element}
     (Container : @key{in out} Map;
      Position  : @key{in}     Cursor;
      Process   : @key{not null access procedure}
                      (Key     : @key{in}     Key_Type;
                       Element : @key{in out} Element_Type));]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[type] Constant_Reference_Type
         (Element : @key[not null access constant] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Reference_Type} (Element : @key[not null access] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Map;
                                Position  : @key[in] Cursor)
      @key[return] Constant_Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] Map;
                       Position  : @key[in] Cursor)
      @key[return] Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Map;
                                Key       : @key[in] Key_Type)
      @key[return] Constant_Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] Map;
                       Key       : @key[in] Key_Type)
      @key[return] Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Assign} (Target : @key[in out] Map; Source : @key[in] Map);]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Copy} (Source : Map) @key[return] Map;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Move} (Target : @key{in out} Map;
                   Source : @key{in out} Map);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Include} (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type;
                      New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace} (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type;
                      New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Exclude} (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Map;
                     Position  : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_First} (Container : @key{in out} Map);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_Last} (Container : @key{in out} Map);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First} (Container : Map) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First_Element} (Container : Map) @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First_Key} (Container : Map) @key{return} Key_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last} (Container : Map) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last_Element} (Container : Map) @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last_Key} (Container : Map) @key{return} Key_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Next} (Position : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Next} (Position : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Previous} (Position : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Previous} (Position : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Find} (Container : Map;
                  Key       : Key_Type) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Container : Map;
                     Key       : Key_Type) @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Floor} (Container : Map;
                   Key       : Key_Type) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Ceiling} (Container : Map;
                     Key       : Key_Type) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Contains} (Container : Map;
                      Key       : Key_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "<" (Left, Right : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} ">" (Left, Right : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "<" (Left : Cursor; Right : Key_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} ">" (Left : Cursor; Right : Key_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "<" (Left : Key_Type; Right : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} ">" (Left : Key_Type; Right : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Iterate}
     (Container : @key{in} Map;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reverse_Iterate}
     (Container : @key{in} Map;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] Iterate (Container : @key[in] Map)
      @key[return] Map_Iterator_Interfaces.Reversible_Iterator'Class;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{private}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   ... -- @RI[not specified by the language]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{end} Ada.Containers.Ordered_Maps;]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn2{Term={equivalent key},Sec={of an ordered map}}
Two keys @i<K1> and @i<K2> are @i<equivalent> if both @i<K1> < @i<K2> and
@i<K2> < @i<K1> return False, using the generic formal "<" operator for keys.
Function Equivalent_Keys returns True if Left and Right are equivalent, and
False otherwise.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0044-1]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
"<" on Key_Type values is expected to return the same value each time it is
called with a particular pair of key values. It should define a strict
@Chg{Version=[3],New=[weak ],Old=[]}ordering relationship@Chg{Version=[3],
New=[ (see @RefSecNum{Containers})],Old=[, that is, be irreflexive, asymmetric,
and transitive]}. If the
actual for "<" behaves in some other manner, the behavior of this package is
unspecified. Which subprograms of this package call "<" and how many times they
call it, is unspecified.@PDefn{unspecified}]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The implementation is not required to protect
  against "<" raising an exception, or returning random results, or any
  other @lquotes@;bad@rquotes behavior. It's not practical to do so, and a
  broken "<" function makes the container unusable.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The implementation can call "<" whenever
  it is needed; we don't want to specify how often that happens. The result
  must remain the same (this is a logically pure function), or the behavior is
  unspecified.]}
@end{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[If the value of a key stored in a map is changed
other than by an operation in this package such that at least one of "<" or "="
give different results, the behavior of this package is
unspecified.@PDefn{unspecified}]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The implementation is not required to protect
  against changes to key values other than via the operations declared in the
  Ordered_Maps package.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[To see how this could happen,
  imagine an instance of Ordered_Maps package where the key type is an
  access-to-variable type and "<" returns a value derived from comparing the
  components of the designated objects. Then, any operation that has a key
  value (even if the key value is constant) could modify those components and
  change the result of "<":]}

@begin{example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Key (Map).Some_Component := New_Value;]}
@end{example}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is really a design error on the part of the
  user of the map; it shouldn't be possible to modify keys stored in a map such
  that "<" changes. But we can't prevent this error anymore than we can prevent
  someone passing as "<" a routine that produces random answers.]}
@end{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[first node],Sec=[of an ordered map]}
@Defn2{Term=[last node],Sec=[of an ordered map]}
@Defn2{Term=[successor node],Sec=[of an ordered map]}@Chg{Version=[3],New=[
@Defn2{Term=[predecessor node],Sec=[of an ordered map]}],Old=[]}
The @Chg{Version=[3],New=[@i<first node>],Old=[first node]}
of a nonempty map is the one whose key is less than the key of
all the other nodes in the map. The @Chg{Version=[3],New=[@i<last node>],Old=[last node]}
of a nonempty map is the one
whose key is greater than the key of all the other elements in the map. The
@Chg{Version=[3],New=[@i<successor>],Old=[successor]}
of a node is the node with the smallest key that is larger than the
key of the given node. The @Chg{Version=[3],New=[@i<predecessor>],Old=[predecessor]}
of a node is the node with the largest key that is smaller than the key of the
given node. All comparisons are done using the generic formal "<" operator for
keys.]}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Copy (Source : Map) @key[return] Map;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a map whose keys and
elements are initialized from the corresponding keys and elements of Source.]}


@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_First (Container : @key{in out} Map);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, Delete_First
has no effect. Otherwise@Chg{Version=[3],New=[,],Old=[]}
the node designated by First (Container) is removed
from Container. Delete_First tampers with the cursors of Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_Last (Container : @key{in out} Map);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, Delete_Last
has no effect. Otherwise@Chg{Version=[3],New=[,],Old=[]}
the node designated by Last (Container) is removed
from Container. Delete_Last tampers with the cursors of Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First_Element (Container : Map) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (First (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First_Key (Container : Map) @key{return} Key_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Key (First (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last (Container : Map) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a cursor that designates
the last node in Container. If Container is empty, returns No_Element.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last_Element (Container : Map) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Last (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last_Key (Container : Map) @key{return} Key_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Key (Last (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Previous (Position : Cursor) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Previous returns No_Element. Otherwise@Chg{Version=[3],New=[,],Old=[]}
Previous returns a cursor designating the @Chg{Version=[3],New=[predecessor
],Old=[]}node @Chg{Version=[3],New=[of],Old=[that precedes]} the one designated
by Position. If Position designates the first element, then Previous returns
No_Element.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Previous (Position : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Previous (Position).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Floor (Container : Map;
                Key       : Key_Type) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Floor searches for the last node
whose key is not greater than Key, using the generic formal "<" operator for keys.
If such a node is found, a cursor that designates it is returned.
Otherwise@Chg{Version=[3],New=[,],Old=[]}
No_Element is returned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Ceiling (Container : Map;
                  Key       : Key_Type) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Ceiling searches for the first node
whose key is not less than Key, using the generic formal "<" operator for keys.
If such a node is found, a cursor that designates it is returned.
Otherwise@Chg{Version=[3],New=[,],Old=[]} No_Element is returned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "<" (Left, Right : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Key (Left) < Key (Right).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} ">" (Left, Right : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Key (Right) < Key (Left).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "<" (Left : Cursor; Right : Key_Type) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Key (Left) < Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} ">" (Left : Cursor; Right : Key_Type) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Right < Key (Left).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "<" (Left : Key_Type; Right : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Left < Key (Right).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} ">" (Left : Key_Type; Right : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Key (Right) < Left.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reverse_Iterate
  (Container : @key{in} Map;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Iterates over the nodes in
Container as per @Chg{Version=[3],New=[procedure ],Old=[]}Iterate,
with the difference that the nodes are traversed in
predecessor order, starting with the last node.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] Map)
   @key[return] Map_Iterator_Interfaces.Reversible_Iterator'Class;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate returns a reversible
iterator object that will generate a value for the loop parameter designating
each node in Container, starting with the first node and moving the cursor
according to the successor relation when used as a forward iterator, and
starting with the last node and moving the cursor according to the predecessor
relation when used as a reverse iterator.
Tampering with the cursors of Container is prohibited while
the iterator object exists (in particular, in
the @nt{sequence_of_statements} of the @nt{loop_statement} whose
@nt{iterator_specification} denotes this object). The iterator object needs
finalization.]}

@end{DescribeCode}
@end{StaticSem}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[If @i<N> is the length of a map, then the
worst-case time complexity of the
Element, Insert, Include, Replace, Delete, Exclude and Find operations that
take a key parameter should be @i{O}((log @i<N>)**2) or better. The worst-case
time complexity of the subprograms that take a cursor parameter should be @i{O}(1).]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The worst-case time complexity of Element, Insert, Include, Replace,
Delete, Exclude and Find operations that
take a key parameter for Containers.Ordered_Maps should be
@i{O}((log @i<N>)**2) or better. The worst-case
time complexity of the subprograms of Containers.Ordered_Maps that take
a cursor parameter should be @i{O}(1).]}]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[A balanced (red-black) tree for keys has
  @i{O}(log @i<N>) worst-case performance. Note that a @i{O}(@i<N>) worst-case
  implementation (like a list) would be wrong.]}
@end{ImplNote}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We do not mean to overly constrain implementation
  strategies here. However, it is important for portability that the
  performance of large containers has roughly the same factors on different
  implementations. If a program is moved to an implementation that takes @i{O}(@i<N>)
  to find elements, that program could be unusable when the maps are large. We
  allow the extra log @i<N> factors because the proportionality constant and
  caching effects are likely to be larger than the log factor, and we don't
  want to discourage innovative implementations.]}
@end{Reason}

@end{ImplAdvice}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The generic package Containers.Ordered_Maps is new.]}
@end{Extend95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}Subprograms Assign and Copy
  are newly added to Containers.Ordered_Maps. If an instance of Containers.Ordered_Maps
  is referenced in a @nt{use_clause}, and an entity @i<E> with the same
  @nt{defining_identifier} as a new entity in Containers.Ordered_Maps is
  defined in a package that is also referenced in a @nt{use_clause}, the
  entity @i<E> may no longer be use-visible, resulting in errors. This should
  be rare and is easily fixed if it does occur.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Added iterator and indexing support to make ordered map containers more
  convenient to use.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0044-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Redefined "<" actuals
  to require a strict weak ordering; the old definition allowed
  indeterminant comparisons that would not have worked in a container.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0084-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a pragma
  Remote_Types so that containers can be used in distributed programs.]}
@end{DiffWord2005}


@LabeledAddedSubclause{Version=[2],Name=[Sets]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The language-defined generic packages
Containers.Hashed_Sets and Containers.Ordered_Sets provide private types Set
and Cursor, and a set of operations for each type. A set container allows
elements of an arbitrary type to be stored without duplication. A hashed set
uses a hash function to organize elements, while an ordered set orders its
element per a specified relation.@Defn{set container}
@Defn2{Term=[container],Sec=[set]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[This section describes the declarations that are
common to both kinds of sets.
See @RefSecNum{The Generic Package Containers.Hashed_Sets} for a description of the
semantics specific to
Containers.Hashed_Sets and @RefSecNum{The Generic Package Containers.Ordered_Sets} for
a description of the semantics specific to Containers.Ordered_Sets.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
"=" on Element_Type values is expected to define a reflexive and symmetric
relationship and return the same result value each time it is called with a
particular pair of values. If it behaves in some other manner, the function
"=" on set values returns an unspecified value. The
exact arguments and number of calls of this generic formal function by the
function "=" on set values are unspecified.@PDefn{unspecified}]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If the actual function for "=" is not symmetric
  and consistent, the result returned by the "=" for Set objects cannot be
  predicted. The implementation is not required to protect
  against "=" raising an exception, or returning random results, or any
  other @lquotes@;bad@rquotes behavior. And it can call "=" in whatever
  manner makes sense. But note that only the result of "=" for Set objects
  is unspecified; other subprograms are not allowed to break if "=" is bad
  (they aren't expected to use "=").]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The type Set is used to represent sets. The type
Set needs finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[A set contains elements. Set cursors designate
elements. There exists an equivalence relation on elements, whose definition is
different for hashed sets and ordered sets. A set never contains two or more
equivalent elements. The @i{length} of a set is the number of elements it
contains.@Defn2{Term={length},Sec={of a set}}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[first element],Sec=[of a set]}
@Defn2{Term=[last element],Sec=[of a set]}
@Defn2{Term=[successor element],Sec=[of a set]}
Each nonempty set has two particular elements called the @i{first element} and
the @i{last element} (which may be the same). Each element except for the last
element has a @i{successor element}. If there are no other intervening
operations, starting with the first element and repeatedly going to the
successor element will visit each element in the set exactly once until the
last element is reached. The exact definition of these terms is different for
hashed sets and ordered sets.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Redundant[Some operations of these generic packages
have access-to-subprogram parameters. To ensure such operations are
well-defined, they guard against certain actions by the designated
subprogram. In particular, some operations check for @lquotes@;tampering with
cursors@rquotes of a container because they depend on the set of elements of
the container remaining constant, and others check for @lquotes@;tampering with
elements@rquotes of a container because they depend on elements of the
container not being replaced.]]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Defn2{Term=[tamper with cursors],Sec=[of a set]}
A subprogram is said to @i{tamper with cursors} of a set object @i{S} if:]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it inserts or deletes elements of @i{S}, that is,
it calls the Insert, Include, Clear, Delete, Exclude, or Replace_Element
procedures with @i{S} as a parameter; or]}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Operations which are defined to be equivalent to
  a call on one of these operations also are included. Similarly, operations
  which call one of these as part of their definition are included.]}
@end{Honest}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We have to include Replace_Element here because
  it might delete and reinsert the element if it moves in the set. That could
  change the order of iteration, which is what this check is designed to
  prevent. Replace is also included, as it is defined in terms of
  Replace_Element.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it finalizes @i<S>; or]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[it calls the Assign procedure with @i<S> as the Target parameter;
or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it calls the Move procedure with @i<S> as a
parameter; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it calls one of the operations defined to tamper with cursors of @i<S>.]}

@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Defn2{Term=[tamper with elements],Sec=[of a set]}
A subprogram is said to @i{tamper with elements} of a set
object @i<S> if:]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[it tampers with cursors of @i<S>.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Complete replacement of an element can cause its
  memory to be deallocated while another operation is holding onto a reference
  to it. That can't be allowed. However, a simple modification of (part of) an
  element is not a problem, so Update_@!Element_@!Preserving_@!Key does not cause a
  problem.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We don't need to list Replace and Replace_Element
  here because they are covered by @lquotes@;tamper with cursors@rquotes.
  For Set, @lquotes@;tamper with cursors@rquotes@;
  and @lquotes@;tamper with elements@rquotes are the same. We leave both
  terms so that the rules for routines like Iterate and
  Query_Element are consistent across all containers.]}
@end{Reason}

@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[prohibited],Sec=[tampering with a set]}
@Defn2{Term=[tampering],Sec=[prohibited for a set]}
If tampering with cursors is @i<prohibited> for a particular set
object @i<S>, Program_Error is propagated by any language-defined subprogram
that is defined to tamper with the cursors of @i<S>. Similarly, if tampering with
elements is @i<prohibited> for a particular set object @i<S>,
Program_Error is propagated by any language-defined subprogram that is defined
to tamper with the elements of @i<S>.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Empty_Set represents the empty Set object. It has a
length of 0. If an object of type Set is not otherwise initialized, it is
initialized to the same value as Empty_Set.]}

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
@ChgAdded{Version=[3],Text=[Set'Write writes exactly Length(Set) elements of the
set to the stream. It may write additional information about the set as well.
Set'Read reads exactly Length(Set) elements of Set from the stream and consumes
any additional information written by Set'Write.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Streaming more elements than the container
  length is wrong. For implementation implications of this rule, see the Implementation Note in
  @RefSecNum{The Generic Package Containers.Vectors}.]}
@end{Ramification}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Has_Element (Position : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if Position designates
an element, and returns False otherwise.]}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This function may not detect cursors that
  designate deleted elements; such cursors are invalid (see below) and the
  result of calling Has_Element with an invalid cursor is unspecified (but
  not erroneous).]}
@end{Honest}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "=" (Left, Right : Set) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Left and Right denote the same
set object, then the function returns True. If Left and Right have different
lengths, then the function returns False. Otherwise, for each element @i<E> in
Left, the function returns False if an element equal to @i<E> (using
the generic formal equality operator) is not present in Right. If the function
has not returned a result after checking all of the elements, it returns True.
Any exception raised during evaluation of element equality is propagated.]}
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
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Equivalent_Sets (Left, Right : Set) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Left and Right denote the same
set object, then the function returns True. If Left and Right have different
lengths, then the function returns False. Otherwise, for each element @i<E> in
Left, the function returns False if an element equivalent to @i<E> is not
present in Right. If the function has not returned a result after checking all
of the elements, it returns True. Any exception raised during evaluation of
element equivalence is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} To_Set (New_Item : Element_Type) @key{return} Set;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a set containing the single element New_Item.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Length (Container : Set) @key{return} Count_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the number of elements in
Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Is_Empty (Container : Set) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Length (Container) = 0.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Clear (Container : @key{in out} Set);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Removes all the elements from
Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Element (Position : Cursor) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Constraint_Error is propagated. Otherwise, Element returns the element
designated by Position.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace_Element (Container : @key{in out} Set;
                           Position  : @key{in}     Cursor;
                           New_Item  : @key{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Constraint_Error is propagated; if Position does not designate an element in
Container, then Program_Error is propagated.
If an element equivalent to New_Item is already present in Container at a
position other than Position, Program_Error is propagated. Otherwise,
Replace_Element assigns New_Item to the element designated by Position. Any
exception raised by the assignment is propagated.]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The final assignment may require that the node of
  the element be moved in the Set's data structures. That could mean that
  implementing this operation exactly as worded above could require the
  overhead of searching twice. Implementations are encouraged to avoid this
  extra overhead when possible, by prechecking if the old element is equivalent
  to the new one, by inserting a placeholder node while checking for an
  equivalent element, and similar optimizations.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The cursor still designates the same element
  after this operation; only the value of that element has changed. Cursors
  cannot include information about the relative position of an element in a
  Set (as they must survive insertions and deletions of other elements), so
  this should not pose an implementation hardship.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Query_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access procedure} (Element : @key{in} Element_Type));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0021-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Constraint_Error is propagated. Otherwise, Query_Element calls
Process.@key{all} with the element designated by Position as the argument.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of @Chg{Version=[3],New=[the set that contains the
element designated by Position is prohibited during the
execution of Process.@key{all}],Old=[Container]}. Any exception raised by
Process.@key{all} is propagated.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key[type] Constant_Reference_Type
      (Element : @key[not null access constant] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[The type Constant_Reference_Type
needs finalization.@PDefn2{Term=<needs finalization>,
Sec=<language-defined type>}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The default initialization of an object of type
Constant_Reference_Type propagates Program_Error.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[It is expected that
  Constant_Reference_Type will be a controlled type, for which finalization
  will have some action to terminate the tampering check for the associated
  container. If the object is created by default, however, there is no
  associated container. Since this is useless, and supporting this case would
  take extra work, we define it to raise an exception.]}
@end{Reason}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] Set;
                             Position  : @key[in] Cursor)
   @key[return] Constant_Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read access to the individual elements of a container starting with a
cursor.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[If Position equals No_Element, then Constraint_Error
is propagated; if Position does not designate an element in Container, then
Program_Error is propagated. Otherwise, Constant_Reference returns an object
whose discriminant is an access value that designates the element designated by
Position. Tampering with the elements of Container is prohibited while the
object returned by Constant_Reference exists and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Assign (Target : @key{in out} Set; Source : @key{in} Set);]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Target denotes the same object as Source, the operation has no
effect. Otherwise, the elements of Source are copied to Target as
for an @nt{assignment_statement} assigning Source to Target.]}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[3],Text=[This routine exists for compatibility with the
  bounded set containers. For an unbounded set, @exam{Assign(A, B)} and
  @exam{A := B} behave identically. For a bounded set, := will raise an
  exception if the container capacities are different, while Assign will
  not raise an exception if there is enough room in the target.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Move (Target : @key{in out} Set;
                Source : @key{in out} Set);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0001-1],ARef=[AI05-0248-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Target denotes the same object
as Source, then @Chg{Version=[3],New=[the operation],Old=[Move]}
has no effect. Otherwise, @Chg{Version=[3],New=[the operation is equivalent
to Assign (Target, Source) followed by Clear (Source)],Old=[Move first clears
Target. Then, each element from Source is removed from Source and inserted into
Target. The length of Source is 0 after a successful call to Move]}.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Set;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Insert checks if an element
equivalent to New_Item is already present in Container. If a match is found,
Inserted is set to False and Position designates the matching element.
Otherwise, Insert adds New_Item to Container; Inserted is set to True and
Position designates the newly-inserted element. Any exception raised during
allocation is propagated and Container is not modified.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Set;
                  New_Item  : @key{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Insert inserts New_Item into
Container as per the four-parameter Insert, with the difference that if an
element equivalent to New_Item is already in the set, then Constraint_Error is
propagated.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[This is equivalent to:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{declare}
  Inserted : Boolean; C : Cursor;
@key{begin}
  Insert (Container, New_Item, C, Inserted);
  @key{if not} Inserted @key{then}
     @key{raise} Constraint_Error;
  @key{end if};
@key{end};]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[but doesn't require the hassle of @key{out}
  parameters.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Include (Container : @key{in out} Set;
                   New_Item  : @key{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Include inserts New_Item into
Container as per the four-parameter Insert, with the difference that if an
element equivalent to New_Item is already in the set, then it is replaced. Any
exception raised during assignment is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace (Container : @key{in out} Set;
                   New_Item  : @key{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Replace checks if an element
equivalent to New_Item is already in the set. If a match is found, that element
is replaced with New_Item; otherwise, Constraint_Error is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Exclude (Container : @key{in out} Set;
                   Item      : @key{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Exclude checks if an element
equivalent to Item is present in Container. If a match is found, Exclude
removes the element from the set.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} Set;
                  Item      : @key{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Delete checks if an element
equivalent to Item is present in Container. If a match is found, Delete removes
the element from the set; otherwise, Constraint_Error is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} Set;
                  Position  : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element,
then Constraint_Error is propagated. If Position does not designate an element
in Container, then Program_Error is propagated. Otherwise, Delete removes the
element designated by Position from the set. Position is set to No_Element on
return.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The check on Position checks that the cursor does
  not belong to some other set. This check implies that a reference to the set
  is included in the cursor value. This wording is not meant to require
  detection of dangling cursors; such cursors are defined to be invalid, which
  means that execution is erroneous, and any result is allowed (including not
  raising an exception).]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Union (Target : @key{in out} Set;
                 Source : @key{in}     Set);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Union inserts into Target the
elements of Source that are not equivalent to some element already in Target.]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If the objects are the same, the result is the
  same as the original object. The implementation needs to take care so that
  aliasing effects do not make the result trash; Union (S, S); must work.]}
@end{ImplNote}


@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Union (Left, Right : Set) @key{return} Set;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a set comprising all of the
elements of Left, and the elements of Right that are not equivalent to some
element of Left.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Intersection (Target : @key{in out} Set;
                        Source : @key{in}     Set);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0004-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[3],New=[Intersection],Old=[Union]} deletes from Target the
elements of Target that are not equivalent to some element of Source.]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If the objects are the same, the result is the
  same as the original object. The implementation needs to take care so that
  aliasing effects do not make the result trash; Intersection (S, S); must
  work.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Intersection (Left, Right : Set) @key{return} Set;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a set comprising all the
elements of Left that are equivalent to the some element of Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Difference (Target : @key{in out} Set;
                      Source : @key{in}     Set);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Target denotes the same object
as Source, then Difference clears Target. Otherwise, it deletes from Target the
elements that are equivalent to some element of Source.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Difference (Left, Right : Set) @key{return} Set;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a set comprising the
elements of Left that are not equivalent to some element of Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Symmetric_Difference (Target : @key{in out} Set;
                                Source : @key{in}     Set);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Target denotes the same object
as Source, then Symmetric_Difference clears Target. Otherwise, it deletes from
Target the elements that are equivalent to some element of Source, and inserts
into Target the elements of Source that are not equivalent to some element of
Target.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Symmetric_Difference (Left, Right : Set) @key{return} Set;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a set comprising the
elements of Left that are not equivalent to some element of Right, and the
elements of Right that are not equivalent to some element of Left.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Overlap (Left, Right : Set) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If an element of Left is equivalent
to some element of Right, then Overlap returns True.
Otherwise@Chg{Version=[3],New=[,],Old=[]} it returns False.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This operation is commutative. If Overlap returns
  False, the two sets are disjoint.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Is_Subset (Subset : Set;
                    Of_Set : Set) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If an element of Subset is not
equivalent to some element of Of_Set, then Is_Subset returns False.
Otherwise@Chg{Version=[3],New=[,],Old=[]} it returns True.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This operation is not commutative, so we use
  parameter names that make it clear in named notation which set is which.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First (Container : Set) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Length (Container) = 0, then
First returns No_Element. Otherwise, First returns a cursor that designates the
first element in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Next (Position  : Cursor) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a cursor that designates the successor of the element
designated by Position. If Position designates the last element, then
No_Element is returned. If Position equals No_Element, then No_Element is
returned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Next (Position  : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Next (Position).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0004-1]}
@ChgDeleted{Version=[3],Type=[Trailing],Text=[@Chg{Version=[2],New=[Equivalent to Find (Container, Item) /= No_Element.],Old=[]}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Find (Container : Set;
               Item      : Element_Type) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Length (Container) equals 0,
then Find returns No_Element. Otherwise, Find checks if an element equivalent
to Item is present in Container. If a match is found, a cursor designating the
matching element is returned; otherwise, No_Element is returned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Contains (Container : Set;
                   Item      : Element_Type) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0004-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to Find (Container, Item) /= No_Element.]}

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
@ChgAdded{Version=[3],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 83 and 84
were moved above.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Iterate
  (Container : @key{in} Set;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Iterate calls Process.@key{all}
with a cursor that designates each element in Container, starting with the
first element and moving the cursor according to the successor relation.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the cursors of Container@Chg{Version=[3],New=[ is prohibited during the
execution of Process.@key{all}],Old=[]}. Any exception raised by
Process.@key{all} is propagated.]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The @lquotes@;tamper with cursors@rquotes@;
  check takes place when the operations that insert or delete elements, and
  so on are called.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[See Iterate for vectors
  (@RefSecNum{The Generic Package Containers.Vectors}) for a suggested
  implementation of the check.]}
@end{ImplNote}

@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Both Containers.Hashed_Set and
Containers.Ordered_Set declare a nested generic package Generic_Keys, which
provides operations that allow set manipulation in terms of a key (typically, a
portion of an element) instead of a complete element. The formal function Key
of Generic_Keys extracts a key value from an element. It is expected to return
the same value each time it is called with a particular element. The behavior
of Generic_Keys is unspecified if Key behaves in some other
manner.@PDefn{unspecified}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[A key is expected to unambiguously determine a
single equivalence class for elements. The behavior of Generic_Keys is
unspecified if the formal parameters of this package behave in some other
manner.@PDefn{unspecified}]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Key (Position : Cursor) @key{return} Key_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Key (Element (Position)).]}
@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The subprograms in package Generic_Keys named
Contains, Find, Element, Delete, and Exclude, are equivalent to the
corresponding subprograms in the parent package, with the difference that the
Key parameter is used to locate an element in the set.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace (Container : @key{in out} Set;
                   Key       : @key{in}     Key_Type;
                   New_Item  : @key{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Replace_Element (Container, Find (Container, Key), New_Item).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Update_Element_Preserving_Key
  (Container : @key{in out} Set;
   Position  : @key{in}     Cursor;
   Process   : @key{not null access procedure}
                                 (Element : @key{in out} Element_Type));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Text=[If Position equals No_Element, then
Constraint_Error is propagated; if Position does not designate an element in
Container, then Program_Error is propagated. Otherwise,
Update_@!Element_@!Preserving_Key uses Key to save the key value @i<K> of the
element designated by Position. Update_@!Element_@!Preserving_Key then calls
Process.@key{all} with that element as the argument.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of Container@Chg{Version=[3],New=[ is prohibited during the
execution of Process.@key{all}],Old=[]}. Any
exception raised by Process.@key{all} is propagated. After Process.@key{all}
returns, Update_@!Element_@!Preserving_Key checks if @i<K> determines the same
equivalence class as that for the new element; if not, the element is removed
from the set and Program_Error is propagated.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The key check ensures that the invariants of
  the set are preserved by the modification. The @lquotes@;tampers with
  the elements@rquotes@; check prevents data loss (if Element_Type is by-copy)
  or erroneous execution (if element type is unconstrained and indefinite).]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Element_Type is unconstrained
and definite, then the actual Element parameter of Process.@key{all} shall be
unconstrained.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This means that the elements cannot be
  directly allocated from the heap; it must be possible to change the
  discriminants of the element in place.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[type] Reference_Type (Element : @key[not null access] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[The type Reference_Type needs
finalization.@PDefn2{Term=<needs finalization>,
Sec=<language-defined type>}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The default initialization of an object of type
Reference_Type propagates Program_Error.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference_Preserving_Key (Container : @key[aliased in out] Set;
                                   Position  : @key[in] Cursor)
   @key[return] Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Implicit_Dereference aspect) provides a convenient way to
gain read and write access to the individual elements of a container starting
with a cursor.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[If Position equals No_Element, then Constraint_Error
is propagated; if Position does not designate an element in Container, then
Program_Error is propagated. Otherwise, Reference_Preserving_Key uses Key to
save the key value @i<K>; then returns an object whose discriminant is an access
value that designates the element designated by
Position. Tampering with the elements of Container is prohibited while the
object returned by Reference_Preserving_Key exists and has not been finalized.
When the object returned by Reference_Preserving_Key is finalized, a check is
made if @i<K> determines the same equivalence class as that for the new element;
if not, the element is removed from the set and Program_Error is propagated.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] Set;
                             Key       : @key[in] Key_Type)
   @key[return] Constant_Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Implicit_Dereference aspect) provides a convenient way to
gain read access to the individual elements of a container starting with a
key value.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Equivalent to
Constant_Reference (Container, Find (Container, Key)).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference_Preserving_Key (Container : @key[aliased in out] Set;
                                   Key       : @key[in] Key_Type)
   @key[return] Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Implicit_Dereference aspect) provides a convenient way to
gain read and write access to the individual elements of a container starting
with a key value.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Equivalent to Reference_Preserving_Key (Container, Find (Container, Key)).]}
@end{DescribeCode}

@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0022-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error for the actual function
associated with a generic formal subprogram, when called as part of an
operation of a set package, to tamper with elements of any set parameter of
the operation. Either Program_Error is raised, or the operation works as
defined on the value of the set either prior to, or subsequent to, some or
all of the modifications to the set.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0027-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to call any subprogram
declared in the visible part of a set package
when the associated container has been finalized. If the operation takes
Container as an @key[in out] parameter, then it raises Constraint_Error or
Program_Error. Otherwise, the operation either proceeds as it would
for an empty container, or it raises Constraint_Error or Program_Error.]}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[
A Cursor value is @i<invalid> if any of the following have occurred since it was
created:@Defn2{Term=[invalid cursor],Sec=[of a set]}
@PDefn2{Term=[cursor],Sec=[invalid]}]}

@begin{Itemize}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The set that contains the element it designates
  has been finalized;]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0160-1]}
  @ChgAdded{Version=[3],Text=[The set that contains the element it designates has been used as the Target of
  a call to Assign, or as the target of an @nt{assignment_statement};]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The set that contains the element it designates
  has been used as the Source or Target of a call to Move; or]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0160-1],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[2],Text=[The element it designates has been
  @Chg{Version=[3],New=[removed],Old=[deleted]} from the
  set@Chg{Version=[3],New=[ that previously contained the element],Old=[]}.]}
  @begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
    @ChgAdded{Version=[3],Text=[This can happen directly via calls to Clear,
    Exclude, Delete, and Update_Element_Preserving_Key, and indirectly via calls
    to procedures Intersection, Difference, and Symmetric_Difference.]}
  @end{Ramification}

@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The result of "=" or Has_Element is
unspecified if these functions are called with an invalid cursor
parameter.@PDefn{unspecified} Execution is erroneous if any other subprogram
declared in Containers.Hashed_Sets or Containers.Ordered_Sets is called with an
invalid cursor parameter.@PDefn2{Term=(erroneous execution),Sec=(cause)}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The list above is intended to be
  exhaustive. In other cases, a cursor value continues to designate its
  original element. For instance, cursor values survive the insertion and
  deletion of other elements.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[While it is possible to check for these cases, in
  many cases the overhead necessary to make the check is substantial in time or
  space. Implementations are encouraged to check for as many of these cases as
  possible and raise Program_Error if detected.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[Execution is erroneous if the set associated with
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
  finalization of the object of Reference_Type will try to access a non-existent
  object. This is a normal case of a dangling pointer created by
  Unchecked_Deallocation; we have to explicitly mention it here as the pointer
  in question is not visible in the specification of the type. (This is the same
  reason we have to say this for invalid cursors.)]}
@end{Reason}
@end{Erron}

@begin{ImplReq}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[No storage associated with a Set object shall be
lost upon assignment or scope exit.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[The execution of an @nt{assignment_statement} for
a set shall have the effect of copying the elements from the source set
object to the target set object@Chg{Version=[3],New=[ and changing the length
of the target object to that of the source object],Old=[]}.]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[An assignment of a Set is a @lquotes@;deep@rquotes
  copy; that is the elements are copied as well as the data structures.
  We say @lquotes@;effect of@rquotes in order to allow the implementation to
  avoid copying elements immediately if it wishes. For instance, an
  implementation that avoided copying until one of the containers is modified
  would be allowed.]}
@end{ImplNote}

@end{ImplReq}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Move should not copy elements, and should minimize
copying of internal data structures.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Move for sets should not copy elements, and should minimize
copying of internal data structures.]}]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Usually that can be accomplished simply by
  moving the pointer(s) to the internal data structures from the Source
  container to the Target container.]}
@end{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[If an exception is propagated from a set
operation, no storage should be lost, nor any elements removed from a set
unless specified by the operation.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[If an exception is propagated from a set
operation, no storage should be lost, nor any elements removed from a set
unless specified by the operation.]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is important so that programs can recover
  from errors. But we don't want to require heroic efforts, so we just require
  documentation of cases where this can't be accomplished.]}
@end{Reason}

@end{ImplAdvice}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[This description of sets is new; the
  extensions are documented with the specific packages.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Added reference support to make set containers more
  convenient to use.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[Added procedure Assign;
  the extension and incompatibility is documented with the specific packages.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[Generalized the definition
  of Move. Specified which elements are read/written by stream attributes.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0022-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a @BoundedName
  to cover tampering by generic actual subprograms.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0027-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a @BoundedName
  to cover access to finalized set containers.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Revised the definition
  of invalid cursors to cover missing (and new) cases.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Defined when a container
  prohibits tampering in order to more clearly define where the check is
  made and the exception raised.]}
@end{DiffWord2005}



@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Hashed_Sets]}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
package Containers.Hashed_Sets has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0084-1],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[@key[with] Ada.Iterator_Interfaces;
],Old=[]}@key{generic}
   @key{type} Element_Type @key{is private};
   @key{with function} Hash (Element : Element_Type) @key{return} Hash_Type;
   @key{with function} Equivalent_Elements (Left, Right : Element_Type)
                 @key{return} Boolean;
   @key{with function} "=" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Hashed_Sets @key{is}@ChildUnit{Parent=[Ada.Containers],Child=[Hashed_Sets]}
   @key{pragma} Preelaborate(Hashed_Sets);@Chg{Version=[3],New=[
   @key{pragma} Remote_Types(Hashed_Sets);],Old=[]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Set} @key{is tagged private}@Chg{Version=[3],New=[
      @key[with] Constant_Indexing => Constant_Reference,
           Default_Iterator  => Iterate,
           Iterator_Element  => Element_Type],Old=[]};
   @key{pragma} Preelaborable_Initialization(Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Cursor} @key{is private};
   @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Empty_Set} : @key{constant} Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[package] @AdaPackDefn{Set_Iterator_Interfaces} @key[is new]
       Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "=" (Left, Right : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Sets} (Left, Right : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{To_Set} (New_Item : Element_Type) @key{return} Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Capacity} (Container : Set) @key{return} Count_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reserve_Capacity} (Container : @key{in out} Set;
                               Capacity  : @key{in}     Count_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Length} (Container : Set) @key{return} Count_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Empty} (Container : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Clear} (Container : @key{in out} Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Position : Cursor) @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} Set;
                              Position  : @key{in}     Cursor;
                              New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Element : @key{in} Element_Type));]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[type] Constant_Reference_Type
         (Element : @key[not null access constant] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Set;
                                Position  : @key[in] Cursor)
      @key[return] Constant_Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Assign} (Target : @key[in out] Set; Source : @key[in] Set);]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Copy} (Source : Set; Capacity : Count_Type := 0) @key[return] Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Move} (Target : @key{in out} Set;
                   Source : @key{in out} Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Set;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Set;
                     New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Include} (Container : @key{in out} Set;
                      New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace} (Container : @key{in out} Set;
                      New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Exclude} (Container : @key{in out} Set;
                      Item      : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Set;
                     Item      : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Set;
                     Position  : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Union} (Target : @key{in out} Set;
                    Source : @key{in}     Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Union} (Left, Right : Set) @key{return} Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "@key{or}" (Left, Right : Set) @key{return} Set @key{renames} Union;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Intersection} (Target : @key{in out} Set;
                           Source : @key{in}     Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Intersection} (Left, Right : Set) @key{return} Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "@key{and}" (Left, Right : Set) @key{return} Set @key{renames} Intersection;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Difference} (Target : @key{in out} Set;
                         Source : @key{in}     Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Difference} (Left, Right : Set) @key{return} Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "-" (Left, Right : Set) @key{return} Set @key{renames} Difference;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Symmetric_Difference} (Target : @key{in out} Set;
                                   Source : @key{in}     Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Symmetric_Difference} (Left, Right : Set) @key{return} Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "@key{xor}" (Left, Right : Set) @key{return} Set
     @key{renames} Symmetric_Difference;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Overlap} (Left, Right : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Subset} (Subset : Set;
                       Of_Set : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First} (Container : Set) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Next} (Position : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Next} (Position : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Find} (Container : Set;
                  Item      : Element_Type) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Contains} (Container : Set;
                      Item      : Element_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Elements} (Left, Right : Cursor)
     @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Elements} (Left  : Cursor;
                                 Right : Element_Type)
     @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Elements} (Left  : Element_Type;
                                 Right : Cursor)
     @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Iterate}
     (Container : @key{in} Set;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] Iterate (Container : @key[in] Set)
      @key[return] Set_Iterator_Interfaces.Forward_Iterator'Class;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{generic}
      @key{type} Key_Type (<>) @key{is private};
      @key{with function} Key (Element : Element_Type) @key{return} Key_Type;
      @key{with function} Hash (Key : Key_Type) @key{return} Hash_Type;
      @key{with function} Equivalent_Keys (Left, Right : Key_Type)
                                     @key{return} Boolean;
   @key{package} @AdaPackDefn{Generic_Keys} @key{is}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{function} @AdaSubDefn{Key} (Position : Cursor) @key{return} Key_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{function} @AdaSubDefn{Element} (Container : Set;
                        Key       : Key_Type)
        @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Replace} (Container : @key{in out} Set;
                         Key       : @key{in}     Key_Type;
                         New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Exclude} (Container : @key{in out} Set;
                         Key       : @key{in}     Key_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Set;
                        Key       : @key{in}     Key_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{function} @AdaSubDefn{Find} (Container : Set;
                     Key       : Key_Type)
         @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{function} @AdaSubDefn{Contains} (Container : Set;
                         Key       : Key_Type)
         @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Update_Element_Preserving_Key}
        (Container : @key{in out} Set;
         Position  : @key{in}     Cursor;
         Process   : @key{not null access procedure}
                         (Element : @key{in out} Element_Type));]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[      @key[type] @AdaTypeDefn{Reference_Type} (Element : @key[not null access] Element_Type) @key[is private]
         @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[      @key[function] @AdaSubDefn{Reference_Preserving_Key} (Container : @key[aliased in out] Set;
                                                Position  : @key[in] Cursor)
         @key[return] Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[      @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Set;
                                   Key       : @key[in] Key_Type)
         @key[return] Constant_Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[      @key[function] @AdaSubDefn{Reference_Preserving_Key} (Container : @key[aliased in out] Set;
                                                Key       : @key[in] Key_Type)
         @key[return] Reference_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{end} Generic_Keys;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{private}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   ... -- @RI[not specified by the language]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{end} Ada.Containers.Hashed_Sets;]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[capacity],Sec=[of a hashed set]}
An object of type Set contains an expandable hash
table, which is used to provide direct access to elements. The @i<capacity> of
an object of type Set is the maximum number of elements that can be inserted
into the hash table prior to it being automatically expanded.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[equivalent element],Sec=[of a hashed set]}
Two elements @i<E1> and @i<E2> are defined to be @i<equivalent> if
Equivalent_Elements (@i<E1>, @i<E2>) returns True.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
Hash is expected to return the same value each time it is called with a
particular element value. For any two equivalent elements, the actual for Hash
is expected to return the same value. If the actual for Hash behaves in some
other manner, the behavior of this package is unspecified. Which subprograms of
this package call Hash, and how many times they call it, is
unspecified.@PDefn{unspecified}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
Equivalent_Elements is expected to return the same value each time it is called
with a particular pair of Element values. It should define an equivalence
relationship, that is, be reflexive, symmetric, and transitive. If the actual
for Equivalent_Elements behaves in some other manner, the behavior of this
package is unspecified. Which subprograms of this package call
Equivalent_Elements, and how many times they call it, is
unspecified.@PDefn{unspecified}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0044-1]}
@ChgAdded{Version=[3],Text=[If the actual function for the generic formal
function "=" returns True for any pair of non-equivalent elements, then the
behavior of the container function "=" is unspecified.@PDefn{unspecified}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[If the value of an element stored in a set is
changed other than by an operation in this package such that at least one of
Hash or Equivalent_Elements give different results, the behavior of this
package is unspecified.@PDefn{unspecified}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[See @RefSec{The Generic Package Containers.Hashed_Maps}
  for a suggested implementation, and for justification of the restrictions
  regarding Hash and Equivalent_Elements. Note that sets only need to store
  elements, not key/element pairs.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[first element],Sec=[of a hashed set]}
@Defn2{Term=[last element],Sec=[of a hashed set]}
@Defn2{Term=[successor element],Sec=[of a hashed set]}
Which elements are the first element and the last
element of a set, and which element is the successor of a given element, are
unspecified, other than the general semantics described in
@RefSecNum{Sets}.@PDefn{unspecified}]}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Capacity (Container : Set) @key{return} Count_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the capacity of Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reserve_Capacity (Container : @key{in out} Set;
                            Capacity  : @key{in}     Count_Type);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Reserve_Capacity allocates a new hash table such
that the length of the resulting set can become at least the value Capacity
without requiring an additional call to Reserve_Capacity, and is large enough
to hold the current length of Container. Reserve_Capacity then rehashes the
elements in Container onto the new hash table. It replaces the old hash table
with the new hash table, and then deallocates the old hash table. Any exception
raised during allocation is propagated and Container is not modified.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Reserve_Capacity tampers with the
cursors of Container.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Trailing],Text=[Reserve_Capacity tampers with the
  cursors, as rehashing probably will change the relationships of the elements
  in Container.]}
@end{Reason}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Clear (Container : @key{in out} Set);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[In addition to the semantics
described in @RefSecNum{Sets}, Clear does not affect the capacity of
Container.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[procedure] Assign (Target : @key[in out] Set; Source : @key[in] Set);]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[In addition to the semantics
described in @RefSecNum{Sets}, if the length of Source is greater than the
capacity of Target, Reserve_Capacity (Target, Length (Source)) is called
before assigning any elements.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Copy (Source : Set; Capacity : Count_Type := 0) @key[return] Set;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a set whose elements are
initialized from the elements of Source. If Capacity is 0, then the set capacity
is the length of Source; if Capacity is equal to or greater than the length of
Source, the set capacity is at least the specified value. Otherwise, the
operation propagates Capacity_Error.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Set;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[In addition to the semantics
described in @RefSecNum{Sets}, if Length (Container) equals Capacity
(Container), then Insert first calls Reserve_Capacity to increase the capacity
of Container to some larger value.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First (Container : Set) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Length (Container) = 0, then
First returns No_Element. Otherwise, First returns a cursor that designates the
first hashed element in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Equivalent_Elements (Left, Right : Cursor)
      @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Equivalent_Elements
(Element (Left), Element (Right)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Equivalent_Elements (Left  : Cursor;
                              Right : Element_Type) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Equivalent_Elements
(Element (Left), Right).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Equivalent_Elements (Left  : Element_Type;
                              Right : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Equivalent_Elements (Left, Element (Right)).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] Set)
   @key[return] Set_Iterator_Interfaces.Forward_Iterator'Class;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate returns an
iterator object that will generate a value for the loop parameter designating
each element in Container, starting with the first element and moving the cursor
according to the successor relation.
Tampering with the cursors of Container is prohibited while
the iterator object exists (in particular, in
the @nt{sequence_of_statements} of the @nt{loop_statement} whose
@nt{iterator_specification} denotes this object). The iterator object needs
finalization.]}

@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[For any element @i{E}, the actual function for the
generic formal function Generic_Keys.Hash is expected to be such that Hash
(@i{E}) = Generic_Keys.Hash (Key (@i{E})). If the actuals for Key or
Generic_Keys.Hash behave in some other manner, the behavior of Generic_Keys is
unspecified. Which subprograms of Generic_Keys call Generic_Keys.Hash, and how
many times they call it, is unspecified.@PDefn{unspecified}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[For any two elements @i{E1} and @i{E2}, the boolean
values Equivalent_Elements (@i{E1}, @i{E2}) and Equivalent_Keys (Key (@i{E1}),
Key (@i{E2})) are expected to be equal. If the actuals for Key or
Equivalent_Keys behave in some other manner, the behavior of Generic_Keys is
unspecified. Which subprograms of Generic_Keys call Equivalent_Keys, and how
many times they call it, is unspecified.@PDefn{unspecified}]}

@end{StaticSem}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[If @i<N> is the length of a set, the
average time complexity of the subprograms
Insert, Include, Replace, Delete, Exclude and Find that take an element
parameter should be @i{O}(log @i<N>). The average time complexity of the
subprograms that take a cursor parameter should be @i{O}(1). The average time
complexity of Reserve_Capacity should be @i{O}(@i<N>).]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The average time complexity of the Insert, Include, Replace, Delete, Exclude and
Find operations of Containers.Hashed_Sets that take an element parameter
should be @i{O}(log @i<N>). The average time complexity of the subprograms
of Containers.Hashed_Sets that take a cursor parameter should be @i{O}(1). The
average time complexity of Containers.@!Hashed_Sets.@!Reserve_Capacity should be
@i{O}(@i<N>).]}]}
@end{ImplAdvice}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[See @RefSec{The Generic Package Containers.Hashed_Maps}
  for implementation notes regarding some of the operations of this package.]}
@end{ImplNote}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The generic package Containers.Hashed_Sets is new.]}
@end{Extend95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}Subprograms Assign and Copy
  are newly added to Containers.Hashed_Sets. If an instance of Containers.Hashed_Sets
  is referenced in a @nt{use_clause}, and an entity @i<E> with the same
  @nt{defining_identifier} as a new entity in Containers.Hashed_Sets is
  defined in a package that is also referenced in a @nt{use_clause}, the
  entity @i<E> may no longer be use-visible, resulting in errors. This should
  be rare and is easily fixed if it does occur.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Added iterator and indexing support to make hashed set containers more
  convenient to use.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0044-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added wording to require
  the formal function be such that equal elements are also equivalent.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0084-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a pragma
  Remote_Types so that containers can be used in distributed programs.]}
@end{DiffWord2005}


@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Ordered_Sets]}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
package Containers.Ordered_Sets has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0084-1],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[@key[with] Ada.Iterator_Interfaces;
],Old=[]}@key{generic}
   @key{type} Element_Type @key{is private};
   @key{with function} "<" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
   @key{with function} "=" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Ordered_Sets @key{is}@ChildUnit{Parent=[Ada.Containers],Child=[Ordered_Sets]}
   @key{pragma} Preelaborate(Ordered_Sets);@Chg{Version=[3],New=[
   @key{pragma} Remote_Types(Ordered_Sets);],Old=[]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Elements} (Left, Right : Element_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Set} @key{is tagged private}@Chg{Version=[3],New=[
      @key[with] Constant_Indexing => Constant_Reference,
           Default_Iterator  => Iterate,
           Iterator_Element  => Element_Type],Old=[]};
   @key{pragma} Preelaborable_Initialization(Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Cursor} @key{is private};
   @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Empty_Set} : @key{constant} Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[package] @AdaPackDefn{Set_Iterator_Interfaces} @key[is new]
       Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "=" (Left, Right : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Sets} (Left, Right : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{To_Set} (New_Item : Element_Type) @key{return} Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Length} (Container : Set) @key{return} Count_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Empty} (Container : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Clear} (Container : @key{in out} Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Position : Cursor) @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} Set;
                              Position  : @key{in}     Cursor;
                              New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Element : @key{in} Element_Type));]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[type] Constant_Reference_Type
         (Element : @key[not null access constant] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Set;
                                Position  : @key[in] Cursor)
      @key[return] Constant_Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Assign} (Target : @key[in out] Set; Source : @key[in] Set);]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Copy} (Source : Set) @key[return] Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Move} (Target : @key{in out} Set;
                   Source : @key{in out} Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Set;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Set;
                     New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Include} (Container : @key{in out} Set;
                      New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace} (Container : @key{in out} Set;
                      New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Exclude} (Container : @key{in out} Set;
                      Item      : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Set;
                     Item      : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Set;
                     Position  : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_First} (Container : @key{in out} Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_Last} (Container : @key{in out} Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Union} (Target : @key{in out} Set;
                    Source : @key{in}     Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Union} (Left, Right : Set) @key{return} Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "@key{or}" (Left, Right : Set) @key{return} Set @key{renames} Union;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Intersection} (Target : @key{in out} Set;
                           Source : @key{in}     Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Intersection} (Left, Right : Set) @key{return} Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "@key{and}" (Left, Right : Set) @key{return} Set @key{renames} Intersection;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Difference} (Target : @key{in out} Set;
                         Source : @key{in}     Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Difference} (Left, Right : Set) @key{return} Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "-" (Left, Right : Set) @key{return} Set @key{renames} Difference;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Symmetric_Difference} (Target : @key{in out} Set;
                                   Source : @key{in}     Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Symmetric_Difference} (Left, Right : Set) @key{return} Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "@key{xor}" (Left, Right : Set) @key{return} Set @key{renames}
      Symmetric_Difference;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Overlap} (Left, Right : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Subset} (Subset : Set;
                       Of_Set : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First} (Container : Set) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First_Element} (Container : Set) @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last} (Container : Set) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last_Element} (Container : Set) @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Next} (Position : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Next} (Position : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Previous} (Position : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Previous} (Position : @key{in out} Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Find} (Container : Set;
                  Item      : Element_Type)
      @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Floor} (Container : Set;
                   Item      : Element_Type)
      @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Ceiling} (Container : Set;
                     Item      : Element_Type)
      @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Contains} (Container : Set;
                      Item      : Element_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}]}


@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "<" (Left, Right : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} ">" (Left, Right : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "<" (Left : Cursor; Right : Element_Type)
      @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} ">" (Left : Cursor; Right : Element_Type)
      @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "<" (Left : Element_Type; Right : Cursor)
      @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} ">" (Left : Element_Type; Right : Cursor)
      @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Iterate}
     (Container : @key{in} Set;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reverse_Iterate}
     (Container : @key{in} Set;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] Iterate (Container : @key[in] Set)
      @key[return] Set_Iterator_Interfaces.Reversible_Iterator'Class;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{generic}
      @key{type} Key_Type (<>) @key{is private};
      @key{with function} Key (Element : Element_Type) @key{return} Key_Type;
      @key{with function} "<" (Left, Right : Key_Type)
         @key{return} Boolean @key{is} <>;
   @key{package} @AdaPackDefn{Generic_Keys} @key{is}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[       @key{function} @AdaSubDefn{Equivalent_Keys} (Left, Right : Key_Type)
          @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[       @key{function} @AdaSubDefn{Key} (Position : Cursor) @key{return} Key_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[       @key{function} @AdaSubDefn{Element} (Container : Set;
                         Key       : Key_Type)
          @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[       @key{procedure} @AdaSubDefn{Replace} (Container : @key{in out} Set;
                          Key       : @key{in}     Key_Type;
                          New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[       @key{procedure} @AdaSubDefn{Exclude} (Container : @key{in out} Set;
                          Key       : @key{in}     Key_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[       @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Set;
                         Key       : @key{in}     Key_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[       @key{function} @AdaSubDefn{Find} (Container : Set;
                      Key       : Key_Type)
          @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[       @key{function} @AdaSubDefn{Floor} (Container : Set;
                       Key       : Key_Type)
          @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[       @key{function} @AdaSubDefn{Ceiling} (Container : Set;
                         Key       : Key_Type)
          @key{return} Cursor;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[       @key{function} @AdaSubDefn{Contains} (Container : Set;
                          Key       : Key_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[       @key{procedure} @AdaSubDefn{Update_Element_Preserving_Key}
         (Container : @key{in out} Set;
          Position  : @key{in}     Cursor;
          Process   : @key{not null access procedure}
                          (Element : @key{in out} Element_Type));]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[      @key[type] @AdaTypeDefn{Reference_Type} (Element : @key[not null access] Element_Type) @key[is private]
         @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[      @key[function] @AdaSubDefn{Reference_Preserving_Key} (Container : @key[aliased in out] Set;
                                                Position  : @key[in] Cursor)
         @key[return] Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[      @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Set;
                                   Key       : @key[in] Key_Type)
         @key[return] Constant_Reference_Type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[      @key[function] @AdaSubDefn{Reference_Preserving_Key} (Container : @key[aliased in out] Set;
                                                Key       : @key[in] Key_Type)
         @key[return] Reference_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{end} Generic_Keys;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{private}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   ... -- @RI[not specified by the language]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{end} Ada.Containers.Ordered_Sets;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Two elements @i<E1> and @i<E2> are @i<equivalent>
if both @i<E1> < @i<E2> and @i<E2> < @i<E1> return False, using the generic
formal "<" operator for elements.@Defn2{Term=[equivalent element],Sec=[of a ordered set]}
Function Equivalent_Elements returns True if Left and Right are equivalent,
and False otherwise.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0044-1]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
"<" on Element_Type values is expected to return the same value each time it is
called with a particular pair of key values. It should define a strict
@Chg{Version=[3],New=[weak ],Old=[]}ordering relationship@Chg{Version=[3],
New=[ (see @RefSecNum{Containers})],Old=[, that is, be irreflexive, asymmetric,
and transitive]}. If the
actual for "<" behaves in some other manner, the behavior of this package is
unspecified. Which subprograms of this package call "<" and how many times they
call it, is unspecified.@PDefn{unspecified}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0044-1]}
@ChgAdded{Version=[3],Text=[If the actual function for the generic formal
function "=" returns True for any pair of non-equivalent elements, then the
behavior of the container function "=" is unspecified.@PDefn{unspecified}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[If the value of an element stored in a set is changed
other than by an operation in this package such that at least one of
"<" or "=" give different
results, the behavior of this package is unspecified.@PDefn{unspecified}]}

@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[See @RefSec{The Generic Package Containers.Ordered_Maps}
for a suggested implementation, and for justification of the restrictions
regarding "<" and "=". Note that sets only need to store elements, not
key/element pairs.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[first element],Sec=[of a ordered set]}
@Defn2{Term=[last element],Sec=[of a ordered set]}
@Defn2{Term=[successor element],Sec=[of a ordered set]}@Chg{Version=[3],New=[
@Defn2{Term=[predecessor element],Sec=[of a ordered set]}],Old=[]}
The @Chg{Version=[3],New=[@i<first element>],Old=[first element]}
of a nonempty set is the one
which is less than all the other elements in the set.
The @Chg{Version=[3],New=[@i<last element>],Old=[last element]} of a
nonempty set is the one which is greater than all the other elements in the
set. The @Chg{Version=[3],New=[@i<successor>],Old=[successor]}
of an element is the smallest element that is larger than
the given element. The @Chg{Version=[3],New=[@i<predecessor>],Old=[predecessor]}
of an element is the largest element that is
smaller than the given element. All comparisons are done using the generic
formal "<" operator for elements.]}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Copy (Source : Set) @key[return] Set;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a set whose
elements are initialized from the corresponding elements of Source.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_First (Container : @key{in out} Set);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, Delete_First
has no effect. Otherwise@Chg{Version=[3],New=[,],Old=[]}
the element designated by First (Container) is removed
from Container. Delete_First tampers with the cursors of Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_Last (Container : @key{in out} Set);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, Delete_Last
has no effect. Otherwise@Chg{Version=[3],New=[,],Old=[]}
the element designated by Last (Container) is removed
from Container. Delete_Last tampers with the cursors of Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First_Element (Container : Set) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to
Element (First (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last (Container : Set) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a cursor that designates
the last element in Container. If Container is empty, returns No_Element.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last_Element (Container : Set) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to
Element (Last (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Previous (Position : Cursor) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Position equals No_Element, then
Previous returns No_Element. Otherwise@Chg{Version=[3],New=[,],Old=[]}
Previous returns a cursor designating
the @Chg{Version=[3],New=[predecessor ],Old=[]}element
@Chg{Version=[3],New=[of],Old=[that precedes]} the one designated by Position.
If Position designates the first element, then Previous returns No_Element.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Previous (Position : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Previous (Position).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Floor (Container : Set;
                Item      : Element_Type) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Floor searches for the last element
which is not greater than Item. If such an element is found, a cursor that
designates it is returned. Otherwise@Chg{Version=[3],New=[,],Old=[]}
No_Element is returned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Ceiling (Container : Set;
                  Item      : Element_Type) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Ceiling searches for the first
element which is not less than Item. If such an element is found, a cursor that
designates it is returned. Otherwise@Chg{Version=[3],New=[,],Old=[]}
No_Element is returned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "<" (Left, Right : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Left) < Element (Right).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} ">" (Left, Right : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Right) < Element (Left).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "<" (Left : Cursor; Right : Element_Type) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Left) < Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} ">" (Left : Cursor; Right : Element_Type) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Right < Element (Left).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "<" (Left : Element_Type; Right : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Left < Element (Right).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} ">" (Left : Element_Type; Right : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Right) < Left.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reverse_Iterate
   (Container : @key{in} Set;
    Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Iterates over the elements in
Container as per @Chg{Version=[3],New=[procedure ],Old=[]}Iterate,
with the difference that the elements are traversed
in predecessor order, starting with the last element.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] Set)
   @key[return] Set_Iterator_Interfaces.Reversible_Iterator'Class;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate returns a reversible
iterator object that will generate a value for the loop parameter designating
each element in Container, starting with the first element and moving the cursor
according to the successor relation when used as a forward iterator, and
starting with the last element and moving the cursor according to the
predecessor relation when used as a reverse iterator.
Tampering with the cursors of Container is prohibited while
the iterator object exists (in particular, in
the @nt{sequence_of_statements} of the @nt{loop_statement} whose
@nt{iterator_specification} denotes this object). The iterator object needs
finalization.]}

@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[For any two elements @i<E1> and @i<E2>, the
boolean values (@i<E1> < @i<E2>) and (Key(@i<E1>) < Key(@i<E2>)) are expected
to be equal. If the actuals for Key or Generic_Keys."<" behave in some other
manner, the behavior of this package is unspecified. Which subprograms of this
package call Key and Generic_Keys."<", and how many times the functions are
called, is unspecified.@PDefn{unspecified}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[In addition to the semantics described in
@RefSecNum{Sets}, the subprograms in package Generic_Keys named
Floor and Ceiling, are equivalent to the corresponding subprograms in the
parent package, with the difference that the
Key subprogram parameter is compared to elements in the container using the
Key and "<" generic formal functions. The function named Equivalent_Keys
in package Generic_Keys returns True if both Left < Right and Right < Left
return False using the generic formal "<" operator, and returns True otherwise.]}

@end{StaticSem}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[If @i<N> is the length of a set, then the
worst-case time complexity of the Insert, Include, Replace, Delete, Exclude and
Find operations that take an element parameter should be @i{O}((log @i<N>)**2) or
better. The worst-case time complexity of the subprograms that take a cursor
parameter should be @i{O}(1).]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The worst-case time complexity of the Insert, Include, Replace, Delete, Exclude and
Find operations of Containers.Ordered_Sets that take an element parameter
should be @i{O}((log @i<N>)**2). The worst-case time complexity of the subprograms
of Containers.Ordered_Sets that take a cursor parameter should be @i{O}(1).]}]}
@end{ImplAdvice}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[See @RefSec{The Generic Package Containers.Ordered_Maps}
  for implementation notes regarding some of the operations of this package.]}
@end{ImplNote}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The generic package Containers.Ordered_Sets is new.]}
@end{Extend95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}Subprograms Assign and Copy
  are newly added to Containers.Ordered_Sets. If an instance of Containers.Ordered_Sets
  is referenced in a @nt{use_clause}, and an entity @i<E> with the same
  @nt{defining_identifier} as a new entity in Containers.Ordered_Sets is
  defined in a package that is also referenced in a @nt{use_clause}, the
  entity @i<E> may no longer be use-visible, resulting in errors. This should
  be rare and is easily fixed if it does occur.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Added iterator and indexing support to make ordered set containers more
  convenient to use.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0044-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added wording to
  require the formal function be such that equal elements are also
  equivalent.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0044-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Redefined "<" actuals
  to require a strict weak ordering; the old definition allowed
  indeterminant comparisons that would not have worked in a container.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0084-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a pragma
  Remote_Types so that containers can be used in distributed programs.]}
@end{DiffWord2005}



