@comment{ $Source: e:\\cvsroot/ARM/Source/pre_containers.mss,v $ }
@comment{ $Revision: 1.7 $ $Date: 2005/01/21 06:07:30 $ $Author: Randy $ }
@Part(precontainers, Root="ada.mss")

@Comment{$Date: 2005/01/21 06:07:30 $}

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
  @ChgAdded{Version=[2],Text=[Hashed Maps keyed by any non-limited hashable type,
  and containing any non-limited type;]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Ordered Maps keyed by any non-limited ordered type,
  and containing any non-limited type;]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Hashed Sets of any non-limited hashable type; and]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Ordered Sets of any non-limited ordered type.]}
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

@end{StaticSem}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Hash_Type'Modulus should be at least 2**32.
Count_Type'Last should be at least 2**31-1.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Containers.Hash_Type'Modulus should be at least 2**32.
Containers.Count_Type'Last should be at least 2**31-1.]}]}

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
@ChgAdded{Version=[2],Text=[The language-defined generic package
Containers.Vectors provides private types Vector and Cursor, and a set of
operations for each type. A vector container allows insertion and deletion at
any position, but it is specifically optimized for insertion and deletion at
the high end (the end with the higher index) of the container. A vector
container also provides random access to its elements.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@Pdefn2{Term=[length], Sec=(of a vector container)}
@Pdefn2{Term=[capacity], Sec=(of a vector container)}
A vector container behaves conceptually as an array that expands as necessary
as items are inserted. The @i{length} of a vector is the number of elements that
the vector contains. The @i{capacity} of a vector is the maximum number of
elements that can be inserted into the vector prior to it being automatically
expanded.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@Pdefn2{Term=[empty element], Sec=(of a vector container)}
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

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaDefn{No_Element} : @key{constant} Cursor;]}

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
@ChgAdded{Version=[2],Text=[   @key{function} "=" (Left, Right : Vector) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Capacity} (Container : Vector) @key{return} Count_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reserve_Capacity} (Container : @key{in out} Vector;
                               Capacity  : @key{in}     Count_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Length} (Container : Vector) @key{return} Count_Type;]}



@end{Example}

@end{StaticSem}


@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The package Ada.Containers.Vectors is new.]}
@end{Extend95}

**** The text below here still needs to be formatted ****

   @key{function} Is_Empty (Container : Vector) @key{return} Boolean;

   @key{procedure} Clear (Container : @key{in out} Vector);

   @key{function} To_Cursor (Container : Vector;
                       Index     : Extended_Index) @key{return} Cursor;

   @key{function} To_Index (Position  : Cursor) @key{return} Extended_Index;

   @key{function} Element (Container : Vector;
                     Index     : Index_Type)
      @key{return} Element_Type;

   @key{function} Element (Position : Cursor) @key{return} Element_Type;

   @key{procedure} Query_Element
     (Container : @key{in} Vector;
      Index     : @key{in} Index_Type;
      Process   : @key{not null access procedure} (Element : @key{in} Element_Type));

   @key{procedure} Query_Element
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Element : @key{in} Element_Type));

   @key{procedure} Update_Element
     (Container : @key{in} Vector;
      Index     : @key{in} Index_Type;
      Process   : @key{not null access procedure} (Element : @key{in out} Element_Type));

   @key{procedure} Update_Element
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Element : @key{in out} Element_Type));

   @key{procedure} Replace_Element (Container : @key{in} Vector;
                              Index     : @key{in} Index_Type;
                              By        : @key{in} Element_Type);

   @key{procedure} Replace_Element (Position : @key{in} Cursor;
                              By       : @key{in} Element_Type);

   @key{procedure} Assign (Target : @key{in out} Vector;
                     Source : @key{in}     Vector);

   @key{procedure} Move (Target : @key{in out} Vector;
                   Source : @key{in out} Vector);

   @key{procedure} Insert (Container : @key{in out} Vector;
                     Before    : @key{in}     Extended_Index;
                     New_Item  : @key{in}     Vector);

   @key{procedure} Insert (Container : @key{in out} Vector;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Vector);

   @key{procedure} Insert (Container : @key{in out} Vector;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Vector;
                     Position  :    @key{out} Cursor);

   @key{procedure} Insert (Container : @key{in out} Vector;
                     Before    : @key{in}     Extended_Index;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1);

   @key{procedure} Insert (Container : @key{in out} Vector;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1);

   @key{procedure} Insert (Container : @key{in out} Vector;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Count     : @key{in}     Count_Type := 1);

   @key{procedure} Prepend (Container : @key{in out} Vector;
                      New_Item  : @key{in}     Vector);

   @key{procedure} Prepend (Container : @key{in out} Vector;
                      New_Item  : @key{in}     Element_Type;
                      Count     : @key{in}     Count_Type := 1);

   @key{procedure} Append (Container : @key{in out} Vector;
                     New_Item  : @key{in}     Vector);

   @key{procedure} Append (Container : @key{in out} Vector;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1);

   @key{procedure} Insert_Space (Container : @key{in out} Vector;
                           Before    : @key{in}     Extended_Index;
                           Count     : @key{in}     Count_Type := 1);

   @key{procedure} Insert_Space (Container : @key{in out} Vector;
                           Before    : @key{in}     Cursor;
                           Position  :    @key{out} Cursor;
                           Count     : @key{in}     Count_Type := 1);

   @key{procedure} Set_Length (Container : @key{in out} Vector;
                         Length    : @key{in}     Count_Type);

   @key{procedure} Delete (Container : @key{in out} Vector;
                     Index     : @key{in}     Extended_Index;
                     Count     : @key{in}     Count_Type := 1);

   @key{procedure} Delete (Container : @key{in out} Vector;
                     Position  : @key{in out} Cursor;
                     Count     : @key{in}     Count_Type := 1);

   @key{procedure} Delete_First (Container : @key{in out} Vector;
                           Count     : @key{in}     Count_Type := 1);

   @key{procedure} Delete_Last (Container : @key{in out} Vector;
                          Count     : @key{in}     Count_Type := 1);

   @key{function} First_Index (Container : Vector) @key{return} Index_Type;

   @key{function} First (Container : Vector) @key{return} Cursor;

   @key{function} First_Element (Container : Vector)
      @key{return} Element_Type;

   @key{function} Last_Index (Container : Vector) @key{return} Extended_Index;

   @key{function} Last (Container : Vector) @key{return} Cursor;

   @key{function} Last_Element (Container : Vector)
      @key{return} Element_Type;

   @key{procedure} Swap (Container : @key{in} Vector;
                   I, J      : @key{in} Index_Type);

   @key{procedure} Swap (I, J      : @key{in}     Cursor);

   @key{generic}
      @key{with function} "<" (Left, Right : Element_Type)
         @key{return} Boolean @key{is} <>;
   @key{procedure} Generic_Sort (Container : @key{in} Vector);

   @key{function} Find_Index (Container : Vector;
                        Item      : Element_Type;
                        Index     : Index_Type := Index_Type'First)
      @key{return} Extended_Index;

   @key{function} Find (Container : Vector;
                  Item      : Element_Type;
                  Position  : Cursor := No_Element)
      @key{return} Cursor;

   @key{function} Reverse_Find_Index (Container : Vector;
                                Item      : Element_Type;
                                Index     : Index_Type := Index_Type'Last)
      @key{return} Extended_Index;

   @key{function} Reverse_Find (Container : Vector;
                          Item      : Element_Type;
                          Position  : Cursor := No_Element)
      @key{return} Cursor;

   @key{function} Contains (Container : Vector;
                      Item      : Element_Type) @key{return} Boolean;


   @key{function} Next (Position : Cursor) @key{return} Cursor;

   @key{function} Previous (Position : Cursor) @key{return} Cursor;

   @key{procedure} Next (Position : @key{in out} Cursor);

   @key{procedure} Previous (Position : @key{in out} Cursor);

   @key{function} Has_Element (Position : Cursor) @key{return} Boolean;

   @key{procedure}  Iterate
     (Container : @key{in} Vector;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));

   @key{procedure} Reverse_Iterate
     (Container : @key{in} Vector;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));

@key{private}

   ... -- @RI[not specified by the language]

@key{end} Ada.Containers.Vectors;>


The type Vector is used to represent vectors. The type Vector needs finalization
(see 7.6).

Empty_Vector represents the empty vector object. It has a length of 0. If an
object of type Vector is not otherwise initialized, it is initialized to the
same value as Empty_Vector.

No_Element represents a cursor that designates no element. If an object of type
Cursor is not otherwise initialized, it is initialized to the same value as
No_Element.

No_Index represents a position that does not correspond to any element. The
subtype Extended_Index includes the indices covered by Index_Type plus the
value No_Index and, if it exists, the successor to the Index_Type'Last.

AARM Note: We require the existence of Index_Type'First - 1, so that No_Index
and Last_Index of an empty vector is well-defined. We don't require the
existence of Index_Type'Last + 1, as it is only used as the position of
insertions (and needs to be allowed only when inserting an empty vector).

Some operations are assumed to work on a constant set of elements. For such
an operation, a subprogram is said to @i<tamper with cursors> of a vector
object @i<V> if:

@xbullet<it inserts or deletes elements of @i<V>, that is, it calls the Insert,
Insert_Space, Clear, Delete, or Set_Length procedures with @i<V> as a
parameter; or>

   AARM To Be Honest: Operations which are defined to be equivalent to
   a call on one of these operations also are included. Similarly, operations
   which call one of these as part of their definition are included.

@xbullet<it finalizes @i<V>; or>

@xbullet<it calls the Move procedure with @i<V> as
a parameter.>

Some operations are assumed to not change elements.  For such an operation, a
subprogram is said to @i<tamper with elements> of a vector object @i<V> if:

@xbullet<it tampers with cursors of @i<V>; or>
@xbullet<it modifies one or more elements of @i<V>, that is, it calls the
Replace_Element, Update_Element, or Swap procedures or an
instance of Generic_Sort with @i<V> as a parameter.>

   AARM Note:
   Swap and Generic_Sort copy elements rather than reordering them, so
   they can be allowed for Iterate.

@xcode<@key{function} To_Vector (Length : Count_Type) @key{return} Vector;>

@xindent<Returns a vector with a length of Length, filled with empty elements.>

@xcode<@key{function} To_Vector
  (New_Item : Element_Type;
   Length   : Count_Type) @key{return} Vector;>

@xindent<Returns a vector with a length of Length, filled with elements initialized to
the value New_Item.>

@xcode<@key{function} "&" (Left, Right : Vector) @key{return} Vector;>

@xindent<Returns a vector comprising the elements of Left followed by the
elements of Right.>

@xcode<@key{function} "&" (Left  : Vector;
              Right : Element_Type) @key{return} Vector;>

@xindent<Returns a vector comprising the elements of Left followed by the
element Right.>

@xcode<@key{function} "&" (Left  : Element_Type;
              Right : Vector) @key{return} Vector;>

@xindent<Returns a vector comprising the element Left followed by the elements of Right.>

@xcode<@key{function} "&" (Left, Right  : Element_Type) @key{return} Vector;>

@xindent<Returns a vector comprising the element Left followed by the element Right.>

@xcode<@key{function} "=" (Left, Right : Vector) @key{return} Boolean;>

@xindent<If Left and Right denote the same vector object, then the function returns
True. If Left and Right have different lengths, then the function returns
False. Otherwise, it compares each element in Left to the corresponding element
in Right using the generic formal equality operator; if element equality
returns False, then the function returns False. If the function has not
returned a result after checking all of the elements, it returns True. Any
exception raised during evaluation of element equality is propagated.>

@xcode<@key{function} Capacity (Container : Vector) @key{return} Count_Type;>

@xindent<Returns the capacity of Container.>

@xcode<@key{procedure} Reserve_Capacity (Container : @key{in out} Vector;
                            Capacity  : @key{in}     Count_Type);>

@xindent<Reserve_Capacity allocates new internal data structures such that the length of
the resulting vector can become at least the value Capacity without requiring
an additional call to Reserve_Capacity, and is large enough to hold the current
length of Container. Reserve_Capacity then copies the elements into the new
data structures and deallocates the old data structures. Any exception raised
during allocation is propagated and Container is not modified.>

AARM Notes

Expanding the internal array can be done by allocating a new, longer array,
copying the elements, and deallocating the original array. This may raise
Storage_Error, or cause an exception from a controlled subprogram. We require
that a failed Reserve_Capacity does not lose any elements if an exception
occurs, but we do not require a specific order of evaluations or copying.

This routine is used to preallocate the internal array to the specified
capacity such that future Inserts do not require memory allocation overhead.
Therefore, the implementation should allocate the needed memory to make that
true at this point, even though the visible semantics could be preserved by
waiting until the memory is needed. This doesn't apply to the indefinite
element container, because elements will have to be allocated individually.

The implementation does not have to contract the internal array if the
capacity is reduced, as any capacity greater than or equal to the specified
capacity is allowed.

End AARM Notes

@xcode<@key{function} Length (Container : Vector) @key{return} Count_Type;>

@xindent<Returns the number of elements in Container.>

@xcode<@key{function} Is_Empty (Container : Vector) @key{return} Boolean;>

@xindent<Equivalent to Length (Container) = 0.>

@xcode<@key{procedure} Clear (Container : @key{in out} Vector);>

@xindent<Removes all the elements from Container. The capacity of Container does not
change.>

@xcode<@key{function} To_Cursor (Container : Vector;
                    Index     : Extended_Index) @key{return} Cursor;>

@xindent<If Index is not in the range First_Index (Container) .. Last_Index (Container),
then No_Element is returned. Otherwise, a cursor designating the
element at position Index in Container is returned.>

@xcode<@key{function} To_Index (Position  : Cursor) @key{return} Extended_Index;>

@xindent<If Position is No_Element, No_Index is returned. Otherwise, the
index (within its containing vector) of the element designated by Cursor is
returned.>

AARM Note: This implies that the index is determinable from a bare cursor
alone. The basic model is that a vector cursor is implemented as a record
containing an access to the vector container and an index value. This does
constrain implementations, but it also allows all of the cursor operations
to be defined in terms of the corresponding index operation (which should be
primary for a vector).

@xcode<@key{function} Element (Container : Vector;
                  Index     : Index_Type)
   @key{return} Element_Type;>

@xindent<If Index is not in the range First_Index (Container) .. Last_Index (Container),
then Constraint_Error is propagated. Otherwise, Element returns the element at
position Index.>

@xcode<@key{function} Element (Position  : Cursor) @key{return} Element_Type;>

@xindent<If Position equals No_Element, then Constraint_Error is propagated. Otherwise,
Element returns the element designated by Position.>

@xcode<@key{procedure} Query_Element
  (Container : @key{in} Vector;
   Index     : @key{in} Index_Type;
   Process   : @key{not null access} @key{procedure} (Element : @key{in} Element_Type));>

@xindent<If Index is not in the range First_Index (Container) .. Last_Index (Container),
then Constraint_Error is propagated. Otherwise, Query_Element calls Process.all
with the element at position Index as the argument. Program_Error is propagated
if Process.all tampers with the elements of Container. Any exception raised by
Process.all is propagated.>

AARM Note: The tamper with the elements check is intended to prevent the
Element parameter of Process from being modified or deleted outside of
Process. The check prevent data loss (if Element_Type is passed by copy)
or erroneous execution (if Element_Type is an unconstrained type in
an indefinite container).

@xcode<@key{procedure} Query_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access} @key{procedure} (Element : @key{in} Element_Type));>

@xindent<If Position equals No_Element, then Constraint_Error is propagated. Otherwise,
Query_Element calls Process.@key{all} with the element designated by Position as the
argument. Program_Error is propagated if Process.@key{all} tampers with the
elements of Container. Any exception raised by Process.@key{all} is propagated.>

@xcode<@key{procedure} Update_Element
  (Container : @key{in} Vector;
   Index     : @key{in} Index_Type;
   Process   : @key{not null access} @key{procedure} (Element : @key{in out} Element_Type));>

@xindent<If Index is not in the range First_Index (Container) .. Last_Index (Container),
then Constraint_Error is propagated. Otherwise, Update_Element calls Process.@key{all}
with the element at position Index as the argument. Program_Error is propagated
if Process.@key{all} tampers with the elements of Container. Any exception raised by
Process.@key{all} is propagated.>

@xindent<If Element_Type is unconstrained and definite, then the Element parameter
of Process.@key{all} shall be unconstrained.>

AARM Note: This means that the elements cannot be directly allocated from the
heap (nor aliased unless AI-363 is included in the Amendment); it must be
possible to change the discriminants of the element in place.

@xindent<The element at position Index is not an empty element after successful
completion of this operation.>

AARM Note: Since reading an empty element is a bounded error, attempting to
use this procedure to replace empty elements may fail. Use Replace_Element
to do that reliably.

@xcode<@key{procedure} Update_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access} @key{procedure} (Element : @key{in out} Element_Type));>

@xindent<If Position equals No_Element, then Constraint_Error is propagated. Otherwise,
Update_Element calls Process.@key{all} with the element designated by Position as the
argument. Program_Error is propagated if Process.@key{all} tampers with the
elements of Container. Any exception raised by Process.@key{all} is propagated.>

@xindent<If Element_Type is unconstrained and definite, then the Element parameter
of Process.@key{all} shall be unconstrained.>

@xindent<The element designated by Position is not an empty element after successful
completion of this operation.>

@xcode<@key{procedure} Replace_Element (Container : @key{in} Vector;
                           Index     : @key{in} Index_Type;
                           By        : @key{in} Element_Type);>

@xindent<If Index is not in the range First_Index (Container) .. Last_Index (Container),
then Constraint_Error is propagated. Otherwise Replace_Element assigns the
value By to the element at position Index. Any exception raised during the
assignment is propagated. The element at position Index is not an empty element
after successful call to Replace_Element.>

@xcode<@key{procedure} Replace_Element (Position : @key{in} Cursor;
                           By       : @key{in} Element_Type);>

@xindent<If Position equals No_Element, then Constraint_Error is propagated. Otherwise
Replace_Element assigns By to the element designated by Position. Any exception
raised during the assignment is propagated. The element at Position is
not an empty element after successful call to Replace_Element.>

AARM Note: Replace_Element and Update_Element are the only ways that an element
can change from empty to non-empty.

@xcode<@key{procedure} Assign (Target : @key{in out} Vector;
                  Source : @key{in}     Vector);>

@xindent<If Target denotes the same object as Source, then the operation has no effect.
Otherwise, Assign first calls Clear (Target), then Reserve_Capacity (Target,
Length (Source)). It then assigns the elements of Source to the corresponding
positions in Target, and sets the length of Target to the length of Source.
Any exception raised during element assignment is propagated.>

@xcode<@key{procedure} Move (Target : @key{in out} Vector;
                Source : @key{in out} Vector);>

@xindent<If Target denotes the same object as Source, then Move has no effect.
Otherwise, Move first calls Clear (Target); then, each element from Source
is removed from Source and inserted into Target in the original order. The
length of Source is 0 after a successful call to Move.>

AARM Note:

The idea is that the internal array is removed from Source and moved to Target.
(See the Implementation Advice for Move).
If Capacity (Target) /= 0, the previous internal array may need to be
deallocated. We don't mention this explicitly, because it is covered by the
"no memory loss" Implementation Requirement.

@xcode<@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Extended_Index;
                  New_Item  : @key{in}     Vector);>

@xindent<If Before is not in the range First_Index (Container) ..
Last_Index (Container) + 1, then Constraint_Error is propagated. If
Length(New_Item) is 0, then Insert does nothing. Otherwise, it computes the new
length @i<NL> as the sum of the current length and Length (New_Item); if the
value of Last appropriate for length @i<NL> would be greater than Index_Type'Last
then Constraint_Error is propagated.>

@xindent<If the current vector capacity is less than or equal to @i<NL>,
Reserve_Capacity (Container, @i<NL>) is called to increase the vector capacity.
Then Insert slides the elements in the range Before .. Last_Index (Container)
up by Length(New_Item) positions, and then copies the elements of New_Item to
the positions starting at Before. Any exception raised during the copying is
propagated.>

AARM Note:
Moving the elements does not necessarily involve copying. Similarly, since
Reserve_Capacity does not require the copying of elements, it does not need to
be explicitly called (the implementation can combine the operations if it
wishes to). [Note to reviewers: I didn't want to duplicate the messy wording
and notes about exceptions not losing anything.]
End AARM Note.

@xcode<@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Vector);>

@xindent<Program_Error is propagated unless Before is equal to No_Element or
designated an element in Target. Otherwise, if Length(New_Item) is 0,
then Insert does nothing.
If Before is No_Element, then the call is equivalent to Insert (Container,
Last_Index (Container) + 1), New_Item); otherwise the call is
equivalent to Insert (Container, To_Index (Before), New_Item);>

AARM Note: The check on Before checks that the cursor does not belong to some
other Container. This check implies that a reference to the container is
included in the cursor value. This wording is not meant to require detection of
dangling cursors; such cursors are defined to be invalid, which means that
execution is erroneous, and any result is allowed (including not raising an
exception).

@xcode<@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Vector;
                  Position  :    @key{out} Cursor);>

@xindent<Program_Error is propagated unless Before is equal to No_Element or
designated an element in Target.
If Before equals No_Element, then
let @i<T> be Last_Index (Container) + 1; otherwise, let @i<T> be
To_Index (Before). Insert (Container, @i<T>, New_Item) is called, and then
Position is set to To_Cursor (Container, @i<T>).>

AARM Note: The messy wording is needed because Before is invalidated by Insert,
and we don't want Position to be invalid after this call. An implementation
probably only needs to copy Before to Position.

@xcode<@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Extended_Index;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1);>

@xindent<Equivalent to Insert (Container, Before, To_Vector (New_Item, Count));>

@xcode<@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1);>

@xindent<Equivalent to Insert (Container, Before, To_Vector (New_Item, Count));>

@xcode<@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Count     : @key{in}     Count_Type := 1);>

@xindent<Equivalent to Insert (Container, Before, To_Vector (New_Item, Count), Position);>

@xcode<@key{procedure} Prepend (Container : @key{in out} Vector;
                   New_Item  : @key{in}     Vector;
                   Count     : @key{in}     Count_Type := 1);>

@xindent<Equivalent to Insert (Container, Index_Type'First, New_Item).>

@xcode<@key{procedure} Prepend (Container : @key{in out} Vector;
                   New_Item  : @key{in}     Element_Type;
                   Count     : @key{in}     Count_Type := 1);>

@xindent<Equivalent to Insert (Container, Index_Type'First, New_Item, Count).>

@xcode<@key{procedure} Append (Container : @key{in out} Vector;
                  New_Item  : @key{in}     Vector);>

@xindent<Equivalent to Insert (Container, Last_Index (Container) + 1, New_Item).>

@xcode<@key{procedure} Append (Container : @key{in out} Vector;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1);>

@xindent<Equivalent to Insert (Container, Last_Index (Container) + 1, New_Item, Count).>

@xcode<@key{procedure} Insert_Space (Container : @key{in out} Vector;
                        Before    : @key{in}     Extended_Index;
                        Count     : @key{in}     Count_Type := 1);>

@xindent<If Before is not in the range First_Index (Container) .. Last_Index (Container)
+ 1, then Constraint_Error is propagated. If Count is 0, then Insert_Space does
nothing. Otherwise, it computes the new length @i<NL> as the sum of the current
length and Count; if the value of Last appropriate for length @i<NL> would be
greater than Index_Type'Last then Constraint_Error is propagated.>

@xindent<If the current vector capacity is less than or equal to @i<NL>,
Reserve_Capacity (Container, @i<NL>) is called to increase the vector capacity.
Then Insert_Space slides the elements in the range Before .. Last_Index
(Container) up by Count positions, and then inserts empty elements in the
positions starting at Before.>

@xcode<@key{procedure} Insert_Space (Container : @key{in out} Vector;
                        Before    : @key{in}     Cursor;
                        Position  :    @key{out} Cursor;
                        Count     : @key{in}     Count_Type := 1);>

@xindent<Program_Error is propagated unless Before is equal to No_Element or
designated an element in Target.
If Before equals No_Element, then let @i<T> be Last_Index (Container) + 1;
otherwise, let @i<T> be To_Index (Before). Insert_Space (Container, @i<T>, Count) is
called, and then Position is set to To_Cursor (Container, @i<T>).>

@xcode<@key{procedure} Set_Length (Container : @key{in out} Vector;
                      Length    : @key{in}     Count_Type);>

@xindent<If Length is larger than the capacity of Container, Set_Length calls
Reserve_Capacity (Container, Length), then sets the length of the
Container to Length. If Length is greater than the original length of
Container, empty elements are added to Container; otherwise elements are
are removed from Container.>

AARM Ramification: No elements are moved by this operation; any new empty
elements are added at the end. This follows from the rules that a cursor
continues to designate the same element unless the the routine is defined
to make the cursor ambiguous or invalid; this operation does not do that.

@xcode<@key{procedure} Delete (Container : @key{in out} Vector;
                  Index     : @key{in}     Extended_Index;
                  Count     : @key{in}     Count_Type := 1);>

@xindent<If Index is not in the range First_Index (Container) .. Last_Index (Container),
then Constraint_Error is propagated. If Count is 0, Delete has no effect.
Otherwise Delete slides the elements (if any) starting at position Index +
Count down to Index. Any exception raised during element assignment is
propagated.>

AARM Note: If Index + Count >= Last_Index(Container), this effectively
truncates the vector (setting Last_Index to Index - 1 and consequently
sets Length to Index - Index_Type'First).

@xcode<@key{procedure} Delete (Container : @key{in out} Vector;
                  Position  : @key{in out} Cursor;
                  Count     : @key{in}     Count_Type := 1);>

@xindent<If Position equals No_Element, then Constraint_Error is propagated. If Position
does not designate an element in Container, then Program_Error is propagated.
Otherwise, Delete (Container, To_Index (Position), Count) is called.>

@xcode<@key{procedure} Delete_First (Container : @key{in out} Vector;
                        Count     : @key{in}     Count_Type := 1);>

@xindent<Equivalent to Delete (Container, Index_Type'First, Count).>

@xcode<@key{procedure} Delete_Last (Container : @key{in out} Vector;
                       Count     : @key{in}     Count_Type := 1);>

@xindent<If Length (Container) <= Count then Delete_Last is equivalent to
Clear (Container). Otherwise it is equivalent to Delete (Container,
Index_Type'Val(Index_Type'Pos(Last_Index(Container)) - Count + 1), Count).>

@xcode<@key{function} First_Index (Container : Vector) @key{return} Index_Type;>

@xindent<Returns the value Index_Type'First.>

AARM Note: We'd rather call this "First", but then calling most routines in
here with First (Some_Vect) would be ambiguous.

@xcode<@key{function} First (Container : Vector) @key{return} Cursor;>

@xindent<If Container is empty, First returns No_Element. Otherwise, it returns a cursor
that designates the first element in Container.>

@xcode<@key{function} First_Element (Container : Vector) @key{return} Element_Type;>

@xindent<Equivalent to Element (Container, First_Index (Container)).>

@xcode<@key{function} Last_Index (Container : Vector) @key{return} Extended_Index;>

@xindent<If Container is empty, Last_Index returns No_Index. Otherwise, it returns the
position of the last element in Container.>

@xcode<@key{function} Last (Container : Vector) @key{return} Cursor;>

@xindent<If Container is empty, Last returns No_Element. Otherwise, it returns a cursor
that designates the last element in Container.>

@xcode<@key{function} Last_Element (Container : Vector) @key{return} Element_Type;>

@xindent<Equivalent to Element (Container, Last_Index (Container)).>

@xcode<@key{procedure} Swap (Container : @key{in} Vector;
                I, J      : @key{in} Index_Type);>

@xindent<If I or J is not in the range First_Index (Container) .. Last_Index (Container),
then Constraint_Error is propagated. Otherwise, Swap exchanges the values of
the elements at positions I and J.>

AARM Notes: To Be Honest: The implementation is not required to actually
copy the elements if it can do the swap some other way. But it is allowed
to copy the elements if needed.

@xcode<@key{procedure} Swap (I, J      : @key{in} Cursor);>

@xindent<If either I or J is No_Element, then Constraint_Error is propagated. If I and J
designate elements in different containers, then Program_Error is propagated.
Otherwise Swap exchanges the values of the elements designated by I and J.>

AARM Notes:
After a call to Swap, I designates the element value previously
designated by J, and J designates the element value previously
designated by I. The cursors do not become ambiguous from this operation.

To Be Honest: The implementation is not required to actually
copy the elements if it can do the swap some other way. But it is allowed
to copy the elements if needed.
End AARM Notes.

@xcode<@key{generic}
   @key{with function} "<" (Left, Right : Element_Type) @key{return} Boolean is <>;
@key{procedure} Generic_Sort (Container : @key{in} Vector);>

@xindent<Reorders the elements of Container such that the elements are
sorted smallest first as determined by the generic formal "<" operator
provided. Any exception raised during evalution of "<" is propagated.>

AARM Notes:
This implies swapping the elements, usually including an intermediate copy.
This means that the elements will usually be copied. (As with Swap,
if the implementation can do this some other way, it is allowed to.) Since
the elements are non-limited, this usually will not be a problem. Note that
there is Implementation Advice below that the implementation should use
a sort that minimizes copying of elements.

The sort is not required to be stable (and the fast algorithm required will
not be stable). If a stable sort is needed, the user can include the original
location of the element as an extra "sort key". We considered requiring the
implementation to do that, but it is mostly extra overhead -- usually there
is something already in the element that provides the needed stability.
End AARM Notes

@xcode<@key{function} Find_Index (Container : Vector;
                     Item      : Element_Type;
                     Index     : Index_Type := Index_Type'First)
   @key{return} Extended_Index;>

@xindent<Searches the elements of Container for an element equal to Item (in the sense
of the generic formal equality operator). The search starts at position Index
and proceeds towards Last_Index (Container). If no equal element is found, then
Find_Index returns No_Index. Otherwise, it returns the index of the first equal
element encountered.>

@xcode<@key{function} Find (Container : Vector;
               Item      : Element_Type;
               Position  : Cursor := No_Element)
   @key{return} Cursor;>

@xindent<If Position is not No_Element, and does not designate an element in Container,
then Program_Error is propagated. Otherwise Find searches the elements of
Container for an element equal to Item (in the sense of the generic formal
equality operator). The search starts at the first element if Cursor equals
No_Element, and at the element designated by Cursor otherwise. It proceeds
towards the last element of Container. If no equal element is found, then
Find returns No_Cursor. Otherwise, it returns a cursor designating the first
equal element encountered.>

@xcode<@key{function} Reverse_Find_Index (Container : Vector;
                             Item      : Element_Type;
                             Index     : Index_Type := Index_Type'Last)
   @key{return} Extended_Index;>

@xindent<Searches the elements of Container for an element equal to Item (in the sense
of the generic formal equality operator). The search starts at position Index
or, if Index is greater than Last_Index (Container), at position Last_Index
(Container). It proceeds towards First_Index (Container). If no equal element
is found, then Reverse_Find_Index returns No_Index. Otherwise, it returns the
index of the first equal element encountered.>

@xcode<@key{function} Reverse_Find (Container : Vector;
                       Item      : Element_Type;
                       Position  : Cursor := No_Element)
   @key{return} Cursor;>

@xindent<If Position is not No_Element, and does not designate an element in Container,
then Program_Error is propagated. Otherwise Reverse_Find searches the elements
of Container for an element equal to Item (in the sense of the generic formal
equality operator). The search starts at the last element if Cursor equals
No_Element, and at the element designated by Cursor otherwise. It proceeds
towards the first element of Container. If no equal element is found, then
Reverse_Find returns No_Cursor. Otherwise, it returns a cursor designating
the first equal element encountered.>

@xcode<@key{function} Contains (Container : Vector;
                   Item      : Element_Type) @key{return} Boolean;>

@xindent<Equivalent to Has_Element (Find (Container, Item)).>

@xcode<@key{function} Next (Position : Cursor) @key{return} Cursor;>

@xindent<If Position equals No_Element or designates the last element of the container,
then Next returns the value No_Element. Otherwise, returns a cursor that
designates the element with index To_Index (Position) + 1 in the same vector as
Position.>

@xcode<@key{function} Previous (Position : Cursor) @key{return} Cursor;>

@xindent<If Position equals No_Element or designates the first element of the container,
then Previous returns the value No_Element. Otherwise, returns a cursor that
designates the element with index (To_Index (Position) - 1) in the same vector
as Position.>

@xcode<@key{procedure} Next (Position : @key{in out} Cursor);>

@xindent<Equivalent to Position := Next (Position).>

@xcode<@key{procedure} Previous (Position : @key{in out} Cursor);>

@xindent<Equivalent to Position := Previous (Position).>

@xcode<@key{function} Has_Element (Position : Cursor) @key{return} Boolean;>

@xindent<Returns True if Position designates an element, and returns False otherwise.>

AARM Note: To Be Honest: This function may not detect cursors that
designate deleted elements; such cursors are invalid (see below) and any
use of them (including in this routine) is erroneous.

@xcode<@key{procedure} Iterate
  (Container : @key{in} Vector;
   Process   : @key{not null access} @key{procedure} (Position : @key{in} Cursor));>

@xindent<Invokes Process.@key{all} with a cursor that designates each element in Container, in
index order. Program_Error is propagated if Process.@key{all} tampers with the
cursors of Container. Any exception raised by Process is propagated.>

AARM Note:
The purpose of the tamper with cursors check is to prevent erroneous execution
from the Position parameter of Process.all becoming invalid. This check takes
place when the operations that tamper with the cursors of the container are
called. The check cannot be made later (say in the body of Iterate), because
that could cause the Position cursor to be invalid and potentially cause
execution to become erroneous -- defeating the purpose of the check.

There is no check needed if an attempt is made to insert or delete
nothing (that is, Count = 0 or Length(Item) = 0).

The check is easy to implement: each container needs a counter. The counter
is incremented when Iterate is called, and decremented when Iterate completes.
If the counter is nonzero when an operation that inserts or deletes is called,
Finalize is called, or one of the other operations in the list occurs,
Program_Error is raised.

Swap and Generic_Sort are not included here, as they only copy elements.
End AARM Notes.

@xcode<@key{procedure} Reverse_Iterate
  (Container : @key{in} Vector;
   Process   : @key{not null access} @key{procedure} (Position : @key{in} Cursor));>

@xindent<Iterates over the nodes in Container as per Iterate, except that elements are
traversed in reverse index order.>

@i<@s8<Bounded (Run-Time) Errors>>

Reading the value of an empty element by calling Element, Query_Element,
Update_Element, Generic_Sort, "=", Find, or Reverse_Find is a bounded error.
The implementation may treat the element as having any valid value of the
element type, or raise Constraint_Error or Program_Error before modifying the
vector.

AARM Notes: For instance, a default initialized element could be returned. Or
some previous value of an element. But returning random junk is not allowed
if the type has default initial value(s).

Assignment and streaming of empty elements are NOT bounded errors.
This is consistent with regular composite types, for which assignment and
streaming of uninitialized components do not cause a bounded error, but reading
the uninitialized component does cause a bounded error.

There are other operations which are defined in terms of the operations listed
above.
End AARM Notes.

A Cursor value is @i<ambiguous> if any of the following have occurred since it
was created:

@xbullet<Insert, Insert_Space, or Delete has been called on the vector that
contains the element the
cursor designates with an index value (or a cursor designating an element at
such an index value) less than or equal to the index value of the element
designated by the cursor;>
@xbullet<The vector that contains the element it designates has been passed
to an instance of Generic_Sort.>

It is a bounded error to call any subprogram other than "=" or Has_Element
declared in Containers.Vectors with an ambiguous (but not invalid, see below)
cursor parameter. Possible results are:

@xbullet<The cursor may be treated as if it was No_Element;>
@xbullet<The cursor may designate some element in the vector (but not
necessarily the element that it originally designated);>
@xbullet<Constraint_Error may be raised; or>
@xbullet<Program_Error may be raised.>

AARM Note: Cursors are made ambiguous if an Insert or Delete occurs that moves
the elements in the internal array including the designated ones. After such an
operation, the cursor probably still designates an element (although it might
not after a deletion), but it is a *different* element. That violates the
definition of cursor -- it designates a particular element.

For "=" or Has_Element, the cursor works normally (it would not be No_Element).
We don't want to trigger an exception simply for comparing a bad cursor.

While it is possible to check for or prevent these cases, in many cases the
overhead necessary to make the check (or prevent the problems) is substantial
in time or space.
End AARM Notes.

@i<@s8<Erroneous Execution>>

A Cursor value is @i<invalid> if any of the following have occurred since it
was created:

@xbullet<The vector that contains the element it designates has been finalized;>
@xbullet<The vector that contains the element it designates has been used as
the Source or Target of a call to Move;>
@xbullet<The element it designates has been deleted.>

The result of "=" or Has_Element is unspecified if it is called with an
invalid cursor parameter. Execution is erroneous if any other subprogram
declared in Containers.Vectors is called with an invalid cursor parameter.

AARM Notes: The list above (combined with the bounded error cases) is intended
to be exhaustive. In other cases, a cursor value continues to designate its
original element. For instance, cursor values survive the appending of new
elements.
End AARM Notes.

@i<@s8<Implementation Requirements>>

No storage associated with a vector object shall be lost upon assignment or
scope exit.

@i<@s8<Implementation Advice>>

Containers.Vectors should be implemented similarly to an array. In particular,
if the length of a vector is @i<N>, then

@xbullet<the worst-case time complexity of Append with Count=1 and Element
should be O(log @i<N>);>
@xbullet<the worst-case time complexity of Prepend with Count=1 or Delete_First
with Count=1 of the vector should be O(@i<N> log @i<N>).>

AARM Note
We do not mean to overly constrain implementation strategies here. However, it
is important for portability that the performance of large containers has
roughly the same factors on different implementations. If a program is moved
to an implementation that takes O(N) time to access elements, that program
could be unusable when the vectors are large. We allow O(log N) access because
the proportionality constant and caching effects are likely to be larger than
the log factor, and we don't want to discourage innovative implementations.

The worst-case time complexity of a call on an instantiation of
Containers.Vectors.Generic_Sort should be O(@i<N>**2), and
the average time complexity should be better than O(@i<N>**2).

AARM Note
In other words, we're requiring the use of a better than O(N**2) sorting
algorithm, such as Quicksort. No Bubble sorts allowed!

Containers.Vectors.Generic_Sort should minimize copying of elements.

AARM Note - To Be Honest
We do not mean "absolutely minimize" here; we're not intending to require a
single copy for each element. Rather, we want to suggest that the sorting
algorithm chosen is one that does not copy items unnecessarily. Bubble sort
would not meet this advice, for instance.

Move should not copy elements, and should minimize copying of internal
data structures.

AARM Note: Usually that can be accomplishing simply by moving the pointer(s) to
the internal data structures from the Source vector to the Target vector.

@xindent<@s9<NOTES:@hr
41 All elements of a vector occupy locations in the internal array.
If a sparse container is required, a Hashed_Map should be used rather than a
vector.
@hr
42 If Index_Type'Base'First = Index_Type'First an instantiation of
Ada.Containers.Vectors will raise Constraint_Error. A value below
Index_Type'First is required so that an empty vector has a meaningful
value of Last_Index.>>

AARM Note: This property is the main reason why only integer types (as opposed
to any discrete type) are allowed as the index type of a vector. An enumeration
or modular type would require a subtype in order to meet this requirement.

!corrigendum A.18.3

@dinsc

The language-defined generic package Containers.Doubly_Linked_Lists provides
private types List and Cursor, and a set of operations for each type. A
list container is optimized for insertion and deletion at any position.

A doubly-linked list container object manages a linked list of internal
@i<nodes>, each of which contains an element and pointers to the
next (successor) and previous (predecessor) internal nodes. A cursor
designates a particular node within a list (and by extension the element
contained in that node). A cursor keeps designating the same node (and element)
as long as the node is part of the container, even if the node is moved in the
container.

The @i<length> of a list is the number of elements it contains.

@i<@s8<Static Semantics>>

The generic library package Containers.Doubly_Linked_Lists has the following
declaration:

@xcode<@key{generic}
   @key{type} Element_Type @key{is private};
   @key{with function} "=" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Doubly_Linked_Lists @key{is}
   @key{pragma} Preelaborate (Doubly_Linked_Lists);

   @key{type} List @key{is tagged private};

   @key{type} Cursor @key{is private};

   Empty_List : @key{constant} List;

   No_Element : @key{constant} Cursor;

   @key{function} "=" (Left, Right : List) @key{return} Boolean;

   @key{function} Length (Container : List) @key{return} Natural;

   @key{function} Is_Empty (Container : List) @key{return} Boolean;

   @key{procedure} Clear (Container : @key{in out} List);

   @key{function} Element (Position : Cursor)
      @key{return} Element_Type;

   @key{procedure} Query_Element
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Element : @key{in} Element_Type));

   @key{procedure} Update_Element
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Element : @key{in out} Element_Type));

   @key{procedure} Replace_Element (Position : @key{in} Cursor;
                              By       : @key{in} Element_Type);

   @key{procedure} Move (Target : @key{in out} List;
                   Source : @key{in out} List);

   @key{procedure} Prepend (Container : @key{in out} List;
                      New_Item  : @key{in}     Element_Type;
                      Count     : @key{in}     Count_Type := 1);

   @key{procedure} Append (Container : @key{in out} List;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1);

   @key{procedure} Insert (Container : @key{in out} List;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Element_Type;
                     Count     : @key{in}     Count_Type := 1);

   @key{procedure} Insert (Container : @key{in out} List;
                     Before    : @key{in}     Cursor;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Count     : @key{in}     Count_Type := 1);

   @key{procedure} Insert (Container : @key{in out} List;
                     Before    : @key{in}     Cursor;
                     Position  :    @key{out} Cursor;
                     Count     : @key{in}     Count_Type := 1);

   @key{procedure} Delete (Container : @key{in out} List;
                     Position  : @key{in out} Cursor;
                     Count     : @key{in}     Count_Type := 1);

   @key{procedure} Delete_First (Container : @key{in out} List;
                           Count     : @key{in}     Count_Type := 1);

   @key{procedure} Delete_Last (Container : @key{in out} List;
                          Count     : @key{in}     Count_Type := 1);

   @key{generic}
      @key{with function} "<" (Left, Right : Element_Type)
         @key{return} Boolean is <>;
   @key{procedure} Generic_Sort (Container : @key{in out} List);

   @key{generic}
      @key{with function} "<" (Left, Right : Element_Type)
         @key{return} Boolean @key{is} <>;
   @key{procedure} Generic_Merge (Target  : @key{in out} List;
                            Source  : @key{in out} List);

   @key{procedure} Reverse_List (Container : @key{in out} List);

   @key{procedure} Swap (I, J  : @key{in} Cursor);

   @key{procedure} Swap_Links (Container : @key{in out} List;
                         I, J      : @key{in}     Cursor);

   @key{procedure} Splice (Target   : @key{in out} List;
                     Before   : @key{in}     Cursor;
                     Source   : @key{in out} List);

   @key{procedure} Splice (Target   : @key{in out} List;
                     Before   : @key{in}     Cursor;
                     Position : @key{in}     Cursor);

   @key{procedure} Splice (Target   : @key{in out} List;
                     Before   : @key{in}     Cursor;
                     Source   : @key{in out} List;
                     Position : @key{in}     Cursor);

   @key{function} First (Container : List) @key{return} Cursor;

   @key{function} First_Element (Container : List)
      @key{return} Element_Type;

   @key{function} Last (Container : List) @key{return} Cursor;

   @key{function} Last_Element (Container : List)
      @key{return} Element_Type;

   @key{function} Contains (Container : List;
                      Item      : Element_Type) @key{return} Boolean;

   @key{function} Find (Container : List;
                  Item      : Element_Type;
                  Position  : Cursor := No_Element)
      @key{return} Cursor;

   @key{function} Reverse_Find (Container : List;
                          Item      : Element_Type;
                          Position  : Cursor := No_Element)
      @key{return} Cursor;

   @key{function} Next (Position : Cursor) @key{return} Cursor;

   @key{function} Previous (Position : Cursor) @key{return} Cursor;

   @key{procedure} Next (Position : @key{in out} Cursor);

   @key{procedure} Previous (Position : @key{in out} Cursor);

   @key{function} Has_Element (Position : Cursor) @key{return} Boolean;

   @key{procedure} Iterate
     (Container : @key{in} List;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));

   @key{procedure} Reverse_Iterate
     (Container : @key{in} List;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));

@key{private}

   ... -- @RI[not specified by the language]

@key{end} Ada.Containers.Doubly_Linked_Lists;>

The type List is used to represent lists. The type List needs finalization (see
7.6).

Empty_List represents the empty List object. It has a length of 0. If an object
of type List is not otherwise initialized, it is initialized to the same value
as Empty_List.

No_Element represents a cursor that designates no element. If an object of type
Cursor is not otherwise initialized, it is initialized to the same value as
No_Element.

Some operations are assumed to work on a constant set of elements. For such
an operation, a subprogram is said to @i<tamper with cursors> of a list object
@i<L> if:

@xbullet<it inserts or deletes elements of @i<L>, that is, it calls the Insert,
Clear, Delete, or Delete_Last procedures with @i<L> as a parameter; or>

   AARM To Be Honest: Operations which are defined to be equivalent to
   a call on one of these operations also are included. Similarly, operations
   which call one of these as part of their definition are included.

@xbullet<it reorders the elements of @i<L>, that is, it calls the Splice,
Swap_Links, or Reverse_List procedures or an instance of Generic_Sort or
Generic_Merge with C as a parameter; or>

@xbullet<it finalizes @i<L>; or>
@xbullet<it calls the Move procedure with @i<L> as a parameter.>

Some operations are assumed to not change elements. For such an operation, a
subprogram is said to @i<tamper with elements> of a list object @i<L> if:

@xbullet<it tampers with cursors of @i<L>; or>
@xbullet<it modifies one or more elements of @i<L>, that is, it calls the
Replace_Element, Update_Element, or Swap procedures with @i<L> as a parameter.>

   AARM Note:
   Swap copies elements rather than reordering them, so it can be allowed
   for Iterate.

@xcode<@key{function} "=" (Left, Right : List) @key{return} Boolean;>

@xindent<If Left and Right denote the same list object, then the function returns True.
If Left and Right have different lengths, then the function returns False.
Otherwise, it compares each element in Left to the corresponding element in
Right using the generic formal equality operator; if element equality returns
False, then the function returns False. If the function has not returned a
result after checking all of the elements, it returns True. Any exception
raised during evaluation of element equality is propagated.>

@xcode<@key{function} Length (Container : List) @key{return} Count_Type;>

@xindent<Returns the number of elements in Container.>

@xcode<@key{function} Is_Empty (Container : List) @key{return} Boolean;>

@xindent<Equivalent to Length (Container) = 0.>

@xcode<@key{procedure} Clear (Container : @key{in out} List);>

@xindent<Removes all the elements from Container.>

@xcode<@key{function} Element (Position : Cursor) @key{return} Element_Type;>

@xindent<If Position equals No_Element, then Constraint_Error is propagated.
Otherwise, Element returns the element designated by Position.>

@xcode<@key{procedure} Query_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access procedure} (Element : @key{in} Element_Type));>

@xindent<If Position equals No_Element, then Constraint_Error is propagated.
Otherwise, Query_Element calls Process.@key{all} with the element on node designated by
Position as the argument. Program_Error is propagated if Process.@key{all} tampers
with the elements of Container. Any exception raised by Process.@key{all} is propagated.>

@xcode<@key{procedure} Update_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access procedure} (Element : @key{in out} Element_Type));>

@xindent<If Position equals No_Element, then Constraint_Error is propagated. Otherwise,
Update_Element calls Process.@key{all} with the element on node designated by
Position as the argument. Program_Error is propagated if Process.@key{all} tampers
with the elements of Container. Any exceptions raised by Process.@key{all} are propagated.>

@xindent<If Element_Type is unconstrained and definite, then the Element parameter
of Process.@key{all} shall be unconstrained.>

AARM Note: This means that the elements cannot be directly allocated from the
heap (nor aliased unless AI-363 is included in the Amendment); it must be
possible to change the discriminants of the element in place.

@xcode<@key{procedure} Replace_Element (Position : Cursor;
                           By       : Element_Type);>

@xindent<If Position equals No_Element, then Constraint_Error is propagated. Otherwise
Replace_Element assigns the value By to the element designated by
Position.>

@xcode<@key{procedure} Move (Target : @key{in out} List;
                Source : @key{in out} List);>

@xindent<If Target denotes the same object as Source, then Move has no
effect. Otherwise, Move first calls Clear (Target). Then, the nodes in Source
are moved to Target (in the original order). The length of Target is set to the
length of Source, and the length of Source is set to 0.>

@xcode<@key{procedure} Prepend (Container : @key{in out} List;
                   New_Item  : @key{in}     Element_Type;
                   Count     : @key{in}     Count_Type := 1);>

@xindent<Equivalent to Insert (Container, First (Container), New_Item, Count).>

@xcode<@key{procedure} Append (Container : @key{in out} List;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1);>

@xindent<Equivalent to Insert (Container, No_Element, New_Item, Count).>

@xcode<@key{procedure} Insert (Container : @key{in out} List;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Element_Type;
                  Count     : @key{in}     Count_Type := 1);>

@xindent<Program_Error is propagated unless Before is equal to No_Element or
designated an element in Container.
Otherwise, Insert inserts Count copies of
New_Item prior to the element designated by Before. If Before equals
No_Element, the new elements are inserted after the last node (if any). Any
exception raised during allocation of internal storage is propagated, and
Container is not modified.>

AARM Note: The check on Before checks that the cursor does not belong to some
other Container. This check implies that a reference to the container is
included in the cursor value. This wording is not meant to require detection of
dangling cursors; such cursors are defined to be invalid, which means that
execution is erroneous, and any result is allowed (including not raising an
exception).

@xcode<@key{procedure} Insert (Container : @key{in out} List;
                  Before    : @key{in}     Cursor;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Count     : @key{in}     Count_Type := 1);>

@xindent<Program_Error is propagated unless Before is equal to No_Element or
designated an element in Container.
Otherwise, Insert allocates Count copies of
New_Item, and inserts them prior to the element designated by Before. If Before
equals No_Element, the new elements are inserted after the last element (if
any). Position designates the first newly-inserted element. Any exception
raised during allocation of internal storage is propagated, and Container is
not modified.>

@xcode<@key{procedure} Insert (Container : @key{in out} List;
                  Before    : @key{in}     Cursor;
                  Position  :    @key{out} Cursor;
                  Count     : @key{in}     Count_Type := 1);>

@xindent<Program_Error is propagated unless Before is equal to No_Element or
designated an element in Container.
Otherwise, Insert inserts Count new elements
prior to the element designated by Before. If Before equals No_Element, the new
elements are inserted after the last node (if any). The new elements are
initialized with any implicit initial value for any part (as for an
object_declaration with no initialization expression - see 3.3.1). Any
exception raised during allocation of internal storage is propagated, and
Container is not modified.>

@xcode<@key{procedure} Delete (Container : @key{in out} List;
                  Position  : @key{in out} Cursor;
                  Count     : @key{in}     Count_Type := 1);>

@xindent<If Position equals No_Element, then Constraint_Error is propagated. If Position
does not designate an element in Container, then Program_Error is propagated.
Otherwise Delete removes (from Container) Count elements starting at the
element designated by Position (or all of the elements if there are less than
Count elements starting at Position).>

@xcode<@key{procedure} Delete_First (Container : @key{in out} List;
                        Count     : @key{in}     Count_Type := 1);>

@xindent<Equivalent to Delete (Container, First (Container), Count).>

@xcode<@key{procedure} Delete_Last (Container : @key{in out} List;
                       Count     : @key{in}     Count_Type := 1);>

@xindent<If Length (Container) <= Count then Delete_Last is equivalent to Clear
(Container). Otherwise it removes the last Count nodes from Container.>

@xcode<@key{generic}
   @key{with function} "<" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
@key{procedure} Generic_Sort (Container : @key{in out} List);>

@xindent<Reorders the nodes of Container such that the elements are
sorted smallest first as determined by the generic formal "<" operator
provided. The sort must be stable. Any exception raised during evaluation of
"<" is propagated.>

AARM Notes
Unlike array sorts, we do require stable sorts here. That's because
algorithms in the merge sort family (as described by Knuth) can be both
fast and stable. Such sorts use the extra memory as offered by
the links to provide better performance.

Note that list sorts never copy elements; it is the nodes, not the elements,
that are reordered.
End AARM Notes

@xcode<@key{generic}
   @key{with function} "<" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
@key{procedure} Generic_Merge (Target  : @key{in out} List;
                         Source  : @key{in out} List);>

@xindent<Generic_Merge removes elements from Source and inserts them into Target so that
Target be sorted smallest first as determined by the generic formal "<"
operator.>

@xindent<Any exception raised during evaluation of "<" is propagated. If Target and
Source are not sorted smallest first, then Program_Error is propagated. In
these cases, Target is left in an unspecified order, but contains the union of
the elements that were initially in Source and Target; Source is left empty.>

AARM Note:
If Program_Error is propagated by Generic_Merge because one of the lists was
unsorted, it is possible to recover by sorting Target. If Source and Target
designate the same object, Generic_Merge is essentially a no-op, but it still
has to check that the list is sorted.

@xcode<@key{procedure} Reverse_List (Container : @key{in out} List);>

@xindent<Reorders the elements of Container in reverse order.>

@xcode<@key{procedure} Swap (I, J  : @key{in} Cursor);>

@xindent<If either I or J is No_Element, then Constraint_Error is propagated. If I and J
designate elements in different containers, then Program_Error is propagated.
Otherwise Swap exchanges the values of the elements designated by I and J.>

AARM Notes:
After a call to Swap, I designates the element value previously
designated by J, and J designates the element value previously
designated by I. The cursors do not become ambiguous from this operation.

AARM Notes: To Be Honest: The implementation is not required to actually
copy the elements if it can do the swap some other way. But it is allowed
to copy the elements if needed.

@xcode<@key{procedure} Swap_Links (Container : @key{in out} List;
                      I, J      : @key{in}     Cursor);>

@xindent<If either I or J is No_Element, then Constraint_Error is propagated. If I or J
do not designate an element in Container, then Program_Error is propagated.
Otherwise, Swap_Links exchanges the nodes designated by I and J.>

AARM Note: Unlike Swap, this exchanges the nodes, not the
elements. No copying is performed. I and J designate the same elements after
this call as they did before it. This operation can provide better performance
than Swap if the element size is large.

@xcode<@key{procedure} Splice (Target   : @key{in out} List;
                  Before   : @key{in}     Cursor;
                  Source   : @key{in out} List);>

@xindent<Program_Error is propagated unless Before is equal to No_Element or
designated an element in Target. Otherwise, if Source denotes the
same object as Target, the
operation has no effect. Otherwise, Splice reorders elements such that they are
removed from Source and moved to Target, immediately prior to Before. If Before
equals No_Element, the nodes of Source are spliced after the last node of
Target. The length of Target is incremented by the number of nodes in Source,
and the length of Source is set to 0.>

@xcode<@key{procedure} Splice (Target   : @key{in out} List;
                  Before   : @key{in}     Cursor;
                  Position : @key{in}     Cursor);>

@xindent<If either of Before or Position is not No_Element, and does not designate an
element in Target, then Program_Error is propagated. If Position equals
No_Element, or if Position equals Before, or if the successor of Position
equals Before, the operation has no effect. Otherwise the element designated by
Position is moved immediately prior to Before, or, if Before equals No_Element,
after the last element.>

@xcode<@key{procedure} Splice (Target   : @key{in out} List;
                  Before   : @key{in}     Cursor;
                  Source   : @key{in out} List;
                  Position : @key{in}     Cursor);>

@xindent<If Position is No_Element then Constraint_Error is propagated. If Before does
not equal No_Element, and does not designate an element in Target, then
Program_Error is propagated. If Position does not equal No_Element, and does
not designate a node in Source, then Program_Error is propagated. If Source
denotes the same object as Target, then Splice is equivalent to Splice (Target,
Before, Position). Otherwise the element designated by Position is removed from
Source and moved to Target, immediately prior to Before, or, if Before equals
No_Element, after the last element of Target. The length of Target is
incremented, and the length of Source is decremented.>

@xcode<@key{function} First (Container : List) @key{return} Cursor;>

@xindent<If Container is empty, First returns the value No_Element. Otherwise it returns
a cursor that designates the first node in Container.>

@xcode<@key{function} First_Element (Container : List) @key{return} Element_Type;>

@xindent<Equivalent to Element (First (Container)).>

@xcode<@key{function} Last (Container : List) @key{return} Cursor;>

@xindent<If Container is empty, Last returns the value No_Element. Otherwise it returns a
cursor that designates the last node in Container.>

@xcode<@key{function} Last_Element (Container : List) @key{return} Element_Type;>

@xindent<Equivalent to Element (Last (Container)).>

@xcode<@key{function} Contains (Container : List;
                   Item      : Element_Type) @key{return} Boolean;>

@xindent<Equivalent to Find (Container, Item) /= No_Element.>

@xcode<@key{function} Find (Container : List;
               Item      : Element_Type;
               Position  : Cursor := No_Element)
  @key{return} Cursor;>

@xindent<If Position is not No_Element, and does not designate an element
in Container, then Program_Error is propagated. Find searches the elements of
Container for an element equal to Item (in the sense of the generic formal
equality operator). The search starts at the element designated by Position, or
at the first element if Position equals No_Element. It proceeds towards Last
(Container). If no equal element is found, then Find returns No_Element.
Otherwise, it returns a cursor designating the first equal element encountered.>

@xcode<@key{function} Reverse_Find (Container : List;
                       Item      : Element_Type;
                       Position  : Cursor := No_Element)
   @key{return} Cursor;>

@xindent<If Position is not No_Element, and does not designate an element
in Container, then Program_Error is propagated. Find searches the elements of
Container for an element equal to Item (in the sense of the generic formal
equality operator). The search starts at the element designated by Position, or
at the lastelement if Position equals No_Element. It proceeds towards First
(Container). If no equal element is found, then Reverse_Find returns
No_Element. Otherwise, it returns a cursor designating the first equal element
encountered.>

@xcode<@key{function} Next (Position : Cursor) @key{return} Cursor;>

@xindent<If Position equals No_Element or designates the last element of the container,
then Next returns the value No_Element. Otherwise, it returns a cursor that
designates the successor of the element designated by Position.>

@xcode<@key{function} Previous (Position : Cursor) @key{return} Cursor;>

@xindent<If Position equals No_Element or designates the first element of the container,
then Previous returns the value No_Element. Otherwise, it returns a cursor that
designates the predecessor of the element designated by Position.>

@xcode<@key{procedure} Next (Position : @key{in out} Cursor);>

@xindent<Equivalent to Position := Next (Position).>

@xcode<@key{procedure} Previous (Position : @key{in out} Cursor);>

@xindent<Equivalent to Position := Previous (Position).>

@xcode<@key{function} Has_Element (Position : Cursor) @key{return} Boolean;>

@xindent<Returns True if Position designates an element, and returns False otherwise.>

AARM Note: To Be Honest: This function may not detect cursors that
designate deleted elements; such cursors are invalid (see below) and any
use of them (including in this routine) is erroneous.

@xcode<@key{procedure} Iterate
  (Container : @key{in} List;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor));>

@xindent<Iterate calls Process.@key{all} with a cursor that designates each node in Container,
starting with the first node and moving the cursor as per the Next function.
Program_Error is propagated if Process.@key{all} tampers with the cursors of
Container. Any exception raised by Process.@key{all} is propagated.>

AARM Note:
The purpose of the tamper with cursors check is to prevent erroneous execution
from the Position parameter of Process.all becoming invalid. This check takes
place when the operations that tamper with the cursors of the container are
called. The check cannot be made later (say in the body of Iterate), because
that could cause the Position cursor to be invalid and potentially cause
execution to become erroneous -- defeating the purpose of the check.

See Iterate for vectors for a suggested implementation of the check.
End AARM Notes.

@xcode<@key{procedure} Reverse_Iterate
  (Container : @key{in} List;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor));>

@xindent<Iterates over the nodes in Container as per Iterate, except that elements are
traversed in reverse order, starting with the last node and moving the cursor
as per the Previous function.>

@i<@s8<Erroneous Execution>>

A Cursor value is @i<invalid> if any of the following have occurred since it was
created:
@xbullet<The list that contains the element it designates has been finalized;>
@xbullet<The list that contains the element it designates has been used as the
    Source or Target of a call to Move;>
@xbullet<The element it designates has been deleted.>

The result of "=" or Has_Element is unspecified if it is called with an invalid
cursor parameter. Execution is erroneous if any other subprogram declared in
Containers.Doubly_Linked_Lists is called with an invalid cursor parameter.

AARM Notes: The list above is intended to be exhaustive. In other cases, a
cursor value continues to designate its original element. For instance,
cursor values survive the insertion and deletion of other nodes.

While it is possible to check for these cases, in many cases the overhead
necessary to make the check is substantial in time or space. Implementations
are encouraged to check for as many of these cases as possible and raise
Program_Error if detected.
End AARM Notes.

@i<@s8<Implementation Requirements>>

No storage associated with a doubly-linked List object shall be lost
upon assignment or scope exit.

@i<@s8<Implementation Advice>>

Containers.Doubly_Linked_Lists should be implemented similarly to a linked
list. In particular, if @i<N> is the length of a list, then the worst-case time
complexity of Element, Insert with Count=1, and Delete with Count=1 should be
O(log @i<N>).

AARM Note
We do not mean to overly constrain implementation strategies here. However, it
is important for portability that the performance of large containers has
roughly the same factors on different implementations. If a program is moved to
an implementation that takes O(N) time to access elements, that program could
be unusable when the lists are large. We allow O(log N) access because the
proportionality constant and caching effects are likely to be larger than the
log factor, and we don't want to discourage innovative implementations.

The worst-case time complexity of a call on an instantiation of
Containers.Doubly_Linked_Lists.Generic_Sort should be O(@i<N>**2), and
the average time complexity should be better than O(@i<N>**2).

AARM Note
In other words, we're requiring the use of a better than O(N**2) sorting
algorithm, such as Quicksort. No Bubble sorts allowed!

Move should not copy elements, and should minimize copying of internal
data structures.

AARM Note: Usually that can be accomplishing simply by moving the pointer(s) to
the internal data structures from the Source container to the Target container.

@xindent<@s9<NOTES:@hr
43 Sorting a list never copies elements, and is a stable sort (equal elements
remain in the original order). This is different than sorting an array or
vector, which may need to copy elements, and is probably not a stable sort.>>

!corrigendum A.18.4

@dinsc
The language-defined generic packages Containers.Hashed_Maps and
Containers.Ordered_Maps provide private types Map and Cursor, and a set of
operations for each type. A map container allows an arbitrary type to be used
as a key to find the element associated with that key. A hashed map uses a hash
function to organize the keys, while an ordered map orders the keys per a
specified relation.

This section describes the declarations that are common to both kinds of maps.
See A.18.5 for a description of the semantics specific to Containers.Hashed_Maps
and A.18.6 for a description of the semantics specific to
Containers.Ordered_Maps.

@i<@s8<Static Semantics>>

The type Map is used to represent maps. The type Map needs finalization (see
7.6).

A map contains pairs of keys and elements, called @i<nodes>. Map cursors
designate nodes, but also can be thought of as designating an element (the
element contained in the node) for consistency with the other containers. There
exists an equivalence relation on keys, whose definition is different for hashed
maps and ordered maps. A map never contains two or more nodes with equivalent
keys. The @i<length> of a map is the number of nodes it contains.

Each nonempty map has two particular nodes called the @i<first node> and the
@i<last node> (which may be the same). Each node except for the last node has a
@i<successor node>. If there are no other intervening operations, starting with
the first node and repeatedly going to the successor node will visit each node
in the map exactly once until the last node is reached. The exact definition of
these terms is different for hashed maps and ordered maps.

Some operations are assumed to work on a constant set of elements. For such
an operation, a subprogram is said to @i<tamper with cursors> of a map object @i<M>
if:

@xbullet<it inserts or deletes elements of @i<M>, that is, it calls the Insert,
Include, Clear, Delete, or Exclude procedures with @i<M> as a parameter; or>

   AARM To Be Honest: Operations which are defined to be equivalent to
   a call on one of these operations also are included. Similarly, operations
   which call one of these as part of their definition are included.

@xbullet<it finalizes @i<M>; or>

@xbullet<it calls the Move procedure with @i<M> as a parameter; or>

@xbullet<it calls one of the operations defined to tamper with the cursors of @i<M>.>

Some operations are assumed to not change elements. For such an operation, a
subprogram is said to @i<tamper with elements> of a map object @i<M> if:

@xbullet<it tampers with cursors of @i<M>; or>

@xbullet<it modifies one or more elements of @i<M>, that is, it calls the
Replace, Replace_Element, or Update_Element procedures with @i<M> as a
parameter.>

   AARM Note:
   Replace only modifies a key and element rather than rehashing, so it can be
   allowed for Iterate.

Empty_Map represents the empty Map object. It has a length of 0. If an object
of type Map is not otherwise initialized, it is initialized to the same
value as Empty_Map.

No_Element represents a cursor that designates no node. If an object of type
Cursor is not otherwise initialized, it is initialized to the same
value as No_Element.

@xcode<@key{function} "=" (Left, Right : Map) @key{return} Boolean;>

@xindent<If Left and Right denote the same map object, then the function returns True. If
Left and Right have different lengths, then the function returns False.
Otherwise, for each key @i<K> in Left, the function returns False if:>

@xinbull<a key equivalent to @i<K> is not present in Right; or>

@xinbull<the element associated with @i<K> in Left is not equal to the element
associated with @i<K> in Right (using the generic formal equality operator for
elements).>

@xindent<If the function has not returned a result after checking all of the keys, it
returns True. Any exception raised during evaluation of key equivalence or
element equality is propagated.>

@xcode<@key{function} Length (Container : Map) @key{return} Count_Type;>

@xindent<Returns the number of nodes in Container.>

@xcode<@key{function} Is_Empty (Container : Map) @key{return} Boolean;>

@xindent<Equivalent to Length (Container) = 0.>

@xcode<@key{procedure} Clear (Container : @key{in out} Map);>

@xindent<Removes all the nodes from Container.>

@xcode<@key{function} Key (Position : Cursor) @key{return} Key_Type;>

@xindent<If Position equals No_Element, then Constraint_Error is propagated. Otherwise,
Key returns the key component of the node designated by Position.>

@xcode<@key{function} Element (Position : Cursor) @key{return} Element_Type;>

@xindent<If Position equals No_Element, then Constraint_Error is propagated. Otherwise,
Element returns the element component of the node designated by Position.>

@xcode<@key{procedure} Query_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access procedure} (Key     : @key{in} Key_Type;
                                         Element : @key{in} Element_Type));>

@xindent<If Position equals No_Element, then Constraint_Error is propagated. Otherwise,
Query_Element calls Process.@key{all} with the key and element from the node
designated by Position as the arguments. Program_Error is propagated if
Process.@key{all} tampers with the elements of Container. Any exceptions raised by
Process.@key{all} are propagated.>

@xcode<@key{procedure} Update_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access procedure} (Key     : @key{in} Key_Type;
                                         Element : @key{in out} Element_Type));>

@xindent<If Position equals No_Element, then Constraint_Error is propagated. Otherwise,
Update_Element calls Process.@key{all} with the key and element from the node
designated by Position as the arguments. Program_Error is propagated if
Process.@key{all} tampers with the elements of Container. Any exceptions raised
by Process.@key{all} are propagated.>

@xindent<If Element_Type is unconstrained and definite, then the Element parameter
of Process.@key{all} shall be unconstrained.>

AARM Note: This means that the elements cannot be directly allocated from the
heap (nor aliased unless AI-363 is included in the Amendment); it must be
possible to change the discriminants of the element in place.

@xcode<@key{procedure} Replace_Element (Position : @key{in} Cursor;
                           By       : @key{in} Element_Type);>

@xindent<If Position equals No_Element, then Constraint_Error is propagated.
Otherwise Replace_Element assigns By to the element of the node designated by
Position.>

@xcode<@key{procedure} Move (Target : @key{in out} Map;
                Source : @key{in out} Map);>

@xindent<If Target denotes the same object as Source, then Move has no effect.
Otherwise, Move first calls Clear (Target). Then, each node from Source is
removed from Source and inserted into Target. The length of Source is 0 after a
successful call to Move.>

@xcode<@key{procedure} Insert (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean);>

@xindent<Insert checks if a node with a key equivalent to Key is already present in
Container. If a match is found, Inserted is set to False and Position
designates the element with the matching key. Otherwise, Insert allocates a new
node, initializes it to Key and New_Item, and adds it to Container; Inserted is
set to True and Position designates the newly-inserted node. Any exception
raised during allocation is propagated and Container is not modified.>

@xcode<@key{procedure} Insert (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean);>

@xindent<Insert inserts Key into Container as per the five-parameter Insert, with the
difference that an element initialized with any implicit initial values for any
part (as for an object_declaration with no initialization expression - see
3.3.1) is inserted.>

@xcode<@key{procedure} Insert (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type;
                  New_Item  : @key{in}     Element_Type);>

@xindent<Insert inserts Key and New_Item into Container as per the five-parameter
Insert, with the difference that if a node with a key equivalent to Key is
already in the map, then Constraint_Error is propagated.>

AARM Note: This is equivalent to:
    declare
      Inserted : Boolean; C : Cursor;
    begin
      Insert (Container, Key, New_Item, C, Inserted);
      if not Inserted then
         raise Constraint_Error;
      end if;
    end;
but doesn't require the hassle of out parameters.

@xcode<@key{procedure} Include (Container : @key{in out} Map;
                   Key       : @key{in}     Key_Type;
                   New_Item  : @key{in}     Element_Type);>

@xindent<Include inserts Key and New_Item into Container as per the five-parameter
Insert, with the difference that if a node with a key equivalent to Key is
already in the map, then this operation assigns Key and New_Item to the
matching node. Any exception raised during assignment is propagated.>

AARM Note: This is equivalent to:
    declare
      C : Cursor := Find (Container, Key);
    begin
      if C = No_Element then
         Insert (Container, Key, New_Item);
      else
         Replace (Container, Key, New_Item);
      end if;
    end;
but this avoids doing the search twice.

@xcode<@key{procedure} Replace (Container : @key{in out} Map;
                   Key       : @key{in}     Key_Type;
                   New_Item  : @key{in}     Element_Type);>

@xindent<Replace checks if a node with a key equivalent to Key is present in Container.
If a match is found, Replace assigns Key and New_Item to the matching node;
otherwise, Constraint_Error is propagated.>

AARM Note: We update the key as well as the element, as the key might include
additional information that does not participate in equivalence. If only the
element needs to be updated, use
Replace_Element (Find (Container, Key), New_Element).

@xcode<@key{procedure} Delete (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type);>

@xindent<Delete checks if a node with a key equivalent to Key is present in Container.
If a match is found, Delete removes the node from the map; otherwise,
Constraint_Error is propagated.>

@xcode<@key{procedure} Delete (Container : @key{in out} Map;
                  Position  : @key{in out} Cursor);>

@xindent<If Position equals No_Element, then Constraint_Error is propagated. If Position
does not designate an element in Container, then Program_Error is propagated.
Otherwise, Delete removes the node designated by Position from the map.
Position is set to No_Element on return.>

AARM Note: The check on Position checks that the cursor does not belong to some
other map. This check implies that a reference to the map is included in the
cursor value. This wording is not meant to require detection of dangling
cursors; such cursors are defined to be invalid, which means that execution is
erroneous, and any result is allowed (including not raising an exception).

@xcode<@key{procedure} Exclude (Container : @key{in out} Map;
                   Key       : @key{in}     Key_Type);>

@xindent<Exclude checks if a node with a key equivalent to Key is present in Container.
If a match is found, Exclude removes the node from the map and then
deallocates the node.>

@xcode<@key{function} Contains (Container : Map;
                   Key       : Key_Type) @key{return} Boolean;>

@xindent<Equivalent to Find (Container, Key) /= No_Element.>

@xcode<@key{function} Find (Container : Map;
               Key       : Key_Type) @key{return} Cursor;>

@xindent<If Length (Container) equals 0, then Find returns No_Element. Otherwise, Find
checks if a node with a key equivalent to Key is present in Container. If a
match is found, a cursor designating the matching node is returned; otherwise,
No_Element is returned.>

@xcode<@key{function} Element (Container : Map;
                  Key       : Key_Type) @key{return} Element_Type;>

@xindent<Equivalent to Element (Find (Container, Key)).>

@xcode<@key{function} First (Container : Map) @key{return} Cursor;>

@xindent<If Length (Container) = 0, then First returns No_Element. Otherwise,
First returns a cursor that designates the first node in Container.>

@xcode<@key{function} Next (Position  : Cursor) @key{return} Cursor;>

@xindent<Returns a cursor that designates the successor of the node designated by
Position. If Position designates the last node, then No_Element is returned. If
Position equals No_Element, then No_Element is returned.>

@xcode<@key{procedure} Next (Position  : @key{in out} Cursor);>

@xindent<Equivalent to Position := Next (Position).>

@xcode<@key{function} Has_Element (Position : Cursor) @key{return} Boolean;>

@xindent<Returns True if Position designates a node, and returns False otherwise.>

AARM Note: To Be Honest: This function may not detect cursors that designate
deleted elements; such cursors are invalid (see below).

@xcode<@key{procedure} Iterate
  (Container : @key{in} Map;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor));>

@xindent<Iterate calls Process.@key{all} with a cursor that designates each node
in Container, starting with the first node and moving the cursor according to
the successor relation. Program_Error is propagated if Process.@key{all} tampers
with the elements of Container. Any exception raised by Process.@key{all} is
propagated.>

AARM Note:
This check takes place when the operations that insert or delete elements, etc.
are called.

See Iterate for vectors for a suggested implementation of the check.
End AARM Notes.

@i<@s8<Erroneous Execution>>

A Cursor value is @i<invalid> if any of the following have occurred since it was
created:
@xbullet<The map that contains the node it designates has been finalized;>
@xbullet<The map that contains the node it designates has been used as the
Source or Target of a call to Move;>
@xbullet<The node it designates has been deleted from the map.>

The result of "=" or Has_Element is unspecified if these functions are called
with an invalid cursor parameter. Execution is erroneous if any other subprogram
declared in Containers.Hashed_Maps or Containers.Ordered_Maps is called with an
invalid cursor parameter.

AARM Notes: The list above is intended to be exhaustive. In other cases, a
cursor value continues to designate its original element. For instance, cursor
values survive the insertion and deletion of other nodes.

While it is possible to check for these cases, in many cases the overhead
necessary to make the check is substantial in time or space. Implementations
are encouraged to check for as many of these cases as possible and raise
Program_Error if detected.
End AARM Notes.

@i<@s8<Implementation Requirements>>

No storage associated with a Map object shall be lost upon assignment or
scope exit.

@i<@s8<Implementation Advice>>

Move should not copy elements, and should minimize copying of internal
data structures.

AARM Note: Usually that can be accomplishing simply by moving the pointer(s) to
the internal data structures from the Source container to the Target container.

!corrigendum A.18.5

@dinsc

@i<@s8<Static Semantics>>

The generic library package Containers.Hashed_Maps has the following
declaration:

@xcode<@key{generic}
   @key{type} Key_Type @key{is private};
   @key{type} Element_Type @key{is private};
   @key{with function} Hash (Key : Key_Type) @key{return} Hash_Type;
   @key{with function} Equivalent_Keys (Left, Right : Key_Type)
      @key{return} Boolean;
   @key{with function} "=" (Left, Right : Element_Type)
      @key{return} Boolean is <>;
@key{package} Ada.Containers.Hashed_Maps @key{is}
   @key{pragma} Preelaborate (Hashed_Maps);

   @key{type} Map @key{is tagged private};

   @key{type} Cursor @key{is private};

   Empty_Map : @key{constant} Map;

   No_Element : @key{constant} Cursor;

   @key{function} "=" (Left, Right : Map) @key{return} Boolean;

   @key{function} Length (Container : Map) @key{return} Count_Type;

   @key{function} Is_Empty (Container : Map) @key{return} Boolean;

   @key{procedure} Clear (Container : @key{in out} Map);

   @key{function} Key (Position : Cursor) @key{return} Key_Type;

   @key{function} Element (Position : Cursor) @key{return} Element_Type;

   @key{procedure} Query_Element
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Key     : @key{in} Key_Type;
                                            Element : @key{in} Element_Type));

   @key{procedure} Update_Element
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Key     : @key{in} Key_Type;
                                            Element : @key{in out} Element_Type));

   @key{procedure} Replace_Element (Position : @key{in} Cursor;
                              By       : @key{in} Element_Type);

   @key{procedure} Move (Target : @key{in out} Map;
                   Source : @key{in out} Map);

   @key{procedure} Insert (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean);

   @key{procedure} Insert (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean);

   @key{procedure} Insert (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     New_Item  : @key{in}     Element_Type);

   @key{procedure} Include (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type;
                      New_Item  : @key{in}     Element_Type);

   @key{procedure} Replace (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type;
                      New_Item  : @key{in}     Element_Type);

   @key{procedure} Delete (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type);

   @key{procedure} Delete (Container : @key{in out} Map;
                     Position  : @key{in out} Cursor);

   @key{procedure} Exclude (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type);

   @key{function} Contains (Container : Map;
                      Key       : Key_Type) @key{return} Boolean;

   @key{function} Find (Container : Map;
                  Key       : Key_Type)
      @key{return} Cursor;

   @key{function} Element (Container : Map;
                     Key       : Key_Type)
      @key{return} Element_Type;

   @key{function} First (Container : Map)
      @key{return} Cursor;

   @key{function} Next (Position  : Cursor) @key{return} Cursor;

   @key{procedure} Next (Position  : @key{in out} Cursor);

   @key{function} Has_Element (Position : Cursor) @key{return} Boolean;

   @key{function} Equivalent_Keys (Left, Right : Cursor)
      @key{return} Boolean;

   @key{function} Equivalent_Keys (Left  : Cursor;
                             Right : Key_Type)
      @key{return} Boolean;

   @key{function} Equivalent_Keys (Left  : Key_Type;
                             Right : Cursor)
      @key{return} Boolean;

   @key{procedure} Iterate
     (Container : @key{in} Map;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));

   @key{function} Capacity (Container : Map) @key{return} Count_Type;

   @key{procedure} Reserve_Capacity (Container : @key{in out} Map;
                               Capacity  : @key{in}     Count_Type);

@key{private}

   ... -- @RI[not specified by the language]

@key{end} Ada.Containers.Hashed_Maps;>

An object of type Map contains an expandable hash table, which is used to
provide direct access to nodes. The @i<capacity> of an object of type Map is
the maximum number of nodes that can be inserted into the hash table prior to it
being automatically expanded.

AARM Notes
The expected implementation for a Map uses a hash table which is grown when it
is too small, with linked lists hanging off of each bucket. Note that in that
implementation a cursor needs a back pointer to the Map object to implement
iteration; that could either be in the nodes, or in the cursor object. To
provide an average O(1) access time, capacity would typically equal the number
of buckets in such an implementation, so that the average bucket linked list
length would be no more than 1.0.

There is no defined relationship between elements in a hashed map. Typically,
iteration will return elements in the order that they are hashed in.
End AARM Notes

Two keys @i<K1> and @i<K2> are defined to be @i<equivalent> if
Equivalent_Keys (@i<K1>, @i<K2>) returns True.

Function Hash is expected to return the same value each time it is called with a
particular key value. For any two equivalent key values, Hash is expected to
return the same value. If Hash behaves in some other manner, the behavior of
this package is unspecified. Which subprograms of this package call Hash, and
how many times they call it, is unspecified.

AARM Notes
The implementation is not required to protect against Hash raising an exception,
or returning random numbers, or any other "bad" behavior. It's not practical to
do so, and a broken Hash function makes the container unusable.

The implementation can call Hash whenever it is needed; we don't want to
specify how often that happens. The result must remain the same (this is
logically a pure function), or the behavior is unspecified.
End AARM Notes

Function Equivalent_Keys is expected to return the same value each time it is
called with a particular pair of key values. For any two keys @i<K1> and @i<K2>, the
boolean values Equivalent_Keys (@i<K1>, @i<K2>) and Equivalent_Key (@i<K2>, @i<K1>)
are expected to be equal. If Equivalent_Keys behaves in some other manner, the
behavior of this package is unspecified. Which subprograms of this package call
Equivalent_Keys, and how many times they call it, is unspecified.

AARM Note
As with Hash, the implementation is not required to protect against
Equivalent_Keys raising an exception or returning random results. Similarly, the
implementation can call this operation whenever it is needed. The result must
remain the same (this is a logically pure function), or the behavior is
unspecified.

If the value of a key stored in a node of a map is changed other than by an
operation in this package such that at least one of Hash or Equivalent_Keys
give different results, the behavior of this package is unspecified.

AARM Notes
The implementation is not required to protect against changes to key values
other than via the operations declared in the Hashed_Maps package.

To see how this could happen, imagine an instantiation of Hashed_Maps where
the key type is an access-to-variable type and Hash returns a value derived
from the components of the designated object. Then, any operation that has a
key value could modify those components and change the hash value:
    Key (Map).Some_Component := New_Value;

This is really a design error on the part of the user of the map; it shouldn't
be possible to modify keys stored in a map. But we can't prevent this error
anymore than we can prevent someone passing as Hash a random number generator.
End AARM Notes

Which nodes are the first node and the last node of a map, and which node is the
successor of a given node, are unspecified, other than the general semantics
described in A.18.4.

AARM Note
Typically the first node will be the first node in the first bucket, the last
node will be the last node in the last bucket, and the successor will be
obtained by following the collision list, and going to the next bucket at the
end of each bucket.

@xcode<@key{procedure} Clear (Container : @key{in out} Map);>

@xindent<In addition to the semantics described in A.18.4, Clear does not
affect the capacity of Container.>

AARM Note:
procedure Move (Target : in out Map;
                Source : in out Map);

The intended implementation is that the internal hash table of Target is first
deallocated; then the internal hash table is removed from Source and moved to
Target.
End AARM Note.

@xcode<@key{procedure} Insert (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean);>

@xindent<In addition to the semantics described in A.18.4, if Length
(Container) equals Capacity (Container), then Insert first calls
Reserve_Capacity to increase the capacity of Container to some larger value.>

AARM Notes:
Insert should only compare keys that hash to the same bucket in the hash table.

We specify when Reserve_Capacity is called to bound the overhead of capacity
expansion operations (which are potentially expensive). Moreover, expansion can
be predicted by comparing Capacity(Map) to Length(Map). Since we don't specify
by how much the hash table is expanded, this only can be used to predict the
next expansion, not later ones.
End AARM Notes.

AARM Notes:
procedure Delete (Container : in out Map;
                  Key       : in     Key_Type);

Delete should only compare keys that hash to the same bucket in the hash
table. The node containing the element may be deallocated now,
or it may be saved and reused later.
End AARM Notes.

AARM Notes:
procedure Exclude (Container : in out Map;
                   Key       : in     Key_Type);

Exclude should only compare keys that hash to the same bucket in the hash
table. Exclude should work on an empty map; nothing happens in that case.
End AARM Notes.

AARM Note:
function Find (Container : Map;
               Key       : Key_Type) return Cursor;

Find should only compare keys that hash to the same bucket in the hash table.
End AARM Notes.

AARM Note:
function First (Container : Map) return Cursor;

In a typical implementation, this will be the first node in the lowest numbered
hash bucket that contains a node.
End AARM Notes.

AARM Note:
function Next (Position  : Cursor) return Cursor;

In a typical implementation, this will return the next node in a bucket; if
Position is the last node in a bucket, this will return the first node in the
next non-empty bucket.

A typical implementation will need to a keep a pointer at the map container
in the cursor in order to implement this function.
End AARM Note.

@xcode<@key{function} Equivalent_Keys (Left, Right : Cursor)
      @key{return} Boolean;>

@xindent<Equivalent to Equivalent_Keys (Key (Left), Key (Right)).>

@xcode<@key{function} Equivalent_Keys (Left  : Cursor;
                          Right : Key_Type) @key{return} Boolean;>

@xindent<Equivalent to Equivalent_Keys (Key (Left), Right).>

@xcode<@key{function} Equivalent_Keys (Left  : Key_Type;
                          Right : Cursor) @key{return} Boolean;>

@xindent<Equivalent to Equivalent_Keys (Left, Key (Right)).>

@xcode<@key{function} Capacity (Container : Map) @key{return} Count_Type;>

@xindent<Returns the capacity of Container.>

@xcode<@key{procedure} Reserve_Capacity (Container : @key{in out} Map;
                            Capacity  : @key{in}     Count_Type);>

@xindent<Reserve_Capacity allocates a new hash table such that the length of the
resulting map can become at least the value Capacity without requiring an
additional call to Reserve_Capacity, and is large enough to hold the current
length of Container. Reserve_Capacity then rehashes the nodes in Container onto
the new hash table. It replaces the old hash table with the new hash table, and
then deallocates the old hash table. Any exception raised during allocation is
propagated and Container is not modified.>

@xindent<Reserve_Capacity tampers with the cursors of Container.>

AARM Notes: This routine is used to preallocate the internal hash table to the
specified capacity such that future Inserts do not require expansion of the
hash table. Therefore, the implementation should allocate the needed memory to
make that true at this point, even though the visible semantics could be
preserved by waiting until enough elements are inserted.

While Reserve_Capacity can be used to reduce the capacity of a map, we do not
specify whether an implementation actually supports reduction of the capacity.
Since the actual capacity can be anything greater than or equal to Count, an
implementation never has to reduce the capacity.

Reserve_Capacity tampers with the cursors, as rehashing probably will change
the order that elements are stored in the map.
End AARM Notes

@i<@s8<Implementation Advice>>

If @i<N> is the length of a map, the average time complexity of the subprograms
Element, Insert, Include, Replace, Delete, Exclude and Find that take a key
parameter should be O(log @i<N>). The average time complexity of the
subprograms that take a cursor parameter should be O(1). The average time
complexity of Reserve_Capacity should be O(@i<N>).

AARM Note
We do not mean to overly constrain implementation strategies here. However, it
is important for portability that the performance of large containers has
roughly the same factors on different implementations. If a program is moved to
an implementation for which Find is O(N), that program could be unusable when
the maps are large. We allow O(log N) access because the proportionality
constant and caching effects are likely to be larger than the log factor, and
we don't want to discourage innovative implementations.

!corrigendum A.18.6

@dinsc

@i<@s8<Static Semantics>>

The generic library package Containers.Ordered_Maps has the following
declaration:

@xcode<@key{generic}
   @key{type} Key_Type @key{is private};
   @key{type} Element_Type @key{is private};
   @key{with function} "<" (Left, Right : Key_Type) @key{return} Boolean @key{is} <>;
   @key{with function} "=" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Ordered_Maps @key{is}
   @key{pragma} Preelaborate (Ordered_Maps);

   @key{type} Map @key{is tagged private};

   @key{type} Cursor @key{is private};

   Empty_Map : @key{constant} Map;

   No_Element : @key{constant} Cursor;

   @key{function} "=" (Left, Right : Map) @key{return} Boolean;

   @key{function} Length (Container : Map) @key{return} Count_Type;

   @key{function} Is_Empty (Container : Map) @key{return} Boolean;

   @key{procedure} Clear (Container : @key{in out} Map);

   @key{function} Key (Position : Cursor) @key{return} Key_Type;

   @key{function} Element (Position : Cursor) @key{return} Element_Type;

   @key{procedure} Query_Element
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Key     : @key{in} Key_Type;
                                            Element : @key{in} Element_Type));

   @key{procedure} Update_Element
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Key     : @key{in}     Key_Type;
                                            Element : @key{in out} Element_Type));

   @key{procedure} Replace_Element (Position : @key{in} Cursor;
                              By       : @key{in} Element_Type);

   @key{procedure} Move (Target : @key{in out} Map;
                   Source : @key{in out} Map);

   @key{procedure} Insert (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean);

   @key{procedure} Insert (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean);

   @key{procedure} Insert (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     New_Item  : @key{in}     Element_Type);

   @key{procedure} Include (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type;
                      New_Item  : @key{in}     Element_Type);

   @key{procedure} Replace (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type;
                      New_Item  : @key{in}     Element_Type);

   @key{procedure} Delete (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type);

   @key{procedure} Delete (Container : @key{in out} Map;
                     Position  : @key{in out} Cursor);

   @key{procedure} Delete_First (Container : @key{in out} Map);

   @key{procedure} Delete_Last (Container : @key{in out} Map);

   @key{procedure} Exclude (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type);

   @key{function} Contains (Container : Map;
                      Key       : Key_Type) @key{return} Boolean;

   @key{function} Find (Container : Map;
                  Key       : Key_Type) @key{return} Cursor;

   @key{function} Element (Container : Map;
                     Key       : Key_Type) @key{return} Element_Type;

   @key{function} Floor (Container : Map;
                   Key       : Key_Type) @key{return} Cursor;

   @key{function} Ceiling (Container : Map;
                     Key       : Key_Type) @key{return} Cursor;

   @key{function} First (Container : Map) @key{return} Cursor;

   @key{function} First_Key (Container : Map) @key{return} Key_Type;

   @key{function} First_Element (Container : Map) @key{return} Element_Type;

   @key{function} Last (Container : Map) @key{return} Cursor;

   @key{function} Last_Key (Container : Map) @key{return} Key_Type;

   @key{function} Last_Element (Container : Map) @key{return} Element_Type;

   @key{function} Next (Position : Cursor) @key{return} Cursor;

   @key{procedure} Next (Position : @key{in out} Cursor);

   @key{function} Previous (Position : Cursor) @key{return} Cursor;

   @key{procedure} Previous (Position : @key{in out} Cursor);

   @key{function} Has_Element (Position : Cursor) @key{return} Boolean;

   @key{function} "<" (Left, Right : Cursor) @key{return} Boolean;

   @key{function} ">" (Left, Right : Cursor) @key{return} Boolean;

   @key{function} "<" (Left : Cursor; Right : Key_Type) @key{return} Boolean;

   @key{function} ">" (Left : Cursor; Right : Key_Type) @key{return} Boolean;

   @key{function} "<" (Left : Key_Type; Right : Cursor) @key{return} Boolean;

   @key{function} ">" (Left : Key_Type; Right : Cursor) @key{return} Boolean;

   @key{procedure} Iterate
     (Container : @key{in} Map;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));

   @key{procedure} Reverse_Iterate
     (Container : @key{in} Map;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));

@key{private}

   ... -- @RI[not specified by the language]

@key{end} Ada.Containers.Ordered_Maps;>

Two keys @i<K1> and @i<K2> are @i<equivalent> if both @i<K1> < @i<K2> and
@i<K2> < @i<K1> return False, using the generic formal "<" operator for
keys.

Functions "<" and "=" on Key_Type values are expected to return the same result
value each time they are called with a particular pair of key values. If @i<A> = @i<B>
returns True, then @i<B> = @i<A> is expected to also return True. If @i<A> < @i<B>
returns True, then @i<B> < @i<A> is expected to return False. For any two
equivalent elements, "=" is expected to return True. If "<" or "=" behaves in
some other manner, the behavior of this package is unspecified. Which
subprograms of this package call "<" and "=", and how many times these
functions are called, is unspecified.

AARM Notes
The implementation is not required to protect against "<" or "=" raising an
exception, or returning random results, or any other "bad" behavior. It's not
practical to do so, and a broken "<" or "=" function makes the container
unusable.

The implementation can call "<" and "=" whenever it is needed; we don't want to
specify how often that happens. The result must remain the same (these are
logically pure functions), or the behavior is unspecified.
End AARM Notes

If the value of a key stored in a map is changed other than by an operation in
this package such that at least one of "<" or "=" give different results, the
behavior of this package is unspecified.

AARM Notes
The implementation is not required to protect against changes to key values
other than via the operations declared in the Ordered_Maps package.

To see how this could happen, imagine an instantiation of Ordered_Maps package
where the key type is an access-to-variable type and "<" returns a value
derived from comparing the components of the designated objects. Then, any
operation that has a key value (even if the key value is constant) could modify
those components and change the result of "<":
    Key (Map).Some_Component := New_Value;

This is really a design error on the part of the user of the map; it shouldn't
be possible to modify keys stored in a map such that "<" changes. But we
can't prevent this error anymore than we can prevent someone passing as "<"
a routine that produces random answers.
End AARM Notes

The first node of a nonempty map is the one whose key is less than the key of
all the other nodes in the map. The last node of a nonempty map is the one
whose key is greater than the key of all the other elements in the map. The
successor of a node is the node with the smallest key that is larger than the
key of the given node. The predecessor of a node is the node with the largest
key that is smaller than the key of the given node. All comparisons are done
using the generic formal "<" operator for keys.

@xcode<@key{procedure} Delete_First (Container : @key{in out} Map);>

@xindent<If Container is empty, Delete_First has no effect. Otherwise the node
designated by First (Container) is removed from Container. Delete_First
tampers with the cursors of Container.>

@xcode<@key{procedure} Delete_Last (Container : @key{in out} Map);>

@xindent<If Container is empty, Delete_Last has no effect. Otherwise the node designated
by Last (Container) is removed from Container. Delete_Last tampers with
the cursors of Container.>

@xcode<@key{function} Floor (Container : Map;
                Key       : Key_Type) @key{return} Cursor;>

@xindent<Floor searches for the last node whose key is not greater than Key. If such a
node is found, a cursor that designates it is returned. Otherwise No_Element is
returned.>

@xcode<@key{function} Ceiling (Container : Map;
                  Key       : Key_Type) @key{return} Cursor;>

@xindent<Ceiling searches for the first node whose key is not less than Key,
using the generic formal "<" operator for keys. If such a node is found,
a cursor that designates it is returned. Otherwise No_Element is returned.>

@xcode<@key{function} First_Key (Container : Map) @key{return} Key_Type;>

@xindent<Equivalent to Key (First (Container)).>

@xcode<@key{function} First_Element (Container : Map) @key{return} Element_Type;>

@xindent<Equivalent to Element (First (Container)).>

@xcode<@key{function} Last (Container : Map) @key{return} Cursor;>

@xindent<Returns a cursor that designates the last node in Container. If Container is
empty, returns No_Element.>

@xcode<@key{function} Last_Key (Container : Map) @key{return} Key_Type;>

@xindent<Equivalent to Key (Last (Container)).>

@xcode<@key{function} Last_Element (Container : Map) @key{return} Element_Type;>

@xindent<Equivalent to Element (Last (Container)).>

@xcode<@key{function} Previous (Position : Cursor) @key{return} Cursor;>

@xindent<If Position equals No_Element, then Previous returns No_Element. Otherwise
Previous returns the a cursor designating the node that precedes the one
designated by Position. If Position designates the first element, then Previous
returns No_Element.>

@xcode<@key{procedure} Previous (Position : @key{in out} Cursor);>

@xindent<Equivalent to Position := Previous (Position).>

@xcode<@key{function} "<" (Left, Right : Cursor) @key{return} Boolean;>

@xindent<Equivalent to Key (Left) < Key (Right).>

@xcode<@key{function} ">" (Left, Right : Cursor) @key{return} Boolean;>

@xindent<Equivalent to Key (Right) < Key (Left).>

@xcode<@key{function} "<" (Left : Cursor; Right : Key_Type) @key{return} Boolean;>

@xindent<Equivalent to Key (Left) < Right.>

@xcode<@key{function} ">" (Left : Cursor; Right : Key_Type) @key{return} Boolean;>

@xindent<Equivalent to Right < Key (Left).>

@xcode<@key{function} "<" (Left : Key_Type; Right : Cursor) @key{return} Boolean;>

@xindent<Equivalent to Left < Key (Right).>

@xcode<@key{function} ">" (Left : Key_Type; Right : Cursor) @key{return} Boolean;>

@xindent<Equivalent to Key (Right) < Left.>

@xcode<@key{procedure} Reverse_Iterate
  (Container : @key{in} Map;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor));>

@xindent<Iterates over the nodes in Container as per Iterate, with the
difference that the nodes are traversed in predecessor order, starting with the
last node.>

@i<@s8<Implementation Advice>>

If @i<N> is the length of a map, then the worst-case time complexity of the
Element, Insert, Include, Replace, Delete, Exclude and Find operations that
take a key parameter should be O((log @i<N>)**2) or better. The worst-case
time complexity of the subprograms that take a cursor parameter should be O(1).

AARM Note
A balanced (red-black) tree for keys has O(log N) worst-case performance. Note
that a O(N) worst-case implementation (like a list) would be wrong. We do not
mean to overly constrain implementation strategies here. However, it is
important for portability that the performance of large containers has roughly
the same factors on different implementations. If a program is moved to an
implementation that takes O(N) to find elements, that program could be unusable
when the maps are large. We allow the extra log N factors because the
proportionality constant and caching effects are likely to be larger than the
log factor, and we don't want to discourage innovative implementations.

!corrigendum A.18.7

@dinsc

The language-defined generic packages Containers.Hashed_Sets and
Containers.Ordered_Sets provide private types Set and Cursor, and a set of
operations for each type. A set container allows elements of an arbitrary type
to be stored without duplication. A hashed set uses a hash function to organize
elements, while an ordered set orders its element per a specified relation.

This section describes the declarations that are common to both kinds of sets.
See A.18.8 for a description of the semantics specific to
Containers.Hashed_Sets and A.18.9 for a description of the semantics specific
to Containers.Ordered_Sets.

@i<@s8<Static Semantics>>

The type Set is used to represent sets. The type Set needs finalization (see
7.6).

A set contains elements. Set cursors designate elements. There exists an
equivalence relation on elements, whose definition is different for hashed sets
and ordered sets. A set never contains two or more equivalent elements. The
@i<length> of a set is the number of elements it contains.

Each nonempty set has two particular elements called the @i<first element> and
the @i<last element> (which may be the same). Each element except for the last
element has a @i<successor element>. If there are no other intervening
operations, starting with the first element and repeatedly going to the
successor element will visit each element in the map exactly once until the
last element is reached. The exact definition of these terms is different for
hashed sets and ordered sets.

Some operations are assumed to work on a constant set of elements. For such
an operation, a subprogram is said to @i<tamper with cursors> of a set object
@i<S> if:

@xbullet<it inserts or deletes elements of @i<S>, that is, it calls the Insert,
Include, Clear, Delete, Exclude, or Replace_Element procedures with S as
a parameter; or>

   AARM To Be Honest: Operations which are defined to be equivalent to
   a call on one of these operations also are included. Similarly, operations
   which call one of these as part of their definition are included.

   AARM Disucssion: We have to include Replace_Element here because it might
   delete and reinsert the element if it moves in the set.

@xbullet<it finalizes @i<S>; or>

@xbullet<it calls the Move procedure with @i<S> as a parameter; or>

@xbullet<it calls one of the operations defined to tamper with cursors of @i<S>.>

Some operations are assumed to not change elements. For such an operation, a
subprogram is said to @i<tamper with elements> of a set object @i<S> if:

@xbullet<it tampers with cursors of @i<S>; or>

@xbullet<it modifies one or more elements of @i<S>, that is, it calls the
Replace or Update_Element_Preserving_Key procedures with @i<S> as a
parameter.>

Empty_Set represents the empty Set object. It has a length of 0. If an object
of type Set is not otherwise initialized, it is initialized to the same
value as Empty_Set.

No_Element represents a cursor that designates no element. If an object of type
Cursor is not otherwise initialized, it is initialized to the same
value as No_Element.

@xcode<@key{function} "=" (Left, Right : Set) @key{return} Boolean;>

@xindent<If Left and Right denote the same set object, then the function returns True.
If Left and Right have different lengths, then the function returns False.
Otherwise, for each element @i<E> in Left, the function returns False if an element
equal to @i<E> (in the sense of the generic formal equality operator) is not
present in Right. If the function has not returned a result after checking all
of the elements, it returns True. Any exception raised during evaluation of
element equality is propagated.>

@xcode<@key{function} Equivalent_Sets (Left, Right : Set) @key{return} Boolean;>

@xindent<If Left and Right denote the same set object, then the function
returns True. If Left and Right have different lengths, then the function
returns False. Otherwise, for each element @i<E> in Left, the function returns
False if an element equivalent to @i<E> is not present in Right. If the
function has not returned a result after checking all of the elements, it
returns True. Any exception raised during evaluation of element equivalence is
propagated.>

@xcode<@key{function} Length (Container : Set) @key{return} Count_Type;>

@xindent<Returns the number of elements in Container.>

@xcode<@key{function} Is_Empty (Container : Set) @key{return} Boolean;>

@xindent<Equivalent to Length (Container) = 0.>

@xcode<@key{procedure} Clear (Container : @key{in out} Set);>

@xindent<Removes all the elements from Container.>

@xcode<@key{function} Element (Position : Cursor) @key{return} Element_Type;>

@xindent<If Position equals No_Element, then Constraint_Error is propagated.
Otherwise, Element returns the element designated by Position.>

@xcode<@key{procedure} Query_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access procedure} (Element : @key{in} Element_Type));>

@xindent<If Position equals No_Element, then Constraint_Error is propagated.
Otherwise, Query_Element calls Process.@key{all} with the element designated by
Position as the argument. Program_Error is propagated if Process.@key{all}
tampers with the elements of Container. Any exceptions raised by
Process.@key{all} are propagated.>

@xcode<@key{procedure} Replace_Element (Container : @key{in} Set;
                           Position  : @key{in} Cursor;
                           By        : @key{in} Element_Type);>

@xindent<If Position equals No_Element, then Constraint_Error is propagated.
If Position does not designate an element in Container, then Program_Error
is propagated. Otherwise, the element designated by Position is tested for
equivalence to By; if they are found to be equivalent, Replace_Element assigns
By to the element designated by Position. Otherwise, the element designated by
Position is removed from the container, then By is inserted into the container.
If the insertion fails, Program_Error is propagated.>

@xcode<@key{procedure} Move (Target : @key{in out} Set;
                Source : @key{in out} Set);>

@xindent<If Target denotes the same object as Source, then Move has no effect.
Otherwise, Move first clears Target. Then, each element from Source is removed
from Source and inserted into Target. The length of Source is 0 after a
successful call to Move.>

@xcode<@key{procedure} Insert (Container : @key{in out} Set;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean);>

@xindent<Insert checks if an element equivalent to New_Item is already present
in Container. If a match is found, Inserted is set to False and Position
designates the matching element. Otherwise, Insert adds New_Item to Container;
Inserted is set to True and Position designates the newly-inserted element. Any
exception raised during allocation is propagated and Container is not
modified.>

@xcode<@key{procedure} Insert (Container : @key{in out} Set;
                  New_Item  : @key{in}     Element_Type);>

@xindent<Insert inserts New_Item into Container as per the four-parameter
Insert, with the difference that if an element equivalent to New_Item is
already in the set, then Constraint_Error is propagated.>

AARM Note: This is equivalent to:
    declare
      Inserted : Boolean; C : Cursor;
    begin
      Insert (Container, New_Item, C, Inserted);
      if not Inserted then
         raise Constraint_Error;
      end if;
    end;
but doesn't require the hassle of out parameters.

@xcode<@key{procedure} Include (Container : @key{in out} Set;
                   New_Item  : @key{in}     Element_Type);>

@xindent<Include inserts New_Item into Container as per the four-parameter
Insert, with the difference that if an element equivalent to New_Item is
already in the set, then it is replaced. Any exception raised during assignment
is propagated.>

@xcode<@key{procedure} Replace (Container : @key{in out} Set;
                   New_Item  : @key{in}     Element_Type);>

@xindent<Replace checks if an element equivalent to New_Item is already in the
set. If a match is found, that element is replaced with New_Item; otherwise,
Constraint_Error is propagated.>

@xcode<@key{procedure} Delete (Container : @key{in out} Set;
                  Item      : @key{in}     Element_Type);>

@xindent<Delete checks if an element equivalent to Item is present in
Container. If a match is found, Delete removes the element from the set;
otherwise, Constraint_Error is propagated.>

@xcode<@key{procedure} Delete (Container : @key{in out} Set;
                  Position  : @key{in out} Cursor);>

@xindent<If Position equals No_Element, Delete has no effect. If Position does
not designate an element in Container, then Program_Error is propagated.
Otherwise, Delete removes the node designated by Position from the set.
Position is set to No_Element on return.>

AARM Note: The check on Position checks that the cursor does not belong to some
other set. This check implies that a reference to the set is included in the
cursor value. This wording is not meant to require detection of dangling
cursors; such cursors are defined to be invalid, which means that execution is
erroneous, and any result is allowed (including not raising an exception).

@xcode<@key{procedure} Exclude (Container : @key{in out} Set;
                   Item      : @key{in}     Element_Type);>

@xindent<Exclude checks if an element equivalent to Item is present in
Container. If a match is found, Exclude removes the element from the set.>

@xcode<@key{function} Contains (Container : Set;
                   Item      : Element_Type) @key{return} Boolean;>

@xindent<Equivalent to Find (Container, Item) /= No_Element.>

@xcode<@key{function} Find (Container : Set;
               Item      : Element_Type) @key{return} Cursor;>

@xindent<If Length (Container) equals 0, then Find returns No_Element.
Otherwise, Find checks if an element equivalent to Item is present in
Container. If a match is found, a cursor designating the matching element is
returned; otherwise, No_Element is returned.>

@xcode<@key{function} First (Container : Set) @key{return} Cursor;>

@xindent<If Length (Container) = 0, then First returns No_Element. Otherwise, First
returns a cursor that designates the first node in Container.>

@xcode<@key{function} Next (Position  : Cursor) @key{return} Cursor;>

@xindent<Returns a cursor that designates the successor of the element
designated by Position. If Position designates the last element, then
No_Element is returned. If Position equals No_Element, then No_Element is
returned.>

@xcode<@key{procedure} Next (Position  : @key{in out} Cursor);>

@xindent<Equivalent to Position := Next (Position).>

@xcode<@key{function} Has_Element (Position : Cursor) @key{return} Boolean;>

@xindent<Returns True if Position designates an element, and returns False otherwise.>

AARM Note: To Be Honest: This function may not detect cursors that designate
deleted elements; such cursors are invalid (see below).

@xcode<@key{procedure} Iterate
  (Container : @key{in} Set;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor));>

@xindent<Iterate calls Process.@key{all} with a cursor that designates each
element in Container, starting with the first node and moving the cursor
according to the successor relation. Program_Error is propagated if
Process.@key{all} tampers with the elements of Container. Any exception raised by
Process.@key{all} is propagated.>

AARM Note:
This check takes place when the operations that insert or delete elements, etc.
are called.

See Iterate for vectors for a suggested implementation of the check.
End AARM Notes.

@xcode<@key{procedure} Union (Target : @key{in out} Set;
                 Source : @key{in}     Set);>

@xindent<Union inserts into Target the elements of Source that are not equivalent to
some element already in Target.>

AARM Note: If the objects are the same, the result is the same as the
original object. The implementation needs to take care so that aliasing effects
do not make the result trash; Union (S, S); must work.

@xcode<@key{function} Union (Left, Right : Set) @key{return} Set;>

@xindent<Returns a set comprising all of the elements of Left, and the elements of Right
that are not equivalent to some element of Left.>

@xcode<@key{procedure} Intersection (Target : @key{in out} Set;
                        Source : @key{in}     Set);>

@xindent<Union deletes from Target the elements of Target that are not equivalent to
some element of Source.>

AARM Note: If the objects are the same, the result is the same as the
original object. The implementation needs to take care so that aliasing effects
do not make the result trash; Intersection (S, S); must work.

@xcode<@key{function} Intersection (Left, Right : Set) @key{return} Set;>

@xindent<Returns a set comprising all the elements of Left that are equivalent to
the some element of Right.>

@xcode<@key{procedure} Difference (Target : @key{in out} Set;
                      Source : @key{in}     Set);>

@xindent<If Target denotes the same object as Source, then Difference clears
Target. Otherwise, it deletes from Target the elements that are equivalent to
some element of Source.>

@xcode<@key{function} Difference (Left, Right : Set) @key{return} Set;>

@xindent<Returns a set comprising the elements of Left that are not equivalent
to some element of Right.>

@xcode<@key{procedure} Symmetric_Difference (Target : @key{in out} Set;
                                Source : @key{in}     Set);>

@xindent<If Target denotes the same object as Source, then Symmetric_Difference
clears Target. Otherwise, it deletes from Target the elements that are
equivalent to some element of Source, and inserts into Target the elements of
Source that are not equivalent to some element of Target.>

@xcode<@key{function} Symmetric_Difference (Left, Right : Set) @key{return} Set;>

@xindent<Returns a set comprising the elements of Left that are not equivalent
to some element of Right, and the elements of Right that are not equivalent to
some element of Left.>

@xcode<@key{function} Overlap (Left, Right : Set) @key{return} Boolean;>

@xindent<If an element of Left is equivalent to some element of Right, then
Overlap returns True. Otherwise it returns False.>

AARM Notes: This operation is commutative. If Overlap returns False, the two
sets are disjoint.

@xcode<@key{function} Is_Subset (Subset : Set;
                    Of_Set : Set) @key{return} Boolean;>

@xindent<If an element of Subset is not equivalent to some element of Of_Set,
then Is_Subset returns False. Otherwise it returns True.>

AARM Note: This operation is not commutative, so we use parameter names that
make it clear in named notation which set is which.

Both Containers.Hashed_Set and Containers.Ordered_Set declare a nested generic
package Generic_Keys, which provides operations that allow set manipulation in
terms of a key (typically, a portion of an element) instead of a complete
element. The formal function Key of Generic_Keys extracts a key value from an
element. It is expected to return the same value each time it is called with a
particular element. The behavior of Generic_Keys is unspecified if Key behaves
in some other manner.

A key is expected to unambiguously determine one equivalence class for elements.
The behavior of Generic_Keys is unspecified if the formal parameters of this
package behave in some other manner.

The subprograms in package Generic_Keys named Contains, Find, Element, Delete,
and Exclude, are equivalent to the corresponding subprograms in the parent
package, with the difference that the Key parameter is used locate an element
in the set.

@xcode<@key{procedure} Replace (Container : @key{in out} Set;
                   Key       : @key{in}     Key_Type;
                   New_Item  : @key{in}     Element_Type);>

@xindent<Equivalent to Replace_Element (Container, Find (Container, Key), New_Item).>

@xcode<@key{function} Key (Position : Cursor) @key{return} Key_Type;>

@xindent<Equivalent to Key (Element (Position)).>

@xcode<@key{procedure} Update_Element_Preserving_Key
  (Container : @key{in out} Set;
   Position  : @key{in}     Cursor;
   Process   : @key{not null access procedure}
                                 (Element : @key{in out} Element_Type));>

@xindent<If Position equals No_Element, then Constraint_Error is propagated. If
Position does not designate an element in Container, then Program_Error is
propagated. Otherwise, Update_Element_Preserving_Key uses Key to save the key
value K of the element designated by Position. Update_Element_Preserving_Key
then calls Process.@key{all} with that element as the argument. Program_Error is
propagated if Process.@key{all} tampers with the elements of Container. Any
exception raised by Process.@key{all} is propagated. After Process.@key{all}
returns, Update_Element_Preserving_Key checks if K determines the same
equivalence class as that for the new element; if not, the element is removed
from the set and Program_Error is propagated.>

AARM Note: The key check insures that the invariants of the set are preserved
by the modification.

@xindent<If Element_Type is unconstrained and definite, then the Element
parameter of Process.@key{all} shall be unconstrained.>

AARM Note: This means that the elements cannot be directly allocated from the
heap (nor aliased unless AI-363 is included in the Amendment); it must be
possible to change the discriminants of the element in place.

@i<@s8<Erroneous Execution>>

A Cursor value is @i<invalid> if any of the following have occurred since it
was created:
@xbullet<The set that contains the element it designates has been finalized;>
@xbullet<The set that contains the element it designates has been used as the
Source or Target of a call to Move;>
@xbullet<The element it designates has been deleted from the set.>

The result of "=" or Has_Element is unspecified if these functions are called
with an invalid cursor parameter. Execution is erroneous if any other
subprogram declared in Containers.Hashed_Sets or Containers.Ordered_Sets is
called with an invalid cursor parameter.

AARM Notes: The list above is intended to be exhaustive. In other cases, a
cursor value continues to designate its original element. For instance, cursor
values survive the insertion and deletion of other elements.

While it is possible to check for these cases, in many cases the overhead
necessary to make the check is substantial in time or space. Implementations
are encouraged to check for as many of these cases as possible and raise
Program_Error if detected.
End AARM Notes.

@i<@s8<Implementation Requirements>>

No storage associated with a Set object shall be lost upon assignment or scope
exit.

@i<@s8<Implementation Advice>>

Move should not copy elements, and should minimize copying of internal
data structures.

AARM Note: Usually that can be accomplishing simply by moving the pointer(s) to
the internal data structures from the Source container to the Target container.

!corrigendum A.18.8

@dinsc

@i<@s8<Static Semantics>>

The generic library package Containers.Hashed_Sets has the following
declaration:

@xcode<@key{generic}
   @key{type} Element_Type @key{is private};
   @key{with function} Hash (Element : Element_Type) @key{return} Hash_Type;
   @key{with function} Equivalent_Elements (Left, Right : Element_Type)
                 @key{return} Boolean;
   @key{with function} "=" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Hashed_Sets @key{is}
   @key{pragma} Preelaborate (Hashed_Sets);

   @key{type} Set @key{is tagged private};

   @key{type} Cursor @key{is private};

   Empty_Set : @key{constant} Set;

   No_Element : @key{constant} Cursor;

   @key{function} "=" (Left, Right : Set) @key{return} Boolean;

   @key{function} Equivalent_Sets (Left, Right : Set) @key{return} Boolean;

   @key{function} Length (Container : Set) @key{return} Count_Type;

   @key{function} Is_Empty (Container : Set) @key{return} Boolean;

   @key{procedure} Clear (Container : @key{in out} Set);

   @key{function} Element (Position : Cursor) @key{return} Element_Type;

   @key{procedure} Query_Element
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Element : @key{in} Element_Type));

   @key{procedure} Replace_Element (Container : @key{in} Set;
                              Position  : @key{in} Cursor;
                              By        : @key{in} Element_Type);

   @key{procedure} Move (Target : @key{in out} Set;
                   Source : @key{in out} Set);

   @key{procedure} Insert (Container : @key{in out} Set;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean);

   @key{procedure} Insert (Container : @key{in out} Set;
                     New_Item  : @key{in}     Element_Type);

   @key{procedure} Include (Container : @key{in out} Set;
                      New_Item  : @key{in}     Element_Type);

   @key{procedure} Replace (Container : @key{in out} Set;
                      New_Item  : @key{in}     Element_Type);

   @key{procedure} Delete (Container : @key{in out} Set;
                     Item      : @key{in}     Element_Type);

   @key{procedure} Delete (Container : @key{in out} Set;
                     Position  : @key{in out} Cursor);

   @key{procedure} Exclude (Container : @key{in out} Set;
                      Item      : @key{in}     Element_Type);

   @key{function} Contains (Container : Set;
                      Item      : Element_Type) @key{return} Boolean;

   @key{function} Find (Container : Set;
                  Item      : Element_Type) @key{return} Cursor;

   @key{function} First (Container : Set) @key{return} Cursor;

   @key{function} Next (Position : Cursor) @key{return} Cursor;

   @key{procedure} Next (Position : @key{in out} Cursor);

   @key{function} Has_Element (Position : Cursor) @key{return} Boolean;

   @key{function} Equivalent_Elements (Left, Right : Cursor)
     @key{return} Boolean;

   @key{function} Equivalent_Elements (Left  : Cursor;
                                 Right : Element_Type)
    @key{return} Boolean;

   @key{function} Equivalent_Elements (Left  : Element_Type;
                                 Right : Cursor)
    @key{return} Boolean;

   @key{procedure} Iterate
     (Container : @key{in} Set;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));

   @key{procedure} Union (Target : @key{in out} Set;
                    Source : @key{in}     Set);

   @key{function} Union (Left, Right : Set) @key{return} Set;

   @key{function} "or" (Left, Right : Set) @key{return} Set @key{renames} Union;

   @key{procedure} Intersection (Target : @key{in out} Set;
                           Source : @key{in}     Set);

   @key{function} Intersection (Left, Right : Set) @key{return} Set;

   @key{function} "and" (Left, Right : Set) @key{return} Set @key{renames} Intersection;

   @key{procedure} Difference (Target : @key{in out} Set;
                         Source : @key{in}     Set);

   @key{function} Difference (Left, Right : Set) @key{return} Set;

   @key{function} "-" (Left, Right : Set) @key{return} Set @key{renames} Difference;

   @key{procedure} Symmetric_Difference (Target : @key{in out} Set;
                                   Source : @key{in}     Set);

   @key{function} Symmetric_Difference (Left, Right : Set) @key{return} Set;

   @key{function} "xor" (Left, Right : Set) @key{return} Set
     @key{renames} Symmetric_Difference;

   @key{function} Overlap (Left, Right : Set) @key{return} Boolean;

   @key{function} Is_Subset (Subset : Set;
                       Of_Set : Set) @key{return} Boolean;

   @key{function} Capacity (Container : Set) @key{return} Count_Type;

   @key{procedure} Reserve_Capacity (Container : @key{in out} Set;
                               Capacity  : @key{in}     Count_Type);

   @key{generic}
      @key{type} Key_Type (<>) @key{is limited private};
      @key{with function} Key (Element : @key{in} Element_Type) @key{return} Key_Type;
      @key{with function} Hash (Key : Key_Type) @key{return} Hash_Type;
      @key{with function} Equivalent_Keys (Left : Key_Type; Right : Element_Type)
                                     @key{return} Boolean;
   @key{package} Generic_Keys @key{is}

      @key{function} Contains (Container : Set;
                         Key       : Key_Type)
         @key{return} Boolean;

      @key{function} Find (Container : Set;
                     Key       : Key_Type)
         @key{return} Cursor;

      @key{function} Key (Position : Cursor) @key{return} Key_Type;

      @key{function} Element (Container : Set;
                        Key       : Key_Type)
        @key{return} Element_Type;

      @key{procedure} Replace (Container : @key{in out} Set;
                         Key       : @key{in}     Key_Type;
                         New_Item  : @key{in}     Element_Type);

      @key{procedure} Delete (Container : @key{in out} Set;
                        Key       : @key{in}     Key_Type);

      @key{procedure} Exclude (Container : @key{in out} Set;
                         Key       : @key{in}     Key_Type);

      @key{procedure} Update_Element_Preserving_Key
        (Container : @key{in out} Set;
         Position  : @key{in}     Cursor;
         Process   : @key{not null access procedure}
                                       (Element : @key{in out} Element_Type));

      @key{function} Equivalent_Keys (Left  : Cursor;
                                Right : Key_Type)
        @key{return} Boolean;

      @key{function} Equivalent_Keys (Left  : Key_Type;
                                Right : Cursor)
        @key{return} Boolean;

   @key{end} Generic_Keys;

@key{private}

   ... -- @RI[not specified by the language]

@key{end} Ada.Containers.Hashed_Sets;>

An object of type Set contains an expandable hash table, which is used to
provide direct access to elements. The @i<capacity> of an object of type Set is
the maximum number of elements that can be inserted into the hash table prior
to it being automatically expanded.

Two elements @i<E1> and @i<E2> are defined to be @i<equivalent> if
Equivalent_Elements (@i<E1>, @i<E2>) returns True.

Function Hash is expected to return the same value each time it is called with
a particular element value. For any two equivalent elements, Hash is expected
to return the same value. If Hash behaves in some other manner, the behavior of
this package is unspecified. Which subprograms of this package call Hash, and
how many times they call it, is unspecified.

Function Equivalent_Elements is expected to return the same value each time
it is called with a particular pair of element values. For any two elements @i<E1>
and @i<E2>, the boolean values Equivalent_Elements (@i<E1>, @i<E2>) and
Equivalent_Elements (@i<E2>, @i<E1>)
are expected to be equal. If Equivalent_Elements behaves in some other manner,
the behavior of this package is unspecified. Which subprograms of this package
call Equivalent_Elements, and how many times they call it, is unspecified.

If the value of an element stored in a set is changed other than by an
operation in this package such that at least one of Hash or Equivalent_Elements
give different results, the behavior of this package is unspecified.

AARM Note
See Hashed_Maps for a suggested implementation, and for justification of the
restrictions regarding Hash and Equivalent_Elements. Note that sets only need
to store elements, not key/element pairs.

Which elements are the first element and the last element of a set, and which
element is the successor of a given element, are unspecified, other than the
general semantics described in A.18.7.

@xcode<@key{procedure} Clear (Container : @key{in out} Set);>

@xindent<In addition to the semantics described in A.18.7, Clear does not
affect the capacity of Container.>

@xcode<@key{procedure} Insert (Container : @key{in out} Set;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean);>

@xindent<In addition to the semantics described in A.18.7, if Length
(Container) equals Capacity (Container), then Insert first calls
Reserve_Capacity to increase the capacity of Container to some larger value.>

@xcode<@key{function} First (Container : Set) @key{return} Cursor;>

@xindent<If Length (Container) = 0, then First returns No_Element. Otherwise,
First returns a cursor that designates the first hashed element in Container.>

@xcode<@key{function} Equivalent_Elements (Left, Right : Cursor)
      @key{return} Boolean;>

@xindent<Equivalent to Equivalent_Elements (Element (Left), Element (Right)).>

@xcode<@key{function} Equivalent_Elements (Left  : Cursor;
                              Right : Element_Type) @key{return} Boolean;>

@xindent<Equivalent to Equivalent_Elements (Element (Left), Right).>

@xcode<@key{function} Equivalent_Elements (Left  : Element_Type;
                              Right : Cursor) @key{return} Boolean;>

@xindent<Equivalent to Equivalent_Elements (Left, Element (Right)).>

@xcode<@key{function} Capacity (Container : Set) @key{return} Count_Type;>

@xindent<Returns the capacity of Container.>

@xcode<@key{procedure} Reserve_Capacity (Container : @key{in out} Set;
                            Capacity  : @key{in}     Count_Type);>

@xindent<Reserve_Capacity allocates a new hash table such that the length of the
resulting set can become at least the value Capacity without requiring an
additional call to Reserve_Capacity, and is large enough to hold the current
length of Container. Reserve_Capacity then rehashes the nodes in Container onto
the new hash table. It replaces the old hash table with the new hash table, and
then deallocates the old hash table. Any exception raised during allocation is
propagated and Container is not modified.>

@xindent<Reserve_Capacity tampers with the cursors of Container.>

AARM Note:
Reserve_Capacity tampers with the cursors, as rehashing probably will change
the relationships of the elements in Container.

For any element @i<E>, the function Generic_Keys.Hash must be such that
Hash (@i<E>) = Generic_Keys.Hash (Key (@i<E>)). If Key or Generic_Keys.Hash
behave in some other manner, the behavior of Generic_Keys is unspecified. Which
subprograms of Generic_Keys call Generic_Keys.Hash, and how many times they
call it, is unspecified.

For any two elements @i<E1> and @i<E2>, the boolean values
Equivalent_Element (@i<E1>, @i<E2>), Equivalent_Keys (Key (@i<E1>), @i<E2>),
and Equivalent_Keys (Key (@i<E2>), @i<E1>) are all expected to be equal. If Key
or Equivalent behave in some other manner, the behavior of Generic_Keys is
unspecified. Which subprograms of Generic_Keys call Equivalent, and how many
times they call it, is unspecified.

@xcode<@key{function} Equivalent_Keys (Left  : Cursor;
                          Right : Key_Type) @key{return} Boolean;>

@xindent<Equivalent to Equivalent_Keys (Key (Left), Right).>

@xcode<@key{function} Equivalent_Keys (Left  : Key_Type;
                          Right : Cursor) @key{return} Boolean;>

@xindent<Equivalent to Equivalent_Keys (Left, Key (Right)).>

@i<@s8<Implementation Advice>>

If @i<N> is the length of a set, the average time complexity of the subprograms
Insert, Include, Replace, Delete, Exclude and Find that take an element
parameter should be O(log @i<N>). The average time complexity of the
subprograms that take a cursor parameter should be O(1). The average time
complexity of Reserve_Capacity should be O(@i<N>).

AARM Note:
See Containers.Hashed_Maps (see A.18.5) for implementation notes regarding some
of the operations of this package.

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Ada.Containers.Hashed_Sets is new.]}
@end{Extend95}

!corrigendum A.18.9

@dinsc

@i<@s8<Static Semantics>>

The generic library package Containers.Ordered_Sets has the following
declaration:

@xcode<@key{generic}
   @key{type} Element_Type @key{is private};
   @key{with function} "<" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
   @key{with function} "=" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Ordered_Sets @key{is}
   @key{pragma} Preelaborate (Ordered_Sets);

   @key{type} Set @key{is tagged private};

   @key{type} Cursor @key{is private};

   Empty_Set : @key{constant} Set;

   No_Element : @key{constant} Cursor;

   @key{function} "=" (Left, Right : Set) @key{return} Boolean;

   @key{function} Equivalent_Sets (Left, Right : Set) @key{return} Boolean;

   @key{function} Length (Container : Set) @key{return} Count_Type;

   @key{function} Is_Empty (Container : Set) @key{return} Boolean;

   @key{procedure} Clear (Container : @key{in out} Set);

   @key{function} Element (Position : Cursor) @key{return} Element_Type;

   @key{procedure} Query_Element
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Element : @key{in} Element_Type));

   @key{procedure} Replace_Element (Container : @key{in} Set;
                              Position  : @key{in} Cursor;
                              By        : @key{in} Element_Type);

   @key{procedure} Move (Target : @key{in out} Set;
                   Source : @key{in out} Set);

   @key{procedure} Insert (Container : @key{in out} Set;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean);

   @key{procedure} Insert (Container : @key{in out} Set;
                     New_Item  : @key{in}     Element_Type);

   @key{procedure} Include (Container : @key{in out} Set;
                      New_Item  : @key{in}     Element_Type);

   @key{procedure} Replace (Container : @key{in out} Set;
                      New_Item  : @key{in}     Element_Type);

   @key{procedure} Delete (Container : @key{in out} Set;
                     Item      : @key{in}     Element_Type);

   @key{procedure} Exclude (Container : @key{in out} Set;
                      Item      : @key{in}     Element_Type);

   @key{procedure} Delete (Container : @key{in out} Set;
                     Position  : @key{in out} Cursor);

   @key{procedure} Delete_First (Container : @key{in out} Set);

   @key{procedure} Delete_Last (Container : @key{in out} Set);

   @key{procedure} Union (Target : @key{in out} Set;
                    Source : @key{in}     Set);

   @key{function} Union (Left, Right : Set) @key{return} Set;

   @key{function} "or" (Left, Right : Set) @key{return} Set @key{renames} Union;

   @key{procedure} Intersection (Target : @key{in out} Set;
                           Source : @key{in}     Set);

   @key{function} Intersection (Left, Right : Set) @key{return} Set;

   @key{function} "and" (Left, Right : Set) @key{return} Set @key{renames} Intersection;

   @key{procedure} Difference (Target : @key{in out} Set;
                         Source : @key{in}     Set);

   @key{function} Difference (Left, Right : Set) @key{return} Set;

   @key{function} "-" (Left, Right : Set) @key{return} Set @key{renames} Difference;

   @key{procedure} Symmetric_Difference (Target : @key{in out} Set;
                                   Source : @key{in}     Set);

   @key{function} Symmetric_Difference (Left, Right : Set) @key{return} Set;

   @key{function} "xor" (Left, Right : Set) @key{return} Set @key{renames}
      Symmetric_Difference;

   @key{function} Overlap (Left, Right : Set) @key{return} Boolean;

   @key{function} Is_Subset (Subset : Set;
                       Of_Set : Set) @key{return} Boolean;

   @key{function} Contains (Container : Set;
                      Item      : Element_Type) @key{return} Boolean;

   @key{function} Find (Container : Set;
                  Item      : Element_Type)
      @key{return} Cursor;

   @key{function} Floor (Container : Set;
                   Item      : Element_Type)
      @key{return} Cursor;

   @key{function} Ceiling (Container : Set;
                     Item      : Element_Type)
      @key{return} Cursor;

   @key{function} First (Container : Set) @key{return} Cursor;

   @key{function} First_Element (Container : Set) @key{return} Element_Type;

   @key{function} Last (Container : Set) @key{return} Cursor;

   @key{function} Last_Element (Container : Set) @key{return} Element_Type;

   @key{function} Next (Position : Cursor) @key{return} Cursor;

   @key{procedure} Next (Position : @key{in out} Cursor);

   @key{function} Previous (Position : Cursor) @key{return} Cursor;

   @key{procedure} Previous (Position : @key{in out} Cursor);

   @key{function} Has_Element (Position : Cursor) @key{return} Boolean;

   @key{function} "<" (Left, Right : Cursor) @key{return} Boolean;

   @key{function} ">" (Left, Right : Cursor) @key{return} Boolean;

   @key{function} "<" (Left : Cursor; Right : Element_Type)
      @key{return} Boolean;

   @key{function} ">" (Left : Cursor; Right : Element_Type)
      @key{return} Boolean;

   @key{function} "<" (Left : Element_Type; Right : Cursor)
      @key{return} Boolean;

   @key{function} ">" (Left : Element_Type; Right : Cursor)
      @key{return} Boolean;

   @key{procedure} Iterate
     (Container : @key{in} Set;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));

   @key{procedure} Reverse_Iterate
     (Container : @key{in} Set;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));

   @key{generic}
      @key{type} Key_Type (<>) @key{is limited private};
      @key{with function} Key (Element : Element_Type) @key{return} Key_Type;
      @key{with function} "<" (Left : Key_Type; Right : Element_Type)
         @key{return} Boolean @key{is} <>;
      @key{with function} ">" (Left : Key_Type; Right : Element_Type)
         @key{return} Boolean @key{is} <>;
   @key{package} Generic_Keys @key{is}

       @key{function} Contains (Container : Set;
                          Key       : Key_Type) @key{return} Boolean;

       @key{function} Find (Container : Set;
                      Key       : Key_Type)
          @key{return} Cursor;

       @key{function} Floor (Container : Set;
                       Item      : Key_Type)
          @key{return} Cursor;

       @key{function} Ceiling (Container : Set;
                         Item      : Key_Type)
          @key{return} Cursor;

       @key{function} Key (Position : Cursor) @key{return} Key_Type;

       @key{function} Element (Container : Set;
                         Key       : Key_Type)
          @key{return} Element_Type;

       @key{procedure} Replace (Container : @key{in out} Set;
                          Key       : @key{in}     Key_Type;
                          New_Item  : @key{in}     Element_Type);

       @key{procedure} Delete (Container : @key{in out} Set;
                         Key       : @key{in}     Key_Type);

       @key{procedure} Exclude (Container : @key{in out} Set;
                          Key       : @key{in}     Key_Type);

       @key{function} "<" (Left : Cursor; Right : Key_Type)
          @key{return} Boolean;

       @key{function} ">" (Left : Cursor; Right : Key_Type)
          @key{return} Boolean;

       @key{function} "<" (Left : Key_Type; Right : Cursor)
          @key{return} Boolean;

       @key{function} ">" (Left : Key_Type; Right : Cursor)
          @key{return} Boolean;

       @key{procedure} Update_Element_Preserving_Key
         (Container : @key{in out} Set;
          Position  : @key{in}     Cursor;
          Process   : @key{not null access procedure}
                                         (Element : @key{in out} Element_Type));

   @key{end} Generic_Keys;

@key{private}

   ... -- @RI[not specified by the language]

@key{end} Ada.Containers.Ordered_Sets;>

Two elements @i<E1> and @i<E2> are @i<equivalent> if both @i<E1> < @i<E2> and
@i<E2> < @i<E1> return False, using the generic formal "<" operator for
elements.

Functions "<" and "=" on Element_Type values are expected to return the same
result value each time they are called with a particular pair of element values.
If @i<A> = @i<B> returns True, then @i<B> = @i<A> is expected to also return
True. If @i<A> < @i<B> returns True, then @i<B> < @i<A> is expected to return
False. For any two equivalent elements, "=" is expected to return True. If "<"
or "=" behaves in some other manner, the behavior of this package is
unspecified. Which subprograms of this package call "<" and "=", and how many
times these functions are called, is unspecified.

If the value of an element stored in a set is changed other than by an
operation in this package such that at least one of "<" or "=" give different
results, the behavior of this package is unspecified.

AARM Note
See Ordered_Maps for a suggested implementation, and for justification of the
restrictions regarding "<" and "=". Note that sets only need to store elements,
not key/element pairs.

The first element of a nonempty set is the one which is less than all the other
elements in the set. The last element of a nonempty set is the one which is
greater than all the other elements in the set. The successor of an element is
the smallest element that is larger than the given element. The predecessor of
an element is the largest element that is smaller than the given element. All
comparisons are done using the generic formal "<" operator for elements.

@xcode<@key{procedure} Delete_First (Container : @key{in out} Set);>

@xindent<If Container is empty, Delete_First has no effect. Otherwise the element
designated by First (Container) is removed from Container. Delete_First
tampers with the cursors of Container.>

@xcode<@key{procedure} Delete_Last (Container : @key{in out} Set);>

@xindent<If Container is empty, Delete_Last has no effect. Otherwise the
element designated by Last (Container) is removed from Container. Delete_Last
tampers with the cursors of Container.>

@xcode<@key{function} Floor (Container : Set;
                Item      : Element_Type) @key{return} Cursor;>

@xindent<Floor searches for the last element which is not greater than Item. If
such an element is found, a cursor that designates it is returned. Otherwise
No_Element is returned.>

@xcode<@key{function} Ceiling (Container : Set;
                  Item      : Element_Type) @key{return} Cursor;>

@xindent<Ceiling searches for the first element which is not less than Item. If
such an element is found, a cursor that designates it is returned. Otherwise
No_Element is returned.>

@xcode<@key{function} First_Element (Container : Set) @key{return} Element_Type;>

@xindent<Equivalent to Element (First (Container)).>

@xcode<@key{function} Last (Container : Set) @key{return} Cursor;>

@xindent<Returns a cursor that designates the last node in Container. If Container is
empty, returns No_Element.>

@xcode<@key{function} Last_Element (Container : Set) @key{return} Element_Type;>

@xindent<Equivalent to Element (Last (Container)).>

@xcode<@key{function} Previous (Position : Cursor) @key{return} Cursor;>

@xindent<If Position equals No_Element, then Previous returns No_Element.
Otherwise Previous returns a cursor designating the element that precedes the
one designated by Position. If Position designates the first element, then
Previous returns No_Element.>

@xcode<@key{procedure} Previous (Position : @key{in out} Cursor);>

@xindent<Equivalent to Position := Previous (Position).>

@xcode<@key{function} "<" (Left, Right : Cursor) @key{return} Boolean;>

@xindent<Equivalent to Element (Left) < Element (Right).>

@xcode<@key{function} ">" (Left, Right : Cursor) @key{return} Boolean;>

@xindent<Equivalent to Element (Right) < Element (Left).>

@xcode<@key{function} "<" (Left : Cursor; Right : Element_Type) @key{return} Boolean;>

@xindent<Equivalent to Element (Left) < Right.>

@xcode<@key{function} ">" (Left : Cursor; Right : Element_Type) @key{return} Boolean;>

@xindent<Equivalent to Right < Element (Left).>

@xcode<@key{function} "<" (Left : Element_Type; Right : Cursor) @key{return} Boolean;>

@xindent<Equivalent to Left < Element (Right).>

@xcode<@key{function} ">" (Left : Element_Type; Right : Cursor) @key{return} Boolean;>

@xindent<Equivalent to Element (Right) < Left.>

@xcode<@key{procedure} Reverse_Iterate
   (Container : @key{in} Set;
    Process   : @key{not null access procedure} (Position : @key{in} Cursor));>

@xindent<Iterates over the elements in Container as per Iterate, with the
difference that the elements are traversed in predecessor order, starting with
the last node.>

For any two elements @i<E1> and @i<E2>, the boolean values (@i<E1> < @i<E2>),
(Key(@i<E1>) < @i<E2>), and (Key(@i<E2>) > @i<E1>) are all expected to be equal.
If Key, Generic_Keys."<", or Generic_Keys.">" behave in some other manner, the
behavior of this package is unspecified. Which subprograms of this package
call Key, Generic_Keys."<" and Generic_Keys.">", and how many times the
functions are called, is unspecified.

In addition to the semantics described in A.18.7, the subprograms in package
Generic_Keys named Floor, Ceiling, "<", and ">", are equivalent to the
corresponding subprograms in the parent package, with the difference that the
Key subprogram parameter is compared to elements in the container using the
Generic_Keys generic formal relational operators.

@i<@s8<Implementation Advice>>

If @i<N> is the length of a set, then the worst-case time complexity of the
Insert, Include, Replace, Delete, Exclude and Find operations that take an
element parameter should be O((log @i<N>)**2) or better. The worst-case time
complexity of the subprograms that take a cursor parameter should be O(1).

AARM Note:
See Containers.Ordered_Maps (see A.18.6) for implementation notes regarding
some of the operations of this package.

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Ada.Containers.Ordered_Sets is new.]}
@end{Extend95}

!corrigendum A.18.10

@dinsc

The language-defined generic package Containers.Indefinite_Vectors provides a
private type Vector and a set of operations. It provides the same
operations as the package Containers.Vectors (see A.18.2) does, with the
difference that the generic formal Element_Type is indefinite.

@i<@s8<Static Semantics>>

The declaration of the generic library package
Containers.Indefinite_Vectors@ChildUnit{Parent=[Ada.Containers],Child=[Indefinite_Vectors]}
has the same contents as Containers.Vectors except:

@xbullet<The generic formal Element_Type is indefinite.>

@xbullet<The Element parameter of access subprogram Process of Update_Element
may be constrained even if Element_Type is unconstrained.>

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Ada.Containers.Indefinite_Vectors is new.]}
@end{Extend95}

!corrigendum A.18.11

@dinsc

The language-defined generic package Containers.Indefinite_Doubly_Linked_Lists
provides private types List and Cursor, and a set of operations for each
type. It provides the same operations as the package
Containers.Doubly_Linked_Lists (see A.18.3) does, with the difference that the
generic formal Element_Type is indefinite.

@i<@s8<Static Semantics>>

The declaration of the generic library package
Containers.Indefinite_Doubly_Linked_Lists@ChildUnit{Parent=[Ada.Containers],Child=[Indefinite_Doubly_Linked_Lists]}
has the same contents as
Containers.Doubly_Linked_Lists except:

@xbullet<The generic formal Element_Type is indefinite.>

@xbullet<The procedure with the profile:>
@xcode<     @key{procedure} Insert (Container : @key{in out} List;
                       Before    : @key{in}     Cursor;
                       Position  :    @key{out} Cursor;
                       Count     : @key{in}     Count_Type := 1);>
@xindent<is omitted.>

AARM Note: This procedure is omitted because there is no way to create a
default-initialized object of an indefinite type. We considered having this
routine insert an empty element similar to the empty elements of a vector,
but rejected this possibility because the semantics are fairly complex and
very different from the existing case. That would make it more error-prone
to convert a container from a definite type to an indefinite type; by
omitting the routine completely, any problems will be diagnosed by the
compiler.

@xbullet<The Element parameter of access subprogram Process of Update_Element
may be constrained even if Element_Type is unconstrained.>

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Ada.Containers.Indefinite_Doubly_Linked_Lists is new.]}
@end{Extend95}

!corrigendum A.18.12

@dinsc

The language-defined generic package Containers.Indefinite_Hashed_Maps provides
a map with the same operations as the package Containers.Hashed_Maps (see A.18.5),
with the difference that the generic formal types Key_Type and Element_Type are
indefinite.

@i<@s8<Static Semantics>>

The declaration of the generic library package
Containers.Indefinite_Hashed_Maps@ChildUnit{Parent=[Ada.Containers],Child=[Indefinite_Hashed_Maps]}
has the same contents as Containers.Hashed_Maps except:

@xbullet<The generic formal Key_Type is indefinite.>

@xbullet<The generic formal Element_Type is indefinite.>

@xbullet<The procedure with the profile:>
@xcode<     @key{procedure} Insert (Container : @key{in out} Map;
                       Key       : @key{in}     Key_Type;
                       Position  :    @key{out} Cursor;
                       Inserted  :    @key{out} Boolean);>
@xindent<is omitted.>

AARM Note: This procedure is omitted because there is no way to create a
default-initialized object of an indefinite type. We considered having this
routine insert an empty element similar to the empty elements of a vector,
but rejected this possibility because the semantics are fairly complex and
very different from the existing case. That would make it more error-prone
to convert a container from a definite type to an indefinite type; by
omitting the routine completely, any problems will be diagnosed by the
compiler.

@xbullet<The Element parameter of access subprogram Process of Update_Element
may be constrained even if Element_Type is unconstrained.>

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Ada.Containers.Indefinite_Hashed_Maps is new.]}
@end{Extend95}

!corrigendum A.18.13

@dinsc

The language-defined generic package Containers.Indefinite_Ordered_Maps
provides a map with the same operations as the package Containers.Ordered_Maps
(see A.18.6), with the difference that the generic formal types Key_Type and
Element_Type are indefinite.

@i<@s8<Static Semantics>>

The declaration of the generic library package
Containers.Indefinite_Ordered_Maps@ChildUnit{Parent=[Ada.Containers],Child=[Indefinite_Ordered_Maps]}
has the same contents as
Containers.Ordered_Maps except:

@xbullet<The generic formal Key_Type is indefinite.>

@xbullet<The generic formal Element_Type is indefinite.>

@xbullet<The procedure with the profile:>
@xcode<     @key{procedure} Insert (Container : @key{in out} Map;
                       Key       : @key{in}     Key_Type;
                       Position  :    @key{out} Cursor;
                       Inserted  :    @key{out} Boolean);>
@xindent<is omitted.>

AARM Note: This procedure is omitted because there is no way to create a
default-initialized object of an indefinite type. We considered having this
routine insert an empty element similar to the empty elements of a vector,
but rejected this possibility because the semantics are fairly complex and
very different from the existing case. That would make it more error-prone
to convert a container from a definite type to an indefinite type; by
omitting the routine completely, any problems will be diagnosed by the
compiler.

@xbullet<The Element parameter of access subprogram Process of Update_Element
may be constrained even if Element_Type is unconstrained.>

**** The text above here still needs to be formatted ****

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Ada.Containers.Indefinite_Ordered_Maps is new.]}
@end{Extend95}

@LabeledAddedSubclause{Version=[2],Name=[The Package Containers.Indefinite_Hashed_Sets]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The language-defined generic package
Containers.Indefinite_Hashed_Sets provides a set with the same operations as
the package Containers.Hashed_Sets
(see @RefSecNum{The Package Containers.Hashed_Sets}), with the difference
that the generic formal type Element_Type is indefinite.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[
The declaration of the generic library package
Containers.Indefinite_Hashed_Sets@ChildUnit{Parent=[Ada.Containers],Child=[Indefinite_Hashed_Sets]}
has the same contents as
Containers.Hashed_Sets except:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The generic formal Element_Type is indefinite.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The Element parameter of access subprogram Process
of Update_Element_Preserving_Key may be constrained even if Element_Type is
unconstrained.]}
@end{Itemize}
@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Ada.Containers.Indefinite_Hashed_Sets is new.]}
@end{Extend95}

@LabeledAddedSubclause{Version=[2],Name=[The Package Containers.Indefinite_Ordered_Sets]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The language-defined generic package
Containers.Indefinite_Ordered_Sets provides a set with the same operations as
the package Containers.Ordered_Sets
(see @RefSecNum{The Package Containers.Ordered_Sets}), with the difference
that the generic formal type Element_Type is indefinite.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Leading],Text=[
Containers.Indefinite_Ordered_Sets@ChildUnit{Parent=[Ada.Containers],Child=[Indefinite_Ordered_Sets]}
has the same contents as Containers.Ordered_Sets except:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The generic formal Element_Type is indefinite.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The Element parameter of access subprogram Process
of Update_Element_Preserving_Key may be constrained even if Element_Type is
unconstrained.]}
@end{Itemize}
@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Ada.Containers.Indefinite_Ordered_Sets is new.]}
@end{Extend95}

@LabeledAddedSubclause{Version=[2],Name=[Array Sorting]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The language-defined generic procedures
Containers.Generic_Array_Sort and Containers.Generic_Constrained_Array_Sort
provide sorting on arbitrary array types.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
procedure Containers.Generic_Array_Sort has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{generic}
   @key{type} Index_Type @key{is} (<>);
   @key{type} Element_Type @key{is private};
   @key{type} Array_Type @key{is array} (Index_Type @key{range} <>) @key{of} Element_Type;
   @key{with function} "<" (Left, Right : Element_Type)
      @key{return} Boolean @key{is} <>;
@key{procedure} Ada.Containers.Generic_Array_Sort (Container : @key{in out} Array_Type);@ChildUnit{Parent=[Ada.Containers],Child=[Generic_Array_Sort]}
@key{pragma} Pure (Ada.Containers.Generic_Array_Sort);]}
@end{Example}

@begin{DescribeCode}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Reorders the elements of Container such that the elements are
sorted smallest first as determined by the generic formal "<" operator
provided. Any exception raised during evaluation of "<" is propagated.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This implies swapping the elements, usually
  including an intermediate copy. This of course means that the elements will be
  copied. Since the elements are non-limited, this usually will not be a problem.
  Note that there is Implementation Advice below that the implementation should
  use a sort that minimizes copying of elements.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The sort is not required to be stable (and the fast
  algorithm required will not be stable). If a stable sort is needed, the user
  can include the original location of the element as an extra "sort key". We
  considered requiring the implementation to do that, but it is mostly extra
  overhead -- usually there is something already in the element that provides the
  needed stability.]}
@end{Ramification}

@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
procedure Containers.Generic_@!Constrained_@!Array_Sort has the following
declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{generic}
   @key{type} Index_Type @key{is} (<>);
   @key{type} Element_Type @key{is private};
   @key{type} Array_Type @key{is array} (Index_Type) @key{of} Element_Type;
   @key{with function} "<" (Left, Right : Element_Type)
      @key{return} Boolean @key{is} <>;
@key{procedure} Ada.Containers.Generic_Constrained_Array_Sort@ChildUnit{Parent=[Ada.Containers],Child=[Generic_Constrained_Array_Sort]}
      (Container : @key{in out} Array_Type);
@key{pragma} Pure (Ada.Containers.Generic_Constrained_Array_Sort);]}
@end{Example}

@begin{DescribeCode}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Reorders the elements of Container such that the
elements are sorted smallest first as determined by the generic formal "<"
operator provided. Any exception raised during evaluation of "<" is
propagated.]}
@end{DescribeCode}

@end{StaticSem}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[
The worst-case time complexity of a call on an instantiation of
Containers.Generic_Array_Sort or Containers.Generic_Constrained_Array_Sort
should be O(@i<N>**2) or better, and the average time complexity should be better
than O(@i<N>**2), where @i<N> is the length of the Container parameter.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Containers.Generic_Array_Sort and Containers.Generic_Constrained_Array_Sort
should have an average time complexity better than O(N**2) and worst case no
worse than O(N**2).]}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[In other words, we're requiring the use of
  a sorting algorithm better than O(N**2), such as Quicksort. No Bubble sorts
  allowed!]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[
Containers.Generic_Array_Sort and Containers.Generic_Constrained_Array_Sort
should minimize copying of elements.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Containers.Generic_Array_Sort and Containers.Generic_Constrained_Array_Sort
should minimize copying of elements.]}]}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We do not mean @lquotes@;absolutely minimize@rquotes@;
  here; we're not intending to require a single copy for each element. Rather,
  we want to suggest that the sorting algorithm chosen is one that does not
  copy items unnecessarily. Bubble sort would not meet this advice, for
  instance.]}
@end{Honest}
@end{ImplAdvice}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic packages Ada.Containers.Generic_Array_Sort and
Ada.Containers.Generic_Constrained_Array_Sort are new.]}
@end{Extend95}

