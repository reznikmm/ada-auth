@Part(xxx, Root="rat.msm")

@comment($Source: e:\\cvsroot/ARM/Rationale/contain.mss,v $)
@comment($Revision: 1.3 $ $Date: 2006/02/11 07:43:11 $)

@LabeledSection{Containers}

@Subheading{Abstract}

@i{This paper
describes the predefined container library in Ada 2005.}

@i{This is one of a number of papers concerning Ada 2005 which are being
published in the Ada User Journal. An earlier version of this paper
appeared in the Ada User Journal, Vol. 26, Number 4, December 2005.
Other papers in this series will be found in other issues of the Journal
or elsewhere on this website.}

@LabeledClause{Organization of containers}


A major enhancement to the predefined library in Ada 2005 is the addition
of a container library. This is quite extensive and merits this separate
paper on its own. Other aspects of the predefined library and the
overall rationale for extending the library were described in the
previous paper (see @RefSecNum{Predefined library}).

The main packages in the container library can be grouped in various
ways. One set of packages concerns the manipulation of objects of
definite types and another, essentially identical, set concerns indefinite
types. (Remember that an indefinite (sub)type is one for which we
cannot declare an object without giving a constraint.) The reason
for the duplication concerns efficiency. It is much easier to manipulate
definite types and although the packages for indefinite types can
be used for definite types, this would be rather inefficient.

We will generally only consider the definite packages. These in turn
comprise two groups.

@begin[Description]
Sequence containers @en@\these hold sequences of
elements. There are packages for manipulating vectors and for manipulating
linked lists. These two packages have much in common. But they have
different behaviours in terms of efficiency according to the pattern
of use. In general (with some planning) it should be possible to change
from one to the other with little effort.

Associative containers @en@\these associate a key
with each element and then store the elements in order of the keys.
There are packages for manipulating hashed maps, ordered maps, hashed
sets and ordered sets. These four packages also have much in common
and changing between hashed and ordered versions is usually feasible.
@end[Description]

There are also quite separate generic procedures for sorting arrays
which we will consider later.

@leading@keepnext@;The root package is@Defn2{Term=[package],Sec=[Ada.Containers]}@Defn{Ada.Containers package}@Defn{Containers package}
@begin[Example]
@key[package] Ada.Containers @key[is]
   @key[pragma] Pure(Containers);

   @key[type] Hash_Type @key[is mod] @examcom[implementation-defined];
   @key[type] Count_Type @key[is] @key[range] 0 .. @examcom[implementation-defined];

@key[end] Ada.Containers;
@end[Example]

The type @exam[Hash_Type] is used by the associative containers and
@exam[Count_Type] is used by both kinds of containers typically for
the number of elements in a container. Note that we talk about elements
in a container rather than the components in a container @en components
is the Ada term for the items of an array or record as an Ada type
and it is convenient to use a different term since in the case of
containers the actual data structure is hidden.

Worst-case and average-case time complexity bounds are given using
the familiar @i[O]( ... ) notation. This encourages implementations
to use techniques that scale reasonably well and avoid junk algorithms
such as bubble sort.

Perhaps a remark about using containers from a multitasking program
would be helpful. The general rule is given in paragraph 3 of Annex
A which says "The implementation shall ensure that each language defined
subprogram is reentrant in the sense that concurrent calls on the
same subprogram perform as specified, so long as all parameters that
could be passed by reference denote nonoverlapping objects." So in
other words we have to protect ourselves by using the normal techniques
such as protected objects when container operations are invoked concurrently
on the same object from multiple tasks even if the operations are
only reading from the container.


@LabeledClause{Lists and vectors}


@leading@;We will first consider the list container since in some ways it is
the simplest.@Defn{list container}@Defn2{Term=[container],Sec=[list]} Here
is its specification interspersed with some
explanation@Defn2{Term=[package],Sec=[Ada.Containers.Doubly_Linked_Lists]}@Defn{Ada.Containers.Doubly_Linked_Lists package}@Defn{Doubly_Linked_Lists package}
@begin[Example]
@key[generic]
   @key[type] Element_Type @key[is private];
   @key[with] @key[function] "=" (Left, Right: Element_Type) @key[return] Boolean @key[is] <>;
@key[package] Ada.Containers.Doubly_Linked_Lists @key[is]
   @key[pragma] Preelaborate(Doubly_Linked_Lists);

   @key[type] List @key[is tagged private];
   @key[pragma] Preelaborable_Initialization(List);
   @key[type] Cursor @key[is private];
   @key[pragma] Preelaborable_Initialization(Cursor);
   Empty_List: @key[constant] List;
   No_Element: @key[constant] Cursor;
@end[Example]

The two generic parameters are the type of the elements in the list
and the definition of equality for comparing elements. This equality
relation must be such that @exam[x = y] and @exam[y = x] always have
the same value.

@leading@;A list container is an object of the type @exam[List]. It is tagged
since it will inevitably be implemented as a controlled type. The
fact that it is visibly tagged means that all the advantages of object
oriented programming are available. For one thing it enables the use
of the prefixed notation so that we can write operations such as
@begin[Example]
My_List.Append(Some_Value);
@end[Example]

@leading@;rather than
@begin[Example]
Append(My_List, Some_Value);
@end[Example]

The type @exam[Cursor] is an important concept. It provides the means
of access to individual elements in the container. Not only does it
contain a reference to an element but it also identifies the container
as well. This enables various checks to be made to ensure that we
don't accidentally meddle with an element in the wrong container.

The constants @exam[Empty_List] and @exam[No_Element] are as expected
and also provide default values for objects of types @exam[List] and
@exam[Cursor] respectively.

@begin[Example]
@key[function] "=" (Left, Right: List) @key[return] Boolean;
@key[function] Length(Container: List) @key[return] Count_Type;
@key[function] Is_Empty(Container: List) @key[return] Boolean;
@key[procedure] Clear(Container: @key[in out] List);
@end[Example]

The function "=" compares two lists. It only returns true if both
lists have the same number of elements and corresponding elements
have the same value as determined by the generic parameter "=" for
comparing elements. The subprograms @exam[Length], @exam[Is_Empty]
and @exam[Clear] are as expected.

Note that @exam[A_List = Empty_List], @exam[Is_Empty(A_List)] and
@exam[Length(A_List) = 0] all have the same value.

@begin[Example]
@key[function] Element(Position: Cursor) @key[return] Element_Type;

@key[procedure] Replace_Element(
      Container: @key[in out] List; Position: @key[in] Cursor;
      New_Item: @key[in] Element_Type);
@end[Example]

These are the first operations we have met that use a cursor. The
function @exam[Element] takes a cursor and returns the value of the
corresponding element (remember that a cursor identifies the list
as well as the element itself). The procedure @exam[Replace_Element]
replaces the value of the element identified by the cursor by the
value given; it makes a copy of course.

Note carefully that @exam[Replace_Element] has both the list and cursor
as parameters. There are two reasons for this concerning correctness.
One is to enable a check that the cursor does indeed identify an element
in the given list. The other is to ensure that we do have write access
to the container (the parameter has mode @key[in out]). Otherwise
it would be possible to modify a container even though we only had
a constant view of it. So as a general principle any operation that
modifies a container must have the container as a parameter whereas
an operation that only reads it such as the function @exam[Element]
does not.

@begin[Example]
@key[procedure] Query_Element(
      Position: @key[in] Cursor;
      Process: @key[not null access procedure] (Element: @key[in] Element_Type));

@key[procedure] Update_Element(
      Container: @key[in out] List; Position: @key[in] Cursor;
      Process: @key[not null access procedure] (Element: @key[in out] Element_Type));
@end[Example]

These procedures provide @i[in situ] access to an element. One parameter
is the cursor identifying the element and another is an access to
a procedure to be called with that element as parameter. In the case
of @exam[Query_Element], we can only read the element whereas in the
case of @exam[Update_Element] we can change it as well since the parameter
mode of the access procedure is @key[in out]. Note that @exam[Update_Element]
also has the container as a parameter for reasons just mentioned when
discussing @exam[Replace_Element].

The reader might wonder whether there is any difference between calling
the function @exam[Element] to obtain the current value of an element
and using the seemingly elaborate mechanism of @exam[Query_Element].
The answer is that the function @exam[Element] makes a copy of the
value whereas @exam[Query_Element] gives access to the value without
making a copy. (And similarly for @exam[Replace_Element] and @exam[Update_Element].)
This wouldn't matter for a simple list of integers but it would matter
if the elements were large or of a controlled type (maybe even lists
themselves).

@begin[Example]
@key[procedure] Move(Target, Source: @key[in out] List);
@end[Example]

This moves the list from the source to the target after first clearing
the target. It does not make copies of the elements so that after
the operation the source is empty and @exam[Length(Source)] is zero.

@begin[Example]
@key[procedure] Insert(
      Container: @key[in out] List;
      Before: @key[in] Cursor;
      New_Item: @key[in] Element_Type;
      Count: @key[in] Count_Type := 1);

@key[procedure] Insert(
      Container: @key[in out] List;
      Before: @key[in] Cursor;
      New_Item: @key[in] Element_Type;
      Position: @key[out] Cursor;
      Count: @key[in] Count_Type := 1);

@key[procedure] Insert(
      Container: @key[in out] List;
      Before: @key[in] Cursor;
      Position: @key[out] Cursor;
      Count: @key[in] Count_Type := 1);
@end[Example]

These three procedures enable one or more identical elements to be
added anywhere in a list. The place is indicated by the parameter
@exam[Before] @en if this is @exam[No_Element], then the new elements
are added at the end. The second procedure is similar to the first
but also returns a cursor to the first of the added elements. The
third is like the second but the new elements take their default values.
Note the default value of one for the number of elements.

@begin[Example]
@key[procedure] Prepend(
      Container: @key[in out] List;
      New_Item: @key[in] Element_Type;
      Count: @key[in] Count_Type := 1);

@key[procedure] Append(
      Container: @key[in out] List;
      New_Item: @key[in] Element_Type;
      Count: @key[in] Count_Type := 1);
@end[Example]

These add one or more new elements at the beginning or end of a list
respectively. Clearly these operations can be done using @exam[Insert]
but they are sufficiently commonly needed that it is convenient to
provide them specially.

@begin[Example]
@key[procedure] Delete(
      Container: @key[in out] List;
      Position: @key[in out] Cursor;
      Count: @key[in] Count_Type := 1);

@key[procedure] Delete_First(Container: @key[in out] List; Count: @key[in] Count_Type := 1);

@key[procedure] Delete_Last(Container: @key[in out] List; Count: @key[in] Count_Type := 1);
@end[Example]

These delete one or more elements at the appropriate position. In
the case of @exam[Delete], the parameter @exam[Position] is set to
@exam[No_Element] upon return. If there are not as many as @exam[Count]
elements to be deleted at the appropriate place then it just deletes
as many as possible (this clearly results in the container becoming
empty in the case of @exam[Delete_First] and @exam[Delete_Last]).

@begin[Example]
@key[procedure] Reverse_Elements(Container: @key[in out] List);
@end[Example]

This does the obvious thing. It would have been nice to call this
procedure @exam[Reverse] but sadly that is a reserved word.

@begin[Example]
@key[procedure] Swap(Container: @key[in out] List; I, J: @key[in] Cursor);
@end[Example]

This handy procedure swaps the values in the two elements denoted
by the two cursors. The elements must be in the given container otherwise
@exam[Program_Error] is raised. Note that the cursors do not change.

@begin[Example]
@key[procedure] Swap_Links(Container: @key[in out] List; I, J: @key[in] Cursor);
@end[Example]

This performs the low level operation of swapping the links rather
than the values which can be much faster if the elements are large.
There is no analogue in the vectors package.

@begin[Example]
@key[procedure] Splice(
      Target: @key[in out] List;
      Before: @key[in] Cursor;
      Source @key[in out] List);

@key[procedure] Splice(
      Target: @key[in out] List;
      Before: @key[in] Cursor;
      Source: @key[in out] List;
      Position: @key[in out] Cursor);

@key[procedure] Splice(
      Container: @key[in out] List;
      Before: @key[in] Cursor;
      Position: @key[in out] Cursor);
@end[Example]

These three procedures enable elements to be moved (without copying).
The place is indicated by the parameter @exam[Before] @en if this
is @exam[No_Element], then the elements are added at the end. The
first moves all the elements of @exam[Source] into @exam[Target] at
the position given by @exam[Before]; as a consequence, like the procedure
@exam[Move], after the operation the source is empty and @exam[Length(Source)]
is zero. The second moves a single element at @exam[Position] from
the list @exam[Source] to @exam[Target] and so the length of target
is incremented whereas that of source is decremented; @exam[Position]
is updated to its new location in @exam[Target]. The third moves a
single element within a list and so the length remains the same (note
the formal parameter is @exam[Container] rather than @exam[Target]
in this case). There are no corresponding operations in the vectors
package because, like @exam[Swap_Links], we are just moving
the links and not copying the elements.

@begin[Example]
@key[function] First(Container: List) @key[return] Cursor;
@key[function] First_Element(Container: List) @key[return] Element_Type;
@key[function] Last(Container: List) @key[return] Cursor;
@key[function] Last_Element(Container: List) @key[return] Element_Type;
@key[function] Next(Position: Cursor) @key[return] Cursor;
@key[function] Previous(Position: Cursor) @key[return] Cursor;
@key[procedure] Next(Position: @key[in out] Cursor);
@key[procedure] Previous(Position: @key[in out] Cursor);

@key[function] Find(
      Container: List;
      Item: Element_Type;
      Position: Cursor:= No_Element) @key[return] Cursor;

@key[function] Reverse_Find(
      Container: List;
      Item: Element_Type;
      Position: Cursor:= No_Element) @key[return] Cursor;

@key[function] Contains(Container: List; Item: Element_Type) @key[return] Boolean;
@end[Example]

Hopefully the purpose of these is almost self-evident. The function
@exam[Find] searches for an element with the given value starting
at the given cursor position (or at the beginning if the position
is @exam[No_Element)]; if no element is found then it returns @exam[No_Element].
@exam[Reverse_Find] does the same but backwards. Note that equality
used for the comparison in @exam[Find] and @exam[Reverse_Find] is
that defined by the generic parameter @exam["="].

@begin[Example]
@key[function] Has_Element(Position: Cursor) @key[return] Boolean;
@end[Example]

This returns @exam[False] if the cursor does not identify an element;
for example if it is @exam[No_Element].

@begin[Example]
@key[procedure] Iterate(
      Container: @key[in] List;
      Process: @key[not null access procedure] (Position: @key[in] Cursor));

@key[procedure] Reverse_Iterate(
      Container: @key[in] List;
      Process: @key[not null access procedure] (Position: @key[in] Cursor));
@end[Example]

These apply the procedure designated by the parameter @exam[Process]
to each element of the container in turn in the appropriate order.

@begin[Example]
@key[generic]
   @key[with function] "<" (Left, Right: Element_Type) @key[return] Boolean @key[is] <>;
@key[package] Generic_Sorting is
   @key[function] Is_Sorted(Container: List) @key[return] Boolean;
   @key[procedure] Sort(Container: @key[in out] List);
   @key[procedure] Merge(Target, Source: @key[in out] List);
@key[end] Generic_Sorting;

@end[Example]
This generic package performs sort and merge operations using the
order specified by the generic formal parameter. Note that we use
generics rather than access to subprogram parameters when the formal
process is given by an operator. This is because the predefined operations
have convention @exam[Intrinsic] and one cannot pass an intrinsic
operation as an access to subprogram parameter. The function @exam[Is_Sorted]
returns @exam[True] if the container is already sorted. The procedure
@exam[Sort] arranges the elements into order as necessary @en note
that no copying is involved since it is only the links that are moved.
The procedure @exam[Merge] takes the elements from @exam[Source] and
adds them to @exam[Target]. After the merge @exam[Length(Source)]
is zero. If both lists were sorted before the merge then the result
is also sorted.

@leading@;And finally we have
@begin[Example]
@key[private]
   ... -- @examcom[not specified by the language]
@key[end] Ada.Containers.Doubly_Linked_Lists;
@end[Example]

@leading@;If the reader has got this far they have probably understood how to
use this package so extensive examples are unnecessary. However, as
a taste, here is a simple stack of floating point numbers
@begin[Example]
@tabset[P42]
@key[package] Stack @key[is]
   @key[procedure] Push(X: @key[in] Float);
   @key[function] Pop @key[return] Float;
   @key[function] Size @key[return] Integer;
   @key[exception] Stack_Empty;
@key[end];

@key[with] Ada.Containers.Doubly_Linked_Lists;
@key[use] Ada.Containers;
@key[package body] Stack is

   @key[package] Float_Container @key[is] @key[new] Doubly_Linked_Lists(Float);
   @key[use] Float_Container;
   The_Stack: List;

   @key[procedure] Push(X: @key[in] Float) @key[is]
   @key[begin]
      Append(The_Stack, X);@\-- @examcom[or The_Stack.Append(X);]
   @key[end] Push;

   @key[function] Pop @key[return] Float @key[is]
      Result: Float;
   @key[begin]
      @key[if] Is_Empty(The_Stack) @key[then]
         @key[raise] Stack_Empty;
      @key[end] @key[if];
      Result := Last_Element(The_Stack);
      Delete_Last(The_Stack);
      @key[return] Result;
   @key[end] Pop;

   @key[function] Size @key[return] Integer @key[is]
   @key[begin]
      @key[return] Integer(Length(The_Stack));
   @key[end] Size;

@key[end] Stack;
@end[Example]

This barely needs any explanation. The lists package is instantiated
in the package @exam[Stack] and the object @exam[The_Stack] is of
course the list container. The rest is really straightforward. We
could of course use the prefixed notation throughout as indicated
in @exam[Push].

An important point should be mentioned concerning lists (and containers
in general). This is that attempts to do foolish things typically
result in @exam[Constraint_Error] or @exam[Program_Error] being raised.
This especially applies to the procedures @exam[Process] in
@exam[Query_Element],
@exam[Update_Element], @exam[Iterate] and @exam[Reverse_Iterate].
The concepts of tampering with cursors and elements are introduced
in order to dignify a general motto of "Thou shalt not violate thy
container".

Tampering with cursors occurs when elements are added to or deleted
from a container (by calling @exam[Insert] and so on) whereas tampering
with elements means replacing an element (by calling @exam[Replace_Element]
for example). Tampering with elements is a greater sin and includes
tampering with cursors. The procedure @exam[Process] in @exam[Query_Element]
and @exam[Update_Element] must not tamper with elements and the procedure
@exam[Process] in the other cases must not tamper with cursors. The
reader might think it rather odd that @exam[Update_Element] should
not be allowed to tamper with elements since the whole purpose is
to update the element; this comes back to the point mentioned earlier
that update element gives access to the existing element @i[in situ]
via the parameter of @exam[Process] and that is allowed @en calling
@exam[Replace_Element] within @exam[Process] would be tampering. Tampering
causes @exam[Program_Error] to be raised.

We will now consider the vectors
package.@Defn{vector container}@Defn2{Term=[container],Sec=[vector]} Its
specification starts@Defn2{Term=[package],Sec=[Ada.Containers.Vectors]}@Defn{Ada.Containers.Vectors package}@Defn{Vectors package}
@begin[Example]
@key[generic]
   @key[type] Index_Type @key[is range] <>;
   @key[type] Element_Type @key[is private];
   @key[with] @key[function] "=" (Left, Right: Element_Type) @key[return] Boolean @key[is] <>;
@key[package] Ada.Containers.Vectors @key[is]
   @key[pragma] Preelaborate(Vectors);
@end[Example]

This is similar to the lists package except for the additional generic
parameter @exam[Index_Type] (note that this is an integer type and
not a discrete type). This additional parameter reflects the idea
that a vector is essentially an array and we can index directly into
an array.

@leading@;In fact the vectors package enables us to access elements either by
using an index or by using a cursor. Thus many operations are duplicated
such as
@begin[Example]
@key[function] Element(Container: Vector; Index: Index_Type) @key[return] Element_Type;
@key[function] Element(Position: Cursor) @key[return] Element_Type;

@key[procedure] Replace_Element(
      Container: @key[in out] Vector;
      Index: @key[in] Index_Type;
      New_Item: @key[in] Element_Type);

@key[procedure] Replace_Element(
      Container: @key[in out] Vector;
      Position: @key[in] Cursor;
      New_Item: @key[in] Element_Type);
@end[Example]

If we use an index then there is always a distinct parameter identifying
the vector as well. If we use a cursor then the vector parameter is
omitted if the vector is unchanged as is the case with the function
@exam[Element]. Remember that we stated earlier that a cursor identifies
both an element and the container but if the container is being changed
as in the case of @exam[Replace_Element] then the container has to
be passed as well to ensure write access and to enable a check that
the cursor does identify an element in the correct container.

@leading@;There are also functions @exam[First_Index] and @exam[Last_Index]
thus
@begin[Example]
@key[function] First_Index(Container: Vector) @key[return] Index_Type;

@key[function] Last_Index(Container: Vector) @key[return] Extended_Index;
@end[Example]

These return the values of the index of the first and last elements
respectively. The function @exam[First_Index] always returns
@exam[Index_Type'First] whereas @exam[Last_Index] will return @exam[No_Index]
if the vector is empty. The function @exam[Length] returns
@exam[Last_Index@en@;First_Index+1] which is zero if the vector is empty. Note
that the irritating subtype @exam[Extended_Index] has to be introduced in order
to cope with end values. The constant @exam[No_Index] has the value
@exam[Extended_Index'First] which is equal to @exam[Index_Type'First@en@;1].

@leading@;There are operations to convert between an index and a cursor thus
@begin[Example]
@key[function] To_Cursor(Container: Vector; Index: Extended_Index) @key[return] Cursor;

@key[function] To_Index(Position: Cursor) @key[return] Extended_Index;
@end[Example]

It is perhaps slightly messier to use the index and vector parameters
because of questions concerning the range of values of the index but
probably slightly faster and maybe more familiar. And sometimes of
course using an index is the whole essence of the problem. In the
paper on access types (see @RefSecNum{Downward closures}) we showed a use of
the procedure @exam[Update_Element] to double the values of those elements of a
vector whose index was in the range 5 to 10. This would be tedious with
cursors.

But an advantage of using cursors is that (provided certain operations
are avoided) it is easy to replace the use of vectors by lists.

@leading@;For example here is the stack package rewritten to use vectors
@begin[Example]
@tabset[P56]
@key[with] Ada.Containers.Vectors;@\-- @examcom[changed]
@key[use] Ada.Containers;
@key[package body] Stack is

   @key[package] Float_Container @key[is] @key[new] Vectors(Natural,Float);@\-- @examcom[changed]
   @key[use] Float_Container;
   The_Stack: Vector;@\-- @examcom[changed]

   @key[procedure] Push(X: @key[in] Float) @key[is]
   @key[begin]
      Append(The_Stack, X);
   @key[end] Push;

   -- @examcom[etc exactly as before]

@key[end] Stack;
@end[Example]

So the changes are very few indeed and can be quickly done with a
simple edit.

Note that the index parameter has been given as @exam[Natural] rather
than @exam[Integer]. Using @exam[Integer] will not work since attempting
to elaborate the subtype @exam[Extended_Index] would raise @exam[Constraint_Error]
when evaluating @exam[Integer'First@en@;1]. But in any event it is more
natural for the index range of the container to start at @exam[0]
(or @exam[1]) rather than a large negative value such as @exam[Integer'First].

@leading@;There are other important properties of vectors that should be
mentioned. One is that there is a concept of capacity. Vectors are adjustable
and will extend if necessary when new items are added. However, this
might lead to lots of extensions and copying and so we can set the
capacity of a container by calling
@begin[Example]
@key[procedure] Reserve_Capacity(Container: @key[in out] Vector; Capacity: @key[in] Count_Type);
@end[Example]

@leading@;There is also
@begin[Example]
@key[function] Capacity(Container: Vector) @key[return] Count_Type;
@end[Example]

which naturally returns the current capacity. Note that @exam[Length(V)]
cannot exceed @exam[Capacity(V)] but might be much less.

If we add items to a vector whose length and capacity are the same
then no harm is done. The capacity will be expanded automatically
by effectively calling @exam[Reserve_Capacity] internally. So the
user does not need to set the capacity although not doing so might
result in poorer performance.

@leading@;There is also the concept of "empty elements". These are elements
whose values have not been set. There is no corresponding concept
with lists. It is a bounded error to read an empty element. Empty
elements arise if we declare a vector by calling
@begin[Example]
@key[function] To_Vector(Length: Count_Type) @key[return] Vector;
@end[Example]

@leading@;as in
@begin[Example]
My_Vector: Vector := To_Vector(100);
@end[Example]

@leading@;There is also the much safer
@begin[Example]
@key[function] To_Vector(New_Item: Element_Type; Length: Count_Type) @key[return] Vector;
@end[Example]

which sets all the elements to the value @exam[New_Item].

@leading@;There is also a procedure

@begin[Example]
@key[procedure] Set_Length(Container: @key[in out] Vector; Length: @key[in] Count_Type);
@end[Example]

This changes the length of a vector. This may require elements to
be deleted (from the end) or to be added (in which case the new ones
are empty).

@leading@;The final way to get an empty element is by calling one of
@begin[Example]
@key[procedure] Insert_Space(
      Container: @key[in out] Vector;
      Before: @key[in] Extended_Index;
      Count: @key[in] Count_Type := 1);

@key[procedure] Insert_Space(Container: @key[in out] Vector;
      Before: @key[in] Cursor;
      Position: @key[out] Cursor;
      Count: @key[in] Count_Type := 1);
@end[Example]

These insert the number of empty elements given by @exam[Count] at
the place indicated. Existing elements are slid along as necessary.
These should not be confused with the versions of @exam[Insert] which
do not provide an explicit value for the elements @en in those cases
the new elements take their default values.

Care needs to be taken if we use empty elements. For example we should
not compare two vectors using @exam["="] if they have empty elements
because this implies reading them. But the big advantage of empty
elements is that they provide a quick way to make a large lump of
space in a vector which can then be filled in with appropriate values.
One big slide is a lot faster than lots of little ones.

For completeness, we briefly mention the remaining few subprograms
that are unique to the vectors package.

@leading@;There are further versions of @exam[Insert] thus
@begin[Example]
@key[procedure] Insert(
      Container: @key[in out] Vector;
      Before: @key[in] Extended_Index; New_Item: @key[in] Vector);

@key[procedure] Insert(
      Container: @key[in out] Vector;
      Before: @key[in] Cursor; New_Item: @key[in] Vector);

@key[procedure] Insert(
      Container: @key[in out] Vector;
      Before: @key[in] Cursor; New_Item: @key[in] Vector; Position: @key[out] Cursor);
@end[Example]

These insert copies of a vector into another vector (rather than just
single elements).

@leading@;There are also corresponding versions of @exam[Prepend] and
@exam[Append] thus
@begin[Example]
@key[procedure] Prepend(Container: @key[in out] Vector; New_Item: @key[in] Vector);

@key[procedure] Append(Container: @key[in out] Vector; New_Item: @key[in] Vector);
@end[Example]

@leading@;Finally, there are four functions @exam["&"] which concatenate
vectors and elements by analogy with those for the type @exam[String]. Their
specifications are
@begin[Example]
@key[function] "&" (Left, Right: Vector) @key[return] Vector;
@key[function] "&" (Left: Vector; Right: Element_Type) @key[return] Vector;
@key[function] "&" (Left: Element_Type; Right: Vector) @key[return] Vector;
@key[function] "&" (Left, Right: Element_Type) @key[return] Vector;
@end[Example]

@leading@;Note the similarity between
@begin[Example]
Append(V1, V2);
V1 := V1 & V2;
@end[Example]

The result is the same but using @exam["&"] is less efficient because
of the extra copying involved. But @exam["&"] is a familiar operation
and so is provided for convenience.


@LabeledClause{Maps}


We will now turn to the maps and sets packages. We will start by considering
maps which are more exciting than sets and begin with ordered maps
which are a little simpler and then consider hashed maps.@Defn{map container}@Defn2{Term=[container],Sec=[map]}

@leading@;Remember that a map is just a means of getting from a value of one
type (the key) to another type (the element). This is not a one-one
relationship. Given a key there is a unique element (if any), but
several keys may correspond to the same element. A simple example
is an array. This is a map from the index type to the component type.
Thus if we have
@begin[Example]
S: String := "animal";
@end[Example]

then this provides a map from integers in the range 1 to 6 to some
values of the type @exam[Character]. Given an integer such as @exam[3]
there is a unique character @exam['i'] but given a character such
as @exam['a'] there might be several corresponding integers (in this
case both @exam[1] and @exam[5]).

More interesting examples are where the set of used key values is
quite sparse. For example we might have a store where various spare
parts are held. The parts have a five-digit part number and there
are perhaps twenty racks where they are held identified by a letter.
However, only a handful of the five digit numbers are in use so it
would be very wasteful to use an array with the part number as index.
What we want instead is a container which holds just the pairs that
matter such as (34618, 'F'), (27134, 'C') and so on. We can do this
using a map. We usually refer to the pairs of values as nodes of the
map.

@leading@;There are two maps packages with much in common. One keeps the keys
in order and the other uses a hash function. Here is the specification
of the ordered maps package generally showing just those facilities
common to
both.@Defn2{Term=[package],Sec=[Ada.Containers.Ordered_Maps]}@Defn{Ada.Containers.Ordered_Maps package}@Defn{Ordered_Maps package}
@begin[Example]
@key[generic]
   @key[type] Key_Type @key[is private];
   @key[type] Element_Type @key[is private];
   @key[with function] "<" (Left, Right: Key_Type) @key[return] Boolean @key[is] <>;
   @key[with function] "=" (Left, Right: Element_Type) @key[return] Boolean @key[is] <>;
@key[package] Ada.Containers.Ordered_Maps @key[is]
   @key[pragma] Preelaborate(Ordered_Maps);

   @key[function] Equivalent_Keys(Left: Right: Key_Type) @key[return] Boolean;
@end[Example]

The generic parameters include the ordering relationship @exam["<"]
on the keys and equality for the elements.

It is assumed that the ordering relationship is well behaved in the
sense that if @exam[x < y] is true then @exam[y < x] is false. We
say that two keys @exam[x] and @exam[y] are equivalent if both @exam[x
< y] and @exam[y < x] are false. In other words this defines an equivalence
class on keys. The relationship must also be transitive, that is,
if @exam[x < y] and @exam[y < z] are both true then @exam[x < z] must
also be true.

This concept of an equivalence relationship occurs throughout the
various maps and sets. Sometimes, as here, it is defined in terms
of an order but in other cases, as we shall see, it is defined by
an equivalence function.

It is absolutely vital that the equivalence relations are defined
properly and meet the above requirements. It is not possible for the
container packages to check this and if the operations are wrong then
peculiar behaviour is almost inevitable.

@leading@;For the convenience of the user the function @exam[Equivalent_Keys]
is declared explicitly. It is equivalent to
@begin[Example]
@key[function] Equivalent_Keys(Left, Right: Key_Type) @key[return] Boolean @key[is]
@key[begin]
   @key[return] @key[not] (Left < Right) @key[and not] (Right < Left);
@key[end] Equivalent_Keys;
@end[Example]

The equality operation on elements is not so demanding. It must be
symmetric so that @exam[x = y] and @exam[y = x] are the same but transitivity
is not required (although cases where it would not automatically be
transitive are likely to be rare). The operation is only used for
the function @exam["="] on the containers as a whole.

Note that @exam[Find] and similar operations for maps and sets work
in terms of the equivalence relationship rather than equality as was
the case with lists and vectors.


@begin[Example]
   @key[type] Map @key[is tagged private];
   @key[pragma] Preelaborable_Initialization(Map);
   @key[type] Cursor @key[is private];
   @key[pragma] Preelaborable_Initialization(Cursor);
   Empty_Map: @key[constant] Map;
   No_Element: @key[constant] Cursor;
@end[Example]

The types @exam[Map] and @exam[Cursor] and constants @exam[Empty_Map]
and @exam[No_Element] are similar to the corresponding entities in
the lists and vectors containers.

@begin[Example]
@key[function] "=" (Left, Right: Map) @key[return] Boolean;
@key[function] Length(Container: Map) @key[return] Count_Type;
@key[function] Is_Empty(Container: Map) @key[return] Boolean;
@key[procedure] Clear(Container: @key[in out] Map);
@end[Example]

These are again similar to the corresponding entities for lists. Note
that two maps are said to be equal if they have the same number of
nodes with equivalent keys (as defined by @exam["<"]) whose corresponding
elements are equal (as defined by @exam["="]).

@begin[Example]
@key[function] Key(Position: Cursor) @key[return] Key_Type;
@key[function] Element(Position: Cursor) @key[return] Element_Type;

@key[procedure] Replace_Element(
      Container: @key[in out] Map;
      Position: @key[in] Cursor;
      New_Item: @key[in] Element_Type);

@key[procedure] Query_Element(
      Position: @key[in] Cursor;
      Process: @key[not null access procedure] (Key: @key[in] Key_Type; Element: @key[in] Element_Type));

@key[procedure] Update_Element(
      Container: @key[in out] Map; Position: @key[in] Cursor;
      Process: @key[not null access procedure] (Key: @key[in] Key_Type; Element: @key[in out] Element_Type));
@end[Example]

In this case there is a function @exam[Key] as well as a function
@exam[Element]. But there is no procedure @exam[Replace_Key] since
it would not make sense to change a key without changing the element
as well and this really comes down to deleting the whole node and
then inserting a new one.

The procedures @exam[Query_Element] and @exam[Update_Element] are
slightly different in that the procedure @exam[Process] also takes
the key as parameter as well as the element to be read or updated.
Note again that the key cannot be changed. Nevertheless the value
of the key is given since it might be useful in deciding how the update
should be performed. Remember that we cannot get uniquely from an
element to a key but only from a key to an element.

@begin[Example]
@key[procedure] Move(Target, Source: @key[in out] Map);
@end[Example]

This moves the map from the source to the target after first clearing
the target. It does not make copies of the nodes so that after the
operation the source is empty and @exam[Length(Source)] is zero.

@begin[Example]
@key[procedure] Insert(
      Container: @key[in out] Map;
      Key: @key[in] Key_Type;
      New_Item: @key[in] Element_Type;
      Position: @key[out] Cursor;
      Inserted: @key[out] Boolean);

@key[procedure] Insert(
      Container: @key[in out] Map;
      Key: @key[in] Key_Type;
      Position: @key[out] Cursor;
      Inserted: @key[out] Boolean);

@key[procedure] Insert(
      Container: @key[in out] Map;
      Key: @key[in] Key_Type;
      New_Item: @key[in] Element_Type);
@end[Example]

These insert a new node into the map unless a node with an equivalent
key already exists. If it does exist then the first two return with
@exam[Inserted] set to @exam[False] and @exam[Position] indicating
the node whereas the third raises @exam[Constraint_Error] (the element
value is not changed). If a node with equivalent key is not found
then a new node is created with the given key, the element value is
set to @exam[New_Item] when that is given and otherwise it takes its
default value (if any), and @exam[Position] is set when given.

Unlike vectors and lists, we do not have to say where the new node
is to be inserted because of course this is an ordered map and it
just goes in the correct place according to the order given by the
generic parameter @exam["<"].

@begin[Example]
@key[procedure] Include(
      Container: @key[in out] Map;
      Key: @key[in] Key_Type;
      New_Item: @key[in] Element_Type);
@end[Example]

This is somewhat like the last @exam[Insert] except that if an existing
node with an equivalent key is found then it is replaced (rather than
raising @exam[Constraint_Error]). Note that both the key and the element
are updated. This is because equivalent keys might not be totally
equal.

@leading@;For example the key part might be a record with part number and year
of introduction, thus
@begin[Example]
@key[type] Part_Key @key[is]
   @key[record]
      Part_Number: Integer;
      Year: Integer;
   @key[end record];
@end[Example]

@leading@;and we might define the ordering relationship to be used as the
generic parameter simply in terms of the part number
@begin[Example]
@key[function] "<" (Left, Right: Part_Key) @key[return] Boolean @key[is]
@key[begin]
   @key[return] Left.Part_Number < Right.Part_Number;
@key[end] "<";
@end[Example]

In this situation, the keys could match without the year component
being the same and so it would need to be updated. In other words
with this definition of the ordering relation, two keys are equivalent
provided just the part numbers are the same.

@begin[Example]
@key[procedure] Replace(
      Container: @key[in out] Map;
      Key: @key[in] Key_Type;
      New_Item: @key[in] Element_Type);
@end[Example]

In this case, @exam[Constraint_Error] is raised if the node does not
already exist. On replacement both the key and the element are updated
as for @exam[Include].

Perhaps a better example of equivalent keys not being totally equal
is if the key were a string. We might decide that the case of letter
did not need to match in the test for equivalence but nevertheless
we would probably want to update with the string as used in the parameter
of @exam[Replace].
@begin[Example]
@key[procedure] Exclude(Container: @key[in out] Map; Key: @key[in] Key_Type);
@end[Example]

If there is a node with an equivalent key then it is deleted. If there
is not then nothing happens.

@begin[Example]
@key[procedure] Delete(Container: @key[in out] Map; Key: @key[in] Key_Type);

@key[procedure] Delete(Container: @key[in out] Map; Position: @key[in out] Cursor);
@end[Example]

These delete a node. In the first case if there is no such equivalent
key then @exam[Constraint_Error] is raised (by contrast to @exam[Exclude]
which remains silent in this case). In the second case if the cursor
is @exam[No_Element] then again @exam[Constraint_Error] is raised
@en there is also a check to ensure that the cursor otherwise does
designate a node in the correct map (remember that cursors designate
both an entity and the container); if this check fails then @exam[Program_Error]
is raised.

Perhaps it is worth observing that @exam[Insert], @exam[Include],
@exam[Replace], @exam[Exclude] and @exam[Delete] form a sort of progression
from an operation that will insert something, through operations that
might insert, will neither insert nor delete, might delete, to the
final operation that will delete something. Note also that @exam[Include],
@exam[Replace] and @exam[Exclude] do not apply to lists and vectors.

@begin[Example]
@key[function] First(Container: Map) @key[return] Cursor;
@key[function] Last(Container: Map) @key[return] Cursor;
@key[function] Next(Position: Cursor) @key[return] Cursor;
@key[procedure] Next(Position: @key[in out] Cursor);
@key[function] Find(Container: Map; Key: Key_Type) @key[return] Cursor;
@key[function] Element(Container: Map; Key: Key_Type) @key[return] Element;
@key[function] Contains(Container: Map; Key: Key_Type) @key[return] Boolean;
@end[Example]

These should be self-evident. Unlike the operations on vectors and
lists, @exam[Find] logically searches the whole map and not just starting
at some point (and since it searches the whole map there is no point
in having @exam[Reverse_Find]). (In implementation terms it won't
actually search the whole map because it will be structured in a way
that makes this unnecessary @en as a balanced tree perhaps.) Moreover,
@exam[Find] uses the equivalence relation based on the @exam["<"]
parameter so in the example it only has to match the part number and
not the year. The function call @exam[Element(My_Map, My_Key)] is
equivalent to @exam[Element(Find(My_Map, My_Key))].

@begin[Example]
@key[function] Has_Element(Position: Cursor) @key[return] Boolean;

@key[procedure] Iterate(
      Container: @key[in] Map;
      Process: @key[not null access procedure] (Position: @key[in] Cursor));
@end[Example]

These are also as for other containers.

@leading@;And at last we have
@begin[Example]
@key[private]
   ... -- @examcom[not specified by the language]
@key[end] Ada.Containers.Ordered_Maps;
@end[Example]

We have omitted to mention quite a few operations that have no equivalent
in hashed maps @en we will come back to these in a moment.

@leading@;As an example we can make a container to hold the information
concerning spare parts. We can use the type @exam[Part_Key] and the function
@exam["<"] as above. We can suppose that the element type is
@begin[Example]
@key[type] Stock_Info @key[is]
   @key[record]
      Shelf: Character @key[range] 'A' .. 'T';
      Stock: Integer;
   @key[end record];
@end[Example]

This gives both the shelf letter and the number in stock.

@leading@;We can then declare the container thus
@begin[Example]
@key[package] Store_Maps @key[is]
   @key[new] Ordered_Maps(
       Key_Type => Part_Key,
       Element_Type => Stock_Info,
       "<" => "<");

The_Store: Store_Maps.Map;
@end[Example]

The last parameter could be omitted because the formal has a @exam[<>]
default.

@leading@;We can now add items to our store by calling
@begin[Example]
The_Store.Insert((34618, 1998), ('F', 25));
The_Store.Insert((27134, 2004), ('C', 45));
...
@end[Example]

We might now have a procedure which, given a part number, checks to
see if it exists and that the stock is not zero, and if so returns
the shelf letter and year number and decrements the stock count.

@begin[Example]
@tabset[P42]
@key[procedure] Request(
      Part: @key[in] Integer; OK: @key[out] Boolean;
      Year: @key[out] Integer; Shelf: @key[out] Character) @key[is]
   C: Cursor;
   K: Part_Key;
   E: Stock_Info;
@key[begin]
   C := The_Store.Find((Part, 0));
   @key[if] C = No_Element @key[then]
      OK := False; @key[return];@\-- @examcom[no such key]
   @key[end if];
   E := Element(C);  K := Key(C);
   Year := K.Year;  Shelf := E.Shelf;
   @key[if] E.Stock = 0 @key[then]
      OK := False; @key[return];@\-- @examcom[out of stock]
   @key[end if];
   Replace_Element(C, (Shelf, E.Stock@en@;1));
   OK := True;
@key[end] Request;
@end[Example]

@leading@;Note that we had to put a dummy year number in the call of
@exam[Find]. We could of course use the new @exam[<>] notation for this
@begin[Example]
C := The_Store.Find((Part, @key[others] => <>));
@end[Example]

The reader can improve this example at leisure @en by using
@exam[Update_Element] for example.

@leading@;As another example suppose we wish to check all through the stock
looking for parts whose stock is low, perhaps less than some given
parameter. We can use @exam[Iterate] for this as follows
@begin[Example]
@key[procedure] Check_Stock(Low: @key[in] Integer) @key[is]

   @key[procedure] Check_It(C: @key[in] Cursor) @key[is]
   @key[begin]
      @key[if] Element(C).Stock < Low @key[then]
         -- @examcom[print a message perhaps]
         Put("Low stock of part ");
         Put_Line(Key(C).Part_Number);
      @key[end if];
   @key[end] Check_It;

@key[begin]
   The_Store.Iterate(Check_It'Access);
@key[end] Check_Stock;
@end[Example]

Note that this uses a so-called downward closure. The procedure @exam[Check_It]
has to be declared locally to @exam[Check_Stock] in order to access
the parameter @exam[Low]. (Well you could declare it outside and copy
the parameter @exam[Low] to a global variable but that is just the
sort of wicked thing one has to do in lesser languages (such as even
Ada 95). It is not task safe for one thing.)

@leading@;Another approach is to use @exam[First] and @exam[Next] and so
on thus
@begin[Example]
@key[procedure] Check_Stock(Low: @key[in] Integer) @key[is]
   C: Cursor := The_Store.First;
@key[begin]
   @key[loop]
      @key[exit when] C = No_Element;
      @key[if] Element(C).Stock < Low @key[then]
         -- @examcom[print a message perhaps]
         Put("Low stock of part ");
         Put_Line(Key(C).Part_Number);
      @key[end if];
      C := The_Store.Next(C);
   @key[end loop];
@key[end] Check_Stock;
@end[Example]

We will now consider hashed maps. The trouble with ordered maps in
general is that searching can be slow when the map has many entries.
Techniques such as a binary tree can be used but even so the search
time will increase at least as the logarithm of the number of entries.
A better approach is to use a hash function. This will be familiar
to many readers (especially those who have written compilers). The
general idea is as follows.

We define a function which takes a key and returns some value in a
given range. In the case of the Ada containers it has to return a
value of the modular type @exam[Hash_Type] which is declared in the
root package @exam[Ada.Containers]. We could then convert this value
onto a range representing an index into an array whose size corresponds
to the capacity of the map. This index value is the preferred place
to store the entry. If there already is an entry at this place (because
some other key has hashed to the same value) then a number of approaches
are possible. One way is to create a list of entries with the same
index value (often called buckets); another way is simply to put it
in the next available slot. The details don't matter. But the overall
effect is that provided the map is not too full and the hash function
is good then we can find an entry almost immediately more or less
irrespective of the size of the map.

So as users all we have to do is to define a suitable hash function.
It should give a good spread of values across the range of @exam[Hash_Type]
for the population of keys, it should avoid clustering and above all
for a given key it must @i[always] return the same hash value. A good
discussion on hash functions by Knuth will be found in
@LocalLink{Target=[R10],Sec=[References],Text={[10]}}.

@leading@;Defining good hash functions needs care. In the case of the
part numbers we might multiply the part number by some obscure prime
number and
then truncate the result down to the modular type @exam[Hash_Type].
The author hesitates to give an example but perhaps
@begin[Example]
@tabset[P42]
@key[function] Part_Hash(P: Part_Key) @key[return] Hash_Type @key[is]
   M31: @key[constant] := 2**31@en@;1;@\-- @examcom[a nice Mersenne prime]
@key[begin]
   @key[return] Hash_Type(P.Part_Number) * M31;
@key[end] Part_Hash;
@end[Example]

On reflection that's probably a very bad prime to use because it is
so close to half of @exam[2**32] a typical value of @exam[Hash_Type'Last+1].
Of course it doesn't have to be prime but simply relatively prime
to it such as @exam[5**13]. Knuth suggests dividing the range by the
golden number @unicode(964) = (@unicode{8730}5+1)/2 = 1.618... and then taking
the nearest number relatively prime which is in fact simply the nearest
odd number (in this case it is 2654435769).

Here is a historic interlude. Marin Mersenne (1588-1648) was a Franciscan
monk who lived in Paris. He studied numbers of the form @i[M]@i{@-{p}} =
2@i{@+{p}} @en 1 where @i[p] is prime. A lot of these are
themselves prime. Mersenne gave a list of those up to 257 which he
said were prime (namely 2, 3, 5, 7, 13, 17, 19, 31, 67, 127, 257).
It was not until 1947 that it was finally settled that he got some
of them wrong (61, 89, and 107 are also prime but 67 and 257 are not).
At the time of writing there are 42 known Mersenne primes and the
largest which is also the largest known prime number is @i[M]@-{25964951}
@en see @URLLink{URL=[http://www.mersenne.org],Text=[www.mersenne.org]}.

The specification of the hashed maps package is very similar to that
for ordered maps. It
starts@Defn2{Term=[package],Sec=[Ada.Containers.Hashed_Maps]}@Defn{Ada.Containers.Hashed_Maps package}@Defn{Hashed_Maps package}

@begin[Example]
@key[generic]
   @key[type] Key_Type @key[is private];
   @key[type] Element_Type @key[is private];
   @key[with function] Hash(Key: Key_Type) @key[return] Hash_Type;
   @key[with function] Equivalent_Keys(Left, Right: Key_Type) @key[return] Boolean;
   @key[with function] "=" (Left, Right: Element_Type) @key[return] Boolean @key[is] <>;
@key[package] Ada.Containers.Hashed_Maps @key[is]
   @key[pragma] Preelaborate(Hashed_Maps);
@end[Example]

The differences from the ordered maps package are that there is an
extra generic parameter @exam[Hash] giving the hash function and the
ordering parameter @exam["<"] has been replaced by the function @exam[Equivalent_Keys].
It is this function that defines the equivalence relationship for
hashed maps; it is important that @exam[Equivalent_Keys(X, Y)] is
always the same as @exam[Equivalent_Keys(Y, X)]. Moreover if @exam[X]
and @exam[Y] are equivalent and @exam[Y] and @exam[Z] are equivalent
then @exam[X] and @exam[Z] must also be equivalent.

Note that the function @exam[Equivalent_Keys] in the ordered maps
package discussed above corresponds to the formal generic parameter
of the same name in this hashed maps package. This should make it
easier to convert between the two forms of packages.

@leading@;Returning to our example, if we now write
@begin[Example]
@key[function] Equivalent_Parts(Left, Right: Part_Key) @key[return] Boolean @key[is]
@key[begin]
   @key[return] Left.Part_Number = Right.Part_Number;
@key[end] Equivalent_Parts;
@end[Example]

@leading@;then we can instantiate the hashed maps package as follows
@begin[Example]
@key[package] Store_Maps @key[is new] Hashed_Maps(
      Key_Type => Part_Key,
      Element_Type => Stock_Info,
      Hash => Part_Hash,
      Equivalent_Keys => Equivalent_Parts);

The_Store: Store_Maps.Map;
@end[Example]

and then the rest of our example will be exactly as before. It is
thus easy to convert from an ordered map to a hashed map and vice
versa provided of course that we only use the facilities common to
both.

We will finish this discussion of maps by briefly considering the
additional facilities in the two packages.

@leading@;The ordered maps package has the following additional subprograms
@begin[Example]
@key[procedure] Delete_First(Container: @key[in out] Map);
@key[procedure] Delete_Last(Container: @key[in out] Map);

@key[function] First_Element(Container: Map) @key[return] Element_Type;
@key[function] First_Key(Container: Map) @key[return] Key_Type;
@key[function] Last_Element(Container: Map) @key[return] Element_Type;
@key[function] Last_Key(Container: Map) @key[return] Key_Type;
@key[function] Previous(Position: Cursor) @key[return] Cursor;
@key[procedure] Previous(Position: @key[in out] Cursor);

@key[function] Floor(Container: Map; Key: Key_Type) @key[return] Cursor;
@key[function] Ceiling(Container: Map; Key: Key_Type) @key[return] Cursor;

@key[function] "<" (Left, Right: Cursor) @key[return] Boolean;
@key[function] ">" (Left, Right: Cursor) @key[return] Boolean;
@key[function] "<" (Left: Cursor; Right: Key_Type) @key[return] Boolean;
@key[function] ">" (Left: Cursor; Right: Key_Type) @key[return] Boolean;
@key[function] "<" (Left: Key_Type; Right: Cursor) @key[return] Boolean;
@key[function] ">" (Left: Key_Type; Right: Cursor) @key[return] Boolean;

@key[procedure] Reverse_Iterate(
      Container: @key[in] Map;
      Process: @key[not null access procedure] (Position: @key[in] Cursor));
@end[Example]

These are again largely self-evident. The functions @exam[Floor] and
@exam[Ceiling] are interesting. @exam[Floor] searches for the last
node whose key is not greater than @exam[Key] and similarly @exam[Ceiling]
searches for the first node whose key is not less than @exam[Key]
@en they return @exam[No_Element] if there is no such element. The
subprograms @exam[Previous] are of course the opposite of @exam[Next]
and @exam[Reverse_Iterate] is like @exam[Iterate] only backwards.

@leading@;The functions @exam["<"] and @exam[">"] are mostly for convenience.
Thus the first is equivalent to
@begin[Example]
@key[function] "<" (Left, Right: Cursor) @key[return] Boolean @key[is]
@key[begin]
   @key[return] Key(Left) < Key(Right);
@key[end] "<";
@end[Example]

Clearly these additional operations must be avoided if we wish to
retain the option of converting to a hashed map later.

@leading@;Hashed maps have a very important facility not in ordered maps which
is the ability to specify a capacity as for the vectors package. (Underneath
their skin the hashed maps are a bit like vectors whereas the ordered
maps are a bit like lists.) Thus we have
@begin[Example]
@key[procedure] Reserve_Capacity(Container: @key[in out] Map; Capacity: @key[in] Count_Type);

@key[function] Capacity(Container: Map) @key[return] Count_Type;
@end[Example]

The behaviour is much as for vectors. We don't have to set the capacity
ourselves since it will be automatically extended as necessary but
it might significantly improve performance to do so. In the case of
maps, increasing the capacity requires the hashing to be redone which
could be quite time consuming, so if we know that our map is going
to be a big one, it is a good idea to set an appropriate capacity
right from the beginning. Note again that @exam[Length(M)] cannot
exceed @exam[Capacity(M)] but might be much less.

@leading@;The other additional subprograms for hashed maps are
@begin[Example]
@key[function] Equivalent_Keys(Left, Right: Cursor) @key[return] Boolean;
@key[function] Equivalent_Keys(Left: Cursor; Right: Key_Type) @key[return] Boolean;
@key[function] Equivalent_Keys(Left: Key_Type; Right: Cursor) @key[return] Boolean;
@end[Example]

@leading@;These (like the additional @exam["<"] and @exam[">"] for ordered maps)
are again mostly for convenience. The first is equivalent to
@begin[Example]
@key[function] Equivalent_Keys(Left, Right: Cursor) @key[return] Boolean @key[is]
@key[begin]
   @key[return] Equivalent_Keys(Key(Left), Key(Right));
@key[end] Equivalent_Keys;
@end[Example]

@leading@;Before moving on to sets it should be noticed that there are also
some useful functions in the string packages. The main one
is@Defn2{Term=[function],Sec=[Ada.Strings.Hash]}@Defn{Ada.Strings.Hash function}@Defn2{Term=[hash function],Sec=[for strings]}
@begin[Example]
@key[with] Ada.Containers;
@key[function] Ada.Strings.Hash(Key: String) @key[return] Containers.Hash_Type;
@key[pragma] Pure(Ada.Strings.Hash);
@end[Example]

There is a similar function @exam[Ada.Strings.Unbounded.Hash] where
the parameter @exam[Key] has type @exam[Unbounded_String]. It simply
converts the parameter to the type @exam[String] and then calls @exam[Ada.Strings.Hash].
There is also a generic function for bounded strings which again calls
the basic function @exam[Ada.Strings.Hash]. For completeness the function
@exam[Ada.Strings.Fixed.Hash] is a renaming of @exam[Ada.Strings.Hash].

These are provided because it is often the case that the key is a
string and they save the user from devising good hash functions for
strings which might cause a nasty headache.

@leading@;We could for example save ourselves the worry of defining a good hash
function in the above example by making the part number into a 5-character
string. So we might write
@begin[Example]
@key[function] Part_Hash(P: Part_Key) @key[return] Hash_Type @key[is]
@key[begin]
   @key[return] Ada.Strings.Hash(P.Part_Number);
@key[end] Part_Hash;
@end[Example]

and if this doesn't work well then we can blame the vendor.


@LabeledClause{Sets}


Sets, like maps, come in two forms: hashed and ordered. Sets are of
course just collections of values and there is no question of a key
(we can perhaps think of the value as being its own key). Thus in
the case of an ordered set the values are stored in order whereas
in the case of a map, it is the keys that are stored in order. As
well as the usual operations of inserting elements into a set and
searching and so on, there are also many operations on sets as a whole
that do not apply to the other containers @en these are the familiar
set operations such as union and intersection.@Defn{set container}@Defn2{Term=[container],Sec=[set]}

Here is the specification of the ordered sets package giving just
those facilities that are common to both kinds of
sets.@Defn2{Term=[package],Sec=[Ada.Containers.Ordered_Sets]}@Defn{Ada.Containers.Ordered_Sets package}@Defn{Ordered_Sets package}
@begin[Example]
@key[generic]
   @key[type] Element_Type @key[is private];
   @key[with function] "<" (Left, Right: Element_Type) @key[return] Boolean @key[is] <>;
   @key[with function] "=" (Left, Right: Element_Type) @key[return] Boolean @key[is] <>;
@key[package] Ada.Containers.Ordered_Sets @key[is]
   @key[pragma] Preelaborate(Ordered_Sets);

   @key[function] Equivalent_Elements(Left, Right: Element_Type) @key[return] Boolean;

   @key[type] Set @key[is tagged private];
   @key[pragma] Preelaborable_Initialization(Set);
   @key[type] Cursor @key[is private];
   @key[pragma] Preelaborable_Initialization(Cursor);
   Empty_Set: @key[constant] Set;
   No_Element: @key[constant] Cursor;
@end[Example]

The only differences from the maps package (apart from the identifiers)
are that there is no key type and both @exam["<"] and @exam["="] apply
to the element type (whereas in the case of maps, the operation @exam["<"]
applies to the key type). Thus the ordering relationship @exam["<"]
defined on elements defines equivalence between the elements whereas
@exam["="] defines equality.

It is possible for two elements to be equivalent but not equal. For
example if they were strings then we might decide that the ordering
(and thus equivalence) ignored the case of letters but that equality
should take the case into account. (They could also be equal but not
equivalent but that is perhaps less likely.)

And as in the case of the maps package, the equality operation on
elements is only used by the function @exam["="] for comparing two sets.

Again we have the usual rules as explained for maps. Thus if @exam[x
< y] is true then @exam[y < x] must be false; @exam[x < y] and @exam[y
< z] must imply @exam[x < z]; and @exam[x = y] and @exam[y = x] must
be the same.

For the convenience of the user the function @exam[Equivalent_Elements]
is declared explicitly. It is equivalent to
@begin[Example]
@key[function] Equivalent_Elements(Left, Right: Element_Type) @key[return] Boolean @key[is]
@key[begin]
   @key[return] @key[not] (Left < Right) @key[and not] (Right < Left);
@key[end] Equivalent_Elements;
@end[Example]

This function @exam[Equivalent_Elements] corresponds to the formal
generic parameter of the same name in the hashed sets package discussed
below. This should make it easier to convert between the two forms
of packages.

@begin[Example]
@key[function] "=" (Left, Right: Set) @key[return] Boolean;
@key[function] Equivalent_Sets(Left, Right: Set) @key[return] Boolean;
@key[function] To_Set(New_Item: Element_Type) @key[return] Set;
@key[function] Length(Container: Set) @key[return] Count_Type;
@key[function] Is_Empty(Container: Set) @key[return] Boolean;
@key[procedure] Clear(Container: @key[in out] Set);
@end[Example]

Note the addition of @exam[Equivalent_Sets] and @exam[To_Set]. Two
sets are equivalent if they have the same number of elements and the
pairs of elements are equivalent. This contrasts with the function
@exam["="] where the pairs of elements have to be equal rather than
equivalent. Remember that elements might be equivalent but not equal
(as in the example of a string mentioned above). The function @exam[To_Set]
takes a single element and creates a set. It is particularly convenient
when used in conjunction with operations such as @exam[Union] described
below. The other subprograms are as in the other containers.

@begin[Example]
@key[function] Element(Position: Cursor) @key[return] Element_Type;

@key[procedure] Replace_Element(
      Container: @key[in out] Set;
      Position: @key[in] Cursor;
      New_Item: @key[in] Element_Type);

@key[procedure] Query_Element(
      Position: @key[in] Cursor;
      Process: @key[not null access procedure] (Element: @key[in] Element_Type));
@end[Example]

Again these are much as expected except that there is no procedure
@exam[Update_Element]. This is because the elements are arranged in
terms of their own value (either by order or through the hash function)
and if we just change an element @i[in situ] then it might become
out of place (this problem does not arise with the other containers).
This also means that @exam[Replace_Element] has to ensure that the
value @exam[New_Item] is not equivalent to an element in a different
position; if it is then @exam[Program_Error] is raised. We will return
to the problem of the missing @exam[Update_Element] later.

@begin[Example]
@key[procedure] Move(Target, Source: @key[in out] Set);
@end[Example]

This is just as for the other containers.

@begin[Example]
@key[procedure] Insert(
      Container: @key[in out] Set;
      New_Item: @key[in] Element_Type;
      Position: @key[out] Cursor;
      Inserted: @key[out] Boolean);

@key[procedure] Insert(
      Container: @key[in out] Set;
      New_Item: @key[in] Element_Type);
@end[Example]

These insert a new element into the set unless an equivalent element
already exists. If it does exist then the first one returns with @exam[Inserted]
set to @exam[False] and @exam[Position] indicating the element whereas
the second raises @exam[Constraint_Error] (the element value is not
changed). If an equivalent element is not in the set then it is added
and @exam[Position] is set accordingly.

@begin[Example]
@key[procedure] Include(Container: @key[in out] Set; New_Item: @key[in] Element_Type);
@end[Example]

This is somewhat like the last @exam[Insert] except that if an equivalent
element is already in the set then it is replaced (rather than raising
@exam[Constraint_Error]).

@begin[Example]
@key[procedure] Replace(Container: @key[in out] Set; New_Item: @key[in] Element_Type);
@end[Example]

In this case, @exam[Constraint_Error] is raised if an equivalent element
does not already exist.

@begin[Example]
@key[procedure] Exclude(Container: @key[in out] Set; Item: @key[in] Element_Type);
@end[Example]

If an element equivalent to @exam[Item] is already in the set, then
it is deleted.

@begin[Example]
@key[procedure] Delete(Container: @key[in out] Set; Item: @key[in] Element_Type);

@key[procedure] Delete(Container: @key[in out] Set; Position: @key[in out] Cursor);
@end[Example]

These delete an element. In the first case if there is no such equivalent
element then @exam[Constraint_Error] is raised. In the second case
if the cursor is @exam[No_Element] then again @exam[Constraint_Error]
is also raised @en there is also a check to ensure that the cursor
otherwise does designate an element in the correct set (remember that
cursors designate both an entity and the container); if this check
fails then @exam[Program_Error] is raised.

And now some new stuff, the usual set operations.

@begin[Example]
@key[procedure] Union(Target: @key[in out] Set; Source: @key[in] Set);
@key[function] Union(Left, Right: Set) @key[return] Set;
@key[function] "or" (Left, Right: Set) @key[return] Set @key[renames] Union;

@key[procedure] Intersection(Target: @key[in out] Set; Source: @key[in] Set);
@key[function] Intersection(Left, Right: Set) @key[return] Set;
@key[function] "@key[and]" (Left, Right: Set) @key[return] Set @key[renames] Intersection;

@key[procedure] Difference(Target: @key[in out] Set; Source: @key[in] Set);
@key[function] Difference(Left, Right: Set) @key[return] Set;
@key[function] "@en" (Left, Right: Set) @key[return] Set @key[renames] Difference;

@key[procedure] Symmetric_Difference(Target: @key[in out] Set; Source: @key[in] Set);
@key[function] Symmetric_Difference (Left, Right: Set) @key[return] Set;
@key[function] "@key[xor]" (Left, Right: Set) @key[return] Set @key[renames] Symmetric_Difference;
@end[Example]

These all do exactly what one would expect using the equivalence relation
on the elements.

@begin[Example]
@key[function] Overlap(Left, Right: Set) @key[return] Boolean;
@key[function] Is_Subset(Subset: Set; Of_Set: Set) @key[return] Boolean;
@end[Example]

These are self-evident as well.

@begin[Example]
@key[function] First(Container: Set) @key[return] Cursor;
@key[function] Last(Container: Set) @key[return] Cursor;
@key[function] Next(Position: Cursor) @key[return] Cursor;
@key[procedure] Next(Position: @key[in out] Cursor);
@key[function] Find(Container: Set; Item: Element_Type) @key[return] Cursor;
@key[function] Contains(Container: Set; Item: Element_Type) @key[return] Boolean;
@end[Example]

These should be self-evident and are very similar to the corresponding
operations on maps. Again unlike the operations on vectors and lists,
@exam[Find] logically searches the whole set and not just starting
at some point (there is also no @exam[Reverse_Find]). Moreover, @exam[Find]
uses the equivalence relation based on the @exam["<"] parameter.

@begin[Example]
@key[function] Has_Element(Position: Cursor) @key[return] Boolean;

@key[procedure] Iterate(
      Container: @key[in] Set;
      Process: @key[not null access procedure] (Position: @key[in] Cursor));
@end[Example]

These are also as for other containers.

The sets packages conclude with an internal generic package called
@exam[Generic_Keys]. This package enables some set operations to be
performed in terms of keys where the key is a function of the element.
Note carefully that in the case of a map, the element is defined in
terms of the key whereas here the situation is reversed. An equivalence
relationship is defined for these keys as well; this is defined by
a generic parameter @exam["<"] for ordered sets and @exam[Equivalent_Keys]
for hashed sets.

@leading@;In the case of ordered sets the formal parameters are
@begin[Example]
@key[generic]
   @key[type] Key_Type(<>) @key[is private];
   @key[with function] Key(Element: Element_Type) @key[return] Key_Type;
   @key[with function] "<" (Left, Right: Key_Type) @key[return] Boolean @key[is] <>;
@key[package] Generic_Keys @key[is]
@end[Example]

The following are then common to the package @exam[Generic_Keys] for
both hashed and ordered sets.

@begin[Example]
@key[function] Key(Position: Cursor) @key[return] Key_Type;
@key[function] Element(Container: Set; Key: Key_Type) @key[return] Element_Type;

@key[procedure] Replace(
      Container: @key[in out] Set;
      Key: @key[in] Key_Type; New_Item: @key[in] Element_Type);

@key[procedure] Exclude(
      Container: @key[in out] Set; Key: @key[in] Key_Type);
@key[procedure] Delete(Container: @key[in out] Set; Key: @key[in] Key_Type);

@key[function] Find(Container: Set; Key: Key_Type) @key[return] Cursor;
@key[function] Contains(Container: Set; Key: Key_Type) @key[return] Boolean;

@key[procedure] Update_Element_Preserving_Key(
      Container: @key[in out] Set; Position: @key[in] Cursor;
      Process: @key[not null access procedure] (Element: @key[in out] Element_Type));
@end[Example]

@leading@;and then finally

@begin[Example]
@key[end] Generic_Keys;

@key[private]
   ... -- @examcom[not specified by the language]
@key[end] Ada.Containers.Ordered_Sets;
@end[Example]

It is expected that most user of sets will use them in a straightforward
manner and that the operations specific to sets such as @exam[Union]
and @exam[Intersection] will be dominant.

@leading@;However, sets can be used as sort of economy class maps by using the
inner package @exam[Generic_Keys]. Although this is certainly not
for the novice we will illustrate how this might be done by reconsidering
the stock problem using sets rather than maps. We declare
@begin[Example]
@key[type] Part_Type @key[is]
   @key[record]
      Part_Number: Integer;
      Year: Integer;
      Shelf: Character @key[range] 'A' .. 'T';
      Stock: Integer;
   @key[end record];
@end[Example]

Here we have put all the information in the one type.

@leading@;We then declare @exam["<"] much as before
@begin[Example]
@key[function] "<" (Left, Right: Part_Type) @key[return] Boolean @key[is]
@key[begin]
   @key[return] Left.Part_Number < Right.Part_Number;
@key[end] "<";
@end[Example]

@leading@;and then instantiate the package thus
@begin[Example]
@key[package] Store_Sets @key[is] @key[new] Ordered_Sets(Element_Type => Part_Type);

The_Store: Store_Sets.Set;
@end[Example]

We have used the default generic parameter mechanism for @exam["<"]
this time by way of illustration.

@leading@;In this case we add items to the store by calling
@begin[Example]
The_Store.Insert((34618, 1998, 'F', 25));
The_Store.Insert((27134, 2004, 'C', 45));
...
@end[Example]

@leading@;The procedure for checking the stock could now become
@begin[Example]
@tabset[P42]
@key[procedure] Request(
       Part: @key[in] Integer: OK: @key[out] Boolean;
       Year: @key[out] Integer; Shelf: @key[out] Character) @key[is]
   C: Cursor;
   E: Part_Type;
@key[begin]
   C := The_Store.Find((Part, @key[others] => <>));
   @key[if] C = No_Element @key[then]
      OK := False; @key[return];@\-- @examcom[no such item]
   @key[end if];
   E := Element(C);
   Year := E.Year;
   Shelf := E.Shelf;
   @key[if] E.Stock = 0 @key[then]
      OK := False; @key[return];@\-- @examcom[out of stock]
   @key[end if];
   Replace_Element(C, (E.Part_Number, Year; Shelf, E.Stock@en@;1));
   OK := True;
@key[end] Request;
@end[Example]

This works but is somewhat unsatisfactory. For one thing we have had
to make up dummy components in the call of @exam[Find] (using @exam[<>])
and moreover we have had to replace the whole of the element although
we only wanted to update the @exam[Stock] component. Moreover, we
cannot use @exam[Update_Element] because it is not defined for sets
at all. Remember that this is because it might make things out of
order; that wouldn't be a problem in this case because we don't want
to change the part number and our ordering is just by the part number.

@leading@;A better approach is to use the part number as a key. We define
@begin[Example]
@key[type] Part_Key @key[is] @key[new] Integer;

@key[function] Part_No(P: Part_Type) @key[return] Part_Key @key[is]
@key[begin]
   @key[return] Part_Key(P.Part_Number);
@key[end] Part_No;
@end[Example]

@leading@keepnext@;and then

@begin[Example]
@key[package] Party @key[is] @key[new] Generic_Keys(Key_Type => Part_Key, Key => Part_No);
@key[use] Party;
@end[Example]

Note that we do not have to define @exam["<"] on the type @exam[Part_Key]
at all because it already exists since @exam[Part_Key] is an integer
type. And the instantiation uses it by default.

@leading@;And now we can rewrite the @exam[Request] procedure as follows

@begin[Example]
@tabset[P42]
@key[procedure] Request(
      Part: @key[in] Part_Key; OK: @key[out] Boolean;
      Year: @key[out] Integer; Shelf: @key[out] Character) @key[is]
   C: Cursor;
   E: Part_Type;
@key[begin]
   C := Find(The_Store, Part);
   @key[if] C = No_Element @key[then]
      OK := False; @key[return];@\-- @examcom[no such item]
   @key[end if];
   E := Element(C);
   Year := E.Year;  Shelf := E.Shelf;
   @key[if] E.Stock = 0 @key[then]
      OK := False; @key[return];@\-- @examcom[out of stock]
   @key[end if];

   -- @examcom[we are now going to update the stock level]
   @key[declare]
      @key[procedure] Do_It(E: @key[in out] Part_Type) @key[is]
      @key[begin]
         E.Stock := E.Stock @en 1;
      @key[end] Do_It;
   @key[begin]
      Update_Element_Preserving_Key(The_Store, C, Do_It'Access);
   @key[end];
   OK := True;
@key[end] Request;
@end[Example]

This seems hard work but has a number of advantages. The first is
that the call of @exam[Find] is more natural and only involves the
part number (the key) @en note that this is a call of the function
@exam[Find] in the instantiation of @exam[Generic_Keys] and takes
just the part number. And the other is that the update only involves
the component being changed. We mentioned earlier that there was no
@exam[Update_Element] for sets because of the danger of creating a
value that was in the wrong place. In the case of the richly named
@exam[Update_Element_Preserving_Key] it also checks to ensure that
the element is indeed still in the correct place (by checking that
the key is still the same); if it isn't it removes the element and
raises @exam[Program_Error].

But the user is warned to take care when using the package @exam[Generic_Keys].
It is absolutely vital that the relational operation and the function
(@exam[Part_No]) used to instantiate @exam[Generic_Keys] are compatible
with the ordering used to instantiate the parent package
@exam[Containers.Ordered_Sets] itself. If this is not the case then the sky
might fall in.

@leading@;Incidentally, the procedure for checking the stock which previously
used the maps package now becomes
@begin[Example]
@tabset[P42]
@key[procedure] Check_Stock(Low: @key[in] Integer) @key[is]

   @key[procedure] Check_It(C: @key[in] Cursor) @key[is]
   @key[begin]
      @key[if] Element(C).Stock < Low @key[then]
         -- @examcom[print a message perhaps]
         Put("Low stock of part ");
         Put_Line(Element(C).Part_Number);@\-- @examcom[changed]
      @key[end if];
   @key[end] Check_It;

@key[begin]
   The_Store.Iterate(Check_It'Access);
@key[end] Check_Stock;
@end[Example]

@leading@;The only change is that the call of @exam[Key] in
@begin[Example]
        Put_Line(Key(C).Part_Number);
@end[Example]

@leading@;when using the maps package has been replaced by @exam[Element]. A
minor point is that we could avoid calling @exam[Element] twice by
declaring a constant @exam[E] in @exam[Check_It] thus
@begin[Example]
E: @key[constant] Part_Type := Element(C);
@end[Example]

and then writing @exam[E.Stock < Low] and calling @exam[Put_Line]
with @exam[E.Part_Number].

A more important point is that if we have instantiated the @exam[Generic_Keys]
inner package as illustrated above then we can leave @exam[Check_It]
unchanged to call @exam[Key]. But it is important to realise that
we are then calling the function @exam[Key] internal to the instantiation
of @exam[Generic_Keys] (flippantly called @exam[Party]) and not that
from the instantiation of the parent ordered sets package (@exam[Store_Sets])
because that has no such function. This illustrates the close affinity
between the sets and maps packages.

And finally there is a hashed sets package which has strong similarities
to both the ordered sets package and the hashed maps package. We can
introduce this much as for hashed maps by giving the differences between
the two sets packages, the extra facilities in each and the impact
on the part number example.

The specification of the hashed sets package
starts@Defn2{Term=[package],Sec=[Ada.Containers.Hashed_Sets]}@Defn{Ada.Containers.Hashed_Sets package}@Defn{Hashed_Sets package}

@begin[Example]
@key[generic]
   @key[type] Element_Type @key[is private];
   @key[with function] Hash(Element: Element_Type) @key[return] Hash_Type;
   @key[with function] Equivalent_Elements(Left, Right: Element_Type)
@key[return] Boolean;
   @key[with function] "=" (Left, Right: Element_Type) @key[return]
Boolean @key[is] <>;
@key[package] Ada.Containers.Hashed_Sets @key[is]
   @key[pragma] Preelaborate(Hashed_Sets);
@end[Example]

The differences from the ordered sets package are that there is an
extra generic parameter @exam[Hash] and the ordering parameter @exam["<"]
has been replaced by the function @exam[Equivalent_Elements].

@leading@;So if we have
@begin[Example]
@tabset[P42]
@key[function] Equivalent_Parts(Left, Right: Part_Type) @key[return] Boolean @key[is]
@key[begin]
   @key[return] Left.Part_Number = Right.Part_Number;
@key[end] Equivalent_Parts;

@key[function] Part_Hash(P: Part_Type) @key[return] Hash_Type @key[is]
   M31: @key[constant] := 2**31@en@;1;@\-- @examcom[a nice Mersenne prime]
@key[begin]
   @key[return] Hash_Type(P.Part_Number) * M31;
@key[end] Part_Hash;
@end[Example]

@leading@;(which are very similar to the hashed map example @en the only changes
are to the parameter type name) then we can instantiate the hashed
sets package as follows
@begin[Example]
@key[package] Store_Sets @key[is new] Hashed_Sets(
      Element_Type => Part_Type,
      Hash => Part_Hash,
      Equivalent_Elements => Equivalent_Parts);

The_Store: Store_Sets.Set;
@end[Example]

and then the rest of our example will be exactly as before. It is
thus easy to convert from an ordered set to a hashed set and vice
versa provided of course that we only use the facilities common to
both.

@leading@;It should also be mentioned that the inner package
@exam[Generic_Keys] for hashed sets has the following formal parameters
@begin[Example]
@key[generic]
   @key[type] Key_Type(<>) @key[is private];
   @key[with function] Key(Element: Element_Type) @key[return] Key_Type
   @key[with function] Hash(Key: Key_Type) @key[return] Hash_Type;
   @key[with function] Equivalent_Keys(Left, Right: Key_Type) @key[return] Boolean;
@key[package] Generic_Keys @key[is]
@end[Example]

The differences from that for ordered sets are the addition of the
function @exam[Hash] and the replacement of the comparison operator
@exam["<"] by @exam[Equivalent_Keys].

(Incidentally the package @exam[Generic_Keys] for ordered sets also
exports a function @exam[Equivalent_Keys] for uniformity with the
hashed sets package.)

@leading@;Although our example itself is unchanged we do have to change the
instantiation of @exam[Generic_Keys] thus
@begin[Example]
@tabset[P42]
@key[type] Part_Key @key[is] @key[new] Integer;

@key[function] Part_No(P: Part_Type) @key[return] Part_Key @key[is]
@key[begin]
   @key[return] Part_Key(P.Part_Number);
@key[end] Part_No;

@key[function] Part_Hash(P: Part_Key) @key[return] Hash_Type @key[is]
   M31: @key[constant] := 2**31@en@;1;@\-- @examcom[a nice Mersenne prime]
@key[begin]
   @key[return] Hash_Type(P) * M31;
@key[end] Part_Hash;

@key[function] Equivalent_Parts(Left: Right: Part_Key) @key[return] Boolean @key[is]
@key[begin]
   @key[return] Left = Right;
@key[end] Equivalent_Parts;
@end[Example]

@leading@keepnext@;and then

@begin[Example]
@key[package] Party @key[is new] Generic_Key(
      Key_Type => Part_Key,
      Key => Part_No;
      Hash => Part_Hash
      Equivalent_Keys => Equivalent_Parts);
@key[use] Party;
@end[Example]

The hash function is similar to that used with hashed maps. The type
@exam[Part_Key] and function @exam[Part_No] are the same as for ordered
sets. We don't really need to declare the function @exam[Equivalent_Parts]
since we could use @exam["="] as the actual parameter for @exam[Equivalent_Keys].

We will finish this discussion of sets by briefly considering the
additional facilities in the two sets packages (and their inner generic
keys packages) just as we did for the two maps packages (the discussion
is almost identical).

@leading@;The ordered sets package has the following additional subprograms
@begin[Example]
@key[procedure] Delete_First(Container: @key[in out] Set);
@key[procedure] Delete_Last(Container: @key[in out] Set);

@key[function] First_Element(Container: Set) @key[return] Element_Type;
@key[function] Last_Element(Container: Set) @key[return] Element_Type;
@key[function] Previous(Position: Cursor) @key[return] Cursor;
@key[procedure] Previous(Position: @key[in out] Cursor);

@key[function] Floor(Container: Set; Item: Element_Type) @key[return] Cursor;
@key[function] Ceiling(Container: Set; Item: Element_Type) @key[return] Cursor;

@key[function] "<" (Left, Right: Cursor) @key[return] Boolean;
@key[function] ">" (Left, Right: Cursor) @key[return] Boolean;
@key[function] "<" (Left: Cursor; Right: Element_Type) @key[return] Boolean;
@key[function] ">" (Left: Cursor; Right: Element_Type) @key[return] Boolean;
@key[function] "<" (Left: Element_Type; Right: Cursor) @key[return] Boolean;
@key[function] ">" (Left: Element_Type; Right: Cursor) @key[return] Boolean;

@key[procedure] Reverse_Iterate(
      Container: @key[in] Set;
      Process: @key[not null access procedure] (Position: @key[in] Cursor));
@end[Example]

These are again largely self-evident. The functions @exam[Floor] and
@exam[Ceiling] are similar to those for ordered maps @en @exam[Floor]
searches for the last element which is not greater than @exam[Item]
and @exam[Ceiling] searches for the first element which is not less
than @exam[Item] @en they return @exam[No_Element] if there is not one.

@leading@;The functions @exam["<"] and @exam[">"] are very important for
ordered sets. The first is equivalent to
@begin[Example]
@key[function] "<" (Left, Right: Cursor) @key[return] Boolean @key[is]
@key[begin]
   @key[return] Element(Left) < Element(Right);
@key[end] "<";
@end[Example]

There is a general philosophy that the container packages should work
efficiently even if the elements themselves are very large @en perhaps
even other containers. We should therefore avoid copying elements.
(Passing them as parameters is of course no problem since they will
be passed by reference if they are large structures.) So in this case
the built-in comparison is valuable because it can avoid the copying
which would occur if we wrote the function ourselves with the explicit
internal calls of the function @exam[Element].

On the other hand, there is a general expectation that keys will be
small and so there is no corresponding problem with copying keys.
Thus such built-in functions are less important for maps than sets
but they are provided for maps for uniformity.

@leading@;The following are additional in the package @exam[Generic_Keys] for
ordered sets

@begin[Example]
@key[function] Equivalent_Keys(Left, Right: Key_Type) @key[return] Boolean;
@end[Example]

This corresponds to the formal generic parameter of the same name
in the package @exam[Generic_Keys] for hashed sets as mentioned earlier.

@begin[Example]
@key[function] Floor(Container: Set; Key: Key_Type) @key[return] Cursor;
@key[function] Ceiling(Container: Set; Key: Key_Type) @key[return] Cursor;
@end[Example]

These are much as the corresponding functions in the parent package
except that they use the formal parameter @exam["<"] of @exam[Generic_Keys]
for the search.

@leading@;Hashed sets, like hashed maps also have the facility to specify a
capacity as for the vectors package. Thus we have
@begin[Example]
@key[procedure] Reserve_Capacity(Container: @key[in out] Set; Capacity: @key[in] Count_Type);

@key[function] Capacity(Container: Set) @key[return] Count_Type;
@end[Example]

The behaviour is much as for vectors and hashed maps. We don't have
to set the capacity ourselves since it will be automatically extended
as necessary but it might significantly improve performance to do
so. Note again that @exam[Length(S)] cannot exceed @exam[Capacity(S)]
but might be much less.

@leading@;The other additional subprograms for hashed sets are
@begin[Example]
@key[function] Equivalent_Elements(Left, Right: Cursor) @key[return] Boolean;
@key[function] Equivalent_Elements(Left: Cursor; Right: Element_Type) @key[return] Boolean;
@key[function] Equivalent_Elements(Left: Element_Type; Right: Cursor) @key[return] Boolean;
@end[Example]

@leading@;Again, these are very important for sets. The first is equivalent
to
@begin[Example]
@key[function] Equivalent_Elements(Left, Right: Cursor) @key[return] Boolean @key[is]
@key[begin]
   @key[return] Equivalent_Elements(Element(Left), Element(Right));
@key[end] Equivalent_Elements;
@end[Example]

and once more we see that the built-in functions can avoid the copying
of the type @exam[Element] that would occur if we wrote the functions
ourselves.


@LabeledClause{Indefinite containers}


There are versions of the six container packages we have just been
discussing for indefinite types.

As mentioned in Section @RefSecNum{Organization of containers}, an
indefinite (sub)type is one for which
we cannot declare an object without giving a constraint (either explicitly
or though an initial value). Moreover we cannot have an array of an
indefinite subtype. The type @exam[String] is a good example. Thus
we cannot declare an array of the type @exam[String] because the components
might not all be the same size and indexing would be a pain. Class
wide types are also indefinite.@Defn{Indefinite subtype}

@leading@;The specification of the indefinite container for lists starts
@begin[Example]
@key[generic]
   @key[type] Element_Type(<>) @key[is private];
   @key[with] @key[function] "=" (Left, Right: Element_Type) @key[return] Boolean @key[is] <>;
@key[package] Ada.Containers.Indefinite_Doubly_Linked_Lists @key[is]
   @key[pragma] Preelaborate(Indefinite_Doubly_Linked_Lists);
@end[Example]

@leading@;where we see that the formal type @exam[Element_Type] has unknown
discriminants and so permits the actual type to be any indefinite
type (and indeed a definite type as well). So if we want to manipulate
lists of strings where the individual strings can be of any length
then we declare
@begin[Example]
@key[package] String_Lists @key[is new]  Ada.Containers.Indefinite_Doubly_Linked_Lists(String);
@end[Example]

@leading@;In the case of ordered maps we have
@begin[Example]
@key[generic]
   @key[type] Key_Type(<>) @key[is private];
   @key[type] Element_Type(<>)  @key[is private];
   @key[with function] "<" (Left, Right: Key_Type) @key[return] Boolean @key[is] <>;
   @key[with function] "=" (Left, Right: Element_Type) @key[return] Boolean @key[is] <>;
@key[package] Ada.Containers.Indefinite_Ordered_Maps @key[is]
   @key[pragma] Preelaborate(Indefinite_Ordered_Maps);
@end[Example]

showing that both @exam[Element_Type] and @exam[Key_Type] can be indefinite.

There are two other differences from the definite versions which should
be noted.

One is that the @exam[Insert] procedures for @exam[Vectors], @exam[Lists]
and @exam[Maps] which insert an element with its default value are
omitted (because there is no way to create a default initialized object
of an indefinite type anyway).

The other is that the parameter @exam[Element] of the access procedure
@exam[Process] of @exam[Update_Element] (or the garrulous
@exam[Update_Element_Preserving_Key] in the case of sets) can be constrained
even if the type @exam[Element_Type] is unconstrained.

@leading@;As an example of the use of an indefinite container consider the
problem of creating an index. For each word in a text file we need a list
of its occurrences. The individual words can be represented as just
objects of the type @exam[String]. It is perhaps convenient to consider
strings to be the same irrespective of the case of characters and
so we define
@begin[Example]
@key[function] Same_Strings(S, T: String) @key[return] Boolean @key[is]
@key[begin]
   @key[return] To_Lower(S) = To_Lower(T);
@key[end] Same_Strings;
@end[Example]

where the function @exam[To_Lower] is from the package @exam[Ada.Characters.Handling].

@leading@;We can suppose that the positions of the words are described by a
type @exam[Place] thus
@begin[Example]
@key[type] Place @key[is]
   @key[record]
      Page: Text_IO.Positive_Count;
      Line: Text_IO.Positive_Count;
      Col: Text_IO.Positive_Count;
   @key[end record];
@end[Example]

@leading@;The index is essentially a map from the type @exam[String] to a list
of values of type @exam[Place]. We first create a definite list container
for handling the lists thus
@begin[Example]
@key[package] Places @key[is new] Doubly_Linked_Lists(Place);
@end[Example]

@leading@;We then create an indefinite map container from the type @exam[String]
to the type @exam[List] thus
@begin[Example]
@key[package] Indexes @key[is new] Indefinite_Hashed_Maps(
        Key_Type => String;
        Element_Type => Places.List;
        Hash => Ada.Strings.Hash;
        Equivalent_Keys => Same_Strings;
        "=" => Places."=");
@end[Example]

@leading@;The index is then declared by writing
@begin[Example]
The_Index: Indexes.Map;
@end[Example]

Note that this example illustrates the use of nested containers since
the elements in the map are themselves containers (lists).

@leading@;It might be helpful for the index to contain information saying
which file it refers to. We can extend the type @exam[Map] thus (remember
that container types are tagged)
@begin[Example]
@key[type] Text_Map @key[is new] Indexes.Map @key[with]
   @key[record]
      File_Ref: Text_IO.File_Access;
   @key[end record];
@end[Example]

@leading@;and now we can more usefully declare
@begin[Example]
My_Index: Text_Map := (Indexes.Empty_Map @key[with] My_File'Access);
@end[Example]

@leading@;We can now declare various subprograms to manipulate our map. For
example to add a new item we have first to see whether the word is
already in the index @en if it is not then we add the new word to
the map and set its list to a single element whereas if it is already
in the index then we add the new place entry to the corresponding
list. Thus
@begin[Example]
@tabset[P42]
@key[procedure] Add_Entry(Index: @key[in out] Text_Map; Word: String; P: Place) @key[is]
   M_Cursor: Indexes.Cursor;
   A_LIst: Places.List;@\-- @examcom[empty list of places]
@key[begin]
   M_Cursor := Index.Find(Word);
   @key[if] M_Cursor = Indexes.No_Element @key[then]
      -- @examcom[it's a new word]
      A_LIst.Append(P);
      Index.Insert(Word, A_List);
   @key[else]
      -- @examcom[it's an old word]
      A_LIst := Element(M_Cursor);@\-- @examcom[get old list]
      A_List.Append(P);@\-- @examcom[add to it]
      Index.Replace_Element(M_Cursor, A_LIst);
   @key[end if];
@key[end] Add_Entry;
@end[Example]

A number of points should be observed. The type @exam[Text_Map] being
derived from @exam[Indexes.Map] inherits all the map operations and
so we can write @exam[Index.Find(Word)] which uses the prefixed notation
(or we can write @exam[Indexes.Find(Index, Word)]). On the other hand
auxiliary entities such as the type @exam[Cursor] and the constant
@exam[No_Element] are of course in the package @exam[Indexes] and
have to be referred to as @exam[Indexes.Cursor] and so on.

@leading@;A big problem with the procedure as written however is that it uses
@exam[Element] and @exam[Replace_Element] rather than @exam[Update_Element].
This means that it copies the whole of the existing list, adds the
new item to it, and then copies it back. Here is an alternative version
@begin[Example]
@tabset[P42]
@key[procedure] Add_Entry(Index: @key[in out] Text_Map; Word: String; P: Place) @key[is]
   M_Cursor: Indexes.Cursor;
   A_LIst: Places.List;@\-- @examcom[empty list of places]
@key[begin]
   M_Cursor := Index.Find(Word);
   @key[if] M_Cursor = Indexes.No_Element @key[then]
      -- @examcom[it's a new word]
      A_LIst.Append(P);
      Index.Insert(Word, A_List);
   @key[else]
      -- @examcom[it's an old word]
      @key[declare]
         -- @examcom[this procedure adds to the list in situ]
         @key[procedure] Add_It(The_Key: @key[in] String; The_List: @key[in out] Places.List) @key[is]
         @key[begin]
            The_List.Append(P);
         @key[end] Add_It;
      @key[begin]
         -- @examcom[and here we call it via Update_Element]
         Index.Update_Element(M_Cursor, Add_It'Access);
      @key[end];
   @key[end if];
@key[end] Add_Entry;
@end[Example]

@leading@;This is still somewhat untidy. In the case of a new word we might
as well make the new map entry with an empty list and then update
it thereby sharing the calls of @exam[Append]. We get
@begin[Example]
@key[procedure] Add_Entry(Index: @key[in out] Text_Map; Word: String; P: Place) @key[is]
   M_Cursor: Indexes.Cursor := Index.Find(Word);
   OK: Boolean;
@key[begin]
   @key[if] M_Cursor = Indexes.No_Element @key[then]
      -- @examcom[it's a new word]
      Index.Insert(Word, Places.Empty_List, M_Cursor, OK);
      -- @examcom[M_Cursor now refers to new position]
      -- @examcom[and OK will be True]
   @key[end if;]
   @key[declare]
     -- @examcom[this procedure adds to the list in situ]
      @key[procedure] Add_It(The_Key: @key[in] String; The_List: @key[in out] Places.List) @key[is]
      @key[begin]
         The_List.Append(P);
      @key[end] Add_It;
   @key[begin]
      -- @examcom[and here we call it via Update_Element]
      Index.Update_Element(M_Cursor, Add_It'Access);
   @key[end];
@key[end] Add_Entry;
@end[Example]

It will be recalled that there are various versions of @exam[Insert].
We have used that which has two out parameters being the position
where the node was inserted and a Boolean parameter indicating whether
a new node was inserted or not. In this case we know that it will
be inserted and so the final parameter is a nuisance (but sadly we
cannot default out parameters). Note also that we need not give the
parameter @exam[Places.Empty_List] because another version of @exam[Insert]
will do that automatically since that is the default value of a list
anyway.

Yet another approach is not to use @exam[Find] but just call @exam[Insert].
We can even use the defaulted version @en if the word is present then
the node is not changed and the position parameter indicates where
it is, if the word is not present then a new node is made with an
empty list and again the position parameter indicates where it is.

@begin[Example]
@key[procedure] Add_Entry(Index: @key[in out] Text_Map; Word: String; P: Place) @key[is]
   M_Cursor: Indexes.Cursor;
   Inserted: Boolean;
@key[begin]
   Index.Insert(Word, M_Cursor, Inserted);
   -- @examcom[M_Cursor now refers to position of node]
   -- @examcom[and Inserted indicates whether it was added]
   @key[declare]
     -- @examcom[this procedure adds to the list in situ]
      @key[procedure] Add_It(The_Key: @key[in] String; The_List: @key[in out] Places.List) @key[is]
      @key[begin]
         The_List.Append(P);
      @key[end] Add_It;
   @key[begin]
      -- @examcom[and here we call it via Update_Element]
      Index.Update_Element(M_Cursor, Add_It'Access);
   @key[end];
@key[end] Add_Entry;
@end[Example]

Curiously enough we do not need to use the value of @exam[Inserted].
We leave the reader to decide which of the various approaches is best.

@leading@;We can now do some queries on the index. For example we might want
to know how many different four-lettered words there are in the text.
We can either use @exam[Iterate] or do it ourselves with @exam[Next]
as follows
@begin[Example]
@key[function] Four_Letters(Index: Text_Map) @key[return] Integer @key[is]
   Count: Integer := 0;
   C: Indexes.Cursor := Index.First;
@key[begin]
   @key[loop]
      @key[if] Key(C)'Length = 4 @key[then]
         Count := Count + 1;
      @key[end if];
      Indexes.Next(C);
      @key[exit when] C = Indexes.No_Element;
   @key[end loop];
   @key[return] Count;
@key[end] Four_Letters;
@end[Example]

@leading@;We might finally wish to know how many four-lettered words there are
on a particular page. (This is just an exercise @en it would clearly
be simplest to search the original text!) We use @exam[Iterate] this
time both to scan the map for the words and then to scan each list
for the page number
@begin[Example]
@key[function] Four_Letters_On_Page(
         Index: Text_Map;
         Page: Text_IO.Positive_Count) @key[return] Integer @key[is]
   Count: Integer := 0;

   @key[procedure] Do_It_Map(C: Indexes.Cursor) @key[is]

      @key[procedure] Do_It_List(C: Places.Cursor) @key[is]
      @key[begin]
         @key[if] Element(C).Page = Page @key[then]
            Count := Count + 1;
         @key[end if];
      @key[end] Do_It_LIst;

      @key[procedure] Action(K: String; E: Places.List) @key[is]
      @key[begin]
         @key[if] K'Length = 4 @key[then]
            -- @examcom[now scan list for instances of Page]
            E.Iterate(Do_It_List'Access);
         @key[end if];
      @key[end] Action;

   @key[begin]
      Indexes.Query_Element(C, Action'Access);
   @key[end] Do_It_Map;

@key[begin]
   Index.Iterate(Do_It_Map'Access);
   @key[return] Count;
@key[end] Four_Letters_On_Page;
@end[Example]

We could of course have used @exam[First] and @exam[Next] to search
the list. But in any event the important point is that by using @exam[Query_Element]
we do not have to copy the list in order to scan it.


@LabeledClause{Sorting}


@leading@Defn2{Term=[sorting],Sec=[array]}The final facilities in the
container library are generic procedures
for array sorting. There are two versions, one for unconstrained arrays
and one for constrained arrays. Their specifications are@Defn2{Term=[package],Sec=[Ada.Containers.Generic_Array_Sort]}@Defn{Ada.Containers.Generic_Array_Sort package}@Defn{Generic_Array_Sort package}
@begin[Example]
@key[generic]
   @key[type] Index_Type @key[is] (<>);
   @key[type] Element_Type @key[is private];
   @key[type] Array_Type @key[is] @key[array] (Index_Type @key[range] <>) @key[of] Element_Type;
   @key[with function] "<" (Left, Right: Element_Type) @key[return] Boolean @key[is] <>;
@key[procedure] Ada.Containers.Generic_Array_Sort(Container: @key[in out] Array_Type);
@key[pragma] Pure(Ada.Containers.Generic_Array_Sort);
@end[Example]

@leading@;and@Defn2{Term=[package],Sec=[Ada.Containers.Generic_Constrained_Array_Sort]}@Defn{Ada.Containers.Generic_Constrained_Array_Sort package}@Defn{Generic_Constrained_Array_Sort package}
@begin[Example]
@key[generic]
   @key[type] Index_Type @key[is] (<>);
   @key[type] Element_Type @key[is private];
   @key[type] Array_Type @key[is] @key[array] (Index_Type) @key[of] Element_Type;
   @key[with function] "<" (Left, Right: Element_Type) @key[return] Boolean @key[is] <>;
@key[procedure] Ada.Containers.Generic_Constrained_Array_Sort(Container: @key[in out] Array_Type);
@key[pragma] Pure(Ada.Containers.Generic_Constrained_Array_Sort);
@end[Example]

These do the obvious thing. They sort the array @exam[Container] into
order as defined by the generic parameter @exam["<".] The emphasis
is on speed.



