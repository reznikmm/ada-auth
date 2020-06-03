@Part(precontainers-1, Root="ada.mss")
@comment{ $Source: e:\\cvsroot/ARM/Source/pre_con1.mss,v $ }
@comment{ $Revision: 1.10 $ $Date: 2020/06/03 00:09:01 $ $Author: randy $ }

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
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@ChgAdded{Version=[2],Text=[This @Chg{Version=[3],New=[subclause],Old=[section]}
describes the declarations that are
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Redundant[Some
operations @Chg{Version=[5],New=[@Defn2{Term=[tamper with cursors],Sec=[of a map]}@Defn2{Term=[tamper with elements],Sec=[of a map]}],Old=[of
these generic packages
have access-to-subprogram parameters. To ensure such operations are
well-defined, they guard against certain actions by the designated
subprogram. In particular, some operations]} check for @lquotes@;tampering with
cursors@rquotes of a container because they depend on the set of elements of
the container remaining constant, and others check for @lquotes@;tampering with
elements@rquotes of a container because they depend on elements of the
container not being replaced.]@Chg{Version=[5],New=[ When
tampering with cursors is @i<prohibited>@Defn2{Term=[prohibited],Sec=[tampering with a map]}
@Defn2{Term=[tampering],Sec=[prohibited for a map]}for a particular
map object @i<M>, Program_Error is propagated by the finalization
of @i<M>@Redundant[, as well as by a call that passes @i<M> to
certain of the operations of this package, as indicated by the precondition
of such an operation]. Similarly, when tampering with elements is @i<prohibited>
for @i<M>, Program_Error is propagated by a call that passes @i<M> to
certain of other operations of this package, as indicated by the precondition
of such an operation.],Old=[]}]}

@begin{NotIso}
@ChgAdded{Version=[5],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 8 through 15
are removed as preconditions now describe these rules.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgDeleted{Version=[5],Type=[Leading],Text=[@Chg{Version=[2],New=[@Defn2{Term=[tamper with cursors],Sec=[of a map]}
A subprogram is said to @i{tamper with cursors} of a map object @i<M>
if:],Old=[]}]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
inserts or deletes elements of @i<M>, that is,
it calls the Insert, Include, Clear, Delete, or Exclude procedures with @i<M>
as a parameter; or]}]}

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
finalizes @i<M>; or]}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[it
calls the Assign procedure with @i<M> as the Target parameter; or]}]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We don't need to explicitly mention
  @nt{assignment_statement}, because that finalizes the target object
  as part of the operation, and finalization of an object is already defined
  as tampering with cursors.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
calls the Move procedure with @i<M> as a parameter; or]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
calls one of the operations defined to tamper with the cursors of @i<M>.]}]}

@end{Itemize}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[Replace only
  modifies a key and element rather than rehashing, so it does not
  tamper with cursors.]}]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgDeleted{Version=[5],Type=[Leading],Text=[@Chg{Version=[2],New=[@Defn2{Term=[tamper with elements],Sec=[of a list]}
A subprogram is said to @i{tamper with elements} of a map
object @i<M> if:],Old=[]}]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
tampers with cursors of @i<M>; or]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
replaces one or more elements of @i<M>, that is,
it calls the Replace or Replace_Element procedures with @i<M>
as a parameter.]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[Complete
  replacement of an element can cause its
  memory to be deallocated while another operation is holding onto a reference
  to it. That can't be allowed. However, a simple modification of (part of) an
  element is not a problem, so Update_Element does not cause a problem.]}]}
@end{Reason}

@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0265-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0110-1]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[@Defn2{Term=[prohibited],Sec=[tampering with a map]}
@Defn2{Term=[tampering],Sec=[prohibited for a map]}
When tampering with cursors is @i<prohibited> for a particular map object
@i<M>, Program_Error is propagated by a call of any language-defined subprogram
that is defined to tamper with the cursors of @i<M>, leaving @i<M> unmodified.
Similarly, when tampering with elements is @i<prohibited> for a particular map
object @i<M>, Program_Error is propagated by a call of any language-defined
subprogram that is defined to tamper with the elements of @i<M> @Redundant[(or
tamper with the cursors of @i<M>)], leaving @i<M>
unmodified.@Chg{Version=[4],New=[ These checks are made before any other
defined behavior of the body of the language-defined subprogram.],Old=[]}]}]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[Tampering
  with elements includes tampering with
  cursors, so we mention it only from completeness in the second sentence.]}]}
@end{TheProof}

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
@ChgAdded{Version=[3],Text=[Map'Write for a Map object @i<M> writes
Length(@i<M>) elements of the map to the stream. It also may write
additional information about the map.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[Map'Read reads the representation of a map
from the stream, and assigns to @i<Item> a map with the same length and
elements as was written by Map'Write.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Streaming more elements than the container
  length is wrong. For implementation implications of this rule, see the Implementation Note in
  @RefSecNum{The Generic Package Containers.Vectors}.]}
@end{Ramification}


@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
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
@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Has_Element (Container : Map; Position : Cursor)
   @key{return} Boolean
   @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns True if Position designates
an element in Container, and returns False otherwise.]}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[If Position is No_element, Has_Element returns
  False.]}
@end{Ramification}
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
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Tampering_With_Cursors_Prohibited
   (Container : Map) @key{return} Boolean
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
   (Container : Map) @key{return} Boolean
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Length (Container : Map) @key{return} Count_Type@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the number of nodes in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Is_Empty (Container : Map) @key{return} Boolean@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => (@key[null], @key[use null]),
        Post => Is_Empty'Result = (Length (Container) = 0)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[Returns True
if Container is empty],Old=[Equivalent to Length (Container) = 0]}.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Clear (Container : @key{in out} Map)@Chg{Version=[5],New=[
   @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                    @key[or else raise] Program_Error,
        Post => Length (Container) = 0],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Removes all the nodes from Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Key (Position : Cursor) @key{return} Key_Type@Chg{Version=[5],New=[
   @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
        Nonblocking => Key_Type'Nonblocking,
        Global => (@key{in all}, @key{use} Key_Type)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then Constraint_Error is propagated.
Otherwise, ]}Key returns the key component of the node designated
by Position.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Key (Container : Map;
              Position : Cursor) @key{return} Key_Type
   @key{with} Pre  => (Position /= No_Element
                    @key{or else raise} Constraint_Error) @key{and then}
                (Has_Element (Container, Position)
                    @key{or else raise} Program_Error),
        Nonblocking => Key_Type'Nonblocking,
        Global => (@key{null}, @key{use} Key_Type);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Key returns the key component
of the node designated by Position.]}

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
]}Element returns the element component of the node designated by Position.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Element (Container : Map;
                  Position  : Cursor) @key{return} Element_Type
   @key{with} Pre  => (Position /= No_Element
                    @key{or else raise} Constraint_Error) @key{and then}
                (Has_Element (Container, Position)
                    @key{or else raise} Program_Error),
        Nonblocking => Element_Type'Nonblocking,
        Global => (@key{null}, @key{use} Element_Type);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Element returns the element
component of the node designated by Position.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace_Element (Container : @key{in out} Map;
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
assigns New_Item to the element of the node designated by
Position.@Chg{Version=[5],New=[ For the purposes of
determining whether the parameters overlap in a call to Replace_Element, the
Container parameter is not considered to overlap with any object
@Redundant[(including itself)].],Old=[]}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Query_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access procedure} (Key     : @key{in} Key_Type;
                                         Element : @key{in} Element_Type))@Chg{Version=[5],New=[
   @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0021-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then
Constraint_Error is propagated. Otherwise, ]}Query_Element calls
Process.@key{all} with the key and element from the node designated by Position
as the arguments.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of @Chg{Version=[3],New=[the map that contains the
element designated by Position is prohibited during the
execution of the call on Process.@key{all}],Old=[Container]}. Any exception raised by
Process.@key{all} is propagated.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],Text=[@key{procedure} Query_Element
  (Container : @key{in} Map;
   Position  : @key{in} Cursor;
   Process  : @key{not null access procedure} (Key     : @key{in} Key_Type;
                                         Element : @key{in} Element_Type))
  @key{with} Pre  => (Position /= No_Element 
                    @key{or else raise} Constraint_Error) @key{and then}
                 (Has_Element (Container, Position) 
                    @key{or else raise} Program_Error);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Query_Element calls
Process.@key{all} with the key and element from the node designated by Position
as the arguments. Tampering with the elements of Container is prohibited during the execution of the call on
Process.@key{all}. Any exception raised by Process.@key{all} is propagated.]}


@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Update_Element
  (Container : @key{in out} Map;
   Position  : @key{in}     Cursor;
   Process   : @key{not null access procedure} (Key     : @key{in}     Key_Type;
                                          Element : @key{in out} Element_Type))@Chg{Version=[5],New=[
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
Process.@key{all} with the key and element from the node designated by Position
as the arguments. @Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the elements of Container@Chg{Version=[3],New=[ is prohibited during the
execution of the call on Process.@key{all}],Old=[]}. Any exception raised by
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
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@key[type] Constant_Reference_Type
      (Element : @key[not null access constant] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
        Nonblocking, Global =>@key[in out synchronized],
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
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] Map;
                             Position  : @key[in] Cursor)
   @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
   @key[with] Pre  => (Position /= No_Element
                    @key[or else raise] Constraint_Error) @key[and then]
                (Has_Element (Container, Position)
                    @key[or else raise] Program_Error),
        Post => Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read access to an individual element of a Map given a
cursor.]}

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
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference (Container : @key[aliased in out] Map;
                    Position  : @key[in] Cursor)
   @key[return] Reference_Type@Chg{Version=[5],New=[
   @key[with] Pre  => (Position /= No_Element
                    @key[or else raise] Constraint_Error) @key[and then]
                (Has_Element (Container, Position)
                    @key[or else raise] Program_Error),
        Post => Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read and write access to an individual element of a Map given
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
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] Map;
                             Key       : @key[in] Key_Type)
   @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
   @key[with] Pre  => Find (Container, Key) /= No_Element @key[or else raise] Constraint_Error,
        Post => Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read access to an individual element of a map given a key value.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Equivalent to
Constant_Reference (Container, Find (Container, Key)).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference (Container : @key[aliased in out] Map;
                    Key       : @key[in] Key_Type)
   @key[return] Reference_Type@Chg{Version=[5],New=[
   @key[with] Pre  => Find (Container, Key) /= No_Element @key[or else raise] Constraint_Error,
        Post => Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read and write access to an individual element of a map given
a key value.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Equivalent to Reference (Container, Find (Container, Key)).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Assign (Target : @key{in out} Map; Source : @key{in} Map)@Chg{Version=[5],New=[
   @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Target)
                   @key[or else raise] Program_Error,
        Post => Length (Source) = Length (Target)],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Move (Target : @key{in out} Map;
                Source : @key{in out} Map)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Target)
                    @key{or else raise} Program_Error) @key{and then}
                (@key{not} Tampering_With_Cursors_Prohibited (Source)
                    @key{or else raise} Program_Error),
        Post => (@key{if} Target = Source @key{then} True
                 @key{else}
                    Length (Target) = Length (Source'Old) @key{and then}
                    Length (Source) = 0)],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Count_Type'Last - 1
                    @key{or else raise} Constraint_Error),
        Post => (@key{declare}
                   Original_Length : @key{constant} Count_Type :=
                      Length (Container)'Old;
                 @key{begin}
                   Has_Element (Container, Position) @key{and then}
                  (@key{if} Inserted then
                     Length (Container) = Original_Length + 1
                   @key{else}
                     Length (Container) = Original_Length))],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Count_Type'Last - 1
                    @key{or else raise} Constraint_Error),
        Post => (@key{declare}
                   Original_Length : @key{constant} Count_Type :=
                      Length (Container)'Old;
                 @key{begin}
                   Has_Element (Container, Position) @key{and then}
                  (@key{if} Inserted then
                     Length (Container) = Original_Length + 1
                   @key{else}
                     Length (Container) = Original_Length))],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Insert inserts Key into Container
as per the five-parameter Insert, with the difference that an element
initialized by default (see @RefSecNum{Object Declarations}) is inserted.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type;
                  New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Count_Type'Last - 1
                    @key{or else raise} Constraint_Error),
        Post => Length (Container) = Length (Container)'Old + 1],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Include (Container : @key{in out} Map;
                   Key       : @key{in}     Key_Type;
                   New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Count_Type'Last - 1
                    @key{or else raise} Constraint_Error),
        Post => (@key{declare}
                   Original_Length : @key{constant} Count_Type :=
                      Length (Container)'Old;
                 @key{begin}
                   Length (Container)
                      @key{in} Original_Length | Original_Length + 1)],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace (Container : @key{in out} Map;
                   Key       : @key{in}     Key_Type;
                   New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => Length (Container) = Length (Container)'Old],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Exclude (Container : @key{in out} Map;
                   Key       : @key{in}     Key_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => (@key{declare}
                   Original_Length : @key{constant} Count_Type :=
                      Length (Container)'Old;
                 @key{begin}
                   Length (Container)
                      @key{in} Original_Length - 1 | Original_Length)],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => Length (Container) = Length (Container)'Old - 1],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Delete checks if a node with a key
equivalent to Key is present in Container. If a match is found, Delete removes
the node from the map; otherwise, Constraint_Error is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} Map;
                  Position  : @key{in out} Cursor)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{if} Tampering_With_Cursors_Prohibited (Container)
                 @key{then raise} Program_Error) @key{and then}
                (Position /= No_Element 
                    @key{or else raise} Constraint_Error) @key{and then}
                (Has_Element (Container, Position)
                    @key{or else raise} Program_Error),
        Post => Length (Container) = Length (Container)'Old - 1 @key{and then}
                Position = No_Element],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then Constraint_Error is propagated. If Position
does not designate an element in Container, then Program_Error is propagated.
Otherwise, ]}Delete removes the node designated by Position from the
map.@Chg{Version=[5],New=[],Old=[ Position is set to No_Element on return.]}]}


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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First (Container : Map) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Post => (@key{if not} Is_Empty (Container)
                 @key{then} Has_Element (Container, First'Result)
                 @key{else} First'Result = No_Element)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Length (Container) = 0, then
First returns No_Element. Otherwise, First returns a cursor that designates the
first node in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Next (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{in all}, @key{use null}),
        Post => (@key{if} Position = No_Element @key{then} Next'Result = No_Element
                 @key{else} True)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a cursor that designates
the successor of the node designated by Position. If Position designates the
last node, then No_Element is returned. If Position equals No_Element, then
No_Element is returned.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Next (Container : Map;
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
successor of the node designated by Position in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Next (Position : @key{in out} Cursor)@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{in all}, @key{use null})],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Next (Position).]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{procedure} Next (Container : @key{in}     Map;
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
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Find (Container : Map;
               Key       : Key_Type) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Post => (@key{if} Find'Result = No_Element @key{then} True
                    @key{else} Has_Element (Container, Find'Result))],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Iterate
  (Container : @key{in} Map;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
   @key{with} Allows_Exit],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Iterate calls Process.@key{all}
with a cursor that designates each node in Container, starting with the first
node and moving the cursor according to the successor relation.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the cursors of Container@Chg{Version=[3],New=[ is prohibited during the
execution of a call on Process.@key{all}],Old=[]}. Any exception raised by
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

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[The nested package Stable
provides a type Stable.Map that represents
a @i<stable> map,@Defn2{Term=(stable),Sec=(map)} which is one that
cannot grow and shrink. Such a list can be created by calling the
Copy function, or by establishing a
@i<stabilized view> of a regular map.@Defn2{Term=[stabilized view],Sec=[map]}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Type=[Leading],Text=[The subprograms of the map package
that have a parameter or result of type Map
are included in the nested package Stable with the same specification, except
that the following are omitted:]}

@begin{Indent}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],Text=[Tampering_With_Cursors_Prohibited,
Tampering_With_Elements_Prohibited, Assign, Move,
Insert, Include, Clear, Delete, Exclude, (for Ordered_Maps) Delete_First and
Delete_Last, and (for Hashed_Maps) Reserve_Capacity]}
@end{Indent}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[Added]}
  @ChgAdded{Version=[5],Text=[The names Map and Cursor mean the types
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
to those for regular maps, except that the calls to
Tampering_With_Cursors_Prohibited and
Tampering_With_Elements_Prohibited that occur in preconditions are replaced
by False, and any that occur in postconditions are replaced by True.]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[If a stable map is declared with the Base
discriminant designating a pre-existing regular map, the stable map
represents a stabilized view of the underlying regular map, and any operation
on the stable map is reflected on the underlying regular map. While a
stabilized view exists, any operation that tampers with elements performed on
the underlying map is prohibited. The finalization of a stable map that
provides such a view removes this restriction on the underlying regular map
@Redundant[(though some other restriction might exist due to other concurrent
iterations or stabilized views)].]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[If a stable map is declared without specifying
Base, the object must be initialized. The initializing expression of the stable
map, @Redundant[typically a call on Copy], determines the Length
of the map. The Length of a stable map never changes after
initialization.]}

@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0022-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error for the actual function
associated with a generic formal subprogram, when called as part of an
operation of a map package, to tamper with elements of any map parameter of
the operation. Either Program_Error is raised, or the operation works as
defined on the value of the map either prior to, or subsequent to, some or
all of the modifications to the
map.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0027-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to call any subprogram
declared in the visible part of a map package
when the associated container has been finalized. If the operation takes
Container as an @key[in out] parameter, then it raises Constraint_Error or
Program_Error. Otherwise, the operation either proceeds as it would
for an empty container, or it raises
Constraint_Error@Defn2{Term=[Constraint_Error],Sec=(raised by detection of a bounded error)}
or Program_Error.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}
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
  finalization of the object of Reference_Type will try to access a nonexistent
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
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0298-1]}
  @ChgAdded{Version=[2],Text=[An assignment of a Map is a @lquotes@;deep@rquotes
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

@begin{Inconsistent2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1]}
  @ChgAdded{Version=[5],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  Tampering with elements is now defined to be equivalent to tampering with
  cursors for regular containers. If a program requires tampering detection
  to work, it might fail in Ada 202x. Needless to say, this shouldn't happen
  outside of test programs. See @Inconsistent2012Title in
  @RefSecNum{The Generic Package Containers.Vectors} for more details.]}
@end{Inconsistent2012}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0196-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}@b{Correction:}
  Replace_Element is now defined such that it can be used
  concurrently so long as it operates on different elements. This allows
  some container operations to be used in parallel without separate
  synchronization.]}
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


@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Hashed_Maps]}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
package Containers.Hashed_Maps has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0084-1],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[@key[with] Ada.Iterator_Interfaces;
],Old=[]}@key{generic}
   @key{type} Key_Type @key{is private};
   @key{type} Element_Type @key{is private};
   @key{with function} Hash (Key : Key_Type) @key{return} Hash_Type;
   @key{with function} Equivalent_Keys (Left, Right : Key_Type)
      @key{return} Boolean;
   @key{with function} "=" (Left, Right : Element_Type)
      @key{return} Boolean @Chg{Version=[3],New=[@b<is>],Old=[is]} <>;
@key{package} Ada.Containers.Hashed_Maps@Chg{Version=[5],New=[],Old=[ @key{is}]}@ChildUnit{Parent=[Ada.Containers],Child=[Hashed_Maps]}@Chg{Version=[5],New=[
   @key[with] Preelaborate, Remote_Types,
        Nonblocking, Global => @key[null] @key[is]],Old=[
   @key{pragma} Preelaborate(Hashed_Maps);@Chg{Version=[3],New=[
   @key{pragma} Remote_Types(Hashed_Maps);],Old=[]}]}]}

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
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Map} @key{is tagged private}@Chg{Version=[3],New=[
      @key[with] Constant_Indexing => Constant_Reference,
           Variable_Indexing => Reference,
           Default_Iterator  => Iterate,
           Iterator_Element  => Element_Type],Old=[]}@Chg{Version=[5],New=[,
           Iterator_View     => Stable.Map,
           Aggregate         => (Empty     => Empty,
                                 Add_Named => Insert),
           Stable_Properties => (Length,
                                 Tampering_With_Cursors_Prohibited,
                                 Tampering_With_Elements_Prohibited),
           Default_Initial_Condition =>
              Length (Map) = 0 @key{and then}
              (@key{not} Tampering_With_Cursors_Prohibited (Map)) @key{and then}
              (@key{not} Tampering_With_Elements_Prohibited (Map))],Old=[]};
   @key{pragma} Preelaborable_Initialization(Map);]}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
   @ChgAdded{Version=[5],Text=[Unlike a Vector, the Stable_Properties of a
   Hashed_Map do not include the Capacity. If we had included it, some of the
   otherwise shared definitions would need different postconditions for
   Hashed_Maps and Ordered_Maps. If we were starting these containers from
   scratch, we probably would have approached the sharing of definitions
   differently so that we could avoid issues like this, but a major
   reorganization of this existing material would be too much change.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Cursor} @key{is private};
   @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Empty_Map} : @key{constant} Map;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[in all], @key[use null])],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Has_Element} (Container : Map; Position : Cursor)
      @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}@ChgNote{Just a paragraph number change}
@ChgAdded{Version=[3],Text=[   @key[package] @AdaPackDefn{Map_Iterator_Interfaces} @key[is new]
       Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "=" (Left, Right : Map) @key{return} Boolean;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Tampering_With_Cursors_Prohibited}
      (Container : Map) @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Tampering_With_Elements_Prohibited}
      (Container : Map) @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0339-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Empty} (Capacity : Count_Type := @RI{implementation-defined})
      @key[return] Map
      @key[with] Post =>
         Capacity (Empty'Result) >= Capacity @key[and then]
         @key[not] Tampering_With_Elements_Prohibited (Empty'Result) @key[and then]
         @key[not] Tampering_With_Cursors_Prohibited (Empty'Result) @key[and then]
         Length (Empty'Result) = 0;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Capacity} (Container : Map) @key{return} Count_Type@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reserve_Capacity} (Container : @key{in out} Map;
                               Capacity  : @key{in}     Count_Type)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                    @key[or else raise] Program_Error,
           Post => Container.Capacity >= Capacity],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Length} (Container : Map) @key{return} Count_Type@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Empty} (Container : Map) @key{return} Boolean@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[null], @key[use null]),
           Post => Is_Empty'Result = (Length (Container) = 0)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Clear} (Container : @key{in out} Map)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                       @key[or else raise] Program_Error,
           Post => Capacity (Container) = Capacity (Container)'Old @key{and then}
                   Length (Container) = 0],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Key} (Position : Cursor) @key{return} Key_Type@Chg{Version=[5],New=[
      @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
           Nonblocking => Key_Type'Nonblocking,
           Global => (@key{in all}, @key{use} Key_Type)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Key} (Container : Map;
                 Position : Cursor) @key{return} Key_Type
      @key{with} Pre  => (Position /= No_Element
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position)
                       @key{or else raise} Program_Error),
           Nonblocking => Key_Type'Nonblocking,
           Global => (@key{null}, @key{use} Key_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Position : Cursor) @key{return} Element_Type@Chg{Version=[5],New=[
      @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
           Nonblocking => Element_Type'Nonblocking,
           Global => (@key{in all}, @key{use} Element_Type)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Element} (Container : Map;
                     Position  : Cursor) @key{return} Element_Type
      @key{with} Pre  => (Position /= No_Element
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position)
                       @key{or else raise} Program_Error),
           Nonblocking => Element_Type'Nonblocking,
           Global => (@key{null}, @key{use} Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} Map;
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
      Process  : @key{not null access procedure} (Key     : @key{in} Key_Type;
                                            Element : @key{in} Element_Type))@Chg{Version=[5],New=[
      @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Container : @key{in} Map;
      Position  : @key{in} Cursor;
      Process   : @key{not null access procedure} (Key     : @key{in} Key_Type;
                                             Element : @key{in} Element_Type))
      @key{with} Pre  => (Position /= No_Element 
                       @key{or else raise} Constraint_Error) @key{and then}
                    (Has_Element (Container, Position) 
                       @key{or else raise} Program_Error);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Update_Element}
     (Container : @key{in out} Map;
      Position  : @key{in}     Cursor;
      Process   : @key{not null access procedure}
                      (Key     : @key{in}     Key_Type;
                       Element : @key{in out} Element_Type))@Chg{Version=[5],New=[
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
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Map;
                                Position  : @key[in] Cursor)
      @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre  => (Position /= No_Element
                       @key[or else raise] Constraint_Error) @key[and then]
                   (Has_Element (Container, Position)
                       @key[or else raise] Program_Error),
           Post => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] Map;
                       Position  : @key[in] Cursor)
      @key[return] Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre  => (Position /= No_Element
                       @key[or else raise] Constraint_Error) @key[and then]
                   (Has_Element (Container, Position)
                       @key[or else raise] Program_Error),
           Post => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Map;
                                Key       : @key[in] Key_Type)
      @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre  => Find (Container, Key) /= No_Element @key[or else raise] Constraint_Error,
           Post => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] Map;
                       Key       : @key[in] Key_Type)
      @key[return] Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre  => Find (Container, Key) /= No_Element @key[or else raise] Constraint_Error,
           Post => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} Map; Source : @key{in} Map)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Target)
                      @key[or else raise] Program_Error,
           Post => Length (Source) = Length (Target) @key[and then]
                   Capacity (Target) >= Length (Source)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Copy} (Source : Map; Capacity : Count_Type := 0)
      @key[return] Map@Chg{Version=[5],New=[
      @key[with] Pre  => Capacity = 0 @key[or else] Capacity >= Length (Source)
                    @key[or else raise] Capacity_Error,
           Post =>
              Length (Copy'Result) = Length (Source) @key[and then]
              @key[not] Tampering_With_Elements_Prohibited (Copy'Result) @key[and then]
              @key[not] Tampering_With_Cursors_Prohibited (Copy'Result) @key[and then]
              Copy'Result.Capacity = (if Capacity = 0 then
                 Length (Source) else Capacity)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Move} (Target : @key{in out} Map;
                   Source : @key{in out} Map)@Chg{Version=[5],New=[
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
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      Has_Element (Container, Position) @key{and then}
                     (@key{if} Inserted then
                        Length (Container) = Original_Length + 1
                      @key{else}
                        Length (Container) = Original_Length)) @key{and then}
                    Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      Has_Element (Container, Position) @key{and then}
                     (@key{if} Inserted then
                        Length (Container) = Original_Length + 1
                      @key{else}
                        Length (Container) = Original_Length)) @key{and then}
                    Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => Length (Container) = Length (Container)'Old + 1 @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Include} (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type;
                      New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      Length (Container)
                         @key{in} Original_Length | Original_Length + 1) @key{and then}
                    Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace} (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type;
                      New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error,
           Post => Length (Container) = Length (Container)'Old],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Exclude} (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error,
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      Length (Container)
                         @key{in} Original_Length - 1 | Original_Length)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error,
           Post => Length (Container) = Length (Container)'Old - 1],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Map;
                     Position  : @key{in out} Cursor)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Position /= No_Element 
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position)
                       @key{or else raise} Program_Error),
           Post => Length (Container) = Length (Container)'Old - 1 @key{and then}
                   Position = No_Element],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First} (Container : Map) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Post => (@key{if not} Is_Empty (Container)
                    @key{then} Has_Element (Container, First'Result)
                    @key{else} First'Result = No_Element)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Next} (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{in all}, @key{use null}),
           Post => (@key{if} Position = No_Element @key{then} Next'Result = No_Element
                    @key{else} True)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Next} (Container : Map;
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
      @key{with} Nonblocking, Global => (@key{in all}, @key{use null})],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Next} (Container : @key{in}     Map;
                   Position  : @key{in out} Cursor)
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Position /= No_Element
                    @key{then} Has_Element (Container, Position));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Find} (Container : Map;
                  Key       : Key_Type)
      @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Post => (@key{if} Find'Result = No_Element @key{then} True
                    @key{else} Has_Element (Container, Find'Result))],Old=[]};]}

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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Keys} (Left, Right : Cursor)
      @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => (Left =/ No_Element @key{and then} Right /= No_Element)
                         @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Keys} (Left  : Cursor;
                             Right : Key_Type)
      @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => Left /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Keys} (Left  : Key_Type;
                             Right : Cursor)
      @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => Right /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Iterate}
     (Container : @key{in} Map;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
      @key{with} Allows_Exit],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[   @key[function] Iterate (Container : @key[in] Map)
      @key[return] Map_Iterator_Interfaces.@Chg{Version=[5],New=[Parallel_Iterator],Old=[Forward_Iterator]}'Class@Chg{Version=[5],New=[
      @key[with] Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[   @key{package} @AdaPackDefn{Stable} @key{is}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1],ARef=[AI12-0339-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Map} (Base : @key{not null access} Hashed_Maps.Map) @key{is}
         @key{tagged limited private}
         @key{with} Constant_Indexing => Constant_Reference,
              Variable_Indexing => Reference,
              Default_Iterator  => Iterate,
              Iterator_Element  => Element_Type,
              Aggregate         => (Empty      => Empty,
                                    Add_Named  => Insert),
              Stable_Properties => (Length),
	      Global => @key[null],
              Default_Initial_Condition => Length (Map) = 0;
      @key{pragma} Preelaborable_Initialization(Map);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Cursor} @key{is private};
      @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @AdaObjDefn{Empty_Map} : @key{constant} Map;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean
         @key{with} Nonblocking, Global => (@key{in all}, @key{use null});]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{package} @AdaPackDefn{Map_Iterator_Interfaces} @key[is new]
         Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} Hashed_Maps.Map;
                        Source : @key{in} Map)
         @key{with} Post => Length (Source) = Length (Target);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Copy} (Source : Hashed_Maps.Map) @key{return} Map
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
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Empty (Capacity : Count_Type := @RI{implementation-defined})
   @key[return] Map
   @key[with] Post =>
      Capacity (Empty'Result) >= Capacity @key[and then]
      @key[not] Tampering_With_Elements_Prohibited (Empty'Result) @key[and then]
      @key[not] Tampering_With_Cursors_Prohibited (Empty'Result) @key[and then]
      Length (Empty'Result) = 0;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI12-0339-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns an empty map.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Capacity (Container : Map) @key{return} Count_Type@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the capacity of Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reserve_Capacity (Container : @key{in out} Map;
                            Capacity  : @key{in}     Count_Type)@Chg{Version=[5],New=[
   @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                 @key[or else raise] Program_Error,
        Post => Container.Capacity >= Capacity],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Deleted],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],
Text=[@Chg{Version=[5],New=[],Old=[Reserve_Capacity tampers with the
cursors of Container.]}]}

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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Clear (Container : @key{in out} Map)@Chg{Version=[5],New=[
   @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                    @key[or else raise] Program_Error,
        Post => Capacity (Container) = Capacity (Container)'Old @key{and then}
                Length (Container) = 0],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[In addition to the semantics
described in @RefSecNum{Maps}, Clear does not affect the capacity of
Container.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[procedure] Assign (Target : @key[in out] Map; Source : @key[in] Map)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Target)
                      @key[or else raise] Program_Error,
           Post => Length (Source) = Length (Target) @key[and then]
                   Capacity (Target) >= Length (Source)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[In addition to the semantics
described in @RefSecNum{Maps}, if the length of Source is greater than the
capacity of Target,
Reserve_Capacity (Target, Length (Source)) is called before assigning
any elements.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Copy (Source : Map; Capacity : Count_Type := 0)
   @key[return] Map@Chg{Version=[5],New=[
   @key[with] Pre  => Capacity = 0 @key[or else] Capacity >= Length (Source)
                 @key[or else raise] Capacity_Error,
        Post =>
           Length (Copy'Result) = Length (Source) @key[and then]
           @key[not] Tampering_With_Elements_Prohibited (Copy'Result) @key[and then]
           @key[not] Tampering_With_Cursors_Prohibited (Copy'Result) @key[and then]
           Copy'Result.Capacity = (if Capacity = 0 then
              Length (Source) else Capacity)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a map whose keys and
elements are initialized from the keys and elements of
Source.@Chg{Version=[5],New=[],Old=[ If Capacity is 0,
then the map capacity is the length of Source; if Capacity is equal to or
greater than the length of Source, the map capacity is at least the specified
value. Otherwise, the operation propagates Capacity_Error.]}]}

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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Count_Type'Last - 1
                    @key{or else raise} Constraint_Error),
        Post => (@key{declare}
                   Original_Length : @key{constant} Count_Type :=
                      Length (Container)'Old;
                 @key{begin}
                   Has_Element (Container, Position) @key{and then}
                  (@key{if} Inserted then
                     Length (Container) = Original_Length + 1
                   @key{else}
                     Length (Container) = Original_Length)) @key{and then}
                 Capacity (Container) >= Length (Container)],Old=[]};]}
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
next nonempty bucket.]}

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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Equivalent_Keys (Left, Right : Cursor)
   @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => (Left /= No_Element @key{and then} Right /= No_Element)
                      @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Equivalent_Keys (Key
(Left), Key (Right)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Equivalent_Keys (Left  : Cursor;
                          Right : Key_Type) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => Left /= No_Element @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Equivalent_Keys (Key
(Left), Right).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Equivalent_Keys (Left  : Key_Type;
                          Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => Right /= No_Element @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Equivalent_Keys
(Left, Key (Right)).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] Map)
   @key[return] Map_Iterator_Interfaces.@Chg{Version=[5],New=[Parallel_Iterator],Old=[Forward_Iterator]}'Class@Chg{Version=[5],New=[
   @key[with] Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1],ARef=[AI05-0269-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate returns
an iterator object (see @RefSecNum{User-Defined Iterator Types}) that
will generate a value for a loop parameter (see
@RefSecNum{Generalized Loop Iteration}) designating
each node in Container, starting with the first node and moving the cursor
according to the successor relation@Chg{Version=[5],New=[ when used as a forward
iterator, and processing all nodes concurrently when used as a parallel
iterator],Old=[]}. Tampering with the cursors of Container is prohibited while
the iterator object exists (in particular, in the @nt{sequence_of_statements} of
the @nt{loop_statement} whose @nt{iterator_specification} denotes this object).
The iterator object needs finalization.]}

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
  are added to Containers.Hashed_Maps. If an instance of Containers.Hashed_Maps
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
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0212-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}Maps now support
  named container aggregates, so @nt{aggregate} syntax can be used to
  create Maps.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0266-1]}
  @ChgAdded{Version=[5],Text=[The iterator for the
  container now can return a parallel iterator which can be used to
  process the container in parallel.]}
@end{Extend2012}

@begin{DiffWord2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[Added contracts to this container. This includes
  describing some of the semantics with pre- and postconditions, rather than
  English text. Note that the preconditions can be Suppressed (see
  @RefSecNum{Suppressing Checks}).]}
@end{DiffWord2012}


@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Ordered_Maps]}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
package Containers.Ordered_Maps has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0084-1],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[@key[with] Ada.Iterator_Interfaces;
],Old=[]}@key{generic}
   @key{type} Key_Type @key{is private};
   @key{type} Element_Type @key{is private};
   @key{with function} "<" (Left, Right : Key_Type) @key{return} Boolean @key{is} <>;
   @key{with function} "=" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Ordered_Maps@Chg{Version=[5],New=[],Old=[ @key{is}]}@ChildUnit{Parent=[Ada.Containers],Child=[Ordered_Maps]}@Chg{Version=[5],New=[
   @key[with] Preelaborate, Remote_Types,
        Nonblocking, Global => @key[null] @key[is]],Old=[
   @key{pragma} Preelaborate(Ordered_Maps);@Chg{Version=[3],New=[
   @key{pragma} Remote_Types(Ordered_Maps);],Old=[]}]}]}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
   @ChgAdded{Version=[5],Text=[For discussion on the reasons and meaning of
   the specifications of the Global and Nonblocking aspects in this generic
   package, see the notes on the equivalent operations in the specification
   of the Containers.Vectors package (see 
   @RefSecNum{The Generic Package Containers.Vectors}).]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Keys} (Left, Right : Key_Type) @key{return} Boolean@Chg{Version=[5],New=[
      @key{is} (@key{not} ((Left < Right) @key{or} (Right < Left)));],Old=[]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0111-1],ARef=[AI12-0112-1],ARef=[AI12-0212-1],ARef=[AI12-0339-1]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Map} @key{is tagged private}@Chg{Version=[3],New=[
      @key[with] Constant_Indexing => Constant_Reference,
           Variable_Indexing => Reference,
           Default_Iterator  => Iterate,
           Iterator_Element  => Element_Type],Old=[]}@Chg{Version=[5],New=[,
           Iterator_View     => Stable.Map,
           Aggregate         => (Empty     => Empty,
                                 Add_Named => Insert),
           Stable_Properties => (Length,
                                 Tampering_With_Cursors_Prohibited,
                                 Tampering_With_Elements_Prohibited),
           Default_Initial_Condition =>
              Length (Map) = 0 @key{and then}
              (@key{not} Tampering_With_Cursors_Prohibited (Map)) @key{and then}
              (@key{not} Tampering_With_Elements_Prohibited (Map))],Old=[]};
   @key{pragma} Preelaborable_Initialization(Map);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Cursor} @key{is private};
   @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Empty_Map} : @key{constant} Map;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[in all], @key[use null])],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Has_Element} (Container : Map; Position : Cursor)
      @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}@ChgNote{Just a paragraph number change}
@ChgAdded{Version=[3],Text=[   @key[package] @AdaPackDefn{Map_Iterator_Interfaces} @key[is new]
       Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "=" (Left, Right : Map) @key{return} Boolean;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Tampering_With_Cursors_Prohibited}
      (Container : Map) @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Tampering_With_Elements_Prohibited}
      (Container : Map) @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0339-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Empty} @key{return} Map
      @key[is] (Empty_Map)
      @key[with] Post =>
         @key[not] Tampering_With_Elements_Prohibited (Empty'Result) @key[and then]
         @key[not] Tampering_With_Cursors_Prohibited (Empty'Result) @key[and then]
         Length (Empty'Result) = 0;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Length} (Container : Map) @key{return} Count_Type@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Empty} (Container : Map) @key{return} Boolean@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[null], @key[use null]),
           Post => Is_Empty'Result = (Length (Container) = 0)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Clear} (Container : @key{in out} Map)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                       @key[or else raise] Program_Error,
           Post => Length (Container) = 0],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Key} (Position : Cursor) @key{return} Key_Type@Chg{Version=[5],New=[
      @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
           Nonblocking => Key_Type'Nonblocking,
           Global => (@key{in all}, @key{use} Key_Type)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Key} (Container : Map;
                 Position : Cursor) @key{return} Key_Type
      @key{with} Pre  => (Position /= No_Element
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position)
                       @key{or else raise} Program_Error),
           Nonblocking => Key_Type'Nonblocking,
           Global => (@key{null}, @key{use} Key_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Position : Cursor) @key{return} Element_Type@Chg{Version=[5],New=[
      @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
           Nonblocking => Element_Type'Nonblocking,
           Global => (@key{in all}, @key{use} Element_Type)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Element} (Container : Map;
                     Position  : Cursor) @key{return} Element_Type
      @key{with} Pre  => (Position /= No_Element
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position)
                       @key{or else raise} Program_Error),
           Nonblocking => Element_Type'Nonblocking,
           Global => (@key{null}, @key{use} Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} Map;
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
      Process  : @key{not null access procedure} (Key     : @key{in} Key_Type;
                                            Element : @key{in} Element_Type))@Chg{Version=[5],New=[
      @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Container : @key{in} Map;
      Position  : @key{in} Cursor;
      Process   : @key{not null access procedure} (Key     : @key{in} Key_Type;
                                             Element : @key{in} Element_Type))
      @key{with} Pre  => (Position /= No_Element 
                       @key{or else raise} Constraint_Error) @key{and then}
                    (Has_Element (Container, Position) 
                       @key{or else raise} Program_Error);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Update_Element}
     (Container : @key{in out} Map;
      Position  : @key{in}     Cursor;
      Process   : @key{not null access procedure}
                      (Key     : @key{in}     Key_Type;
                       Element : @key{in out} Element_Type))@Chg{Version=[5],New=[
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
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Map;
                                Position  : @key[in] Cursor)
      @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre  => (Position /= No_Element
                       @key[or else raise] Constraint_Error) @key[and then]
                   (Has_Element (Container, Position)
                       @key[or else raise] Program_Error),
           Post => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] Map;
                       Position  : @key[in] Cursor)
      @key[return] Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre  => (Position /= No_Element
                       @key[or else raise] Constraint_Error) @key[and then]
                   (Has_Element (Container, Position)
                       @key[or else raise] Program_Error),
           Post => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =>(@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Map;
                                Key       : @key[in] Key_Type)
      @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre  => Find (Container, Key) /= No_Element @key[or else raise] Constraint_Error,
           Post => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] Map;
                       Key       : @key[in] Key_Type)
      @key[return] Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre  => Find (Container, Key) /= No_Element @key[or else raise] Constraint_Error,
           Post => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} Map; Source : @key{in} Map)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Target)
                      @key[or else raise] Program_Error,
           Post => Length (Source) = Length (Target)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Copy} (Source : Map)
      @key[return] Map@Chg{Version=[5],New=[
      @key[with] Post =>
         Length (Copy'Result) = Length (Source) @key[and then]
         @key[not] Tampering_With_Elements_Prohibited (Copy'Result) @key[and then]
         @key[not] Tampering_With_Cursors_Prohibited (Copy'Result)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Move} (Target : @key{in out} Map;
                   Source : @key{in out} Map)@Chg{Version=[5],New=[
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
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      Has_Element (Container, Position) @key{and then}
                     (@key{if} Inserted then
                        Length (Container) = Original_Length + 1
                      @key{else}
                        Length (Container) = Original_Length))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      Has_Element (Container, Position) @key{and then}
                     (@key{if} Inserted then
                        Length (Container) = Original_Length + 1
                      @key{else}
                        Length (Container) = Original_Length))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type;
                     New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => Length (Container) = Length (Container)'Old + 1],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Include} (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type;
                      New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      Length (Container)
                         @key{in} Original_Length | Original_Length + 1)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace} (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type;
                      New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error,
           Post => Length (Container) = Length (Container)'Old],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Exclude} (Container : @key{in out} Map;
                      Key       : @key{in}     Key_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error,
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      Length (Container)
                         @key{in} Original_Length - 1 | Original_Length)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Map;
                     Key       : @key{in}     Key_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error,
           Post => Length (Container) = Length (Container)'Old - 1],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Map;
                     Position  : @key{in out} Cursor)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Position /= No_Element 
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position)
                       @key{or else raise} Program_Error),
           Post => Length (Container) = Length (Container)'Old - 1 @key{and then}
                   Position = No_Element],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_First} (Container : @key{in out} Map)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error,
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      (@key{if} Original_Length = 0 @key{then} Length (Container) = 0
                       @key{else} Length (Container) = Original_Length - 1))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_Last} (Container : @key{in out} Map)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error,
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      (@key{if} Original_Length = 0 @key{then} Length (Container) = 0
                       @key{else} Length (Container) = Original_Length - 1))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First} (Container : Map) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Post => (@key{if not} Is_Empty (Container)
                    @key{then} Has_Element (Container, First'Result)
                    @key{else} First'Result = No_Element)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First_Element} (Container : Map)
      @key{return} Element_Type@Chg{Version=[5],New=[
      @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First_Key} (Container : Map)
      @key{return} Key_Type@Chg{Version=[5],New=[
      @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last} (Container : Map) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Post => (@key{if} not Is_Empty (Container)
                    @key{then} Has_Element (Container, Last'Result)
                    @key{else} Last'Result = No_Element)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last_Element} (Container : Map)
      @key{return} Element_Type@Chg{Version=[5],New=[
      @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last_Key} (Container : Map)
      @key{return} Key_Type@Chg{Version=[5],New=[
      @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Next} (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Global => (@key{in all}, @key{use null}),
           Nonblocking => True,
           Post => (@key{if} Position = No_Element @key{then} Next'Result = No_Element
                    @key{else} True)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Next} (Container : Map;
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
      @key{with} Nonblocking, Global => (@key{in all}, @key{use null})],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Next} (Container : @key{in}     Map;
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
      @key{with} Nonblocking, Global => (@key{in all}, @key{use null}),
           Post => (@key{if} Position = No_Element @key{then}
                       Previous'Result = No_Element
                    @key{else} True)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Previous} (Container : Map;
                      Position  : Cursor) @key{return} Cursor
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Position = No_Element @key{then}
                       Previous'Result = No_Element
                    @key{elsif} Previous'Result = No_Element @key{then}
                       Position = First (Container)
                    @key{else} Has_Element (Container, Previous'Result));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Previous} (Position : @key{in out} Cursor)@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{in all}, @key{use null})],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Previous} (Container : @key{in}     Map;
                       Position  : @key{in out} Cursor)
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Position /= No_Element
                    @key{then} Has_Element (Container, Position));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Find} (Container : Map;
                  Key       : Key_Type) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Post => (@key{if} Find'Result = No_Element @key{then} True
                    @key{else} Has_Element (Container, Find'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Container : Map;
                     Key       : Key_Type) @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Floor} (Container : Map;
                   Key       : Key_Type) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Post => (@key{if} Floor'Result = No_Element @key{then} True
                    @key{else} Has_Element (Container, Floor'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Ceiling} (Container : Map;
                     Key       : Key_Type) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Post => (@key{if} Ceiling'Result = No_Element @key{then} True
                    @key{else} Has_Element (Container, Ceiling'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Contains} (Container : Map;
                      Key       : Key_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} "<" (Left, Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => (Left /= No_Element @key{and then} Right /= No_Element)
                         @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} ">" (Left, Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => (Left /= No_Element @key{and then} Right /= No_Element)
                         @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} "<" (Left : Cursor; Right : Key_Type) @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => Left /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} ">" (Left : Cursor; Right : Key_Type) @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => Left /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} "<" (Left : Key_Type; Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => Right /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} ">" (Left : Key_Type; Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => Right /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure}  @AdaSubDefn{Iterate}
     (Container : @key{in} Map;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
      @key{with} Allows_Exit],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reverse_Iterate}
     (Container : @key{in} Map;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
      @key{with} Allows_Exit],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Iterate} (Container : @key[in] Map)
      @key[return] Map_Iterator_Interfaces.@Chg{Version=[5],New=[Parallel_Reversible_Iterator],Old=[Reversible_Iterator]}'Class@Chg{Version=[5],New=[
      @key[with] Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0262-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Iterate} (Container : @key[in] Map; Start : @key[in] Cursor)
      @key[return] Map_Iterator_Interfaces.Reversible_Iterator'Class@Chg{Version=[5],New=[
      @key[with] Pre  => (Start /= No_Element
                             @key[or else raise] Constraint_Error) @key[and then]
                      (Has_Element (Container, Start)
                          @key[or else raise] Program_Error),
           Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[   @key{package} @AdaPackDefn{Stable} @key{is}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1],ARef=[AI12-0339-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Map} (Base : @key{not null access} Ordered_Maps.Map) @key{is}
         @key{tagged limited private}
         @key{with} Constant_Indexing => Constant_Reference,
              Variable_Indexing => Reference,
              Default_Iterator  => Iterate,
              Iterator_Element  => Element_Type,
              Aggregate         => (Empty      => Empty,
                                    Add_Named  => Insert),
              Stable_Properties => (Length),
              Global            => @key[null],
              Default_Initial_Condition => Length (Map) = 0;
      @key{pragma} Preelaborable_Initialization(Map);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Cursor} @key{is private};
      @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @AdaObjDefn{Empty_Map} : @key{constant} Map;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean
         @key{with} Nonblocking, Global => (@key{in all}, @key{use null});]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{package} @AdaPackDefn{Map_Iterator_Interfaces} @key[is new]
         Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} Ordered_Maps.Map;
                        Source : @key{in} Map)
         @key{with} Post => Length (Source) = Length (Target);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Copy} (Source : Ordered_Maps.Map) @key{return} Map
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
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Copy (Source : Map)
   @key[return] Map@Chg{Version=[5],New=[
   @key[with] Post =>
      Length (Copy'Result) = Length (Source) @key[and then]
      @key[not] Tampering_With_Elements_Prohibited (Copy'Result) @key[and then]
      @key[not] Tampering_With_Cursors_Prohibited (Copy'Result)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a map whose keys and
elements are initialized from the corresponding keys and elements of Source.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_First (Container : @key{in out} Map)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => (@key{declare}
                   Original_Length : @key{constant} Count_Type :=
                      Length (Container)'Old;
                 @key{begin}
                   (@key{if} Original_Length = 0 @key{then} Length (Container) = 0
                    @key{else} Length (Container) = Original_Length - 1))],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, Delete_First
has no effect. Otherwise@Chg{Version=[3],New=[,],Old=[]}
the node designated by First (Container) is removed
from Container. Delete_First tampers with the cursors of Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_Last (Container : @key{in out} Map)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => (@key{declare}
                   Original_Length : @key{constant} Count_Type :=
                      Length (Container)'Old;
                 @key{begin}
                   (@key{if} Original_Length = 0 @key{then} Length (Container) = 0
                    @key{else} Length (Container) = Original_Length - 1))],Old=[]};]}
@end{Example}


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, Delete_Last
has no effect. Otherwise@Chg{Version=[3],New=[,],Old=[]}
the node designated by Last (Container) is removed
from Container. Delete_Last tampers with the cursors of Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First_Element (Container : Map)
   @key{return} Element_Type@Chg{Version=[5],New=[
   @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (First (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First_Key (Container : Map)
   @key{return} Key_Type@Chg{Version=[5],New=[
   @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Key (First (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last (Container : Map) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Post => (@key{if} not Is_Empty (Container)
                 @key{then} Has_Element (Container, Last'Result)
                 @key{else} Last'Result = No_Element)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a cursor that designates
the last node in Container. If Container is empty, returns No_Element.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last_Element (Container : Map)
   @key{return} Element_Type@Chg{Version=[5],New=[
   @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Last (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last_Key (Container : Map)
   @key{return} Key_Type@Chg{Version=[5],New=[
   @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Key (Last (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Previous (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{in all}, @key{use null}),
        Post => (@key{if} Position = No_Element @key{then}
                    Previous'Result = No_Element
                 @key{else} True)],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Previous (Container : Map;
                   Position : Cursor) @key{return} Cursor
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)
                    @key{or else raise} Program_Error,
        Post => (@key{if} Position = No_Element @key{then}
                    Previous'Result = No_Element
                 @key{elsif} Previous'Result = No_Element @key{then}
                    Position = First (Container)
                 @key{else} Has_Element (Container, Previous'Result));]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns a cursor designating the
predecessor of the node designated by Position in Container, if any.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Previous (Position : @key{in out} Cursor)@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{in all}, @key{use null})],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Previous (Position).]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{procedure} Previous (Container : @key{in}     Map;
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Floor (Container : Map;
                Key       : Key_Type) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Post => (@key{if} Floor'Result = No_Element @key{then} True
                 @key{else} Has_Element (Container, Floor'Result))],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Ceiling (Container : Map;
                  Key       : Key_Type) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Post => (@key{if} Ceiling'Result = No_Element @key{then} True
                 @key{else} Has_Element (Container, Ceiling'Result))],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Ceiling searches for the first node
whose key is not less than Key, using the generic formal "<" operator for keys.
If such a node is found, a cursor that designates it is returned.
Otherwise@Chg{Version=[3],New=[,],Old=[]} No_Element is returned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "<" (Left, Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => (Left /= No_Element @key{and then} Right /= No_Element)
                      @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Key (Left) < Key (Right).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} ">" (Left, Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => (Left /= No_Element @key{and then} Right /= No_Element)
                      @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Key (Right) < Key (Left).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "<" (Left : Cursor; Right : Key_Type) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => Left /= No_Element @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Key (Left) < Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} ">" (Left : Cursor; Right : Key_Type) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => Left /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Right < Key (Left).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "<" (Left : Key_Type; Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => Right /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Left < Key (Right).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} ">" (Left : Key_Type; Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => Right /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Key (Right) < Left.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reverse_Iterate
  (Container : @key{in} Map;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
   @key{with} Allows_Exit],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Iterates over the nodes in
Container as per @Chg{Version=[3],New=[procedure ],Old=[]}Iterate,
with the difference that the nodes are traversed in
predecessor order, starting with the last node.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] Map)
   @key[return] Map_Iterator_Interfaces.@Chg{Version=[5],New=[Parallel_Reversible_Iterator],Old=[Reversible_Iterator]}'Class@Chg{Version=[5],New=[
   @key[with] Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1],ARef=[AI05-0269-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate returns
@Chg{Version=[5],New=[an],Old=[a reversible]}
iterator object (see @RefSecNum{User-Defined Iterator Types}) that
will generate a value for a loop parameter
(see @RefSecNum{Generalized Loop Iteration}) designating
each node in Container, starting with the first node and moving the cursor
according to the successor relation when used as a forward iterator, and
starting with the last node and moving the cursor according to the predecessor
relation when used as a reverse iterator@Chg{Version=[5],New=[, and processing
all nodes concurrently when used as a parallel iterator],Old=[]}.
Tampering with the cursors of Container is prohibited while
the iterator object exists (in particular, in
the @nt{sequence_of_statements} of the @nt{loop_statement} whose
@nt{iterator_specification} denotes this object). The iterator object needs
finalization.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] Map; Start : @key[in] Cursor)
   @key[return] Map_Iterator_Interfaces.Reversible_Iterator'Class@Chg{Version=[5],New=[
   @key[with] Pre  => (Start /= No_Element
                       @key[or else raise] Constraint_Error) @key[and then]
                   (Has_Element (Container, Start)
                       @key[or else raise] Program_Error),
        Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0262-1],ARef=[AI05-0265-1],ARef=[AI05-0269-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Start is not No_Element and does not designate an item in Container,
then Program_Error is propagated. If Start is No_Element, then Constraint_Error
is propagated. Otherwise, ]}Iterate returns a reversible iterator object
(see @RefSecNum{User-Defined Iterator Types}) that will generate
a value for a loop parameter (see @RefSecNum{Generalized Loop Iteration})
designating each node in Container, starting with
the node designated by Start and moving the cursor according to the successor
relation when used as a forward iterator, or moving the cursor according to the
predecessor relation when used as a reverse iterator.
Tampering with the cursors of Container is prohibited while the iterator object
exists (in particular, in the @nt{sequence_of_statements} of the
@nt{loop_statement} whose @nt{iterator_specification} denotes this object). The
iterator object needs finalization.]}

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
  are added to Containers.Ordered_Maps. If an instance of Containers.Ordered_Maps
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

@begin{Incompatible2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1],ARef=[AI12-0112-1],ARef=[AI12-0339-1]}
  @ChgAdded{Version=[5],Text=[@Defn{incompatibilities with Ada 2012}A number of
  new subprograms, types, and even a nested package were added to
  Containers.Ordered_Maps to better support contracts and stable views. If an
  instance of Containers.Ordered_Maps
  is referenced in a @nt{use_clause}, and an entity @i<E> with the same
  @nt{defining_identifier} as a new entity in Containers.Ordered_Maps is
  defined in a package that is also referenced in a @nt{use_clause}, the
  entity @i<E> may no longer be use-visible, resulting in errors. This should
  be rare and is easily fixed if it does occur.]}
@end{Incompatible2012}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0212-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}Maps now support
  named container aggregates, so @nt{aggregate} syntax can be used to
  create Maps.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0266-1]}
  @ChgAdded{Version=[5],Text=[The iterator for the
  entire container now can return a parallel iterator which can be used to
  process the container in parallel.]}
@end{Extend2012}

@begin{DiffWord2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[Added contracts to this container. This includes
  describing some of the semantics with pre- and postconditions, rather than
  English text. Note that the preconditions can be Suppressed (see
  @RefSecNum{Suppressing Checks}).]}
@end{DiffWord2012}


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
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@ChgAdded{Version=[2],Text=[This @Chg{Version=[3],New=[subclause],Old=[section]}
describes the declarations that are common to both kinds of sets.
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Redundant[Some
operations @Chg{Version=[5],New=[@Defn2{Term=[tamper with cursors],Sec=[of a set]}@Defn2{Term=[tamper with elements],Sec=[of a set]}],Old=[of
these generic packages
have access-to-subprogram parameters. To ensure such operations are
well-defined, they guard against certain actions by the designated
subprogram. In particular, some operations]} check for @lquotes@;tampering with
cursors@rquotes of a container because they depend on the set of elements of
the container remaining constant@Chg{Version=[5],New=[ and ],Old=[, and others
check for @lquotes@;tampering with
elements@rquotes of a container because they depend]} on elements of the
container not being replaced.]@Chg{Version=[5],New=[ When
tampering with cursors is @i<prohibited>@Defn2{Term=[prohibited],Sec=[tampering with a set]}
@Defn2{Term=[tampering],Sec=[prohibited for a set]}for a particular
set object @i<M>, Program_Error is propagated by the finalization
of @i<M>@Redundant[, as well as by a call that passes @i<M> to
certain of the operations of this package, as indicated by the precondition
of such an operation].],Old=[]}]}

@begin{Discussion}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[Note that Replace_Element tampers with cursors
  because it might delete and reinsert the element if it moves in the set.
  That could change the order of iteration, which is what this check is
  designed to prevent. Replace also tampers with cursors, as it is defined
  in terms of Replace_Element.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[These inclusions mean that there are no operations
  that would tamper with elements that do not tamper with cursors. As such,
  we do not define tampering with elements at all for set containers. Earlier
  versions of Ada did so just so the description of subprograms are the same
  between containers, but since we've changed those to pre- and postconditions
  which are necessarily specific to each container, there no longer seems to
  be any reason to define tampering with elements for sets.]}
@end{Discussion}

@begin{NotIso}
@ChgAdded{Version=[5],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 8 through 14
are removed as preconditions now describe these rules.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgDeleted{Version=[5],Type=[Leading],Text=[@Chg{Version=[2],New=[@Defn2{Term=[tamper with cursors],Sec=[of a set]}
A subprogram is said to @i{tamper with cursors} of a set object @i<S>
if:],Old=[]}]}
@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
inserts or deletes elements of @i{S}, that is,
it calls the Insert, Include, Clear, Delete, Exclude, or Replace_Element
procedures with @i{S} as a parameter; or]}]}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[Operations which
  are defined to be equivalent to
  a call on one of these operations also are included. Similarly, operations
  which call one of these as part of their definition are included.]}]}
@end{Honest}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[We have to
  include Replace_Element here because
  it might delete and reinsert the element if it moves in the set. That could
  change the order of iteration, which is what this check is designed to
  prevent. Replace is also included, as it is defined in terms of
  Replace_Element.]}]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
finalizes @i<S>; or]}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[it
calls the Assign procedure with @i<S> as the Target parameter; or]}]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We don't need to explicitly mention
  @nt{assignment_statement}, because that finalizes the target object
  as part of the operation, and finalization of an object is already defined
  as tampering with cursors.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
calls the Move procedure with @i<S> as a parameter; or]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
calls one of the operations defined to tamper with the cursors of @i<S>.]}]}

@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgDeleted{Version=[5],Type=[Leading],Text=[@Chg{Version=[2],New=[@Defn2{Term=[tamper with elements],Sec=[of a list]}
A subprogram is said to @i{tamper with elements} of a set
object @i<S> if:],Old=[]}]}


@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[it
tampers with cursors of @i<S>.]}]}


@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[Complete
  replacement of an element can cause its
  memory to be deallocated while another operation is holding onto a reference
  to it. That can't be allowed. However, a simple modification of (part of) an
  element is not a problem, so Update_@!Element_@!Preserving_@!Key does not cause a
  problem.]}]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[We
  don't need to list Replace and Replace_Element
  here because they are covered by @lquotes@;tamper with cursors@rquotes.
  For Set, @lquotes@;tamper with cursors@rquotes@;
  and @lquotes@;tamper with elements@rquotes are the same. We leave both
  terms so that the rules for routines like Iterate and
  Query_Element are consistent across all containers.]}]}
@end{Reason}

@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0265-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0110-1]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg],ARef=[AI12-0111-1],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[@Defn2{Term=[prohibited],Sec=[tampering with a set]}
@Defn2{Term=[tampering],Sec=[prohibited for a set]}
When tampering with cursors is @i<prohibited> for a particular set object
@i<S>, Program_Error is propagated by a call of any language-defined subprogram
that is defined to tamper with the cursors of @i<S>, leaving @i<S> unmodified.
Similarly, when tampering with elements is @i<prohibited> for a particular set
object @i<S>, Program_Error is propagated by a call of any language-defined
subprogram that is defined to tamper with the elements of @i<S> @Redundant[(or
tamper with the cursors of @i<S>)], leaving @i<S>
unmodified.@Chg{Version=[4],New=[ These checks are made before any other
defined behavior of the body of the language-defined subprogram.],Old=[]}]}]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[Tampering
  with elements includes tampering with
  cursors, so we mention it only from completeness in the second sentence.]}]}
@end{TheProof}

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
@ChgAdded{Version=[3],Text=[Set'Write for a Set object @i<S> writes
Length(@i<S>) elements of the set to the stream. It also may write
additional information about the set.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[Set'Read reads the representation of a set
from the stream, and assigns to @i<Item> a set with the same length and
elements as was written by Set'Write.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Streaming more elements than the container
  length is wrong. For implementation implications of this rule, see the Implementation Note in
  @RefSecNum{The Generic Package Containers.Vectors}.]}
@end{Ramification}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
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
@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Has_Element (Container : Set; Position : Cursor)
   @key{return} Boolean
   @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns True if Position designates
an element in Container, and returns False otherwise.]}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[If Position is No_Element, Has_Element returns
  False.]}
@end{Ramification}

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
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key[function] Tampering_With_Cursors_Prohibited
   (Container : Set) @key{return} Boolean
   @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns True if tampering with
cursors is currently prohibited for Container, and returns False otherwise.]}

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
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key[function] To_Set (New_Item : Element_Type) @key{return} Set@Chg{Version=[5],New=[
   @key[with] Post => Length (To_Set'Result) = 1 @key[and then]
              @key{not} Tampering_with_Cursors_Prohibited (To_Set'Result)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a set containing the single element New_Item.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key[function] Length (Container : Set) @key{return} Count_Type@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the number of elements in
Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key[function] Is_Empty (Container : Set) @key{return} Boolean@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => (@key[null], @key[use null]),
        Post => Is_Empty'Result = (Length (Container) = 0)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[Returns True
if Container is empty],Old=[Equivalent to Length (Container) = 0]}.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Clear (Container : @key{in out} Set)@Chg{Version=[5],New=[
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
Position equals No_Element, then
Constraint_Error is propagated. Otherwise, ]}Element returns the element
designated by Position.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Element (Container : Set;
                  Position  : Cursor) @key{return} Element_Type
   @key{with} Pre  => (Position /= No_Element
                    @key{or else raise} Constraint_Error) @key{and then}
                (Has_Element (Container, Position)
                    @key{or else raise} Program_Error),
        Nonblocking => Element_Type'Nonblocking,
        Global => (@key{null}, @key{use} Element_Type);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Element returns the element
designated by Position.]}


@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace_Element (Container : @key{in out} Set;
                           Position  : @key{in}     Cursor;
                           New_item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Elements_Prohibited (Container)
                   @key{or else raise} Program_Error) @key{and then}
                (Position /= No_Element 
                   @key{or else raise} Constraint_Error) @key{and then}
                (Has_Element (Container, Position) @key{or else raise} Program_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1],ARef=[AI12-0196-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then
Constraint_Error is propagated; if Position does not designate an element in
Container, then Program_Error is propagated.
If an element equivalent to New_Item is already present in Container at a
position other than Position, Program_Error is propagated.
Otherwise@Chg{Version=[3],New=[,],Old=[]} ]}Replace_Element
assigns New_Item to the element designated by Position. Any
exception raised by the assignment is
propagated.@Chg{Version=[5],New=[ For the purposes of
determining whether the parameters overlap in a call to Replace_Element, the
Container parameter is not considered to overlap with any object
@Redundant[(including itself)].],Old=[]}]}

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
with the elements of @Chg{Version=[3],New=[the set that contains the
element designated by Position is prohibited during the
execution of the call on Process.@key{all}],Old=[Container]}. Any exception
raised by Process.@key{all} is propagated.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],Text=[@key{procedure} Query_Element
  (Container : @key{in} Set;
   Position  : @key{in} Cursor;
   Process  : @key{not null access procedure} (Element : @key{in} Element_Type))
   @key{with} Pre  => (Position /= No_Element 
                    @key{or else raise} Constraint_Error) @key{and then}
                 (Has_Element (Container, Position) 
                    @key{or else raise} Program_Error);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Query_Element calls
Process.@key{all} with the key and element from the node designated by Position
as the arguments. Tampering with the elements of Container is prohibited during the execution of the call on
Process.@key{all}. Any exception raised by Process.@key{all} is propagated.]}


@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@key[type] Constant_Reference_Type
      (Element : @key[not null access constant] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
        Nonblocking, Global => @key[in out synchronized],
        Default_Initial_Condition => (@key[raise] Program_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[The type Constant_Reference_Type
needs finalization.@PDefn2{Term=<needs finalization>,
Sec=<language-defined type>}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[DeletedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[The default
initialization of an object of type
Constant_Reference_Type propagates Program_Error.]}]}

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
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] Set;
                             Position  : @key[in] Cursor)
   @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
   @key[with] Pre  => (Position /= No_Element
                    @key[or else raise] Constraint_Error) @key[and then]
                (Has_Element (Container, Position)
                    @key[or else raise] Program_Error),
        Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to
gain read access to an individual element of a set given a cursor.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then Constraint_Error
is propagated; if Position does not designate an element in Container, then
Program_Error is propagated. Otherwise, ]}Constant_Reference returns an object
whose discriminant is an access value that designates the element designated by
Position. Tampering with the @Chg{Version=[5],New=[cursors],Old=[elements]}
of Container is prohibited while the
object returned by Constant_Reference exists and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Assign (Target : @key{in out} Set; Source : @key{in} Set)@Chg{Version=[5],New=[
   @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Target)
                   @key[or else raise] Program_Error,
        Post => Length (Source) = Length (Target)],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Move (Target : @key{in out} Set;
                Source : @key{in out} Set)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Target)
                    @key{or else raise} Program_Error) @key{and then}
                (@key{not} Tampering_With_Cursors_Prohibited (Source)
                    @key{or else raise} Program_Error),
        Post => (@key{if} Target = Source @key{then} True
                 @key{else}
                    Length (Target) = Length (Source'Old) @key{and then}
                    Length (Source) = 0)],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Set;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Elements_Prohibited (Container)
                   @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Count_Type'Last - 1
                    @key{or else raise} Constraint_Error),
        Post => (@key{declare}
                   Original_Length : @key{constant} Count_Type :=
                      Length (Container)'Old;
                 @key{begin}
                   Has_Element (Container, Position) @key{and then}
                  (@key{if} Inserted then
                     Length (Container) = Original_Length + 1
                   @key{else}
                     Length (Container) = Original_Length))],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Set;
                  New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Count_Type'Last - 1
                    @key{or else raise} Constraint_Error),
        Post => Length (Container) = Length (Container)'Old + 1],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Include (Container : @key{in out} Set;
                   New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Count_Type'Last - 1
                    @key{or else raise} Constraint_Error),
        Post => (@key{declare}
                   Original_Length : @key{constant} Count_Type :=
                      Length (Container)'Old;
                 @key{begin}
                   Length (Container)
                      @key{in} Original_Length | Original_Length + 1)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Include inserts New_Item into
Container as per the four-parameter Insert, with the difference that if an
element equivalent to New_Item is already in the set, then it is replaced. Any
exception raised during assignment is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace (Container : @key{in out} Set;
                   New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => Length (Container) = Length (Container)'Old],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Replace checks if an element
equivalent to New_Item is already in the set. If a match is found, that element
is replaced with New_Item; otherwise, Constraint_Error is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Exclude (Container : @key{in out} Set;
                   Item       : @key{in}     Element_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => (@key{declare}
                   Original_Length : @key{constant} Count_Type :=
                      Length (Container)'Old;
                 @key{begin}
                   Length (Container) @key{in}
                      Original_Length - 1 | Original_Length)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Exclude checks if an element
equivalent to Item is present in Container. If a match is found, Exclude
removes the element from the set.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} Set;
                  Item       : @key{in}     Element_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => Length (Container) = Length (Container)'Old - 1],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Delete checks if an element
equivalent to Item is present in Container. If a match is found, Delete removes
the element from the set; otherwise, Constraint_Error is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete (Container : @key{in out} Set;
                  Position  : @key{in out} Cursor)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error) @key{and then}
                (Position /= No_Element 
                    @key{or else raise} Constraint_Error) @key{and then}
                (Has_Element (Container, Position)
                    @key{or else raise} Program_Error),
        Post => Length (Container) = Length (Container)'Old - 1 @key{and then}
                Position = No_Element],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element,
then Constraint_Error is propagated. If Position does not designate an element
in Container, then Program_Error is propagated. Otherwise, ]}Delete removes the
element designated by Position from the set.@Chg{Version=[5],New=[],Old=[
Position is set to No_Element on return.]}]}

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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Union (Target : @key{in out} Set;
                 Source : @key{in}     Set)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Target)
                    @key{or else raise} Program_Error,
        Post => Length (Target) <= Length (Target)'Old + Length (Source)],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Union (Left, Right : Set) @key{return} Set@Chg{Version=[5],New=[
   @key{with} Post => Length (Union'Result) <= Length (Left) + Length (Right) @key{and then}
                @key{not} Tampering_With_Cursors_Prohibited (Union'Result)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a set comprising all of the
elements of Left, and the elements of Right that are not equivalent to some
element of Left.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Intersection (Target : @key{in out} Set;
                        Source : @key{in}     Set)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Target)
                    @key{or else raise} Program_Error,
        Post => Length (Target) <= Length (Target)'Old + Length (Source)],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Intersection (Left, Right : Set) @key{return} Set@Chg{Version=[5],New=[
   @key{with} Post => Length (Intersection'Result) <= Length (Left) + Length (Right) @key{and then}
                @key{not} Tampering_With_Cursors_Prohibited (Intersection'Result)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a set comprising all the
elements of Left that are equivalent to the some element of Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Difference (Target : @key{in out} Set;
                      Source : @key{in}     Set)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Target)
                    @key{or else raise} Program_Error,
        Post => Length (Target) <= Length (Target)'Old + Length (Source)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Target denotes the same object
as Source, then Difference clears Target. Otherwise, it deletes from Target the
elements that are equivalent to some element of Source.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Difference (Left, Right : Set) @key{return} Set@Chg{Version=[5],New=[
   @key{with} Post => Length (Difference'Result) <= Length (Left) + Length (Right) @key{and then}
                @key{not} Tampering_With_Cursors_Prohibited (Difference'Result)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a set comprising the
elements of Left that are not equivalent to some element of Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Symmetric_Difference (Target : @key{in out} Set;
                                Source : @key{in}     Set)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Target)
                    @key{or else raise} Program_Error,
        Post => Length (Target) <= Length (Target)'Old + Length (Source)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Target denotes the same object
as Source, then Symmetric_Difference clears Target. Otherwise, it deletes from
Target the elements that are equivalent to some element of Source, and inserts
into Target the elements of Source that are not equivalent to some element of
Target.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Symmetric_Difference (Left, Right : Set) @key{return} Set@Chg{Version=[5],New=[
   @key{with} Post => Length (Symmetric_Difference'Result) <= Length (Left) + Length (Right) @key{and then}
                @key{not} Tampering_With_Cursors_Prohibited (Symmetric_Difference'Result)],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First (Container : Set) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Post => (@key{if not} Is_Empty (Container)
                 @key{then} Has_Element (Container, First'Result)
                 @key{else} First'Result = No_Element)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Length (Container) = 0, then
First returns No_Element. Otherwise, First returns a cursor that designates the
first element in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Next (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{in all}, @key{use null}),
        Post => (@key{if} Position = No_Element @key{then} Next'Result = No_Element
                 @key{else} True)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a cursor that designates the successor of the element
designated by Position. If Position designates the last element, then
No_Element is returned. If Position equals No_Element, then No_Element is
returned.]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Next (Container : Set;
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
successor of the node designated by Position in Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Next (Position : @key{in out} Cursor)@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{in all}, @key{use null})],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Next (Position).]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{procedure} Next (Container : @key{in}     Set;
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

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0004-1]}
@ChgDeleted{Version=[3],Type=[Trailing],Text=[@Chg{Version=[2],New=[Equivalent to Find (Container, Item) /= No_Element.],Old=[]}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Find (Container : Set;
               Item      : Element_Type) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Post => (@key{if} Find'Result = No_Element @key{then} True
                    @key{else} Has_Element (Container, Find'Result))],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Iterate
  (Container : @key{in} Set;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
   @key{with} Allows_Exit],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0265-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Iterate calls Process.@key{all}
with a cursor that designates each element in Container, starting with the
first element and moving the cursor according to the successor relation.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the cursors of Container@Chg{Version=[3],New=[ is prohibited during the
execution of a call on Process.@key{all}],Old=[]}. Any exception raised by
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Key (Position : Cursor) @key{return} Key_Type@Chg{Version=[5],New=[
   @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Key (Element (Position)).]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Key (Container : Set;
              Position : Cursor) @key{return} Key_Type
   @key{with} Pre  => (Position /= No_Element 
                   @key{or else raise} Constraint_Error) @key{and then}
                (Has_Element (Container, Position)
                   @key{or else raise} Program_Error);]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Equivalent to
Key (Element (Container, Position)).]}

@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The subprograms in package Generic_Keys named
Contains, Find, Element, Delete, and Exclude, are equivalent to the
corresponding subprograms in the parent package, with the difference that the
Key parameter is used to locate an element in the set.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Replace (Container : @key{in out} Set;
                   Key       : @key{in}     Key_Type;
                   New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => Length (Container) = Length (Container)'Old],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Replace_Element (Container, Find (Container, Key), New_Item).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Update_Element_Preserving_Key
  (Container : @key{in out} Set;
   Position  : @key{in}     Cursor;
   Process   : @key{not null access procedure}
                                 (Element : @key{in out} Element_Type))@Chg{Version=[5],New=[
   @key{with} Pre  => (Position /= No_Element 
                    @key{or else raise} Constraint_Error) @key{and then}
                 (Has_Element (Container, Position) 
                    @key{or else raise} Program_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then
Constraint_Error is propagated; if Position does not designate an element in
Container, then Program_Error is propagated. Otherwise,
]}Update_@!Element_@!Preserving_Key uses Key to save the key value @i<K> of the
element designated by Position. Update_@!Element_@!Preserving_Key then calls
Process.@key{all} with that element as the argument.
@Chg{Version=[3],New=[Tampering],Old=[Program_Error
is propagated if Process.@key{all} tampers]}
with the @Chg{Version=[5],New=[cursors],Old=[elements]}
of Container@Chg{Version=[3],New=[ is prohibited during the
execution of the call on Process.@key{all}],Old=[]}. Any
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
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[type] Reference_Type (Element : @key[not null access] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
        Nonblocking, Global => @key[in out synchronized],
        Default_Initial_Condition => (@key[raise] Program_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[The type Reference_Type needs
finalization.@PDefn2{Term=<needs finalization>,
Sec=<language-defined type>}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[DeletedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[The default
initialization of an object of type
Reference_Type propagates Program_Error.]}]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference_Preserving_Key (Container : @key[aliased in out] Set;
                                   Position  : @key[in] Cursor)
   @key[return] Reference_Type@Chg{Version=[5],New=[
   @key[with] Pre  => (Position /= No_Element
                    @key[or else raise] Constraint_Error) @key[and then]
                (Has_Element (Container, Position)
                    @key[or else raise] Program_Error),
        Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Implicit_Dereference aspect) provides a convenient way to
gain read and write access to an individual element of a set given a cursor.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[If
Position equals No_Element, then Constraint_Error
is propagated; if Position does not designate an element in Container, then
Program_Error is propagated. Otherwise, ]}Reference_Preserving_Key uses Key to
save the key value @i<K>; then returns an object whose discriminant is an access
value that designates the element designated by
Position. Tampering with the @Chg{Version=[5],New=[cursors],Old=[elements]}
of Container is prohibited while the
object returned by Reference_Preserving_Key exists and has not been finalized.
When the object returned by Reference_Preserving_Key is finalized, a check is
made if @i<K> determines the same equivalence class as that for the new element;
if not, the element is removed from the set and Program_Error is propagated.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] Set;
                             Key       : @key[in] Key_Type)
   @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
   @key[with] Pre  => Find (Container, Key) /= No_Element @key[or else raise] Constraint_Error,
        Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Implicit_Dereference aspect) provides a convenient way to
gain read access to an individual element of a set given a key value.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Equivalent to
Constant_Reference (Container, Find (Container, Key)).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference_Preserving_Key (Container : @key[aliased in out] Set;
                                   Key       : @key[in] Key_Type)
   @key[return] Reference_Type@Chg{Version=[5],New=[
   @key[with] Pre  => Find (Container, Key) /= No_Element @key[or else raise] Constraint_Error,
        Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Implicit_Dereference aspect) provides a convenient way to gain
read and write access to an individual element of a set given a key value.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Equivalent to Reference_Preserving_Key (Container, Find (Container, Key)).]}
@end{DescribeCode}


@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[The nested package Stable
provides a type Stable.Set that represents
a @i<stable> set,@Defn2{Term=(stable),Sec=(set)} which is one that
cannot grow and shrink. Such a list can be created by calling the
Copy function, or by establishing a
@i<stabilized view> of a regular set.@Defn2{Term=[stabilized view],Sec=[set]}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Type=[Leading],Text=[The subprograms of the set package
that have a parameter or result of type Set
are included in the nested package Stable with the same specification, except
that the following are omitted:]}

@begin{Indent}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],Text=[Tampering_With_Cursors_Prohibited,
Assign, Move, Insert, Include, Clear, Delete, Exclude, Replace, Replace_Element,
procedures Union, Intersection, Difference, and Symmetric_Difference,
(for Ordered_sets) Delete_First and
Delete_Last, and (for Hashed_sets) Reserve_Capacity]}
@end{Indent}

@begin{Discussion}
  @ChgRef{Version=[5],Kind=[Added]}
  @ChgAdded{Version=[5],Text=[The Generic_Keys package is not included in the
    Stable package. The functions Union, Intersection, Difference, and
    Symmetric_Difference are included in the Stable package.]}
@end{Discussion}
@begin{Ramification}
  @ChgRef{Version=[5],Kind=[Added]}
  @ChgAdded{Version=[5],Text=[The names Set and Cursor mean the types
    declared in the nested package in these subprogram specifications.]}
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[5],Kind=[Added]}
  @ChgAdded{Version=[5],Text=[The omitted routines are those that tamper with
    cursors (or test that state). The model is that it is
    impossible to tamper with cursors of a stable view since no
    such operations are included. Thus tampering checks are not needed for
    a stable view, and we omit the operations associated with those checks.]}
@end{Reason}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[The operations of this package are equivalent
to those for regular sets, except that the calls to
Tampering_With_Cursors_Prohibited that occur in preconditions are replaced
by False, and any that occur in postconditions are replaced by True.]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[If a stable set is declared with the Base
discriminant designating a pre-existing regular set, the stable set
represents a stabilized view of the underlying regular set, and any operation
on the stable set is reflected on the underlying regular set. While a
stabilized view exists, any operation that tampers with cursors performed on
the underlying set is prohibited. The finalization of a stable set that
provides such a view removes this restriction on the underlying regular set
@Redundant[(though some other restriction might exist due to other concurrent
iterations or stabilized views)].]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
@ChgAdded{Version=[5],Text=[If a stable set is declared without specifying
Base, the object must be initialized. The initializing expression of the stable
set, @Redundant[typically a call on Copy], determines the Length
of the set. The Length of a stable set never changes after
initialization.]}

@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0022-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error for the actual function
associated with a generic formal subprogram, when called as part of an
operation of a set package, to tamper with elements of any set parameter of
the operation. Either Program_Error is raised, or the operation works as
defined on the value of the set either prior to, or subsequent to, some or
all of the modifications to the set.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0027-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to call any subprogram
declared in the visible part of a set package
when the associated container has been finalized. If the operation takes
Container as an @key[in out] parameter, then it raises Constraint_Error or
Program_Error. Otherwise, the operation either proceeds as it would
for an empty container, or it raises
Constraint_Error or@Defn2{Term=[Constraint_Error],Sec=(raised by detection of a bounded error)}
Program_Error.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}
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
  finalization of the object of Reference_Type will try to access a nonexistent
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
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0298-1]}
  @ChgAdded{Version=[2],Text=[An assignment of a Set is a @lquotes@;deep@rquotes
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

@begin{Inconsistent2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1]}
  @ChgAdded{Version=[5],Text=[@Defn{inconsistencies with Ada 2012}Procedures
  Union, Intersection, Difference, and Symmeteric_Difference are now defined
  to tamper with the cursors of the Target parameter. A program which attempts
  to use one of these operations while tampering is prohibited will raise
  Program_Error. However, since the operations do modify the container, the
  effects would have been unpredictable, so this change will likely fix
  bugs.]}
@end{Inconsistent2012}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0196-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}@b{Correction:}
  Replace_Element is now defined such that it can be used
  concurrently so long as it operates on different elements. This allows
  some container operations to be used in parallel without separate
  synchronization.]}
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


@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Hashed_Sets]}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
package Containers.Hashed_Sets has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0084-1],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[@key[with] Ada.Iterator_Interfaces;
],Old=[]}@key{generic}
   @key{type} Element_Type @key{is private};
   @key{with function} Hash (Element : Element_Type) @key{return} Hash_Type;
   @key{with function} Equivalent_Elements (Left, Right : Element_Type)
                 @key{return} Boolean;
   @key{with function} "=" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Hashed_Sets@Chg{Version=[5],New=[],Old=[ @key{is}]}@ChildUnit{Parent=[Ada.Containers],Child=[Hashed_Sets]}@Chg{Version=[5],New=[
   @key[with] Preelaborate, Remote_Types,
        Nonblocking, Global => @key[null] @key[is]],Old=[
   @key{pragma} Preelaborate(Hashed_Sets);@Chg{Version=[3],New=[
   @key{pragma} Remote_Types(Hashed_Sets);],Old=[]}]}]}

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
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Set} @key{is tagged private}@Chg{Version=[3],New=[
      @key[with] Constant_Indexing => Constant_Reference,
           Default_Iterator  => Iterate,
           Iterator_Element  => Element_Type],Old=[]}@Chg{Version=[5],New=[,
           Iterator_View     => Stable.Set,
           Aggregate         => (Empty       => Empty,
                                 Add_Unnamed => Include),
           Stable_Properties => (Length,
                                 Tampering_With_Cursors_Prohibited),
           Default_Initial_Condition =>
              Length (Set) = 0 @key{and then}
              (@key{not} Tampering_With_Cursors_Prohibited (Set))],Old=[]};
   @key{pragma} Preelaborable_Initialization(Set);]}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
   @ChgAdded{Version=[5],Text=[Unlike a Vector, the Stable_Properties of a
   Hashed_Set do not include the Capacity. If we had included it, some of the
   otherwise shared definitions would need different postconditions for
   Hashed_Sets and Ordered_Sets. If we were starting these containers from
   scratch, we probably would have approached the sharing of definitions
   differently so that we could avoid issues like this, but a major
   reorganization of this existing material would be too much change.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Cursor} @key{is private};
   @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Empty_Set} : @key{constant} Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[in all], @key[use null])],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Has_Element} (Container : Set; Position : Cursor)
      @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[package] @AdaPackDefn{Set_Iterator_Interfaces} @key[is new]
       Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "=" (Left, Right : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Sets} (Left, Right : Set) @key{return} Boolean;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Tampering_With_Cursors_Prohibited}
      (Container : Set) @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0339-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Empty} (Capacity : Count_Type := @RI{implementation-defined})
      @key[return] Set
      @key[with] Post =>
         Capacity (Empty'Result) >= Capacity @key[and then]
         @key[not] Tampering_With_Cursors_Prohibited (Empty'Result) @key[and then]
         Length (Empty'Result) = 0;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{To_Set} (New_Item : Element_Type) @key{return} Set@Chg{Version=[5],New=[
      @key[with] Post => Length (To_Set'Result) = 1 @key{and then}
                 @key{not} Tampering_with_Cursors_Prohibited (To_Set'Result)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Capacity} (Container : Set) @key{return} Count_Type@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reserve_Capacity} (Container : @key{in out} Set;
                               Capacity  : @key{in}     Count_Type)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                    @key[or else raise] Program_Error,
           Post => Container.Capacity >= Capacity],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Length} (Container : Set) @key{return} Count_Type@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Empty} (Container : Set) @key{return} Boolean@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[null], @key[use null]),
           Post => Is_Empty'Result = (Length (Container) = 0)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Clear} (Container : @key{in out} Set)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                       @key[or else raise] Program_Error,
           Post => Capacity (Container) = Capacity (Container)'Old @key{and then}
                   Length (Container) = 0],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Element} (Position : Cursor) @key{return} Element_Type@Chg{Version=[5],New=[
      @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
           Nonblocking => Element_Type'Nonblocking,
           Global => (@key{in all}, @key{use} Element_Type)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Element} (Container : Set;
                     Position  : Cursor) @key{return} Element_Type
      @key{with} Pre  => (Position /= No_Element
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position)
                       @key{or else raise} Program_Error),
           Nonblocking => Element_Type'Nonblocking,
           Global => (@key{null}, @key{use} Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} Set;
                              Position  : @key{in}     Cursor;
                              New_item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
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
     (Container : @key{in} Set;
      Position  : @key{in} Cursor;
      Process   : @key{not null access procedure} (Element : @key{in} Element_Type))
      @key{with} Pre  => (Position /= No_Element 
                       @key{or else raise} Constraint_Error) @key{and then}
                    (Has_Element (Container, Position) 
                       @key{or else raise} Program_Error);]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[type] Constant_Reference_Type
         (Element : @key[not null access constant] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
           Nonblocking, Global => @key[in out synchronized],
           Default_Initial_Condition => (@key[raise] Program_Error)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Set;
                                Position  : @key[in] Cursor)
      @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre  => (Position /= No_Element
                       @key[or else raise] Constraint_Error) @key[and then]
                   (Has_Element (Container, Position)
                       @key[or else raise] Program_Error),
           Post => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} Set; Source : @key{in} Set)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Target)
                      @key[or else raise] Program_Error,
           Post => Length (Source) = Length (Target) @key[and then]
                   Capacity (Target) >= Length (Source)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Copy} (Source : Set; Capacity : Count_Type := 0)
      @key[return] Set@Chg{Version=[5],New=[
      @key[with] Pre  => Capacity = 0 @key[or else] Capacity >= Length (Source)
                    @key[or else raise] Capacity_Error,
           Post =>
              Length (Copy'Result) = Length (Source) @key[and then]
              @key[not] Tampering_With_Cursors_Prohibited (Copy'Result) @key[and then]
              Copy'Result.Capacity = (if Capacity = 0 then
                 Length (Source) else Capacity)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Move} (Target : @key{in out} Set;
                   Source : @key{in out} Set)@Chg{Version=[5],New=[
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
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Set;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      Has_Element (Container, Position) @key{and then}
                     (@key{if} Inserted then
                        Length (Container) = Original_Length + 1
                      @key{else}
                        Length (Container) = Original_Length)) @key{and then}
                    Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Set;
                     New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => Length (Container) = Length (Container)'Old + 1 @key{and then}
                   Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Include} (Container : @key{in out} Set;
                      New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      Length (Container)
                         @key{in} Original_Length | Original_Length + 1) @key{and then}
                    Capacity (Container) >= Length (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace} (Container : @key{in out} Set;
                      New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error,
           Post => Length (Container) = Length (Container)'Old],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Exclude} (Container : @key{in out} Set;
                      Item      : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error,
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      Length (Container) @key{in}
                         Original_Length - 1 | Original_Length)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Set;
                     Position  : @key{in out} Cursor)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                       @key{or else raise} Program_Error) @key{and then}
                   (Position /= No_Element 
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position)
                       @key{or else raise} Program_Error),
           Post => Length (Container) = Length (Container)'Old - 1 @key{and then}
                   Position = No_Element],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} Union (Target : @key{in out} Set;
                    Source : @key{in}     Set)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Target)
                      @key{or else raise} Program_Error,
           Post => Length (Target) <= Length (Target)'Old + Length (Source)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} Union (Left, Right : Set) @key{return} Set@Chg{Version=[5],New=[
      @key{with} Post => Length (Union'Result) <= Length (Left) + Length (Right) @key{and then}
                   @key{not} Tampering_With_Cursors_Prohibited (Union'Result)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "@key{or}" (Left, Right : Set) @key{return} Set @key{renames} Union;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} Intersection (Target : @key{in out} Set;
                           Source : @key{in}     Set)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Target)
                      @key{or else raise} Program_Error,
           Post => Length (Target) <= Length (Target)'Old + Length (Source)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} Intersection (Left, Right : Set) @key{return} Set@Chg{Version=[5],New=[
      @key{with} Post => Length (Intersection'Result) <= Length (Left) + Length (Right) @key{and then}
                   @key{not} Tampering_With_Cursors_Prohibited (Intersection'Result)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "@key{and}" (Left, Right : Set) @key{return} Set @key{renames} Intersection;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} Difference (Target : @key{in out} Set;
                         Source : @key{in}     Set)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Target)
                      @key{or else raise} Program_Error,
           Post => Length (Target) <= Length (Target)'Old + Length (Source)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} Difference (Left, Right : Set) @key{return} Set@Chg{Version=[5],New=[
      @key{with} Post => Length (Difference'Result) <= Length (Left) + Length (Right) @key{and then}
                   @key{not} Tampering_With_Cursors_Prohibited (Difference'Result)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "-" (Left, Right : Set) @key{return} Set @key{renames} Difference;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} Symmetric_Difference (Target : @key{in out} Set;
                                   Source : @key{in}     Set)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Target)
                      @key{or else raise} Program_Error,
           Post => Length (Target) <= Length (Target)'Old + Length (Source)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} Symmetric_Difference (Left, Right : Set) @key{return} Set@Chg{Version=[5],New=[
      @key{with} Post => Length (Symmetric_Difference'Result) <= Length (Left) + Length (Right) @key{and then}
                   @key{not} Tampering_With_Cursors_Prohibited (Symmetric_Difference'Result)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "@key{xor}" (Left, Right : Set) @key{return} Set
     @key{renames} Symmetric_Difference;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Overlap} (Left, Right : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Subset} (Subset : Set;
                       Of_Set : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First} (Container : Set) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Post => (@key{if not} Is_Empty (Container)
                    @key{then} Has_Element (Container, First'Result)
                    @key{else} First'Result = No_Element)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Next} (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{in all}, @key{use null}),
           Post => (@key{if} Position = No_Element @key{then} Next'Result = No_Element
                    @key{else} True)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Next} (Container : Set;
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
      @key{with} Nonblocking, Global => (@key{in all}, @key{use null})],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Next} (Container : @key{in}     Set;
                   Position  : @key{in out} Cursor)
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Position /= No_Element
                    @key{then} Has_Element (Container, Position));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Find} (Container : Set;
                  Item      : Element_Type)
      @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Post => (@key{if} Find'Result = No_Element @key{then} True
                    @key{else} Has_Element (Container, Find'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Contains} (Container : Set;
                      Item      : Element_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Elements} (Left, Right : Cursor)
      @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => (Left /= No_Element @key{and then} Right /= No_Element)
                         @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Elements} (Left  : Cursor;
                                 Right : Element_Type)
      @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => Left /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Elements} (Left  : Element_Type;
                                 Right : Cursor)
      @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => Right /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Iterate}
     (Container : @key{in} Set;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
      @key{with} Allows_Exit],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[   @key[function] Iterate (Container : @key[in] Set)
      @key[return] Set_Iterator_Interfaces.@Chg{Version=[5],New=[Parallel_Iterator],Old=[Forward_Iterator]}'Class@Chg{Version=[5],New=[
      @key[with] Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{generic}
      @key{type} Key_Type (<>) @key{is private};
      @key{with function} Key (Element : Element_Type) @key{return} Key_Type;
      @key{with function} Hash (Key : Key_Type) @key{return} Hash_Type;
      @key{with function} Equivalent_Keys (Left, Right : Key_Type)
                                     @key{return} Boolean;
   @key{package} @AdaPackDefn{Generic_Keys}@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => @key[null]],Old=[]} @key{is}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{function} @AdaSubDefn{Key} (Position : Cursor) @key{return} Key_Type@Chg{Version=[5],New=[
         @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
              Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Key} (Container : Set;
                    Position : Cursor) @key{return} Key_Type
         @key{with} Pre  => (Position = No_Element
                         @key{or else raise} Constraint_Error) @key{and then}
                      (Has_Element (Container, Position)
                         @key{or else raise} Program_Error);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{function} @AdaSubDefn{Element} (Container : Set;
                        Key       : Key_Type)
        @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Replace} (Container : @key{in out} Set;
                         Key       : @key{in}     Key_Type;
                         New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
         @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                          @key{or else raise} Program_Error,
              Post => Length (Container) = Length (Container)'Old],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Exclude} (Container : @key{in out} Set;
                         Key       : @key{in}     Key_Type)@Chg{Version=[5],New=[
         @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                          @key{or else raise} Program_Error,
              Post => (@key{declare}
                         Original_Length : @key{constant} Count_Type :=
                            Length (Container)'Old;
                       @key{begin}
                         Length (Container)
                            @key{in} Original_Length - 1 | Original_Length)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Set;
                        Key       : @key{in}     Key_Type)@Chg{Version=[5],New=[
         @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                          @key{or else raise} Program_Error,
              Post => Length (Container) = Length (Container)'Old - 1],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{function} @AdaSubDefn{Find} (Container : Set;
                     Key       : Key_Type)
         @key{return} Cursor@Chg{Version=[5],New=[
         @key{with} Post => (@key{if} Find'Result = No_Element @key{then} True
                       @key{else} Has_Element (Container, Find'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key{function} @AdaSubDefn{Contains} (Container : Set;
                         Key       : Key_Type)
         @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Update_Element_Preserving_Key}
        (Container : @key{in out} Set;
         Position  : @key{in}     Cursor;
         Process   : @key{not null access procedure}
                         (Element : @key{in out} Element_Type))@Chg{Version=[5],New=[
         @key{with} Pre  => (Position /= No_Element @key{or else}
                          @key{raise} Constraint_Error) @key{and then}
                       (Has_Element (Container, Position) @key{or else raise} Program_Error)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[      @key[type] @AdaTypeDefn{Reference_Type}
            (Element : @key[not null access] Element_Type) @key[is private]
         @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
              Nonblocking, Global => @key[in out synchronized],
              Default_Initial_Condition => (@key[raise] Program_Error)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[      @key[function] @AdaSubDefn{Reference_Preserving_Key} (Container : @key[aliased in out] Set;
                                         Position  : @key[in] Cursor)
         @key[return] Reference_Type@Chg{Version=[5],New=[
         @key[with] Pre  => (Position /= No_Element
                          @key[or else raise] Constraint_Error) @key[and then]
                      (Has_Element (Container, Position)
                          @key[or else raise] Program_Error),
              Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[      @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Set;
                                   Key       : @key[in] Key_Type)
         @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
         @key[with] Pre  => Find (Container, Key) /= No_Element @key[or else]
                         @key[raise] Constraint_Error,
              Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[      @key[function] @AdaSubDefn{Reference_Preserving_Key} (Container : @key[aliased in out] Set;
                                         Key       : @key[in] Key_Type)
         @key[return] Reference_Type@Chg{Version=[5],New=[
         @key[with] Pre  => Find (Container, Key) /= No_Element @key[or else]
                         @key[raise] Constraint_Error,
              Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{end} Generic_Keys;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[   @key{package} @AdaPackDefn{Stable} @key{is}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1],ARef=[AI12-0339-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Set} (Base : @key{not null access} Hashed_Sets.Set) @key{is}
         @key{tagged limited private}
         @key{with} Constant_Indexing => Constant_Reference,
              Default_Iterator  => Iterate,
              Iterator_Element  => Element_Type,
              Aggregate         => (Empty       => Empty,
                                    Add_Unnamed => Include),
              Stable_Properties => (Length),
              Global            => @key[null],
              Default_Initial_Condition => Length (Set) = 0;
      @key{pragma} Preelaborable_Initialization(Set);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Cursor} @key{is private};
      @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @AdaObjDefn{Empty_Set} : @key{constant} Set;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean
         @key{with} Nonblocking, Global => (@key{in all}, @key{use null});]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{package} @AdaPackDefn{Set_Iterator_Interfaces} @key[is new]
         Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} Hashed_Sets.Set;
                        Source : @key{in} Set)
         @key{with} Post => Length (Source) = Length (Target);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Copy} (Source : Hashed_Sets.Set) @key{return} Set
         @key{with} Post => Length (Copy'Result) = Length (Source);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Constant_Reference_Type}
            (Element : @key{not null access constant} Element_Type) @key{is private}
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
function "=" returns True for any pair of nonequivalent elements, then the
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
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Empty (Capacity : Count_Type := @RI{implementation-defined})
   @key[return] Set
   @key[with] Post =>
      Capacity (Empty'Result) >= Capacity @key[and then]
      @key[not] Tampering_With_Cursors_Prohibited (Empty'Result) @key[and then]
      Length (Empty'Result) = 0;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI12-0339-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns an empty set.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Capacity (Container : Set) @key{return} Count_Type@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the capacity of Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reserve_Capacity (Container : @key{in out} Set;
                            Capacity  : @key{in}     Count_Type)@Chg{Version=[5],New=[
   @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                 @key[or else raise] Program_Error,
        Post => Container.Capacity >= Capacity],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Deleted],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Type=[Trailing],
Text=[@Chg{Version=[5],New=[],Old=[Reserve_Capacity tampers with the
cursors of Container.]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Trailing],Text=[Reserve_Capacity tampers with the
  cursors, as rehashing probably will change the relationships of the elements
  in Container.]}
@end{Reason}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Clear (Container : @key{in out} Set)@Chg{Version=[5],New=[
   @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Container)
                    @key[or else raise] Program_Error,
        Post => Capacity (Container) = Capacity (Container)'Old @key{and then}
                Length (Container) = 0],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[In addition to the semantics
described in @RefSecNum{Sets}, Clear does not affect the capacity of
Container.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[procedure] Assign (Target : @key[in out] Set; Source : @key[in] Set)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Target)
                      @key[or else raise] Program_Error,
           Post => Length (Source) = Length (Target) @key[and then]
                   Capacity (Target) >= Length (Source)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[In addition to the semantics
described in @RefSecNum{Sets}, if the length of Source is greater than the
capacity of Target, Reserve_Capacity (Target, Length (Source)) is called
before assigning any elements.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Copy (Source : Set; Capacity : Count_Type := 0)
   @key[return] Set@Chg{Version=[5],New=[
   @key[with] Pre  => Capacity = 0 @key[or else] Capacity >= Length (Source)
                 @key[or else raise] Capacity_Error,
        Post =>
           Length (Copy'Result) = Length (Source) @key[and then]
           @key[not] Tampering_With_Cursors_Prohibited (Copy'Result) @key[and then]
           Copy'Result.Capacity = (if Capacity = 0 then
              Length (Source) else Capacity)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a set whose elements are
initialized from the elements of Source.@Chg{Version=[5],New=[],Old=[  If
Capacity is 0, then the set capacity
is the length of Source; if Capacity is equal to or greater than the length of
Source, the set capacity is at least the specified value. Otherwise, the
operation propagates Capacity_Error.]}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Insert (Container : @key{in out} Set;
                  New_Item  : @key{in}     Element_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean)@Chg{Version=[5],New=[
   @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                   @key{or else raise} Program_Error) @key{and then}
                (Length (Container) <= Count_Type'Last - 1
                    @key{or else raise} Constraint_Error),
        Post => (@key{declare}
                   Original_Length : @key{constant} Count_Type :=
                      Length (Container)'Old;
                 @key{begin}
                   Has_Element (Container, Position) @key{and then}
                  (@key{if} Inserted then
                     Length (Container) = Original_Length + 1
                   @key{else}
                     Length (Container) = Original_Length)) @key{and then}
                 Capacity (Container) >= Length (Container)],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Equivalent_Elements (Left, Right : Cursor)
   @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => (Left /= No_Element @key{and then} Right /= No_Element)
                      @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Equivalent_Elements
(Element (Left), Element (Right)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Equivalent_Elements (Left  : Cursor;
                              Right : Element_Type) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => Left /= No_Element @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Equivalent_Elements
(Element (Left), Right).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Equivalent_Elements (Left  : Element_Type;
                              Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => Right /= No_Element @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Equivalent_Elements (Left, Element (Right)).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] Set)
   @key[return] Set_Iterator_Interfaces.@Chg{Version=[5],New=[Parallel_Iterator],Old=[Forward_Iterator]}'Class@Chg{Version=[5],New=[
   @key[with] Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1],ARef=[AI05-0269-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate returns
an iterator object (see @RefSecNum{User-Defined Iterator Types}) that
will generate a value for a loop parameter
(see @RefSecNum{Generalized Loop Iteration}) designating
each element in Container, starting with the first element and moving the cursor
according to the successor relation@Chg{Version=[5],New=[ when used as a forward
iterator, and processing all nodes concurrently when used as a parallel
iterator],Old=[]}.
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
  are added to Containers.Hashed_Sets. If an instance of Containers.Hashed_Sets
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

@begin{Incompatible2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1],ARef=[AI12-0112-1],ARef=[AI12-0339-1]}
  @ChgAdded{Version=[5],Text=[@Defn{incompatibilities with Ada 2012}A number of
  new subprograms, types, and even a nested package were added to
  Containers.Hashed_Sets to better support contracts and stable views. If an
  instance of Containers.Hashed_Sets
  is referenced in a @nt{use_clause}, and an entity @i<E> with the same
  @nt{defining_identifier} as a new entity in Containers.Hashed_Sets is
  defined in a package that is also referenced in a @nt{use_clause}, the
  entity @i<E> may no longer be use-visible, resulting in errors. This should
  be rare and is easily fixed if it does occur.]}
@end{Incompatible2012}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0212-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}Sets now support
  positional container aggregates, so @nt{aggregate} syntax can be used to
  create Sets.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0266-1]}
  @ChgAdded{Version=[5],Text=[The iterator for the
  container now can return a parallel iterator which can be used to
  process the container in parallel.]}
@end{Extend2012}

@begin{DiffWord2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[Added contracts to this container. This includes
  describing some of the semantics with pre- and postconditions, rather than
  English text. Note that the preconditions can be Suppressed (see
  @RefSecNum{Suppressing Checks}).]}
@end{DiffWord2012}


@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Ordered_Sets]}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
package Containers.Ordered_Sets has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0084-1],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[@key[with] Ada.Iterator_Interfaces;
],Old=[]}@key{generic}
   @key{type} Element_Type @key{is private};
   @key{with function} "<" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
   @key{with function} "=" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Ordered_Sets@Chg{Version=[5],New=[],Old=[ @key{is}]}@ChildUnit{Parent=[Ada.Containers],Child=[Ordered_Sets]}@Chg{Version=[5],New=[
   @key[with] Preelaborate, Remote_Types,
        Nonblocking, Global => @key[null] @key[is]],Old=[
   @key{pragma} Preelaborate(Ordered_Sets);@Chg{Version=[3],New=[
   @key{pragma} Remote_Types(Ordered_Sets);],Old=[]}]}]}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
   @ChgAdded{Version=[5],Text=[For discussion on the reasons and meaning of
   the specifications of the Global and Nonblocking aspects in this generic
   package, see the notes on the equivalent operations in the specification
   of the Containers.Vectors package (see 
   @RefSecNum{The Generic Package Containers.Vectors}).]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Elements} (Left, Right : Element_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0111-1],ARef=[AI12-0112-1],ARef=[AI12-0212-1],ARef=[AI12-0339-1]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Set} @key{is tagged private}@Chg{Version=[3],New=[
      @key[with] Constant_Indexing => Constant_Reference,
           Default_Iterator  => Iterate,
           Iterator_Element  => Element_Type],Old=[]}@Chg{Version=[5],New=[,
           Iterator_View     => Stable.Set,
           Aggregate         => (Empty       => Empty,
                                 Add_Unnamed => Include),
           Stable_Properties => (Length,
                                 Tampering_With_Cursors_Prohibited),
           Default_Initial_Condition =>
              Length (Set) = 0 @key{and then}
              (@key{not} Tampering_With_Cursors_Prohibited (Set))],Old=[]};
   @key{pragma} Preelaborable_Initialization(Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Cursor} @key{is private};
   @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Empty_Set} : @key{constant} Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[in all], @key[use null])],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Has_Element} (Container : Set; Position : Cursor)
      @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[package] @AdaPackDefn{Set_Iterator_Interfaces} @key[is new]
       Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "=" (Left, Right : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Equivalent_Sets} (Left, Right : Set) @key{return} Boolean;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Tampering_With_Cursors_Prohibited}
      (Container : Set) @key{return} Boolean
      @key[with] Nonblocking, Global => (@key[null], @key[use null]);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0339-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Empty} @key{return} Set
      @key[is] (Empty_Set)
      @key[with] Post =>
         @key[not] Tampering_With_Cursors_Prohibited (Empty'Result) @key[and then]
         Length (Empty'Result) = 0;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{To_Set} (New_Item : Element_Type) @key{return} Set@Chg{Version=[5],New=[
      @key[with] Post => Length (To_Set'Result) = 1 @key{and then}
                 @key{not} Tampering_with_Cursors_Prohibited (To_Set'Result)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Length} (Container : Set) @key{return} Count_Type@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Empty} (Container : Set) @key{return} Boolean@Chg{Version=[5],New=[
      @key[with] Nonblocking, Global => (@key[null], @key[use null]),
           Post => Is_Empty'Result = (Length (Container) = 0)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Clear} (Container : @key{in out} Set)@Chg{Version=[5],New=[
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
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Element} (Container : Set;
                     Position  : Cursor) @key{return} Element_Type
      @key{with} Pre  => (Position /= No_Element
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position)
                       @key{or else raise} Program_Error),
           Nonblocking => Element_Type'Nonblocking,
           Global => (@key{null}, @key{use} Element_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} Set;
                              Position  : @key{in}     Cursor;
                              New_item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
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
     (Container : @key{in} Set;
      Position  : @key{in} Cursor;
      Process   : @key{not null access procedure} (Element : @key{in} Element_Type))
      @key{with} Pre  => (Position /= No_Element 
                       @key{or else raise} Constraint_Error) @key{and then}
                    (Has_Element (Container, Position) 
                       @key{or else raise} Program_Error);]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[type] Constant_Reference_Type
         (Element : @key[not null access constant] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
           Nonblocking, Global => @key[in out synchronized],
           Default_Initial_Condition => (@key[raise] Program_Error)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Set;
                                Position  : @key[in] Cursor)
      @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
      @key[with] Pre  => (Position /= No_Element
                       @key[or else raise] Constraint_Error) @key[and then]
                   (Has_Element (Container, Position)
                       @key[or else raise] Program_Error),
           Post => Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global => (@key[null], @key[use null])],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} Set; Source : @key{in} Set)@Chg{Version=[5],New=[
      @key[with] Pre  => @key[not] Tampering_With_Cursors_Prohibited (Target)
                      @key[or else raise] Program_Error,
           Post => Length (Source) = Length (Target)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0001-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Copy} (Source : Set) @key[return] Set@Chg{Version=[5],New=[
      @key[with] Post => Length (Copy'Result) = Length (Source) @key[and then]
                   @key[not] Tampering_With_Cursors_Prohibited (Copy'Result)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Move} (Target : @key{in out} Set;
                   Source : @key{in out} Set)@Chg{Version=[5],New=[
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
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Set;
                     New_Item  : @key{in}     Element_Type;
                     Position  :    @key{out} Cursor;
                     Inserted  :    @key{out} Boolean)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                      @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      Has_Element (Container, Position) @key{and then}
                     (@key{if} Inserted then
                        Length (Container) = Original_Length + 1
                      @key{else}
                        Length (Container) = Original_Length))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Insert} (Container : @key{in out} Set;
                     New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                      @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => Length (Container) = Length (Container)'Old + 1],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Include} (Container : @key{in out} Set;
                      New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                      @key{or else raise} Program_Error) @key{and then}
                   (Length (Container) <= Count_Type'Last - 1
                       @key{or else raise} Constraint_Error),
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      Length (Container)
                         @key{in} Original_Length | Original_Length + 1)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Replace} (Container : @key{in out} Set;
                      New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                      @key{or else raise} Program_Error,
           Post => Length (Container) = Length (Container)'Old],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Exclude} (Container : @key{in out} Set;
                      Item      : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                      @key{or else raise} Program_Error,
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      Length (Container)
                         @key{in} Original_Length - 1 | Original_Length)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Set;
                     Item      : @key{in}     Element_Type)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                      @key{or else raise} Program_Error,
           Post => Length (Container) = Length (Container)'Old - 1],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Set;
                     Position  : @key{in out} Cursor)@Chg{Version=[5],New=[
      @key{with} Pre  => (@key{not} Tampering_With_Cursors_Prohibited (Container)
                      @key{or else raise} Program_Error) @key{and then}
                   (Position /= No_Element 
                       @key{or else raise} Constraint_Error) @key{and then}
                   (Has_Element (Container, Position)
                       @key{or else raise} Program_Error),
           Post => Length (Container) = Length (Container)'Old - 1 @key{and then}
                   Position = No_Element],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_First} (Container : @key{in out} Set)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                      @key{or else raise} Program_Error,
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      (@key{if} Original_Length = 0 @key{then} Length (Container) = 0
                       @key{else} Length (Container) = Original_Length - 1))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_Last} (Container : @key{in out} Set)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                      @key{or else raise} Program_Error,
           Post => (@key{declare}
                      Original_Length : @key{constant} Count_Type :=
                         Length (Container)'Old;
                    @key{begin}
                      (@key{if} Original_Length = 0 @key{then} Length (Container) = 0
                       @key{else} Length (Container) = Original_Length - 1))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} Union (Target : @key{in out} Set;
                    Source : @key{in}     Set)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Target)
                      @key{or else raise} Program_Error,
           Post => Length (Target) <= Length (Target)'Old + Length (Source)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} Union (Left, Right : Set) @key{return} Set@Chg{Version=[5],New=[
      @key{with} Post => Length (Union'Result) <= Length (Left) + Length (Right) @key{and then}
                   @key{not} Tampering_With_Cursors_Prohibited (Union'Result)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "@key{or}" (Left, Right : Set) @key{return} Set @key{renames} Union;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} Intersection (Target : @key{in out} Set;
                           Source : @key{in}     Set)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Target)
                      @key{or else raise} Program_Error,
           Post => Length (Target) <= Length (Target)'Old + Length (Source)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} Intersection (Left, Right : Set) @key{return} Set@Chg{Version=[5],New=[
      @key{with} Post => Length (Intersection'Result) <= Length (Left) + Length (Right) @key{and then}
                   @key{not} Tampering_With_Cursors_Prohibited (Intersection'Result)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "@key{and}" (Left, Right : Set) @key{return} Set @key{renames} Intersection;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} Difference (Target : @key{in out} Set;
                         Source : @key{in}     Set)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Target)
                      @key{or else raise} Program_Error,
           Post => Length (Target) <= Length (Target)'Old + Length (Source)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} Difference (Left, Right : Set) @key{return} Set@Chg{Version=[5],New=[
      @key{with} Post => Length (Difference'Result) <= Length (Left) + Length (Right) @key{and then}
                   @key{not} Tampering_With_Cursors_Prohibited (Difference'Result)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "-" (Left, Right : Set) @key{return} Set @key{renames} Difference;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} Symmetric_Difference (Target : @key{in out} Set;
                                   Source : @key{in}     Set)@Chg{Version=[5],New=[
      @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Target)
                      @key{or else raise} Program_Error,
           Post => Length (Target) <= Length (Target)'Old + Length (Source)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} Symmetric_Difference (Left, Right : Set) @key{return} Set@Chg{Version=[5],New=[
      @key{with} Post => Length (Symmetric_Difference'Result) <= Length (Left) + Length (Right) @key{and then}
                   @key{not} Tampering_With_Cursors_Prohibited (Symmetric_Difference'Result)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "@key{xor}" (Left, Right : Set) @key{return} Set @key{renames}
      Symmetric_Difference;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Overlap} (Left, Right : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Is_Subset} (Subset : Set;
                       Of_Set : Set) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First} (Container : Set) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Post => (@key{if not} Is_Empty (Container)
                    @key{then} Has_Element (Container, First'Result)
                    @key{else} First'Result = No_Element)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{First_Element} (Container : Set)
      @key{return} Element_Type@Chg{Version=[5],New=[
      @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last} (Container : Set) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Post => (@key{if} not Is_Empty (Container)
                    @key{then} Has_Element (Container, Last'Result)
                    @key{else} Last'Result = No_Element)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Last_Element} (Container : Set)
      @key{return} Element_Type@Chg{Version=[5],New=[
      @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Next} (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Nonblocking, Global => (@key{in all}, @key{use null}),
           Post => (@key{if} Position = No_Element @key{then} Next'Result = No_Element
                    @key{else} True)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Next} (Container : Set;
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
      @key{with} Nonblocking, Global => (@key{in all}, @key{use null})],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Next} (Container : @key{in}     Set;
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
      @key{with} Nonblocking, Global => (@key{in all}, @key{use null}),
           Post => (@key{if} Position = No_Element @key{then}
                       Previous'Result = No_Element
                    @key{else} True)],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Previous} (Container : Set;
                      Position  : Cursor) @key{return} Cursor
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Position = No_Element @key{then}
                       Previous'Result = No_Element
                    @key{elsif} Previous'Result = No_Element @key{then}
                       Position = First (Container)
                    @key{else} Has_Element (Container, Previous'Result));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Previous} (Position : @key{in out} Cursor)@Chg{Version=[5],New=[
      @key{with} Global => (@key{in all}, @key{use null}),
           Nonblocking => True],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Previous} (Container : @key{in}     Set;
                       Position  : @key{in out} Cursor)
      @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
           Pre  => Position = No_Element @key{or else}
                   Has_Element (Container, Position)
                       @key{or else raise} Program_Error,
           Post => (@key{if} Position /= No_Element
                    @key{then} Has_Element (Container, Position));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Find} (Container : Set;
                  Item      : Element_Type) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Post => (@key{if} Find'Result = No_Element @key{then} True
                    @key{else} Has_Element (Container, Find'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Floor} (Container : Set;
                   Item      : Element_Type) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Post => (@key{if} Floor'Result = No_Element @key{then} True
                    @key{else} Has_Element (Container, Floor'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Ceiling} (Container : Set;
                     Item      : Element_Type) @key{return} Cursor@Chg{Version=[5],New=[
      @key{with} Post => (@key{if} Ceiling'Result = No_Element @key{then} True
                    @key{else} Has_Element (Container, Ceiling'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Contains} (Container : Set;
                      Item      : Element_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}]}


@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} "<" (Left, Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => (Left /= No_Element @key{and then} Right /= No_Element)
                         @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} ">" (Left, Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => (Left /= No_Element @key{and then} Right /= No_Element)
                         @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} "<" (Left : Cursor; Right : Element_Type) @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => Left /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} ">" (Left : Cursor; Right : Element_Type) @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => Left /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} "<" (Left : Element_Type; Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => Right /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{function} ">" (Left : Element_Type; Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
      @key{with} Pre    => Right /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure}  @AdaSubDefn{Iterate}
     (Container : @key{in} Set;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
      @key{with} Allows_Exit],Old=[]};]}


@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Reverse_Iterate}
     (Container : @key{in} Set;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
      @key{with} Allows_Exit],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[   @key[function] Iterate (Container : @key[in] Set)
      @key[return] Set_Iterator_Interfaces.@Chg{Version=[5],New=[Parallel_Reversible_Iterator],Old=[Reversible_Iterator]}'Class@Chg{Version=[5],New=[
      @key[with] Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0262-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[   @key[function] Iterate (Container : @key[in] Set; Start : @key[in] Cursor)
      @key[return] Set_Iterator_Interfaces.Reversible_Iterator'Class@Chg{Version=[5],New=[
      @key[with] Pre  => (Start /= No_Element
                          @key[or else raise] Constraint_Error) @key[and then]
                      (Has_Element (Container, Start)
                          @key[or else raise] Program_Error),
           Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[   @key{generic}
      @key{type} Key_Type (<>) @key{is private};
      @key{with function} Key (Element : Element_Type) @key{return} Key_Type;
      @key{with function} "<" (Left, Right : Key_Type)
         @key{return} Boolean @key{is} <>;
   @key{package} @AdaPackDefn{Generic_Keys}@Chg{Version=[5],New=[
   @key[with] Nonblocking, Global => @key[null]],Old=[]} @key{is}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[       @key{function} @AdaSubDefn{Equivalent_Keys} (Left, Right : Key_Type)
          @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{function} @AdaSubDefn{Key} (Position : Cursor) @key{return} Key_Type@Chg{Version=[5],New=[
         @key{with} Pre  => Position /= No_Element @key{or else raise} Constraint_Error,
              Global => @key{in all}],Old=[]};]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Key} (Container : Set;
                    Position : Cursor) @key{return} Key_Type
         @key{with} Pre  => (Position /= No_Element 
                         @key{or else raise} Constraint_Error) @key{and then}
                      (Has_Element (Container, Position)
                         @key{or else raise} Program_Error);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[       @key{function} @AdaSubDefn{Element} (Container : Set;
                         Key       : Key_Type)
          @key{return} Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Replace} (Container : @key{in out} Set;
                         Key       : @key{in}     Key_Type;
                         New_Item  : @key{in}     Element_Type)@Chg{Version=[5],New=[
         @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                           @key{or else raise} Program_Error,
              Post => Length (Container) = Length (Container)'Old],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Exclude} (Container : @key{in out} Set;
                         Key       : @key{in}     Key_Type)@Chg{Version=[5],New=[
         @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                           @key{or else raise} Program_Error,
              Post => (@key{declare}
                         Original_Length : @key{constant} Count_Type :=
                            Length (Container)'Old;
                       @key{begin}
                         Length (Container) @key{in} Original_Length - 1 | Original_Length)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Delete} (Container : @key{in out} Set;
                        Key       : @key{in}     Key_Type)@Chg{Version=[5],New=[
         @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                           @key{or else raise} Program_Error,
              Post => Length (Container) = Length (Container)'Old - 1],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{function} @AdaSubDefn{Find} (Container : Set;
                     Key       : Key_Type)
         @key{return} Cursor@Chg{Version=[5],New=[
         @key{with} Post => (@key{if} Find'Result = No_Element @key{then} True
                       @key{else} Has_Element (Container, Find'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[       @key{function} @AdaSubDefn{Floor} (Container : Set;
                       Key       : Key_Type)
         @key{return} Cursor@Chg{Version=[5],New=[
         @key{with} Post => (@key{if} Floor'Result = No_Element @key{then} True
                       @key{else} Has_Element (Container, Floor'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[       @key{function} @AdaSubDefn{Ceiling} (Container : Set;
                         Key       : Key_Type)
         @key{return} Cursor@Chg{Version=[5],New=[
         @key{with} Post => (@key{if} Ceiling'Result = No_Element @key{then} True
                       @key{else} Has_Element (Container, Ceiling'Result))],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[       @key{function} @AdaSubDefn{Contains} (Container : Set;
                          Key       : Key_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],Text=[      @key{procedure} @AdaSubDefn{Update_Element_Preserving_Key}
        (Container : @key{in out} Set;
         Position  : @key{in}     Cursor;
         Process   : @key{not null access procedure}
                         (Element : @key{in out} Element_Type))@Chg{Version=[5],New=[
         @key{with} Pre  => (Position /= No_Element @key{or else}
                          @key{raise} Constraint_Error) @key{and then}
                       (Has_Element (Container, Position) @key{or else raise} Program_Error)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[      @key[type] @AdaTypeDefn{Reference_Type}
            (Element : @key[not null access] Element_Type) @key[is private]
         @key[with] Implicit_Dereference => Element@Chg{Version=[5],New=[,
              Nonblocking, Global => @key[in out synchronized],
              Default_Initial_Condition => (@key[raise] Program_Error)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[      @key[function] @AdaSubDefn{Reference_Preserving_Key} (Container : @key[aliased in out] Set;
                                         Position  : @key[in] Cursor)
         @key[return] Reference_Type@Chg{Version=[5],New=[
         @key[with] Pre  => (Position /= No_Element
                          @key[or else raise] Constraint_Error) @key[and then]
                      (Has_Element (Container, Position)
                          @key[or else raise] Program_Error),
              Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[      @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Set;
                                   Key       : @key[in] Key_Type)
         @key[return] Constant_Reference_Type@Chg{Version=[5],New=[
         @key[with] Pre  => Find (Container, Key) /= No_Element @key[or else]
                         @key[raise] Constraint_Error,
              Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Text=[      @key[function] @AdaSubDefn{Reference_Preserving_Key} (Container : @key[aliased in out] Set;
                                         Key       : @key[in] Key_Type)
         @key[return] Reference_Type@Chg{Version=[5],New=[
         @key[with] Pre  => Find (Container, Key) /= No_Element @key[or else]
                         @key[raise] Constraint_Error,
              Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{end} Generic_Keys;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[   @key{package} @AdaPackDefn{Stable} @key{is}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1],ARef=[AI12-0339-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Set} (Base : @key{not null access} Hashed_Sets.Set) @key{is}
         @key{tagged limited private}
         @key{with} Constant_Indexing => Constant_Reference,
              Default_Iterator  => Iterate,
              Iterator_Element  => Element_Type,
              Aggregate         => (Empty       => Empty,
                                    Add_Unnamed => Include),
              Stable_Properties => (Length),
              Global            => @key[null],
              Default_Initial_Condition => Length (Set) = 0;
      @key{pragma} Preelaborable_Initialization(Set);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Cursor} @key{is private};
      @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @AdaObjDefn{Empty_Set} : @key{constant} Set;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean
         @key{with} Nonblocking, Global => (@key{in all}, @key{use null});]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{package} @AdaPackDefn{Set_Iterator_Interfaces} @key[is new]
         Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} Hashed_Sets.Set;
                        Source : @key{in} Set)
         @key{with} Post => Length (Source) = Length (Target);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{function} @AdaSubDefn{Copy} (Source : Hashed_Sets.Set) @key{return} Set
         @key{with} Post => Length (Copy'Result) = Length (Source);]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI05-0111-1]}
@ChgAdded{Version=[5],Text=[      @key{type} @AdaTypeDefn{Constant_Reference_Type}
            (Element : @key{not null access constant} Element_Type) @key{is private}
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
@ChgAdded{Version=[2],Text=[@key{end} Ada.Containers.Ordered_Sets;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[Two elements @i<E1> and @i<E2> are @i<equivalent>
if both @i<E1> < @i<E2> and @i<E2> < @i<E1> return False, using the generic
formal "<" operator for elements.@Defn2{Term=[equivalent element],Sec=[of an ordered set]}
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
function "=" returns True for any pair of nonequivalent elements, then the
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
@ChgAdded{Version=[2],Text=[@Defn2{Term=[first element],Sec=[of an ordered set]}
@Defn2{Term=[last element],Sec=[of an ordered set]}
@Defn2{Term=[successor element],Sec=[of an ordered set]}@Chg{Version=[3],New=[
@Defn2{Term=[predecessor element],Sec=[of an ordered set]}],Old=[]}
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
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Copy (Source : Set) @key[return] Set@Chg{Version=[5],New=[
   @key[with] Post => Length (Copy'Result) = Length (Source) @key[and then]
                @key[not] Tampering_With_Cursors_Prohibited (Copy'Result)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a set whose
elements are initialized from the corresponding elements of Source.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_First (Container : @key{in out} Set)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => (@key{declare}
                   Original_Length : @key{constant} Count_Type :=
                      Length (Container)'Old;
                 @key{begin}
                   (@key{if} Original_Length = 0 @key{then} Length (Container) = 0
                    @key{else} Length (Container) = Original_Length - 1))],Old=[]};]}
@end{Example}


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, Delete_First
has no effect. Otherwise@Chg{Version=[3],New=[,],Old=[]}
the element designated by First (Container) is removed
from Container. Delete_First tampers with the cursors of Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Delete_Last (Container : @key{in out} Set)@Chg{Version=[5],New=[
   @key{with} Pre  => @key{not} Tampering_With_Cursors_Prohibited (Container)
                    @key{or else raise} Program_Error,
        Post => (@key{declare}
                   Original_Length : @key{constant} Count_Type :=
                      Length (Container)'Old;
                 @key{begin}
                   (@key{if} Original_Length = 0 @key{then} Length (Container) = 0
                    @key{else} Length (Container) = Original_Length - 1))],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Container is empty, Delete_Last
has no effect. Otherwise@Chg{Version=[3],New=[,],Old=[]}
the element designated by Last (Container) is removed
from Container. Delete_Last tampers with the cursors of Container.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} First_Element (Container : Set)
   @key{return} Element_Type@Chg{Version=[5],New=[
   @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to
Element (First (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last (Container : Set) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Post => (@key{if} not Is_Empty (Container)
                 @key{then} Has_Element (Container, Last'Result)
                 @key{else} Last'Result = No_Element)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a cursor that designates
the last element in Container. If Container is empty, returns No_Element.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Last_Element (Container : Set)
   @key{return} Element_Type@Chg{Version=[5],New=[
   @key{with} Pre => (@key{if} Is_Empty (Container) @key{then raise} Constraint_Error)],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to
Element (Last (Container)).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Previous (Position : Cursor) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{in all}, @key{use null}),
        Post => (@key{if} Position = No_Element @key{then}
                    Previous'Result = No_Element
                 @key{else} True)],Old=[]};]}
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
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{function} Previous (Container : Set;
                   Position : Cursor) @key{return} Cursor
   @key{with} Nonblocking, Global => (@key{null}, @key{use null}),
        Pre  => Position = No_Element @key{or else}
                Has_Element (Container, Position)
                    @key{or else raise} Program_Error,
        Post => (@key{if} Position = No_Element @key{then}
                    Previous'Result = No_Element
                 @key{elsif} Previous'Result = No_Element @key{then}
                    Position = First (Container)
                 @key{else} Has_Element (Container, Previous'Result));]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0112-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Returns a cursor designating the
predecessor of the node designated by Position in Container, if any.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Previous (Position : @key{in out} Cursor)@Chg{Version=[5],New=[
   @key{with} Nonblocking, Global => (@key{in all}, @key{use null})],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Position := Previous (Position).]}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],KeepNext=[T],Text=[@key{procedure} Previous (Container : @key{in}     Set;
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Floor (Container : Set;
                Item      : Element_Type) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Post => (@key{if} Floor'Result = No_Element @key{then} True
                 @key{else} Has_Element (Container, Floor'Result))],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Floor searches for the last element
which is not greater than Item. If such an element is found, a cursor that
designates it is returned. Otherwise@Chg{Version=[3],New=[,],Old=[]}
No_Element is returned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Ceiling (Container : Set;
                  Item      : Element_Type) @key{return} Cursor@Chg{Version=[5],New=[
   @key{with} Post => (@key{if} Ceiling'Result = No_Element @key{then} True
                 @key{else} Has_Element (Container, Ceiling'Result))],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Ceiling searches for the first
element which is not less than Item. If such an element is found, a cursor that
designates it is returned. Otherwise@Chg{Version=[3],New=[,],Old=[]}
No_Element is returned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "<" (Left, Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => (Left /= No_Element @key{and then} Right /= No_Element)
                      @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Left) < Element (Right).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} ">" (Left, Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => (Left /= No_Element @key{and then} Right /= No_Element)
                      @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Right) < Element (Left).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "<" (Left : Cursor; Right : Element_Type) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => Left /= No_Element @key{or else raise} Constraint_Error,
        Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Left) < Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} ">" (Left : Cursor; Right : Element_Type) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => Left /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Right < Element (Left).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "<" (Left : Element_Type; Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => Right /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Left < Element (Right).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} ">" (Left : Element_Type; Right : Cursor) @key{return} Boolean@Chg{Version=[5],New=[
   @key{with} Pre    => Right /= No_Element @key{or else raise} Constraint_Error,
           Global => @key{in all}],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Element (Right) < Left.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0112-1]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Reverse_Iterate
  (Container : @key{in} Set;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor))@Chg{Version=[5],New=[
   @key{with} Allows_Exit],Old=[]};]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Iterates over the elements in
Container as per @Chg{Version=[3],New=[procedure ],Old=[]}Iterate,
with the difference that the elements are traversed
in predecessor order, starting with the last element.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] Set)
   @key[return] Set_Iterator_Interfaces.@Chg{Version=[5],New=[Parallel_Reversible_Iterator],Old=[Reversible_Iterator]}'Class@Chg{Version=[5],New=[
   @key[with] Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0212-1],ARef=[AI05-0265-1],ARef=[AI05-0269-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate returns
@Chg{Version=[5],New=[an],Old=[a reversible]}
iterator object (see @RefSecNum{User-Defined Iterator Types}) that
will generate a value for a loop parameter
(see @RefSecNum{Generalized Loop Iteration}) designating
each element in Container, starting with the first element and moving the cursor
according to the successor relation when used as a forward iterator, and
starting with the last element and moving the cursor according to the
predecessor relation when used as a reverse iterator@Chg{Version=[5],New=[, and
processing all nodes concurrently when used as a parallel iterator],Old=[]}.
Tampering with the cursors of Container is prohibited while
the iterator object exists (in particular, in
the @nt{sequence_of_statements} of the @nt{loop_statement} whose
@nt{iterator_specification} denotes this object). The iterator object needs
finalization.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Iterate (Container : @key[in] Set; Start : @key[in] Cursor)
   @key[return] Set_Iterator_Interfaces.Reversible_Iterator'Class@Chg{Version=[5],New=[
   @key[with] Pre  => (Start /= No_Element
                       @key[or else raise] Constraint_Error) @key[and then]
                   (Has_Element (Container, Start)
                       @key[or else raise] Program_Error),
        Post => Tampering_With_Cursors_Prohibited (Container)],Old=[]};]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0262-1],ARef=[AI05-0265-1],ARef=[AI05-0269-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0112-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[@Chg{Version=[5],New=[],Old=[If
Start is not No_Element and does
not designate an item in Container, then Program_Error is propagated. If Start
is No_Element, then Constraint_Error is propagated. Otherwise, ]}Iterate returns
a reversible
iterator object (see @RefSecNum{User-Defined Iterator Types}) that
will generate a value for a loop parameter
(see @RefSecNum{Generalized Loop Iteration}) designating
each element in Container, starting with the element designated by Start and
moving the cursor according to the successor relation when used as a forward
iterator, or moving the cursor according to the predecessor relation when used
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
  are added to Containers.Ordered_Sets. If an instance of Containers.Ordered_Sets
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

@begin{Incompatible2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1],ARef=[AI12-0112-1],ARef=[AI12-0339-1]}
  @ChgAdded{Version=[5],Text=[@Defn{incompatibilities with Ada 2012}A number of
  new subprograms, types, and even a nested package were added to
  Containers.Ordered_Sets to better support contracts and stable views. If an
  instance of Containers.Ordered_Sets
  is referenced in a @nt{use_clause}, and an entity @i<E> with the same
  @nt{defining_identifier} as a new entity in Containers.Ordered_Sets is
  defined in a package that is also referenced in a @nt{use_clause}, the
  entity @i<E> may no longer be use-visible, resulting in errors. This should
  be rare and is easily fixed if it does occur.]}
@end{Incompatible2012}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0212-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}Sets now support
  positional container aggregates, so @nt{aggregate} syntax can be used to
  create Sets.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0266-1]}
  @ChgAdded{Version=[5],Text=[The iterator for the
  entire container now can return a parallel iterator which can be used to
  process the container in parallel.]}
@end{Extend2012}

@begin{DiffWord2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0112-1]}
  @ChgAdded{Version=[5],Text=[Added contracts to this container. This includes
  describing some of the semantics with pre- and postconditions, rather than
  English text. Note that the preconditions can be Suppressed (see
  @RefSecNum{Suppressing Checks}).]}
@end{DiffWord2012}


