@Part(precontainers-2, Root="ada.mss")
@comment{ $Source: e:\\cvsroot/ARM/Source/pre_con2.mss,v $ }
@comment{ $Revision: 1.29 $ $Date: 2015/04/03 04:12:42 $ $Author: randy $ }

@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Multiway_Trees]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Multiway_Trees provides private types Tree and Cursor, and a set of
operations for each type. A multiway tree container is well-suited to represent
nested structures.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Text=[This tree just provides a basic structure, and
  make no promises about balancing or other automatic organization. In this
  sense, it is different than the indexed (Map, Set) forms. Rather, it
  provides a building block on which to construct more complex and more
  specialized tree containers.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0078-1],ARef=[AI12-0159-1]}
@ChgAdded{Version=[3],Text=[A multiway tree container object manages a tree of
@Chg{Version=[4],New=[],Old=[internal ]}@i<nodes>,
@Chg{Version=[4],New=[consisting of a @i<root node>@Defn2{Term=[root],Sec=[of a tree]}@Defn2{Term=[root node],Sec=[of a tree]}
and a set of@Defn2{Term=[internal node],Sec=[of a tree]}
@i<internal nodes>;],Old=[]} each @Chg{Version=[4],New=[internal node],Old=[of
which]} contains an element and pointers to the parent,
first child, last child, next (successor) sibling, and previous (predecessor)
sibling internal nodes.@Defn2{Term=[node],Sec=[of a tree]} A cursor designates a
particular node within a tree (and by extension the element contained in that
node, if any). A cursor keeps designating the same node (and element) as long as
the node is part of the container, even if the node is moved within
the container.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0269-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0078-1]}
@ChgAdded{Version=[3],Text=[A @i<subtree> is a particular node (which @i<roots the subtree>) and all of its child
nodes (including all of the children of the child nodes, recursively).
@Defn2{Term=[subtree],Sec=[of a tree]}@Defn{roots the subtree}@Defn2{Term=[subtree],Sec=[node which roots]}
@Chg{Version=[4],New=[The root node],Old=[There is
a special node, the @i<root>, which]} is always present and has neither an
associated element value nor any parent node@Chg{Version=[4],New=[; it has
pointers to its first child and its last child, if any],Old=[]}. The root node
provides a place to add nodes to an otherwise empty tree and
represents the base of the
tree.@Chg{Version=[4],New=[],Old=[@Defn2{Term=[root],Sec=[of a tree]}@Defn2{Term=[root node],Sec=[of a tree]}]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[A node that has no children is called a
@i<leaf node>.@Defn2{Term=[leaf node],Sec=[of a tree]} The @i<ancestors> of
a node are the node itself, its parent node, the parent of the parent node,
and so on until a node with no parent is reached.@Defn2{Term=[ancestor],Sec=[of a tree node]}
Similarly, the @i<descendants> of a node are the node itself, its child nodes,
the children of each child node, and so on.@Defn2{Term=[descendant],Sec=[of a tree node]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0262-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[The nodes of a subtree can be visited in several
different orders. For a @i<depth-first order>, after visiting a node, the nodes
of its child list are each visited in depth-first order, with each child node
visited in natural order (first child to last child).@Defn{depth-first order}]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[For the depth-first order, when each child node is
    visited, the child list of the child node is visited before the next sibling
    of the child node is visited.]}
@end{Ramification}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The generic library
package Containers.Multiway_Trees has the following declaration:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[@key{with} Ada.Iterator_Interfaces;
@key{generic}
   @key{type} Element_Type @key{is private};
   @key{with function} "=" (Left, Right : Element_Type) @key{return} Boolean @key{is} <>;
@key{package} Ada.Containers.Multiway_Trees @key{is}@ChildUnit{Parent=[Ada.Containers],Child=[Multiway_Trees]}
   @key{pragma} Preelaborate(Multiway_Trees);
   @key{pragma} Remote_Types(Multiway_Trees);]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key{type} @AdaTypeDefn{Tree} @key{is tagged private}
      @key{with} Constant_Indexing => Constant_Reference,
           Variable_Indexing => Reference,
           Default_Iterator  => Iterate,
           Iterator_Element  => Element_Type;
   @key{pragma} Preelaborable_Initialization(Tree);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{type} @AdaTypeDefn{Cursor} @key{is private};
   @key{pragma} Preelaborable_Initialization(Cursor);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @AdaObjDefn{Empty_Tree} : @key{constant} Tree;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @AdaObjDefn{No_Element} : @key{constant} Cursor;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Has_Element} (Position : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key{package} @AdaPackDefn{Tree_Iterator_Interfaces} @key{is new}
      Ada.Iterator_Interfaces (Cursor, Has_Element);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Equal_Subtree} (Left_Position : Cursor;
                           Right_Position: Cursor) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} "=" (Left, Right : Tree) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Is_Empty} (Container : Tree) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Node_Count} (Container : Tree) @key{return} Count_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Subtree_Node_Count} (Position : Cursor) @key{return} Count_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Depth} (Position : Cursor) @key{return} Count_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Is_Root} (Position : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Is_Leaf} (Position : Cursor) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Root} (Container : Tree) @key{return} Cursor;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Clear} (Container : @key{in out} Tree);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Element} (Position : Cursor) @key{return} Element_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Replace_Element} (Container : @key{in out} Tree;
                              Position  : @key{in}     Cursor;
                              New_Item  : @key{in}     Element_Type);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Query_Element}
     (Position : @key{in} Cursor;
      Process  : @key{not null access procedure} (Element : @key{in} Element_Type));]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Update_Element}
     (Container : @key{in out} Tree;
      Position  : @key{in}     Cursor;
      Process   : @key{not null access procedure}
                      (Element : @key{in out} Element_Type));]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Constant_Reference_Type}
         (Element : @key[not null access constant] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Reference_Type} (Element : @key[not null access] Element_Type) @key[is private]
      @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Tree;
                                Position  : @key[in] Cursor)
      @key[return] Constant_Reference_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] Tree;
                       Position  : @key[in] Cursor)
      @key[return] Reference_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Assign} (Target : @key{in out} Tree; Source : @key{in} Tree);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Copy} (Source : Tree) @key{return} Tree;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Move} (Target : @key{in out} Tree;
                   Source : @key{in out} Tree);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Delete_Leaf} (Container : @key{in out} Tree;
                          Position  : @key{in out} Cursor);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Delete_Subtree} (Container : @key{in out} Tree;
                             Position  : @key{in out} Cursor);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Swap} (Container : @key{in out} Tree;
                   I, J      : @key{in}     Cursor);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Find} (Container : Tree;
                  Item      : Element_Type)
      @key{return} Cursor;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Find_In_Subtree} (Position : Cursor;
                             Item     : Element_Type)
      @key{return} Cursor;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Ancestor_Find} (Position : Cursor;
                           Item     : Element_Type)
      @key{return} Cursor;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Contains} (Container : Tree;
                      Item      : Element_Type) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Iterate}
     (Container : @key{in} Tree;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Iterate_Subtree}
     (Position  : @key{in} Cursor;
      Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Iterate} (Container : @key{in} Tree)
      @key{return} Tree_Iterator_Interfaces.Forward_Iterator'Class;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Iterate_Subtree} (Position : @key{in} Cursor)
      @key{return} Tree_Iterator_Interfaces.Forward_Iterator'Class;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Child_Count} (Parent : Cursor) @key{return} Count_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Child_Depth} (Parent, Child : Cursor) @key{return} Count_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Insert_Child} (Container : @key{in out} Tree;
                           Parent    : @key{in}     Cursor;
                           Before    : @key{in}     Cursor;
                           New_Item  : @key{in}     Element_Type;
                           Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Insert_Child} (Container : @key{in out} Tree;
                           Parent    : @key{in}     Cursor;
                           Before    : @key{in}     Cursor;
                           New_Item  : @key{in}     Element_Type;
                           Position  :    @key{out} Cursor;
                           Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Insert_Child} (Container : @key{in out} Tree;
                           Parent    : @key{in}     Cursor;
                           Before    : @key{in}     Cursor;
                           Position  :    @key{out} Cursor;
                           Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Prepend_Child} (Container : @key{in out} Tree;
                            Parent    : @key{in}     Cursor;
                            New_Item  : @key{in}     Element_Type;
                            Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Append_Child} (Container : @key{in out} Tree;
                           Parent    : @key{in}     Cursor;
                           New_Item  : @key{in}     Element_Type;
                           Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Delete_Children} (Container : @key{in out} Tree;
                              Parent    : @key{in}     Cursor);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Copy_Subtree} (Target   : @key{in out} Tree;
                           Parent   : @key{in}     Cursor;
                           Before   : @key{in}     Cursor;
                           Source   : @key{in}     Cursor);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Splice_Subtree} (Target   : @key{in out} Tree;
                             Parent   : @key{in}     Cursor;
                             Before   : @key{in}     Cursor;
                             Source   : @key{in out} Tree;
                             Position : @key{in out} Cursor);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Splice_Subtree} (Container: @key{in out} Tree;
                             Parent   : @key{in}     Cursor;
                             Before   : @key{in}     Cursor;
                             Position : @key{in}     Cursor);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Splice_Children} (Target          : @key{in out} Tree;
                              Target_Parent   : @key{in}     Cursor;
                              Before          : @key{in}     Cursor;
                              Source          : @key{in out} Tree;
                              Source_Parent   : @key{in}     Cursor);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Splice_Children} (Container       : @key{in out} Tree;
                              Target_Parent   : @key{in}     Cursor;
                              Before          : @key{in}     Cursor;
                              Source_Parent   : @key{in}     Cursor);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Parent} (Position : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{First_Child} (Parent : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{First_Child_Element} (Parent : Cursor) @key{return} Element_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Last_Child} (Parent : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Last_Child_Element} (Parent : Cursor) @key{return} Element_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Next_Sibling} (Position : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Previous_Sibling} (Position : Cursor) @key{return} Cursor;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Next_Sibling} (Position : @key{in out} Cursor);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Previous_Sibling} (Position : @key{in out} Cursor);]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Iterate_Children}
     (Parent  : @key{in} Cursor;
      Process : @key{not null access procedure} (Position : @key{in} Cursor));]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[   @key{procedure} @AdaSubDefn{Reverse_Iterate_Children}
     (Parent  : @key{in} Cursor;
      Process : @key{not null access procedure} (Position : @key{in} Cursor));]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Iterate_Children} (Container : @key[in] Tree; Parent : @key[in] Cursor)
      @key[return] Tree_Iterator_Interfaces.Reversible_Iterator'Class;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{private}
   ... -- @Examcom[not specified by the language]
@key[end] Ada.Containers.Multiway_Trees;]}

@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[The actual function for the generic formal function "=" on Element_Type values
is expected to define a reflexive and symmetric relationship and return the same
result value each time it is called with a particular pair of values. If it
behaves in some other manner, the functions Find, Reverse_Find, Equal_Subtree,
and "=" on tree values return an unspecified value. The exact arguments and
number of calls of this generic formal function by the functions Find,
Reverse_Find, Equal_Subtree, and "=" on tree values are unspecified.@PDefn{unspecified}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[The type Tree is used to represent trees. The
type Tree needs finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[Empty_Tree represents the empty Tree object. It
contains only the root node (Node_Count (Empty_Tree) returns 1). If an object of type
Tree is not otherwise initialized, it is initialized to the same value as
Empty_Tree.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[No_Element represents a cursor that designates no
element. If an object of type Cursor is not otherwise initialized, it is
initialized to the same value as No_Element.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[The predefined "=" operator for type Cursor returns
True if both cursors are No_Element, or designate the same element in the same
container.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[Execution of the default implementation of the
Input, Output, Read, or Write attribute of type Cursor raises Program_Error.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[Tree'Write for a Tree object @i<T> writes
Node_Count(@i<T>) - 1 elements of the tree to the stream. It also may write
additional information about the tree.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[Tree'Read reads the representation of a tree
from the stream, and assigns to @i<Item> a tree with the same elements and
structure as was written by Tree'Write.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Streaming more elements than the container
  holds is wrong. For implementation implications of this rule, see the Implementation Note in
  @RefSecNum{The Generic Package Containers.Vectors}.]}
@end{Ramification}



@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[@Redundant[Some operations of this generic package
have access-to-subprogram parameters. To ensure such operations are
well-defined, they guard against certain actions by the designated subprogram.
In particular, some operations check for "tampering with cursors" of a container
because they depend on the set of elements of the container remaining constant,
and others check for "tampering with elements" of a container because they
depend on elements of the container not being replaced.]]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[@Defn2{Term=[tamper with cursors],Sec=[of a tree]}
A subprogram is said to
@i{tamper with cursors} of a tree object @i<T> if:]}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[it inserts or deletes elements of @i<T>, that is,
  it calls the Clear, Delete_Leaf, Insert_Child, Delete_Children,
  Delete_Subtree, or Copy_Subtree procedures
  with @i<T> as a parameter; or]}

@begin{Honest}
@ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Operations which are defined to be equivalent to
    a call on one of these operations also are included. Similarly, operations
    which call one of these as part of their definition are included.]}
@end{Honest}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Text=[it reorders the elements of @i<T>, that is, it
  calls the Splice_Subtree or Splice_Children procedures with @i<T> as a
  parameter; or]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[it finalizes @i<T>; or]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[it calls Assign with @i<T> as the Target parameter; or]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[We don't need to explicitly mention
  @nt{assignment_statement}, because that finalizes the target object
  as part of the operation, and finalization of an object is already defined
  as tampering with cursors.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[it calls the Move procedure with @i<T> as a parameter.]}

@begin{Reason}
@ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Swap copies elements rather than reordering
    them, so it doesn't tamper with cursors.]}
@end{Reason}
@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[@Defn2{Term=[tamper with elements],Sec=[of a tree]}
A subprogram is said to
@i{tamper with elements} of a tree object @i<T> if:]}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[it tampers with cursors of @i<T>; or]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[it replaces one or more elements of @i<T>, that is, it calls the Replace_Element or Swap
  procedures with @i<T> as a parameter.]}

@begin{Reason}
@ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Complete replacement of an element can cause its
    memory to be deallocated while another operation is holding onto a reference
    to it. That can't be allowed. However, a simple modification of (part of) an
    element is not a problem, so Update_Element does not cause a problem.]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[Assign is defined in terms of Clear and Replace_Element,
  so we don't need to mention it explicitly. Similarly, we don't need to
  explicitly mention @nt{assignment_statement}, because that finalizes the
  target object as part of the operation, and finalization of an object is
  already defined as tampering with the element.]}
@end{Ramification}
@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0110-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[prohibited],Sec=[tampering with a tree]}
@Defn2{Term=[tampering],Sec=[prohibited for a tree]}
When tampering with cursors is @i<prohibited> for a particular tree object
@i<T>, Program_Error is propagated by a call of any language-defined subprogram
that is defined to tamper with the cursors of @i<T>, leaving @i<T> unmodified.
Similarly, when tampering with elements is @i<prohibited> for a particular tree
object @i<T>, Program_Error is propagated by a call of any language-defined
subprogram that is defined to tamper with the elements of @i<T> @Redundant[(or
tamper with the cursors of @i<T>)], leaving @i<T>
unmodified.@Chg{Version=[4],New=[ These checks are made before any other
defined behavior of the body of the language-defined subprogram.],Old=[]}]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Tampering with elements includes tampering with
  cursors, so we mention it only from completeness in the second sentence.]}
@end{TheProof}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Has_Element (Position : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if Position designates
an element, and returns False otherwise. @Redundant[In particular, Has_Element
returns False if the cursor designates a root node or equals No_Element.]]}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Text=[This function might not detect cursors that
  designate deleted elements; such cursors are invalid (see below) and the
  result of calling Has_Element with an invalid cursor is unspecified (but
  not erroneous).]}
@end{Honest}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Equal_Subtree (Left_Position : Cursor;
                        Right_Position: Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1],ARef=[AI05-0262-1],ARef=[AI05-0264-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Left_Position or Right_Position
  equals No_Element, propagates Constraint_Error. If the number of child nodes
  of the element designated by Left_Position is different from the number of
  child nodes of the element designated by Right_Position, the function
  returns False. If Left_Position designates a root node and Right_Position does
  not, the function returns False. If Right_Position designates a root
  node and Left_Position does not, the function returns False. Unless both
  cursors designate a root node, the elements are compared using the generic
  formal equality operator. If the result of the element comparison is False,
  the function returns
  False. Otherwise, it calls Equal_Subtree on a cursor designating each child
  element of the element designated by Left_Position and a cursor designating
  the corresponding child element of the element designated by Right_Position.
  If any such call returns False, the function returns False; otherwise, it
  returns True. Any exception raised during the evaluation of element equality
  is propagated.]}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Left_Position and Right_Position do not need to
    be from the same tree.]}
@end{Ramification}

@begin{ImplNote}
@ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[This wording describes the canonical semantics.
    However, the order and number of calls on the formal equality function
    is unspecified for all of the operations that use it in this package, so an
    implementation can call it as many or as few times as it needs to get the
    correct answer. Similarly, a global rule (see the introduction of
    @RefSecNum{Predefined Language Environment})
    says that language-defined routines are not affected by overriding of other
    language-defined routines. This means that no reasonable program can tell
    how many times Equal_Subtree is called, and thus an implementation can call
    it as many or as few times as it needs to get the correct answer.
    Specifically, there is no requirement to call the formal equality or
    Equal_Subtree additional times once the answer has been determined.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} "=" (Left, Right : Tree) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Left and Right denote the same
  tree object, then the function returns True. Otherwise, it calls Equal_Subtree
  with cursors designating the root nodes of Left and Right; the result is
  returned. Any exception raised during the evaluation of Equal_Subtree is
  propagated.]}

@begin{ImplNote}
@ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Similar considerations apply here as apply to
    Equal_Subtree. The actual number of calls performed is unspecified.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Node_Count (Container : Tree) @key{return} Count_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Node_Count returns the number of
  nodes in Container.]}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Since all tree objects have a root node, this
    can never return a value of 0. Node_Count (Some_Tree) should always equal
    Subtree_Node_Count (Root (Some_Tree)).]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Subtree_Node_Count (Position : Cursor) @key{return} Count_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position is No_Element,
  Subtree_Node_Count returns 0; otherwise, Subtree_Node_Count returns the number of
  nodes in the subtree that is rooted by Position.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Is_Empty (Container : Tree) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to Node_Count (Container) = 1.]}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[An empty tree contains just the root node.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Depth (Position : Cursor) @key{return} Count_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position equals No_Element,
  Depth returns 0; otherwise, Depth returns the number of ancestor nodes of the
  node designated by Position (including the node itself).]}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Depth (Root (Some_Tree)) = 1.]}
@end{Ramification}


@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Is_Root (Position : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Is_Root returns True if the
  Position designates the root node of some tree; and returns False otherwise.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Is_Leaf (Position : Cursor) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Is_Leaf returns True if Position
  designates a node that does not have any child nodes; and returns False
  otherwise.]}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Is_Leaf returns False if passed No_Element,
    since No_Element does not designate a node. Is_Leaf can be passed a cursor
    that designates the root node; Is_Leaf will return True if passed the root
    node of an empty tree.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Root (Container : Tree) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Root returns a cursor that
  designates the root node of Container.]}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[There is always a root node, even in an empty
    container, so this function never returns No_Element.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Clear (Container : @key{in out} Tree);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Removes all the elements from
  Container.]}

@begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The root node is not removed; all trees have a
    root node.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Element (Position : Cursor) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position equals No_Element,
  then Constraint_Error is propagated; if Position designates the root node of a
  tree, then Program_Error is propagated. Otherwise, Element returns the element
  designated by Position.]}

@begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The root node does not contain an element, so
    that value cannot be read or written.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Replace_Element (Container : @key{in out} Tree;
                           Position  : @key{in}     Cursor;
                           New_Item  : @key{in}     Element_Type);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0264-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position equals No_Element,
  then Constraint_Error is propagated; if Position does not designate an element
  in Container (including if it designates the root node), then Program_Error is
  propagated. Otherwise, Replace_Element assigns the value New_Item to the
  element designated by Position.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Query_Element
  (Position : @key{in} Cursor;
   Process  : @key{not null access procedure} (Element : @key{in} Element_Type));]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0265-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position equals No_Element,
  then Constraint_Error is propagated; if Position designates the root node of a
  tree, then Program_Error is propagated. Otherwise, Query_Element calls
  Process.@key{all} with the element designated by Position as the argument.
  Tampering with the elements of the tree that contains the element designated
  by Position is prohibited during the execution of the call on
  Process.@key{all}. Any exception raised by Process.@key{all} is propagated.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Update_Element
  (Container : @key{in out} Tree;
   Position  : @key{in}     Cursor;
   Process   : @key{not null access procedure}
                   (Element : @key{in out} Element_Type));]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0264-1],ARef=[AI05-0265-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position equals No_Element,
  then Constraint_Error is propagated; if Position does not designate an element
  in Container (including if it designates the root node), then Program_Error is
  propagated. Otherwise, Update_Element calls Process.@key{all} with the element
  designated by Position as the argument. Tampering with the elements of
  Container is prohibited during the execution of the call on Process.@key{all}.
  Any exception raised by Process.@key{all} is propagated.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[If Element_Type is unconstrained and definite,
  then the actual Element parameter of Process.@key{all} shall be
  unconstrained.]}

  @begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[This means that the elements cannot be directly
    allocated from the heap; it must be possible to change the discriminants of
    the element in place.]}
  @end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{type} Constant_Reference_Type
      (Element : @key[not null access constant] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[type] Reference_Type (Element : @key[not null access] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[The types Constant_Reference_Type
  and Reference_Type need finalization.@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
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
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] Tree;
                             Position  : @key[in] Cursor)
   @key[return] Constant_Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
  Constant_Indexing and Implicit_Dereference aspects) provides a convenient way
  to gain read access to an individual element of a tree given a
  cursor.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
  @ChgAdded{Version=[3],Text=[If Position equals No_Element, then
  Constraint_Error is propagated; if Position does not designate an element in
  Container, then Program_Error is propagated. Otherwise, Constant_Reference
  returns an object whose discriminant is an access value that designates the
  element designated by Position. Tampering with the elements of Container
  is prohibited while the object returned by Constant_Reference exists
  and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference (Container : @key[aliased in out] Tree;
                    Position  : @key[in] Cursor)
   @key[return] Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
  Variable_Indexing and Implicit_Dereference aspects) provides a convenient way
  to gain read and write access to an individual element of a tree
  given a cursor.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
  @ChgAdded{Version=[3],Text=[If Position equals No_Element, then
  Constraint_Error is propagated; if Position does not designate an element in
  Container, then Program_Error is propagated. Otherwise, Reference returns an
  object whose discriminant is an access value that designates the element
  designated by Position. Tampering with the elements
  of Container is prohibited while the object returned by Reference exists
  and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Assign (Target : @key{in out} Tree; Source : @key{in} Tree);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Target denotes the same object
  as Source, the operation has no effect. Otherwise, the elements of Source are
  copied to Target as for an @nt{assignment_statement} assigning Source to
  Target.]}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Each element in Target has a
    parent element that corresponds to the parent element of the Source element,
    and has child elements that correspond to the child elements of the Source
    element.]}
@end{Ramification}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[3],Text=[This routine exists for compatibility with the
  bounded tree container. For an unbounded tree, @exam{Assign(A, B)} and
  @exam{A := B} behave identically. For a bounded tree, := will raise an
  exception if the container capacities are different, while Assign will
  not raise an exception if there is enough room in the target.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Copy (Source : Tree) @key{return} Tree;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a tree with the same
  structure as Source and whose elements are initialized from the corresponding
  elements of Source.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Move (Target : @key{in out} Tree;
                Source : @key{in out} Tree);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Target denotes the same object
  as Source, then the operation has no effect. Otherwise, Move first calls Clear
  (Target). Then, the nodes other than the root node in Source are moved to
  Target (in the same positions). After Move completes, Node_Count (Target) is
  the number of nodes originally in Source, and Node_Count (Source) is 1.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Delete_Leaf (Container : @key{in out} Tree;
                       Position  : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position equals No_Element,
  then Constraint_Error is propagated; if Position does not designate an element
  in Container (including if it designates the root node), then Program_Error is
  propagated. If the element designated by position has any child elements, then
  Constraint_Error is propagated. Otherwise, Delete_Leaf removes (from Container)
  the element designated by Position. Finally, Position is set to No_Element.]}

@begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The check on Position
    checks that the cursor does not belong to some other Container. This check
    implies that a reference to the container is included in the cursor value.
    This wording is not meant to require detection of dangling cursors; such
    cursors are defined to be invalid, which means that execution is erroneous,
    and any result is allowed (including not raising an exception).]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The root node cannot be deleted.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Delete_Subtree (Container : @key{in out} Tree;
                          Position  : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0264-1],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position equals No_Element,
  then Constraint_Error is propagated. If Position does not designate an element
  in Container (including if it designates the root node), then Program_Error is
  propagated. Otherwise, Delete_Subtree removes (from Container) the subtree
  designated by Position (that is, all descendants of the node designated
  by Position including the node itself), and Position is set to No_Element.]}

@begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The root node cannot be deleted. To delete the
    entire contents of the tree, call Clear(Container).]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Swap (Container : @key{in out} Tree;
                I, J      : @key{in}     Cursor);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If either I or J equals
  No_Element, then Constraint_Error is propagated. If either I or J do not
  designate an element in Container (including if either designates the root
  node), then Program_Error is propagated. Otherwise, Swap exchanges the values
  of the elements designated by I and J.]}

@begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[After a call to Swap, I designates the element
    value previously designated by J, and J designates the element value
    previously designated by I. The position of the elements do not change; for
    instance, the parent node and the first child node of I are unchanged by the
    operation.]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The root nodes do not contain element values, so
    they cannot be swapped.]}
@end{Ramification}

@begin{Honest}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The implementation is not required to actually
    copy the elements if it can do the swap some other way. But it is allowed to
    copy the elements if needed.]}
@end{Honest}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Find (Container : Tree;
               Item      : Element_Type)
   @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Find searches the elements of
  Container for an element equal to Item (using the generic formal equality
  operator). The search starts at the root node. The search traverses the tree
  in a depth-first order. If no equal element is found, then Find returns
  No_Element. Otherwise, it returns a cursor designating the first equal element
  encountered.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Find_In_Subtree (Position : Cursor;
                          Item     : Element_Type)
   @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position equals No_Element,
  then Constraint_Error is propagated. Find_In_Subtree searches
  the subtree rooted by Position for an element equal to Item (using the
  generic formal equality operator). The search starts at the element designated
  by Position. The search traverses the subtree in a depth-first
  order. If no equal element is found, then Find returns No_Element. Otherwise,
  it returns a cursor designating the first equal element encountered.]}

@begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Find_In_Subtree does not check any siblings of
    the element designated by Position. The root node does not contain an
    element, and therefore it can never be returned, but it can be explicitly
    passed to Position.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Ancestor_Find (Position : Cursor;
                        Item     : Element_Type)
   @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position equals No_Element,
  then Constraint_Error is propagated. Otherwise, Ancestor_Find searches
  for an element equal to Item (using the generic formal equality operator). The
  search starts at the node designated by Position, and checks each ancestor
  proceeding toward the root of the subtree. If no equal element is found, then
  Ancestor_Find returns No_Element. Otherwise, it returns a cursor designating
  the first equal element encountered.]}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Text=[No_Element is returned if Position is the root
  node.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Contains (Container : Tree;
                   Item      : Element_Type) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to Find (Container, Item) /= No_Element.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Iterate
  (Container : @key{in} Tree;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0069-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate calls Process.@key{all}
  with a cursor that designates each element in Container, starting
  @Chg{Version=[4],New=[from],Old=[with]} the
  root node and proceeding in a depth-first order. Tampering with the cursors
  of Container is prohibited during the execution of a call on Process.@key{all}.
  Any exception raised by Process.@key{all} is propagated.]}

@begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Process is not called with the root node, which
    does not have an associated element.]}
@end{Ramification}

@begin{ImplNote}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The purpose of the tamper with cursors check is
    to prevent erroneous execution from the Position parameter of
    Process.@key{all} becoming invalid. This check takes place when the
    operations that tamper with the cursors of the container are called. The
    check cannot be made later (say in the body of Iterate), because that could
    cause the Position cursor to be invalid and potentially cause execution to
    become erroneous @em defeating the purpose of the check.]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[See Iterate for vectors
    (@RefSecNum{The Generic Package Containers.Vectors}) for a suggested
    implementation of the check.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Iterate_Subtree
  (Position  : @key{in} Cursor;
   Process   : @key{not null access procedure} (Position : @key{in} Cursor));]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0265-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0069-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position equals No_Element,
  then Constraint_Error is propagated. Otherwise, Iterate_Subtree calls
  Process.@key{all}
  with a cursor that designates each element in the subtree rooted by the node
  designated by Position, starting
  @Chg{Version=[4],New=[from],Old=[with]} the node designated by Position and
  proceeding in a depth-first order.
  Tampering with the cursors of the tree that contains the element
  designated by Position
  is prohibited during the execution of a call on Process.@key{all}.
  Any exception raised by Process.@key{all} is propagated.]}

@begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Position can be passed a cursor designating the
    root node; in that case, Process is not called with the root node, which
    does not have an associated element.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Iterate (Container : @key{in} Tree)
   @key{return} Tree_Iterator_Interfaces.Forward_Iterator'Class;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1],ARef=[AI05-0265-1],ARef=[AI05-0269-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0069-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate returns an iterator object
  (see @RefSecNum{User-Defined Iterator Types}) that will
  generate a value for a loop parameter (see @RefSecNum{Generalized Loop Iteration})
  designating each @Chg{Version=[4],New=[element],Old=[node]} in Container, starting
  @Chg{Version=[4],New=[from],Old=[with]} the root node and proceeding in a depth-first order.
  Tampering with the cursors of Container is prohibited while the iterator
  object exists (in particular, in the
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

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Iterate_Subtree (Position : @key{in} Cursor)
   @key{return} Tree_Iterator_Interfaces.Forward_Iterator'Class;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1],ARef=[AI05-0265-1],ARef=[AI05-0269-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0069-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position equals No_Element,
  then Constraint_Error is propagated. Otherwise, Iterate_Subtree returns an
  iterator object (see @RefSecNum{User-Defined Iterator Types}) that will
  generate a value for a loop parameter (see @RefSecNum{Generalized Loop Iteration})
  designating each element in the subtree rooted by the node designated by Position,
  starting @Chg{Version=[4],New=[from],Old=[with]} the
  node designated by Position and proceeding in a depth-first
  order. If Position equals No_Element, then Constraint_Error is propagated.
  Tampering with the cursors of the container that contains the node
  designated by Position is prohibited while the iterator object exists
  (in particular, in the
  @nt{sequence_of_statements} of the @nt{loop_statement} whose
  @nt{iterator_specification} denotes this object). The iterator object
  needs finalization.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Child_Count (Parent : Cursor) @key{return} Count_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Child_Count returns the number of
  child nodes of the node designated by Parent.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Child_Depth (Parent, Child : Cursor) @key{return} Count_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Child or Parent is equal to
  No_Element, then Constraint_Error is propagated. Otherwise, Child_Depth
  returns the number of ancestor nodes of Child (including Child itself), up to
  but not including Parent; Program_Error is propagated if Parent
  is not an ancestor of Child.]}

@begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Program_Error is propagated if Parent and Child
    are nodes in different containers.]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Child_Depth (Root (Some_Tree), Child) + 1 =
    Depth (Child) as the root is not counted.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Insert_Child (Container : @key{in out} Tree;
                        Parent    : @key{in}     Cursor;
                        Before    : @key{in}     Cursor;
                        New_Item  : @key{in}     Element_Type;
                        Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Parent equals No_Element, then
  Constraint_Error is propagated. If Parent does not designate a node in
  Container, then Program_Error is propagated. If Before is not equal to
  No_Element, and does not designate a node in Container, then Program_Error is
  propagated. If Before is not equal to No_Element, and
  Parent does not designate the parent node of the node designated by Before,
  then Constraint_Error is propagated.
  Otherwise, Insert_Child allocates Count nodes containing copies of New_Item
  and inserts them as children of Parent. If Parent already has child nodes, then
  the new nodes are inserted prior to the node designated by Before, or, if
  Before equals No_Element, the new nodes are inserted after the last existing
  child node of Parent. Any exception raised during allocation of internal
  storage is propagated, and Container is not modified.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Insert_Child (Container : @key{in out} Tree;
                        Parent    : @key{in}     Cursor;
                        Before    : @key{in}     Cursor;
                        New_Item  : @key{in}     Element_Type;
                        Position  :    @key{out} Cursor;
                        Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1],ARef=[AI05-0257-1],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Parent equals No_Element, then
  Constraint_Error is propagated. If Parent does not designate a node in
  Container, then Program_Error is propagated. If Before is not equal to
  No_Element, and does not designate a node in Container, then Program_Error is
  propagated. If Before is not equal to No_Element, and
  Parent does not designate the parent node of the node designated by Before,
  then Constraint_Error is propagated.
  Otherwise, Insert_Child allocates Count nodes containing copies of New_Item
  and inserts them as children of Parent. If Parent already has child nodes, then
  the new nodes are inserted prior to the node designated by Before, or, if
  Before equals No_Element, the new nodes are inserted after the last existing
  child node of Parent. Position designates the first newly-inserted node, or if
  Count equals 0, then Position is assigned the value of Before. Any exception
  raised during allocation of internal storage is propagated, and Container is
  not modified.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Insert_Child (Container : @key{in out} Tree;
                        Parent    : @key{in}     Cursor;
                        Before    : @key{in}     Cursor;
                        Position  :    @key{out} Cursor;
                        Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0257-1],ARef=[AI05-0262-1],ARef=[AI05-0264-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Parent equals No_Element, then
  Constraint_Error is propagated. If Parent does not designate a node in
  Container, then Program_Error is propagated. If Before is not equal to
  No_Element, and does not designate a node in Container, then Program_Error is
  propagated. If Before is not equal to No_Element, and
  Parent does not designate the parent node of the node designated by Before,
  then Constraint_Error is propagated.
  Otherwise, Insert_Child allocates Count nodes, the elements contained in the
  new nodes are initialized by default (see @RefSecNum{Object Declarations}),
  and the new nodes are inserted as children of Parent. If Parent already has
  child nodes, then the new nodes are inserted prior to the node designated by
  Before, or, if Before equals No_Element, the new nodes are inserted after the
  last existing child node of Parent. Position designates the first
  newly-inserted node, or if Count equals 0, then Position is assigned the value
  of Before. Any exception raised during allocation of internal storage is
  propagated, and Container is not modified.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Prepend_Child (Container : @key{in out} Tree;
                         Parent    : @key{in}     Cursor;
                         New_Item  : @key{in}     Element_Type;
                         Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to Insert_Child
  (Container, Parent, First_Child (Container, Parent), New_Item, Count).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Append_Child (Container : @key{in out} Tree;
                        Parent    : @key{in}     Cursor;
                        New_Item  : @key{in}     Element_Type;
                        Count     : @key{in}     Count_Type := 1);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to Insert_Child
  (Container, Parent, No_Element, New_Item, Count).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Delete_Children (Container : @key{in out} Tree;
                           Parent    : @key{in}     Cursor);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Parent equals No_Element, then
  Constraint_Error is propagated. If Parent does not designate a node in
  Container, Program_Error is propagated. Otherwise, Delete_Children removes
  (from Container) all of the descendants of Parent other than Parent itself.]}

@begin{Discussion}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[This routine deletes all of the child subtrees
    of Parent at once. Use Delete_Subtree to delete an individual subtree.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Copy_Subtree (Target   : @key{in out} Tree;
                        Parent   : @key{in}     Cursor;
                        Before   : @key{in}     Cursor;
                        Source   : @key{in}     Cursor);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Parent equals No_Element, then
  Constraint_Error is propagated. If Parent does not designate a node in Target,
  then Program_Error is propagated. If Before is not equal to No_Element, and
  does not designate a node in Target, then Program_Error is propagated. If
  Before is not equal to No_Element, and
  Parent does not designate the parent node of the node designated by Before,
  then Constraint_Error is propagated. If Source designates a root node,
  then Constraint_Error is propagated. If Source is equal to No_Element, then
  the operation has no effect. Otherwise, the subtree rooted by Source (which
  can be from any tree; it does not have to be a subtree of Target) is copied
  (new nodes are allocated to create a new subtree with the same structure as
  the Source subtree, with each element initialized from the corresponding
  element of the Source subtree) and inserted into Target as a child of Parent.
  If Parent already has child nodes, then the new nodes are inserted prior to the
  node designated by Before, or, if Before equals No_Element, the new nodes are
  inserted after the last existing child node of Parent. The parent of the newly
  created subtree is set to Parent, and the overall count of Target is
  incremented by Subtree_Node_Count (Source). Any exception raised during allocation
  of internal storage is propagated, and Container is not modified.]}

@begin{Discussion}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[We only need one routine here, as the source
    object is not modified, so we can use the same routine for both copying
    within and between containers.]}
@end{Discussion}

@begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[We do not allow copying a subtree that includes
    a root node, as that would require inserting a node with no value in the
    middle of the target tree. To copy an entire tree to another tree object,
    use Copy.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Splice_Subtree (Target   : @key{in out} Tree;
                          Parent   : @key{in}     Cursor;
                          Before   : @key{in}     Cursor;
                          Source   : @key{in out} Tree;
                          Position : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1],ARef=[AI05-0262-1],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Parent equals No_Element, then
  Constraint_Error is propagated. If Parent does not designate a node in Target,
  then Program_Error is propagated. If Before is not equal to No_Element, and
  does not designate a node in Target, then Program_Error is propagated. If
  Before is not equal to No_Element, and
  Parent does not designate the parent node of the node designated by Before,
  then Constraint_Error is propagated. If Position equals No_Element,
  Constraint_Error is propagated. If Position does not designate a node in
  Source or designates a root node, then Program_Error is propagated. If Source
  denotes the same object as Target, then:
  if Position equals Before there is no effect; if Position designates an
  ancestor of Parent (including Parent itself), Constraint_Error is propagated;
  otherwise, the subtree rooted by the element designated by Position is
  moved to be a child of Parent. If Parent already has child nodes, then the
  moved nodes are inserted prior to the node designated by Before, or, if Before
  equals No_Element, the moved nodes are inserted after the last existing child
  node of Parent. In each of these cases, Position and the count of Target are
  unchanged, and the parent of the element designated by Position is set to
  Parent.]}

@begin{Reason}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[We can't allow moving the subtree of Position to
    a proper descendant node of the subtree, as the descendant node will be part of the
    subtree being moved. The result would be a circularly linked tree, or one
    with inaccessible nodes. Thus we have to check Position against Parent, even
    though such a check is @i<O>(Depth(Source)).]}
@end{Reason}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Text=[Otherwise (if Source does not denote the same
  object as Target), the subtree designated by Position is removed from Source
  and moved to Target. The subtree is inserted as a child of Parent. If Parent
  already has child nodes, then the moved nodes are inserted prior to the node
  designated by Before, or, if Before equals No_Element, the moved nodes are
  inserted after the last existing child node of Parent. In each of these cases,
  the count of Target is incremented by Subtree_Node_Count (Position), and the
  count of Source is decremented by Subtree_Node_Count (Position), Position is
  updated to represent an element in Target.]}

@begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[If Source is the same as Target, and Position =
    Before, or Next_Sibling(Position) = Before, Splice_Subtree has no effect, as
    the subtree does not have to move to meet the postcondition.]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[We do not allow splicing a subtree that includes
    a root node, as that would require inserting a node with no value in the
    middle of the target tree. Splice the children of the root node instead.]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[For this reason there is no operation to splice
    an entire tree, as that would necessarily involve splicing a root node.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Splice_Subtree (Container: @key{in out} Tree;
                          Parent   : @key{in}     Cursor;
                          Before   : @key{in}     Cursor;
                          Position : @key{in}     Cursor);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1],ARef=[AI05-0262-1],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Parent equals No_Element, then
  Constraint_Error is propagated. If Parent does not designate a node in
  Container, then Program_Error is propagated. If Before is not equal to
  No_Element, and does not designate a node in Container, then Program_Error is
  propagated. If Before is not equal to No_Element, and
  Parent does not designate the parent node of the node designated by Before,
  then Constraint_Error is propagated. If Position
  equals No_Element, Constraint_Error is propagated. If Position does not
  designate a node in Container or designates a root node, then Program_Error is
  propagated. If Position equals Before, there is no effect. If Position
  designates an ancestor of Parent (including Parent itself), Constraint_Error is
  propagated. Otherwise, the subtree rooted by the element designated by
  Position is moved to be a child of Parent. If Parent already has child nodes,
  then the moved nodes are inserted prior to the node designated by Before, or,
  if Before equals No_Element, the moved nodes are inserted after the last
  existing child node of Parent. The parent of the element designated by
  Position is set to Parent.]}

@begin{Reason}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[We can't allow moving the subtree of Position to
    a proper descendant node of the subtree, as the descendant node will be part of the
    subtree being moved.]}
@end{Reason}


@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Splice_Children (Target          : @key{in out} Tree;
                           Target_Parent   : @key{in}     Cursor;
                           Before          : @key{in}     Cursor;
                           Source          : @key{in out} Tree;
                           Source_Parent   : @key{in}     Cursor);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Target_Parent equals
  No_Element, then Constraint_Error is propagated. If Target_Parent does not
  designate a node in Target, then Program_Error is propagated. If Before is not
  equal to No_Element, and does not designate an element in Target, then
  Program_Error is propagated. If Source_Parent equals No_Element, then
  Constraint_Error is propagated. If Source_Parent does not designate a node in
  Source, then Program_Error is propagated. If Before is not equal to
  No_Element, and Target_Parent does not designate the parent node of the
  node designated by Before, then Constraint_Error is propagated.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[If Source denotes the same object as Target, then:]}

@begin{Itemize}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[if Target_Parent equals Source_Parent there is
    no effect; else]}

    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0269-1]}
    @ChgAdded{Version=[3],Text=[if Source_Parent is an ancestor of
    Target_Parent other than Target_Parent itself, then Constraint_Error is propagated; else]}

    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1],ARef=[AI05-0269-1]}
    @ChgAdded{Version=[3],Text=[the child elements (and the further descendants) of
    Source_Parent are moved to be child elements of Target_Parent. If
    Target_Parent already has child elements, then the moved elements are
    inserted prior to the node designated by Before, or, if Before equals
    No_Element, the moved elements are inserted after the last existing child
    node of Target_Parent. The parent of each moved child element is set to
    Target_Parent.]}
@end{Itemize}

@begin{Reason}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[We can't allow moving the children of
    Source_Parent to a proper descendant node, as the descendant node will be
    part of one of the subtrees being moved.]}
@end{Reason}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],Text=[Otherwise (if Source does not denote the same
  object as Target), the child elements (and the further descendants) of Source_Parent
  are removed from Source and moved to Target. The child elements are inserted
  as children of Target_Parent. If Target_Parent already has child elements, then
  the moved elements are inserted prior to the node designated by Before, or, if
  Before equals No_Element, the moved elements are inserted after the last
  existing child node of Target_Parent. In each of these cases, the overall
  count of Target is incremented by Subtree_Node_Count (Source_Parent)-1, and
  the overall count of Source is decremented by Subtree_Node_Count
  (Source_Parent)-1.]}

@begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The node designated by Source_Parent is not
    moved, thus we never need to update Source_Parent.]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Move (Target, Source) could be written
    Splice_Children (Target, Target.Root, No_Element, Source, Source.Root);]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Splice_Children (Container       : @key{in out} Tree;
                           Target_Parent   : @key{in}     Cursor;
                           Before          : @key{in}     Cursor;
                           Source_Parent   : @key{in}     Cursor);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1],ARef=[AI05-0262-1],ARef=[AI05-0264-1],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Target_Parent equals
  No_Element, then Constraint_Error is propagated. If Target_Parent does not
  designate a node in Container, then Program_Error is propagated. If Before is
  not equal to No_Element, and does not designate an element in Container, then
  Program_Error is propagated. If Source_Parent equals No_Element, then
  Constraint_Error is propagated. If Source_Parent does not designate a node in
  Container, then Program_Error is propagated. If Before is not equal to
  No_Element, and Target_Parent does not designate the parent node of the
  node designated by Before, then Constraint_Error is propagated.
  If Target_Parent equals Source_Parent there is
  no effect. If Source_Parent is an ancestor of Target_Parent other than
  Target_Parent itself, then
  Constraint_Error is propagated. Otherwise, the child elements (and the further
  descendants) of Source_Parent are moved to be child elements of Target_Parent.
  If Target_Parent already has child elements, then the moved elements are
  inserted prior to the node designated by Before, or, if Before equals
  No_Element, the moved elements are inserted after the last existing child node
  of Target_Parent. The parent of each moved child element is set to
  Target_Parent.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Parent (Position : Cursor) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position is equal to No_Element
  or designates a root node, No_Element is returned. Otherwise, a cursor
  designating the parent node of the node designated by Position is returned.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} First_Child (Parent : Cursor) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Parent is equal to No_Element,
  then Constraint_Error is propagated. Otherwise, First_Child returns a cursor
  designating the first child node of the node designated by Parent; if there is
  no such node, No_Element is returned.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} First_Child_Element (Parent : Cursor) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to Element (First_Child (Parent)).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Last_Child (Parent : Cursor) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Parent is equal to No_Element,
  then Constraint_Error is propagated. Otherwise, Last_Child returns a cursor
  designating the last child node of the node designated by Parent; if there is
  no such node, No_Element is returned.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Last_Child_Element (Parent : Cursor) @key{return} Element_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to Element (Last_Child (Parent)).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Next_Sibling (Position : Cursor) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position equals No_Element or
  designates the last child node of its parent, then Next_Sibling returns the
  value No_Element. Otherwise, it returns a cursor that designates the successor
  (with the same parent) of the node designated by Position.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Previous_Sibling (Position : Cursor) @key{return} Cursor;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[If Position equals No_Element or
  designates the first child node of its parent, then Previous_Sibling returns
  the value No_Element. Otherwise, it returns a cursor that designates the
  predecessor (with the same parent) of the node designated by Position.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Next_Sibling (Position : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to Position := Next_Sibling (Position);]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Previous_Sibling (Position : @key{in out} Cursor);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to Position := Previous_Sibling (Position);]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Iterate_Children
  (Parent  : @key{in} Cursor;
   Process : @key{not null access procedure} (Position : @key{in} Cursor));]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Parent equals No_Element, then
Constraint_Error is propagated.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Iterate_Children calls Process.@key{all} with a
cursor that designates each child node of Parent, starting with the first child
node and moving the cursor as per the Next_Sibling function.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[Tampering with the cursors of the tree containing
Parent is prohibited during the execution of a call on Process.@key{all}.
Any exception raised by Process.@key{all} is propagated.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{procedure} Reverse_Iterate_Children
  (Parent  : @key{in} Cursor;
   Process : @key{not null access procedure} (Position : @key{in} Cursor));]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Parent equals No_Element, then
Constraint_Error is propagated.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Reverse_Iterate_Children calls Process.@key{all}
with a cursor that designates each child node of Parent, starting with the last
child node and moving the cursor as per the Previous_Sibling function.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[Tampering with the cursors of the tree containing
Parent is prohibited during the execution of a call on Process.@key{all}.
Any exception raised by Process.@key{all} is propagated.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Iterate_Children (Container : @key[in] Tree; Parent : @key[in] Cursor)
   @key[return] Tree_Iterator_Interfaces.Reversible_Iterator'Class;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Iterate_Children returns a
reversible iterator object
(see @RefSecNum{User-Defined Iterator Types}) that will generate
a value for a loop parameter (see @RefSecNum{Generalized Loop Iteration})
designating each child node of Parent. If Parent equals No_Element, then
Constraint_Error is propagated. If Parent does not designate a node in
Container, then Program_Error is propagated. Otherwise, when used as a forward
iterator, the nodes are designated starting with the first child node and moving
the cursor as per the function Next_Sibling; when used as a reverse iterator,
the nodes are designated starting with the last child node and moving the cursor
as per the function Previous_Sibling. Tampering with the cursors of Container is
prohibited while the iterator object exists (in particular, in the
@nt{sequence_of_statements} of the @nt{loop_statement} whose
@nt{iterator_specification} denotes this object). The iterator object needs
finalization.]}

@end{DescribeCode}

@end{StaticSem}

@begin{Bounded}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error for the actual function associated with a generic formal
subprogram, when called as part of an operation of this package, to tamper with
elements of any Tree parameter of the operation. Either Program_Error is raised,
or the operation works as defined on the value of the Tree either prior to, or
subsequent to, some or all of the modifications to the Tree.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to call any subprogram declared in the visible part of
Containers.Multiway_Trees when the associated container has been finalized. If
the operation takes Container as an @key[in out] parameter, then it raises
Constraint_Error or Program_Error. Otherwise, the operation either proceeds as
it would for an empty container, or it raises Constraint_Error or Program_Error.]}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Type=[Leading],Keepnext=[T],Text=[A Cursor
value is @i<invalid> if any of the following have occurred since
it was created:@Defn2{Term=[invalid cursor],Sec=[of a tree]}
@PDefn2{Term=[cursor],Sec=[invalid]}]}
@begin{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[The tree that contains the element it designates has
been finalized;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[The tree that contains the element it designates has
been used as the Source or Target of a call to Move;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[The tree that contains the element it designates has
been used as the Target of a call to Assign or the target of an
@nt{assignment_statement};]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[The element it designates has been removed from the
tree that previously contained the element.]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We talk about which tree the element was removed
  from in order to handle splicing nodes from one tree to another. The node
  still exists, but any cursors that designate it in the original tree are now
  invalid. This bullet covers removals caused by calls to Clear, Delete_Leaf,
  Delete_Subtree, Delete_Children, Splice_Children, and Splice_Subtree.]}
@end{Reason}

@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[The result of "=" or Has_Element is unspecified if
it is called with an invalid cursor parameter.@PDefn{unspecified} Execution is
erroneous if any other subprogram declared in Containers.Multiway_Trees is
called with an invalid cursor parameter.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The list above is intended to be exhaustive. In
  other cases, a cursor value continues to designate its original element (or
  the root node). For instance, cursor values survive the insertion and deletion
  of other nodes.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[While it is possible to check for these cases, in
  many cases the overhead necessary to make the check is substantial in time or
  space. Implementations are encouraged to check for as many of these cases as
  possible and raise Program_Error if detected.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[Execution is erroneous if the tree associated with
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

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[No storage associated with a multiway tree object
shall be lost upon assignment or scope exit.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[The execution of an @nt{assignment_statement} for
a tree shall have the effect of copying the elements from the source tree
object to the target tree object and changing the node count of the target
object to that of the source object.]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0298-1]}
  @ChgAdded{Version=[3],Text=[An assignment of a Tree is a @lquotes@;deep@rquotes
  copy; that is the elements are copied as well the data structures.
  We say @lquotes@;effect of@rquotes in order to allow the implementation to
  avoid copying elements immediately if it wishes. For instance, an
  implementation that avoided copying until one of the containers is modified
  would be allowed. (Note that this implementation would
  require care, see @RefSecNum{The Generic Package Containers.Vectors} for more.)]}
@end{ImplNote}
@end{ImplReq}

@begin{ImplAdvice}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[Containers.Multiway_Trees should be implemented
similarly to a multiway tree. In particular, if @i{N} is the overall number of nodes
for a particular tree, then the worst-case time complexity of Element, Parent,
First_Child, Last_Child, Next_Sibling, Previous_Sibling,
Insert_Child with Count=1, and Delete should be @i{O}(log @i{N}).]}
@ChgImplAdvice{Version=[3],Kind=[AddedNormal],Text=[@ChgAdded{Version=[3],
Text=[The worst-case time complexity of the Element, Parent,
First_Child, Last_Child, Next_Sibling, Previous_Sibling,
Insert_Child with Count=1, and Delete
operations of Containers.Multiway_Trees should be @i{O}(log @i<N>).]}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We do not mean to overly constrain implementation
  strategies here. However, it is important for portability that the performance
  of large containers has roughly the same factors on different implementations.
  If a program is moved to an implementation that takes @i{O}(@i<N>) time to access
  elements, that program could be unusable when the trees are large. We allow
  @i{O}(log @i<N>) access because the proportionality constant and caching effects are
  likely to be larger than the log factor, and we don't want to discourage
  innovative implementations.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[Move should not copy elements, and should minimize
copying of internal data structures.]}
@ChgImplAdvice{Version=[3],Kind=[AddedNormal],Text=[@ChgAdded{Version=[3],
Text=[Containers.Multiway_Trees.Move should not copy elements, and should
minimize copying of internal data structures.]}]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Usually that can be accomplished simply by
  moving the pointer(s) to the internal data structures from the Source
  container to the Target container.]}
@end{ImplNote}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[If an exception is propagated from a tree
operation, no storage should be lost, nor any elements removed from a tree
unless specified by the operation.]}
@ChgImplAdvice{Version=[3],Kind=[AddedNormal],Text=[@ChgAdded{Version=[3],
Text=[If an exception is propagated from a tree
operation, no storage should be lost, nor any elements removed from a tree
unless specified by the operation.]}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is important so that programs can recover
  from errors. But we don't want to require heroic efforts, so we just require
  documentation of cases where this can't be accomplished.]}
@end{Reason}
@end{ImplAdvice}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0257-1],ARef=[AI05-0265-1],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The generic package Containers.Multiway_Trees is new.]}
@end{Extend2005}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0069-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Fixed the function Iterate
  so it is clear that the root node is never visited.]}

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0078-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> The definition of @i<node> is
  clarified so that it it doesn't appear to say all nodes have an element.]}

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0110-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Clarified that tampering checks
  precede all other checks made by a subprogram (but come after those associated
  with the call).]}
@end{DiffWord2012}


@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Indefinite_Vectors]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The language-defined generic package
Containers.Indefinite_Vectors provides a private type Vector and a set of
operations. It provides the same operations as the package Containers.Vectors
(see @RefSecNum{The Generic Package Containers.Vectors}), with the difference that
the generic formal Element_Type is indefinite.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The declaration
of the generic library package
Containers.Indefinite_Vectors@ChildUnit{Parent=[Ada.Containers],Child=[Indefinite_Vectors]}
has the same contents @Chg{Version=[3],New=[and semantics ],Old=[]}as
Containers.Vectors except:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The generic formal Element_Type is indefinite.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Type=[Leading],Text=[The procedures with
the profiles:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Noprefix=[T],Keepnext=[F],Type=[Leading],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Extended_Index;
                  Count     : @key{in}     Count_Type := 1);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Noprefix=[T],Keepnext=[T],Type=[Leading],Text=[@key{procedure} Insert (Container : @key{in out} Vector;
                  Before    : @key{in}     Cursor;
                  Position  :    @key{out} Cursor;
                  Count     : @key{in}     Count_Type := 1);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Noprefix=[T],Text=[are omitted.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[These procedures are omitted because there is no
  way to create a default-initialized object of an indefinite type. Note that
  Insert_Space can be used instead of this routine in most cases. Omitting
  the routine completely allows any problems to be diagnosed by
  the compiler when converting from a definite to indefinite vector.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The actual Element parameter of access subprogram Process
of Update_Element may be constrained even if Element_Type is unconstrained.]}

@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
@ChgAdded{Version=[4],Text=[The operations "&", Append, Insert, Prepend,
Replace_Element, and To_Vector that have a formal parameter of type
Element_Type perform indefinite insertion (see @RefSecNum{Containers}).]}
@end{Itemize}
@end{StaticSem}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The generic package Containers.Indefinite_Vectors is new.]}
@end{Extend95}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  Defined some routines to @ldquote@;perform indefinite insertion@rdquote.
  This could mean that some calls to those routines would now raise
  Program_Error where they previously worked. However, this is extremely
  unlikely, as it would require that the package was not implemented in Ada
  (an Ada @nt{allocator} would raise Program_Error in these circumstances), and
  that a program inserted a more nested tagged type (or access discriminant)
  into a container, and then used that object before its type or discriminant
  went out of scope. All known implementations are implemented in Ada, so we
  believe there is no practical incompatibility. As such, we
  mention this only for completeness.]}
@end{Inconsistent2012}


@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Indefinite_Doubly_Linked_Lists]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The language-defined generic package Containers.Indefinite_Doubly_Linked_Lists
provides private types List and Cursor, and a set of operations for each
type. It provides the same operations as the package
Containers.Doubly_Linked_Lists
(see @RefSecNum{The Generic Package Containers.Doubly_Linked_Lists}),
with the difference that the generic formal Element_Type is indefinite.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The declaration of
the generic library package
Containers.@!Indefinite_@!Doubly_@!Linked_@!Lists@ChildUnit{Parent=[Ada.Containers],Child=[Indefinite_Doubly_Linked_Lists]}
has the same contents @Chg{Version=[3],New=[and semantics ],Old=[]}as
Containers.@!Doubly_@!Linked_@!Lists except:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The generic formal Element_Type is indefinite.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Type=[Leading],Text=[The procedure with
the profile:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Noprefix=[T],Keepnext=[T],Type=[Leading],Text=[@key{procedure} Insert (Container : @key{in out} List;
                  Before    : @key{in}     Cursor;
                  Position  :    @key{out} Cursor;
                  Count     : @key{in}     Count_Type := 1);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Noprefix=[T],Text=[is omitted.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This procedure is omitted because there is no way
  to create a default-initialized object of an indefinite type. We considered
  having this routine insert an empty element similar to the empty elements of
  a vector, but rejected this possibility because the semantics are fairly
  complex and very different from the existing definite container. That would
  make it more error-prone to convert a container from a definite type to an
  indefinite type; by omitting the routine completely, any problems will be
  diagnosed by the compiler.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The actual Element parameter of access subprogram Process
of Update_Element may be constrained even if Element_Type is unconstrained.]}

@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
@ChgAdded{Version=[4],Text=[The operations Append, Insert, Prepend, and
Replace_Element that have a formal parameter of type Element_Type perform
indefinite insertion (see @RefSecNum{Containers}).]}
@end{Itemize}
@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Containers.Indefinite_Doubly_Linked_Lists is new.]}
@end{Extend95}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  Defined some routines to @ldquote@;perform indefinite insertion@rdquote.
  This could mean that some calls to those routines would now raise
  Program_Error where they previously worked. However, this is extremely
  unlikely; see @Inconsistent2012Title in
  @RefSecNum{The Generic Package Containers.Indefinite_Vectors} for details.]}
@end{Inconsistent2012}


@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Indefinite_Hashed_Maps]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The language-defined generic package Containers.Indefinite_Hashed_Maps provides
a map with the same operations as the package Containers.Hashed_Maps
(see @RefSecNum{The Generic Package Containers.Hashed_Maps}),
with the difference that the generic formal types Key_Type and Element_Type are
indefinite.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The declaration of
the generic library package
Containers.Indefinite_Hashed_Maps@ChildUnit{Parent=[Ada.Containers],Child=[Indefinite_Hashed_Maps]}
has the same contents @Chg{Version=[3],New=[and semantics ],Old=[]}as
Containers.Hashed_Maps except:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The generic formal Key_Type is indefinite.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The generic formal Element_Type is indefinite.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Type=[Leading],Text=[The procedure with
the profile:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Noprefix=[T],Keepnext=[T],Type=[Leading],Text=[@key{procedure} Insert (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Noprefix=[T],Text=[is omitted.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This procedure is omitted because there is no way
  to create a default-initialized object of an indefinite type. We considered
  having this routine insert an empty element similar to the empty elements of
  a vector, but rejected this possibility because the semantics are fairly
  complex and very different from the existing case. That would make it more
  error-prone to convert a container from a definite type to an indefinite
  type; by omitting the routine completely, any problems will be diagnosed by
  the compiler.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The actual Element parameter of access subprogram Process
of Update_Element may be constrained even if Element_Type is unconstrained.]}

@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
@ChgAdded{Version=[4],Text=[The operations Include, Insert, Replace, and
Replace_Element that have a formal parameter of type Element_Type perform
indefinite insertion (see @RefSecNum{Containers}).]}
@begin{Discussion}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[Some of the named operations also have a formal
  of the indefinite formal type Key_Type and perform indefinite insertion using
  that value, but it is sufficient to mention the formal of type Element_Type
  to cover those.]}
@end{Discussion}

@end{Itemize}
@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Containers.Indefinite_Hashed_Maps is new.]}
@end{Extend95}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  Defined some routines to @ldquote@;perform indefinite insertion@rdquote.
  This could mean that some calls to those routines would now raise
  Program_Error where they previously worked. However, this is extremely
  unlikely; see @Inconsistent2012Title in
  @RefSecNum{The Generic Package Containers.Indefinite_Vectors} for details.]}
@end{Inconsistent2012}


@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Indefinite_Ordered_Maps]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The language-defined generic package Containers.Indefinite_Ordered_Maps
provides a map with the same operations as the package Containers.Ordered_Maps
(see @RefSecNum{The Generic Package Containers.Ordered_Maps}), with the difference that
the generic formal types Key_Type and Element_Type are indefinite.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The declaration of
the generic library package
Containers.Indefinite_Ordered_Maps@ChildUnit{Parent=[Ada.Containers],Child=[Indefinite_Ordered_Maps]}
has the same contents @Chg{Version=[3],New=[and semantics ],Old=[]}as
Containers.Ordered_Maps except:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The generic formal Key_Type is indefinite.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The generic formal Element_Type is indefinite.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Type=[Leading],Text=[The procedure with
the profile:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Noprefix=[T],Keepnext=[T],Type=[Leading],Text=[@key{procedure} Insert (Container : @key{in out} Map;
                  Key       : @key{in}     Key_Type;
                  Position  :    @key{out} Cursor;
                  Inserted  :    @key{out} Boolean);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Noprefix=[T],Text=[is omitted.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This procedure is omitted because there is no way
  to create a default-initialized object of an indefinite type. We considered
  having this routine insert an empty element similar to the empty elements of
  a vector, but rejected this possibility because the semantics are fairly
  complex and very different from the existing case. That would make it more
  error-prone to convert a container from a definite type to an indefinite
  type; by omitting the routine completely, any problems will be diagnosed by
  the compiler.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The actual Element parameter of access subprogram Process
of Update_Element may be constrained even if Element_Type is unconstrained.]}

@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
@ChgAdded{Version=[4],Text=[The operations Include, Insert, Replace, and
Replace_Element that have a formal parameter of type Element_Type perform
indefinite insertion (see @RefSecNum{Containers}).]}
@begin{Discussion}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[Some of the named operations also have a formal
  of the indefinite formal type Key_Type and perform indefinite insertion using
  that value, but it is sufficient to mention the formal of type Element_Type
  to cover those.]}
@end{Discussion}

@end{Itemize}
@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Containers.Indefinite_Ordered_Maps is new.]}
@end{Extend95}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  Defined some routines to @ldquote@;perform indefinite insertion@rdquote.
  This could mean that some calls to those routines would now raise
  Program_Error where they previously worked. However, this is extremely
  unlikely; see @Inconsistent2012Title in
  @RefSecNum{The Generic Package Containers.Indefinite_Vectors} for details.]}
@end{Inconsistent2012}


@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Indefinite_Hashed_Sets]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The language-defined generic package
Containers.Indefinite_Hashed_Sets provides a set with the same operations as
the package Containers.Hashed_Sets
(see @RefSecNum{The Generic Package Containers.Hashed_Sets}), with the difference
that the generic formal type Element_Type is indefinite.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The declaration
of the generic library package
Containers.Indefinite_Hashed_Sets@ChildUnit{Parent=[Ada.Containers],Child=[Indefinite_Hashed_Sets]}
has the same contents @Chg{Version=[3],New=[and semantics ],Old=[]}as
Containers.Hashed_Sets except:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The generic formal Element_Type is indefinite.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The actual Element parameter of access subprogram Process
of Update_@!Element_@!Preserving_Key may be constrained even if Element_Type is
unconstrained.]}

@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
@ChgAdded{Version=[4],Text=[The operations Include, Insert, Replace,
Replace_Element, and To_Set that have a formal parameter of type Element_Type
perform indefinite insertion (see @RefSecNum{Containers}).]}
@begin{Ramification}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[This includes the procedure Replace declared in
  the nested generic package Generic_Keys, as well as the routines declared
  directly in the Containers.Indefinite_Hashed_Sets package.]}
@end{Ramification}
@end{Itemize}
@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Containers.Indefinite_Hashed_Sets is new.]}
@end{Extend95}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  Defined some routines to @ldquote@;perform indefinite insertion@rdquote.
  This could mean that some calls to those routines would now raise
  Program_Error where they previously worked. However, this is extremely
  unlikely; see @Inconsistent2012Title in
  @RefSecNum{The Generic Package Containers.Indefinite_Vectors} for details.]}
@end{Inconsistent2012}


@LabeledAddedSubclause{Version=[2],Name=[The Generic Package Containers.Indefinite_Ordered_Sets]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The language-defined generic package
Containers.Indefinite_Ordered_Sets provides a set with the same operations as
the package Containers.Ordered_Sets
(see @RefSecNum{The Generic Package Containers.Ordered_Sets}), with the difference
that the generic formal type Element_Type is indefinite.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The declaration
of the generic library package
Containers.Indefinite_Ordered_Sets@ChildUnit{Parent=[Ada.Containers],Child=[Indefinite_Ordered_Sets]}
has the same contents @Chg{Version=[3],New=[and semantics ],Old=[]}as
Containers.Ordered_Sets except:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The generic formal Element_Type is indefinite.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The actual Element parameter of access subprogram Process
of Update_@!Element_@!Preserving_Key may be constrained even if Element_Type is
unconstrained.]}

@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
@ChgAdded{Version=[4],Text=[The operations Include, Insert, Replace,
Replace_Element, and To_Set that have a formal parameter of type Element_Type
perform indefinite insertion (see @RefSecNum{Containers}).]}
@begin{Ramification}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[This includes the procedure Replace declared in
  the nested generic package Generic_Keys, as well as the routines declared
  directly in the Containers.Indefinite_Ordered_Sets package.]}
@end{Ramification}
@end{Itemize}
@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Containers.Indefinite_Ordered_Sets is new.]}
@end{Extend95}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  Defined some routines to @ldquote@;perform indefinite insertion@rdquote.
  This could mean that some calls to those routines would now raise
  Program_Error where they previously worked. However, this is extremely
  unlikely; see @Inconsistent2012Title in
  @RefSecNum{The Generic Package Containers.Indefinite_Vectors} for details.]}
@end{Inconsistent2012}


@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Indefinite_Multiway_Trees]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Indefinite_Multiway_Trees
provides a multiway tree with the same operations as the package
Containers.Multiway_Trees (see @RefSecNum{The Generic Package Containers.Multiway_Trees}),
with the difference that the generic formal Element_Type is indefinite.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The declaration
of the generic library package
Containers.Indefinite_Multiway_Trees@ChildUnit{Parent=[Ada.Containers],Child=[Indefinite_Multiway_Trees]}
has the same contents and semantics as
Containers.Multiway_Trees except:]}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[The generic formal Element_Type is indefinite.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Keepnext=[T],Type=[Leading],Text=[The procedure with
the profile:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Keepnext=[T],Type=[Leading],Text=[@key{procedure} Insert_Child (Container : @key{in out} Tree;
                        Parent    : @key{in}     Cursor;
                        Before    : @key{in}     Cursor;
                        Position  :    @key{out} Cursor;
                        Count     : @key{in}     Count_Type := 1);]}
@end{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[is omitted.]}

@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[This procedure is omitted because there is no way to create a
default-initialized object of an indefinite type. We considered having this
routine insert an empty element similar to the empty elements of a vector, but
rejected this possibility because the semantics are fairly complex and very
different from the existing case. That would make it more error-prone to convert
a container from a definite type to an indefinite type; by omitting the routine
completely, any problems will be diagnosed by the compiler.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[The actual Element parameter of access subprogram
Process of Update_Element may be constrained even if Element_Type is
unconstrained.]}

@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
@ChgAdded{Version=[4],Text=[The operations Append_Child, Insert_Child,
Prepend_Child, and Replace_Element that have a formal parameter of type
Element_Type perform indefinite insertion (see @RefSecNum{Containers}).]}

@end{Itemize}
@end{StaticSem}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The generic package Containers.Indefinite_Multiway_Trees is new.]}
@end{Extend2005}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  Defined some routines to @ldquote@;perform indefinite insertion@rdquote.
  This could mean that some calls to those routines would now raise
  Program_Error where they previously worked. However, this is extremely
  unlikely; see @Inconsistent2012Title in
  @RefSecNum{The Generic Package Containers.Indefinite_Vectors} for details.]}
@end{Inconsistent2012}


@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Indefinite_Holders]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Indefinite_Holders provides a private type Holder and a set of
operations for that type. A holder container holds a single element of an
indefinite type.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Text=[A holder container allows the declaration of an
object that can be used like an uninitialized variable or component of an
indefinite type.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Text=[A holder container may be @i{empty}.
An empty holder does not contain an element.@Defn{empty holder}]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The generic library
package Containers.Indefinite_Holders has the following declaration:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],Aref=[AI05-0069-1],Aref=[AI05-0084-1]}
@ChgAdded{Version=[3],Text=[@key[generic]
   @key[type] Element_Type (<>) @key[is private];
   @key[with function] "=" (Left, Right : Element_Type) @key[return] Boolean @key[is] <>;
@key[package] Ada.Containers.Indefinite_Holders @key[is]@ChildUnit{Parent=[Ada.Containers],Child=[Indefinite_Holders]}
   @key[pragma] Preelaborate(Indefinite_Holders);
   @key[pragma] Remote_Types(Indefinite_Holders);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Holder} @key[is tagged private];
   @key[pragma] Preelaborable_Initialization (Holder);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @AdaObjDefn{Empty_Holder} : @key[constant] Holder;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] "=" (Left, Right : Holder) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{To_Holder} (New_Item : Element_Type) @key[return] Holder;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Empty} (Container : Holder) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Clear} (Container : @key[in out] Holder);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Element} (Container : Holder) @key[return] Element_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Replace_Element} (Container : @key[in out] Holder;
                              New_Item  : @key[in]     Element_Type);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Query_Element}
  (Container : @key[in] Holder;
   Process   : @key[not null access procedure] (Element : @key[in] Element_Type));]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Update_Element}
  (Container : @key[in out] Holder;
   Process   : @key[not null access procedure] (Element : @key[in out] Element_Type));]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Constant_Reference_Type}
      (Element : @key[not null access constant] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Reference_Type} (Element : @key[not null access] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Constant_Reference} (Container : @key[aliased in] Holder)
   @key[return] Constant_Reference_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Reference} (Container : @key[aliased in out] Holder)
   @key[return] Reference_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Assign} (Target : @key[in out] Holder; Source : @key[in] Holder);]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Copy} (Source : Holder) @key[return] Holder;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Move} (Target : @key[in out] Holder; Source : @key[in out] Holder);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{private}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   ... -- @Examcom[not specified by the language]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{end} Ada.Containers.Indefinite_Holders;]}

@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Text=[The actual function for the generic formal
function "=" on Element_Type values is expected to define a reflexive and
symmetric relationship and return the same result value each time it is called
with a particular pair of values. If it behaves in some other manner, the
function "=" on holder values returns an unspecified value. The exact arguments
and number of calls of this generic formal function by the function "=" on
holder values are unspecified.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[If the actual function for "=" is not symmetric
  and consistent, the result returned by any of the functions defined to use
  "=" cannot be predicted. The implementation is not required to protect
  against "=" raising an exception, or returning random results, or any other
  "bad" behavior. And it can call "=" in whatever manner makes sense. But
  note that only the results of the function "=" is unspecified; other
  subprograms are not allowed to break if "=" is bad.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Text=[The type Holder is used to represent holder
containers. The type Holder needs finalization@PDefn2{Term=<needs finalization>,
Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Text=[Empty_Holder represents an empty holder object. If
an object of type Holder is not otherwise initialized, it is initialized to the
same value as Empty_Holder.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[@Redundant[Some operations of this generic package
have access-to-subprogram parameters. To ensure such operations are
well-defined, they guard against certain actions by the designated subprogram.
In particular, some operations check for @ldquote@;tampering with
the element@rdquote of a container because they depend on the element of the
container not being replaced.]]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[@Defn2{Term=[tamper with elements],Sec=[of a holder]}
A subprogram is said to @i{tamper with the element} of a holder object @i<H> if:]}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[It clears the element contained by @i<H>, that is,
it calls the Clear procedure with @i<H> as a parameter;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[It replaces the element contained by @i<H>, that is,
it calls the Replace_Element procedure with @i<H> as a parameter;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[It calls the Move procedure with @i<H> as a parameter;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[It finalizes @i<H>.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Complete replacement of an element can cause its
  memory to be deallocated while another operation is holding onto a reference
  to it. That can't be allowed. However, a simple modification of (part of) an
  element is not a problem, so Update_Element does not cause a problem.]}
@end{Reason}
@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0110-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[prohibited],Sec=[tampering with a holder]}
@Defn2{Term=[tampering],Sec=[prohibited for a holder]}
When tampering with the element is @i<prohibited> for a particular holder object
@i<H>, Program_Error is propagated by a call of any language-defined subprogram
that is defined to tamper with the element of @i<H>, leaving @i<H>
unmodified.@Chg{Version=[4],New=[ These checks are made before any other
defined behavior of the body of the language-defined subprogram.],Old=[]}]}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} "=" (Left, Right : Holder) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Left and Right denote the same
holder object, then the function returns True. Otherwise, it compares the
element contained in Left to the element contained in Right using the
generic formal equality operator, returning the result of that operation. Any
exception raised during the evaluation of element equality is propagated.]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This wording describes the canonical semantics.
  However, the order and number of calls on the formal equality @key[function]
  is unspecified, so an implementation need not call the equality function
  if the correct answer can be determined without doing so.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] To_Holder (New_Item : Element_Type) @key[return] Holder;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0035-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a nonempty holder
containing an element initialized to New_Item.@Chg{Version=[4],New=[
To_Holder performs indefinite insertion (see @RefSecNum{Containers}).],Old=[]}]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Is_Empty (Container : Holder) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if Container is
empty, and False if it contains an element.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[procedure] Clear (Container : @key[in out] Holder);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Removes the element from Container.
Container is empty after a successful Clear operation.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Element (Container : Holder) @key[return] Element_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Container is empty,
Constraint_Error is propagated. Otherwise, returns the element stored in
Container.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[procedure] Replace_Element (Container : @key[in out] Holder;
                           New_Item  : @key[in]     Element_Type);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0035-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Replace_Element assigns the value
New_Item into Container, replacing any preexisting content of
Container@Chg{Version=[4],New=[; Replace_Element performs indefinite insertion
(see @RefSecNum{Containers})],Old=[]}.
Container is not empty after a successful call to Replace_Element.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[procedure] Query_Element
  (Container : @key[in] Holder;
   Process   : @key[not null access procedure] (Element : @key[in] Element_Type));]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1],ARef=[AI05-0262-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Container is empty,
Constraint_Error is propagated. Otherwise, Query_Element calls
Process.@key[all] with the contained element as the argument.
Tampering with the element
of Container is prohibited during the execution of the call on
Process.@key[all]. Any exception raised by Process.@key[all] is propagated.]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[3],Text=[The @ldquote@;tamper with the element@rdquote
  check is intended to prevent the Element parameter of Process from being
  replaced or deleted outside of Process. The check prevents data loss (if
  Element_Type is passed by copy) or erroneous execution (if Element_Type is an
  unconstrained type).]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[procedure] Update_Element
  (Container : @key[in out] Holder;
   Process   : @key[not null access procedure] (Element : @key[in out] Element_Type));]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1],ARef=[AI05-0262-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Container is empty,
Constraint_Error is propagated. Otherwise, Update_Element calls
Process.@key[all] with the contained element as the argument.
Tampering with the element
of Container is prohibited during the execution of the call on Process.@key[all].
Any exception raised by Process.@key[all] is propagated.]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The Element parameter of Process.@key[all] may be
  constrained even if Element_Type is unconstrained.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[@key[type] Constant_Reference_Type
      (Element : @key[not null access constant] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[type] Reference_Type (Element : @key[not null access] Element_Type) @key[is private]
   @key[with] Implicit_Dereference => Element;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[The types Constant_Reference_Type and Reference_Type
need finalization.@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
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
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Constant_Reference (Container : @key[aliased in] Holder)
   @key[return] Constant_Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Implicit_Dereference aspect) provides a convenient way to gain read access to
the contained element of a holder container.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1],ARef=[AI05-0262-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[If Container is empty, Constraint_Error is
propagated. Otherwise, Constant_Reference returns an object whose discriminant
is an access value that designates the contained element. Tampering with the
elements of Container is prohibited while the object returned by
Constant_Reference exists and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference (Container : @key[aliased in out] Holder)
   @key[return] Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Implicit_Dereference aspects) provides a convenient way to gain read and write
access to the contained element of a holder container.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1],ARef=[AI05-0262-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[If Container is empty, Constraint_Error is
propagated. Otherwise, Reference returns an object whose discriminant is an
access value that designates the contained element. Tampering with the
elements of Container is prohibited while the object returned by
Reference exists and has not been finalized.]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[procedure] Assign (Target : @key[in out] Holder; Source : @key[in] Holder);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Target denotes the same object as
Source, the operation has no effect. If Source is empty, Clear (Target) is
called. Otherwise, Replace_Element (Target, Element (Source)) is called.]}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[3],Text=[This routine exists for compatibility with the
  other containers. For a holder, @exam{Assign(A, B)} and
  @exam{A := B} behave effectively the same. (Assign Clears the Target, while
  := finalizes the Target, but these should have similar effects.)]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Copy (Source : Holder) @key[return] Holder;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Source is empty, returns an empty
holder container; otherwise, returns To_Holder (Element (Source)).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[procedure] Move (Target : @key[in out] Holder; Source : @key[in out] Holder);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Target denotes the same object
as Source, then the operation has no effect. Otherwise, the element contained
by Source (if any) is removed from Source and inserted into Target, replacing
any preexisting content. Source is empty after a successful call to Move.]}

@end{DescribeCode}

@end{StaticSem}

@begin{Bounded}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0022-1],ARef=[AI05-0069-1],ARef=[AI05-0248-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error for the actual function associated with a
generic formal subprogram, when called as part of an operation of
this package, to tamper with the element of any Holder parameter of the
operation. Either Program_Error is raised, or the operation works as
defined on the value of the Holder either prior to, or subsequent to,
some or all of the modifications to the Holder.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0027-1],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to call any subprogram declared in the visible part
of Containers.Indefinite_Holders when the associated container has been
finalized. If the operation takes Container as an @key[in out] parameter,
then it raises Constraint_Error or Program_Error. Otherwise, the operation
either proceeds as it would for an empty container, or it raises
Constraint_Error or Program_Error.]}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[Execution is erroneous if the holder container
associated with the result of a call to Reference or Constant_Reference is
finalized before the result object returned by the call to Reference or
Constant_Reference is finalized.@PDefn2{Term=(erroneous execution),Sec=(cause)}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
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

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Text=[No storage associated with a holder object shall be
lost upon assignment or scope exit.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[The execution of an @nt{assignment_statement}
for a holder container shall have the effect of copying the element (if any)
from the source holder object to the target holder object.]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0298-1]}
  @ChgAdded{Version=[3],Text=[An assignment of a holder container is a
  @ldquote@;deep@rdquote copy; that is the element is copied as well as any
  data structures. We say @ldquote@;effect of@rdquote in order to allow the
  implementation to avoid copying the element immediately if it wishes. For
  instance, an implementation that avoided copying until one of the containers
  is modified would be allowed. (Note that this implementation would
  require care, see @RefSecNum{The Generic Package Containers.Vectors} for more.)]}
@end{ImplNote}
@end{ImplReq}

@begin{ImplAdvice}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[Move should not copy the element, and should minimize
copying of internal data structures.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Containers.Indefinite_Holders.Move should not copy the element, and should
minimize copying of internal data structures.]}]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Usually that can be accomplished simply by moving
  the pointer(s) to the internal data structures from the Source holder to the
  Target holder.]}
@end{ImplNote}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[If an exception is propagated from a holder
operation, no storage should be lost, nor should the element be removed from a
holder container unless specified by the operation.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[If an exception is propagated from a holder
operation, no storage should be lost, nor should the element be removed from a
holder container unless specified by the operation.]}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is important so that programs can recover
  from errors. But we don't want to require heroic efforts, so we just require
  documentation of cases where this can't be accomplished.]}
@end{Reason}
@end{ImplAdvice}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1],ARef=[AI05-0084-1],ARef=[AI05-0265-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005} The generic package
  Containers.Indefinite_Holders is new.]}
@end{Extend2005}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0035-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  Defined some routines to @ldquote@;perform indefinite insertion@rdquote.
  This could mean that some calls to those routines would now raise
  Program_Error where they previously worked. However, this is extremely
  unlikely; see @Inconsistent2012Title in
  @RefSecNum{The Generic Package Containers.Indefinite_Vectors} for details.]}
@end{Inconsistent2012}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0110-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Clarified that tampering checks
  precede all other checks made by a subprogram (but come after those associated
  with the call).]}
@end{DiffWord2012}

@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Bounded_Vectors]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Bounded_Vectors provides a private type Vector and a set of
operations. It provides the same operations as the package Containers.Vectors
(see @RefSecNum{The Generic Package Containers.Vectors}), with the difference that the
maximum storage is bounded.]}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The declaration of the generic
library package Containers.Bounded_Vectors has the same contents and semantics
as Containers.Vectors except:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The @nt{pragma} Preelaborate is replaced with @nt{pragma} Pure.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The type Vector is declared with a
    discriminant that specifies the capacity:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key{type} Vector (Capacity : Count_Type) @key[is tagged private];]}
@end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The type Vector needs finalization if and only if
    type Element_Type needs finalization.]}

  @begin{ImplNote}
    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
    @ChgAdded{Version=[3],Text=[The type Vector cannot depend on package
    Ada.Finalization unless the element type depends on that package.
    The objects returned from the Iterator and Reference functions probably do
    depend on package Ada.Finalization. Restricted environments may need to
    avoid use of those functions and their associated types.]}
  @end{ImplNote}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[In function Copy, if the Capacity parameter is
    equal to or greater than the length of Source, the vector capacity exactly equals
    the value of the Capacity parameter.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The description of Reserve_Capacity
  is replaced with:]}
@begin{Indent}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],NoPrefix=[T],Text=[If the specified Capacity is larger
    than the capacity of Container, then Reserve_Capacity propagates Capacity_Error.
    Otherwise, the operation has no effect.]}
@end{Indent}
@end{Itemize}
@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to assign from a bounded vector object while tampering
with elements @Redundant[or cursors] of that object is prohibited. Either
Program_Error is raised by the assignment, execution proceeds with the target
object prohibiting tampering with elements @Redundant[or cursors], or execution
proceeds normally.]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Tampering with elements includes tampering
  with cursors, so we only really need to talk about tampering with elements
  here; we mention cursors for clarity.]}
@end{TheProof}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[When a bounded vector object @i<V> is
finalized, if tampering with cursors is prohibited for @i<V> other than due
to an assignment from another vector, then execution is erroneous.
@PDefn2{Term=(erroneous execution),Sec=(cause)}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is a tampering event, but since the
  implementation is not allowed to use Ada.Finalization, it is not possible in a
  pure Ada implementation to detect this error. (There is no Finalize routine
  that will be called that could make the check.) Since the check probably
  cannot be made, the bad effects that could occur (such as an iterator going
  into an infinite loop or accessing a nonexistent element) cannot be prevented
  and we have to allow anything. We do allow re-assigning an object that only
  prohibits tampering because it was copied from another object as that cannot
  cause any negative effects.]}
@end{Reason}
@end{Erron}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1],ARef=[AI05-0264-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For each instance of
Containers.Vectors and each instance of Containers.Bounded_Vectors,
if the two instances meet the following conditions,
then the output generated by
the Vector'Output or Vector'Write subprograms of either instance
shall be readable by the Vector'Input
or Vector'Read of the other instance, respectively:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Text=[the Element_Type parameters of the two instances
    are statically matching subtypes of the same type; and]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[the output generated by Element_Type'Output or
    Element_Type'Write is readable by Element_Type'Input or Element_Type'Read,
    respectively (where Element_Type denotes the type of the two actual
    Element_Type parameters); and]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[the preceding two conditions also hold for the
    Index_Type parameters of the instances.]}
@end{Itemize}
@end{ImplReq}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[Bounded vector objects should be implemented without
implicit pointers or dynamic allocation.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Bounded vector objects should be implemented without
implicit pointers or dynamic allocation.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The implementation advice for procedure Move to
minimize copying does not apply.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The implementation advice for procedure Move to
minimize copying does not apply to bounded vectors.]}]}

@end{ImplAdvice}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0160-1],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005} The generic package
  Containers.Bounded_Vectors is new.]}
@end{Extend2005}


@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Bounded_Doubly_Linked_Lists]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Bounded_Doubly_Linked_Lists provides a private type List
and a set of operations. It provides the same operations as the
package Containers.Doubly_Linked_Lists
(see @RefSecNum{The Generic Package Containers.Doubly_Linked_Lists}), with the
difference that the maximum storage is bounded.]}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The declaration of the generic
library package Containers.Bounded_Doubly_Linked_Lists has the same contents and semantics
as Containers.Doubly_Linked_Lists except:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The @nt{pragma} Preelaborate is replaced with @nt{pragma} Pure.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The type List is declared with a
    discriminant that specifies the capacity (maximum number of elements)
    as follows:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key{type} List (Capacity : Count_Type) @key[is tagged private];]}
@end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The type List needs finalization if and only if
    type Element_Type needs finalization.]}

  @begin{ImplNote}
    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
    @ChgAdded{Version=[3],Text=[The type List cannot depend on package
    Ada.Finalization unless the element type depends on that package.
    The objects returned from the Iterator and Reference functions probably do
    depend on package Ada.Finalization. Restricted environments may need to
    avoid use of those functions and their associated types.]}
  @end{ImplNote}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The allocation of internal storage includes a
    check that the capacity is not exceeded, and Capacity_Error is raised
    if this check fails.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[In procedure Assign, if Source length is greater
    than Target capacity, then Capacity_Error is propagated.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The function Copy is replaced with:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key[function] @AdaSubDefn{Copy} (Source : List; Capacity : Count_Type := 0)
     @key[return] List;]}
@end{Example}
@begin{Indent}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Noprefix=[T],Text=[If Capacity is 0, then the list capacity is the length of
    Source; if Capacity is equal to or greater than the length of Source,
    the list capacity equals the value of the Capacity parameter;
    otherwise, the operation propagates Capacity_Error.]}
@end{Indent}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[In the three-parameter procedure Splice whose
    Source has type List, if the sum of the length of Target and the length
    of Source is greater than the capacity of Target, then Splice propagates
    Capacity_Error.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[In the four-parameter procedure Splice, if
    the length of Target equals the capacity of Target, then Splice
    propagates Capacity_Error.]}

@end{Itemize}
@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to assign from a bounded list object while tampering
with elements @Redundant[or cursors] of that object is prohibited. Either
Program_Error is raised by the assignment, execution proceeds with the target
object prohibiting tampering with elements @Redundant[or cursors], or execution
proceeds normally.]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Tampering with elements includes tampering
  with cursors, so we only really need to talk about tampering with elements
  here; we mention cursors for clarity.]}
@end{TheProof}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[When a bounded list object @i<L> is
finalized, if tampering with cursors is prohibited for @i<L> other than due
to an assignment from another list, then execution is erroneous.
@PDefn2{Term=(erroneous execution),Sec=(cause)}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is a tampering event, but since the
  implementation is not allowed to use Ada.Finalization, it is not possible in a
  pure Ada implementation to detect this error. (There is no Finalize routine
  that will be called that could make the check.) Since the check probably
  cannot be made, the bad effects that could occur (such as an iterator going
  into an infinite loop or accessing a nonexistent element) cannot be prevented
  and we have to allow anything. We do allow re-assigning an object that only
  prohibits tampering because it was copied from another object as that cannot
  cause any negative effects.]}
@end{Reason}
@end{Erron}


@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1],ARef=[AI05-0264-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For each instance of
Containers.Doubly_Linked_Lists and each instance of
Containers.Bounded_Doubly_Linked_Lists,
if the two instances meet the following conditions,
then the output generated by
the List'Output or List'Write subprograms of either instance
shall be readable by the List'Input
or List'Read of the other instance, respectively:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Text=[the Element_Type parameters of the two instances
    are statically matching subtypes of the same type; and]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[the output generated by Element_Type'Output or
    Element_Type'Write is readable by Element_Type'Input or Element_Type'Read,
    respectively (where Element_Type denotes the type of the two actual
    Element_Type parameters).]}
@end{Itemize}
@end{ImplReq}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[Bounded list objects should be implemented without
implicit pointers or dynamic allocation.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Bounded list objects should be implemented without
implicit pointers or dynamic allocation.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The implementation advice for procedure Move to
minimize copying does not apply.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The implementation advice for procedure Move to
minimize copying does not apply to bounded lists.]}]}

@end{ImplAdvice}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0160-1],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005} The generic package
  Containers.Bounded_Doubly_Linked_Lists is new.]}
@end{Extend2005}


@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Bounded_Hashed_Maps]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Bounded_Hashed_Maps provides a private type Map
and a set of operations. It provides the same operations as the
package Containers.Hashed_Maps
(see @RefSecNum{The Generic Package Containers.Hashed_Maps}), with the
difference that the maximum storage is bounded.]}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The declaration of the generic
library package Containers.Bounded_Hashed_Maps has the same contents and semantics
as Containers.Hashed_Maps except:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The @nt{pragma} Preelaborate is replaced with @nt{pragma} Pure.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The type Map is declared with
    discriminants that specify both the capacity (number of elements) and
    modulus (number of distinct hash values) of the hash table as follows:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key[type] Map (Capacity : Count_Type;
            Modulus  : Hash_Type) @key[is tagged private];]}
@end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The type Map needs finalization if and only if type
      Key_Type or type Element_Type needs finalization.]}

  @begin{ImplNote}
    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
    @ChgAdded{Version=[3],Text=[The type Map cannot depend on package
    Ada.Finalization unless the element or key type depends on that package.
    The objects returned from the Iterator and Reference functions probably do
    depend on package Ada.Finalization. Restricted environments may need to
    avoid use of those functions and their associated types.]}
  @end{ImplNote}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The description of Reserve_Capacity
  is replaced with:]}
@begin{Indent}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],NoPrefix=[T],Text=[If the specified Capacity is larger
    than the capacity of Container, then Reserve_Capacity propagates Capacity_Error.
    Otherwise, the operation has no effect.]}
@end{Indent}


  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[An additional operation is added immediately following Reserve_Capacity:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key[function] @AdaSubDefn{Default_Modulus} (Capacity : Count_Type) @key[return] Hash_Type;]}
@end{Example}
@begin{Indent}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Noprefix=[T],Text=[Default_Modulus returns an
    implementation-defined value for the number of distinct hash values to be
    used for the given capacity (maximum number of elements).]}
@end{Indent}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The function Copy is replaced with:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key[function] @AdaSubDefn{Copy} (Source   : Map;
                 Capacity : Count_Type := 0;
                 Modulus  : Hash_Type := 0) @key[return] Map;]}
@end{Example}
@begin{Indent}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0264-1]}
  @ChgAdded{Version=[3],Noprefix=[T],Text=[Returns a map with key/element pairs initialized from the values
    in Source. If Capacity is 0, then the map capacity is the
    length of Source; if Capacity is equal to or greater than
    the length of Source, the map capacity is the value of the Capacity
    parameter; otherwise, the operation propagates Capacity_Error.  If
    the Modulus argument is 0, then the map modulus is the value
    returned by a call to Default_Modulus with the map capacity as its
    argument; otherwise, the map modulus is the value of the Modulus parameter.]}
@end{Indent}

@end{Itemize}
@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to assign from a bounded map object while tampering
with elements @Redundant[or cursors] of that object is prohibited. Either
Program_Error is raised by the assignment, execution proceeds with the target
object prohibiting tampering with elements @Redundant[or cursors], or execution
proceeds normally.]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Tampering with elements includes tampering
  with cursors, so we only really need to talk about tampering with elements
  here; we mention cursors for clarity.]}
@end{TheProof}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[When a bounded map object @i<M> is
finalized, if tampering with cursors is prohibited for @i<M> other than due
to an assignment from another map, then execution is erroneous.
@PDefn2{Term=(erroneous execution),Sec=(cause)}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is a tampering event, but since the
  implementation is not allowed to use Ada.Finalization, it is not possible in a
  pure Ada implementation to detect this error. (There is no Finalize routine
  that will be called that could make the check.) Since the check probably
  cannot be made, the bad effects that could occur (such as an iterator going
  into an infinite loop or accessing a nonexistent element) cannot be prevented
  and we have to allow anything. We do allow re-assigning an object that only
  prohibits tampering because it was copied from another object as that cannot
  cause any negative effects.]}
@end{Reason}
@end{Erron}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1],ARef=[AI05-0264-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For each instance of
Containers.Hashed_Maps and each instance of Containers.Bounded_Hashed_Maps,
if the two instances meet the following conditions,
then the output generated by
the Map'Output or Map'Write subprograms of either instance
shall be readable by the Map'Input
or Map'Read of the other instance, respectively:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Text=[the Element_Type parameters of the two instances
    are statically matching subtypes of the same type; and]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[the output generated by Element_Type'Output or
    Element_Type'Write is readable by Element_Type'Input or Element_Type'Read,
    respectively (where Element_Type denotes the type of the two actual
    Element_Type parameters); and]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[the preceding two conditions also hold for the
    Key_Type parameters of the instances.]}
@end{Itemize}
@end{ImplReq}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[Bounded hashed map objects should be implemented without
implicit pointers or dynamic allocation.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Bounded hashed map objects should be implemented without
implicit pointers or dynamic allocation.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The implementation advice for procedure Move to
minimize copying does not apply.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The implementation advice for procedure Move to
minimize copying does not apply to bounded hashed maps.]}]}

@end{ImplAdvice}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0160-1],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005} The generic package
  Containers.Bounded_Hashed_Maps is new.]}
@end{Extend2005}


@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Bounded_Ordered_Maps]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Bounded_Ordered_Maps provides a private type Map
and a set of operations. It provides the same operations as the
package Containers.Ordered_Maps
(see @RefSecNum{The Generic Package Containers.Ordered_Maps}), with the
difference that the maximum storage is bounded.]}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The declaration of the generic
library package Containers.Bounded_Ordered_Maps has the same contents and semantics
as Containers.Ordered_Maps except:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The @nt{pragma} Preelaborate is replaced with @nt{pragma} Pure.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The type Map is declared with a
    discriminant that specifies the capacity (maximum number of elements)
    as follows:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key[type] Map (Capacity : Count_Type) @key[is tagged private];]}
@end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The type Map needs finalization if and only if
     type Key_Type or type Element_Type needs finalization.]}

  @begin{ImplNote}
    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
    @ChgAdded{Version=[3],Text=[The type Map cannot depend on package
    Ada.Finalization unless the element type depends on that package.
    The objects returned from the Iterator and Reference functions probably do
    depend on package Ada.Finalization. Restricted environments may need to
    avoid use of those functions and their associated types.]}
  @end{ImplNote}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The allocation of a new node includes a check that
    the capacity is not exceeded, and Capacity_Error is raised if this check
    fails.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[In procedure Assign, if Source length is greater
    than Target capacity, then Capacity_Error is propagated.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The function Copy is replaced with:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key[function] @AdaSubDefn{Copy} (Source   : Map;
                 Capacity : Count_Type := 0) @key[return] Map;]}
@end{Example}
@begin{Indent}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Noprefix=[T],Text=[Returns a map with key/element pairs
    initialized from the values in Source. If Capacity is 0, then the map
    capacity is the length of Source; if Capacity is equal to or greater than
    the length of Source, the map capacity is the specified value; otherwise, the
    operation propagates Capacity_Error.]}
@end{Indent}

@end{Itemize}
@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to assign from a bounded map object while tampering
with elements @Redundant[or cursors] of that object is prohibited. Either
Program_Error is raised by the assignment, execution proceeds with the target
object prohibiting tampering with elements @Redundant[or cursors], or execution
proceeds normally.]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Tampering with elements includes tampering
  with cursors, so we only really need to talk about tampering with elements
  here; we mention cursors for clarity.]}
@end{TheProof}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[When a bounded map object @i<M> is
finalized, if tampering with cursors is prohibited for @i<M> other than due
to an assignment from another map, then execution is erroneous.
@PDefn2{Term=(erroneous execution),Sec=(cause)}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is a tampering event, but since the
  implementation is not allowed to use Ada.Finalization, it is not possible in a
  pure Ada implementation to detect this error. (There is no Finalize routine
  that will be called that could make the check.) Since the check probably
  cannot be made, the bad effects that could occur (such as an iterator going
  into an infinite loop or accessing a nonexistent element) cannot be prevented
  and we have to allow anything. We do allow re-assigning an object that only
  prohibits tampering because it was copied from another object as that cannot
  cause any negative effects.]}
@end{Reason}
@end{Erron}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1],ARef=[AI05-0264-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For each instance of
Containers.Ordered_Maps and each instance of Containers.Bounded_Ordered_Maps,
if the two instances meet the following conditions,
then the output generated by
the Map'Output or Map'Write subprograms of either instance
shall be readable by the Map'Input
or Map'Read of the other instance, respectively:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Text=[the Element_Type parameters of the two instances
    are statically matching subtypes of the same type; and]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[the output generated by Element_Type'Output or
    Element_Type'Write is readable by Element_Type'Input or Element_Type'Read,
    respectively (where Element_Type denotes the type of the two actual
    Element_Type parameters); and]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[the preceding two conditions also hold for the
    Key_Type parameters of the instances.]}
@end{Itemize}
@end{ImplReq}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[Bounded ordered map objects should be implemented
without implicit pointers or dynamic allocation.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Bounded ordered map objects should be implemented without
implicit pointers or dynamic allocation.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The implementation advice for procedure Move to
minimize copying does not apply.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The implementation advice for procedure Move to
minimize copying does not apply to bounded ordered maps.]}]}

@end{ImplAdvice}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0160-1],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005} The generic package
  Containers.Bounded_Ordered_Maps is new.]}
@end{Extend2005}


@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Bounded_Hashed_Sets]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Bounded_Hashed_Sets provides a private type Set
and a set of operations. It provides the same operations as the
package Containers.Hashed_Sets
(see @RefSecNum{The Generic Package Containers.Hashed_Sets}), with the
difference that the maximum storage is bounded.]}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The declaration of the generic
library package Containers.Bounded_Hashed_Sets has the same contents and semantics
as Containers.Hashed_Sets except:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The @nt{pragma} Preelaborate is replaced with @nt{pragma} Pure.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The type Set is declared with
    discriminants that specify both the capacity (number of elements) and
    modulus (number of distinct hash values) of the hash table as follows:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key[type] Set (Capacity : Count_Type;
            Modulus  : Hash_Type) @key[is tagged private];]}
@end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The type Set needs finalization if and only if
    type Element_Type needs finalization.]}

  @begin{ImplNote}
    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
    @ChgAdded{Version=[3],Text=[The type Set cannot depend on package
    Ada.Finalization unless the element or key type depends on that package.
    The objects returned from the Iterator and Reference functions probably do
    depend on package Ada.Finalization. Restricted environments may need to
    avoid use of those functions and their associated types.]}
  @end{ImplNote}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The description of Reserve_Capacity
  is replaced with:]}
@begin{Indent}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],NoPrefix=[T],Text=[If the specified Capacity is larger
    than the capacity of Container, then Reserve_Capacity propagates Capacity_Error.
    Otherwise, the operation has no effect.]}
@end{Indent}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[An additional operation is added immediately following Reserve_Capacity:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key[function] @AdaSubDefn{Default_Modulus} (Capacity : Count_Type) @key[return] Hash_Type;]}
@end{Example}
@begin{Indent}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Noprefix=[T],Text=[Default_Modulus returns an
    implementation-defined value for the number of distinct hash values to be
    used for the given capacity (maximum number of elements).]}
@end{Indent}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The function Copy is replaced with:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key[function] @AdaSubDefn{Copy} (Source   : Set;
                 Capacity : Count_Type := 0;
                 Modulus  : Hash_Type := 0) @key[return] Set;]}
@end{Example}
@begin{Indent}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0264-1]}
  @ChgAdded{Version=[3],Noprefix=[T],Text=[Returns a set whose elements are
    initialized from the values in Source. If Capacity is 0, then the set
    capacity is the length of Source; if Capacity is equal to or greater than
    the length of Source, the set capacity is the value of the Capacity parameter;
    otherwise, the operation propagates Capacity_Error. If the Modulus argument is
    0, then the set modulus is the value returned by a call to Default_Modulus
    with the set capacity as its argument; otherwise, the set modulus is the
    value of the Modulus parameter.]}
@end{Indent}

@end{Itemize}
@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to assign from a bounded set object while tampering
with elements @Redundant[or cursors] of that object is prohibited. Either
Program_Error is raised by the assignment, execution proceeds with the target
object prohibiting tampering with elements @Redundant[or cursors], or execution
proceeds normally.]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Tampering with elements includes tampering
  with cursors, so we only really need to talk about tampering with elements
  here; we mention cursors for clarity.]}
@end{TheProof}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[When a bounded set object @i<S> is
finalized, if tampering with cursors is prohibited for @i<S> other than due
to an assignment from another set, then execution is erroneous.
@PDefn2{Term=(erroneous execution),Sec=(cause)}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is a tampering event, but since the
  implementation is not allowed to use Ada.Finalization, it is not possible in a
  pure Ada implementation to detect this error. (There is no Finalize routine
  that will be called that could make the check.) Since the check probably
  cannot be made, the bad effects that could occur (such as an iterator going
  into an infinite loop or accessing a nonexistent element) cannot be prevented
  and we have to allow anything. We do allow re-assigning an object that only
  prohibits tampering because it was copied from another object as that cannot
  cause any negative effects.]}
@end{Reason}
@end{Erron}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1],ARef=[AI05-0264-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For each instance of
Containers.Hashed_Sets and each instance of Containers.Bounded_Hashed_Sets,
if the two instances meet the following conditions,
then the output generated by
the Set'Output or Set'Write subprograms of either instance
shall be readable by the Set'Input
or Set'Read of the other instance, respectively:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Text=[the Element_Type parameters of the two instances
    are statically matching subtypes of the same type; and]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[the output generated by Element_Type'Output or
    Element_Type'Write is readable by Element_Type'Input or Element_Type'Read,
    respectively (where Element_Type denotes the type of the two actual
    Element_Type parameters).]}
@end{Itemize}
@end{ImplReq}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[Bounded hashed set objects should be implemented
without implicit pointers or dynamic allocation.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Bounded hashed set objects should be implemented without
implicit pointers or dynamic allocation.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The implementation advice for procedure Move to
minimize copying does not apply.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The implementation advice for procedure Move to
minimize copying does not apply to bounded hashed sets.]}]}

@end{ImplAdvice}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0160-1],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005} The generic package
  Containers.Bounded_Hashed_Sets is new.]}
@end{Extend2005}


@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Bounded_Ordered_Sets]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Bounded_Ordered_Sets provides a private type Set
and a set of operations. It provides the same operations as the
package Containers.Ordered_Sets
(see @RefSecNum{The Generic Package Containers.Ordered_Sets}), with the
difference that the maximum storage is bounded.]}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The declaration of the generic
library package Containers.Bounded_Ordered_Sets has the same contents and semantics
as Containers.Ordered_Sets except:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The @nt{pragma} Preelaborate is replaced with @nt{pragma} Pure.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The type Set is declared with a
    discriminant that specifies the capacity (maximum number of elements)
    as follows:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key{type} Set (Capacity : Count_Type) @key[is tagged private];]}
@end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The type Set needs finalization if and only if
    type Element_Type needs finalization.]}

  @begin{ImplNote}
    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
    @ChgAdded{Version=[3],Text=[The type Set cannot depend on package
    Ada.Finalization unless the element type depends on that package. The
    objects returned from the Iterator and Reference functions probably do
    depend on package Ada.Finalization. Restricted environments may need to
    avoid use of those functions and their associated types.]}
  @end{ImplNote}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[If Insert (or Include) adds an element, a check is
    made that the capacity is not exceeded, and Capacity_Error is raised
    if this check fails.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[In procedure Assign, if Source length is greater
    than Target capacity, then Capacity_Error is propagated.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The function Copy is replaced with:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key[function] @AdaSubDefn{Copy} (Source   : Set;
                 Capacity : Count_Type := 0) @key[return] Set;]}
@end{Example}
@begin{Indent}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Noprefix=[T],Text=[Returns a set whose elements
    are initialized from the values in Source. If Capacity is 0, then the set
    capacity is the length of Source; if Capacity is equal to or greater than
    the length of Source, the set capacity is the specified value; otherwise, the
    operation propagates Capacity_Error.]}
@end{Indent}

@end{Itemize}
@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to assign from a bounded set object while tampering
with elements @Redundant[or cursors] of that object is prohibited. Either
Program_Error is raised by the assignment, execution proceeds with the target
object prohibiting tampering with elements @Redundant[or cursors], or execution
proceeds normally.]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Tampering with elements includes tampering
  with cursors, so we only really need to talk about tampering with elements
  here; we mention cursors for clarity.]}
@end{TheProof}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[When a bounded set object @i<S> is
finalized, if tampering with cursors is prohibited for @i<S> other than due
to an assignment from another set, then execution is erroneous.
@PDefn2{Term=(erroneous execution),Sec=(cause)}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is a tampering event, but since the
  implementation is not allowed to use Ada.Finalization, it is not possible in a
  pure Ada implementation to detect this error. (There is no Finalize routine
  that will be called that could make the check.) Since the check probably
  cannot be made, the bad effects that could occur (such as an iterator going
  into an infinite loop or accessing a nonexistent element) cannot be prevented
  and we have to allow anything. We do allow re-assigning an object that only
  prohibits tampering because it was copied from another object as that cannot
  cause any negative effects.]}
@end{Reason}
@end{Erron}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1],ARef=[AI05-0264-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For each instance of
Containers.Ordered_Sets and each instance of Containers.Bounded_Ordered_Sets,
if the two instances meet the following conditions,
then the output generated by
the Set'Output or Set'Write subprograms of either instance
shall be readable by the Set'Input
or Set'Read of the other instance, respectively:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Text=[the Element_Type parameters of the two instances
    are statically matching subtypes of the same type; and]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[the output generated by Element_Type'Output or
    Element_Type'Write is readable by Element_Type'Input or Element_Type'Read,
    respectively (where Element_Type denotes the type of the two actual
    Element_Type parameters).]}
@end{Itemize}
@end{ImplReq}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[Bounded ordered set objects should be implemented
without implicit pointers or dynamic allocation.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Bounded ordered set objects should be implemented without
implicit pointers or dynamic allocation.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The implementation advice for procedure Move to
minimize copying does not apply.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The implementation advice for procedure Move to
minimize copying does not apply to bounded ordered sets.]}]}

@end{ImplAdvice}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0160-1],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005} The generic package
  Containers.Bounded_Ordered_Sets is new.]}
@end{Extend2005}


@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Bounded_Multiway_Trees]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Bounded_Multiway_Trees provides a private type Tree and a set of
operations. It provides the same operations as the package
Containers.Multiway_Trees (see
@RefSecNum{The Generic Package Containers.Multiway_Trees}), with the difference
that the maximum storage is bounded.]}

@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The declaration of the generic
library package Containers.Bounded_Multiway_Trees has the same contents and
semantics as Containers.Multiway_Trees except:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The @nt{pragma} Preelaborate is replaced with @nt{pragma} Pure.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The type Tree is declared with a
    discriminant that specifies the capacity (maximum number of elements)
    as follows:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key{type} Tree (Capacity : Count_Type) @key[is tagged private];]}
@end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The type Tree needs finalization if and only if
    type Element_Type needs finalization.]}

  @begin{ImplNote}
    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
    @ChgAdded{Version=[3],Text=[The type Tree cannot depend on package
    Ada.Finalization unless the element type depends on that package. The
    objects returned from the Iterator and Reference functions probably do
    depend on package Ada.Finalization. Restricted environments may need to
    avoid use of those functions and their associated types.]}
  @end{ImplNote}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The allocation of internal storage includes a
    check that the capacity is not exceeded, and Capacity_Error is raised if
    this check fails.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[In procedure Assign, if Source length is greater
    than Target capacity, then Capacity_Error is propagated.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[Function Copy is declared as follows:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0056-1]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key{function} Copy (Source : Tree; Capacity : Count_Type := 0)
     @key{return} @Chg{Version=[4],New=[Tree],Old=[List]};]}
@end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Noprefix=[T],Text=[If Capacity is 0, then the tree
    capacity is the count of Source; if Capacity is equal to or greater than
    Source.Count, the tree capacity equals the value of the Capacity parameter;
    otherwise, the operation propagates Capacity_Error.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Text=[In the five-parameter procedure Splice_Subtree, if
    Source is not the same object as Target, and if the sum of Target.Count and
    Subtree_Node_Count (Position) is greater than Target.Capacity, then
    Splice_Subtree propagates Capacity_Error.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Text=[In the five-parameter procedure Splice_Children,
    if Source is not the same object as Target, and if the sum of Target.Count
    and Subtree_Node_Count (Source_Parent)-1 is greater than Target.Capacity,
    then Splice_Children propagates Capacity_Error.]}
@end{Itemize}
@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to assign from a bounded tree object while tampering
with elements @Redundant[or cursors] of that object is prohibited. Either
Program_Error is raised by the assignment, execution proceeds with the target
object prohibiting tampering with elements @Redundant[or cursors], or execution
proceeds normally.]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Tampering with elements includes tampering
  with cursors, so we only really need to talk about tampering with elements
  here; we mention cursors for clarity.]}
@end{TheProof}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0265-1]}
@ChgAdded{Version=[3],Text=[When a bounded tree object @i<T> is
finalized, if tampering with cursors is prohibited for @i<T> other than due
to an assignment from another tree, then execution is erroneous.
@PDefn2{Term=(erroneous execution),Sec=(cause)}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is a tampering event, but since the
  implementation is not allowed to use Ada.Finalization, it is not possible in a
  pure Ada implementation to detect this error. (There is no Finalize routine
  that will be called that could make the check.) Since the check probably
  cannot be made, the bad effects that could occur (such as an iterator going
  into an infinite loop or accessing a nonexistent element) cannot be prevented
  and we have to allow anything. We do allow re-assigning an object that only
  prohibits tampering because it was copied from another object as that cannot
  cause any negative effects.]}
@end{Reason}
@end{Erron}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1],ARef=[AI05-0264-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For each instance of
Containers.Multiway_Trees and each instance of Containers.Bounded_Multiway_Trees,
if the two instances meet the following conditions,
then the output generated by
the Tree'Output or Tree'Write subprograms of either instance
shall be readable by the Tree'Input
or Tree'Read of the other instance, respectively:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Text=[the Element_Type parameters of the two instances
    are statically matching subtypes of the same type; and]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[the output generated by Element_Type'Output or
    Element_Type'Write is readable by Element_Type'Input or Element_Type'Read,
    respectively (where Element_Type denotes the type of the two actual
    Element_Type parameters).]}
@end{Itemize}
@end{ImplReq}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[Bounded tree objects should be implemented without
implicit pointers or dynamic allocation.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Bounded tree objects should be implemented without
implicit pointers or dynamic allocation.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[The implementation advice for procedure Move to
minimize copying does not apply.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The implementation advice for procedure Move to
minimize copying does not apply to bounded trees.]}]}
@end{ImplAdvice}


@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1],ARef=[AI05-0184-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The generic package Containers.Bounded_Multiway_Trees is new.]}
@end{Extend2005}


@RMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledAddedSubclause{Version=[2],Name=[Array Sorting]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0001-1]}
@ChgAdded{Version=[2],Text=[The language-defined generic procedures
Containers.@!Generic_@!Array_Sort@Chg{Version=[3],New=[,],Old=[ and]}
Containers.@!Generic_@!Constrained_@!Array_Sort@Chg{Version=[3],New=[, and
Containers.@!Generic_Sort],Old=[]}
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
@key{procedure} Ada.Containers.Generic_Array_Sort (Container : @key{in out} Array_Type);@SubChildUnit{Parent=[Ada.Containers],Child=[Generic_Array_Sort]}
@key{pragma} Pure(Ada.Containers.Generic_Array_Sort);]}
@end{Example}

@begin{DescribeCode}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Reorders the elements of Container such that the elements are
sorted smallest first as determined by the generic formal "<" operator
provided. Any exception raised during evaluation of "<" is propagated.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0044-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
"<" of Generic_Array_Sort is expected to return the same value each time it is
called with a particular pair of element values. It should define a strict
@Chg{Version=[3],New=[weak ],Old=[]}ordering relationship@Chg{Version=[3],
New=[ (see @RefSecNum{Containers})],Old=[, that is, be irreflexive, asymmetric,
and transitive]}; it
should not modify Container. If the actual for "<" behaves in some other
manner, the behavior of the instance of Generic_Array_Sort is unspecified.
@Chg{Version=[3],New=[The number of],Old=[How many]}
times Generic_Array_Sort calls "<" is unspecified.@PDefn{unspecified}]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This implies swapping the elements, usually
  including an intermediate copy. This of course means that the elements will be
  copied. Since the elements are nonlimited, this usually will not be a problem.
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
procedure Containers.@!Generic_@!Constrained_@!Array_Sort has the following
declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{generic}
   @key{type} Index_Type @key{is} (<>);
   @key{type} Element_Type @key{is private};
   @key{type} Array_Type @key{is array} (Index_Type) @key{of} Element_Type;
   @key{with function} "<" (Left, Right : Element_Type)
      @key{return} Boolean @key{is} <>;
@key{procedure} Ada.Containers.Generic_Constrained_Array_Sort@SubChildUnit{Parent=[Ada.Containers],Child=[Generic_Constrained_Array_Sort]}
      (Container : @key{in out} Array_Type);
@key{pragma} Pure(Ada.Containers.Generic_Constrained_Array_Sort);]}
@end{Example}

@begin{DescribeCode}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Reorders the elements of Container such that the
elements are sorted smallest first as determined by the generic formal "<"
operator provided. Any exception raised during evaluation of "<" is
propagated.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0044-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
"<" of Generic_Constrained_Array_Sort is expected to return the same value each
time it is called with a particular pair of element values. It should define a
strict @Chg{Version=[3],New=[weak ],Old=[]}ordering relationship@Chg{Version=[3],
New=[ (see @RefSecNum{Containers})],Old=[, that is, be irreflexive, asymmetric,
and transitive]}; it should not modify Container. If the actual for "<" behaves in
some other manner, the behavior of the instance of
Generic_Constrained_Array_Sort is unspecified. @Chg{Version=[3],New=[The number
of],Old=[How many]} times Generic_Constrained_Array_Sort calls "<" is
unspecified.@PDefn{unspecified}]}

@end{DescribeCode}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The generic library
procedure Containers.@!Generic_@!Sort has the following declaration:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0056-1]}
@ChgAdded{Version=[3],Text=[@key{generic}
   @key{type} Index_Type @key{is} (<>);
   @key{with function} Before (Left, Right : Index_Type) @key{return} Boolean;
   @key{with procedure} Swap (Left, Right : @Chg{Version=[4],New=[@key[in] ],Old=[]}Index_Type);
@key{procedure} Ada.Containers.Generic_Sort@SubChildUnit{Parent=[Ada.Containers],Child=[Generic_Sort]}
      (First, Last : Index_Type'Base);
@key{pragma} Pure(Ada.Containers.Generic_Sort);]}
@end{Example}

@begin{DescribeCode}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[Reorders the elements of an indexable structure,
over the range First .. Last, such that the elements are sorted in the ordering
determined by the generic formal function Before; Before should return True if
Left is to be sorted before Right. The generic formal Before
compares the elements having the given indices, and the generic formal Swap
exchanges the values of the indicated elements. Any exception raised during
evaluation of Before or Swap is propagated.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The actual function for the generic formal function Before of
Generic_Sort is expected to return the same value each time it is
called with index values that identify a particular pair of element values.
It should define a strict weak ordering relationship (see @RefSecNum{Containers});
it should not modify the elements. The actual function for the generic formal
Swap should exchange the values of the indicated elements. If the actual for
either Before or Swap behaves in some other manner, the behavior of
Generic_Sort is unspecified. The number of times the Generic_Sort calls Before
or Swap is unspecified.@PDefn{unspecified}]}

@end{DescribeCode}

@end{StaticSem}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[
The worst-case time complexity of a call on an instance of
Containers.Generic_Array_Sort or
Containers.Generic_Constrained_Array_Sort should be @i{O}(@i<N>**2) or better,
and the average time complexity should be better
than @i{O}(@i<N>**2), where @i<N> is the length of the Container parameter.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Containers.Generic_Array_Sort and Containers.Generic_Constrained_Array_Sort
should have an average time complexity better than @i{O}(@i{N}**2) and worst case no
worse than @i{O}(@i{N}**2).]}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[In other words, we're requiring the use of
  a sorting algorithm better than @i{O}(@i<N>**2), such as Quicksort. No bubble
  sorts allowed!]}
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

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[
The worst-case time complexity of a call on an instance of
Containers.Generic_Sort should be @i{O}(@i<N>**2) or better,
and the average time complexity should be better
than @i{O}(@i<N>**2), where @i<N> is the difference between the Last and First
parameters plus 1.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Containers.Generic_Sort
should have an average time complexity better than @i{O}(@i{N}**2) and worst case no
worse than @i{O}(@i{N}**2).]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[Containers.Generic_Sort
should minimize calls to the generic formal Swap.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Containers.Generic_Sort should minimize calls to the generic formal Swap.]}]}
@end{ImplAdvice}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The generic procedures Containers.Generic_Array_Sort and
  Containers.Generic_Constrained_Array_Sort are new.]}
@end{Extend95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005} The generic
  procedure Containers.Generic_Sort is new.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0044-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Redefined "<" actuals
  to require a strict weak ordering; the old definition allowed
  indeterminant comparisons that would not have worked in a sort.]}
@end{DiffWord2005}



@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Synchronized_Queue_Interfaces]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Synchronized_Queue_Interfaces provides interface type Queue, and a
set of operations for that type. Interface Queue specifies a first-in, first-out
queue.]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The generic library
package Containers.Synchronized_Queue_Interfaces has the following declaration:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[generic]
   @key[type] Element_Type @key[is private];
@key[package] Ada.Containers.Synchronized_Queue_Interfaces @key[is]@ChildUnit{Parent=[Ada.Containers],Child=[Synchronized_Queue_Interfaces]}
   @key[pragma] Pure(Synchronized_Queue_Interfaces);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Queue} @key[is synchronized interface];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Enqueue}
     (Container : @key[in out] Queue;
      New_Item  : @key[in]     Element_Type) @key[is abstract]
       @key[with] Synchronization => By_Entry;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Dequeue}
     (Container : @key[in out] Queue;
      Element   :    @key[out] Element_Type) @key[is abstract]
       @key[with] Synchronization => By_Entry;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Current_Use} (Container : Queue) @key[return] Count_Type @key[is abstract];
   @key[function] @AdaSubDefn{Peak_Use} (Container : Queue) @key[return] Count_Type @key[is abstract];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{end} Ada.Containers.Synchronized_Queue_Interfaces;]}
@end{Example}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[procedure] Enqueue
  (Container : @key[in out] Queue;
   New_Item  : @key[in]     Element_Type) @key[is abstract];]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1],ARef=[AI05-0262-1],ARef=[AI05-0264-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[A queue type that implements this
interface is allowed to have a bounded @i<capacity>@Defn2{Term=[capacity],Sec=[of a queue]}.
If the queue object has a bounded
capacity, and the number of existing elements equals the capacity, then Enqueue
blocks until storage becomes available; otherwise, Enqueue does not block. In any
case, it then copies New_Item onto the queue.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[procedure] Dequeue
  (Container : @key[in out] Queue;
   Element   :    @key[out] Element_Type) @key[is abstract];]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1],ARef=[AI05-0251-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If the queue is empty, then Dequeue
blocks until an item becomes available. In any case, it then assigns the
element at the head of the queue to Element, and removes it from the queue.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Current_Use (Container : Queue) @key[return] Count_Type @key[is abstract];]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the number of elements
currently in the queue.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Peak_Use (Container : Queue) @key[return] Count_Type @key[is abstract];]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the maximum number of
elements that have been in the queue at any one time.]}

@end{DescribeCode}

@end{StaticSem}

@begin{Notes}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0251-1]}
@ChgAdded{Version=[3],Text=[Unlike other language-defined containers, there are
no queues whose element types are indefinite. Elements of an indefinite type can
be handled by defining the element of the queue to be a holder container (see
@RefSecNum{The Generic Package Containers.Indefinite_Holders}) of the indefinite
type, or to be an explicit access type that designates the indefinite type.]}

  @begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[There are no indefinite queues, as a useful
  definition for Dequeue is not possible. Dequeue cannot be a function, as Ada
  does not have entries that are functions (thus conditional and timed calls
  would not be possible). Moreover, protected functions do not allow modifying
  the queue object (thus it doesn't work even if we decided we didn't care about
  conditional and timed calls). If Dequeue is an entry, then the dequeued object
  would have to be an @key[out] parameter and that would require the queue client to
  guess the tag and constraints of the value that will be dequeued (otherwise
  Constraint_Error would be raised), and that is rarely going to be possible.]}
  @end{Reason}
@end{Notes}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1],ARef=[AI05-0251-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005} The generic
  package Containers.Synchronized_Queue_Interfaces is new.]}
@end{Extend2005}


@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Unbounded_Synchronized_Queues]}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Unbounded_Synchronized_Queues provides type Queue, which implements
the interface type Containers.Synchronized_Queue_Interfaces.Queue.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[with] System;
@key[with] Ada.Containers.Synchronized_Queue_Interfaces;
@key[generic]
   @key[with package] Queue_Interfaces @key[is new] Ada.Containers.Synchronized_Queue_Interfaces (<>);
   Default_Ceiling : System.Any_Priority := System.Priority'Last;
@key[package] Ada.Containers.Unbounded_Synchronized_Queues @key[is]@ChildUnit{Parent=[Ada.Containers],Child=[Unbounded_Synchronized_Queues]}
   @key[pragma] Preelaborate(Unbounded_Synchronized_Queues);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[package] Implementation @key[is]
      ... -- @Examcom[not specified by the language]
   @key[end] Implementation;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[protected type] @AdaTypeDefn{Queue}
        (Ceiling : System.Any_Priority := Default_Ceiling)
           @key[with] Priority => Ceiling @key[is]
        @key[new] Queue_Interfaces.Queue @key[with]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[overriding]
      @key[entry] @AdaSubDefn{Enqueue} (New_Item : @key[in] Queue_Interfaces.Element_Type);
      @key[overriding]
      @key[entry] @AdaSubDefn{Dequeue} (Element : @key[out] Queue_Interfaces.Element_Type);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[overriding]
      @key[function] @AdaSubDefn{Current_Use} @key[return] Count_Type;
      @key[overriding]
      @key[function] @AdaSubDefn{Peak_Use} @key[return] Count_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[private]
      ... -- @Examcom[not specified by the language]
   @key[end] Queue;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{private}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   ... -- @Examcom[not specified by the language]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{end} Ada.Containers.Unbounded_Synchronized_Queues;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Text=[The type Queue is used to represent task-safe
queues.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Text=[The capacity for instances of type Queue is
unbounded.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Enqueue never blocks; if more storage is needed
  for a new element, it is allocated dynamically. We don't need to explicitly
  specify that Queue needs finalization, because it is visibly protected.]}
@end{Ramification}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Nested package Implementation can be used to
  declare the types needed to implement the protected type Queue. This
  nested package is necessary as types cannot be declared in the private
  part of a protected type, and the types have to be declared within the
  generic unit in order to depend on the types imported with package
  Queue_Interfaces. Clients should never depend on the contents of
  nested package Implementation.]}
@end{Discussion}

@end{StaticSem}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005} The generic
  package Containers.Unbounded_Synchronized_Queues is new.]}
@end{Extend2005}



@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Bounded_Synchronized_Queues]}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Bounded_Synchronized_Queues provides type Queue, which implements
the interface type Containers.Synchronized_Queue_Interfaces.Queue.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[with] System;
@key[with] Ada.Containers.Synchronized_Queue_Interfaces;
@key[generic]
   @key[with package] Queue_Interfaces @key[is new] Ada.Containers.Synchronized_Queue_Interfaces (<>);
   Default_Capacity : Count_Type;
   Default_Ceiling  : System.Any_Priority := System.Priority'Last;
@key[package] Ada.Containers.Bounded_Synchronized_Queues @key[is]@ChildUnit{Parent=[Ada.Containers],Child=[Bounded_Synchronized_Queues]}
   @key[pragma] Preelaborate(Bounded_Synchronized_Queues);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[package] Implementation @key[is]
      ... -- @Examcom[not specified by the language]
   @key[end] Implementation;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[protected type] @AdaTypeDefn{Queue}
        (Capacity : Count_Type := Default_Capacity;
         Ceiling  : System.Any_Priority := Default_Ceiling)
           @key[with] Priority => Ceiling @key[is]
        @key[new] Queue_Interfaces.Queue @key[with]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[overriding]
      @key[entry] @AdaSubDefn{Enqueue} (New_Item : @key[in] Queue_Interfaces.Element_Type);
      @key[overriding]
      @key[entry] @AdaSubDefn{Dequeue} (Element : @key[out] Queue_Interfaces.Element_Type);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[overriding]
      @key[function] @AdaSubDefn{Current_Use} @key[return] Count_Type;
      @key[overriding]
      @key[function] @AdaSubDefn{Peak_Use} @key[return] Count_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[private]
      ... -- @Examcom[not specified by the language]
   @key[end] Queue;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{private}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   ... -- @Examcom[not specified by the language]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{end} Ada.Containers.Bounded_Synchronized_Queues;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Keepnext=[T],Type=[Leading],Text=[The semantics are the
same as for Unbounded_Synchronized_Queues, except:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The capacity for instances of type Queue is
  bounded and specified by the discriminant Capacity.]}
@end{Itemize}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Since this type has a bounded capacity, Enqueue
  might block if the queue is full.]}
@end{Ramification}

@end{StaticSem}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Text=[Bounded queue objects should be implemented without
implicit pointers or dynamic allocation.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Bounded queue objects should be implemented without
implicit pointers or dynamic allocation.]}]}
@end{ImplAdvice}


@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005} The generic
  package Containers.Bounded_Synchronized_Queues is new.]}
@end{Extend2005}



@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Unbounded_Priority_Queues]}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Unbounded_Priority_Queues provides type Queue, which implements
the interface type Containers.Synchronized_Queue_Interfaces.Queue.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[with] System;
@key[with] Ada.Containers.Synchronized_Queue_Interfaces;
@key[generic]
   @key[with package] Queue_Interfaces @key[is new] Ada.Containers.Synchronized_Queue_Interfaces (<>);
   @key[type] Queue_Priority @key[is private];
   @key[with] @key[function] Get_Priority
     (Element : Queue_Interfaces.Element_Type) @key[return] Queue_Priority @key[is] <>;
   @key[with] @key[function] Before
     (Left, Right : Queue_Priority) @key[return] Boolean @key[is] <>;
   Default_Ceiling : System.Any_Priority := System.Priority'Last;
@key[package] Ada.Containers.Unbounded_Priority_Queues @key[is]@ChildUnit{Parent=[Ada.Containers],Child=[Unbounded_Priority_Queues]}
   @key[pragma] Preelaborate(Unbounded_Priority_Queues);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[package] Implementation @key[is]
      ... -- @Examcom[not specified by the language]
   @key[end] Implementation;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[protected type] @AdaTypeDefn{Queue}
        (Ceiling : System.Any_Priority := Default_Ceiling)
           @key[with] Priority => Ceiling @key[is]
        @key[new] Queue_Interfaces.Queue @key[with]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[overriding]
      @key[entry] @AdaSubDefn{Enqueue} (New_Item : @key[in] Queue_Interfaces.Element_Type);
      @key[overriding]
      @key[entry] @AdaSubDefn{Dequeue} (Element : @key[out] Queue_Interfaces.Element_Type);]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1],ARef=[AI05-0251-1]}
@ChgAdded{Version=[3],Text=[      @key[not overriding]
      @key[procedure] @AdaSubDefn{Dequeue_Only_High_Priority}
        (At_Least : @key[in]     Queue_Priority;
         Element  : @key[in out] Queue_Interfaces.Element_Type;
         Success  :    @key[out] Boolean);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[overriding]
      @key[function] @AdaSubDefn{Current_Use} @key[return] Count_Type;
      @key[overriding]
      @key[function] @AdaSubDefn{Peak_Use} @key[return] Count_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[private]
      ... -- @Examcom[not specified by the language]
   @key[end] Queue;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{private}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   ... -- @Examcom[not specified by the language]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{end} Ada.Containers.Unbounded_Priority_Queues;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Text=[The type Queue is used to represent task-safe
priority queues.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Text=[The capacity for instances of type Queue is
unbounded.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Text=[Two elements @i<E1> and @i<E2> are equivalent if
Before(Get_Priority(@i<E1>), Get_Priority(@i<E2>)) and
Before(Get_Priority(@i<E2>), Get_Priority(@i<E1>)) both return False.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[The actual functions for Get_Priority and Before are
expected to return the same value each time they are called with the same
actuals, and should not modify their actuals. Before should define a strict weak
ordering relationship (see @RefSecNum{Containers}). If the actual
functions behave in some other manner, the behavior of Unbounded_Priority_Queues
is unspecified.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Text=[Enqueue inserts an item according to the order
specified by the Before function on the result of Get_Priority on the elements;
Before should return True if Left is to be inserted before Right.
If the queue already contains elements equivalent to New_Item, then it is
inserted after the existing equivalent elements.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Enqueue never blocks; if more storage is needed
  for a new element, it is allocated dynamically. We don't need to explicitly
  specify that Queue needs finalization, because it is visibly protected.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1],ARef=[AI05-0251-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[For a call on Dequeue_Only_High_Priority, if the
head of the nonempty queue is @i<E>, and the function Before(At_Least,
Get_Priority(@i<E>)) returns False, then @i<E> is assigned to
Element and then removed from the queue, and Success is set to True;
otherwise, Success is set to False and Element is unchanged.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0251-1]}
  @ChgAdded{Version=[3],Text=[Unlike Dequeue, Dequeue_Only_High_Priority is not
  blocking; it always returns immediately.]}
@end{Ramification}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0251-1]}
  @ChgAdded{Version=[3],Text=[The use of Before is "backwards" so that it acts
  like ">=" (it is defined similarly to ">"); thus we dequeue only when it is
  False.]}
@end{Reason}

@end{StaticSem}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1],ARef=[AI05-0251-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005} The generic
  package Containers.Unbounded_Priority_Queues is new.]}
@end{Extend2005}


@LabeledAddedSubclause{Version=[3],Name=[The Generic Package Containers.Bounded_Priority_Queues]}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Text=[The language-defined generic package
Containers.Bounded_Priority_Queues provides type Queue, which implements
the interface type Containers.Synchronized_Queue_Interfaces.Queue.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[with] System;
@key[with] Ada.Containers.Synchronized_Queue_Interfaces;
@key[generic]
   @key[with package] Queue_Interfaces @key[is new] Ada.Containers.Synchronized_Queue_Interfaces (<>);
   @key[type] Queue_Priority @key[is private];
   @key[with function] Get_Priority
     (Element : Queue_Interfaces.Element_Type) @key[return] Queue_Priority @key[is] <>;
   @key[with function] Before
     (Left, Right : Queue_Priority) @key[return] Boolean @key[is] <>;
   Default_Capacity : Count_Type;
   Default_Ceiling  : System.Any_Priority := System.Priority'Last;
@key[package] Ada.Containers.Bounded_Priority_Queues @key[is]@ChildUnit{Parent=[Ada.Containers],Child=[Bounded_Priority_Queues]}
   @key[pragma] Preelaborate(Bounded_Priority_Queues);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[package] Implementation @key[is]
      ... -- @Examcom[not specified by the language]
   @key[end] Implementation;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[protected type] @AdaTypeDefn{Queue}
        (Capacity : Count_Type := Default_Capacity;
         Ceiling  : System.Any_Priority := Default_Ceiling)
           @key[with] Priority => Ceiling @key[is]
      @key[new] Queue_Interfaces.Queue @key[with]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[overriding]
      @key[entry] @AdaSubDefn{Enqueue} (New_Item : @key[in] Queue_Interfaces.Element_Type);
      @key[overriding]
      @key[entry] @AdaSubDefn{Dequeue} (Element : @key[out] Queue_Interfaces.Element_Type);]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1],ARef=[AI05-0251-1]}
@ChgAdded{Version=[3],Text=[      @key[not overriding]
      @key[procedure] @AdaSubDefn{Dequeue_Only_High_Priority}
        (At_Least : @key[in]     Queue_Priority;
         Element  : @key[in out] Queue_Interfaces.Element_Type;
         Success  :    @key[out] Boolean);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[overriding]
      @key[function] @AdaSubDefn{Current_Use} @key[return] Count_Type;
      @key[overriding]
      @key[function] @AdaSubDefn{Peak_Use} @key[return] Count_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[private]
      ... -- @Examcom[not specified by the language]
   @key[end] Queue;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{private}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   ... -- @Examcom[not specified by the language]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{end} Ada.Containers.Bounded_Priority_Queues;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Keepnext=[T],Type=[Leading],Text=[The semantics are the
same as for Unbounded_Priority_Queues, except:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The capacity for instances of type Queue is
  bounded and specified by the discriminant Capacity.]}
@end{Itemize}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Since this type has a bounded capacity, Enqueue
  might block if the queue is full.]}
@end{Ramification}

@end{StaticSem}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Text=[Bounded priority queue objects should be implemented without
implicit pointers or dynamic allocation.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Bounded priority queue objects should be implemented without
implicit pointers or dynamic allocation.]}]}
@end{ImplAdvice}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1],ARef=[AI05-0251-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005} The generic
  package Containers.Bounded_Priority_Queues is new.]}
@end{Extend2005}


@LabeledAddedSubclause{Version=[3],Name=[Example of Container Use]}

@begin{Examples}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[The following example is an implementation of
Dijkstra's shortest path algorithm in a directed graph with positive distances.
The graph is represented by a map from nodes to sets of edges.]}

@begin{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[with] Ada.Containers.Vectors;
@key[with] Ada.Containers.Doubly_Linked_Lists;
@key[use] Ada.Containers;
@key[generic]
   @key[type] Node @key[is range] <>;
@key[package] Shortest_Paths @key[is]
   @key[type] Distance @key[is new] Float @key[range] 0.0 .. Float'Last;
   @key[type] Edge @key[is record]
      To, From : Node;
      Length   : Distance;
   @key[end record];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[package] Node_Maps @key[is new] Vectors (Node, Node);
   -- @Examcom{The algorithm builds a map to indicate the node used to reach a given}
   -- @Examcom{node in the shortest distance.}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[package] Adjacency_Lists @key[is new] Doubly_Linked_Lists (Edge);
   @key[use] Adjacency_Lists;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[package] Graphs @key[is new] Vectors (Node, Adjacency_Lists.List);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[package] Paths @key[is new] Doubly_Linked_Lists (Node);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] Shortest_Path
     (G : Graphs.Vector; Source : Node; Target : Node) @key[return] Paths.List
      @key[with] Pre => G (Source) /= Adjacency_Lists.Empty_List;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[end] Shortest_Paths;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[package body] Shortest_Paths @key[is]
   @key[function] Shortest_Path
     (G : Graphs.Vector; Source : Node; Target : Node) @key[return] Paths.List
   @key[is]
      @key[use] Adjacency_Lists, Node_Maps, Paths, Graphs;
      Reached  : @key[array] (Node) @key[of] Boolean := (@key[others] => False);
      -- @ExamCom{The set of nodes whose shortest distance to the source is known.}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0299-1]}
@ChgAdded{Version=[3],Text=[      Reached_From : @key[array] (Node) @key[of] Node;
      So_Far   : @key[array] (Node) @key[of] Distance := (@key[others] => Distance'Last);
      The_Path : Paths.List := Paths.Empty_List;
      Nearest_Distance : Distance;
      Next     : Node;
   @key[begin]
      So_Far(Source)  := 0.0;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[while not] Reached(Target) @key[loop]
         Nearest_Distance := Distance'Last;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[         -- @Examcom{Find closest node not reached yet, by iterating over all nodes.}
         -- @Examcom{A more efficient algorithm uses a priority queue for this step.}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[         Next := Source;
         @key[for] N @key[in] Node'First .. Node'Last @key[loop]
            @key[if not] Reached(N)
              @key[and then] So_Far(N) < Nearest_Distance @key[then]
                 Next := N;
                 Nearest_Distance := So_Far(N);
            @key[end if];
         @key[end loop];]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0299-1]}
@ChgAdded{Version=[3],Text=[         @key[if] Nearest_Distance = Distance'Last @key[then]
            -- @Examcom{No next node found, graph is not connected}
            @key[return] Paths.Empty_List;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[         @key[else]
            Reached(Next) := True;
         @key[end if];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[         -- @ExamCom{Update minimum distance to newly reachable nodes.}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0299-1]}
@ChgAdded{Version=[3],Text=[         @key[for] E @key[of] G (Next) @key[loop]
            @key[if not] Reached(E.To) @key[then]
               Nearest_Distance := E.Length + So_Far(Next);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[               @key[if] Nearest_Distance < So_Far(E.To) @key[then]
                  Reached_From(E.To) := Next;
                  So_Far(E.To) := Nearest_Distance;
               @key[end if];
            @key[end if];
         @key[end loop];
      @key[end loop];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      -- @ExamCom{Rebuild path from target to source.}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[declare]
         N : Node := Target;
      @key[begin]
         @key[while] N /= Source @key[loop]
            N := Reached_From(N);
            Prepend (The_Path, N);
         @key[end loop];
      @key[end];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[return] The_Path;
   @key[end];
@key[end] Shortest_Paths;]}

@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[Note that the effect of the
Constant_Indexing aspect (on type Vector) and the Implicit_Dereference aspect
(on type Reference_Type) is that]}

@begin{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[G (Next)]}

@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[is a convenient short hand for]}

@begin{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[G.Constant_Reference (Next).Element.@key[all]]}

@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Leading],Keepnext=[T],Text=[Similarly, the effect of the loop:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[for] E @key[of] G (Next) @key[loop]
   @key[if not] Reached(E.To) @key[then]
      ...
   @key[end if];
@key[end loop];]}

@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Leading],Keepnext=[T],Text=[is the same as:]}

@begin{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0080-1]}
@ChgAdded{Version=[3],Text=[@key[for] C @key[in] G (Next).Iterate @key[loop]
   @key[declare]
      E : Edge @key[renames] G (Next)(C)@Chg{Version=[4],New=[],Old=[.@key[all]]};
   @key[begin]
      @key[if not] Reached(E.To) @key[then]
         ...
      @key[end if];
   @key[end];
@key[end loop];]}

@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Leading],Keepnext=[T],Text=[which is the same as:]}

@begin{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0080-1]}
@ChgAdded{Version=[3],Text=[@key[declare]
   L : Adjacency_Lists.List @key[renames] G (Next);
   C : Adjacency_Lists.Cursor := L.First;
@key[begin]
   @key[while] Has_Element (C) @key[loop]
      @key[declare]
         E : Edge @key[renames] L(C)@Chg{Version=[4],New=[],Old=[.@key[all]]};
      @key[begin]
         @key[if not] Reached(E.To) @key[then]
            ...
         @key[end if];
      @key[end];
      C := L.Next (C);
   @key[end loop];
@key[end];]}

@end{Example}
@end{Examples}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[3],Text=[This example of container use is new.]}
@end{DiffWord2005}



