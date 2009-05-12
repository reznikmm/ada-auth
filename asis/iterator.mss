@Part(iterator, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/iterator.mss,v $}
@comment{$Revision: 1.4 $ $Date: 2009/05/09 06:28:46 $}

@LabeledSection{package Asis.Iterator}


@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Iterator]}Asis.Iterator
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis],Child=[Iterator]}Asis.Iterator @key[is]}]}

Asis.Iterator encapsulates the generic procedure Traverse_Element which
allows an ASIS application to perform an iterative traversal of a
logical syntax tree. It requires the use of two generic procedures,
Pre_Operation, which identifies processing for the traversal, and
Post_Operation, which identifies processing after the traversal.
The State_Information allows processing state to be passed during the
iteration of Traverse_Element.

Package Asis.Iterator is established as a child package to highlight the
iteration capability and to facilitate the translation of ASIS to IDL.

@LabeledClause{procedure Traverse_Element}


@begin{DescribeCode}
@begin{Example}
@key[generic]

   @key[type] State_Information @key[is limited private];

   @key[with procedure] Pre_Operation
                    (Element : @key[in]     Asis.Element;
                     Control : @key[in out] Traverse_Control;
                     State   : @key[in out] State_Information) @key[is] <>;

   @key[with procedure] Post_Operation
                    (Element : @key[in]     Asis.Element;
                     Control : @key[in out] Traverse_Control;
                     State   : @key[in out] State_Information) @key[is] <>;

@key[procedure] @AdaSubDefn{Traverse_Element}
            (Element : @key[in]     Asis.Element;
             Control : @key[in out] Traverse_Control;
             State   : @key[in out] State_Information);
@end{Example}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Element @Chg{Version=[1],New=[specifies],Old=[            @en Specifies]} the initial element in the traversal.
Control @Chg{Version=[1],New=[specifies],Old=[            @en Specifies]} what next to do with the traversal.
@Chg{Version=[2],New=[State],Old=[State_Information]} @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} other information for the traversal.

Traverses the element and all its component elements, if any.
Component elements are all elements that can be obtained by a combination
of the ASIS structural queries appropriate for the given element.

If an element has one or more component elements, each is called a child
element. An element's parent element is its Enclosing_Element. Children
with the same parent are sibling elements. The type Traverse_Control uses
the terms children and siblings to control the traversal.

For each element, the formal procedure Pre_Operation is called when first
visiting the element. Each of that element's children are then visited
and finally the formal procedure Post_Operation is called for the element.

The order of Element traversal is in terms of the textual representation of
the Elements. Elements are traversed in left-to-right and top-to-bottom
order.

@leading@keepnext@;Traversal of Implicit Elements:

Implicit elements are not traversed by default. However, they may be
explicitly queried and then passed to the traversal instance. Implicit
elements include implicit predefined operator declarations, implicit
inherited subprogram declarations, implicit expanded generic specifications
and bodies, default expressions supplied to procedure, function, and entry
calls, etc.

Applications that wish to traverse these implicit Elements shall query for
them at the appropriate places in a traversal and then recursively call
their instantiation of the traversal generic. (Implicit elements provided
by ASIS do not cover all possible Ada implicit constructs. For example,
implicit initializations for variables of an access type are not provided
by ASIS.)

@leading@keepnext@;Traversal of Association lists:

Argument and association lists for procedure calls, function calls, entry
calls, generic instantiations, and aggregates are traversed in their
unnormalized forms, as if the Normalized parameter was False for those
queries. Implementations that always normalize certain associations may
return Is_Normalized associations. See the @ImplPermTitle
for the queries Discriminant_Associations, Generic_Actual_Part,
Call_Statement_Parameters, Record_Component_Associations, or
Function_Call_Parameters.

Applications that wish to explicitly traverse normalized associations can
do so by querying the appropriate locations in order to obtain the
normalized list. The list can then be traversed by recursively calling
the traverse instance. Once that sub-traversal is finished, the Control
parameter can be set to Abandon_Children to skip processing of the
unnormalized argument list.

Traversal can be controlled with the Control parameter.

A call to an instance of Traverse_Element will not result in calls to
Pre_Operation or Post_Operation unless Control is set to Continue.

@leading@;The subprograms matching Pre_Operation and Post_Operation can set
their Control parameter to affect the traversal:

@begin{Itemize}
Continue
@begin{InnerItemize}

  Continues the normal depth-first traversal.
@end{InnerItemize}

Abandon_Children
@begin{InnerItemize}

   Prevents traversal of the current element's children.

   If set in a Pre_Operation, traversal picks up with the next sibling element
   of the current element.

   If set in a Post_Operation, this is the same as Continue, all children will
   already have been traversed. Traversal picks up with the Post_Operation of
   the parent.
@end{InnerItemize}

Abandon_Siblings
@begin{InnerItemize}

   Prevents traversal of the current element's children and remaining siblings.

   If set in a Pre_Operation, this abandons the associated Post_Operation for the
   current element. Traversal picks up with the Post_Operation of the parent.

   If set in a Post_Operation, traversal picks up with the Post_Operation of the
   parent.
@end{InnerItemize}

Terminate_Immediately
@begin{InnerItemize}
   Does exactly that.
@end{InnerItemize}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
Raises ASIS_Inappropriate_Element @ChgAdded{Version=[2],Text=[with a Status
of Value_Error]} if the element is a Nil_Element.
@end{DescribeCode}

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Iterator;]}
@end{Example}

