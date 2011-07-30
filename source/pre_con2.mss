@Part(precontainers-2, Root="ada.mss")
@comment{ $Source: e:\\cvsroot/ARM/Source/pre_con2.mss,v $ }
@comment{ $Revision: 1.9 $ $Date: 2011/07/29 05:59:20 $ $Author: randy $ }

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
@end{Itemize}
@end{StaticSem}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The generic package Containers.Indefinite_Vectors is new.]}
@end{Extend95}


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
@end{Itemize}
@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Containers.Indefinite_Doubly_Linked_Lists is new.]}
@end{Extend95}


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
@end{Itemize}
@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Containers.Indefinite_Hashed_Maps is new.]}
@end{Extend95}


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

@end{Itemize}
@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Containers.Indefinite_Ordered_Maps is new.]}
@end{Extend95}


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
@end{Itemize}
@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Containers.Indefinite_Hashed_Sets is new.]}
@end{Extend95}


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
@end{Itemize}
@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
The generic package Containers.Indefinite_Ordered_Sets is new.]}
@end{Extend95}


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
@end{Itemize}
@end{StaticSem}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The generic package Containers.Indefinite_Multiway_Trees is new.]}
@end{Extend2005}


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
@ChgAdded{Version=[3],Text=[   ... -- @RI[not specified by the language]]}

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

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Text=[@Redundant[Some operations of this generic package
have access-to-subprogram parameters. To ensure such operations are
well-defined, they guard against certain actions by the designated subprogram.
In particular, some operations check for @ldquote@;tampering with
elements@rdquote of a container because they depend on elements of the
container not being replaced.]]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[@Defn2{Term=[tamper with elements],Sec=[of a holder]}
A subprogram is said to @i{tamper with elements} of a holder object @i<H> if:]}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[It clears the element contained by @i<H>, that is,
it calls the Clear procedure with @i<H> as a parameter;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[It replaces the element contained by @i<H>, that is,
it calls the Replace_Element procedure with H as a parameter;]}

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
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns a non-empty holder
containing an element initialized to New_Item.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Is_Empty (Container : Holder) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the holder is
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
@ChgAdded{Version=[3],Type=[Trailing],Text=[Replace_Element assigns the value
New_Item into Container, replacing any preexisting content of Container.
Container is not empty after a successful call to Replace_Element.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[procedure] Query_Element
  (Container : @key[in] Holder;
   Process   : @key[not null access procedure] (Element : @key[in] Element_Type));]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Container is empty,
Constraint_Error is propagated. Otherwise, Query_Element calls
Process.@key[all] with the contained element as the argument. Program_Error is
raised if Process.@key[all] tampers with the elements of Container. Any
exception raised by Process.@key[all] is propagated.]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The @ldquote@;tamper with the elements@rdquote
  check is intended to prevent the Element parameter of Process from being
  modified or deleted outside of Process. The check prevents data loss (if
  Element_Type is passed by copy) or erroneous execution (if Element_Type is an
  unconstrained type).]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[procedure] Update_Element
  (Container : @key[in out] Holder;
   Process   : @key[not null access procedure] (Element : @key[in out] Element_Type));]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Container is empty,
Constraint_Error is propagated. Otherwise, Update_Element calls
Process.@key[all] with the contained element as the argument. Program_Error is
raised if Process.@key[all] tampers with the elements of Container. Any
exception raised by Process.@key[all] is propagated.]}

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
@ChgAdded{Version=[3],Type=[Trailing],Text=[Constant_Reference_Type and Reference_Type
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
@ChgAdded{Version=[3],Type=[Trailing],Text=[This routine (combined with the
Implicit_Dereference aspect) provides a convenient way to gain read access to
the contained element of a holder container.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[If Container is empty, Constraint_Error is
propagated. Otherwise, Constant_Reference returns an object whose discriminant
is an access value that designates the contained element. Program_Error is
propagated if any operation tampers with the elements of Container while the
object returned by Constant_Reference exists and has not been finalized.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Reference (Container : @key[aliased in out] Holder)
   @key[return] Reference_Type;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[This function (combined with the
Implicit_Dereference aspects) provides a convenient way to gain read and write
access to the contained element of a holder container.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[If Container is empty, Constraint_Error is
propagated. Otherwise, Reference returns an object whose discriminant is an
access value that designates the contained element. Program_Error is propagated
if any operation tampers with the elements of Container while the object
returned by Reference exists and has not been finalized.]}

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
  @exam{A := B} behave identically.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key[function] Copy (Source : Holder) @key[return] Holder;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Source is empty, returns an empty
holder; otherwise, returns To_Holder (Element (Source)).]}

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

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0022-1],ARef=[AI05-0069-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error for the actual function associated with a
generic formal subprogram, when called as part of an operation of
this package, to tamper with elements of any Holder parameter ofto the
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
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Text=[Execution is erroneous if the holder associated with
the result of a call to Reference or Constant_Reference is finalized before the
result object returned by the call to Reference or Constant_Reference is
finalized.@PDefn2{Term=(erroneous execution),Sec=(cause)}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
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

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Text=[No storage associated with a holder object shall be
lost upon assignment or scope exit.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Text=[The execution of an @nt{assignment_statement}
for a holder shall have the effect of copying the element (if any) from the
source holder object to the target holder object.]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[An assignment of a holder is a
  @ldquote@;deep@rdquote copy; that is the elements are copied as well as the
  data structures. We say @ldquote@;effect of@rdquote in order to allow the
  implementation to avoid copying elements immediately if it wishes. For
  instance, an implementation that avoided copying until one of the containers
  is modified would be allowed.]}
@end{ImplNote}
@end{ImplReq}

@begin{ImplAdvice}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Text=[Move should not copy elements, and should minimize
copying of internal data structures.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Containers.Indefinite_Holders.Move should not copy elements, and should
minimize copying of internal data structures.]}]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Usually that can be accomplished simply by moving
  the pointer(s) to the internal data structures from the Source holder to the
  Target holder.]}
@end{ImplNote}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1]}
@ChgAdded{Version=[3],Text=[If an exception is propagated from a holder
operation, no storage should be lost, nor should the element be removed from a
holder unless specified by the operation.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[If an exception is propagated from a holder
operation, no storage should be lost, nor should the element be removed from a
holder unless specified by the operation.]}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is important so that programs can recover
  from errors. But we don't want to require heroic efforts, so we just require
  documentation of cases where this can't be accomplished.]}
@end{Reason}
@end{ImplAdvice}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0069-1],ARef=[AI05-0084-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005} The generic package
  Containers.Indefinite_Holders is new.]}
@end{Extend2005}


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
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to use a bounded vector
if it was the target of an @nt{assignment_statement} whose source was in the middle of
an operation that disallows tampering with elements @Redundant[or cursors].
Either Program_Error is raised, or the operation proceeds as defined.]}
@end{Bounded}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For each instance of
Containers.Vectors and each instance of Containers.Bounded_Vectors,
if the two instances meet the following conditions
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
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to use a bounded list if it was the
target of an @nt{assignment_statement} whose source was in the middle of
an operation that disallows tampering with elements @Redundant[or cursors].
Either Program_Error is raised, or the operation proceeds as defined.]}
@end{Bounded}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For each instance of
Containers.Doubly_Linked_Lists and each instance of
Containers.Bounded_Doubly_Linked_Lists,
if the two instances meet the following conditions
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
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key[function] @AdaSubDefn{Copy} (Source : Map;
                 Capacity : Count_Type := 0;
                 Modulus  : Hash_Type := 0) @key[return] Map;]}
@end{Example}
@begin{Indent}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Noprefix=[T],Text=[Returns a map with key/element pairs initialized from the values
    in Source. If Capacity is 0, then the map capacity is the
    length of Source; if Capacity is equal to or greater than
    the length of Source, the map capacity is the value of the Capacity
    parameter; otherwise, the operation propagates Capacity_Error.  If
    the Modulus argument is 0, then the map modulus is the value
    returned by a call to Default_Modulus with the map capacity as its
    argument; otherwise the map modulus is the value of the Modulus parameter.]}
@end{Indent}

@end{Itemize}
@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to use a bounded map if it was the
target of an @nt{assignment_statement} whose source was in the middle of
an operation that disallows tampering with elements @Redundant[or cursors].
Either Program_Error is raised, or the operation proceeds as defined.]}
@end{Bounded}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For each instance of
Containers.Hashed_Maps and each instance of Containers.Bounded_Hashed_Maps,
if the two instances meet the following conditions
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
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[Bounded map objects should be implemented without
implicit pointers or dynamic allocation.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Bounded map objects should be implemented without
implicit pointers or dynamic allocation.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The implementation advice for procedure Move to
minimize copying does not apply.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The implementation advice for procedure Move to
minimize copying does not apply to bounded maps.]}]}

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
    type Element_Type needs finalization.]}

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
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key[function] @AdaSubDefn{Copy} (Source : Map;
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
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to use a bounded map if it was the
target of an @nt{assignment_statement} whose source was in the middle of
an operation that disallows tampering with elements @Redundant[or cursors].
Either Program_Error is raised, or the operation proceeds as defined.]}
@end{Bounded}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For each instance of
Containers.Ordered_Maps and each instance of Containers.Bounded_Ordered_Maps,
if the two instances meet the following conditions
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
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[Bounded map objects should be implemented without
implicit pointers or dynamic allocation.]}
@Comment{We omit the @ChgImplAdvice as it is identical to that of the previous clause.}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The implementation advice for procedure Move to
minimize copying does not apply.]}
@Comment{We omit the @ChgImplAdvice as it is identical to that of the previous clause.}

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
  @ChgAdded{Version=[3],Text=[The type Set needs finalization if and only if type
      Key_Type or type Element_Type needs finalization.]}

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
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key[function] @AdaSubDefn{Copy} (Source : Set;
                 Capacity : Count_Type := 0;
                 Modulus  : Hash_Type := 0) @key[return] Set;]}
@end{Example}
@begin{Indent}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Noprefix=[T],Text=[Returns a set whose elements are
    initialized from the values in Source. If Capacity is 0, then the set
    capacity is the length of Source; if Capacity is equal to or greater than
    the length of Source, the set capacity is the value of the Capacity parameter;
    otherwise, the operation propagates Capacity_Error. If the Modulus argument is
    0, then the set modulus is the value returned by a call to Default_Modulus
    with the set capacity as its argument; otherwise the set modulus is the
    value of the Modulus parameter.]}
@end{Indent}

@end{Itemize}
@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to use a bounded set if it was the
target of an @nt{assignment_statement} whose source was in the middle of
an operation that disallows tampering with elements @Redundant[or cursors].
Either Program_Error is raised, or the operation proceeds as defined.]}
@end{Bounded}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For each instance of
Containers.Hashed_Sets and each instance of Containers.Bounded_Hashed_Sets,
if the two instances meet the following conditions
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
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[Bounded set objects should be implemented without
implicit pointers or dynamic allocation.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Bounded set objects should be implemented without
implicit pointers or dynamic allocation.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The implementation advice for procedure Move to
minimize copying does not apply.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The implementation advice for procedure Move to
minimize copying does not apply to bounded sets.]}]}

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
    made that the capacity is not exceeded, and Capacity_Error is raised if this
    check fails.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[In procedure Assign, if Source length is greater
    than Target capacity, then Capacity_Error is propagated.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The function Copy is replaced with:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key[function] @AdaSubDefn{Copy} (Source : Set;
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
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0160-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to use a bounded set if it was the
target of an @nt{assignment_statement} whose source was in the middle of
an operation that disallows tampering with elements @Redundant[or cursors].
Either Program_Error is raised, or the operation proceeds as defined.]}
@end{Bounded}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For each instance of
Containers.Ordered_Sets and each instance of Containers.Bounded_Ordered_Sets,
if the two instances meet the following conditions
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
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[Bounded set objects should be implemented without
implicit pointers or dynamic allocation.]}
@Comment{We omit the @ChgImplAdvice as it is identical to that of the previous clause.}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],Text=[The implementation advice for procedure Move to
minimize copying does not apply.]}
@Comment{We omit the @ChgImplAdvice as it is identical to that of the previous clause.}

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
@ChgAdded{Version=[3],Noprefix=[T],Text=[  @key{function} Copy (Source : Tree; Capacity : Count_Type := 0)
     @key{return} List;]}
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
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0136-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to use a bounded tree if it was the
target of an @nt{assignment_statement} whose source was in the middle of
an operation that disallows tampering with elements @Redundant[or cursors].
Either Program_Error is raised, or the operation proceeds as defined.]}
@end{Bounded}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0184-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For each instance of
Containers.Multiway_Trees and each instance of Containers.Bounded_Multiway_Trees,
if the two instances meet the following conditions
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
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0044-1]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
"<" of Generic_Array_Sort is expected to return the same value each time it is
called with a particular pair of element values. It should define a strict
@Chg{Version=[3],New=[weak ],Old=[]}ordering relationship@Chg{Version=[3],
New=[ (see @RefSecNum{Containers})],Old=[, that is, be irreflexive, asymmetric,
and transitive]}; it
should not modify Container. If the actual for "<" behaves in some other
manner, the behavior of the instance of Generic_Array_Sort is unspecified. How
many times Generic_Array_Sort calls "<" is unspecified.@PDefn{unspecified}]}

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
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0044-1]}
@ChgAdded{Version=[2],Text=[The actual function for the generic formal function
"<" of Generic_Constrained_Array_Sort is expected to return the same value each
time it is called with a particular pair of element values. It should define a
strict @Chg{Version=[3],New=[weak ],Old=[]}ordering relationship@Chg{Version=[3],
New=[ (see @RefSecNum{Containers})],Old=[, that is, be irreflexive, asymmetric,
and transitive]}; it should not modify Container. If the actual for "<" behaves in
some other manner, the behavior of the instance of
Generic_Constrained_Array_Sort is unspecified. How many times
Generic_Constrained_Array_Sort calls "<" is unspecified.@PDefn{unspecified}]}

@end{DescribeCode}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The generic library
procedure Containers.@!Generic_@!Sort has the following declaration:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key{generic}
   @key{type} Index_Type @key{is} (<>);
   @key{with function} Before (Left, Right : Index_Type) @key{return} Boolean;
   @key{with procedure} Swap (Left, Right : Index_Type);
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
Generic_Sort is unspecified. How many times the Generic_Sort calls Before
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

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0248-1]}
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

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[Containers.Generic_Sort
should minimize should minimize to the generic formal Swap.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[Containers.Generic_Sort should minimize to the generic formal Swap.]}]}
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

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[A queue type that implements this
interface may have a bounded @i<capacity>@Defn2{Term=[capacity],Sec=[of a queue]}.
If the queue object has a bounded
capacity, and the number of existing elements equals the capacity, then Enqueue
blocks until storage becomes available; otherwise Enqueue does not block. In any
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
  @ChgAdded{Version=[3],Text=[There are no indefinite queues as a useful
  definition for Dequeue is not possible. Dequeue cannot be a function as Ada
  does not have entries that are functions (thus conditional and timed calls
  would not be possible) and moreover protected functions do not allow modifying
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
   Default_Ceiling: System.Any_Priority := System.Priority'Last;
@key[package] Ada.Containers.Unbounded_Synchronized_Queues is@ChildUnit{Parent=[Ada.Containers],Child=[Unbounded_Synchronized_Queues]}
   @key[pragma] Preelaborate(Unbounded_Synchronized_Queues);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[package] Implementation @key[is]
      ... -- @RI[not specified by the language]
   @key[end] Implementation;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[protected type] @AdaTypeDefn{Queue}
        (Ceiling: System.Any_Priority := Default_Ceiling)
           @key[with] Priority => Ceiling @key[is]
        @key[new] Queue_Interfaces.Queue @key[with]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[overriding]
      @key[entry] @AdaSubDefn{Enqueue} (New_Item: @key[in] Queue_Interfaces.Element_Type);
      @key[overriding]
      @key[entry] @AdaSubDefn{Dequeue} (Element: @key[out] Queue_Interfaces.Element_Type);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[overriding]
      @key[function] @AdaSubDefn{Current_Use} @key[return] Count_Type;
      @key[overriding]
      @key[function] @AdaSubDefn{Peak_Use} @key[return] Count_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[private]
      ... -- @RI[not specified by the language]
   @key[end] Queue;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{private}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   ... -- @RI[not specified by the language]]}

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
   Default_Ceiling: System.Any_Priority := System.Priority'Last;
@key[package] Ada.Containers.Bounded_Synchronized_Queues is@ChildUnit{Parent=[Ada.Containers],Child=[Bounded_Synchronized_Queues]}
   @key[pragma] Preelaborate(Bounded_Synchronized_Queues);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[package] Implementation @key[is]
      ... -- @RI[not specified by the language]
   @key[end] Implementation;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[protected type] @AdaTypeDefn{Queue}
        (Capacity : Count_Type := Default_Capacity;
         Ceiling: System.Any_Priority := Default_Ceiling)
           @key[with] Priority => Ceiling @key[is]
        @key[new] Queue_Interfaces.Queue @key[with]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[overriding]
      @key[entry] @AdaSubDefn{Enqueue} (New_Item: @key[in] Queue_Interfaces.Element_Type);
      @key[overriding]
      @key[entry] @AdaSubDefn{Dequeue} (Element: @key[out] Queue_Interfaces.Element_Type);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[overriding]
      @key[function] @AdaSubDefn{Current_Use} @key[return] Count_Type;
      @key[overriding]
      @key[function] @AdaSubDefn{Peak_Use} @key[return] Count_Type;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[private]
      ... -- @RI[not specified by the language]
   @key[end] Queue;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{private}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   ... -- @RI[not specified by the language]]}

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
     (Element: Queue_Interfaces.Element_Type) @key[return] Queue_Priority is <>;
   @key[with] @key[function] Before
     (Left, Right : Queue_Priority) @key[return] Boolean is <>;
   Default_Ceiling: System.Any_Priority := System.Priority'Last;
@key[package] Ada.Containers.Unbounded_Priority_Queues @key[is]@ChildUnit{Parent=[Ada.Containers],Child=[Unbounded_Priority_Queues]}
   @key[pragma] Preelaborate(Unbounded_Priority_Queues);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[package] Implementation @key[is]
      ... -- @RI[not specified by the language]
   @key[end] Implementation;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[protected type] @AdaTypeDefn{Queue}
        (Ceiling: System.Any_Priority := Default_Ceiling)
           @key[with] Priority => Ceiling @key[is]
        @key[new] Queue_Interfaces.Queue @key[with]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[overriding]
      @key[entry] @AdaSubDefn{Enqueue} (New_Item: @key[in] Queue_Interfaces.Element_Type);
      @key[overriding]
      @key[entry] @AdaSubDefn{Dequeue} (Element: @key[out] Queue_Interfaces.Element_Type);]}

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
      ... -- @RI[not specified by the language]
   @key[end] Queue;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{private}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   ... -- @RI[not specified by the language]]}

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

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0159-1],ARef=[AI05-0251-1]}
@ChgAdded{Version=[3],Text=[For a call on Dequeue_Only_High_Priority, if the
head of the non-empty queue is @i<E>, and the function Before(At_Least,
Get_Priority(@i<E>)) returns False, then @i<E> is assigned to
Element and then removed from the queue, and Success is set to True;
otherwise Success is set to False and Element is unchanged.]}

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
     (Element: Queue_Interfaces.Element_Type) @key[return] Queue_Priority is <>;
   @key[with function] Before
     (Left, Right : Queue_Priority) @key[return] Boolean is <>;
   Default_Capacity : Count_Type;
   Default_Ceiling: System.Any_Priority := System.Priority'Last;
@key[package] Ada.Containers.Bounded_Priority_Queues @key[is]@ChildUnit{Parent=[Ada.Containers],Child=[Bounded_Priority_Queues]}
   @key[pragma] Preelaborate(Bounded_Priority_Queues);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[package] Implementation @key[is]
      ... -- @RI[not specified by the language]
   @key[end] Implementation;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[protected type] @AdaTypeDefn{Queue}
        (Capacity : Count_Type := Default_Capacity;
         Ceiling: System.Any_Priority := Default_Ceiling)
           @key[with] Priority => Ceiling @key[is]
      @key[new] Queue_Interfaces.Queue @key[with]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[overriding]
      @key[entry] @AdaSubDefn{Enqueue} (New_Item: @key[in] Queue_Interfaces.Element_Type);
      @key[overriding]
      @key[entry] @AdaSubDefn{Dequeue} (Element: @key[out] Queue_Interfaces.Element_Type);]}

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
      ... -- @RI[not specified by the language]
   @key[end] Queue;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{private}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   ... -- @RI[not specified by the language]]}

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
   --  @Examcom{The algorithm builds a map to indicate the node used to reach a given}
   --  @Examcom{node in the shortest distance.}]}

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
      --  @ExamCom{The set of nodes whose shortest distance to the source is known.}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      Reached_From : @key[array] (Node) @key[of] Node;
      So_Far   : @key[array] (Node) @key[of] Distance := (@key[others] => Distance'Last);
      The_Path : Paths.List := Paths.Empty_List;
      Nearest_Distance : Distance;
      Next     : Node;
   @key[begin]
      Reached (Source) := True;
      So_Far (Source)  := 0.0;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[while not] Reached (Target) @key[loop]
         Nearest_Distance := Distance'Last;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[         --  @Examcom{Find closest node not reached yet, by iterating over all nodes.}
         --  @Examcom{A more efficient algorithm uses a priority queue for this step.}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[         Next := Source;
         @key[for] N @key[in] Node'First .. Node'Last @key[loop]
            @key[if not] Reached (N)
              @key[and then] So_Far (N) < Nearest_Distance @key[then]
                 Next := N;
                 Nearest_Distance := So_Far (N);
            @key[end if];
         @key[end loop];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[         @key[if] Next = Source @key[then]  --  @Examcom{No next node found, graph is not connected}
            @key[return] Paths.Empty_List;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[         @key[else]
            Reached (Next) := True;
         @key[end if];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[         --  @ExamCom{Update minimum distance to newly reachable nodes.}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[         @key[for] E @key[of] G (Next) @key[loop]
            @key[if not] Reached (E.To) @key[then]
               Nearest_Distance :=
                 Distance'Min (So_Far (E.To) + So_Far (Next),
                               So_Far (E.To));]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[               @key[if] Nearest_Distance < So_Far (E.To) @key[then]
                  Reached_From (E.To) := Next;
                  So_Far (E.To) := Nearest_Distance;
               @key[end if];
            @key[end if];
         @key[end loop];
      @key[end loop];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      --  @ExamCom{Rebuild path from target to source.}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[declare]
         N : Node := Target;
      @key[begin]
         @key[while] N /= Source @key[loop]
            N := Reached_From (N);
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
@ChgAdded{Version=[3],Type=[Leading],Text=[Similarly, the effect of the loop:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[for] E @key[of] G (Next) @key[loop]
   @key[if not] Reached (E.To) @key[then]
      ...
   @key[end if];
@key[end loop];]}

@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[is the same as:]}

@begin{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[for] C @key[in] G (Next).Iterate @key[loop]
   @key[declare]
      E : Edge @key[renames] G (Next)(C).@key[all];
   @key[begin]
      @key[if not] Reached (E.To) @key[then]
         ...
      @key[end if];
   @key[end];
@key[end loop];]}

@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[which is the same as:]}

@begin{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[declare]
   L : Adjacency_Lists.List @key[renames] G (Next);
   C : Adjacency_Lists.Cursor := L.First;
@key[begin]
   @key[while] Has_Element (C) @key[loop]
      @key[declare]
         E : Edge @key[renames] L(C).@key[all];
      @key[begin]
         @key[if not] Reached (E.To) @key[then]
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






