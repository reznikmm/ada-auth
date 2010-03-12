@Part(ids, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/ids.mss,v $}
@comment{$Revision: 1.7 $ $Date: 2010/03/09 06:46:51 $}

@LabeledSection{package Asis.Ids}


@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Ids]}Asis.Ids
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis],Child=[Ids]}Asis.Ids @key[is]}]}

Asis.Ids provides support for permanent unique Element "Identifiers" (Ids).
An Id is an efficient way for a tool to reference an ASIS element. The Id
is permanent from session to session provided that the Ada compilation
environment is unchanged.
This package encapsulates a set of operations and queries that implement
the ASIS Id abstraction. An Id is a way of identifying a particular
Element, from a particular Compilation_Unit, from a particular Context.
Ids can be written to files. Ids can be read from files and converted into
an Element value with the use of a suitable open Context.


@LabeledClause{type Id}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0037-1]}
@Chg{Version=[2],New=[],Old=[The Ada element Id abstraction (a private type).]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0037-1]}
The Id type @Chg{Version=[2],New=[represents],Old=[is a distinct abstract type representing]} permanent "names"
that correspond to specific Element values.

These names can be written to files, retrieved at a later time, and
converted to Element values.

ASIS Ids are a means of identifying particular Element values obtained from
a particular physical compilation unit. Ids are "relative names". Each Id
value is valid (is usable, makes sense, can be interpreted) only in the
context of an appropriate open ASIS Context.

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0037-1]}
@Chg{Version=[2],New=[],Old=[Id shall be an undiscriminated private type, or, shall be derived from an
undiscriminated private type. It shall be declared as a new type or as a
subtype of an existing type.]}

@begin{DescribeCode}
@begin{Example}
@key[type] @AdaTypeDefn{Id} @key[is private];
@AdaObjDefn{Nil_Id} : @key[constant] Id;

@key[function] "=" (Left  : @key[in] Id;
              Right : @key[in] Id)
              @key[return] Boolean @key[is abstract];
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1],ARef=[SI99-0055-1]}
@ChgAdded{Version=[2],Text=[Nil_Id is the value of an Id that represents no
element.]}
@end{DescribeCode}


@LabeledClause{function Hash (id)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Hash} (The_Id : @key[in] Id)
              @key[return] Asis.ASIS_Integer;
@end{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0037-1]}@Comment{From the RM A.4.9(3/2)}
@ChgAdded{Version=[2],Text=[Returns an implementation-defined value which is a function of the value of The_Id.
If A and B are ids such that Is_Equal (A, B) is true, Hash(A) equals
Hash(B).]}
@begin{Implnote}
@ChgAdded{Version=[2],Text=[This function should produce a result (not raise
an exception) when passed Nil_Id.]}
@end{Implnote}
@end{DescribeCode}


@LabeledClause{function "<"}

@begin{DescribeCode}
@begin{Example}
@key[function] "<" (Left  : @key[in] Id;
              Right : @key[in] Id) @key[return] Boolean;
@end{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0037-1]}
@Chg{Version=[2],New=[Defines a total ordering on Ids.],Old=[]}
@end{DescribeCode}


@LabeledClause{function ">"}

@begin{DescribeCode}
@begin{Example}
@key[function] ">" (Left  : @key[in] Id;
              Right : @key[in] Id) @key[return] Boolean;
@end{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0037-1]}
@Chg{Version=[2],New=[Equivalent to Right < Left.],Old=[]}
@end{DescribeCode}


@LabeledClause{function Is_Nil (id)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Nil} (Right : @key[in] Id) @key[return] Boolean;
@end{Example}

Right @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the Id to check.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Returns True if the Id is the Nil_Id@Chg{Version=[2],New=[, and
returns False otherwise],Old=[]}.
@end{DescribeCode}


@LabeledClause{function Is_Equal (ids)}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Equal} (Left  : @key[in] Id;
                   Right : @key[in] Id) @key[return] Boolean;
@end{Example}

Left @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the left Id to compare.
Right @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the right Id to compare.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1],ARef=[SI99-0055-1]}
Returns True if Left and Right represent the same physical Id@Chg{Version=[2],New=[],Old=[,]}
from the same physical compilation unit@Chg{Version=[2],New=[, and
returns False otherwise],Old=[]}. @Chg{Version=[2],New=[If True, the],Old=[The]}
two Ids convert to Is_Identical Elements when
converted with the same open ASIS Context.
@end{DescribeCode}


@LabeledClause{function Create_Id}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Create_Id} (Element : @key[in] Asis.Element) @key[return] Id;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} any Element value whose Id is desired.

Returns a unique Id value corresponding to this Element, from the
corresponding Enclosing_Compilation_Unit and the corresponding
Enclosing_Context. The Id value will not be equal ("=") to the Id value
for any other Element value unless the two Elements are Is_Identical.

Nil_Id is returned for a Nil_Element.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Element expects any kind of element.], Old=[All Element_Kinds are appropriate.]}
@end{DescribeCode}


@LabeledClause{function Create_Element}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Create_Element} (The_Id      : @key[in] Id;
                         The_Context : @key[in] Asis.Context)
                       @key[return] Asis.Element;
@end{Example}

The_Id @Chg{Version=[1],New=[specifies],Old=[     @en Specifies]} the Id to be converted to an Element.
The_Context @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the Context containing the Element with this Id.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0037-1]}
Returns the Element value corresponding to The_Id. The_Id shall
correspond to an Element available from a Compilation_Unit contained by
(@Chg{Version=[2],New=[can be referenced],Old=[referencible]} through)
The_Context.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0037-1]}
@ChgAdded{Version=[2],Text=[Is_Identical (Create_Element (Create_Id(E),
Enclosing_Context(Enclosing_Compilation_Unit(E))), E) is True for any element E.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0037-1]}
@ChgAdded{Version=[2],Text=[If Create_Element is called twice with the same Id and context, the two results
are Is_Identical.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0049-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[If The_Context is different from the
context @i<C> used to create the Id, but the Id refers to an entity in a
compilation unit which represents the same physical compilation unit in both
contexts:]}
@begin{Itemize}
   @ChgRef{Version=[2],Kind=[Added]}
   @ChgAdded{Version=[2],Text=[if all of the compilation units in The_Context are
   the same physical compilation units as in @i<C>, Create_Element shall return
   an element referring to the same entity as in context @i<C>;]}

   @ChgRef{Version=[2],Kind=[Added]}
   @ChgAdded{Version=[2],Text=[otherwise, it is implementation-defined whether
   Create_Element can return a element referring to the same entity.]}
@end{Itemize}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0049-1]}
  @ChgAdded{Version=[2],Text=[If no units are changed in the environment, then
  Create_Element will return the same element even if the implementation
  consider contexts different each time they are created. Changes in
  configuration pragmas, project files, and the like are considered changes in
  compilation units as they are provided as part of one or more configuration
  compilation units (which are considered part of the context).
  "Implementation-defined" here means that implementations are expected to
  document what sort of changes to other units in an environment can be made
  without invalidating Asis.Ids. Note that we have no requirements for this
  function if the compilation unit containing the element has changed.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0037-1]}
Raises ASIS_Inappropriate_Element if the Element value is not available
@Chg{Version=[2],New=[through],Old=[though]} The_Context. The Status is
Value_Error and the Diagnosis string will attempt to indicate the reason for the
failure. (e.g., "Unit is inconsistent", "No such unit", "Element is inconsistent
(Unit inconsistent)", etc.)
@end{DescribeCode}


@LabeledClause{function Debug_Image (id)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Debug_Image} (The_Id : @key[in] Id) @key[return] Wide_String;
@end{Example}

The_Id @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} an Id to convert.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0037-1]}
Returns a string value containing implementation-defined debug
information associated with the Id @Chg{Version=[2],New=[including for Nil_Id],Old=[]}.

The return value uses Asis.Text.Delimiter_Image to separate the lines
of multi-line results. The return value does not end with
Asis.Text.Delimiter_Image.

These values are intended for two purposes. They are suitable for
inclusion in problem reports sent to the ASIS implementor. They can
be presumed to contain information useful when debugging the
implementation itself. They are also suitable for use by the ASIS
application when printing simple application debugging messages during
application development. They are intended to be, to some worthwhile
degree, intelligible to the user.
@end{DescribeCode}


@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[private]]}

@ChgDeleted{Version=[1],Text=[    @key[type] Id is @i{(Implementation_Defined)};
    Nil_Id : @key[constant] Id := @i{Implementation_Defined};]}

@ChgDeleted{Version=[1],Text=[@key[end] Asis.Ids;]}
@end{Example}

