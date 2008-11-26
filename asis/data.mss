@Part(data, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/data.mss,v $}
@comment{$Revision: 1.9 $ $Date: 2008/10/25 05:28:50 $}

@LabeledSection{package Asis.Data_Decomposition (optional)}


@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0035-1]}
@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Data_Decomposition]}Asis.Data_Decomposition
may exist. If it @Chg{Version=[2], New=[exists], Old=[existis]},, the package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis],Child=[Data_Decomposition]}Asis.Data_Decomposition @key[is]}]}

@ChgDeleted{Version=[1],Text=[Asis.Data_Decomposition]}

This package is optional.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0035-1]}
@Chg{Version=[2], New=[The operations provided by this package
may be used to determine the layout of a record or array type in cases where
this information is known statically.], Old=[Operations to decompose data values
using the ASIS type information and a Portable_Data stream, representing a data
value of that type.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2], Text=[An application can write data, using the Asis.Data_Decomposition.Portable_Transfer
package to an external medium for later retrieval by another application. The
second application reads that data and then uses this package to convert that data
into useful information. Simple discrete scalar types can be converted
directly into useful information. Composite types, such as records and arrays,
shall first be broken into their various discriminants and components.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2], Text=[A data stream representing a record value can be decomposed into a group
of discriminant and component data streams by extracting those streams from
the record's data stream. This extraction is performed by applying any of
the Record_Components which describe the discriminants and components of
the record type. Each discriminant and each component of a record type is
described by a Record_Component value. Each value encapsulates the
information needed, by the implementation, to efficiently extract the
associated field from a data stream representing a record value of the
correct type.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2], Text=[A data stream representing an array value can be decomposed into a group of
component data streams by extracting those streams from the array's data
stream. This extraction is performed by applying the single
Array_Component which describes the components of the array type. One
Array_Component value is used to describe all array components. The value
encapsulates the information needed, by the implementation, to efficiently
extract any of the array components.]}

@leading@keepnext@;Assumptions and Limitations of this Interface:
@begin{Enumerate}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2], Text=[The data stream is appropriate for the ASIS host machine. For example,
the implementation of this interface will not need to worry about
byte flipping or reordering of bits caused by movement of data between
machine architectures.]}

Records, arrays, and their components may be packed.

Records, array components, enumerations, and scalar types may have
representation and length clauses applied to them. This includes scalar
types used as record discriminants and array indices.

This specification supports two of the three type models discussed
below. Models @Chg{Version=[1],New=[1 and 2],Old=[A and B]} are
supported. Model @Chg{Version=[1],New=[3],Old=[C]} is not
supported.@ChgNote{Fix to match actual numbering}

@begin{InnerEnumerate}
@leading@;Simple "static" types contain no variants, have a single fixed 'Size,
      and all components and attributes are themselves static and/or fully
      constrained. The size and position for any component of the type can be
      determined without regard to constraints. For example:

@begin{ChildExample}
@key[type] Static_Record @key[is]
    @key[record]
        F1, F2 : Natural;
        C1     : Wide_Character;
        A1     : Wide_String (1..5);
    @key[end record];

@key[type] Static_Discriminated (X : Boolean) @key[is]
    @key[record]
        F1, F2 : Natural;
        C1     : Wide_Character;
    @key[end record];

@key[type] Static_Array   @key[is array] (Integer @key[range] 1 .. 100) @key[of] Boolean;
@key[type] Static_Enum    @key[is] (One, Two, Three);
@key[type] Static_Integer @key[is range] 1 .. 512;
@key[type] Static_Float   @key[is digits] 15 @key[range] -100.0 .. 100.0;
@key[type] Static_Fixed   @key[is delta] 0.1 @key[range] -100.0 .. 100.0;
@end{ChildExample}

@Comment{Start item 2}Simple "dynamic" types contain one or more components or
attributes whose size, position, or value depends on the value of one or more
constraints computed at execution time. This means that the size, position, or
number of components within the data type cannot be determined without
reference to constraint values.

@noprefix@leading@;Records containing components, whose size depends on discriminants
      of the record, can be handled because the discriminants for a record
      value are fully specified by the data stream form of the record value.
      For example:

@begin{ChildExample}
@key[type] Dynamic_Length (Length : Natural) @key[is]
    @key[record]
        S1 : Wide_String (1 .. Length);
    @key[end record];

@key[type] Dynamic_Variant (When : Boolean) @key[is]
    @key[record]
        @key[case] When @key[is]
            @key[when] True =>
                C1 : Wide_Character;
            @key[when] False =>
                @key[null];
        @key[end case];
    @key[end record];
@end{ChildExample}

@noprefix@leading@;Arrays with an unconstrained subtype, whose 'Length, 'First,
and 'Last depend on dynamic index constraints, can be handled because these
attributes can be queried and stored when the data stream is written. For
example:

@begin{ChildExample}
I : Integer := Some_Function;
@key[type] Dynamic_Array @key[is]
    @key[array] (Integer @key[range] I .. I + 10) @key[of] Boolean;

@key[type] Heap_Array   @key[is array] (Integer @key[range] <>) @key[of] Boolean;
@key[type] Access_Array @key[is access] Heap_Array;
X : Access_Array := @key[new] Heap_Array (1 .. 100);
@end{ChildExample}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0035-1]}
@Comment{Start item 3}@leading@;Complex, externally "discriminated" records,
contain one or more components whose size or position depends on the value of
one or more non-static external values (values not stored within instances of
the type) at execution time. The size for a value of the type cannot be
determined without reference to these external values, whose runtime values are
not known to the ASIS Context and cannot be automatically recorded by the
Asis.Data_Decomposition.Portable_Transfer generics. For example:
Asis.Data_Decomposition.Portable_Transfer generics. @ChgAdded{Version=[2], Text=[A class-wide type also falls in this category,
as does an array type with a dynamic component size.]}
For example:

@begin{ChildExample}
N : Natural := Function_Call();
....
@key[declare]
    @key[type] Complex @key[is]
        @key[record]
            S1 : Wide_String (1 .. N);
        @key[end record];
@key[begin]
    ....
@key[end];
@end{ChildExample}
@end{InnerEnumerate}
@end{Enumerate}

General Usage Rules:

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[All operations in this package will attempt to detect the use of invalid
data streams. A data stream is "invalid" if an operation determines that
the stream could not possibly represent a value of the expected variety.
Possible errors are: stream is of incorrect length, stream contains bit
patterns which are illegal, etc. The exception ASIS_Inappropriate_Element
is raised in these cases. The Status value is Data_Error. The
Diagnosis string will indicate the kind of error detected.]}

All implementations will handle arrays with a minimum of 16 dimensions,
or the number of dimensions allowed by their compiler, whichever is
smaller.


@LabeledClause{type Record_Component}

Type Record_Component describes one discriminant or component of a record type.

Implementation is highly implementation dependent. The "=" operator is not
meaningful between Record_Component values unless one of them is the
Nil_Record_Component value.

A record type describes composite values which contain zero or more
discriminant and component fields. A_Record_Type_Definition can be queried
to obtain a list of Record_Components. Each Record_Component contains the
information necessary to extract one discriminant or component field of the
record.

Record_Components are intended for use with data stream extraction
operations. An extraction operation is performed using a Record_Component,
in conjunction with a data stream representing a value of the record type.
The record data stream contains data for all fields of the record. The
result is an extracted data stream representing just the value of the one
field. Record_Components are implemented so as to allow for efficient
extraction of field values.

An extracted field data stream is suitable for all uses. If the field is a
scalar type, it can be converted directly into useful information. If the
field is, in turn, another composite value, it can be further decomposed
into its own component values.

There are two ways to obtain the Record_Components or the Array_Component
needed to further decompose an embedded composite field. First, if the
type of the field is known, the type definition can be directly queried to
obtain the Record_Components or the Array_Component that describe its
internal structure. Second, the Record_Component used to extract the field
can be queried to obtain the same Record_Components or the same
Array_Component. Both methods return identical information.

This kind of nested decomposition can be carried to any required level.

Record_Components become invalid when the Context, from which they
originate, is closed. All Record_Components are obtained by referencing a)
an Element, which has an associated Context, b) another Record_Component,
or c) an Array_Component. Ultimately, all component values originate from
a A_Type_Definition Element; that Element determines their Context of
origin.

@begin{DescribeCode}
@begin{Example}
@key[type] @AdaTypeDefn{Record_Component} @key[is private];
@AdaObjDefn{Nil_Record_Component} : @key[constant] Record_Component;

@key[function] "=" (Left  : @key[in] Record_Component;
              Right : @key[in] Record_Component)
              @key[return] Boolean @key[is abstract];
@end{Example}
@end{DescribeCode}


@LabeledClause{type Record_Component_List}

@begin{DescribeCode}
@begin{Example}
@key[type] @AdaTypeDefn{Record_Component_List} @key[is]
   @key[array] (Asis.List_Index @key[range] <>) @key[of] Record_Component;
@end{Example}
@end{DescribeCode}


@LabeledClause{type Array_Component}

Type Array_Component describes the components of an array valued field for a
record type.

Implementation is highly implementor dependent. The "=" operator is not
meaningful between Array_Component values unless one of them is the
Nil_Array_Component value.

An array type describes composite values which contain zero or more indexed
components. Both An_Unconstrained_Array_Definition or
A_Constrained_Array_Definition can be queried to obtain a single
Array_Component. The Array_Component contains the information necessary to
extract any arbitrary component of the array.

Array_Components are intended for use with data stream extraction
operations. An extraction operation is performed using an Array_Component,
in conjunction with a data stream representing a value of the array type.
The array data stream contains data for all components of the array. The
result is an extracted data stream representing just the value of the one
component. Array_Components are implemented so as to allow for efficient
extraction of array component values.

An extracted component data stream is suitable for all uses. If the
component is a scalar type, it can be converted directly into useful
information. If the component is, in turn, another composite value, it can
be further decomposed into its own component values.

There are two ways to obtain the Record_Components or the Array_Component
needed to further decompose an embedded composite component. First, if the
type of the component is known, the type definition can be directly queried
to obtain the Record_Components or the Array_Component that describe its
internal structure. Second, the Array_Component used to extract the
component can be queried to obtain the same Record_Components or the same
Array_Component. Both methods return identical information.

This kind of nested decomposition can be carried to any required level.

Array_Components become invalid when the Context, from which they
originate, is closed. All Record_Components are obtained by referencing a)
an Element, which has an associated Context, b) a Record_Component, or c)
another Array_Component. Ultimately, all component values originate from a
A_Type_Definition Element; that Element determines their Context of origin.

@begin{DescribeCode}
@begin{Example}
@key[type] @AdaTypeDefn{Array_Component} @key[is private];
@AdaObjDefn{Nil_Array_Component} : @key[constant] Array_Component;

@key[function] "=" (Left  : @key[in] Array_Component;
              Right : @key[in] Array_Component)
              @key[return] Boolean @key[is abstract];
@end{Example}
@end{DescribeCode}


@LabeledClause{type Array_Component_List}

@begin{DescribeCode}
@begin{Example}
@key[type] @AdaTypeDefn{Array_Component_List} @key[is]
   @key[array] (Asis.List_Index @key[range] <>) @key[of] Array_Component;
@end{Example}
@end{DescribeCode}


@LabeledClause{type Dimension_Indexes}

@Chg{Version=[1],New=[Type ],Old=[]}Dimension_Indexes
@Chg{Version=[1],New=[is],Old=[@en]} an array of index values used to access an
array stream.

@begin{DescribeCode}
@begin{Example}
@key[type] @AdaTypeDefn{Dimension_Indexes} @key[is]
   @key[array] (Asis.ASIS_Positive @key[range] <>) @key[of] Asis.ASIS_Positive;
@end{Example}
@end{DescribeCode}


@LabeledClause{type Array_Component_Iterator}

Type Array_Component_Iterator is used to iterate over successive components of
an array. This can be more efficient than using individual index values when
extracting array components from a data stream because it substitutes two
subroutine calls (Next and Done) for the multiplications and divisions implicit
in indexing an N dimensional array with a single index value.

Iterators can be copied. The copies operate independently (have separate
state).

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[An example:]}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[declare]
   Component        : Array_Component := ...;
   Iter             : Array_Component_Iterator;
   Array_Stream     : Portable_Data (...) := ...;
   Component_Stream : Portable_Data (...);
@key[begin]
   Iter := Array_Iterator (Component);
   @key[while not] Done (Iter) @key[loop]
      Component_Stream := Component_Data_Stream (Iter, Array_Stream);
      Next (Iter);
   @key[end loop];
@key[end];]}
@end{Example}


@begin{DescribeCode}
@begin{Example}
@key[type] @AdaTypeDefn{Array_Component_Iterator} @key[is] private;

@AdaObjDefn{Nil_Array_Component_Iterator} : @key[constant] Array_Component_Iterator;
@end{Example}
@end{DescribeCode}


@ChgNote{SI99-0035-1 remove type}
@LabeledRevisedClause{Version=[2],New=[obsolete type Portable_Data],
Old=[type Portable_Data]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - RLB}}]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[Portable_Data represents an ordered "stream" of data values.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[The portable representation for application data is an array of data
values. This portable data representation is guaranteed to be valid when
written, and later read, on the same machine architecture, using the same
implementor's compiler and runtime system. Portability of the data
values, across implementations and architectures, is not guaranteed.
Some implementors may be able to provide data values which are portable
across a larger subset of their supported machine architectures.]}

@ChgDeleted{Version=[2],Text=[Some of the problems encountered when changing architectures are: bit
order, byte order, floating point representation, and alignment
constraints. Some of the problems encountered when changing runtime
systems or implementations are: type representation, optimization,
record padding, and other I/O subsystem implementation variations.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Type=[Leading],Text=[The nature of these data values is deliberately unspecified. An
implementor will choose a data value type that is suitable for the
expected uses of these arrays and data values. Arrays and data
values have these uses:]}

@begin{Enumerate}
@ChgDeleted{Version=[2],Text=[Array values are used in conjunction with the
   Asis.Data_Decomposition interface. The data value type should be
   readily decomposable, by that package, so that array and record
   components can be efficiently extracted from a data stream represented
   by this array type. The efficiency of that interface is a priority.]}

@ChgDeleted{Version=[2],Type=[Leading],Text=[The data value type is read and
   written by applications. It should have a size that makes efficient I/O
   possible. Applications can be expected to perform I/O in any or all of these
   ways:]}

@begin{InnerEnumerate}
@ChgDeleted{Version=[2],Text=[Ada.Sequential_Io or Ada.Direct_Io could be used to read or write
      these values.]}

@ChgDeleted{Version=[2],Text=[Individual values may be placed inside other types and those types
      may be read or written.]}

@ChgDeleted{Version=[2],Text=[The 'Address of a data value, plus the 'Size of the data value
      type, may be used to perform low level system I/O. Note: This
      requires the 'Size of the type and the 'Size of a variable of that
      type to be the same for some implementations.]}

@ChgDeleted{Version=[2],Text=[Individual values may be passed through Unchecked_Conversion in
      order to obtain a different value type, of the same 'Size, suitable
      for use with some user I/O facility. This usage is non-portable
      across implementations.]}
@end{InnerEnumerate}

@ChgDeleted{Version=[2],Type=[Leading],Text=[Array values are read and written by applications. The data value
   type should have a size that makes efficient I/O possible.
   Applications can be expected to perform I/O in any or all of these ways:]}

@begin{InnerEnumerate}
@ChgDeleted{Version=[2],Text=[Ada.Sequential_Io or Ada.Direct_Io could be used to read or write a
      constrained array subtype.]}

@ChgDeleted{Version=[2],Text=[Array values may be placed inside other types and those types may
      be read and written.]}

@ChgDeleted{Version=[2],Text=[The 'Address of the first array value, plus the 'Length of the
      array times the 'Size of the values, may be used to perform low
      level system I/O. Note: This implies that the array type is
      unpacked, or, that the packed array type has no "padding" (e.g.,
      groups of five 6-bit values packed into 32-bit words with 2 bits
      of padding every 5 elements).]}

@ChgDeleted{Version=[2],Text=[Array values may be passed through Unchecked_Conversion in order to
      obtain an array value, with a different value type, suitable for
      use with some user I/O facility. This usage is non-portable across
      implementations.]}
@end{InnerEnumerate}
@end{Enumerate}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[The data value type should be chosen so that the 'Address of the first
array data value is also the 'Address of the first storage unit containing
array data. This is especially necessary for target architectures where
the "bit" instructions address bits in the opposite direction as that used
by normal machine memory (or array component) indexing. A recommended
'Size is System.Storage_Unit (or a multiple of that size).]}

@ChgDeleted{Version=[2],Text=[Implementations that do not support Unchecked_Conversion of array values,
or which do not guarantee that Unchecked_Conversion of array values will
always "do the right thing" (convert only the data, and not the dope vector
information), should provide warnings in their ASIS documentation that
detail possible consequences and work-arounds.]}

@ChgDeleted{Version=[2],Text=[The index range for the Portable_Data type shall be a numeric type whose
range is large enough to encompass the Portable_Data representation for all
possible runtime data values.]}

@ChgDeleted{Version=[2],Text=[All conversion interfaces always return Portable_Data array values with a
'First of one (1).]}

@ChgDeleted{Version=[2],Text=[The Portable_Value type may be implemented in any way
whatsoever. It need not be a numeric type.]}

@begin{DescribeCode}
@begin{Example}
@ChgDeleted{Version=[2],Text=[@key[type] @AdaTypeDefn{Portable_Value} @key[is] @i{(Implementation_Defined)};]}

@ChgDeleted{Version=[2],Text=[@key[subtype] @AdaSubTypeDefn{Name=[Portable_Positive],Of=[Asis.ASIS_Positive]} @key[is] Asis.ASIS_Positive
   @key[range] 1 .. @i{Implementation_Defined_Integer_Constant};]}

@ChgDeleted{Version=[2],Text=[@key[type] @AdaTypeDefn{Portable_Data} @key[is array] (Portable_Positive @key[range] <>) @key[of] Portable_Value;]}

@ChgDeleted{Version=[2],Text=[@AdaObjDefn{Nil_Portable_Data} : Portable_Data (1 .. 0);]}
@end{Example}
@end{DescribeCode}


@LabeledClause{type Type_Model_Kinds}

@ChgDeleted{Version=[1],Text=[Type_Model_Kinds]}

Each Type_Definition fits into one of three type models.

@begin{DescribeCode}
@begin{Example}
@key[type] @AdaTypeDefn{Type_Model_Kinds} @key[is] (A_Simple_Static_Model,
                          A_Simple_Dynamic_Model,
                          A_Complex_Dynamic_Model,
                          Not_A_Type_Model);           -- @examcom{Nil arguments}
@end{Example}
@end{DescribeCode}


@LabeledClause{function Type_Model_Kind}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Type_Model_Kind} (Type_Definition : @key[in] Asis.Type_Definition)
                         @key[return] Type_Model_Kinds;

@key[function] @AdaSubDefn{Type_Model_Kind} (Component : @key[in] Record_Component)
                         @key[return] Type_Model_Kinds;

@key[function] @AdaSubDefn{Type_Model_Kind} (Component : @key[in] Array_Component)
                         @key[return] Type_Model_Kinds;
@end{Example}

Type_Definition @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the type definition to query.
Component @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} a record field with a record or array type.

Returns the model that best describes the type indicated by the argument.
Returns Not_A_Type_Model for any unexpected argument such as a Nil value.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Type_Definition expects an element
that has the following],Old=[Expected]} Element_Kinds:
@begin{Display}
A_Type_Definition
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}
@end{DescribeCode}


@LabeledClause{function Is_Nil (component)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Nil} (Right : @key[in] Record_Component) @key[return] Boolean;

@key[function] @AdaSubDefn{Is_Nil} (Right : @key[in] Array_Component)  @key[return] Boolean;
@end{Example}

Right @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component to
check.

Returns True if Right is a Nil (or uninitialized) component value.

Returns False for all other values.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Right expects any kind of component.],
Old=[All component values are appropriate.]}
@end{DescribeCode}


@LabeledClause{function Is_Equal (component)}

@begin{DescribeCode}
@begin{Example}
    @key[function] @AdaSubDefn{Is_Equal} (Left  : @key[in] Record_Component;
                       Right : @key[in] Record_Component) @key[return] Boolean;

    @key[function] @AdaSubDefn{Is_Equal} (Left  : @key[in] Array_Component;
                       Right : @key[in] Array_Component)  @key[return] Boolean;
@end{Example}

Left @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the left component to compare.
Right @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the right component to compare.

Returns True if Left and Right represent the same physical component of the
same record or array type, from the same physical compilation unit. The
two components may or may not be from the same open ASIS Context variable.

@leading@keepnext@;Implies:
@begin{ChildExample}
Is_Equal (Enclosing_Compilation_Unit (Component_Declaration (Left)),
          Enclosing_Compilation_Unit (Component_Declaration (Right))) = True
@end{ChildExample}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Right and Left expect any kind of component.],
Old=[All component values are appropriate.]}
@end{DescribeCode}


@LabeledClause{function Is_Identical (component)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Identical} (Left  : @key[in] Record_Component;
                       Right : @key[in] Record_Component) @key[return] Boolean;

@key[function] @AdaSubDefn{Is_Identical} (Left  : @key[in] Array_Component;
                       Right : @key[in] Array_Component)  @key[return] Boolean;
@end{Example}

Left @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the left component to compare.
Right @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the right component to compare.

Returns True if Left and Right represent the same physical component of the
same record or array type, from the same physical compilation unit and the
same open ASIS Context variable.

@leading@keepnext@;Implies:
@begin{ChildExample}
Is_Identical (Enclosing_Compilation_Unit (Component_Declaration (Left)),
              Enclosing_Compilation_Unit (Component_Declaration (Right))) = True
@end{ChildExample}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Right and Left expect any kind of component.],
Old=[All component values are appropriate.]}
@end{DescribeCode}


@LabeledClause{function Is_Array}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Array} (Component : @key[in] Record_Component) @key[return] Boolean;

@key[function] @AdaSubDefn{Is_Array} (Component : @key[in] Array_Component)  @key[return] Boolean;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} any component.

Returns True if the component has an array subtype (contains an array
value).

Returns False for Nil components and any component that is not an embedded
array.
@end{DescribeCode}


@LabeledClause{function Is_Record}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Record} (Component : @key[in] Record_Component) @key[return] Boolean;

@key[function] @AdaSubDefn{Is_Record} (Component : @key[in] Array_Component)  @key[return] Boolean;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} any
component.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0035-1]}
Returns True if the component has a record@Chg{Version=[2], New=[,
task, or protected subtype.
Returns True for a task or protected component because such a
component may have discriminants], Old=[ subtype]}.
@end{DescribeCode}


@LabeledClause{function Done}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Done} (Iterator : @key[in] Array_Component_Iterator) @key[return] Boolean;
@end{Example}

Iterator @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the iterator
to query.

Returns True if the iterator has been advanced past the last array
component. Returns True for a Nil_Array_Component_Iterator.
@end{DescribeCode}


@LabeledClause{procedure Next}

@begin{DescribeCode}
@begin{Example}
@key[procedure] @AdaSubDefn{Next} (Iterator : @key[in out] Array_Component_Iterator);
@end{Example}

Iterator @Chg{Version=[1],New=[specifies],Old=[    @en Specifies]} the iterator to advance.

Advances the iterator to the next array component. Use Done to test the
iterator to see if it has passed the last component. Does nothing if the
iterator is already past the last component.
@end{DescribeCode}


@LabeledClause{procedure Reset}

@begin{DescribeCode}
@begin{Example}
@key[procedure] @AdaSubDefn{Reset} (Iterator : @key[in out] Array_Component_Iterator);
@end{Example}

Iterator @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the iterator to reset

Resets the iterator to the first array component.
@end{DescribeCode}


@LabeledClause{function Array_Index}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Array_Index} (Iterator : @key[in] Array_Component_Iterator)
                     @key[return] Asis.ASIS_Natural;
@end{Example}

Iterator @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the iterator to query.

Returns the Index value which, when used in conjunction with the
Array_Component value used to create the Iterator, indexes the same array
component as that presently addressed by the Iterator.

Raises ASIS_Inappropriate_Element if given a Nil_Array_Component_Iterator
or one where Done(Iterator) = True. The Status value is Data_Error.
The Diagnosis string will indicate the kind of error detected.
@end{DescribeCode}


@LabeledClause{function Array_Indexes}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Array_Indexes} (Iterator : @key[in] Array_Component_Iterator)
                        @key[return] Dimension_Indexes;
@end{Example}

Iterator @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the iterator to query.

Returns the index values which, when used in conjunction with the
Array_Component value used to create the Iterator, indexes the same array
component as that presently addressed by the Iterator.

Raises ASIS_Inappropriate_Element if given a Nil_Array_Component_Iterator
or one where Done(Iterator) = True. The Status value is Data_Error.
The Diagnosis string will indicate the kind of error detected.
@end{DescribeCode}


@LabeledClause{function Discriminant_Components}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Discriminant_Components} (Type_Definition : @key[in] Asis.Type_Definition)
                                 @key[return] Record_Component_List;

@key[function] @AdaSubDefn{Discriminant_Components} (Component : @key[in] Record_Component)
                                 @key[return] Record_Component_List;

@key[function] @AdaSubDefn{Discriminant_Components} (Component : @key[in] Array_Component)
                                 @key[return] Record_Component_List;
@end{Example}

Type_Definition @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the record type definition to query.
Component @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} a component which has a record subtype,
                  Is_Record(Component) = True.

Returns a list of the discriminant components for records of the indicated
record type.

The result describes the locations of the record type's discriminants,
regardless of the static or dynamic nature of the record type.
All return components are intended for use with a data stream representing
a value of the indicated record type.

All Is_Record(Component) = True arguments are appropriate. All return
values are valid parameters for all query operations.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Type_Definition expects an element
that has one of the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Type_Definition@Chg{Version=[2],New=[ that has one of the following Type_Kinds:
   A_Derived_Type_Definition       (derived from a record type)
   A_Record_Type_Definition],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Derived_Type_Definition       (derived from a record type)
A_Record_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Component expects a component
that has one of the following],Old=[Appropriate]} Asis.Data_Decomposition.Type_Model_Kinds:
@begin{Display}
A_Simple_Static_Model
A_Simple_Dynamic_Model
A_Complex_Dynamic_Model
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element or component that does not have one of these
expected kinds.]}
@end{DescribeCode}


@LabeledClause{function Record_Components (data decomposition)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Record_Components} (Type_Definition : @key[in] Asis.Type_Definition)
                           @key[return] Record_Component_List;

@key[function] @AdaSubDefn{Record_Components} (Component : @key[in] Record_Component)
                           @key[return] Record_Component_List;

@key[function] @AdaSubDefn{Record_Components} (Component : @key[in] Array_Component)
                           @key[return] Record_Component_List;
@end{Example}

Type_Definition @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the record type definition to query.
Component @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} a component
which has a record subtype, Is_Record(Component) = True.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0035-1]}
Returns a list of the discriminants and components for the indicated simple
static record type.@Chg{Version=[2],New=[],Old=[ (See rule 6.A above.)]}

The result describes the locations of the record type's discriminants and
components. All return components are intended for use with a data stream
representing a value of the indicated record type.

All Is_Record (Component) = True values, having simple static types, are
appropriate. All return values are valid parameters for all query operations.

@begin{SingleNote}
@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
If an Ada implementation uses implementation-dependent record
components (@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 13.5.1 (15)), then each such component of
the record type is included in the result.
@end{SingleNote}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Type_Definition expects an element
that has one of the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Type_Definition@Chg{Version=[2],New=[ that has one of Type_Kinds:
   A_Derived_Type_Definition       (derived from a record type)
   A_Record_Type_Definition],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Derived_Type_Definition       (derived from a record type)
A_Record_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Component expects a component
of],Old=[Appropriate]} Asis.Data_Decomposition.Type_Model_Kinds:
@begin{Display}
A_Simple_Static_Model
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element or component that does not have one of these
expected kinds.]}
@end{DescribeCode}


@ChgNote{SI99-0035-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function Record_Components (stream)],
Old=[function Record_Components (stream)]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - RLB}}]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Record_Components}
            (Type_Definition : @key[in] Asis.Type_Definition;
             Data_Stream     : @key[in] Portable_Data)
            @key[return] Record_Component_List;]}

@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Record_Components}
            (Component   : @key[in] Record_Component;
             Data_Stream : @key[in] Portable_Data)
            @key[return] Record_Component_List;]}

@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Record_Components}
            (Component   : @key[in] Array_Component;
             Data_Stream : @key[in] Portable_Data)
            @key[return] Record_Component_List;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[Type_Definition @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the record type definition to query.
Component @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} a component which has a record subtype,
                  Is_Record(Component) = True.
Data_Stream @Chg{Version=[1],New=[specifies],Old=[    @en Specifies]} a data stream containing, at least, the
                  complete set of discriminant or index constraints for the type.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[Returns a list of the discriminants and components for the indicated record
type, using the data stream argument as a guide. The record type shall be
either a simple static, or a simple dynamic, record
type. (See rules 6.A and 6.B above.)]}

@ChgDeleted{Version=[2],Text=[The result describes the locations of the record type's discriminants and
all components of the appropriate variant parts. The contents of the list
are determined by the discriminant values present in the data stream.]}

@ChgDeleted{Version=[2],Text=[A simple static type will always return the same component list (Is_Equal
parts) regardless of the Data_Stream, because the layout of a simple static
type does not change with changes in discriminant values. A simple dynamic
type returns different component lists (non-Is_Equal parts) depending on
the contents of the Data_Stream, because the contents and layout of a
simple dynamic type changes with changes in discriminant values. All
return components are intended for use with a data stream representing a
value of the indicate record type.]}

@ChgDeleted{Version=[2],Text=[The Data_Stream shall represent a fully discriminated value of the indicated
record type. The stream may have been read from a file, it may have been
extracted from some enclosing data stream, or it may be an artificial value
created by the Construct_Artificial_Data_Stream operation. Only the
discriminant portion of the Data_Stream is checked for validity, and, only
some discriminant fields may need to be checked, depending on the
complexity of the record type. The best approach, for any application that
is constructing artificial data streams, is to always provide appropriate
values for all discriminant fields. It is not necessary to provide values
for non-discriminant fields.]}

@ChgDeleted{Version=[2],Text=[All Is_Record(Component) = True values are appropriate. All return values
are valid parameters for all query operations.]}

@begin{SingleNote}
@ChgDeleted{Version=[2],Text=[If an Ada implementation uses implementation-dependent record
components (Reference Manual 13.5.1 (15)), then each such component of the
record type is included in the result.]}
@end{SingleNote}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Type=[Leading],Keepnext=[T],Text=[
@Chg{Version=[2],New=[Type_Definition expects an element
that has one of the following],Old=[Appropriate]} Element_Kinds:]}
@begin{Display}
@ChgDeleted{Version=[2],Text=[A_Type_Definition@Chg{Version=[2],New=[ that has one of the following Type_Kinds:
   A_Derived_Type_Definition       (derived from a record type)
   A_Record_Type_Definition],Old=[]}]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Derived_Type_Definition       (derived from a record type)
A_Record_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Type=[Leading],Keepnext=[T],Text=[Appropriate Asis.Data_Decomposition.Type_Model_Kinds:]}
@begin{Display}
@ChgDeleted{Version=[2],Text=[A_Simple_Static_Model
A_Simple_Dynamic_Model]}
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element or component that does not have one of these
expected kinds.]}
@end{DescribeCode}


@LabeledClause{function Array_Components}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Array_Components} (Type_Definition : @key[in] Asis.Type_Definition)
                          @key[return] Array_Component;

@key[function] @AdaSubDefn{Array_Components} (Component : @key[in] Record_Component)
                          @key[return] Array_Component;

@key[function] @AdaSubDefn{Array_Components} (Component : @key[in] Array_Component)
                          @key[return] Array_Component;
@end{Example}

Type_Definition @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the array type definition to query.
Component @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} a
component which has an array subtype, Is_Array(Component) = True.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0035-1]}
Returns a single component, describing all components of the indicated
array type. The array type shall be a simple static, or a simple dynamic
array type.@Chg{Version=[2],New=[],Old=[ (See rules 6.A and 6.B above.)]}

The result contains all information necessary to index and extract any
component of a data stream representing a value of the indicated array
type.

All Is_Array (Component) = True values are appropriate. All return values
are valid parameters for all query operations.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Type_Definition expects an element
that has one of the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Type_Definition@Chg{Version=[2],New=[ that has one of the following Type_Kinds:
   A_Derived_Type_Definition       (derived from an array type)
   An_Unconstrained_Array_Definition
   A_Constrained_Array_Definition],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Derived_Type_Definition       (derived from an array type)
An_Unconstrained_Array_Definition
A_Constrained_Array_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Component expects a component
that has one of the following],Old=[Appropriate]} Asis.Data_Decomposition.Type_Model_Kinds:
@begin{Display}
A_Simple_Static_Model
A_Simple_Dynamic_Model
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element or component that does not have one of these
expected kinds.]}
@end{DescribeCode}


@LabeledClause{function Array_Iterator}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2], Text=[@key[function] @AdaSubDefn{Array_Iterator} (Type_Definition : @key[in] Asis.Type_Definition)
                        @key[return] Array_Component_Iterator;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0035-1]}
@ChgAdded{Version=[2], Text=[@key[function] @AdaSubDefn{Array_Iterator} (Component : @key[in] Record_Component)
                        @key[return] Array_Component_Iterator;]}

@key[function] @AdaSubDefn{Array_Iterator} (Component : @key[in] Array_Component)
                        @key[return] Array_Component_Iterator;
@end{Example}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0035-1]}
@Chg{Version=[2], New=[Type_Definition specifies the array type definition to
query. ],Old=[]}omponent @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]}
New=[a component which has an array subtype,
Is_Array (Component) = True.],Old=[an array component to be used for iteration]}

Returns an iterator poised to fetch the 1st component of an array.
@end{DescribeCode}


@ChgNote{SI99-0035-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function Component_Data_Stream],
Old=[function Component_Data_Stream]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - RLB}}]}


@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Component_Data_Stream}
            (Component   : @key[in] Record_Component;
             Data_Stream : @key[in] Portable_Data)
            @key[return] Portable_Data;]}

@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Component_Data_Stream}
            (Component   : @key[in] Array_Component;
             Index       : @key[in] Asis.ASIS_Positive;
             Data_Stream : @key[in] Portable_Data)
            @key[return] Portable_Data;]}

@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Component_Data_Stream}
            (Component   : @key[in] Array_Component;
             Indexes     : @key[in] Dimension_Indexes;
             Data_Stream : @key[in] Portable_Data)
            @key[return] Portable_Data;]}

@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Component_Data_Stream}
            (Iterator    : @key[in] Array_Component_Iterator;
             Data_Stream : @key[in] Portable_Data)
            @key[return] Portable_Data;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component or discriminant to be extracted.
Index @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} an index, 1..Array_Length, within an array.
Indexes @Chg{Version=[1],New=[specifies],Old=[    @en Specifies]} a list of index values, there is one value for
              each dimension of the array type, each index N is in the
              range 1..Array_Length (Component, N);.
Iterator @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the array component to extract.
Data_Stream @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the data stream from which to extract the result.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[Returns a data stream representing just the value of the chosen Component.
The return value is sliced from the data stream. The Data_Stream shall
represent a value of the appropriate type. It may have been obtained from
a file, extracted from another data stream, or artificially constructed
using the Construct_Artificial_Data_Stream operation.]}

@ChgDeleted{Version=[2],Text=[An artificial data stream may raise ASIS_Inappropriate_Element (the Status
is Value_Error). Only the constraint values are valid, once they
have been properly initialized, and can be safely extracted from an
artificial data stream.]}

@ChgDeleted{Version=[2],Text=[Raises ASIS_Inappropriate_Element if given a Nil_Array_Component_Iterator
or one where Done(Iterator) = True. The Status value is Data_Error.
The Diagnosis string will indicate the kind of error detected.]}
@end{DescribeCode}


@LabeledClause{function Component_Declaration}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Component_Declaration} (Component : @key[in] Record_Component)
                               @key[return] Asis.Declaration;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component to be queried

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
Returns an Asis.Declaration, which is either A_Component_Declaration
or A_Discriminant_Specification. These values can be used to determine the
subtype, type, and base type of the record component. The result may be an
explicit declaration made by the user, or, it may be an implicit
component declaration for an implementation-defined component (@Chg{Version=[2],
New=[Ada Standard],Old=[Reference Manual]} 13.5.1(15)).

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Component expects any kind of non-Nil component.],
Old=[All non-Nil component values are appropriate.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that has
one of the following ],Old=[]}Element_Kinds:
@begin{Display}
A_Declaration@Chg{Version=[2],New=[ that has one of the following Declaration_Kinds:
    A_Component_Declaration
    A_Discriminant_Specification],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Returns Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Component_Declaration
A_Discriminant_Specification]}
@end{Display}
@end{DescribeCode}


@LabeledClause{function Component_Indication}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Component_Indication} (Component : @key[in] Array_Component)
                              @key[return] Asis.Subtype_Indication;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component
to be queried.

Returns an Asis.Subtype_Indication. These values can be used to determine
the subtype, type, and base type of the array components.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Component expects any kind of non-Nil component.], Old=[All non-Nil component values are appropriate.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;Returns @Chg{Version=[2],New=[an element that has one
of the following ],Old=[]}Element_Kinds:
@begin{Display}
A_Subtype_Indication
@end{Display}
@end{DescribeCode}


@LabeledClause{function All_Named_Components}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{All_Named_Components} (Type_Definition : @key[in] Asis.Type_Definition)
                              @key[return] Asis.Defining_Name_List;
@end{Example}

Type_Definition @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the record type definition to query.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
Returns a list of all discriminant and component entity names defined by
the record type. All record type definitions are appropriate for this
operation. This query provides a means for determining whether a field,
with a particular name, exists for some possible instance of the record
type. This list does not include the names of implementation-defined
components (@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]} 13.5.1 (15)); those name have the form of
An_Attribute_Reference expression.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Type_Definition expects an element
that has one of the following],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Type_Definition@Chg{Version=[2],New=[ that has one of the following Type_Kinds:
   A_Derived_Type_Definition       (derived from a record type)
   A_Record_Type_Definition],Old=[]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Derived_Type_Definition       (derived from a record type)
A_Record_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[and Type_Definition expects an element
that has one of the following],Old=[Appropriate]} Asis.Data_Decomposition.Type_Model_Kinds:
@begin{Display}
A_Simple_Static_Model
A_Simple_Dynamic_Model
A_Complex_Dynamic_Model
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element that does not have one of these expected
kinds.]}
@end{DescribeCode}


@LabeledClause{function Array_Length}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Array_Length} (Component : @key[in] Record_Component)
                      @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Array_Length} (Component : @key[in] Array_Component)
                      @key[return] Asis.ASIS_Natural;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component
to query.

Returns the number of components within an array valued component. The
array subtype may be multidimensional. The result treats the array as if
it were unidimensional. It is the product of the 'Lengths of the
individual array dimensions.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Component expects a component that has
Is_Array(Component) = True],Old=[values are appropriate]}.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any component that is not an array component.]}
@end{DescribeCode}


@LabeledClause{function Array_Length (with dimension)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Array_Length} (Component : @key[in] Record_Component;
                       Dimension : @key[in] Asis.ASIS_Natural)
                      @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Array_Length} (Component : @key[in] Array_Component;
                       Dimension : @key[in] Asis.ASIS_Natural)
                       @key[return] Asis.ASIS_Natural;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component
to query. Dimension @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
array dimension to query.

Returns the number of components within an array valued component. The
array subtype may be unidimensional. The result is the 'Length(Dimension)
of the array.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@Chg{Version=[2],New=[Component expects a component that has
Is_Array(Component) = True],Old=[values are appropriate]}.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any component that is not an array component.]}
@end{DescribeCode}


@LabeledClause{function Size}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Size} (Type_Definition : @key[in] Asis.Type_Definition)
              @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Size} (Component : @key[in] Record_Component) @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Size} (Component : @key[in] Array_Component)  @key[return] Asis.ASIS_Natural;
@end{Example}

Type_Definition @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} a type definition, whose 'Size is desired.
Component @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} a component, whose 'Size is desired.

Returns the minimum number of bits required to hold a simple static type,
the number of bits allocated to hold a record field, or the number of bits
allocated to hold each array component.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Type_Definition expects any kind of non-Nil element.], Old=[All non-Nil component values are appropriate.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Type_Definition expects an element
of],Old=[Appropriate]} Element_Kinds:
@begin{Display}
A_Type_Definition
@end{Display}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0028-1]}
@leading@keepnext@;@Chg{Version=[2],New=[Component expects a component
that has the following],Old=[Appropriate]} Asis.Data_Decomposition.Type_Model_Kinds:
@begin{Display}
A_Simple_Static_Model
@end{Display}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@ChgAdded{Version=[2],Text=[Raises ASIS_Inappropriate_Element with a Status
of Value_Error for any element or component that does not have one of these
expected kinds.]}
@end{DescribeCode}

@begin{UsageNote}
For components, this is the number of bits allocated
within the composite value. It may be greater than the number
of bits occupied by data values of this component type.
Also, the data value, when occupying more space than is
minimally required, may be preceded, followed, or surrounded by
padding bits which are necessary to fully occupy the space allotted.
@end{UsageNote}


@ChgNote{SI99-0035-1 remove subprogram}
@LabeledRevisedClause{Version=[2],New=[obsolete function Size (stream)],
Old=[function Size (stream)]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header is left for now;
removing it now would change all of the clause numbers,
and that would make a mess for editing and reference purposes. Ultimately,
when the final standard is produced, it will be removed. - RLB}}]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Size} (Type_Definition : @key[in] Asis.Type_Definition;
               Data_Stream     : @key[in] Portable_Data)
              @key[return] Asis.ASIS_Natural;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[Type_Definition @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the type definition to query.
Data_Stream @Chg{Version=[1],New=[specifies],Old=[    @en Specifies]} a data stream containing, at least, the complete
set of discriminant or index constraints for the type.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[Returns the 'Size of a value of this type, with these constraints. This is
the minimum number of bits that is needed to hold any possible value of the
given fully constrained subtype. Only the constraint portion of the
Data_Stream is checked.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[The Data_Stream may be a data stream or it may be an artificial
data stream created by the Construct_Artificial_Data_Stream operation.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Element_Kinds:]}
@begin{Display}
@ChgDeleted{Version=[2],Text=[A_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate
Asis.Data_Decomposition.Type_Model_Kinds:]}
@begin{Display}
@ChgDeleted{Version=[2],Text=[A_Simple_Static_Model
A_Simple_Dynamic_Model]}
@end{Display}
@end{DescribeCode}


@LabeledClause{function Position}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Position} (Component : @key[in] Record_Component)
                  @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Position} (Component : @key[in] Array_Component;
                   Index     : @key[in] Asis.ASIS_Positive)
                  @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Position} (Component : @key[in] Array_Component;
                   Indexes   : @key[in] Dimension_Indexes)
                  @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Position} (Iterator : @key[in] Array_Component_Iterator)
                  @key[return] Asis.ASIS_Natural;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component to query.
Index @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} a value in the range 1..Array_Length (Component),
the index of the component to query.
Indexes @Chg{Version=[1],New=[specifies],Old=[    @en Specifies]} a list of index values, there is one value for
each dimension of the array type, each index N is in the
range 1..Array_Length (Component, N);.
Iterator @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} a particular
array component to query.

Returns the System.Storage_Unit offset, from the start of the first storage
unit occupied by the enclosing composite type, of the first of the storage
units occupied by the Component. The offset is measured in storage units.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Component expects any kind of non-Nil component.],
Old=[All non-Nil component values are appropriate.]} Raises
ASIS_Inappropriate_Element with a Status of Data_Error if any index is not
in the expected range or if Done (Iterator) = True. The Status value will
be Data_Error. The Diagnosis string will indicate the kind of error
detected.
@end{DescribeCode}


@LabeledClause{function First_Bit}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{First_Bit} (Component : @key[in] Record_Component)
                   @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{First_Bit} (Component : @key[in] Array_Component;
                    Index     : @key[in] Asis.ASIS_Positive)
                   @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{First_Bit} (Component : @key[in] Array_Component;
                    Indexes   : @key[in] Dimension_Indexes)
                   @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{First_Bit} (Iterator : @key[in] Array_Component_Iterator)
                   @key[return] Asis.ASIS_Natural;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component to query.
Index @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} a value in the range
1..Array_Length (Component), the index of the component to query.
Indexes @Chg{Version=[1],New=[specifies],Old=[    @en Specifies]} a list of index values, there is one value for
each dimension of the array type, each index N is in the
range 1..Array_Length (Component, N);.
Iterator @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} a particular
array component to query.

Returns the bit offset, from the start of the first of the storage units
occupied by the Component, of the first bit occupied by the Component. The
offset is measured in bits.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Component expects any kind of non-Nil component.],
Old=[All non-Nil component values are appropriate.]} Raises
ASIS_Inappropriate_Element with a Status of Data_Error if any index is not
in the expected range or if Done (Iterator) = True. The Status value will
be Data_Error. The Diagnosis string will indicate the kind of error
detected.
@end{DescribeCode}


@LabeledClause{function Last_Bit}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Last_Bit} (Component : @key[in] Record_Component)
                  @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Last_Bit} (Component : @key[in] Array_Component;
                   Index     : @key[in] Asis.ASIS_Positive)
                  @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Last_Bit} (Component : @key[in] Array_Component;
                   Indexes   : @key[in] Dimension_Indexes)
                  @key[return] Asis.ASIS_Natural;

@key[function] @AdaSubDefn{Last_Bit} (Iterator : @key[in] Array_Component_Iterator)
                  @key[return] Asis.ASIS_Natural;
@end{Example}

Component @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the component to query.
Index @Chg{Version=[1],New=[specifies],Old=[      @en Specifies]} a value in the range 1..Array_Length (Component),
the index of the component to query.
Indexes @Chg{Version=[1],New=[specifies],Old=[    @en Specifies]} a list of index values, there is one value for
each dimension of the array type, each index N is in the
range 1..Array_Length (Component, N);.
Iterator @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} a particular array component to query.

Returns the bit offset, from the start of the first of the storage units
occupied by the Index'th Element, of the last bit occupied by the Element.
The offset is measured in bits.

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0028-1]}
@Chg{Version=[2], New=[Component expects any kind of non-Nil component.],
Old=[All non-Nil component values are appropriate.]} Raises
ASIS_Inappropriate_Element with a Status of Data_Error if any index is not
in the expected range or if Done (Iterator) = True. The Status value will
be Data_Error. The Diagnosis string will indicate the kind of error detected.
@end{DescribeCode}


@ChgNote{SI99-0035-1 remove subprogram}
@Chg{Version=[2],New=[],Old=[@b<@i<obsolete function Portable_Constrained_Subtype>>]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header was removed as there are
no remaining clauses after it. - RLB}}]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[Generic for Data Stream Conversions.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[@key[generic]
    -- @examcom{Ada notation for a constrained subtype.}
    -- @examcom{@key[type] Constrained_Subtype (<>) @key[is private];}
    @key[type] Constrained_Subtype @key[is private];
@key[function] @AdaSubDefn{Portable_Constrained_Subtype}
            (Data_Stream : @key[in] Portable_Data)
            @key[return] Constrained_Subtype;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[Data_Stream @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} an extracted
component of a record.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[Instantiated with an appropriate scalar type, (e.g.,
@Chg{Version=[2], New=[Standard.Integer], Old=[System.Integer]}, can be
used to convert a data stream to a value that can be directly examined).]}

@ChgDeleted{Version=[2],Text=[Instantiated with a record type, can be used to convert a data stream to a
value that can be directly examined.]}

@ChgDeleted{Version=[2],Text=[Instantiations with constrained array subtypes may not convert array values
if they were created using the Portable_Array_Type_1,
Portable_Array_Type_2, or Portable_Array_Type_3 interfaces.]}

@ChgDeleted{Version=[2],Text=[May raise Constraint_Error if the subtype is a scalar and the converted
value is not in the subtype's range.]}
@end{DescribeCode}


@Chg{Version=[2],New=[],Old=[@b<@i<obsolete function Construct_Artificial_Data_Stream>>]}
@ChgAdded{Version=[2],Text=[@b{@i{This clause header was removed as there are
no remaining clauses after it. - RLB}}]}


@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[@key[function] @AdaSubDefn{Construct_Artificial_Data_Stream}
            (Type_Definition : @key[in] Asis.Type_Definition;
             Data_Stream     : @key[in] Portable_Data;
             Discriminant    : @key[in] Record_Component;
             Value           : @key[in] Portable_Data)
            @key[return] Portable_Data;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Text=[Type_Definition @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the record type definition for the record
valued data stream being constructed.
Data_Stream @Chg{Version=[1],New=[specifies],Old=[    @en Specifies]} the data stream constructed so far; initially
specified as the Nil_Portable_Data value.
Discriminant @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the discriminant of the record type that is
being set or changed.
Value @Chg{Version=[1],New=[specifies],Old=[          @en Specifies]} a data stream representing a single
discriminant value of the appropriate type.]}

@ChgDeleted{Version=[2],Text=[Used to artificially construct a data stream which represents the
discriminant portion of a fully constrained value of the indicated record
type. This operation is called once with a value for each discriminant of
the record type (the order in which the discriminants are specified is not
important). The return value of each call is used as the input Data_Stream
for the next.]}

@ChgDeleted{Version=[2],Text=[The resulting artificial data stream may be used solely for the purpose of
creating Record_Component values. The values of any non-discriminant
fields are arbitrary and quite possibly invalid. The resulting
component values may then be used for any purpose. In particular, they may
be used to determine First_Bit, Last_Bit, and Size values for all record
discriminants and components.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1],ARef=[SI99-0035-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[
Appropriate Element_Kinds:]}
@begin{Display}
@ChgDeleted{Version=[2],Text=[A_Type_Definition]}
@end{Display}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0028-1]}
@ChgDeleted{Version=[2],Keepnext=[T],Type=[Leading],Text=[Appropriate Type_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[A_Derived_Type_Definition       (derived from a record type)
A_Record_Type_Definition]}
@end{Display}

@ChgDeleted{Version=[2],Text=[Raises ASIS_Inappropriate_Element, with a Status of Data_Error, if the
discriminant Value is inappropriate for the specified Discriminant.]}
@end{DescribeCode}


@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[private]]}

@ChgDeleted{Version=[1],Text=[   @key[type] Record_Component @key[is]
      @key[record]
         A : Boolean;
      @key[end record];]}

@ChgDeleted{Version=[1],Text=[   Nil_Record_Component : @key[constant] Record_Component := (A => False);]}

@ChgDeleted{Version=[1],Text=[   @key[type] Array_Component @key[is]
      @key[record]
         A : Boolean;
      @key[end record];]}

@ChgDeleted{Version=[1],Text=[   Nil_Array_Component : @key[constant] Array_Component := (A => False);]}

@ChgDeleted{Version=[1],Text=[   @key[type] Array_Component_Iterator @key[is]
      @key[record]
         Component : Array_Component;
         Index     : Asis.ASIS_Natural := 0;
      @key[end record];]}

@ChgDeleted{Version=[1],Text=[   Nil_Array_Component_Iterator : @key[constant] Array_Component_Iterator :=
      (Component => Nil_Array_Component,
       Index     => 0);]}

@ChgDeleted{Version=[1],Text=[@key[end] Asis.Data_Decomposition;]}
@end{Example}


