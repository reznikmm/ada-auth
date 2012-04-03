@Part(13, Root="ada.mss")

@Comment{$Date: 2012/04/03 20:37:02 $}

@Comment{$Source: e:\\cvsroot/ARM/Source/13b.mss,v $}
@Comment{$Revision: 1.98 $}

@RMNewPage
@LabeledClause{The Package System}

@begin{Intro}
@redundant[For each implementation there is a library package called System
which includes the definitions of certain configuration-dependent
characteristics.]
@end{Intro}

@begin{StaticSem}
@Leading@;The following language-defined library package exists:
@ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],
Text=[The contents of the visible part
of package System@Chg{Version=[2],New=[],Old=[and its language-defined children]}.]}
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00362-01]}
@RootLibUnit{System}@key[package] System @key[is]
   @key{pragma} @Chg{Version=[2],New=[Pure],Old=[Preelaborate]}(System);

   @key[type] @AdaTypeDefn{Name} @key[is] @RI{implementation-defined-enumeration-type};
   @AdaObjDefn{System_Name} : @key[constant] Name := @RI{implementation-defined};


   --@RI{ System-Dependent Named Numbers:}

   @AdaObjDefn{Min_Int}               : @key[constant] := @RI{root_integer}'First;
   @AdaObjDefn{Max_Int}               : @key[constant] := @RI{root_integer}'Last;

   @AdaObjDefn{Max_Binary_Modulus}    : @key[constant] := @RI{implementation-defined};
   @AdaObjDefn{Max_Nonbinary_Modulus} : @key[constant] := @RI{implementation-defined};

   @AdaObjDefn{Max_Base_Digits}       : @key[constant] := @RI{root_real}'Digits;
   @AdaObjDefn{Max_Digits}            : @key[constant] := @RI{implementation-defined};

   @AdaObjDefn{Max_Mantissa}          : @key[constant] := @RI{implementation-defined};
   @AdaObjDefn{Fine_Delta}            : @key[constant] := @RI{implementation-defined};

   @AdaObjDefn{Tick}                  : @key[constant] := @RI{implementation-defined};


   --@RI{ Storage-related Declarations:}

   @key[type] @AdaTypeDefn{Address} @key[is] @RI{implementation-defined};
   @AdaObjDefn{Null_Address} : @key[constant] Address;

   @AdaObjDefn{Storage_Unit} : @key[constant] := @RI{implementation-defined};
   @AdaObjDefn{Word_Size}    : @key[constant] := @RI{implementation-defined} * Storage_Unit;
   @AdaObjDefn{Memory_Size}  : @key[constant] := @RI{implementation-defined};

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
   --@RI{ @Defn2{Term=[address], Sec=(comparison)}Address Comparison:}
   @key(function) "<" (Left, Right : Address) @key(return) Boolean@Chg{Version=[3],New=[
      @key(with) Convention => Intrinsic],Old=[]};
   @key(function) "<="(Left, Right : Address) @key(return) Boolean@Chg{Version=[3],New=[
      @key(with) Convention => Intrinsic],Old=[]};
   @key(function) ">" (Left, Right : Address) @key(return) Boolean@Chg{Version=[3],New=[
      @key(with) Convention => Intrinsic],Old=[]};
   @key(function) ">="(Left, Right : Address) @key(return) Boolean@Chg{Version=[3],New=[
      @key(with) Convention => Intrinsic],Old=[]};
   @key(function) "=" (Left, Right : Address) @key(return) Boolean@Chg{Version=[3],New=[
      @key(with) Convention => Intrinsic],Old=[]};
-- @key(function) "/=" (Left, Right : Address) @key(return) Boolean;
   --@RI{ "/=" is implicitly defined}@Chg{Version=[3],New=[],Old=[
   @key[pragma] Convention(Intrinsic, "<");
   ... --@RI{ and so on for all language-defined subprograms in this package}]}


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00221-01]}
   --@RI{ Other System-Dependent Declarations:}
   @key[type] @AdaTypeDefn{Bit_Order} @key[is] (@AdaObjDefn{High_Order_First}, @AdaObjDefn{Low_Order_First});
   @AdaObjDefn{Default_Bit_Order} : @key[constant] Bit_Order@Chg{Version=[2],New=[ := @RI{implementation-defined}],Old=[]};


   --@RI{ Priority-related declarations (see @RefSecNum{Task Priorities}):}
   @key{subtype} @AdaSubtypeDefn{Name=[Any_Priority],Of=[Integer]} @key{is} Integer @key{range} @RI{implementation-defined};
   @key{subtype} @AdaSubtypeDefn{Name=[Priority],Of=[Any_Priority]} @key{is} Any_Priority @key{range} Any_Priority'First ..
             @RI{implementation-defined};
   @key{subtype} @AdaSubtypeDefn{Name=[Interrupt_Priority],Of=[Any_Priority]} @key{is} Any_Priority @key{range} Priority'Last+1 ..
             Any_Priority'Last;

   @AdaObjDefn{Default_Priority} : @key{constant} Priority :=
             (Priority'First + Priority'Last)/2;

@key[private]
   ... -- @RI{not specified by the language}
@key[end] System;
@end{Example}


Name is an enumeration subtype.
Values of type Name are the names of alternative machine
configura@!tions handled by the implementation.
System_Name represents the current machine configuration.

The named numbers Fine_Delta and Tick are of the type
@i{universal_real}; the others are of the type @i{universal_integer}.

@Leading@;The meanings of the named numbers are:
@begin{Description}
@Redundant[
Min_Int @\The smallest (most negative) value
allowed for the expressions of a
@nt{signed_@!integer_@!type_@!definition}.

Max_Int @\The largest (most positive) value
allowed for the expressions of a
@nt{signed_@!integer_@!type_@!definition}.

Max_Binary_Modulus @\A power of two such that it,
and all lesser positive powers of two, are allowed
as the modulus of a @nt<modular_type_definition>.

Max_Nonbinary_Modulus @\A value such that it,
and all lesser positive integers, are allowed
as the modulus of a @nt<modular_type_definition>.
@begin{Ramification}
There is no requirement that Max_Nonbinary_Modulus
be less than or equal to Max_Binary_Modulus,
although that's what makes most sense.
On a typical 32-bit machine, for example,
Max_Binary_Modulus will be 2**32
and Max_Nonbinary_Modulus will be 2**31,
because supporting nonbinary moduli in above 2**31
causes implementation difficulties.
@end{Ramification}

Max_Base_Digits @\The largest value allowed for the requested decimal
precision in a @nt{floating_@!point_@!definition}.

Max_Digits @\The largest value allowed for the requested decimal
precision in a @nt{floating_@!point_@!definition}
that has no @nt{real_@!range_@!specification}.
Max_Digits is less than or equal to Max_Base_Digits.

Max_Mantissa @\The largest possible number of binary digits in the mantissa
of machine numbers of a user-defined ordinary fixed point type.
(The mantissa is defined in @RefSecNum{Numerics}.)

Fine_Delta @\The smallest delta allowed in an @nt{ordinary_fixed_point_definition} that
has the @nt{real_@!range_@!specification} @key{range} @en@;1.0 .. 1.0.
]

Tick @\A period in seconds approximating the real time interval during
which the value of Calendar.Clock remains constant.
@begin{Ramification}
There is no required relationship between System.Tick and
Duration'Small,
other than the one described here.

The inaccuracy of the @nt{delay_statement} has no relation to Tick.
In particular, it is possible that the clock used for the
@nt{delay_statement} is less accurate than Calendar.Clock.

We considered making Tick a run-time-determined quantity,
to allow for easier configurability.
However, this would not be upward compatible,
and the desired configurability can be achieved using
functionality defined in @RefSec{Real-Time Systems}.
@end{Ramification}

Storage_Unit @\The number of bits per storage element.

Word_Size @\The number of bits per word.

Memory_Size @\An implementation-defined value
@Redundant[that is intended to reflect the memory size of the
configuration in storage elements.]
@begin{Discussion}
It is unspecified whether this refers to the size of the
address space, the amount of physical memory on the machine,
or perhaps some other interpretation of @lquotes@;memory size.@rquotes@;
In any case, the value has to be given by a static expression,
even though the amount of memory on many modern machines is
a dynamic quantity in several ways.
Thus, Memory_Size is not very useful.
@end{Discussion}
@end{Description}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00161-01]}
Address is @Chg{Version=[2],New=[],Old=[of ]}a definite, nonlimited
type@Chg{Version=[2],New=[ with preelaborable initialization (see
@RefSecNum{Elaboration Control})],Old=[]}.
Address represents machine addresses capable of addressing individual
storage elements.
Null_Address is an address that is distinct from the
address of any object or program unit.
@IndexSee{Term=[pointer],See=(type System.Address)}
@begin{Ramification}
The implementation has to ensure that there is at least one
address that nothing will be allocated to;
Null_Address will be one such address.
@end{Ramification}
@begin{Ramification}
Address is the type of the result of the attribute Address.
@end{Ramification}
@begin{Reason}
Address is required to be nonlimited and definite because
it is important to be able to assign addresses,
and to declare uninitialized address variables.
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00161-01]}
  @ChgAdded{Version=[2],Text=[If System.Address is defined as a private type
  (as suggested below), it might be necessary to add a pragma
  Preelaborable_Initialization to the specification of System in order that
  Address have preelaborable initialization as required.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00221-01]}
@Chg{Version=[2],New=[ Default_Bit_Order shall
be a static constant. ],Old=[]}See @RefSecNum{Bit Ordering} for an
explanation of Bit_Order and Default_Bit_Order.
@end{StaticSem}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00362-01]}
An implementation may add additional implementation-defined
declarations to package System and its children.
@Redundant[However, it is usually better
for the implementation to provide
additional functionality via implementation-defined
children of System.]@Chg{Version=[2],New=[],Old=[
Package System may be declared pure.]}
@begin{Ramification}
The declarations in package System and its children can be implicit.
For example, since Address is not limited,
the predefined "=" and "/=" operations are probably sufficient.
However, the implementation is not @i{required} to use the predefined
"=".
@end{Ramification}
@end{ImplPerm}

@begin{ImplAdvice}
@ChgNote{Version=[1],Kind=[Revised]}@ChgNote{Remove bogus "of"}
Address should be @Chg{New=[],Old=[of ]}a private type.
@begin{Reason}
This promotes uniformity by avoiding having
implementation-defined predefined operations for the type.
We don't require it, because implementations may want to stick with
what they have.
@end{Reason}
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Type System.Address should be a private type.]}]}
@begin{ImplNote}
It is not necessary for Address to be able to point at
individual bits within a storage element.
Nor is it necessary for it to be able to point at machine registers.
It is intended as a memory address that matches the hardware's notion
of an address.

The representation of the @key{null} value of a general access type should
be the same as that of Null_Address;
instantiations of Unchecked_Conversion should work accordingly.
If the implementation supports interfaces to other languages,
the representation of the @key{null} value of a general access type
should be the same as in those other languages, if appropriate.

Note that the children of the Interfaces package will generally provide
foreign-language-specific null values where appropriate.
See UI-0065 regarding Null_Address.
@end{ImplNote}
@end{ImplAdvice}

@begin{Notes}
There are also some language-defined child packages of System
defined elsewhere.
@end{Notes}

@begin{Extend83}
@ChgRef{Version=[1],Kind=[Added]}@ChgNote{Presentation AI-00114}
@ChgAdded{Version=[1],Text=[@Defn{extensions to Ada 83}
The declarations Max_Binary_Modulus, Max_Nonbinary_Modulus, Max_Base_Digits,
Null_Address, Word_Size, Bit_Order, Default_Bit_Order, Any_Priority,
Interrupt_Priority, and Default_Priority are added to System in Ada 95.
The presence of ordering operators for type Address is also guaranteed (the
existence of these depends on the definition of Address in an Ada 83
implementation). We do not list these as incompatibilities, as the contents of
System can vary between implementations anyway; thus a program that depends on
the contents of System (by using @f{@key[use] System;} for example) is
already at risk of being incompatible when moved between Ada implementations.]}
@end{Extend83}

@begin{DiffWord83}
Much of the content of System is standardized,
to provide more uniformity across implementations.
Implementations can still add their own declarations to System,
but are encouraged to do so via children of System.

Some of the named numbers are defined more explicitly in terms of the
standard numeric types.

The pragmas System_Name, Storage_Unit, and Memory_Size are no
longer defined by the language.
However, the corresponding declarations in package
System still exist.
Existing implementations may continue to support the three
pragmas as implementation-defined pragmas,
if they so desire.

Priority semantics, including subtype Priority,
have been moved to the Real Time Annex.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00161-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @b[Amendment Correction:] Type Address is defined to have preelaborable
  initialization, so that it
  can be used without restriction in preelaborated units. (If Address is
  defined to be a private type, as suggested by the @ImplAdviceTitle,
  in Ada 95 it cannot be used in some contexts in a preelaborated units.
  This is an unnecessary portability issue.)]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00221-01]}
  @ChgAdded{Version=[2],Text=[@b[Amendment Correction:] Default_Bit_Order
  is now a static constant.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00362-01]}
  @ChgAdded{Version=[2],Text=[Package System is now Pure, so it can be
  portably used in more places. (Ada 95 allowed it to be Pure, but did not
  require that.)]}
@end{Extend95}


@LabeledSubClause{The Package System.Storage_Elements}

@begin{StaticSem}
@Leading@;The following language-defined library package exists:
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00362-01]}
@ChildUnit{Parent=[System],Child=[Storage_Elements]}@key[package] System.Storage_Elements @key[is]
   @key{pragma} @Chg{Version=[2],New=[Pure(],Old=[Preelaborate(System.]}Storage_Elements);

   @key[type] @AdaTypeDefn{Storage_Offset} @key[is] @key[range] @RI(implementation-defined);

   @key[subtype] @AdaSubtypeDefn{Name=[Storage_Count],Of=[Storage_Offset]} @key[is] Storage_Offset @key[range] 0..Storage_Offset'Last;


   @key[type] @AdaTypeDefn{Storage_Element} @key[is] @key[mod] @RI{implementation-defined};
   @key[for] Storage_Element'Size @key[use] Storage_Unit;
   @key[type] @AdaTypeDefn{Storage_Array} @key[is] @key[array]
     (Storage_Offset @key[range] <>) @key[of] @key[aliased] Storage_Element;
   @key[for] Storage_Array'Component_Size @key[use] Storage_Unit;


   --@RI{ @Defn2{Term=[address], Sec=(arithmetic)}Address Arithmetic:}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
   @key(function) "+"(Left : Address; Right : Storage_Offset)@Chg{Version=[3],New=[],Old=[
     ]} @key(return) Address@Chg{Version=[3],New=[
      @key(with) Convention => Intrinsic],Old=[]};
   @key(function) "+"(Left : Storage_Offset; Right : Address)@Chg{Version=[3],New=[],Old=[
     ]} @key(return) Address@Chg{Version=[3],New=[
      @key(with) Convention => Intrinsic],Old=[]};
   @key(function) "-"(Left : Address; Right : Storage_Offset)@Chg{Version=[3],New=[],Old=[
     ]} @key(return) Address@Chg{Version=[3],New=[
      @key(with) Convention => Intrinsic],Old=[]};
   @key(function) "-"(Left, Right : Address)@Chg{Version=[3],New=[],Old=[
     ]} @key(return) Storage_Offset@Chg{Version=[3],New=[
      @key(with) Convention => Intrinsic],Old=[]};


@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
   @key(function) "@key(mod)"(Left : Address; Right : Storage_Offset)
      @key(return) Storage_Offset@Chg{Version=[3],New=[
         @key(with) Convention => Intrinsic],Old=[]};


   --@RI{ Conversion to/from integers:}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
   @key[type] @AdaTypeDefn{Integer_Address} @key[is] @RI{implementation-defined};
   @key[function] @AdaSubDefn{To_Address}(Value : Integer_Address) @key[return] Address@Chg{Version=[3],New=[
      @key(with) Convention => Intrinsic],Old=[]};
   @key[function] @AdaSubDefn{To_Integer}(Value : Address) @key[return] Integer_Address@Chg{Version=[3],New=[
      @key(with) Convention => Intrinsic],Old=[]};


@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[],Old=[   @key[pragma] Convention(Intrinsic, "+");
      @RI(-- ...and so on for all language-defined subprograms declared in this package.)
]}@key[end] System.Storage_Elements;
@end{Example}
@begin{Reason}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
The Convention @Chg{Version=[3],New=[aspects],Old=[@nt{pragma}s]} imply that
the attribute Access is not allowed for those operations.

The @key(mod) function is needed so that the
definition of Alignment makes sense.
@end{Reason}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The range of Storage_Elements.Storage_Offset, the modulus of
Storage_Elements.Storage_Element, and the declaration of
Storage_Elements.Integer_Address.]}.]}

Storage_Element represents a storage element.
Storage_Offset represents an offset in storage elements.
Storage_Count represents a number of storage elements.
@PDefn{contiguous representation}
@PDefn{discontiguous representation}
Storage_Array represents a contiguous sequence of storage elements.
@begin{Reason}
The index subtype of Storage_Array is Storage_Offset
because we wish to allow maximum flexibility.
Most Storage_Arrays will probably have a lower bound of 0 or 1,
but other lower bounds, including negative ones,
make sense in some situations.

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00114-01]}
@ChgDeleted{Version=[2],Text=[Note that there are some language-defined
subprograms that fill part of a Storage_Array, and return the index of the
last element filled as a Storage_Offset.
The Read procedures in
Streams (see @RefSecNum{The Package Streams}),
Streams.Stream_IO (see @RefSecNum{The Package Streams.Stream_IO}),
and System.RPC (see @RefSecNum{Partition Communication Subsystem})
behave in this manner.
These will raise Constraint_Error if the resulting Last value is not in
Storage_Offset.
This implies that the Storage_Array passed to these subprograms should
not have a lower bound of Storage_Offset'First,
because then a read of 0 elements would always raise Constraint_Error.
A better choice of lower bound is 1.]}
@end{Reason}

Integer_Address is a @Redundant[(signed or modular)] integer subtype.
To_Address and To_Integer convert back and forth between
this type and Address.
@end{StaticSem}

@begin{ImplReq}
Storage_Offset'Last shall be greater than or equal to Integer'Last or
the largest possible storage offset,
whichever is smaller.
Storage_Offset'First shall be <= (@en@;Storage_Offset'Last).
@end{ImplReq}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00362-01]}
@ChgDeleted{Version=[2],Text=[Package System.Storage_Elements may be declared
pure.]}
@end{ImplPerm}
@begin{NotIso}
@ChgAdded{Version=[2],Noparanum=[T],Text=[@Shrink{@i<Paragraph 15 was
deleted.>}]}@Comment{This message should be deleted if the paragraphs
are ever renumbered.}
@end{NotIso}

@begin{ImplAdvice}
Operations in System and its children should reflect the
target environment semantics as closely as is reasonable.
For example, on most machines, it makes sense for address arithmetic
to @lquotes@;wrap around.@rquotes@;
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
Operations that do not make sense should raise
Program_Error.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Operations in System and its children should reflect the
target environment; operations that do not make sense should raise Program_Error.]}]}
@begin{Discussion}
For example, on a segmented architecture,
X < Y might raise Program_Error if X and Y do not point at the same
segment (assuming segments are unordered).
Similarly, on a segmented architecture,
the conversions between Integer_Address and Address
might not make sense for some values,
and so might raise Program_Error.
@end{Discussion}
@begin{Reason}
We considered making Storage_Element a private type.
However, it is better to declare it as a modular type in the visible part,
since code that uses it is already low level,
and might as well have access to the underlying representation.
We also considered allowing Storage_Element to be any
integer type, signed integer or modular, but it is better to have
uniformity across implementations in this regard, and viewing storage elements
as unsigned seemed to make the most sense.
@end{Reason}
@begin{ImplNote}
To_Address is intended for use in Address clauses.
Implementations should overload To_Address if appropriate.
For example, on a segmented architecture,
it might make sense to have a record type representing a
segment/offset pair, and have a To_Address conversion that
converts from that record type to type Address.
@end{ImplNote}
@end{ImplAdvice}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00362-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Package System.Storage_Elements is now Pure, so it can be
  portably used in more places. (Ada 95 allowed it to be Pure, but did not
  require that.)]}
@end{Extend95}



@ISOOnlyRMNewPageVer{Version=[3]}@Comment{For ISO version of Ada 2012 Standard}
@LabeledSubClause{The Package System.Address_To_Access_Conversions}

@begin{StaticSem}

@Leading@;The following language-defined generic library package exists:
@begin{Example}
@ChildUnit{Parent=[System],Child=[Address_@!To_@!Access_@!Conversions]}@key[generic]
    @key[type] Object(<>) @key[is] @key[limited] @key[private];
@key[package] System.Address_To_Access_Conversions @key[is]
   @key[pragma] Preelaborate(Address_To_Access_Conversions);

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
   @key[type] Object_Pointer @key[is] @key[access] @key[all] Object;
   @key[function] @AdaSubDefn{To_Pointer}(Value : Address) @key[return] Object_Pointer@Chg{Version=[3],New=[
      @key(with) Convention => Intrinsic],Old=[]};
   @key[function] @AdaSubDefn{To_Address}(Value : Object_Pointer) @key[return] Address@Chg{Version=[3],New=[
      @key(with) Convention => Intrinsic],Old=[]};

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[],Old=[   @key[pragma] Convention(Intrinsic, To_Pointer);
   @key[pragma] Convention(Intrinsic, To_Address);
]}@key[end] System.Address_To_Access_Conversions;
@end{Example}



@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
The To_Pointer and To_Address subprograms
convert back and forth between
values of types Object_Pointer and Address.
To_Pointer(X'Address) is equal to X'Unchecked_Access
for any X that allows Unchecked_Access.
To_Pointer(Null_Address) returns @key[null].
@PDefn{unspecified}
For other addresses,
the behavior is unspecified.
To_Address(@key[null]) returns Null_Address@Chg{Version=[2],New=[],Old=[ (for
@key[null] of the appropriate type)]}.
To_Address(Y), where Y /= @key[null], returns Y.@key[all]'Address.
@begin{Discussion}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
The programmer should ensure that
the address passed to To_Pointer is either Null_Address,
or the address of an object of type Object. @Chg{Version=[2],New=[(If Object is
not a @Chg{Version=[3],New=[],Old=[not ]}by-reference type, the object ought to
be aliased; recall that the Address attribute is not required to provide a
useful result @Chg{Version=[3],New=[for ],Old=[]}other objects.)],Old=[]}
Otherwise, the behavior of the program is unspecified;
it might raise an exception or crash, for example.
@end{Discussion}
@begin{Reason}
Unspecified is almost the same thing as erroneous;
they both allow arbitrarily bad behavior.
We don't say erroneous here, because the implementation
might allow the address passed to To_Pointer to point
at some memory that just happens to @lquotes@;look like@rquotes@; an
object of type Object.
That's not necessarily an error; it's just not portable.
However, if the actual type passed to Object is (for example)
an array type, the programmer
would need to be aware of any dope that the implementation expects to
exist, when passing an address that did not come from the Address
attribute of an object of type Object.

One might wonder why To_Pointer and To_Address are any better than
unchecked conversions. The answer is that Address does not necessarily
have the same representation as an access type. For example, an access
value might point at the bounds of an array when an address would point
at the first element. Or an access value might be an offset in words
from someplace, whereas an address might be an offset in bytes from the
beginning of memory.
@end{Reason}
@end{StaticSem}

@begin{ImplPerm}
An implementation may place restrictions on instantiations of
Address_To_Access_Conversions.
@begin{Ramification}
For example, if the hardware requires aligned loads and stores,
then dereferencing an access value that is not properly aligned might
raise an exception.

For another example,
if the implementation has chosen to use negative component offsets
(from an access value),
it might not be possible to preserve the semantics,
since negative offsets from the Address are not allowed.
(The Address attribute always points at
@lquotes@;the first of the storage elements....@rquotes@;)
Note that while the implementation knows how to convert an access
value into an address, it might not be able to do the reverse.
To avoid generic contract model violations,
the restriction might have to be detected at run time in some cases.
@end{Ramification}
@end{ImplPerm}

@LabeledClause{Machine Code Insertions}

@begin{Intro}
@redundant[@Defn{machine code insertion}
A machine code insertion can be achieved by a call to a subprogram whose
@nt{sequence_of_statements} contains @nt{code_statement}s.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<code_statement>,rhs="@Syn2{qualified_expression};"}

@begin{SyntaxText}
A @nt{code_statement} is only allowed in the
@nt{handled_sequence_of_@!statements} of a @nt{subprogram_@!body}.
If a @nt{subprogram_@!body} contains any @nt{code_@!statement}s, then within
this @nt{subprogram_@!body} the only allowed form of @nt{statement} is a
@nt{code_@!statement} (labeled or not),
the only allowed @nt{declarative_@!item}s are @nt{use_@!clause}s,
and no @nt{exception_@!handler} is allowed (@nt{comment}s and
@nt{pragma}s are allowed as usual).
@end{SyntaxText}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type],
  Sec=(code_statement)}
The @nt{qualified_expression} is expected to be of any type.
@end{Resolution}

@begin{Legality}
The @nt{qualified_expression} shall be of a
type declared in package System.Machine_Code.
@begin{Ramification}
This includes types declared in children of System.Machine_Code.
@end{Ramification}

A @nt<code_statement> shall appear only within the scope of
a @nt<with_clause> that mentions package System.Machine_Code.
@begin{Ramification}
Note that this is not a note;
without this rule, it would be possible to write machine code
in compilation units which depend on System.Machine_Code only indirectly.
@end{Ramification}
@end{Legality}

@begin{StaticSem}
@Defn{System.Machine_Code}
@ChildUnit{Parent=[System],Child=[Machine_Code]}
The contents of the library package System.Machine_Code
(if provided) are implementation defined.
The meaning of @nt{code_statement}s is implementation defined.
@Redundant{Typically, each @nt{qualified_expression}
represents a machine instruction or assembly directive.}
@begin{Discussion}
For example, an instruction might be a record with an Op_Code
component and other components for the operands.
@end{Discussion}
@ImplDef{The contents of the visible part of package System.Machine_Code,
and the meaning of @nt{code_statement}s.}
@end{StaticSem}

@begin{ImplPerm}
An implementation may place restrictions on @nt{code_statement}s.
An implementation is not required to provide package System.Machine_Code.
@end{ImplPerm}

@begin{Notes}
An implementation may provide implementation-defined pragmas
specifying register conventions and calling conventions.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
Machine code functions are exempt from the rule that a
@Chg{Version=[2],New=[return statement],Old=[@nt{return_@!statement}]}
is required. In fact,
@Chg{Version=[2],New=[return statements],Old=[@nt{return_@!statement}s]} are
forbidden, since only @nt{code_statement}s are allowed.
@begin{Discussion}
The idea is that the author of a machine code subprogram knows
the calling conventions, and refers to parameters and results
accordingly.
The implementation should document where to put the result of a
machine code function, for example,
@lquotes@;Scalar results are returned in register 0.@rquotes@;
@end{Discussion}

Intrinsic subprograms (see @RefSec{Conformance Rules})
can also be used to achieve machine code insertions.
Interface to assembly language can be achieved
using the features in @RefSec{Interface to Other Languages}.
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of a code statement:}
@begin{Example}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
M : Mask;
@key[procedure] Set_Mask@Chg{Version=[3],New=[
  @key[with] Inline],Old=[]};@Chg{Version=[3],New=[],Old=[ @key[pragma] Inline(Set_Mask);]}

@key[procedure] Set_Mask @key[is]
  @key[use] System.Machine_Code; --@RI{ assume @lquotes@;@key[with] System.Machine_Code;@rquotes@; appears somewhere above}
@key[begin]
  SI_Format'(Code => SSM, B => M'Base_Reg, D => M'Disp);
  --@RI{  Base_Reg and Disp are implementation-defined attributes}
@key[end] Set_Mask;
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
Machine code functions are allowed in Ada 95;
in Ada 83, only procedures were allowed.
@end{Extend83}

@begin{DiffWord83}
The syntax for @nt{code_statement} is changed to say
@lquotes@;@nt{qualified_expression}@rquotes@; instead of
@lquotes@;@Syn2{subtype_mark}'@Syn2{record_aggregate}@rquotes@;.
Requiring the type of each instruction to be a record type is
overspecification.
@end{DiffWord83}

@LabeledClause{Unchecked Type Conversions}

@begin{Intro}
@redundant[@Defn{unchecked type conversion}
@Defn2{Term=[type conversion], Sec=(unchecked)}
@Defn2{Term=[conversion], Sec=(unchecked)}
@IndexSeeAlso{Term=[type_conversion],See=(unchecked type conversion)}
@IndexSee{Term=[cast],See=(unchecked type conversion)}
An unchecked type conversion can be achieved by a call to an instance
of the generic function Unchecked_Conversion.]
@end{Intro}

@begin{StaticSem}
@Leading@keepnext@;The following language-defined generic library function exists:
@begin{Example}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@key[generic]
   @key[type] Source(<>) @key[is] @key[limited] @key[private];
   @key[type] Target(<>) @key[is] @key[limited] @key[private];
@SubChildUnit{Parent=[Ada],Child=[Unchecked_Conversion]}@key[function] Ada.Unchecked_Conversion(S : Source) @key[return] Target@Chg{Version=[3],New=[
   @key(with) Convention => Intrinsic],Old=[]};@Chg{Version=[3],New=[],Old=[
@key[pragma] Convention(Intrinsic, Ada.Unchecked_Conversion);]}
@key[pragma] Pure(Ada.Unchecked_Conversion);
@end{Example}
@begin{Reason}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
The @Chg{Version=[3],New=[aspect],Old=[@nt{pragma}]} Convention implies that
the attribute Access is not allowed
for instances of Unchecked_Conversion.
@end{Reason}
@end{StaticSem}

@begin{RunTime}
The size of the formal parameter S in an instance of
Unchecked_Conversion is that of its subtype.
@Redundant[This is the actual subtype passed to Source,
except when the actual is an unconstrained composite subtype,
in which case the subtype is constrained by the bounds or
discriminants of the value of the actual expression passed to S.]

@Leading@;If all of the following are true,
the effect of an unchecked conversion is to return the
value of an object of the target subtype whose representation is the
same as that of the source object S:
@begin{Itemize}
S'Size = Target'Size.
@begin{Ramification}
Note that there is no requirement that the Sizes be known at compile
time.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0078-1]}
S'Alignment @Chg{Version=[3],New=[is a multiple of],Old=[=]}
Target'Alignment@Chg{Version=[3],New=[ or Target'Alignment is zero],Old=[]}.

The target subtype is not an unconstrained composite subtype.

@PDefn{contiguous representation}
@PDefn{discontiguous representation}
S and the target subtype both have a contiguous
representation.

The representation of S is a representation of
an object of the target subtype.
@end{Itemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00426-01]}
Otherwise, @Chg{Version=[2],New=[if the result type is scalar, the result
of the function is implementation defined, and can have an invalid
representation (see @RefSecNum{Data Validity}).
If the result type is nonscalar, ],Old=[]}the effect is implementation defined;
in particular, the result can be abnormal
(see @RefSecNum{Data Validity}).

@ChgImplDef{Version=[2],Kind=[Added],Text=[@Chg{Version=[2],New=[The result
of unchecked conversion for instances with scalar result types whose
result is not defined by the language.],Old=[]}]}
@ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],
Text=[The effect of unchecked
conversion@Chg{Version=[2],New=[ for instances
with nonscalar result types whose effect is not defined by
the language],Old=[]}.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00426-01]}
  @ChgAdded{Version=[2],Text=[Note the difference between these sentences;
  the first only says that the bits returned are implementation defined, while
  the latter allows any effect. The difference is because scalar objects should
  never be abnormal unless their assignment was disrupted or if they are a
  subcomponent of an abnormal composite object. Neither exception applies to
  instances of Unchecked_Conversion.]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00426-01]}
  Whenever unchecked conversions are used, it is the programmer's
  responsibility to ensure that these conversions maintain the properties
  that are guaranteed by the language for objects of the target type.
  @Chg{Version=[2],New=[For nonscalar types, this],Old=[This]} requires the
  user to understand the underlying run-time model of  the implementation.
  The execution of a program that violates these properties by means of
  unchecked conversions @Chg{Version=[2],New=[returning a nonscalar type ],
  Old=[]}is erroneous.@Chg{Version=[2],New=[ Properties of scalar types can
  be checked by using the Valid attribute (see @RefSecNum{The Valid Attribute});
  programs can avoid violating properties of the type (and erroneous
  execution) by careful use of this attribute.],Old=[]}

  An instance of Unchecked_Conversion can be applied to an object of a
  private type, assuming the implementation allows it.
@end{Ramification}
@end{RunTime}

@begin{ImplPerm}
An implementation may return the result of
an unchecked conversion
by reference, if the Source type is not a by-copy type.
@Redundant[In this case, the result of the unchecked conversion represents
simply a different (read-only) view of the operand of the conversion.]
@begin{Ramification}
  In other words, the result object of a call on an instance
  of Unchecked_Conversion can occupy the same storage as the
  formal parameter S.
@end{Ramification}

An implementation may place restrictions on Unchecked_Conversion.
@begin{Ramification}
For example, an instantiation of Unchecked_Conversion for types for
which unchecked conversion doesn't make sense may be disallowed.
@end{Ramification}
@end{ImplPerm}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00051-02]}
@Chg{Version=[2],New=[Since the],Old=[The]} Size of an array object
@Chg{Version=[2],New=[generally does],Old=[should]} not include its
bounds@Chg{Version=[2],New=[],Old=[; hence]}, the bounds should not be part
of the converted data.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Since the Size of an array object generally does not include its bounds,
the bounds should not be part of the converted data in an instance of
Unchecked_Conversion.]}]}
@begin{Ramification}
On the other hand, we have no advice to offer about
discriminants and tag fields.
@end{Ramification}

The implementation should not generate unnecessary run-time checks
to ensure that the representation of S is a representation of the target
type.
It should take advantage of the permission to return by reference
when possible.
Restrictions on unchecked conversions should be avoided
unless required by the target environment.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[There should not be unnecessary run-time checks on the result of
an Unchecked_Conversion; the result should be returned by reference when
possible. Restrictions on Unchecked_Conversions should be avoided.]}]}
@begin{ImplNote}
  As an example of an unnecessary run-time check,
  consider a record type with gaps between components.
  The compiler might assume that such gaps are always zero bits.
  If a value is produced that does not obey that assumption,
  then the program might misbehave.
  The implementation should not generate extra code to check for zero bits
  (except, perhaps, in a special error-checking mode).
@end{ImplNote}

@Leading@PDefn2{Term=[recommended level of support], Sec=(unchecked conversion)}
The recommended level of support for unchecked conversions is:
@begin{Itemize}
Unchecked conversions should be supported and should be reversible in
the cases where this clause defines the result.
@PDefn{contiguous representation}
@PDefn{discontiguous representation}
To enable meaningful use of unchecked conversion,
a contiguous representation should be used for elementary subtypes,
for statically constrained array subtypes whose component subtype is
one of the subtypes described in this paragraph,
and for record subtypes without discriminants
whose component subtypes are described in this paragraph.
@end{Itemize}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The recommended level of support for Unchecked_Conversion should be
followed.]}]}
@end{ImplAdvice}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00051-02]}
  @ChgAdded{Version=[2],Text=[The implementation advice about the size of
  array objects was moved to 13.3 so that all of the advice about Size is
  in one place.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00426-01]}
  @ChgAdded{Version=[2],Text=[Clarified that the result of Unchecked_Conversion
  for scalar types can be invalid, but not abnormal.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0078-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Relaxed the alignment
  requirement slightly, giving a defined result in more cases.]}
  @ChgNote{This is not an extension, as a program taking advantage of this
  change was (and very well might have worked) legal before the change; it's
  just guaranteed to be portable now.}
@end{Diffword2005}


@LabeledSubClause{Data Validity}

@begin{Intro}
Certain actions that can potentially lead to erroneous execution are not
directly erroneous,
but instead can cause objects to become @i{abnormal}.
Subsequent uses of abnormal objects can be erroneous.

A scalar object can have an @i{invalid representation},
which means that the object's representation does not represent any
value of the object's subtype.
@RootDefn{uninitialized variables}
The primary cause of invalid representations is uninitialized variables.

Abnormal objects and invalid representations are explained in this
subclause.
@end{Intro}

@begin{RunTime}
@Leading@RootDefn{normal state of an object}
@RootDefn{abnormal state of an object}
When an object is first created, and any explicit or default
initializations have been performed, the object
and all of its parts are in the @i{normal} state.
Subsequent operations generally leave them normal.
However, an object or part of an object can become @i{abnormal}
in the following ways:
@begin{Itemize}
@Defn{disruption of an assignment}
An assignment to the object is disrupted due to an abort
(see @RefSecNum{Abort of a Task - Abort of a Sequence of Statements})
or due to the failure of a language-defined check
(see @RefSecNum{Exceptions and Optimization}).

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00426-01]}
The object is not scalar, and is passed to an @key[in out]
or @key[out] parameter of an imported procedure@Chg{Version=[2],New=[,
the Read procedure of an instance of Sequential_IO,
Direct_IO, or Storage_IO, or the stream attribute T'Read], Old=[ or
language-defined input procedure]},
if after return from the procedure the representation of the parameter
does not represent a value of the parameter's subtype.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00426-01]}
@ChgAdded{Version=[2],Text=[The object is the return object of a function call
of a nonscalar type, and the function is an imported function, an instance of
Unchecked_Conversion, or the stream attribute T'Input, if after return from the
function the representation of the return object does not represent a value of
the function's subtype.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We explicitly list the routines involved in order
  to avoid future arguments. All possibilities are listed.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We did not include Stream_IO.Read in the list
  above. A Stream_Element should include all possible bit patterns, and thus it
  cannot be invalid. Therefore, the parameter will always represent a value of
  its subtype. By omitting this routine, we make it possible to write arbitrary
  I/O operations without any possibility of abnormal objects.]}
@end{Discussion}

@end{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00426-01]}
@ChgAdded{Version=[2],Text=[@Redundant[For an imported object, it is the
programmer's responsibility to ensure that the object remains in a normal
state.]]}
@begin{TheProof}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This follows (and echos) the standard rule
  of interfacing; the programmer must ensure that Ada semantics are
  followed (see @RefSecNum{Interfacing Aspects}).]}
@end{TheProof}

@PDefn{unspecified}
Whether or not an object actually becomes abnormal in these cases is
not specified.
An abnormal object becomes normal again upon successful completion of
an assignment to the object as a whole.
@end{RunTime}

@begin{Erron}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
It is erroneous to evaluate a @nt<primary> that is a @nt<name>
denoting an abnormal object,
or to evaluate a @nt{prefix} that denotes an abnormal object.
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00114-01]}
@Comment{There appears to be no justification for this statement; everything
can become abnormal. We leave the prefix for the next paragraph, so we use
Chg rather than ChgDeleted.}
@Chg{Version=[2],New=[],Old=[Although a composite object with no
subcomponents of an access type, and with static constraints all the way down
cannot become abnormal, a scalar subcomponent of such an object can become
abnormal.]}

The @key[in out] or @key[out] parameter case does not apply to scalars;
bad scalars are merely invalid representations,
rather than abnormal, in this case.
@end{Ramification}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
The reason we allow access objects, and objects containing subcomponents of an
access type, to become abnormal is because the correctness of an access
value cannot necessarily be determined merely by looking at the bits of
the object.
The reason we allow scalar objects to become abnormal is that we wish to
allow the compiler to optimize assuming that the value of a scalar
object belongs to the object's subtype,
if the compiler can prove that the object is initialized with a value
that belongs to the subtype.
The reason we allow composite objects to become abnormal
@Chg{Version=[2],New=[],Old=[if some constraints are nonstatic ]}is
that such object might be represented with implicit levels of indirection;
if those are corrupted, then even assigning into a component of the
object, or simply asking for its Address, might have an unpredictable
effect. The same is true if the discriminants have been destroyed.
@end{Reason}
@end{Erron}

@begin{Bounded}
@Leading@Defn{invalid representation}@PDefn2{Term=(bounded error),Sec=(cause)}
If the representation of a scalar object does not represent a value of
the object's subtype
(perhaps because the object was not initialized),
the object is said to have an @i{invalid representation}.
It is a bounded error to evaluate the value of such
an object.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
If the error is detected, either Constraint_Error or Program_Error is
raised.
Otherwise, execution continues using the invalid representation.
The rules of the language outside this subclause assume that all objects
have valid representations.
The semantics of operations on invalid representations are as follows:

@begin{Discussion}

The AARM is more explicit about what happens when
the value of the case expression is an invalid representation.

@end{Discussion}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00426-01]}
  @ChgAdded{Version=[2],Text=[This includes the result object of functions,
  including the result of Unchecked_Conversion, T'Input, and imported
  functions.]}
@end{Ramification}

@begin{Itemize}
If the representation of the object represents a value of the object's
type, the value of the type is used.

If the representation of the object does not represent a value of the
object's type,
the semantics of operations on such representations is
implementation-defined, but does not by itself lead to
erroneous or unpredictable
execution, or to other objects becoming abnormal.
@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00426-01]}
  @ChgAdded{Version=[2],Text=[This means that the implementation must take care
  not to use an invalid representation in a way that might cause erroneous
  execution. For instance, the exception mandated for @nt{case_statement}s
  must be raised. Array indexing must not cause memory outside of
  the array to be written (and usually, not read either). These cases and
  similar cases may require explicit checks by the implementation.]}
@end{ImplNote}

@end{Itemize}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00167-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0279-1]}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
A call to an imported function or an instance of Unchecked_Conversion is
erroneous if the result is scalar, @Chg{Version=[2],New=[],Old=[and ]}the
result object has an invalid representation@Chg{Version=[2],New=[, and
the result is used other than as the @nt{expression} of
an @nt{assignment_statement} or an @nt{object_declaration},@Chg{Version=[3],New=[
as the @SynI{object_}@nt{name} of an @nt{object_renaming_declaration},],Old=[]} or as the
@nt{prefix} of a Valid attribute. If such a result object is used as the source
of an assignment, and the assigned value is an invalid representation for the
target of the assignment, then any use of the target object prior to a further
assignment to the target object, other than as the @nt{prefix} of a Valid
attribute reference, is erroneous],Old=[]}.
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00167-01]}
In a typical implementation, every bit pattern that fits in an object of
@Chg{Version=[2],New=[a signed],Old=[an]} integer subtype will represent a
value of the type, if not of the subtype.
However, for an enumeration or floating point type,@Chg{Version=[2],New=[ as
well as some modular types,],Old=[]}
there are typically bit patterns that do not represent any
value of the type.
In such cases, the implementation ought to define the semantics of
operations on the invalid representations in the obvious manner
(assuming the bounded error is not detected):
a given representation should be equal to itself,
a representation that is in between the internal codes of
two enumeration literals should behave accordingly when passed to
comparison operators and membership tests, etc.
We considered @i{requiring} such sensible behavior,
but it resulted in too much arcane verbiage,
and since implementations have little incentive to behave
irrationally, such verbiage is not important to have.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00167-01]}
If a stand-alone scalar object is initialized to a an in-range
value, then the implementation can take advantage of the fact
that @Chg{Version=[2],New=[the use of ],Old=[]}any out-of-range value
has to be @Chg{Version=[2],New=[erroneous],Old=[abnormal]}.@ChgNote{Do you
see "abnormal" in the rules above? Thought not.}
Such an out-of-range value can be produced only by things like
unchecked conversion, @Chg{Version=[2],New=[imported functions],Old=[input]},
and @Chg{Version=[2],New=[abnormal values caused by ],Old=[]}disruption
of an assignment due to abort or to failure of a
language-defined check.
This depends on out-of-range values being checked before
assignment (that is, checks are not optimized away unless they
are proven redundant).

@Leading@;Consider the following example:
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00167-01]}
@key[type] My_Int @key[is] @key[range] 0..99;
@key[function] Safe_Convert @key[is] @key[new] Unchecked_Conversion(My_Int, Integer);
@key[function] Unsafe_Convert @key[is] @key[new] Unchecked_Conversion(My_Int, Positive);
X : Positive := Safe_Convert(0); --@RI{ Raises Constraint_Error.}
Y : Positive := Unsafe_Convert(0); --@Chg{Version=[2],New=[@RI{ Bounded Error, may be invalid.}
B : Boolean  := Y'Valid; --@RI{ OK, B = False.}
Z : Positive := Y+1; --@RI{ Erroneous to use Y.}],Old=[@RI{ Erroneous.}]}

@end{Example}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00167-01],ARef=[AI95-00426-01]}
  The call to Unsafe_Convert @Chg{Version=[2],New=[ is a bounded error, which
  might raise Constraint_Error, Program_Error, or return an invalid value.
  Moreover, if an exception is not raised, most uses of that invalid value
  (including the use of Y) cause],Old=[causes]} erroneous execution. The call to
  Safe_Convert is not erroneous. The result object is an object of subtype
  Integer containing the value 0. The assignment to X is required to do a
  constraint check; the fact that the conversion is unchecked does not obviate
  the need for subsequent checks required by the language rules.

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00167-01],ARef=[AI95-00426-01]}
  @ChgAdded{Version=[2],Text=[The reason for delaying erroneous execution until
  the object is used is so that the invalid representation can be tested for
  validity using the Valid attribute (see @RefSecNum{The Valid Attribute})
  without causing execution to become erroneous. Note that this delay does not
  imply an exception will not be raised; an implementation could treat both
  conversions in the example in the same way and raise Constraint_Error.]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0279-1]}
  @ChgAdded{Version=[3],Text=[The rules are defined in terms of the result
  object, and thus the name used to reference that object is irrelevant.
  That is why we don't need any special rules to describe what happens
  when the function result is renamed.]}

@end{Ramification}
@begin{ImplNote}
  If an implementation wants to have a @lquotes@;friendly@rquotes@; mode, it
  might always assign an uninitialized scalar a default initial value
  that is outside the object's subtype (if there is one), and check
  for this value on some or all reads of the object, so as to help
  detect references to uninitialized scalars.
  Alternatively, an implementation might want to provide an
  @lquotes@;unsafe@rquotes@; mode where it presumed even uninitialized scalars
  were always within their subtype.
@end{ImplNote}
@begin{Ramification}
  The above rules imply that it is a bounded error
  to apply a predefined operator to
  an object with a scalar subcomponent having an invalid representation,
  since this implies reading the value of each subcomponent.
  Either Program_Error or Constraint_Error is raised,
  or some result is produced, which if composite, might have a corresponding
  scalar subcomponent still with an invalid representation.

  Note that it is not an error to assign, convert, or pass as a parameter
  a composite object with an uninitialized scalar subcomponent.
  In the other hand, it is a (bounded) error to apply
  a predefined operator such as =, <, and @key(xor)
  to a composite operand with an invalid scalar subcomponent.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0054-2]}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
The dereference of an access value is erroneous if
it does not designate an object of an appropriate type or a subprogram
with an appropriate profile,
if it designates a nonexistent object,
or if it is an access-to-variable value that designates a
constant object@Chg{Version=[3],New=[ and it did not originate from an
attribute_reference applied to an aliased variable view
of a controlled or immutably limited object],Old=[]}.
@Redundant[@Chg{Version=[3],New=[An],Old=[Such an]} access value
@Chg{Version=[3],New=[whose dereference is erroneous ],Old=[]}can exist,
for example, because of
Unchecked_Deallocation, Unchecked_Access, or Unchecked_Conversion.]

@begin{Ramification}
The above mentioned Unchecked_... features are not the only causes
of such access values.
For example, interfacing to other languages can also cause the problem.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0054-2]}
@Chg{Version=[3],New=[We permit the use of access-to-variable
values that designate constant objects so long as they originate
from an aliased variable view of a controlled or immutably limited
constant, such as during the initialization of a constant (both via
the @ldquote@;current instance@rdquote and during a call to Initialize) or
during an assignment (during a call to Adjust).],Old=[One obscure example is if
the Adjust subprogram of a controlled type uses Unchecked_Access to create an
access-to-variable value designating a subcomponent of its controlled parameter,
and saves this access value in a global object. When Adjust is called during the
initialization of a constant object of the type, the end result will be an
access-to-variable value that designates a constant object.]}
@end{Ramification}
@end{Erron}

@begin{Notes}
Objects can become abnormal due to other kinds of actions that directly
update the object's representation;
such actions are generally considered directly erroneous, however.
@end{Notes}

@begin{DiffWord83}
In order to reduce the amount of erroneousness,
we separate the concept of an undefined value into
objects with invalid representation (scalars only)
and abnormal objects.

Reading an object with an invalid representation is a bounded error
rather than erroneous;
reading an abnormal object is still erroneous.
In fact, the only safe thing to do to an abnormal object is
to assign to the object as a whole.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00167-01]}
  @ChgAdded{Version=[2],Text=[The description of erroneous execution for
  Unchecked_Conversion and imported objects was tightened up so that
  using the Valid attribute to test such a value is not erroneous.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00426-01]}
  @ChgAdded{Version=[2],Text=[Clarified the definition of objects that can
  become abnormal; made sure that all of the possibilities are included.]}
@end{DiffWord95}

@begin{Diffword2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0054-2]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Common programming
  techniques such as squirreling away@Defn{squirrel away} an access to a
  controlled object during initialization and using a self-referencing
  discriminant (the so-called @ldquote@;Rosen trick@rdquote@Defn{Rosen trick})
  no longer are immediately erroneous if the object is declared constant,
  so these techniques can be used portably and safely. Practically,
  these techniques already worked as compilers did not take much advantage
  of this rule, so the impact of this change will be slight.]}
  @ChgNote{This is not an extension, as such a program was already legal
  (although erroneous).}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0279-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> The description of erroneous
  execution for Unchecked_Conversion and imported objects was adjusted to
  clarify that renaming such an object is not, by itself, erroneous.]}
@end{Diffword2005}



@LabeledSubClause{The Valid Attribute}

@begin{Intro}
The Valid attribute can be used to check the validity of data produced
by unchecked conversion, input, interface to foreign languages,
and the like.
@end{Intro}

@begin{StaticSem}
@Leading@;For @PrefixType{a
@nt<prefix> X that denotes a scalar object
@Redundant[(after any implicit dereference)]},
the following attribute is defined:
@begin(description)
@ChgAttribute{Version=[3],Kind=[Revised],ChginAnnex=[T],
  Leading=<F>, Prefix=<X>, AttrName=<Valid>,
  ARef=[AI05-0153-3],
  Text=<Yields True if and only if
the object denoted by X is normal@Chg{Version=[3],New=[,],Old=[ and]} has a
valid representation@Chg{Version=[3],New=[, and the predicate@Defn2{Term=[predicate evaluated],Sec=[Valid attribute]}
of the nominal subtype of X evaluates to True],Old=[]}.
The value of this attribute is of the predefined type Boolean.>}
@begin{Ramification}
  Having checked that X'Valid is True, it is safe to read the
  value of X without fear of erroneous execution
  caused by abnormality,
  or a bounded error caused by an invalid representation.
  Such a read will produce a value in the subtype of X.
@end{Ramification}
@end{description}
@EndPrefixType{}
@end{StaticSem}

@begin{Notes}
@Leading@;Invalid data can be created in the following cases
(not counting erroneous or unpredictable execution):
@begin{Itemize}
an uninitialized scalar object,

the result of an unchecked conversion,

input,

interface to another language (including machine code),

aborting an assignment,

disrupting an assignment due to the failure
of a language-defined check
(see @RefSecNum{Exceptions and Optimization}), and

use of an object whose Address has been specified.
@end{Itemize}

X'Valid is not considered to be a read of X;
hence, it is not an error to check the validity
of invalid data.

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00426-01]}
@ChgAdded{Version=[2],Text=[The Valid attribute may be used to check the
result of calling an instance of Unchecked_Conversion (or any other
operation that can return invalid values). However, an exception handler
should also be provided because implementations are permitted to raise
Constraint_Error or Program_Error if they detect the use of an invalid
representation (see @RefSecNum{Data Validity}).]}

@begin{Ramification}
If X is of an enumeration type with a representation clause, then
X'Valid checks that the value of X when viewed as an integer is one of
the specified internal codes.
@end{Ramification}
@begin{Reason}
Valid is defined only for scalar objects because the implementation
and description burden would be too high for other types.
For example, given a typical run-time model, it is impossible to check
the validity of an access value.
The same applies to composite types implemented with internal pointers.
One can check the validity of a composite object by checking the
validity of each of its scalar subcomponents.
The user should ensure that any composite types that need to be checked
for validity are represented in a way that does not involve
implementation-defined components, or gaps between components.
Furthermore, such types should not contain access subcomponents.

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00114-01]}
@ChgNote{This was never true, even in Ada 95}
@ChgDeleted{Version=[2],Text=[Note that one can safely check the validity
of a composite object with
an abnormal value only if the constraints on
the object and all of its subcomponents are static.
Otherwise, evaluation of the @nt{prefix} of the @nt{attribute_reference}
causes erroneous execution (see @RefSecNum{Names}).]}
@end{Reason}
@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
X'Valid is new in Ada 95.
@end{Extend83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00426-01]}
  @ChgAdded{Version=[2],Text=[Added a note explaining that handlers for
  Constraint_Error and Program_Error are needed in the general case of
  testing for validity. (An implementation could document cases where these
  are not necessary, but there is no language requirement.)]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3]}
  @ChgAdded{Version=[3],Text=[The validity check now also includes a check
  of the predicate aspects (see @RefSecNum{Subtype Predicates}), if any, of
  the subtype of the object.]}
@end{DiffWord2005}



@LabeledClause{Unchecked Access Value Creation}

@begin{Intro}
@redundant[The attribute Unchecked_Access is used to create access values
in an unsafe manner @em the programmer is responsible for preventing
@lquotes@;dangling references.@rquotes@;]
@end{Intro}

@begin{StaticSem}
@Leading@;The following attribute is defined for @PrefixType{a @nt{prefix} X that
denotes an aliased view of an object}:
@begin{Description}
@Attribute{Prefix=<X>, AttrName=<Unchecked_Access>,
  Text=<All rules and semantics that apply to
X'Access (see @RefSecNum{Operations of Access Types})
apply also to X'Unchecked_Access,
except that,
for the purposes of accessibility rules and checks,
it is as if X were declared immediately within a library package.>}
@IndexSeeAlso{Term=[Access attribute],See=(Unchecked_Access attribute)}
@end{Description}
@EndPrefixType{}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
@ChgAdded{Version=[3],Text=[We say @ldquote@;rules and semantics@rdquote@;
here so that library-level accessibility applies to the value created by
X'Unchecked_Access as well as to the checks needed for the attribute itself.
This means that any anonymous access values that inherit the accessibility of
this attribute (such as access parameters) also act as if they have
library-level accessibility. We don't want the "real" accessibility of the
created value re-emerging at a later point @en that would create
hard-to-understand bugs.]}
@end{Ramification}
@end{StaticSem}

@begin{Notes}
This attribute is provided to support the situation where a local
object is to be inserted into a global linked data structure, when the
programmer knows that it will always be removed from the data structure prior
to exiting the object's scope. The Access attribute would
be illegal in this case
(see @RefSec{Operations of Access Types}).
@begin{Ramification}
@PDefn2{Term=[expected type],
  Sec=(Unchecked_Access attribute)}
The expected type for X'Unchecked_Access is as for X'Access.

If an @nt<attribute_reference> with Unchecked_Access is used
as the actual parameter for an access parameter,
an Accessibility_Check can never fail on that access
parameter.
@end{Ramification}

There is no Unchecked_Access attribute for subprograms.
@begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00254-01]}
  Such an attribute would allow @Chg{Version=[2],New=[unsafe ],
  Old=[]}@lquotes@;downward closures@rquotes@;,
  where an access value designating a more nested subprogram is passed
  to a less nested subprogram.@Chg{Version=[2],New=[ (Anonymous
  access-to-subprogram parameters provide safe
  @lquotes@;downward closures@rquotes@;.)],Old=[]}
  This requires some means of reconstructing the global environment for
  the more nested subprogram,
  so that it can do up-level references to objects.
  The two methods of implementing up-level references are displays and
  static links.
  If @Chg{Version=[2],New=[unsafe ],Old=[]}downward closures were
  supported,
  each access-to-subprogram value would have to carry the static link
  or display with it.
  @Chg{Version=[2],New=[We don't want to require the space and time
  overhead of requiring the extra information for all access-to-subprogram
  types, especially as including it
  would make interfacing to other languages (like C) harder],Old=[In the case
  of displays, this was judged to be infeasible,
  and we don't want to disrupt implementations by forcing them
  to use static links if they already use displays]}.

  If desired, an instance of Unchecked_Conversion can be used to create
  an access value of a global access-to-subprogram type that
  designates a local subprogram. The semantics of using
  such a value are not specified by the language.
  In particular, it is not specified what happens if such
  subprograms make up-level references; even if the frame
  being referenced still exists, the up-level reference might
  go awry if the representation of a value of a global access-to-subprogram
  type doesn't include a static link.
@end{Reason}

@end{Notes}

@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledClause{Storage Management}

@begin{Intro}
@Redundant[
@Defn{user-defined storage management}
@Defn2{Term=[storage management], Sec=(user-defined)}
@Defn{user-defined heap management}
@Defn2{Term=[heap management], Sec=(user-defined)}
Each access-to-object type has an associated storage
pool.
The storage allocated by an @nt{allocator} comes from the pool;
instances of Unchecked_Deallocation return storage to the pool.
Several access types can share the same pool.]

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00435-01]}
@Redundant[A storage pool is a variable of a type in the class rooted
at Root_Storage_Pool, which is an abstract limited controlled type.
By default, the implementation chooses a @i{standard storage pool}
for each access@Chg{Version=[2],New=[-to-object],Old=[]} type.
The user may define new pool types,
and may override the choice of pool for an access@Chg{Version=[2],New=[-to-object],Old=[]}
type by specifying Storage_Pool for the type.]
@begin{Ramification}
By default, the implementation might choose to have a single global
storage pool,
which is used (by default) by all access types,
which might mean that storage is reclaimed automatically only
upon partition completion.
Alternatively, it might choose to create a new pool
at each accessibility level,
which might mean that storage is reclaimed for an access type
when leaving the appropriate scope.
Other schemes are possible.
@end{Ramification}

@ChgToGlossary{Version=[3],Kind=[Added],Term=<Storage pool>,
Text=<@ChgAdded{Version=[3],Text=[Each access-to-object type has an associated
storage pool object. The storage for an object created by an @nt{allocator}
comes from the storage pool of the type of the @nt{allocator}. Some storage
pools may be partitioned into subpools in order to support finer-grained storage
management.]}>}

@end{Intro}

@begin{Legality}
If Storage_Pool is specified for a given access type,
Storage_Size shall not be specified for it.
@begin{Reason}
The Storage_Pool determines the Storage_Size;
hence it would not make sense to specify both.
Note that this rule is simplified by the fact that the aspects in
question cannot be specified for derived types,
nor for nonfirst subtypes,
so we don't have to worry about whether, say, Storage_Pool on a
derived type overrides Storage_Size on the parent type.
For the same reason, @lquotes@;specified@rquotes@; means the same thing as
@lquotes@;directly specified@rquotes@; here.
@end{Reason}
@end{Legality}

@begin{StaticSem}
@Leading@keepnext@;The following language-defined library package exists:
@begin{Example}
@key[with] Ada.Finalization;
@key[with] System.Storage_Elements;
@ChildUnit{Parent=[System],Child=[Storage_Pools]}@key[package] System.Storage_Pools @key[is]
    @key{pragma} Preelaborate(System.Storage_Pools);

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00161-01]}
    @key[type] @AdaTypeDefn{Root_Storage_Pool} @key[is]
        @key[abstract] @key[new] Ada.Finalization.Limited_Controlled @key[with] @key[private];@Chg{Version=[2],New=[
    @key[pragma] Preelaborable_Initialization(Root_Storage_Pool);],Old=[]}

    @key[procedure] @AdaSubDefn{Allocate}(
      Pool : @key[in] @key[out] Root_Storage_Pool;
      Storage_Address : @key[out] Address;
      Size_In_Storage_Elements : @key[in] Storage_Elements.Storage_Count;
      Alignment : @key[in] Storage_Elements.Storage_Count) @key[is] @key[abstract];

    @key[procedure] @AdaSubDefn{Deallocate}(
      Pool : @key[in] @key[out] Root_Storage_Pool;
      Storage_Address : @key[in] Address;
      Size_In_Storage_Elements : @key[in] Storage_Elements.Storage_Count;
      Alignment : @key[in] Storage_Elements.Storage_Count) @key[is] @key[abstract];

    @key[function] @AdaSubDefn{Storage_Size}(Pool : Root_Storage_Pool)
        @key[return] Storage_Elements.Storage_Count @key[is] @key[abstract];

@key[private]
   ... -- @RI{not specified by the language}
@key[end] System.Storage_Pools;
@end{Example}
@begin{Reason}
The Alignment parameter is provided to Deallocate because some
allocation strategies require it.
If it is not needed, it can be ignored.
@end{Reason}

@Defn{storage pool type}
@Defn{pool type}
A @i{storage pool type} (or @i{pool type}) is a descendant of
Root_Storage_Pool.
@Defn{storage pool element}
@Defn{pool element}
@Defn2{Term=[element], Sec=(of a storage pool)}
The @i{elements} of a storage pool are the objects allocated in the
pool by @nt{allocator}s.
@begin{Discussion}
In most cases, an element corresponds to a single memory block
allocated by Allocate.
However, in some cases the implementation may choose to associate
more than one memory block with a given pool element.
@end{Discussion}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00435-01]}
@Leading@;For @PrefixType{every access@Chg{Version=[2],New=[-to-object],Old=[]} subtype S},
the following @Chg{New=[representation ],Old=[]}attributes are defined:
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<Storage_Pool>,
  Text=<Denotes the storage pool of the type of S.
The type of this attribute is Root_@!Storage_@!Pool'Class.>}

@Attribute{Prefix=<S>, AttrName=<Storage_Size>,
  Text=<Yields the result of calling
Storage_Size(S'Storage_Pool)@Redundant{,
which is intended to be a measure of the number of storage elements
reserved for the pool.}
The type of this attribute is @i{universal_integer}.>}
@EndPrefixType{}
@begin{Ramification}
Storage_Size is also defined for task subtypes and objects
@em see @RefSecNum{Operational and Representation Attributes}.

Storage_Size is not a measure of how much un-allocated space is
left in the pool.
That is, it includes both allocated and unallocated space.
Implementations and users may provide a Storage_Available function
for their pools, if so desired.
@end{Ramification}
@end{Description}

@PDefn2{Term=[specifiable], Sec=(of Storage_Size for
a nonderived access-to-object type)}
@PDefn2{Term=[specifiable], Sec=(of Storage_Pool for
a nonderived access-to-object type)}
@Defn{Storage_Pool clause}
@Defn{Storage_Size clause}
Storage_Size or Storage_Pool may be specified for
a nonderived access-to-object type
via an @nt{attribute_@!definition_@!clause};
the @nt{name} in a Storage_Pool clause shall denote a
variable.@Chg{Version=[3],New=[@AspectDefn{Storage_Pool}@AspectDefn{Storage_Size (access)}],Old=[]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Storage_Pool],
    Text=[@ChgAdded{Version=[3],Text=[Pool of memory from which @key[new] will
    allocate for a given access type.]}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Storage_Size (access)],
    Text=[@ChgAdded{Version=[3],Text=[Sets memory size for allocations for an
      access type.]}]}



@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0107-1],ARef=[AI05-0111-3],ARef=[AI05-0116-1]}
An @nt{allocator} of @Chg{Version=[3],New=[a ],Old=[]}type @i<T>
@Chg{Version=[3],New=[that does not support subpools ],Old=[]}allocates
storage from @i<T>'s storage pool.
If the storage pool is a user-defined object, then
the storage is allocated by calling Allocate@Chg{Version=[3],New=[ as described
below. @nt{Allocator}s for types that support subpools are described
in @RefSecNum{Storage Subpools}.],Old=[,
passing T'Storage_Pool as the Pool parameter.
The Size_In_Storage_Elements parameter indicates the number of storage elements
to be allocated,
and is no more than D'Max_Size_In_Storage_Elements,
where D is the designated subtype.
The Alignment parameter is D'Alignment.
@PDefn{contiguous representation}
@PDefn{discontiguous representation}
The result returned in the Storage_Address parameter is used by the
@nt{allocator} as the address of the allocated storage,
which is a contiguous block of memory of Size_In_@!Storage_@!Elements
storage elements.
@Redundant[Any exception propagated by Allocate is propagated by the
@nt{allocator}.]]}
@begin{Ramification}
If the implementation chooses to represent the designated
subtype in multiple pieces,
one @nt{allocator} evaluation might result in more than one call upon
Allocate.
In any case, @nt{allocator}s for the access type obtain all the required
storage for an object of the designated type by calling the
specified Allocate procedure.

@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0107-1]}
@ChgDeleted{Version=[3],Text=[Note that the implementation does not turn other
exceptions into Storage_Error.]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0111],ARef=[AI95-00103-01]}
@ChgAdded{Version=[1],Text=[If @i<D> (the designated type of @i<T>) includes
subcomponents of other access types, they will be allocated from the storage
pools for those types, even if those @nt{allocator}s are executed as part of
the @nt{allocator} of @i<T> (as part of the initialization of the object). For
instance, an access-to-task type @i<TT> may allocate the data structures used to
implement the task value from other storage pools. (In particular, the task
stack does not necessarily need to be allocated from the storage pool for
@i<TT>.)]}
@end{Ramification}

@Defn{standard storage pool}
If Storage_Pool is not specified for a type defined by an
@nt{access_to_object_definition},
then the implementation chooses a standard storage pool for it
in an implementation-defined manner.
@IndexCheck{Storage_Check}
@Defn2{Term=[Storage_Error],Sec=(raised by failure of run-time check)}
In this case,
the exception Storage_Error is raised by an @nt{allocator}
if there is not enough storage.
It is implementation defined whether or not the implementation
provides user-accessible names for the standard pool type(s).
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],Text=[The
manner of choosing a storage pool for an access type when
Storage_Pool is not specified for the type.]}]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[The manner of choosing a storage pool is
  covered by a @DocReqName below, so it is not summarized here.]}
@end{Discussion}
@ImplDef{Whether or not the implementation
provides user-accessible names for the standard pool type(s).}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
@ChgNote{This was never true.}@Chg{Version=[2],New=[],Old=[An anonymous access
type has no pool. ]}An access-to-object type defined by a
@nt{derived_type_definition}
inherits its pool from its parent type, so
all access-to-object types in the same derivation class share the same pool.
Hence the @lquotes@;defined by an @nt{access_to_object_definition}@rquotes@; wording
above.

@PDefn{contiguous representation}
@PDefn{discontiguous representation}
There is no requirement that all storage pools be implemented using a
contiguous block of memory (although each allocation returns
a pointer to a contiguous block of memory).
@end{Ramification}

If Storage_Size is specified for an access type,
then the Storage_Size of this pool is at least that requested,
and the storage for the pool is reclaimed when the master containing
the declaration of the access type is left.
@Defn2{Term=[Storage_Error],Sec=(raised by failure of run-time check)}
If the implementation cannot satisfy the request,
Storage_Error is raised at the point of the
@nt{attribute_@!definition_@!clause}.
If neither Storage_Pool nor Storage_Size are specified,
then the meaning of Storage_Size is implementation defined.
@ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],Text=[The meaning of
Storage_Size@Chg{Version=[2], New=[ when neither the
Storage_Size nor the Storage_Pool is specified for an access type],Old=[]}.]}
@begin{Ramification}
The Storage_Size function and attribute will return the actual
size, rather than the requested size.
Comments about rounding up, zero, and negative
on task Storage_Size apply here, as well.
See also AI83-00557, AI83-00558, and AI83-00608.

The expression in a Storage_Size clause
need not be static.

The reclamation happens after the master is finalized.
@end{Ramification}
@begin{ImplNote}
For a pool allocated on the stack, normal stack cut-back can
accomplish the reclamation.
For a library-level pool, normal partition termination actions can
accomplish the reclamation.
@end{ImplNote}

If Storage_Pool is specified for an access type,
then the specified pool is used.

@PDefn{unspecified}
The effect of calling Allocate and Deallocate for a standard storage
pool directly
(rather than implicitly via an @nt{allocator} or an instance
of Unchecked_Deallocation) is unspecified.
@begin{Ramification}
For example, an @nt{allocator} might put the pool element on a
finalization list.
If the user directly Deallocates it, instead of calling an instance
of Unchecked_Deallocation, then the implementation would probably try
to finalize the object upon master completion,
which would be bad news.
Therefore, the implementation should define such situations as
erroneous.
@end{Ramification}
@end{StaticSem}

@begin{Erron}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
If Storage_Pool is specified for an access type,
then if Allocate can satisfy the request,
it should allocate a contiguous block of memory,
and return the address of the first storage element in Storage_Address.
The block should contain Size_In_Storage_Elements storage elements,
and should be aligned according to Alignment.
The allocated storage should not be used for any
other purpose while the pool element remains in existence.
If the request cannot be satisfied,
then Allocate should propagate an exception
@Redundant[(such as Storage_Error)].
If Allocate behaves in any other manner,
then the program execution is erroneous.
@end{Erron}

@begin{ImplReq}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0107-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The Allocate procedure of a
user-defined storage pool object @i<P> may be called by the implementation only
to allocate storage for a type @i<T> whose pool is @i<P>, only at the
following points:]}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[During the execution of an @nt{allocator} of type
@i<T>;]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This includes during the evaluation of the
  initializing expression such as an @nt{aggregate}; this is important if the
  initializing expression is built in place. We need to allow allocation to be
  deferred until the size of the object is known.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[During the execution of a return statement for a
function whose result is built-in-place in the result of an @nt{allocator}
of type @i<T>;]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We need this bullet as well as the preceding one
  in order that exceptions that propagate from such a call to Allocate can be
  handled within the return statement. We don't want to require the generation
  of special handling code in this unusual case, as it would add overhead to
  most return statements of composite types.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[During the execution of an assignment operation with
a target of an allocated object of type @i<T> with a part that has an
unconstrained discriminated subtype with defaults.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We allow Allocate to be called during assignment
  of objects with mutable parts so that mutable objects can be implemented with
  reallocation on assignment. (Unfortunately, the term "mutable" is only defined
  in the AARM, so we have to use the long-winded wording shown here.)]}
@end{Reason}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Of course, explicit calls to Allocate are also
  allowed and are not bound by any of the rules found here.]}
@end{Discussion}
@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0107-1],ARef=[AI05-0116-1],ARef=[AI05-0193-1],ARef=[AI05-0262-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[For each of the calls of Allocate described above,
@i{P} (equivalent to @i{T}'Storage_Pool) is passed as the Pool parameter. The
Size_In_Storage_Elements parameter indicates the number of storage elements to
be allocated, and is no more than @i{D}'Max_Size_In_Storage_Elements, where
@i{D} is the designated subtype of @i{T}. The Alignment parameter is a nonzero
integral multiple of @i{D}'Alignment if @i{D} is a specific type, and otherwise
is a nonzero integral multiple of the alignment of the specific type identified
by the tag of the object being created; it is
unspecified if there is no such value. The Alignment parameter is no more than
@i{D}'Max_Alignment_For_Allocation. The result returned in the Storage_Address
parameter is used as the address of the allocated storage, which is a contiguous
block of memory of Size_In_Storage_Elements storage elements. @Redundant[Any
exception propagated by Allocate is propagated by the construct that contained
the call.]]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Note that the implementation does not turn other
  exceptions into Storage_Error.]}@ChgNote{Moved from 13.11(16).}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[@ldquote@;Nonzero integral multiple@rdquote
  of an alignment includes the alignment value itself, of course. The
  value is unspecified if the alignment of the specific type is zero.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0107-1]}
@ChgAdded{Version=[3],Text=[The number of calls to Allocate needed to implement
an @nt{allocator} for any particular type is unspecified.@PDefn{unspecified}
@PDefn{contiguous representation}
@PDefn{discontiguous representation}
The number of calls to Deallocate needed to implement an instance of
Unchecked_Deallocation (see @RefSecNum{Unchecked Storage Deallocation}) for any
particular object is the same as the number of Allocate calls for that object.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This supports objects that are allocated in one or
  more parts. The second sentence prevents extra or missing calls to Deallocate.]}
@end{Reason}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[3],Text=[The number of calls to Deallocate from all sources
  for an object always will be the same as the number of calls to Allocate from
  all sources for that object. However, in unusual cases, not all of those
  Deallocate calls may be made by an instance of Unchecked_Deallocation.
  Specifically, in the unusual case of assigning to an object of a mutable
  variant record type such that the variant changes, some of the Deallocate
  calls may be made by the assignment (as may some of the Allocate calls).]}
@end{Honest}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We do not define the relative order of multiple
  calls used to deallocate the same object @em that is, if the @nt{allocator}
  allocated two pieces @i{x} and @i{y}, then an instance of Unchecked_Deallocation
  might deallocate @i{x} and then @i{y}, or it might deallocate @i{y} and then
  @i{x}.]}@ChgNote{Moved from 13.11.2}
@end{Ramification}


@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0107-1]}
@ChgAdded{Version=[3],Text=[The Deallocate procedure of a user-defined storage
pool object @i{P} may be called by the implementation to deallocate storage for a
type @i{T} whose pool is @i{P} only at the places when an Allocate call is allowed for
@i{P}, during the execution of an instance of Unchecked_Deallocation for @i{T}, or as
part of the finalization of the collection of @i{T}. For such a call of Deallocate,
@i{P} (equivalent to @i{T}'Storage_Pool) is passed as the Pool parameter. The
value of the Storage_Address parameter for a call to Deallocate is the value
returned in the Storage_Address parameter of the corresponding successful call
to Allocate. The values of the Size_In_Storage_Elements and Alignment parameters
are the same values passed to the corresponding Allocate call. Any exception
propagated by Deallocate is propagated by the construct that contained the
call.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We allow Deallocate to be called anywhere that
  Allocate is, in order to allow the recovery of storage from failed allocations
  (that is, those that raise exceptions); from extended return statements that
  exit via a goto, exit, or locally handled exception; and from objects that
  are reallocated when they are assigned. In each of these cases, we would have
  a storage leak if the implementation did not recover the storage (there is no
  way for the programmer to do it). We do not require such recovery, however, as
  it could be a serious performance drag on these operations.]}
@end{Reason}
@end{ImplReq}

@begin{DocReq}
An implementation shall document
the set of values that a user-defined Allocate procedure needs
to accept for the Alignment parameter.
An implementation shall document
how the standard storage pool is chosen,
and how storage is allocated by standard storage pools.
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Implementation-defined aspects of storage pools.]}]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[
The set of values that a user-defined Allocate procedure needs
to accept for the Alignment parameter.
How the standard storage pool is chosen, and how storage is allocated
by standard storage pools.]}]}
@end{DocReq}

@begin{ImplAdvice}
An implementation should document any cases in which it dynamically
allocates heap storage for a purpose other than the evaluation of an
@nt{allocator}.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Any cases in which heap storage is dynamically allocated other than
as part of the evaluation of an @nt{allocator} should be documented.]}]}
@begin{Reason}
This is @lquotes@;@ImplAdviceTitle@rquotes@; because the term @lquotes@;heap storage@rquotes@;
is not formally definable;
therefore, it is not testable whether the implementation obeys this advice.
@end{Reason}

A default (implementation-provided) storage pool for an
access-to-constant type should
not have overhead to support deallocation of individual objects.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[A default storage pool for an access-to-constant type should
not have overhead to support deallocation of individual objects.]}]}
@begin{Ramification}
Unchecked_Deallocation is not defined for
such types. If the access-to-constant type is library-level,
then no deallocation (other than at partition completion) will
ever be necessary, so if the size needed by an @nt{allocator}
of the type is known at link-time, then the allocation
should be performed statically.
If, in addition, the initial value of the designated object is known
at compile time, the object can be allocated to read-only memory.
@end{Ramification}
@begin{ImplNote}
If the Storage_Size for an access type is specified,
the storage pool should consist of a contiguous block of memory,
possibly allocated on the stack.
The pool should contain approximately this number of
storage elements.
These storage elements should be reserved at the place of the
Storage_Size clause,
so that @nt{allocator}s cannot raise Storage_Error due to running out
of pool space until the appropriate number of storage elements has
been used up.
This approximate (possibly rounded-up) value should be used as a maximum;
the implementation should not increase the size of the pool on the fly.
If the Storage_Size for an access type is specified as zero,
then the pool should not take up any storage space,
and any @nt{allocator} for the type should raise Storage_Error.
@end{ImplNote}
@begin{Ramification}
Note that most of this is approximate,
and so cannot be (portably) tested.
That's why we make it an Implementation Note.
There is no particular number of allocations that is guaranteed to
succeed, and there is no particular number of allocations that is
guaranteed to fail.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
@ChgNote{Use ChgAdded to get conditional Leading}@ChgAdded{Version=[2],
Type=[Leading],Text=[]}@Chg{Version=[2],New=[The],Old=[A]}
storage pool @Chg{Version=[2],New=[used ],Old=[]}for
@Chg{Version=[2],New=[an @nt{allocator} of ],Old=[]}an
anonymous access type should be
@Chg{Version=[2],New=[determined as follows:],Old=[created
at the point of an allocator for the type, and be reclaimed when
the designated object becomes inaccessible;]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01],ARef=[AI95-00416-01]}
@ChgAdded{Version=[2],Text=[If the @nt{allocator} is defining a
coextension (see @RefSecNum{Operations of Access Types}) of an object
being created by an outer @nt{allocator}, then
the storage pool used for the outer @nt{allocator} should also be used for
the coextension;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01]}
@ChgAdded{Version=[2],Text=[For other access discriminants and access
parameters, the storage pool should be created at the point of the
@nt{allocator}, and be reclaimed when the allocated object becomes
inaccessible;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0051-1]}
@ChgAdded{Version=[3],Text=[If the @nt{allocator} defines the result of a
function with an access result, the storage pool is determined as though the
@nt{allocator} were in place of the call of the function. If the call is the
operand of a type conversion, the storage pool is that of the target access type
of the conversion. If the call is itself defining the result of a function with
an access result, this rule is applied recursively;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01]}
@ChgAdded{Version=[2],Text=[Otherwise, a default storage pool should be
created at the point where the anonymous access type is elaborated; such
a storage pool need not support deallocation of individual objects.]}
@end{Itemize}

@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Usually, a storage pool for an access discriminant or access parameter
should be created at the point of an @nt{allocator}, and be reclaimed when
the designated object becomes inaccessible. For other anonymous access types,
the pool should be created at the point where the type is elaborated and need
not support deallocation of individual objects.]}]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
  @Chg{Version=[2],New=[For access parameters and access discriminants,],
  Old=[Normally]} the "storage pool" for an anonymous access type
  would not @Chg{Version=[2],New=[normally ],Old=[]}exist as a separate
  entity.
  Instead, the designated object of the allocator
  would be allocated, in the case of an access parameter,
  as a local aliased variable at the call site, and in the
  case of an access discriminant, contiguous with the object
  containing the discriminant.
  This is similar to the way storage for @nt{aggregate}s is typically
  managed.

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01]}
  @ChgAdded{Version=[2],Text=[For other sorts of anonymous access types, this
  implementation is not possible in general, as the accessibility of the
  anonymous access type is that of its declaration, while the @nt{allocator}
  could be more nested. In this case, a "real" storage pool is required.
  Note, however, that this storage pool need not support (separate)
  deallocation, as it is not possible to instantiate Unchecked_Deallocation
  with an anonymous access type. (If deallocation is needed, the object should
  be allocated for a named access type and converted.) Thus, deallocation only
  need happen when the anonymous access type itself goes out of scope;
  this is similar to the case of an access-to-constant type.]}
@end{ImplNote}
@end{ImplAdvice}

@begin{Notes}
A user-defined storage pool type can be obtained by extending the
Root_Storage_Pool type,
and overriding the primitive subprograms Allocate, Deallocate, and
Storage_Size.
A user-defined storage pool can then be obtained by declaring
an object of the type extension.
The user can override Initialize and Finalize if there is any need
for nontrivial initialization and finalization for a user-defined
pool type.
For example, Finalize might reclaim blocks of storage that are allocated
separately from the pool object itself.

@Leading@;The writer of the user-defined allocation and deallocation
procedures, and users of @nt{allocator}s for the associated access
type, are responsible for dealing with any interactions with
tasking. In particular:
@begin{itemize}
  If the @nt{allocator}s are used in different tasks, they require
mutual exclusion.

 If they are used inside protected objects, they cannot block.

 If they are used by interrupt handlers
(see @RefSec{Interrupt Support}),
the mutual exclusion mechanism has to work properly in that context.
 @end{itemize}

The primitives Allocate, Deallocate, and Storage_Size are declared as
abstract (see @RefSecNum{Abstract Types and Subprograms}),
and therefore they have to be overridden when
a new (nonabstract) storage pool type is declared.
@begin{Ramification}
Note that the Storage_Pool attribute denotes an object,
rather than a value,
which is somewhat unusual for attributes.

The calls to Allocate, Deallocate, and Storage_Size are dispatching
calls @em this follows from the fact that the actual parameter for
Pool is T'Storage_Pool, which is of type Root_Storage_Pool'Class. In
many cases (including all cases in which Storage_Pool is not
specified), the compiler can determine the tag statically.
However, it is possible to construct cases where it cannot.

All access types in the same derivation class share the same pool,
whether implementation defined or user defined.
This is necessary because we allow type conversions among them
(even if they are pool-specific),
and we want pool-specific access values to always designate
an element of the right pool.
@end{Ramification}
@begin{ImplNote}
If an access type has a standard storage pool,
then the implementation doesn't actually have to follow the pool
interface described here,
since this would be semantically invisible.
For example, the allocator could conceivably be implemented with inline
code.
@end{ImplNote}
@end{Notes}

@begin{Examples}
@Leading@;To associate an access type with a storage pool object, the user
first declares a pool object of some type derived from
Root_Storage_Pool. Then, the user defines its Storage_Pool
attribute, as follows:

@begin{Example}
Pool_Object : Some_Storage_Pool_Type;

@key[type] T @key[is] @key[access] Designated;
@key[for] T'Storage_Pool @key[use] Pool_Object;
@end{Example}

@begin{WideAbove}
@Leading@;Another access type may be added to an existing storage pool, via:
@end{WideAbove}
@begin{Example}
@key[for] T2'Storage_Pool @key[use] T'Storage_Pool;
@end{Example}

The semantics of this is implementation defined
for a standard storage pool.
@begin{Reason}
For example, the implementation is allowed to choose a storage pool
for T that takes advantage of the fact that T is of a certain size.
If T2 is not of that size, then the above will probably not work.
@end{Reason}

@Leading@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0111-3]}
As usual, a derivative of Root_Storage_Pool may define additional
operations. For example, @Chg{Version=[3],New=[consider the],Old=[presuming that]}
Mark_Release_Pool_Type @Chg{Version=[3],New=[defined in
@RefSecNum{Storage Subpool Example}, that ],Old=[]}has
two additional operations, Mark and Release,
the following is a possible use:
@begin{Example}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0041],ARef=[AI95-00066-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0111-3]}
@key[type] Mark_Release_Pool_Type
   (Pool_Size : Storage_Elements.Storage_Count@Chg{Version=[3],New=[],Old=[;
    Block_Size : Storage_Elements.Storage_Count]})
        @key[is] @key[new] @Chg{Version=[3],New=[Subpools.Root_Storage_Pool_With_Subpools],Old=[Root_Storage_Pool]} @key[with @Chg{New=[],Old=[limited ]}private];@Chg{Version=[3],New=[
           -- @Examcom{As defined in package MR_Pool, see @RefSecNum{Storage Subpool Example}}],Old=[]}

...

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0111-3]}
@Chg{Version=[3],New=[Our_Pool],Old=[MR_Pool]} : Mark_Release_Pool_Type (Pool_Size => 2000@Chg{Version=[3],New=[],Old=[,
                                  Block_Size => 100]});
@Chg{Version=[3],New=[My_Mark : MR_Pool.Subpool_Handle; -- @Examcom{See @RefSecNum{Storage Subpool Example}}],Old=[]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0111-3]}
@key[type] Acc @key[is] @key[access] ...;
@key[for] Acc'Storage_Pool @key[use] @Chg{Version=[3],New=[Our_Pool],Old=[MR_Pool]};
...

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0111-3]}
@Chg{Version=[3],New=[My_Mark := ],Old=[]}Mark(@Chg{Version=[3],New=[Our_Pool],Old=[MR_Pool]});
... --@RI{ Allocate objects using @lquotes@;@key[new] @Chg{Version=[3],New=[(My_Mark) ],Old=[]}Designated(...)@rquotes@;.}
Release(@Chg{Version=[3],New=[My_Mark],Old=[MR_Pool]}); --@RI{ @Chg{Version=[3],New=[Finalize objects and reclaim],Old=[Reclaim the]} storage.}
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
User-defined storage pools are new to Ada 95.
@end{Extend83}

@begin{DiffWord83}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1],ARef=[AI05-0190-1]}
Ada 83 @Chg{Version=[3],New=[originally introduced the],Old=[had a]}
concept called a @lquotes@;collection,@rquotes@;
which is similar to what we call a storage pool.
All access types in the same derivation class
@Chg{Version=[3],New=[share],Old=[shared]} the same collection.
@Chg{Version=[3],New=[],Old=[In ]}Ada 95@Chg{Version=[3],New=[ introduces
the storage pool, which is similar in that],Old=[,]} all access types in the
same derivation class
share the same storage pool,
but other (unrelated) access types can also share the same storage pool,
either by default, or as specified by the user.
A collection @Chg{Version=[3],New=[is],Old=[was]} an amorphous
@Chg{Version=[3],New=[grouping],Old=[collection]}
of objects@Chg{Version=[3],New=[ (mainly used to describe finalization of
access types)],Old=[]};
a storage pool is a more concrete concept @em hence
the different name.

RM83 states the erroneousness of reading or updating deallocated
objects incorrectly by missing various cases.
@end{DiffWord83}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00435-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @b<Amendment Correction:> Storage pools (and Storage_Size)
  are not defined for access-to-subprogram types. The original Ada 95 wording
  defined the attributes, but said nothing about their values.
  If a program uses attributes Storage_Pool or Storage_Size on an
  access-to-subprogram type, it will need to be corrected for Ada 2005.
  That's a good thing, as such a use is a bug @em the concepts
  never were defined for such types.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00161-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @b[Amendment Correction:] Added @nt{pragma} Preelaborable_Initialization to
  type Root_Storage_Pool, so that extensions of it can be used to declare
  default-initialized objects in preelaborated units.]}
@end{Extend95}


@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0009],ARef=[AI95-00137-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added wording to specify that
  these are representation attributes.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[Added wording to clarify that an @nt{allocator}
  for a coextension nested inside an outer @nt{allocator} shares
  the pool with the outer @nt{allocator}.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0051-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added the missing definition
  of the storage pool of an @nt{allocator} for an anonymous access result type.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0107-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified when an implementation
  is allowed to call Allocate and Deallocate, and the requirements on such
  calls.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
  @ChgAdded{Version=[3],Text=[Added wording to support subpools and refer to
  the subpool example, see @RefSecNum{Storage Subpools}.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0116-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added wording to specify that the
  alignment for an @nt{allocator} with a class-wide designated type comes from
  the specific type that is allocated.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0193-1]}
  @ChgAdded{Version=[3],Text=[Added wording to allow larger
  alignments for calls to Allocate made by @nt{allocator}s,
  up to Max_Alignment_For_Allocation. This eases implementation in some cases.]}
@end{DiffWord2005}



@LabeledRevisedSubClause{Version=[3],New=[Storage Allocation Attributes],
Old=[The Max_Size_In_Storage_Elements Attribute]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0193-1]}
@redundant[The Max_Size_In_Storage_Elements @Chg{Version=[3],New=[and
Max_Alignment_For_Allocation attributes may be],Old=[attribute is]}
useful in writing user-defined pool types.]
@end{Intro}

@begin{StaticSem}
@Leading@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0193-1]}
For @PrefixType{every subtype S},
the following @Chg{Version=[3],New=[attributes are],Old=[attribute is]} defined:
@begin{Description}
@ChgAttribute{Version=[3],Kind=[Revised],ChginAnnex=[T],
  Leading=<F>, Prefix=<S>, AttrName=<Max_Size_In_Storage_Elements>,
  ARef=[AI95-00256-01],ARef=[AI95-00416-01],ARef=[AI05-0193-1],
  Text=<Denotes the maximum value for Size_In_Storage_Elements
that @Chg{Version=[2],New=[could],Old=[will]} be requested @Chg{Version=[2],
New=[by the implementation ],Old=[]}via Allocate for an access type whose
designated subtype is S.@Chg{Version=[2],New=[@Chg{Version=[3],New=[],Old=[ For a type with access
discriminants, if the implementation allocates space for a coextension
in the same pool as that of the object having the access discriminant,
then this accounts for any calls on Allocate that could be performed to
provide space for such coextensions.]}],Old=[]}
The value of this attribute is of type @i{universal_integer}.>}
@begin{Ramification}
If S is an unconstrained array subtype,
or an unconstrained subtype with discriminants,
S'Max_Size_In_Storage_Elements might be very large.
@end{Ramification}

@ChgAttribute{Version=[3], Kind=[AddedNormal], ChginAnnex=[T], Leading=[F],
  Prefix=<S>, AttrName=<Max_Alignment_For_Allocation>, ARef=[AI05-0193-1],
  Text=<@Chg{Version=[3],New=[Denotes the maximum value for Alignment that
  could be requested by the implementation via Allocate for an access type
  whose designated subtype is S. The value of this attribute is of type
  @i{universal_integer}.],Old=[]}>}
@EndPrefixType{}
@end{Description}
@end{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0193-1]}
@ChgAdded{Version=[3],Text=[For a type with access discriminants, if the
implementation allocates space for a coextension in the same pool as that of the
object having the access discriminant, then these attributes account for any
calls on Allocate that could be performed to provide space for such
coextensions.]}
@begin{Reason}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0193-1]}
@ChgAdded{Version=[3],Text=[The values of these attributes should reflect only
the calls that might be made to the pool specified for an access type with
designated type S. Thus, if the coextensions would normally be
allocated from a different pool than the one used for the main object (that is,
the @ImplAdviceName of @RefSecNum{Storage Management} for determining the pool
of an anonymous access discriminant is not followed), then these attributes
should not reflect any calls on Allocate used to allocate the coextensions.]}
@end{Reason}
@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0193-1]}
@ChgAdded{Version=[3],Text=[Coextensions of coextensions of this type (and so on)
are included in the values of these attributes if they are allocated from the
same pool.]}
@end{Ramification}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00256-01]}
  @ChgAdded{Version=[2],Text=[Corrected the wording so that a
  fortune-telling compiler that can see the future execution of the
  program is not required.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0193-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}The
  Max_Alignment_For_Allocation attribute is new.]}
@end{Extend2005}



@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
@LabeledSubClause{Unchecked Storage Deallocation}

@begin{Intro}
@redundant[@Defn{unchecked storage deallocation}
@Defn2{Term=[storage deallocation], Sec=(unchecked)}
@Defn{deallocation of storage}
@Defn{reclamation of storage}
@Defn{freeing storage}
Unchecked storage deallocation of an object designated by a value of an
access type is achieved by a call to an instance of
the generic procedure Unchecked_Deallocation.]
@end{Intro}

@begin{StaticSem}
@leading@keepnext@;The following language-defined generic library procedure exists:
@begin{Example}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@key[generic]
   @key[type] Object(<>) @key[is] @key[limited] @key[private];
   @key[type] Name   @key[is] @key[access]  Object;
@SubChildUnit{Parent=[Ada],Child=[Unchecked_Deallocation]}@key[procedure] Ada.Unchecked_Deallocation(X : @key[in] @key[out] Name)@Chg{Version=[3],New=[
   @key(with) Convention => Intrinsic;],Old=[;
@key[pragma] Convention(Intrinsic, Ada.Unchecked_Deallocation);]}
@key[pragma] Preelaborate(Ada.Unchecked_Deallocation);
@end{Example}
@begin{Reason}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
The @Chg{Version=[3],New=[aspect],Old=[@nt{pragma}]} Convention implies that
the attribute Access is not allowed
for instances of Unchecked_Deallocation.
@end{Reason}
@end{StaticSem}

@begin{Legality}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0157-1]}
@ChgAdded{Version=[3],Text=[A call on an instance of Unchecked_Deallocation is
illegal if the actual access type of the instance is a type for which the
Storage_Size has been specified by a static expression with value zero or is
defined by the language to be zero.
@PDefn{generic contract issue}In addition to the places where
@LegalityTitle normally apply (see @RefSecNum{Generic Instantiation}),
this rule applies also in the private part of an instance of a generic unit.]}

@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[This rule is the same as the rule for
@nt{allocator}s. We could have left the last sentence out, as a call to
Unchecked_Deallocation cannot occur in a specification as it is a procedure
call, but we left it for consistency and to avoid future maintenance hazards.]}
@end{Discussion}

@end{Legality}

@begin{RunTime}
@Leading@;Given an instance of Unchecked_Deallocation
declared as follows:
@begin{Example}
@key[procedure] Free @key[is]
    @key[new] Ada.Unchecked_Deallocation(
        @RI[object_subtype_name], @RI[access_to_variable_subtype_name]);
@end{Example}

@begin{WideAbove}
@Leading@Keepnext@;Procedure Free has the following effect:
@end{WideAbove}
@begin{Enumerate}
After executing Free(X), the value of X is @key{null}.

Free(X), when X is already equal to @key{null}, has no effect.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0107-1]}
Free(X), when X is not equal to @key{null} first
performs finalization@Chg{Version=[2],New=[ of the object designated by X (and
any coextensions of the object @em see @RefSecNum{Operations of Access Types})],
Old=[]}, as described in
@Chg{Version=[2],New=[@RefSecNum{Completion and Finalization}],
Old=[@RefSecNum{Assignment and Finalization}]}.
It then deallocates the storage occupied by the object designated by
X@Chg{Version=[2],New=[ (and any coextensions)],Old=[]}.
If the storage pool is a user-defined object, then
the storage is deallocated by calling Deallocate@Chg{Version=[3],New=[ as described
in @RefSecNum{Storage Management}],Old=[,
passing @i[access_to_@!variable_@!subtype_name]'Storage_Pool as the Pool parameter.
Storage_Address is the value returned in the Storage_Address parameter of the
corresponding Allocate call.
Size_In_@!Storage_@!Elements and Alignment are the same values passed to the
corresponding Allocate call]}.
There is one exception: if the object being freed contains tasks,
the object might not be deallocated.
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0107-1]}
Free calls only the specified Deallocate procedure
to do deallocation.@Chg{Version=[3],New=[],Old=[ For any given
object deallocation, the number of calls to Free (usually one)
will be equal to the number of Allocate calls it took
to allocate the object.
We do not define the relative order of multiple calls used to deallocate
the same object @em that is, if the @nt{allocator} allocated two pieces @i{x}
and @i{y}, then Free might deallocate @i{x} and then @i{y},
or it might deallocate @i{y} and then @i{x}.]}
@end{Ramification}
@end{Enumerate}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
@IndexSee{Term=[freed],See=(nonexistent)}
@Defn{nonexistent}
@PDefn2{Term=[exist],Sec=[cease to]}
@PDefn2{Term=[cease to exist],Sec=[object]}
After Free(X), the object designated by X, and any
subcomponents @Chg{Version=[2],New=[(and coextensions) ],Old=[]}thereof, no
longer exist; their storage can be reused for other purposes.
@end{RunTime}

@begin{Bounded}
@Leading@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to free a discriminated, unterminated
task object. The possible consequences are:
@begin{Reason}
  This is an error because the task might refer to its discriminants,
  and the discriminants might be deallocated by freeing the task object.
@end{Reason}
@begin{Itemize}
No exception is raised.

@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
@Defn2{Term=[Tasking_Error],Sec=(raised by failure of run-time check)}
Program_Error or Tasking_Error is raised at the point of the
deallocation.

@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
@Defn2{Term=[Tasking_Error],Sec=(raised by failure of run-time check)}
Program_Error or Tasking_Error is raised in the task the next time it
references any of the discriminants.
@begin{ImplNote}
  This last case presumes an implementation where the task references
  its discriminants indirectly,
  and the pointer is nulled out when the task object is deallocated.
@end{ImplNote}
@end{Itemize}

In the first two cases,
the storage for the discriminants
(and for any enclosing object if it is designated by an access
discriminant of the task)
is not reclaimed prior to task termination.
@begin{Ramification}
  The storage might never be reclaimed.
@end{Ramification}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0033-1],ARef=[AI05-0262-1]}
@Defn{nonexistent}@PDefn2{Term=(erroneous execution),Sec=(cause)}
Evaluating a name that denotes a nonexistent object@Chg{Version=[3],
New=[, or a protected subprogram or subprogram renaming whose
associated object (if any) is nonexistent, ],Old=[]}is erroneous.
The execution of a call to an instance of Unchecked_Deallocation is
erroneous if the object was created other than by an @nt<allocator> for
an access type whose pool is Name'Storage_Pool.

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0033-1],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Text=[The part about a protected subprogram is intended
  to cover the case of an access-to-protected-subprogram where the associated
  object has been deallocated. The part about a subprogram renaming is intended
  to cover the case of a renaming of a prefixed view where the prefix object has
  been deallocated, or the case of a renaming of an entry or protected
  subprogram where the associated task or protected object has been
  deallocated.]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0157-1]}
  @ChgAdded{Version=[3],Text=[This text does not cover the case of
  a name that contains a null access value, as @key[null] does not
  denote an object (rather than denoting a nonexistent object).]}
@end{Ramification}

@end{Erron}

@begin{ImplAdvice}
For a standard storage pool,
Free should actually reclaim the storage.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[For a standard storage pool, an instance of Unchecked_Deallocation
should actually reclaim the storage.]}]}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
  This is not a testable property,
  since we do not @Chg{Version=[2],New=[know ],Old=[]}how much storage is used
  by a given pool element, nor whether fragmentation can occur.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0157-1]}
@ChgAdded{Version=[3],Text=[A call on an instance of Unchecked_Deallocation with
a nonnull access value should raise Program_Error if the actual access type of
the instance is a type for which the Storage_Size has been specified to be zero
or is defined by the language to be zero.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[A call on an instance of Unchecked_Deallocation with
a nonnull access value should raise Program_Error if the actual access type of
the instance is a type for which the Storage_Size has been specified to be zero
or is defined by the language to be zero.]}]}
@begin{Discussion}
  @ChgAdded{Version=[3],Text=[If the call is not illegal (as in a generic body), we
  recommend that it raise Program_Error. Since the execution of this call is
  erroneous (any allocator from the pool will have raised Storage_Error, so
  the nonnull access value must have been allocated from a different pool or
  be a stack-allocated object), we can't require any behavior @em anything at
  all would be a legitimate implementation.]}
@end{Discussion}
@end{ImplAdvice}

@begin{Notes}
The rules here that refer to Free apply to any instance
of Unchecked_Deallocation.

Unchecked_Deallocation cannot be instantiated for an
access-to-constant type.
This is implied by the rules of @RefSecNum{Formal Access Types}.
@end{Notes}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[The rules for coextensions are clarified
  (mainly by adding that term). In theory, this reflects no change from
  Ada 95 (coextensions existed in Ada 95, they just didn't have a name).]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0033-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a rule that using
  an access-to-protected-subprogram is erroneous if the associated
  object no longer exists. It is hard to imagine an alternative meaning here,
  and this has no effect on correct programs.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0107-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Moved the requirements
  on an implementation-generated call to Deallocate to
  @RefSecNum{Storage Management}, in order to put all of the rules
  associated with implementation-generated calls to Allocate and Deallocate
  together.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0157-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added wording so that calling
  an instance of Unchecked_Deallocation is treated similarly to @nt{allocator}s
  for access types where @nt{allocator}s would be banned.]}
@end{DiffWord2005}


@LabeledRevisedSubClause{Version=[3],New=[Default Storage Pools],
Old=[Pragma Controlled]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[@Redundant[Pragma Controlled is used to prevent
any automatic reclamation of storage (garbage collection) for the objects
created by @nt<allocator>s of a given access type.]]}
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
@Leading@Keepnext@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0190-1],ARef=[AI05-0229-1]}
The form of a @nt{pragma} @Chg{Version=[3],New=[Default_Storage_Pool],Old=[Controlled]}
is as follows:
@end{SyntaxText}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0190-1],ARef=[AI05-0229-1]}
@AddedPragmaSyn<Version=[3],@Chg{Version=[3],
New=[@key{pragma} @prag<Default_Storage_Pool> (@Syn2{storage_pool_indicator});],Old=[]}>
@DeletedPragmaSyn<Version=[3],InitialVersion=[0],@Chg{Version=[3],New=[],
Old=[@key{pragma} @prag(Controlled)(@SynI{first_subtype_}@Syn2{local_name});]}>

@begin{Discussion}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[Not to be confused with type
Finalization.Controlled.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0190-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<storage_pool_indicator>,Old=<>}>,
rhs="@Chg{Version=[3],New=<@SynI{storage_pool_}@Syn2{name} | @key[null]>,Old=<>}"}

@begin{SyntaxText}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0190-1]}
@ChgAdded{Version=[3],Text=[A @nt{pragma} Default_Storage_Pool is allowed
immediately within the visible part of a @nt{package_specification}, immediately
within a @nt{declarative_part}, or as a configuration pragma.]}
@end{SyntaxText}
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0190-1]}
@ChgAdded{Version=[3],Text=[The @SynI{storage_pool_}@nt{name} is expected to be
of type Root_Storage_Pool'Class.]}
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0190-1],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[The @SynI{storage_pool_}@nt{name} shall denote a variable.],
Old=[The @SynI{first_subtype_}@nt<local_name> of a @nt{pragma} Controlled
shall denote a nonderived access subtype.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0190-1]}
@ChgAdded{Version=[3],Text=[If the @nt{pragma} is used as a configuration pragma,
the @nt{storage_pool_indicator} shall be @key[null], and it defines the
@i<default pool>@Defn{default pool}@Defn2{Term=[storage pool],Sec=[default]}@Defn2{Term=[pool],Sec=[default]} to be @b<null> within all
applicable compilation units (see @RefSecNum{Pragmas and Program Units}),
except within the immediate scope of
another @nt{pragma} Default_Storage_Pool. Otherwise, @Redundant[the pragma
occurs immediately within a sequence of declarations, and] it defines the
default pool within the immediate scope of the pragma to be either @key[null] or
the pool denoted by the @SynI{storage_pool_}@nt{name}, except within the
immediate scope of a later pragma Default_Storage_Pool. @Redundant[Thus, an
inner pragma overrides an outer one.]]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0190-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[A @nt{pragma} Default_Storage_Pool shall not be used
as a configuration pragma that applies to a compilation unit that is within the
immediate scope of another @nt{pragma} Default_Storage_Pool.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[This is to prevent confusion in cases like this:]}
@begin{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[package] Parent @key[is]
   pragma Default_Storage_Pool(...);
   ...
@key[end] Parent;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[pragma] Default_Storage_Pool(...); --@examcom{ Illegal!}
@key[package] Parent.Child @key[is]
   ...
@key[end] Parent.Child;]}
@end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Trailing],Text=[where the Default_Storage_Pool
  on Parent.Child would not (if it were legal) override the one in Parent.]}
@end{Reason}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0190-1],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[The language-defined aspect Default_Storage_Pool may be
specified for a generic instance; it defines the default pool for
access types within an instance.@AspectDefn{Default_Storage_Pool}
The expected type for the
Default_Storage_Pool aspect is Root_Storage_Pool'Class. The @nt{aspect_definition}
must be a name that denotes a variable. This aspect overrides any
Default_Storage_Pool pragma that might apply to the generic unit; if the aspect
is not specified, the default pool of the instance is that defined for the
generic unit],
Old=[@PDefn2{Term=[representation pragma], Sec=(Controlled)}
@PDefn2{Term=[pragma, representation], Sec=(Controlled)}
A @nt{pragma} Controlled is a representation pragma
@PDefn2{Term=[aspect of representation], Sec=(controlled)}
@Defn2{Term=[controlled], Sec=(aspect of representation)}
that specifies the @i{controlled} aspect of representation]}.

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Default_Storage_Pool],
    Text=[@ChgAdded{Version=[3],Text=[Default storage pool for a generic instance.]}]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0190-1],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[]}@Comment{Conditional leading}
@Chg{Version=[3],New=[For nonderived access types declared in places where the
default pool is defined by the pragma or aspect, their Storage_Pool or
Storage_Size attribute is determined as follows, unless Storage_Pool or
Storage_Size is specified for the type:],
Old=[@Defn{garbage collection}
@i{Garbage collection} is a process that automatically reclaims storage,
or moves objects to a different address,
while the objects still exist.]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0190-1]}
  @ChgAdded{Version=[3],Text=[If the default pool is @key[null], the
  Storage_Size attribute is defined by the language to be zero.
  @Redundant[Therefore, an @nt{allocator} for such a type is illegal.]]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0190-1]}
  @ChgAdded{Version=[3],Text=[If the default pool is nonnull, the Storage_Pool
  attribute is that pool.]}

@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0190-1]}
@ChgAdded{Version=[3],Text=[@Redundant[Otherwise, there is no default pool; the standard storage
pool is used for the type as described in @RefSecNum{Storage Management}.]]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0190-1],ARef=[AI05-0229-1]}
  @Chg{Version=[3],New=[Default_Storage_Pool is the only way to specify the
  storage pool for an anonymous access type],
  Old=[Storage reclamation upon leaving a master is not considered garbage
  collection]}.

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0190-1],ARef=[AI05-0229-1]}
  Note that @Chg{Version=[3],New=[coextensions should be allocated in the same
  pool (or on the stack) as the outer object (see @RefSecNum{Storage Management});
  the Storage_Pool of the access discriminant
  (and hence the Default_Storage_Pool) is supposed to be ignored for
  coextensions. This matches the required finalization point for coextensions],
  Old=[garbage collection includes compaction of a pool (@lquotes@;moved to
  a different Address@rquotes@;), even if storage reclamation is not done]}.

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0190-1]}
  @ChgAdded{Version=[3],Text=[The default storage pool for an allocator that
  occurs within an instance of a generic is defined by the Default_Storage_Pool
  aspect of the instantiation (if specified), or by the Default_Storage_Pool
  pragma that applied to the generic; the Default_Storage_Pool pragma that applies
  to the instantiation is irrelevant.]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0190-1]}
  @ChgAdded{Version=[3],Text=[It is possible to specify the Default_Storage_Pool
  aspect for an instantiation such that allocations will fail. For example, the
  generic unit might be expecting a pool that supports certain sizes and
  alignments, and the one on the instance might be more restrictive. It is the
  programmer's responsibility to get this right.]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0190-1]}
  @ChgAdded{Version=[3],Text=[The semantics of the Default_Storage_Pool aspect are
  similar to passing a pool object as a generic formal, and putting pragma
  Default_Storage_Pool at the top of the generic's visible part, specifying that
  formal.]}
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
  @ChgDeleted{Version=[3],Text=[Programs that will be damaged by automatic storage
  reclamation are just as likely to be damaged by having objects moved to
  different locations in memory. A @nt{pragma} Controlled should turn off both
  flavors of garbage collection.]}
@end{Reason}
@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
  @ChgDeleted{Version=[3],Text=[If garbage collection reclaims the storage of
  a controlled object, it should first finalize it.
  Finalization is not done when moving an object;
  any self-relative pointers will have to be updated by the garbage
  collector.
  If an implementation provides garbage collection
  for a storage pool containing controlled objects
  (see @RefSecNum{Assignment and Finalization}),
  then it should provide a means for deferring garbage collection of
  those controlled objects.]}
@end{ImplNote}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
  @ChgDeleted{Version=[3],Type=[Leading],Text=[This allows the manager of a
  resource released by a Finalize operation to defer garbage collection during
  its critical regions; it is up to the author of the Finalize operation to do
  so. Garbage collection, at least in some systems, can happen asynchronously
  with respect to normal user code. Note that it is not enough to defer garbage
  collection during Initialize, Adjust, and Finalize, because the resource in
  question might be used in other situations as well. For example:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[@key[with] Ada.Finalization;
@key[package] P @key[is]]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[    @key[type] My_Controlled @key[is]
        @key[new] Ada.Finalization.Limited_Controlled @key[with] @key[private];
    @key[procedure] Finalize(Object : @key[in] @key[out] My_Controlled);
    @key[type] My_Controlled_Access @key[is] @key[access] My_Controlled;]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[    @key[procedure] Non_Reentrant;]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[@key[private]
    ...
@key[end] P;]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[@key[package] @key[body] P @key[is]
    X : Integer := 0;
    A : @key[array](Integer @key[range] 1..10) @key[of] Integer;]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[    @key[procedure] Non_Reentrant @key[is]
    @key[begin]
        X := X + 1;
        --@Examcom{ If the system decides to do a garbage collection here,}
        --@Examcom{ then we're in trouble, because it will call Finalize on}
        --@Examcom{ the collected objects; we essentially have two threads}
        --@Examcom{ of control erroneously accessing shared variables.}
        --@Examcom{ The garbage collector behaves like a separate thread}
        --@Examcom{ of control, even though the user hasn't declared}
        --@Examcom{ any tasks.}
        A(X) := ...;
    @key[end] Non_Reentrant;]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[    @key[procedure] Finalize(Object : @key[in] @key[out] My_Controlled) @key[is]
    @key[begin]
        Non_Reentrant;
    @key[end] Finalize;
@key[end] P;]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[@key[with] P; @key[use] P;
@key[procedure] Main @key[is]
@key[begin]
    ... @key[new] My_Controlled ... --@Examcom{ allocate some objects}
    ... @Examcom{ forget the pointers to some of them, so they become garbage}
    Non_Reentrant;
@key[end] Main;]}
@end{Example}

  @ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[3],Text=[It is the user's responsibility to protect
  against this sort of thing, and the implementation's responsibility to provide
  the necessary operations.]}

  @ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[3],Text=[We do not give these operations names,
  nor explain their exact semantics,
  because different implementations of garbage collection might have
  different needs, and because garbage collection is not supported by
  most Ada implementations, so portability is not important here.
  Another reason not to
  turn off garbage collection during each entire
  Finalize operation is that it would create a serial bottleneck;
  it might be only part of the Finalize operation that conflicts with
  some other resource.
  It is the intention that the mechanisms provided be finer-grained
  than pragma Controlled.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[If a @nt{pragma} Controlled is specified
for an access type with a standard storage pool,
then garbage collection is not performed for objects in that pool.]}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
  @ChgDeleted{Version=[3],Text=[If Controlled is not specified,
  the implementation may, but need not, perform garbage
  collection. If Storage_Pool is specified,
  then a @nt{pragma} Controlled for that type is ignored.]}
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
  @ChgDeleted{Version=[3],Text=[Controlled means that implementation-provided
  garbage collection is turned off;
  if the Storage_Pool is specified, the pool controls
  whether garbage collection is done.]}
@end{Reason}
@end{StaticSem}

@begin{ImplPerm}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0190-1],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[An object created by an @nt{allocator} that is passed as
the actual parameter to an access parameter may be allocated on the stack, and
automatically reclaimed, regardless of the default pool.],
Old=[An implementation need not support garbage collection, in
which case, a pragma Controlled has no effect]}.
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0190-1]}
  @ChgAdded{Version=[3],Text=[This matches the required finalization point for
  such an allocated object.]}
@end{Discussion}
@end{ImplPerm}

@begin{Notes}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0190-1]}
@ChgAdded{Version=[3],Text=[Default_Storage_Pool may be used with restrictions No_Coextensions and
No_Access_Parameter_Allocators (see @RefSecNum{High Integrity Restrictions})
to ensure that all @nt{allocator}s use the default pool.]}
@end{Notes}

@begin{DiffWord83}
  @ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0229-1]}
  @ChgDeleted{Version=[3],Text=[Ada 83 used the term @lquotes@;automatic storage
  reclamation@rquotes@; to refer to what is known traditionally as
  @lquotes@;garbage collection@rquotes@;. Because of the existence of storage
  pools (see @RefSecNum{Storage Management}), we need to distinguish this from
  the storage reclamation that might happen upon leaving a master. Therefore, we
  now use the term @lquotes@;garbage collection@rquotes@; in its normal
  computer-science sense. This has the additional advantage of making our
  terminology more accessible to people outside the Ada world.]}
@end{DiffWord83}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 95}Pragma
  Controlled has been dropped from Ada,
  as it has no effect in any known Ada implementations and it seems to promise
  capabilities not expected in Ada implementations. This is usually not an
  incompatibility, as the pragma merely becomes unrecognized (with a warning)
  and can be implemented as an implementation-defined pragma if desired. However,
  it is incompatible if it is (now) implemented as an implementation-defined
  pragma, someone used this pragma in a unit, and they
  also used restriction No_Implementation_Pragmas on that unit. In that
  case, the pragma would now violate the restriction; but use of this pragma
  (which does nothing) should be very rare, so this is not a significant
  issue.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0190-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}The
  pragma Default_Storage_Pool is new.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[The entire discussion of garbage collection
  (and especially that of controlled objects) is deleted. Ada 2012 provides
  subpools (see @RefSecNum{Storage Subpools}) for storage management of objects,
  including controlled objects, a mechanism which is much more predictable
  than garbage collection. Note that no version of Ada allows early
  finalization of controlled objects (other than via the use of
  Unchecked_Deallocation or Unchecked_Deallocate_Subpool), so that garbage
  collection of such objects would be ineffective in the standard mode anyway.]}
@end{DiffWord2005}


@LabeledAddedSubClause{Version=[3],Name=[Storage Subpools]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[This subclause defines a package to support the
partitioning of a storage pool into subpools. A subpool may be specified as the
default to be used for allocation from the associated storage pool, or a
particular subpool may be specified as part of an @nt{allocator}
(see @RefSecNum{Allocators}).]}
@end{Intro}


@begin{StaticSem}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[The following language-defined library package
exists:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@ChildUnit{Parent=[System.Storage_Pools],Child=[Subpools]}@key[package] System.Storage_Pools.Subpools @key[is]
   @key[pragma] Preelaborate (Subpools);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Root_Storage_Pool_With_Subpools} @key[is]
      @key[abstract new] Root_Storage_Pool @key[with private];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Root_Subpool} @key[is abstract tagged limited private];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Subpool_Handle} @key[is access all] Root_Subpool'Class;
   @key[for] Subpool_Handle'Storage_Size @key[use] 0;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Create_Subpool} (Pool : @key[in out] Root_Storage_Pool_With_Subpools)
      @key[return not null] Subpool_Handle @key[is abstract];]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0252-1]}
@ChgAdded{Version=[3],Text=[   -- @Examcom{The following operations are intended for pool implementers:}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Pool_of_Subpool} (Subpool : @key[not null] Subpool_Handle)
      @key[return access] Root_Storage_Pool_With_Subpools'Class;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Set_Pool_of_Subpool} (
      Subpool : @key[in not null] Subpool_Handle;
      To : @key[in out] Root_Storage_Pool_With_Subpools'Class);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Allocate_From_Subpool} (
      Pool : @key[in out] Root_Storage_Pool_With_Subpools;
      Storage_Address : @key[out] Address;
      Size_In_Storage_Elements : @key[in] Storage_Elements.Storage_Count;
      Alignment : @key[in] Storage_Elements.Storage_Count;
      Subpool : @key[in not null] Subpool_Handle) @key[is abstract]
         @key[with] Pre'Class => Pool_of_Subpool(Subpool) = Pool'Access;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Deallocate_Subpool} (
      Pool : @key[in out] Root_Storage_Pool_With_Subpools;
      Subpool : @key[in out] Subpool_Handle) @key[is abstract]
         @key[with] Pre'Class => Pool_of_Subpool(Subpool) = Pool'Access;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Default_Subpool_for_Pool} (
      Pool : Root_Storage_Pool_With_Subpools)
         @key[return not null] Subpool_Handle;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[overriding]
   @key[procedure] @AdaSubDefn{Allocate} (
      Pool : @key[in out] Root_Storage_Pool_With_Subpools;
      Storage_Address : @key[out] Address;
      Size_In_Storage_Elements : @key[in] Storage_Elements.Storage_Count;
      Alignment : @key[in] Storage_Elements.Storage_Count);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[overriding]
   @key[procedure] @AdaSubDefn{Deallocate} (
      Pool : @key[in out] Root_Storage_Pool_With_Subpools;
      Storage_Address : @key[in] Address;
      Size_In_Storage_Elements : @key[in] Storage_Elements.Storage_Count;
      Alignment : @key[in] Storage_Elements.Storage_Count) @key[is null];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[overriding]
   @key[function] @AdaSubDefn{Storage_Size} (Pool : Root_Storage_Pool_With_Subpools)
      @key[return] Storage_Count @key[is] (Storage_Elements.Storage_Count'Last);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[private
   ... -- @Examcom{not specified by the language}
end System.Storage_Pools.Subpools;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[@Defn{subpool}@Defn2{Term=[pool],Sec={subpool}}
@Defn{subpool handle}@Defn2{Term=[handle],Sec=[subpool]}
@Defn{storage pool that supports subpools}A @i<subpool> is
a separately reclaimable portion of a storage pool, identified by
an object of type Subpool_Handle (a @i<subpool handle>). A subpool handle also
identifies the enclosing storage pool, a @i<storage pool that supports subpools>,
which is a storage pool whose type is descended from
Root_Storage_Pool_With_Subpools. A subpool is created by calling Create_Subpool
or a similar constructor; the constructor returns the subpool handle.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[A @i<subpool object> is an object of a type
descended from Root_Subpool. @Redundant[Typically, subpool objects are
managed by the containing storage pool; only the handles need be exposed to
clients of the storage pool. Subpool objects are designated by subpool handles,
and are the run-time representation of a subpool.]]}

@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We know that subpool handles designate
  subpool objects because the declaration of Subpool_Handle says so.]}
@end{TheProof}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[Each subpool @i<belongs>@Defn2{Term=[belongs],Sec=[subpool to a pool]}
to a single storage pool @Redundant[(which will always be a pool
that supports subpools)]. An access to the pool that a subpool belongs to can
be obtained by calling Pool_of_Subpool with the subpool handle.
Set_Pool_of_Subpool causes the subpool of the subpool handle to belong to the
given pool@Redundant[; this is intended to be called from subpool constructors
like Create_Subpool.] Set_Pool_of_Subpool propagates Program_Error if the
subpool already belongs to a pool.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Pool_of_Subpool and Set_Pool_of_Subpool are
  provided by the Ada implementation and typically will not be overridden by the
  pool implementer.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[When an @nt{allocator} for a type whose storage pool
supports subpools is evaluated, a call is made on Allocate_From_Subpool passing
in a Subpool_Handle, in addition to the parameters as defined for calls on
Allocate (see @RefSecNum{Storage Management}). The subpool designated by the
@Syni{subpool_handle_}@nt{name} is used, if specified in an @nt{allocator}.
Otherwise, Default_Subpool_for_Pool of the Pool is used to provide a subpool
handle. All requirements on the Allocate procedure also apply to
Allocate_from_Subpool.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Deallocate_Subpool is expected to do whatever is
  needed to deallocate all of the objects contained in the subpool; it is called
  from Unchecked_Deallocate_Subpool (see @RefSecNum{Subpool Reclamation}).]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Typically, the pool implementer will not override
  Allocate. In the canonical definition of the language, it will never be called
  for a pool that supports subpools (there is an @ImplPermName below that allows
  it to be called in certain rare cases).]}
@end{Discussion}

@end{StaticSem}

@begin{Legality}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[If a storage pool that supports subpools is specified as the Storage_Pool for
an access type, the access type is called a
@i<subpool access type>.@Defn{subpool access type}@Defn2{Term=[access type],Sec=[subpool]}
A subpool access type shall be a pool-specific access type.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3],ARef=[AI05-0252-1]}
@ChgAdded{Version=[3],Text=[The accessibility level of a subpool access type
shall not be statically deeper than that of the storage pool object.
If the specified storage pool object is a storage pool that supports subpools,
then the @nt{name} that denotes the object shall not denote part of a formal
parameter, nor shall it denote part of a dereference of a value of a
non-library-level general access type.
@PDefn{generic contract issue}In addition to the places where
@LegalityTitle normally apply (see @RefSecNum{Generic Instantiation}),
these rules also apply in the private part of an instance of a generic unit.]}
@end{Legality}

@begin{Runtime}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3],ARef=[AI05-0252-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[When an access type with a specified
storage pool is frozen (see @RefSecNum{Freezing Rules}), if the tag of the
storage pool object identifies a storage pool that supports subpools, the
following checks are made:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[the @nt{name} used to specify the storage pool
  object does not denote part of a formal parameter nor part of a dereference of
  a value of a non-library-level general access type; and@IndexCheck{Accessibility_Check}]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[the accessibility level of the access type is not
  deeper than that of the storage pool object.@IndexCheck{Accessibility_Check}]}
@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0252-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Program_Error is raised if either
of these checks
fail.@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This check (and its static counterpart) ensures
  that the type of the allocated objects exists at least as long as the storage
  pool object, so that the subpools are finalized (which finalizes any remaining
  allocated objects) before the type of the objects ceases to exist. The access
  type itself (and the associated collection) will cease to exist before the
  storage pool ceases to exist.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We also disallow the use of formal parameters and
  dereferences of non-library-level general access types when specifying a
  storage pool object if it supports subpools, because the "apparent"
  accessibility level is potentially deeper than that of the underlying object.
  Neither of these cases is very likely to occur in practice.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Type=[Leading],Text=[A call to Subpools.Allocate(P, Addr, Size, Align) does the following:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Allocate_From_Subpool
  (Root_Storage_Pool_With_Subpools'Class(P),
   Addr, Size, Align,
   Subpool => Default_Subpool_for_Pool
                (Root_Storage_Pool_With_Subpools'Class(P)));]}
@end{Example}


@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[An @nt{allocator} that allocates in a subpool raises
Program_Error if the allocated object has task
parts.@IndexCheck{Allocation_Check}@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is to ease implementation. We envision
  relaxing this restriction in a future version of Ada, once implementation
  experience has been gained. At this time, we are unable to come up with a set
  of rules for task termination that is both useful, and surely feasible to
  implement.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[Unless overridden, Default_Subpool_for_Pool
propagates Program_Error.]}

@end{Runtime}

@begin{ImplPerm}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[When an allocator for a type whose storage pool is
of type Root_Storage_Pool'Class is evaluated, but supports subpools, the
implementation may call Allocate rather than Allocate_From_Subpool.
@Redundant[This will have the same effect, so long as Allocate has not been
overridden.]]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[This ensures either of two implementation models
  are possible for an @nt{allocator} with no @nt{subpool_specification}.
  Note that the "supports subpools" property is not known at compile time for a
  pool of the class-wide type.]}

  @begin{Itemize}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The implementation can dispatch to
      Storage_Pools.Allocate. If the pool supports subpools, this will call
      Allocate_From_Subpool with the default subpool so long as Allocate has not
      been overridden.]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The implementation can declare
      Allocate_From_Subpool as a primitive of Root_Storage_Pool in the private
      part of Storage_Pools. This means that the Allocate_From_Subpool for
      Root_Storage_Pool_With_Subpools overrides that private one. The
      implementation can thus call the private one, which will call Allocate for
      non-subpool-supporting pools. The effect of this implementation does not
      change if Allocate is overridden for a pool that supports subpools.]}
  @end{Itemize}
@end{Reason}

@end{ImplPerm}

@begin{Notes}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[A user-defined storage pool type that supports
subpools can be implemented by extending the Root_Storage_Pool_With_Subpools
type, and overriding the primitive subprograms Create_Subpool,
Allocate_From_Subpool, and Deallocate_Subpool. Create_Subpool should call
Set_Pool_Of_Subpool before returning the subpool handle. To make use of such a
pool, a user would declare an object of the type extension, use it to define the
Storage_Pool attribute of one or more access types, and then call Create_Subpool
to obtain subpool handles associated with the pool.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[A user-defined storage pool type that supports
subpools may define additional subpool constructors similar to Create_Subpool
(these typically will have additional parameters).]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[The pool implementor should override
Default_Subpool_For_Pool if the pool is to support a default subpool for the
pool. The implementor can override Deallocate if individual object reclamation
is to be supported, and can override Storage_Size if there is some limit on the
total size of the storage pool. The implementor can override Initialize and
Finalize if there is any need for nontrivial initialization and finalization
for the pool as a whole. For example, Finalize might reclaim blocks of storage
that are allocated over and above the space occupied by the pool object itself.
The pool implementor may extend the Root_Subpool type as necessary to carry
additional information with each subpool provided by Create_Subpool.]}

@end{Notes}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3],ARef=[AI05-0252-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}Subpools and the
  package System.Storage_Pools.Subpools are new.]}
@end{Extend2005}


@LabeledAddedSubClause{Version=[3],Name=[Subpool Reclamation]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[A subpool may be explicitly deallocated using
Unchecked_Deallocate_Subpool.]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The following language-defined
library procedure exists:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[with] System.Storage_Pools.Subpools;
@SubChildUnit{Parent=[Ada],Child=[Ada.Unchecked_Deallocate_Subpool]}@key[procedure] Ada.Unchecked_Deallocate_Subpool
   (Subpool : @key[in out] System.Storage_Pools.Subpools.Subpool_Handle);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[If Subpool is @key[null], a call on
Unchecked_Deallocate_Subpool has no effect. Otherwise, the subpool is
finalized, and Subpool is set to @key[null].]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Type=[Leading],Text=[Finalization of a subpool has the
following effects:]}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[The subpool no longer belongs to any pool;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Any of the objects allocated from the subpool that
still exist are finalized in an arbitrary order;@PDefn2{Term=[arbitrary order],Sec=[allowed]}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The following @Redundant[dispatching] call is then made:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],NoPrefix=[T],Text=[   Deallocate_Subpool(Pool_of_Subpool(Subpool).@key[all], Subpool);]}
@end{Example}
@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[Finalization of a Root_Storage_Pool_With_Subpools
object finalizes all subpools that belong to that pool that have not yet been
finalized.]}

@begin{Discussion}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[There is no need to call Unchecked_Deallocation on
an object allocated in a subpool. Such objects are deallocated all at once, when
Unchecked_Deallocate_Subpool is called.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[If Unchecked_Deallocation is called, the object is
finalized, and then Deallocate is called on the Pool, which typically will do
nothing. If it wants to free memory, it will need some way to get from the
address of the object to the subpool.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[There is no Deallocate_From_Subpool. There is
no efficient way for the implementation to determine the subpool for
an arbitrary object, and if the pool implementer can determinate that,
they can use that as part of the implementation of Deallocate.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[If Unchecked_Deallocation is not called (the usual
case), the object will be finalized when Unchecked_Deallocate_Subpool is
called.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[If that's never called, then the object will be
finalized when the Pool_With_Subpools is finalized (by permission @em it might
happen when the collection of the access type is finalized).]}

@end{Discussion}

@end{StaticSem}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}Unchecked_Deallocate_Subpool is new.]}
@end{Extend2005}


@ISOOnlyRMNewPageVer{Version=[3]}@Comment{For ISO version of Ada 2012 Standard}
@LabeledAddedSubClause{Version=[3],Name=[Storage Subpool Example]}

@begin{Examples}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The following example is a simple but
complete implementation of the classic Mark/Release pool using subpools:]}

@begin{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[with] System.Storage_Pools.Subpools;
@key[with] System.Storage_Elements;
@key[with] Ada.Unchecked_Deallocate_Subpool;
@key[package] MR_Pool @key[is]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[use] System.Storage_Pools;
      -- @Examcom{For uses of Subpools.}
   @key[use] System.Storage_Elements;
      -- @Examcom{For uses of Storage_Count and Storage_Array.}]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   -- @Examcom{Mark and Release work in a stack fashion, and allocations are not allowed}
   -- @Examcom{from a subpool other than the one at the top of the stack. This is also}
   -- @Examcom{the default pool.}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[subtype] Subpool_Handle @key[is] Subpools.Subpool_Handle;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] Mark_Release_Pool_Type (Pool_Size : Storage_Count) @key[is new]
      Subpools.Root_Storage_Pool_With_Subpools @key[with private];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] Mark (Pool : @key[in out] Mark_Release_Pool_Type)
      @key[return not null] Subpool_Handle;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] Release (Subpool : @key[in out] Subpool_Handle) @key[renames]
      Ada.Unchecked_Deallocate_Subpool;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[private]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] MR_Subpool @key[is new] Subpools.Root_Subpool @key[with record]
      Start : Storage_Count;
   @key[end record];
   @key[subtype] Subpool_Indexes @key[is] Positive @key[range] 1 .. 10;
   @key[type] Subpool_Array @key[is array] (Subpool_Indexes) @key[of aliased] MR_Subpool;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] Mark_Release_Pool_Type (Pool_Size : Storage_Count) @key[is new]
      Subpools.Root_Storage_Pool_With_Subpools @key[with record]
      Storage         : Storage_Array (1 .. Pool_Size);
      Next_Allocation : Storage_Count := 1;
      Markers         : Subpool_Array;
      Current_Pool    : Subpool_Indexes := 1;
   @key[end record];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[overriding]
   @key[function] Create_Subpool (Pool : @key[aliased in out] Mark_Release_Pool_Type)
      @key[return not null] Subpool_Handle;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] Mark (Pool : @key[in out] Mark_Release_Pool_Type)
      @key[return not null] Subpool_Handle @key[renames] Create_Subpool;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[overriding]
   @key[procedure] Allocate_From_Subpool (
      Pool : @key[in out] Mark_Release_Pool_Type;
      Storage_Address : @key[out] System.Address;
      Size_In_Storage_Elements : @key[in] Storage_Count;
      Alignment : @key[in] Storage_Count;
      Subpool : @key[not null] Subpool_Handle);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[overriding]
   @key[procedure] Deallocate_Subpool (
      Pool : @key[in out] Mark_Release_Pool_Type;
      Subpool : @key[in out] Subpool_Handle);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[overriding]
   @key[function] Default_Subpool_for_Pool (
      Pool : @key[in] Mark_Release_Pool_Type) @key[return not null] Subpool_Handle;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[overriding]
   @key[procedure] Initialize (Pool : @key[in out] Mark_Release_Pool_Type);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   -- @Examcom{We don't need Finalize.}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[end] MR_Pool;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[package body] MR_Pool @key[is]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] Initialize (Pool : @key[in out] Mark_Release_Pool_Type) @key[is]
      -- @Examcom{Initialize the first default subpool.}
   @key[begin]
      Pool.Markers(1).Start := 1;
      Subpools.Set_Pool_of_Subpool
         (Pool.Markers(1)'Unchecked_Access,
          Pool'Unchecked_Access);
   @key[end] Initialize;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] Create_Subpool (Pool : @key[in out] Mark_Release_Pool_Type)
      @key[return not null] Subpool_Handle @key[is]
      -- @Examcom{Mark the current allocation location.}
   @key[begin]
      @key[if] Pool.Current_Pool = Subpool_Indexes'Last @key[then]
         @key[raise] Storage_Error; -- @Examcom{No more subpools.}
      @key[end if];
      Pool.Current_Pool := Pool.Current_Pool + 1; -- @Examcom{Move to the next subpool}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      @key[return] Result : @key[constant not null] Subpool_Handle :=
         Pool.Markers(Pool.Current_Pool)'Unchecked_Access
      @key[do]
         Result.Start := Pool.Next_Allocation;
         Subpools.Set_Pool_of_Subpool (Result, Pool'Unchecked_Access);
      @key[end return];
   @key[end] Create_Subpool;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] Deallocate_Subpool (
      Pool : @key[in out] Mark_Release_Pool_Type;
      Subpool : @key[in out] Subpool_Handle) @key[is]
   @key[begin]
      @key[if] Subpool /= Pool.Markers(Pool.Current_Pool)'Unchecked_Access @key[then]
         @key[raise] Program_Error; -- @Examcom{Only the last marked subpool can be released.}
      @key[end if];
      @key[if] Pool.Current_Pool /= 1 @key[then]
         Pool.Next_Allocation := Pool.Markers(Pool.Current_Pool);
         Pool.Current_Pool := Pool.Current_Pool - 1; -- @Examcom{Move to the previous subpool}
      @key[else] -- @Examcom{Reinitialize the default subpool:}
         Pool.Next_Allocation := 1;
         Subpools.Set_Pool_of_Subpool
            (Pool.Markers(1)'Unchecked_Access,
             Pool'Unchecked_Access);
      @key[end if];
   @key[end] Deallocate_Subpool;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] Default_Subpool_for_Pool (
      Pool : @key[in] Mark_Release_Pool_Type) @key[return not null] Subpool_Handle @key[is]
   @key[begin]
      @key[return] Pool.Markers(Pool.Current_Pool)'Unchecked_Access;
   @key[end] Default_Subpool_for_Pool;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] Allocate_From_Subpool (
      Pool : @key[in out] Mark_Release_Pool_Type;
      Storage_Address : @key[out] System.Address;
      Size_In_Storage_Elements : @key[in] Storage_Count;
      Alignment : @key[in] Storage_Count;
      Subpool : @key[not null] Subpool_Handle) @key[is]
   @key[begin]
      @key[if] Subpool /= Pool.Markers(Pool.Current_Pool)'Unchecked_Access @key[then]
         @key[raise] Program_Error; -- @Examcom{Only the last marked subpool can be used for allocations.}
      @key[end if];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[      -- @Examcom{Correct the alignment if necessary:}
      Pool.Next_Allocation := Pool.Next_Allocation +
         ((-Pool.Next_Allocation) @key[mod] Alignment);
      @key[if] Pool.Next_Allocation + Size_In_Storage_Elements >
         Pool.Pool_Size @key[then]
         @key[raise] Storage_Error; -- @Examcom{Out of space.}
      @key[end if];
      Storage_Address := Pool.Storage (Pool.Next_Allocation)'Address;
      Pool.Next_Allocation :=
         Pool.Next_Allocation + Size_In_Storage_Elements;
   @key[end] Allocate_From_Subpool;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[end] MR_Pool;]}
@end{Example}

@end{Examples}


@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
  @ChgAdded{Version=[3],Text=[This example of subpools is new.]}
@end{DiffWord2005}



@LabeledRevisedClause{Version=[3],New=[Pragma Restrictions and Pragma Profile],Old=[Pragma Restrictions]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0246-1]}
@redundant[A @nt{pragma} Restrictions expresses the user's intent to abide by
certain restrictions.@Chg{Version=[3],New=[ A @nt{pragma} Profile expresses the
user's intent to abide by a set of Restrictions or other specified run-time
policies.],Old=[]}
@Chg{Version=[3],New=[These],Old=[This]} may facilitate the construction of
simpler run-time environments.]
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
@Leading@keepnext@;The form of a @nt{pragma} Restrictions is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Restrictions)(@Syn2{restriction}{, @Syn2{restriction}});'

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00381-01]}
@Syn{lhs=(restriction), rhs="@SynI{restriction_}@Syn2{identifier}
    | @SynI{restriction_parameter_}@Syn2{identifier} => @Chg{Version=[2],New=[@Syn2{restriction_parameter_argument}],Old=[@Syn2{expression}]}"}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00381-01]}
@AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=<restriction_parameter_argument>,Old=<>}>,
rhs="@Chg{Version=[2],New=<@Syn2{name} | @Syn2{expression}>,Old=<>}"}

@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type],
  Sec=(restriction parameter expression)}
Unless otherwise specified for a particular restriction,
the @nt{expression} is expected to be of any integer type.
@end{Resolution}

@begin{Legality}
Unless otherwise specified for a particular restriction,
the @nt{expression} shall be static,
and its value shall be nonnegative.
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00394-01]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0269-1]}
@ChgDeleted{Version=[3],Text=[The set of
@Chg{Version=[2],New=[restrictions],Old=[@ntf{restrictions}]} is
implementation defined.]}
@ChgImplDef{Version=[3],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[3],Text=[The set of
@Chg{Version=[2],New=[restrictions],Old=[@ntf{restrictions}]}
allowed in a @nt{pragma} Restrictions.]}]}
@end{StaticSem}
@begin{NotIso}
@ChgAdded{Version=[3],Noparanum=[T],Text=[@Shrink{@i<Paragraph 7 was
deleted.>}]}@Comment{This message should be deleted if the paragraphs
are ever renumbered.}
@end{NotIso}

@begin{LinkTime}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0013-1]}
@ChgNote{Use ChgAdded to get conditional Leading}@ChgAdded{Version=[3],
Type=[Leading],Text=[]}@PDefn2{Term=[configuration pragma], Sec=(Restrictions)}
@PDefn2{Term=[pragma, configuration], Sec=(Restrictions)}
A @nt{pragma} Restrictions is a configuration pragma@Chg{Version=[3],
New=[. If a @nt{pragma} Restrictions applies to any compilation unit included in
the partition, this may impose either (or both) of two kinds of
requirements, as],
Old=[; unless otherwise]} specified for @Chg{Version=[3],New=[the],Old=[a]}
particular restriction@Chg{Version=[3],New=[:],Old=[,
a partition shall obey the restriction
if a @nt{pragma} Restrictions applies to any compilation unit
included in the partition.]}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0013-1]}
@ChgAdded{Version=[3],Text=[A restriction may impose requirements on some or
all of the units comprising the partition. Unless otherwise specified for a
particular restriction, such a requirement applies to all of the units
comprising the partition and is enforced via a post-compilation check.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0013-1]}
@ChgAdded{Version=[3],Text=[A restriction may impose requirements on the
run-time behavior of the program, as indicated by the specification of run-time
behavior associated with a violation of the requirement.]}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[In this latter case, there is no post-compilation
check needed for the requirement.]}
@end{Ramification}
@end{Itemize}


@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0042],ARef=[AI95-00130-01]}
@ChgAdded{Version=[1],Type=[Leading],Text=[For the purpose of checking whether
a partition contains constructs that violate any restriction (unless specified
otherwise for a particular restriction):]}

@begin{itemize}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0042],ARef=[AI95-00130-01]}
@ChgAdded{Version=[1],Text=[Generic instances are logically expanded at the
point of instantiation;]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0042],ARef=[AI95-00130-01]}
@ChgAdded{Version=[1],Text=[If an object of a type is declared or allocated and
not explicitly initialized, then all expressions appearing in the definition
for the type and any of its ancestors are presumed to be used;]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0042],ARef=[AI95-00130-01]}
@ChgAdded{Version=[1],Text=[A @nt{default_expression} for a formal parameter or
a generic formal object is considered to be used if and only if the
corresponding actual parameter is not provided in a given call or
instantiation.]}
@end{itemize}
@end{LinkTime}

@begin{ImplPerm}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[An implementation may provide implementation-defined
restrictions; the identifier for an implementation-defined restriction
shall differ from those of the language-defined restrictions.]}
@ChgImplDef{Version=[3],Kind=[Added],
Text=[@ChgAdded{Version=[3],Text=[Implementation-defined restrictions
allowed in a @nt{pragma} Restrictions.]}]}

An implementation may place limitations on the values of the
@nt{expression} that are supported,
and limitations on the supported combinations of restrictions.
The consequences of violating such limitations are
implementation defined.
@ImplDef{The consequences of violating limitations on
Restrictions @nt{pragma}s.}
@begin{Ramification}
Such limitations may be enforced at compile time or at run time.
Alternatively, the implementation is allowed to declare violations of
the restrictions to be erroneous, and not enforce them at all.
@end{Ramification}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0042],ARef=[AI95-00130-01]}
@ChgAdded{Version=[1],Text=[An implementation is permitted to omit restriction
checks for code that is recognized at compile time to be unreachable and for
which no code is generated.]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0043],ARef=[AI95-00190-01]}
@ChgAdded{Version=[1],Text=[Whenever enforcement of a restriction is not
required prior to execution, an implementation may nevertheless enforce the
restriction prior to execution of a partition to which the restriction applies,
provided that every execution of the partition would violate the restriction.]}
@end{ImplPerm}

@begin{Syntax}
@begin{SyntaxText}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI95-00249-01],ARef=[AI05-0246-1]}
@ChgAdded{Version=[3],Type=[Leading],Keepnext=[T],Text=[The form of a
@nt{pragma} Profile is as follows:]}
@end{SyntaxText}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@AddedPragmaSyn{Version=[3],@ChgAdded{Version=[3],
Text=[@key{pragma} @prag<Profile> (@SynI{profile_}@Syn2{identifier} {, @SynI{profile_}@Syn2{pragma_argument_association}});]}}

@end{Syntax}

@begin{Legality}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI95-00249-01],ARef=[AI05-0246-1]}
@ChgAdded{Version=[3],Text=[The @SynI{profile_}@nt{identifier} shall be the name
of a usage profile. The semantics of any
@SynI{profile_}@nt{pragma_@!argument_@!association}s are defined by
the usage profile specified by the @SynI{profile_}@nt{identifier}.]}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI95-00249-01],ARef=[AI05-0246-1]}
@ChgAdded{Version=[3],Text=[A profile is equivalent to the set of configuration
pragmas that is defined for each usage profile.]}
@end{StaticSem}

@begin{Linktime}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI95-00249-01]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=[configuration pragma], Sec=(Profile)}
@PDefn2{Term=[pragma, configuration], Sec=(Profile)}
A @nt{pragma} Profile is a configuration pragma.
There may be more than one @nt{pragma} Profile for a partition.]}
@end{Linktime}

@begin{ImplPerm}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[An implementation may provide implementation-defined
usage profiles; the identifier for an implementation-defined usage profile
shall differ from those of the language-defined usage profiles.]}

@ChgImplDef{Version=[3],Kind=[Added],
Text=[@ChgAdded{Version=[3],Text=[Implementation-defined usage profiles
allowed in a @nt{pragma} Profile.]}]}
@end{ImplPerm}

@begin{Notes}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00347-01]}
Restrictions intended to facilitate the construction of
efficient tasking run-time systems are defined
in @RefSecNum{Tasking Restrictions}.
@Chg{Version=[2],New=[Restrictions intended for use when constructing
high integrity systems],Old=[Safety- and security-related
restrictions]} are defined in
@RefSecNum{High Integrity Restrictions}.

An implementation has to enforce the restrictions in cases where
enforcement is required,
even if it chooses not to take advantage of the restrictions in terms of
efficiency.
@begin{Discussion}
It is not the intent that an implementation will support a different
run-time system for every possible combination of restrictions.
An implementation might support only two run-time systems,
and document a set of restrictions that is sufficient to allow
use of the more efficient and safe one.
@end{Discussion}
@end{Notes}

@begin{Extend83}
  @Defn{extensions to Ada 83}
  Pragma Restrictions is new to Ada 95.
@end{Extend83}

@begin{Extend95}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI95-00249-01],ARef=[AI05-0246-1]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @nt{Pragma} Profile is new; it was moved here by Ada 2012 and renamed to
  a "usage profile" but was otherwise unchanged.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0042],ARef=[AI95-00130-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the wording so that
  restrictions are checked inside of generic instantiations and in default
  expressions. Since not making these checks would violate the purpose of
  restrictions, we are not documenting this as an incompatibility.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0043],ARef=[AI95-00190-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added a permission that
  restrictions can be enforced at compile-time. While this is technically
  incompatible, documenting it as such would be unnecessarily alarming -
  there should not be any programs depending on the runtime failure of
  restrictions.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00381-01]}
  @ChgAdded{Version=[2],Text=[The syntax of a @nt{restriction_parameter_argument}
  has been defined to better support restriction No_Dependence (see
  @RefSecNum{Language-Defined Restrictions and Profiles}).]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0013-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction>: When restrictions are
  checked has been clarified.]}
@end{DiffWord2005}


@LabeledRevisedSubClause{Version=[3],InitialVersion=[2],New=[Language-Defined Restrictions and Profiles],Old=[Language-Defined Restrictions]}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00257-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The following
@SynI{restriction_}@nt{identifier}s are language defined (additional
restrictions are defined in the Specialized Needs Annexes):]}

@begin{Description}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0241-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[restrictions],
   Sec=(No_Implementation_Aspect_Specifications)}@Chg{Version=[3],New=[@Defn{No_Implementation_Aspect_Specifications restriction}],
   Old=[]}No_Implementation_Aspect_Specifications @\There
   are no implementation-defined aspects specified by an
   @nt{aspect_specification}. This restriction applies only to the current
   compilation or environment, not the entire partition.]}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0241-1]}
  @ChgAdded{Version=[3],Text=[This restriction (as well as others below) applies
  only to the current compilation, because it is likely that the runtime (and
  possibly user-written low-level code) will need to use implementation-defined
  aspects. But a partition-wide restriction applies everywhere, including the
  runtime.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00257-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[restrictions],
   Sec=(No_Implementation_Attributes)}@Chg{Version=[3],New=[@Defn{No_Implementation_Attributes restriction}],
   Old=[]}No_Implementation_Attributes @\There
   are no implementation-defined attributes. This restriction applies
   only to the current compilation or environment, not the entire partition.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0241-1]}
  @ChgDeleted{Version=[3],Text=[@Chg{Version=[2],New=[This restriction (as well as No_Implementation_Pragmas)
  only applies to the current compilation, because it is likely that the
  runtime (and possibly user-written low-level code) will need to use
  implementation-defined entities. But a partition-wide restriction applies
  everywhere, including the runtime.],Old=[]}]}
@end{Discussion}

@Comment{The end part of this and the bullets are Redundant, but that would
cause bad nesting, so we don't mark it}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0246-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[restrictions],
   Sec=(No_Implementation_Identifiers)}@Defn{No_Implementation_Identifiers restriction}No_Implementation_Identifiers
   @\There are no usage names that denote declarations with
   implementation-defined identifiers that occur within language-defined
   packages or instances of language-defined generic packages.
   Such identifiers can arise as follows:]}
@begin{Itemize}
  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The following language-defined
  packages and generic packages allow implementation-defined identifiers:]}

  @begin{InnerItemize}
    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[package System (see
      @RefSecNum{The Package System});]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[package Standard (see
      @RefSecNum{The Package Standard});]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[package Ada.Command_Line (see
      @RefSecNum{The Package Command_Line});]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[package Interfaces.C (see
      @RefSecNum{Interfacing with C and C++});]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[package Interfaces.C.Strings (see
      @RefSecNum{The Package Interfaces.C.Strings});]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[package Interfaces.C.Pointers (see
      @RefSecNum{The Generic Package Interfaces.C.Pointers});]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[package Interfaces.COBOL (see
      @RefSecNum{Interfacing with COBOL});]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[package Interfaces.Fortran (see
      @RefSecNum{Interfacing with Fortran});]}
  @end{InnerItemize}

  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[The following language-defined
  packages contain only implementation-defined identifiers:]}

  @begin{InnerItemize}
    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[package System.Machine_Code (see
    @RefSecNum{Machine Code Insertions});]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[package Ada.Directories.Information (see
    @RefSecNum{The Package Directories});]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[nested Implementation packages of the Queue
    containers (see @RefSecNum{The Generic Package Containers.Unbounded_Synchronized_Queues}-31);]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[package Interfaces (see
      @RefSecNum{The Package Interfaces});]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[package Ada.Interrupts.Names (see
      @RefSecNum{The Package Interrupts}).]}

  @end{InnerItemize}

@end{Itemize}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],NoPrefix=[T],Text=[For package Standard,
Standard.Long_Integer and Standard.Long_Float are considered language-defined
identifiers, but identifiers such as Standard.Short_Short_Integer are considered
implementation-defined.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],NoPrefix=[T],Text=[This restriction applies only to the
current compilation or environment, not the entire partition.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00257-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[restrictions],
Sec=(No_Implementation_Pragmas)}@Chg{Version=[3],
New=[@Defn{No_Implementation_Pragmas restriction}],
Old=[]}No_Implementation_Pragmas @\There are no
implementation-defined pragmas or pragma arguments. This restriction applies
only to the current compilation or environment, not the entire partition.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0242-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[restrictions],
   Sec=(No_Implementation_Units)}@Defn{No_Implementation_Units restriction}No_Implementation_Units @\There
   is no mention in the @nt{context_clause} of any implementation-defined
   descendants of packages Ada, Interfaces, or System. This restriction applies
   only to the current compilation or environment, not the entire partition.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00368-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[restrictions],
   Sec=(No_Obsolescent_Features)}@Chg{Version=[3],New=[@Defn{No_Obsolescent_Features restriction}],
   Old=[]}No_Obsolescent_Features @\There
   is no use of language features defined in Annex J. It is
   @Chg{Version=[3],New=[implementation defined whether],
   Old=[implementation-defined if]} uses of the renamings of
   @RefSecNum{Renamings of Library Units} @Chg{Version=[3],New=[and of the
   @nt{pragma}s of @RefSecNum{Aspect-related Pragmas} ],Old=[]}are detected
   by this restriction. This restriction applies only to the current
   compilation or environment, not the entire partition.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[A user could compile a rename
  like]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{with} Ada.Text_IO;
@key{package} Text_IO @key{renames} Ada.Text_IO;]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Such a rename must not be disallowed
  by this restriction, nor should the compilation of such a rename be
  restricted by an implementation. Many implementations implement the renames
  of @RefSecNum{Renamings of Library Units}
  by compiling them normally; we do not want to require implementations to use
  a special mechanism to implement these renames.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[The pragmas have the same functionality as the
  corresponding aspect (unlike the typical obsolescent feature), and rejecting
  them could be a significant portability problem for existing code.]}
@end{Reason}
@end{Description}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00381-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0241-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The following
@SynI{restriction_parameter_}@nt{identifier}@Chg{Version=[3],New=[s are],Old=[is]}
language defined:]}

@begin{Description}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00381-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[restrictions],Sec=(No_Dependence)}@Defn{No_Dependence restriction}No_Dependence @\Specifies
   a library unit on which there are no semantic dependences.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0241-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[restrictions],
   Sec=(No_Specification_of_Aspect)}@Defn{No_Specification_of_Aspect restriction}No_Specification_of_Aspect
   @\Identifies an aspect for which no
   @nt{aspect_specification}, @nt{attribute_definition_clause}, or @nt{pragma}
   is given.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0272-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[restrictions],
   Sec=(No_Use_Of_Attribute)}@Defn{No_Use_Of_Attribute restriction}No_Use_Of_Attribute
   @\Identifies an attribute for which no @nt{attribute_reference} or
   @nt{attribute_definition_clause} is given.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0272-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[restrictions],
   Sec=(No_Use_Of_Pragma)}@Defn{No_Use_Of_Pragma restriction}No_Use_Of_Pragma
   @\Identifies a @nt{pragma} which is not to be used.]}

@end{Description}
@end{StaticSem}

@begin{Legality}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00381-01]}
@ChgAdded{Version=[2],Text=[The @nt{restriction_parameter_argument} of a
No_Dependence restriction shall be a @nt{name}; the @nt{name} shall have
the form of a full expanded name of a library unit, but need not denote a unit
present in the environment.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This @nt{name} is not resolved.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0241-1]}
@ChgAdded{Version=[3],Text=[The @nt{restriction_parameter_argument} of a
No_Specification_of_Aspect restriction shall be an @nt{identifier}; this
is an identifier specific to a pragma (see @RefSecNum{Pragmas}) and
does not denote any declaration.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This @nt{restriction_parameter_argument} is not
    resolved as it is an identifier specific to a pragma. As
    for No_Dependence, there is no check that the aspect
    @nt{identifier} is meaningful; it might refer to an implementation-defined
    aspect on one implementation, but nothing at all on another implementation.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0272-1]}
@ChgAdded{Version=[3],Text=[The @nt{restriction_parameter_argument} of a
No_Use_Of_Attribute restriction
shall be an @nt{identifier} or one of the reserved words Access, Delta, Digits,
Mod, or Range; this is an identifier specific to a pragma.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This @nt{restriction_parameter_argument} is not
    resolved as it is an identifier specific to a pragma. There is no check that
    the attribute identifier refers to a known @nt{attribute_designator};
    it might refer to an implementation-defined
    attribute on one implementation, but nothing at all on another implementation.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0272-1]}
@ChgAdded{Version=[3],Text=[The @nt{restriction_parameter_argument} of a
No_Use_Of_Pragma restriction
shall be an @nt{identifier} or the reserved word Interface;
this is an identifier specific to a pragma.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This @nt{restriction_parameter_argument} is not
    resolved as it is an identifier specific to a pragma. There is no check that
    the pragma identifier refers to a known @nt{pragma};
    it might refer to an implementation-defined pragma on one implementation,
    but nothing at all on another implementation.]}
@end{Ramification}

@end{Legality}

@begin{LinkTime}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00381-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0241-1]}
@ChgAdded{Version=[2],Text=[No compilation unit included in the partition shall
depend semantically on the library unit identified by the
@nt{name}@Chg{Version=[3],New=[ of a No_Dependence restriction],Old=[]}.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[There is no requirement that the library unit
  actually exist. One possible use of the pragma is to prevent the use of
  implementation-defined units; when the program is ported to a different
  compiler, it is perfectly reasonable that no unit with the name exist.]}
@end{Ramification}

@end{LinkTime}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0246-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The following
@SynI{profile_}@nt{identifier} is language defined:]}

@begin{Description}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0246-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[profile],Sec=(No_Implementation_Extensions)}No_Implementation_Extensions @\]}

@end{Description}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0246-1]}
@ChgAdded{Version=[3],Text=[For usage profile No_Implementation_Extensions,
there shall be no @SynI{profile_}@nt{pragma_argument_association}s.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0246-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The No_Implementation_Extensions
usage profile is equivalent to the following restrictions:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[No_Implementation_Aspect_Specifications,
No_Implementation_Attributes,
No_Implementation_Identifiers,
No_Implementation_Pragmas,
No_Implementation_Units.]}
@end{Example}
@end{StaticSem}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00257-01],ARef=[AI95-00368-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Restrictions No_Implementation_Attributes, No_Implementation_Pragmas, and
  No_Obsolescent_Features are new.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00381-01]}
  @ChgAdded{Version=[2],Text=[Restriction No_Dependence is new.]}
@end{Extend95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0241-1],ARef=[AI05-0242-1],ARef=[AI05-0246-1],ARef=[AI05-0272-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Restrictions No_Implementation_Aspect_Specifications, No_Implementation_Identifiers,
  No_Implementation_Units, No_Specification_of_Aspect, No_Use_of_Attribute, and
  No_Use_of_Pragma are new.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0246-1]}
  @ChgAdded{Version=[3],Text=[Profile No_Implementation_Extensions is new.]}
@end{Extend2005}


@LabeledClause{Streams}

@begin{Intro}
@Defn{stream}
@Defn{stream type}
A @i{stream} is a sequence of elements comprising values from
possibly different types and allowing sequential access to these values.
A @i{stream type} is a type in the class whose root type is
Streams.Root_Stream_Type.
A stream type may be implemented in various ways,
such as an external sequential file, an internal buffer,
or a network channel.
@begin{Discussion}
A stream element will often be the same size as a storage element,
but that is not required.
@end{Discussion}

@ChgToGlossary{Version=[3],Kind=[Added],Term=<Stream>,
Text=<@ChgAdded{Version=[3],Text=[A stream is a sequence of elements that can be
used, along with the stream-oriented attributes, to support marshalling and
unmarshalling of values of most types.]}>}

@end{Intro}

@begin{Extend83}
@Defn{extensions to Ada 83}
Streams are new in Ada 95.
@end{Extend83}

@LabeledSubClause{The Package Streams}

@begin{StaticSem}
The abstract type Root_Stream_Type is the root type of the class of
stream types. The types in this class represent different kinds of
streams. A new stream type is defined by extending the root type
(or some other stream type), overriding the Read and Write
operations, and optionally defining additional primitive
subprograms,
according to the requirements of the particular kind of stream.
The predefined stream-oriented attributes like T'Read and T'Write
make dispatching
calls on the Read and Write procedures of the Root_Stream_Type.
(User-defined T'Read and T'Write attributes can also make such calls,
or can call the Read and Write attributes of other types.)
@begin{example}
@ChildUnit{Parent=[Ada],Child=[Streams]}@key[package] Ada.Streams @key[is]
    @key[pragma] Pure(Streams)@Defn{unpolluted};@Comment{This *must* be a Duff joke}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00161-01]}
    @key[type] @AdaTypeDefn{Root_Stream_Type} @key[is] @key[abstract tagged limited private];@Chg{Version=[2],New=[
    @key[pragma] Preelaborable_Initialization(Root_Stream_Type);],Old=[]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0044],ARef=[AI95-00181-01]}
    @key[type] @AdaTypeDefn{Stream_Element} @key[is] @key[mod] @RI{implementation-defined};
    @key[type] @AdaTypeDefn{Stream_Element_Offset} @key[is] @key[range] @RI{implementation-defined};
    @key[subtype] @AdaSubtypeDefn{Name=[Stream_Element_Count],Of=[Stream_Element_Offset]} @key[is]
        Stream_Element_Offset @key[range] 0..Stream_Element_Offset'Last;
    @key[type] @AdaTypeDefn{Stream_Element_Array} @key[is]
        @key[array](Stream_Element_Offset @key[range] <>) @key[of]@Chg{New=[@key[ aliased]],Old=[]} Stream_Element;

    @key[procedure] @AdaSubDefn{Read}(
      Stream : @key[in] @key[out] Root_Stream_Type;
      Item   : @key[out] Stream_Element_Array;
      Last   : @key[out] Stream_Element_Offset) @key[is abstract];

    @key[procedure] @AdaSubDefn{Write}(
      Stream : @key[in] @key[out] Root_Stream_Type;
      Item   : @key[in] Stream_Element_Array) @key[is abstract];

@key[private]
   ... -- @RI{not specified by the language}
@key[end] Ada.Streams;
@end{example}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00227-01]}
The Read operation transfers @Chg{Version=[2],New=[],Old=[Item'Length ]}stream
elements from the specified stream to fill the array Item.
@Chg{Version=[2],New=[Elements are transferred until Item'Length elements have
been transferred, or until the end of the stream is reached. If any elements
are transferred, the],Old=[The]} index of the last stream element transferred is
returned in Last. @Chg{Version=[2],New=[Otherwise, Item'First - 1 is returned
in Last. ],Old=[]}Last is less than Item'Last only if the end of the stream is
reached.@Comment{This last sentence should be marked Redundant.}

The Write operation appends Item to the specified stream.

@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00114-01]}
@ChgNote{This was moved from 13.7.1.}
@ChgAdded{Version=[2],Text=[The index subtype of Stream_Element_Array is
Stream_Element_Offset because we wish to allow maximum flexibility. Most
Stream_Element_Arrays will probably have a lower bound of 0 or 1, but other
lower bounds, including negative ones, make sense in some situations.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00114-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
@ChgNote{This was moved from 13.7.1, where it was totally bogus.}
@ChgAdded{Version=[2],Text=[Note that there are some language-defined
subprograms that fill part of a Stream_Element_Array, and return the index of the
last element filled as a Stream_Element_Offset. The Read procedures declared
here, Streams.Stream_IO (see @RefSecNum{The Package Streams.Stream_IO}),
and System.RPC (see @RefSecNum{Partition Communication Subsystem})
behave in this manner.
These will raise Constraint_Error if the resulting Last value is not in
Stream_Element_Offset.
This implies that the Stream_Element_Array passed to these subprograms should
not have a lower bound of Stream_Element_Offset'First,
because then a read of 0 elements would always raise Constraint_Error.
A better choice of lower bound is @Chg{Version=[3],New=[0 or ],Old=[]}1.]}
@end{Discussion}

@end{StaticSem}

@begin{ImplPerm}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0044],ARef=[AI95-00181-01]}
@ChgAdded{Version=[1],Text=[If Stream_Element'Size is not a multiple of
System.Storage_Unit, then the components of Stream_@!Element_@!Array need
not be aliased.]}
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00114-01]}
@ChgAdded{Version=[1],Text=[If the Stream_Element'Size is less than the size of
System.Storage_Unit, then components of Stream_@!Element_@!Array need not be
aliased. This is necessary as the components of type Stream_Element size might
not be addressable on the target @Chg{Version=[2],New=[architecture],Old=[architechture]}.]}
@end{Ramification}
@end{ImplPerm}

@begin{Notes}
See @RefSec{The Package Streams.Stream_IO} for an example of extending
type Root_Stream_Type.

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00227-01]}
@ChgAdded{Version=[2],Text=[If the end of stream has been reached, and
Item'First is Stream_Element_Offset'First, Read will raise Constraint_Error.]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Thus, Stream_Element_Arrays should start at 0 or
  1, not Stream_Element_Offset'First.]}
@end{Ramification}
@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00161-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @b[Amendment Correction:] Added @nt{pragma} Preelaborable_Initialization
  to type Root_Stream_Type.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0044],ARef=[AI95-00181-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Stream elements are aliased
  presuming that makes sense.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00227-01]}
  @ChgAdded{Version=[2],Text=[Fixed the wording for Read to properly define
  the result in Last when no stream elements are transfered.]}
@end{DiffWord95}


@LabeledSubClause{Stream-Oriented Attributes}

@begin{Intro}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
The @Chg{Version=[3],New=[type-related ],Old=[]}@Chg{New=[operational
attributes ],Old=[]}Write, Read, Output, and
Input @Chg{New=[],Old=[attributes ]}convert values to a
stream of elements and reconstruct values from a stream.
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00270-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For @PrefixType{every subtype S of
an elementary type @i(T)}, the following representation attribute is defined:]}
@begin{Description}

@Comment{Originally Version=[2],Kind=[Added], but we don't have a way to do both;
  also, Kind should be RevisedAdded, but that changes the attribute name to not inserted.}
@ChgAttribute{Version=[3],Kind=[Added],ChginAnnex=[T],
  Leading=<T>, Prefix=<S>, AttrName=<Stream_Size>, ARef=[AI95-00270-01], ARef=[AI05-0194-1],
  Text=[@Chg{Version=[2],New=[Denotes the number of bits
  @Chg{Version=[3],New=[read from or written to a stream by the
  default implementations of S'Read and S'Write],Old=[occupied
  in a stream by items of subtype S]}. Hence, the number of stream elements
  required per item of elementary type @i<T> is:],Old=[]}

@begin(Descexample)
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@i<T>'Stream_Size / Ada.Streams.Stream_Element'Size]}
@end(Descexample)

  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],NoPrefix=[T],Text=[The value of this attribute is of
  type @i{universal_integer} and is a multiple of Stream_Element'Size.]}]}@Comment{end attribute Stream_Size}

  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],NoPrefix=[T],Text=[Stream_Size may be specified for
  first subtypes via an @nt{attribute_definition_clause}; the @nt{expression}
  of such a clause shall be static, nonnegative, and a multiple of
  Stream_Element'Size.@Chg{Version=[3],New=[@AspectDefn{Stream_Size}],Old=[]}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Stream_Size],
    Text=[@ChgAdded{Version=[3],Text=[Size in bits used to represent elementary
      objects in a stream.]}]}

@end{Description}
@EndPrefixType{}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Stream_Size is a type-related attribute (see
  @RefSecNum{Operational and Representation Aspects}).]}
@end{Discussion}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal], ARef=[AI05-0194-1]}
  @ChgAdded{Version=[3],Text=[The value of S'Stream_Size is unaffected by the
    presence or absence of any @nt{attribute_definition_clause}s or
    @nt{aspect_specification}s specifying the Read or Write attributes of any
    ancestor of S. S'Stream_Size is defined in terms of the behavior of the
    default implementations of S'Read and S'Write even if those default
    implementations are overridden.]}
@end{Ramification}
@end{StaticSem}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00270-01]}
@ChgAdded{Version=[2],Text=[If not specified, the value of Stream_Size for an
elementary type should be the number of bits that corresponds to the
minimum number of stream elements required by the
first subtype of the type, rounded up to the nearest factor or multiple of the
word size that is also a multiple of the stream element size.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[If not specified, the value of Stream_Size for an
elementary type should be the number of bits that corresponds to the
minimum number of stream elements required by the
first subtype of the type, rounded up to the nearest factor or multiple of the
word size that is also a multiple of the stream element size.]}]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00270-01]}
  @ChgAdded{Version=[2],Text=[This is @ImplAdviceTitle because we want to
  allow implementations to remain compatible with their Ada 95 implementations,
  which may have a different handling of the number of stream elements. Users
  can always specify Stream_Size if they need a specific number of stream
  elements.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00270-01]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[@PDefn2{Term=[recommended level of support],
Sec=(Stream_Size attribute)}
The recommended level of support for the Stream_Size attribute is:]}
@begin{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00270-01]}
@ChgAdded{Version=[2],Text=[A Stream_Size clause should be supported for a
discrete or fixed point type @i<T> if the specified Stream_Size is a multiple
of Stream_Element'Size and is no less than the size of the first subtype
of @i<T>, and no greater than the size of the largest type of the same
elementary class (signed integer, modular integer, enumeration,
ordinary fixed point, or decimal fixed point).]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The recommended level of support for the Stream_Size attribute should be
followed.]}]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[There are no requirements beyond supporting
  confirming Stream_Size clauses for floating point and access types.
  Floating point and access types usually only have a handful of defined
  formats, streaming anything else makes no sense for them.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[For discrete and fixed point types, this
  may require support for sizes other than the @lquotes@;natural@rquotes
  ones. For instance, on a typical machine with 32-bit integers and a
  Stream_Element'Size of 8, setting Stream_Size to 24 must be supported.
  This is required as such formats can be useful for interoperability with
  unusual machines, and there is no difficulty with the implementation
  (drop extra bits on output, sign extend on input).]}
@end{Ramification}

@end{Itemize}
@end{ImplAdvice}

@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
@begin{StaticSem}

For @PrefixType{every subtype S of a specific type @i(T)},
the following attributes are defined.
@begin{Description}
@AttributeLeading{Prefix=<S>, AttrName=<Write>,
  Text=<S'Write denotes a procedure with the following specification:
@begin{DescExample}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00441-01]}
@key(procedure) S'Write(
   @RI{Stream} : @key{@Chg{Version=[2],New=[not null ],Old=[]}access} Ada.Streams.Root_Stream_Type'Class;
   @RI{Item} : @key{in} @RI(T))
@end{DescExample}

@noprefix@;S'Write writes the value of @i{Item} to @i{Stream}.>}

@AttributeLeading{Prefix=<S>, AttrName=<Read>,
  Text=<S'Read denotes a procedure with the following specification:
@begin{DescExample}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00441-01]}
@key(procedure) S'Read(
   @RI{Stream} : @key{@Chg{Version=[2],New=[not null ],Old=[]}access} Ada.Streams.Root_Stream_Type'Class;
   @RI{Item} : @key{out} @RI(T))
@end{DescExample}

@noprefix@;S'Read reads the value of @i{Item} from @i{Stream}.>}
@end{Description}
@EndPrefixType{}


@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0040],ARef=[AI95-00108-01]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00444-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0192-1]}
@ChgAdded{Version=[1],Text=[For @Chg{Version=[2],New=[an ],Old=[]}untagged
derived @Chg{Version=[2],New=[type],Old=[types]}, the Write
@Chg{Version=[2],New=[(resp.],Old=[and]}
Read@Chg{Version=[2],New=[) attribute is],Old=[ attributes are]}
inherited @Chg{Version=[2],New=[according to the rules given],Old=[as specified]}
in @RefSecNum{Operational and Representation Aspects}@Chg{Version=[2],
New=[ if the attribute is @Chg{Version=[3],New=[@Redundant[specified and] ],
Old=[]}available for the parent type at the point
where @i{T} is declared. For a tagged derived type, these attributes are
not inherited, but rather],Old=[; otherwise,]} the default
implementations @Chg{Version=[2],New=[],Old=[of these attributes ]}are
used.@Chg{Version=[2],New=[],Old=[The default implementations of Write and
Read attributes execute as follows:]}]}

@begin{TheProof}
  @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0192-1]}
  @ChgAdded{Version=[3],Text=[The inheritance rules of
  @RefSecNum{Operational and Representation Aspects} say that only specified or
  inherited aspects are inherited; we mention it again here as a clarification.]}
@end{TheProof}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00444-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The default implementations of
the Write and Read attributes, where available, execute as follows:]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0040],ARef=[AI95-00108-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00195-01],ARef=[AI95-00251-01],ARef=[AI95-00270-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0139-2]}
For elementary types, @Chg{Version=[2],New=[Read reads (and Write writes) the
number of stream elements implied by the Stream_Size for the type @i<T>;],
Old=[]} the representation @Chg{Version=[2],New=[],Old=[in terms ]}of@Chg{Version=[2],
New=[ those],Old=[]} stream elements is implementation defined.
For composite types, the Write or Read attribute for each component is
called in @Chg{New=[],Old=[a ]}canonical order@Chg{New=[, which],
Old=[. The canonical order of components]} is last dimension varying
fastest for an array@Chg{Version=[3],New=[ (unless the convention of the array is
Fortran, in which case it is first dimension varying fastest)],Old=[]},
and positional aggregate order for a record.
Bounds are not included in the stream if @i(T) is an array type.
If @i(T) is a discriminated type, discriminants are included only if they have
defaults. If @i(T) is a tagged type, the tag is not included.
@Chg{New=[For type extensions, the Write or Read attribute for the parent type
is called, followed by the Write or Read attribute of each component of the
extension part, in canonical order. For a limited type extension, if the
attribute of @Chg{Version=[2],New=[the parent],Old=[any ancestor]} type
@Chg{Version=[2],New=[or any progenitor type ],Old=[]}of
@i(T) @Chg{Version=[2],New=[is available anywhere within the immediate scope
of @i<T>,],Old=[has been directly specified]} and the attribute
of @Chg{Version=[2],New=[the parent type or],Old=[any ancestor type of]} the
type of any of the extension components
@Chg{Version=[2],New=[is not available at the freezing point of @i<T>, then],
Old=[which are of a limited type has not been specified,]}
the attribute of @i(T) shall be directly specified.],Old=[]}

@ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],
Text=[The @Chg{Version=[2],New=[contents
of the stream elements read and written],Old=[representation used]} by the
Read and Write attributes of elementary types@Chg{Version=[2],New=[],Old=[ in
terms of stream elements]}.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0023-1],ARef=[AI05-0264-1]}
@ChgAdded{Version=[3],Text=[If @i<T> is a discriminated type and its
discriminants have defaults, then
S'Read first reads the discriminants from the stream without modifying
@i<Item>. S'Read then creates an object of type @i<T> constrained by these
discriminants. The value of this object is then converted to the subtype
of @i<Item> and is assigned to @i<Item>. Finally, the Read attribute for each
nondiscriminant component of @i<Item> is called in canonical
order as described above. Normal default initialization and finalization
take place for the created object.]}

@begin{Reason}
  A discriminant with a default value is treated simply as
  a component of the object. On the other hand,
  an array bound or a discriminant without a default value,
  is treated as @lquotes@;descriptor@rquotes@; or @lquotes@;dope@rquotes@;
  that must be provided in order to create the object and thus is logically
  separate from the regular components. Such @lquotes@;descriptor@rquotes@;
  data are written by 'Output and produced as part of the delivered result by
  the 'Input function, but they are not written by 'Write nor read by 'Read.
  A tag is like a discriminant without a default.

  @ChgRef{Version=[1],Kind=[Added],Ref=[8652/0040],ARef=[AI95-00108-01]}
  @ChgAdded{Version=[1],Text=[For limited type extensions, we must have a
  definition of 'Read and 'Write if the parent type has one, as it is possible
  to make a dispatching call through the attributes. The rule is designed to
  automatically do the right thing in as many cases as possible.]}

  @ChgRef{Version=[1],Kind=[Added],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[1],Text=[Similarly, a type that has a progenitor
  with an available attribute must also have that attribute, for the
  same reason.]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0023-1]}
  @ChgAdded{Version=[3],Text=[The semantics of S'Read for a discriminated
  type with defaults involves an anonymous object so that the
  point of required initialization and finalization is well-defined,
  especially for objects that change shape and have controlled components.
  The creation of this anonymous object often can be omitted (see the
  @ImplPermTitle below).]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00195-01]}
  For a composite object, the subprogram denoted by the
  Write or Read attribute of each component is called, whether it is the
  default or is user-specified.@Chg{Version=[2],New=[ Implementations are
  allowed to optimize these calls (see below), presuming the properties
  of the attributes are preserved.],Old=[]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00270-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[Constraint_Error is raised by the predefined Write
attribute if the value of the elementary item is outside the range of values
representable using Stream_Size bits. For a signed integer type, an enumeration
type, or a fixed point type, the range is unsigned only if the integer code for
the lower bound of the first subtype is nonnegative, and a (symmetric) signed
range that covers all values of the first subtype would require more than
Stream_Size bits; otherwise@Chg{Version=[3],New=[,],Old=[]} the range is signed.]}


@Leading@;For @PrefixType{every subtype S'Class of a class-wide type
@i(T)'Class}:
@begin{Description}
@AttributeLeading{Prefix=<S'Class>, AttrName=<Write>,
  Text=<S'Class'Write denotes a procedure with the following
specification:
@begin{DescExample}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00441-01]}
@key(procedure) S'Class'Write(
   @RI{Stream} : @key{@Chg{Version=[2],New=[not null ],Old=[]}access} Ada.Streams.Root_Stream_Type'Class;
   @RI{Item}   : @key{in} @RI(T)'Class)
@end{DescExample}

@noprefix@;Dispatches to the subprogram denoted by the Write attribute of
the specific type identified by the tag of Item.>}

@AttributeLeading{Prefix=<S'Class>, AttrName=<Read>,
  Text=<S'Class'Read denotes a procedure with the following specification:
@begin{DescExample}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00441-01]}
@key(procedure) S'Class'Read(
   @RI{Stream} : @key{@Chg{Version=[2],New=[not null ],Old=[]}access} Ada.Streams.Root_Stream_Type'Class;
   @RI{Item} : @key{out} @RI(T)'Class)
@end{DescExample}

@noprefix@;Dispatches to the subprogram denoted by the Read attribute of
the specific type identified by the tag of Item.>}
@begin{Reason}
It is necessary to have class-wide versions of Read and Write
in order to avoid generic contract model violations;
in a generic, we don't necessarily know at compile time whether a given
type is specific or class-wide.
@end{Reason}
@end{Description}
@EndPrefixType{}
@end{StaticSem}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00270-01]}
@ChgDeleted{Version=[2],Text=[If a
stream element is the same size as a storage element,
then the normal in-memory representation should be used by Read and
Write for scalar objects.
Otherwise, Read and Write should use the smallest
number of stream elements needed
to represent all values in the base range of the scalar type.]}
@ChgNote{We don't add an Implementation Advice tag here, as we're deleting this.}
@end{ImplAdvice}
@begin{NotIso}
@ChgAdded{Version=[2],Noparanum=[T],Text=[@Shrink{@i<Paragraph 17 was
deleted.>}]}@Comment{This message should be deleted if the paragraphs
are ever renumbered.}
@end{NotIso}

@begin{StaticSem}
For @PrefixType{every subtype S of a specific type @i(T)},
the following attributes are defined.
@begin{Description}
@AttributeLeading{Prefix=<S>, AttrName=<Output>,
  Text=<S'Output denotes a procedure with the following specification:
@begin{DescExample}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00441-01]}
@key(procedure) S'Output(
   @RI{Stream} : @key{@Chg{Version=[2],New=[not null ],Old=[]}access} Ada.Streams.Root_Stream_Type'Class;
   @RI{Item} : @key{in} @RI(T))
@end{DescExample}

@noprefix@;S'Output writes the value of @i{Item} to @i{Stream}, including
any bounds or discriminants.>}
@begin{Ramification}
Note that the bounds are included even for an array type whose
first subtype is constrained.
@end{Ramification}

@AttributeLeading{Prefix=<S>, AttrName=<Input>,
  Text=<S'Input denotes a function with the following specification:
@begin{DescExample}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00441-01]}
@key(function) S'Input(
   @RI{Stream} : @key{@Chg{Version=[2],New=[not null ],Old=[]}access} Ada.Streams.Root_Stream_Type'Class)
   @key(return) @RI(T)
@end{DescExample}

@noprefix@;S'Input reads and returns one value from
@i{Stream}, using any bounds or discriminants written by a corresponding
S'Output to determine how much to read.>}
@end{Description}
@EndPrefixType{}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0040],ARef=[AI95-00108-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00444-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0192-1]}
@Chg{New=[For @Chg{Version=[2],New=[an ],Old=[]}untagged derived
@Chg{Version=[2],New=[type],Old=[types]}, the Output
@Chg{Version=[2],New=[(resp.],Old=[and]} Input@Chg{Version=[2],New=[) attribute
is],Old=[attributes of the parent type are]}
inherited @Chg{Version=[2],New=[according to the rules given],
Old=[as specified]} in
@RefSecNum{Operational and Representation Aspects}@Chg{Version=[2],
New=[ if the attribute is @Chg{Version=[3],New=[@Redundant[specified and] ],
Old=[]}available for the parent type at the point where @i{T}
is declared. For a tagged derived type, these attributes are not
inherited, but rather],Old=[; otherwise,]} the default
implementations@Chg{Version=[2],New=[],Old=[ of these
attributes]} are used.@Chg{Version=[2],New=[],Old=[ The default implementations of
Output and Input attributes execute as follows:]}],
Old=[Unless overridden by an @nt<attribute_definition_clause>, these
subprograms execute as follows:]}

@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0192-1]}
  @ChgAdded{Version=[3],Text=[See the note following the inheritance rules for
  the Write attribute, above.]}
@end{TheProof}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00444-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The default implementations of the
Output and Input attributes, where available, execute as follows:]}
@begin(Itemize)
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0269-1]}
If @i(T) is an array type, S'Output first writes the bounds,
and S'Input first reads the bounds.
If @i(T) has discriminants without defaults, S'Output first writes
the discriminants (using @Chg{Version=[3],New=[the Write attribute of the
discriminant type],Old=[S'Write]} for each), and S'Input first
reads the discriminants (using @Chg{Version=[3],New=[the Read attribute of the
discriminant type],Old=[S'Read]} for each).

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00195-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0023-1]}
S'Output then calls S'Write to write the value of @i{Item} to the stream.
S'Input then creates an object @Chg{Version=[3],New=[of type
@i<T>, ],Old=[(]}with the bounds or @Chg{Version=[3],New=[(when without
defaults) the ],Old=[]}discriminants, if any,
taken from the stream@Chg{Version=[3],New=[],Old=[)]},
@Chg{Version=[2],New=[passes],Old=[initializes]} it
@Chg{Version=[2],New=[to],Old=[with]} S'Read, and returns
the value of the object.@Chg{Version=[3],New=[ If @i<T> has discriminants, then this
object is unconstrained if and only the discriminants have
defaults.],Old=[]}@Chg{Version=[2],New=[ Normal default initialization
and finalization take place for this object (see @RefSecNum{Object Declarations},
@RefSecNum{Assignment and Finalization}, and
@RefSecNum{Completion and Finalization}).],Old=[]}
@end(Itemize)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
@ChgAdded{Version=[2],Text=[If @i<T> is an abstract type, then S'Input is
an abstract function.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[For an abstract type @i<T>, S'Input can be
  called in a dispatching call, or passed to an abstract formal
  subprogram. But it cannot be used in nondispatching
  contexts, because we don't allow objects of abstract types to exist.
  The designation of this function as abstract has no
  impact on descendants of @i<T>, as @i<T>'Input is not inherited for
  tagged types, but rather recreated (and the default
  implementation of @i<T>'Input calls @i<T>'Read, not the parent type's
  @i<T>'Input). Note that @i<T>'Input cannot be specified in this case, as any
  function with the proper profile is necessarily abstract, and specifying
  abstract subprograms in an @nt{attribute_definition_clause} is illegal.]}
@end{Ramification}

@Leading@;For @PrefixType{every subtype S'Class of a class-wide type
@i(T)'Class}:
@begin{Description}
@AttributeLeading{Prefix=<S'Class>, AttrName=<Output>,
  Text=<S'Class'Output denotes a procedure with the following
specification:
@begin{DescExample}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00441-01]}
@key(procedure) S'Class'Output(
   @RI{Stream} : @key{@Chg{Version=[2],New=[not null ],Old=[]}access} Ada.Streams.Root_Stream_Type'Class;
   @RI{Item}   : @key{in} @RI(T)'Class)
@end{DescExample}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00344-01]}
@noprefix@;First writes the external tag of @i{Item} to @i{Stream}
(by calling String'Output(@Chg{Version=[2],New=[@I{Stream}, ],Old=[]}Tags.@!External_Tag(@i{Item}'Tag)@Chg{Version=[2],New=[)],Old=[]}
@em see @RefSecNum{Tagged Types and Type Extensions})
and then dispatches to the subprogram denoted by the Output attribute of
the specific type identified by the tag.@Chg{Version=[2],New=[ Tag_Error is
raised if the tag of Item identifies a type declared at an accessibility
level deeper than that of S.],Old=[]}>}@Comment{End of S'Class'Output attribute}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00344-01]}
  @ChgAdded{Version=[2],Text=[We raise Tag_Error here for nested types as
  such a type cannot be successfully read with S'Class'Input, and it doesn't
  make sense to allow writing a value that cannot be read.]}
@end{Reason}

@AttributeLeading{Prefix=<S'Class>, AttrName=<Input>,
  Text=<S'Class'Input denotes a function with the following specification:
@begin{DescExample}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00441-01]}
@key(function) S'Class'Input(
   @RI{Stream} : @key{@Chg{Version=[2],New=[not null ],Old=[]}access} Ada.Streams.Root_Stream_Type'Class)
   @key{return} @RI(T)'Class
@end{DescExample}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00279-01],ARef=[AI95-00344-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0109-1]}
@noprefix@;First reads the external tag from @i{Stream} and determines
the corresponding internal tag
(by calling Tags.@Chg{Version=[2],New=[Descendant_Tag],
Old=[Internal_Tag]}(String'Input(@i{Stream})@Chg{Version=[2],New=[, S'Tag],Old=[]})
@Chg{Version=[2],New=[which might raise Tag_Error ],Old=[]}@em
see @RefSecNum{Tagged Types and Type Extensions})
and then dispatches to the subprogram denoted by the Input attribute of
the specific type identified by the internal tag;
returns that result.@Chg{Version=[2],New=[ If the specific type identified
by the internal tag @Chg{Version=[3],New=[],Old=[is not covered by
@i<T>'Class or ]}is abstract, Constraint_Error
is raised.],Old=[]}>}@Comment{End S'Class'Input attribute}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0109-1]}
  @ChgAdded{Version=[3],Text=[Descendant_Tag will ensure that the tag it
  returns is covered by T'Class; Tag_Error will be raised if it would not
  cover T'Class.]}
@end{Ramification}

@end{Description}
@EndPrefixType{}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00195-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0228-1]}
@IndexCheck{Range_Check}
In the default implementation of Read and Input for a composite type,
for each scalar component that is a discriminant or @Chg{Version=[3],New=[that
has an implicit initial value],Old=[whose
@nt{component_declaration} includes a @nt{default_expression}]},
a check is made that the value returned by Read for the component
belongs to its subtype.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
Constraint_Error is raised if this check fails.
For other scalar components, no check is made.
For each component that is of an access type, if the implementation can
detect that the value returned by Read for the component is not
a value of its subtype, Constraint_Error is raised. If the value
is not a value of its subtype and this error is not detected,
the component has an abnormal value, and erroneous execution
can result (see @RefSecNum{Data Validity}).@Chg{Version=[2],New=[ In the
default implementation of Read for a composite type with defaulted
discriminants, if the actual parameter of Read is constrained, a check is made
that the discriminants read from the stream are equal to those of the actual
parameter. Constraint_Error is raised if this check fails.],Old=[]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0228-1]}
  @ChgAdded{Version=[3],Text=[The check for scalar components that have an
  implicit initial value is to preserve our @MetaRulesName that all objects
  that have an implicit initial value do not become "deinitialized".]}
@end{Reason}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0228-1]}
  @ChgAdded{Version=[3],Text=[A scalar component can have an implicit initial
  value if it has a default_expression, if the component's type has the
  Default_Value aspect specified, or if the component is that of an array type
  that has the Default_Component_Value aspect specified.]}
@end{Ramification}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0228-1]}
  @ChgAdded{Version=[3],Text=[An implementation should always be able to detect
  the error for a null value read into a component of an access subtype with
  a null exclusion; the "if the implementation can detect" is intended to cover
  nonnull access values.]}
@end{Honest}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00195-01]}
@ChgAdded{Version=[2],Text=[@PDefn{unspecified}It is unspecified at which point
and in which order these checks are performed. In particular, if
Constraint_Error is raised due to the failure of one of these checks, it is
unspecified how many stream elements have been read from the stream.]}

@ChgRef{Version=[1],Kind=[AddedNormal],Ref=[8652/0045],ARef=[AI95-00132-01]}
@ChgAdded{Version=[1],Text=[@Defn2{Term=[End_Error],Sec=(raised by failure of run-time check)}
In the default implementation of Read and Input for a type, End_Error
is raised if the end of the stream is reached before the reading of a value of
the type is completed.]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0040],ARef=[AI95-00108-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00195-01],ARef=[AI95-00251-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0039-1]}
@PDefn2{Term=[specifiable], Sec=(of Read for a type)}
@PDefn2{Term=[specifiable], Sec=(of Write for a type)}
@PDefn2{Term=[specifiable], Sec=(of Input for a type)}
@PDefn2{Term=[specifiable], Sec=(of Output for a type)}
@Defn{Read clause}
@Defn{Write clause}
@Defn{Input clause}
@Defn{Output clause}
The stream-oriented attributes may be specified
for any type via an @nt{attribute_definition_clause}.
@Chg{Version=[2],New=[The subprogram name given in such a
clause shall @Chg{Version=[3],New=[statically denote a subprogram that
is not],Old=[not denote]} an abstract subprogram. Furthermore, if a
stream-oriented attribute is specified for an interface type by an
@nt{attribute_definition_clause}, the subprogram name given in the clause shall
statically denote a null procedure.],
Old=[All nonlimited types have default implementations
for these operations. An @nt{attribute_reference} for one of
these attributes is illegal if the type is limited,
unless the attribute has been specified by an
@nt{attribute_@!definition_@!clause}@Chg{New=[ or @Redundant[(for a type extension)]
the attribute has been specified for an ancestor type],Old=[]}.
For an @nt{attribute_@!definition_@!clause} specifying one of these
attributes, the subtype of the Item parameter shall be the base subtype
if scalar, and the first subtype otherwise.
The same rule applies to the result of the Input function.]}
@ChgNote{Most of the old text is moved down}
@Chg{Version=[3],New=[@AspectDefn{Read}@AspectDefn{Write}@AspectDefn{Input}@AspectDefn{Output}],Old=[]}

@begin{Reason}@ChgNote{This belongs below}
  @ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00195-01]}
  @ChgDeleted{Version=[2],Text=[This is to simplify implementation.]}
@end{Reason}

@begin{Discussion}@ChgNote{This is junk}
  @ChgRef{Version=[1],Kind=[Added],Ref=[8652/0040],ARef=[AI95-00108-01]}
  @ChgRef{Version=[2],Kind=[DeletedAdded],ARef=[AI95-00195-01]}
  @ChgDeleted{Version=[2],Text=[@Chg{Version=[1],New=[@lquotes@;Specified@rquotes
  includes inherited attributes, and default implementations are never inherited.
  So, for untagged limited types, the second part of the @nt{attribute_reference}
  rule has the same meaning as the first part. However, tagged types never inherit
  attributes, so the second rule is needed so that the default implementations
  for the attributes can be called when those are constructed from a directly
  specified ancestor.],Old=[]}]}
@end{Discussion}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[Stream attributes (other than
  Input) are always null procedures for interface types (they have no
  components). We need to allow explicit setting of the Read and Write
  attributes in order that the class-wide attributes like LI'Class'Input
  can be made available. (In that case, any descendant of the interface type
  would require available attributes.) But we don't allow any concrete
  implementation because these don't participate in extensions (unless the
  interface is the parent type). If we didn't ban concrete implementations,
  the order of declaration of a pair of interfaces would become significant.
  For example, if Int1 and Int2 are interfaces with concrete implementations
  of 'Read, then the following declarations would have different
  implementations for 'Read:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{type} Con1 @key{is new} Int1 @key{and} Int2 @key{with null record};
@key{type} Con2 @key{is new} Int2 @key{and} Int1 @key{with null record};]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Trailing],Text=[This would violate our design
  principle that the order of the specification
  of the interfaces in a @nt{derived_type_definition} doesn't matter.]}
@end{Discussion}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[The Input attribute cannot be
  specified for an interface. As it is a function, a null procedure is
  impossible; a concrete function is not possible anyway as any function
  returning an abstract type must be abstract. And we don't allow specifying
  stream attributes to be abstract subprograms. This has no impact, as the
  availability of Int'Class'Input (where Int is a limited interface) depends
  on whether Int'Read (not Int'Input) is specified. There is no reason to
  allow Int'Output to be specified, either, but there is equally no reason to
  disallow it, so we don't have a special rule for that.]}
@end{Ramification}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00195-01]}
  @ChgAdded{Version=[2],Text=[Limited types generally do not have default
  implementations of the stream-oriented attributes. The rules defining when
  a stream-oriented attribute is available (see below) determine when an
  attribute of a limited type is in fact well defined and usable. The rules are
  designed to maximize the number of cases in which the attributes are usable.
  For instance, when the language provides a default implementation of
  an attribute for a limited type based on a specified attribute for the parent
  type, we want to be able to call that attribute.]}
@end{Discussion}

@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Read],
  Text=[@ChgAdded{Version=[3],Text=[Procedure to read a value from a stream for
    a given type.]}]}

@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Write],
  Text=[@ChgAdded{Version=[3],Text=[Procedure to write a value to a stream for a
    given type.]}]}

@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Input],
  Text=[@ChgAdded{Version=[3],Text=[Function to read a value from a stream for a
    given type, including any bounds and discriminants.]}]}

@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Output],
  Text=[@ChgAdded{Version=[3],Text=[Procedure to write a value to a stream for a
    given type, including any bounds and discriminants.]}]}


@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00195-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[A stream-oriented attribute for a
subtype of a specific type @i<T> is @i<available> at places where one of the
following conditions is true: @Defn2{Term=[available],Sec=[stream attribute]}]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@i<T> is nonlimited.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The @nt{attribute_designator} is Read (resp. Write)
and @i<T> is a limited record extension, and the attribute Read (resp. Write)
is available for the parent type of @i<T> and for the types of all of the
extension components.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[In this case, the language provides a
  well-defined default implementation, which we want to be able to call.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@i{T} is a limited untagged derived type, and the
attribute was inherited for the type.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Attributes are only inherited for
  untagged derived types, and surely we want to be able to call
  inherited attributes.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The @nt{attribute_designator} is Input (resp.
Output), and @i<T> is a limited type, and the attribute Read (resp. Write) is
available for @i<T>.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The default implementation of Input and Output
  are based on Read and Write; so if the implementation of Read or Write is
  good, so is the matching implementation of Input or Output.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The attribute has been specified via an
@nt{attribute_definition_clause}, and the @nt{attribute_definition_clause}
is visible.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We always want to allow calling a specified
  attribute. But we don't want availability to break privacy.
  Therefore, only attributes whose specification can be seen count. Yes, we
  defined the visibility of an @nt{attribute_definition_clause}
  (see @RefSecNum{Visibility}).]}
@end{Reason}
@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00195-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[A stream-oriented attribute for a subtype of a class-wide type
@i<T>'Class is available at places where one of the following conditions is true:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@i<T> is nonlimited;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[the attribute has been specified via an
@nt{attribute_definition_clause}, and the @nt{attribute_definition_clause}
is visible; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[the corresponding attribute of @i<T> is
available, provided that if @i<T> has a partial view, the corresponding
attribute is available at the end of the visible part where @i<T> is
declared.]}

@end{Itemize}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The rules are stricter for class-wide attributes
  because (for the default implementation) we must ensure that any specific
  attribute that might ever be dispatched to is available. Because we require
  specification of attributes for extensions of limited parent types with
  available attributes, we can in fact know this. Otherwise, we would not be
  able to use default class-wide attributes with limited types, a significant
  limitation.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00195-01]}
@ChgAdded{Version=[2],Text=[An @nt{attribute_reference} for one of the
stream-oriented attributes is illegal unless the attribute is available at
the place of the @nt{attribute_reference}. Furthermore, an
@nt{attribute_reference} for @i<T>'Input is illegal if @i<T> is an abstract
type.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Stream attributes always exist. It is illegal
  to call them in some cases. Having the attributes not be defined for
  some limited types would seem to be a cleaner solution, but it would lead
  to contract model problems for limited private types.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[@i<T>'Input is available for abstract types
  so that @i<T>'Class'Input is available. But we certainly don't want to allow
  calls that could create an object of an abstract type. Remember that
  @i<T>'Class is never abstract, so the above legality rule doesn't apply to
  it. We don't have to discuss whether the attribute is specified, as it cannot
  be: any function returning the type would have to be abstract, and we do not
  allow specifying an attribute with an abstract subprogram.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00195-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0192-1]}
@ChgAdded{Version=[2],Text=[In the @nt{parameter_and_result_profile}s for
@Chg{Version=[3],New=[the default implementations of ],Old=[]}the
stream-oriented attributes, the subtype of the Item parameter is the base
subtype of @i<T> if @i<T> is a scalar type, and the first subtype otherwise.
The same rule applies to the result of the Input attribute.]}

@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0192-1]}
@ChgAdded{Version=[3],Text=[An inherited stream attribute has a profile as
determined by the rules for inheriting primitive subprograms (see
@RefSecNum{Operational and Representation Aspects} and
@RefSecNum{Derived Types and Classes}).]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00195-01]}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0007-1]}
@ChgAdded{Version=[2],Text=[For an @nt{attribute_definition_clause} specifying
one of these attributes, the subtype of the
@Chg{Version=[3],New=[@i<Item>],Old=[Item]} parameter shall be the
@Chg{Version=[3],New=[first subtype or the ],Old=[]}base
subtype if scalar, and the first subtype
@Chg{Version=[3],New=[if not scalar],Old=[otherwise]}. The same rule
applies to the result of the Input function.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is to simplify implementation.]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The view of the type at the point of the
  @nt{attribute_definition_clause} determines whether the @Chg{Version=[3],
  New=[],Old=[first subtype or ]}base subtype is
  @Chg{Version=[3],New=[allowed],Old=[required]}. Thus, for a scalar
  type with a partial view (which is never scalar), whether the
  @Chg{Version=[3], New=[],Old=[first subtype or the ]} base subtype is
  @Chg{Version=[3], New=[allowed],Old=[required]} is
  determined by whether the @nt{attribute_definition_clause} occurs before or
  after the full definition of the scalar type.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00366-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0065-1]}
@ChgAdded{Version=[2],Text=[@Defn{support external streaming}
@Defn2{Term=[external streaming],Sec={type supports}}
@Redundant[A type is said to @i{support external streaming} if Read and Write attributes
are provided for sending values of such a type between active
partitions, with Write marshalling the representation, and Read unmarshalling
the representation.] A limited type supports external streaming only if
it has available Read and Write attributes. A type with a part that is of
@Chg{Version=[3],New=[a nonremote],Old=[an]}
access type supports external streaming only if that access type or the type of
some part that includes the access type component, has Read and Write
attributes that have been specified via an @nt{attribute_definition_clause},
and that @nt{attribute_definition_clause} is visible. @Redundant[An anonymous
access type does not support external streaming. ]All other types
@Chg{Version=[3],New=[(including remote access types,
see @RefSecNum{Remote Types Library Units}) ],Old=[]}support
external streaming.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised]}
  @ChgAdded{Version=[2],Text=[A limited type with a part that is of
  @Chg{Version=[3],New=[a nonremote],Old=[an]} access
  type needs to satisfy both rules.]}
@end{Ramification}

@end{StaticSem}

@begin{Erron}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00279-01],ARef=[AI95-00344-01]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=(erroneous execution),Sec=(cause)}
If the internal tag returned by Descendant_Tag to T'Class'Input identifies a
type that is not library-level and whose tag has not been created, or does not
exist in the partition at the time of the call, execution is erroneous.]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The definition of Descendant_Tag prevents such
  a tag from being provided to T'Class'Input if T is a library-level type.
  However, this rule is needed for nested tagged types.]}
@end{Ramification}
@end{Erron}

@begin{ImplReq}
  @ChgRef{Version=[1],Kind=[AddedNormal],Ref=[8652/0040],ARef=[AI95-00108-01]}
  @ChgAdded{Version=[1],Text=[For every subtype @i<S> of a language-defined
  nonlimited specific type @i<T>, the output generated by S'Output or S'Write
  shall be readable by S'Input or S'Read, respectively. This rule applies
  across partitions if the implementation conforms to the Distributed Systems
  Annex.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00195-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
  @ChgAdded{Version=[2],Text=[If Constraint_Error is raised during a call to
  Read because of failure of one the above checks, the implementation
  @Chg{Version=[3],New=[shall],Old=[must]} ensure that the discriminants
  of the actual parameter of Read are not modified.]}
@end{ImplReq}

@begin{ImplPerm}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00195-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
  @ChgAdded{Version=[2],Text=[The number of calls performed by the predefined
  implementation of the stream-oriented attributes on the Read and Write
  operations of the stream type is unspecified. An implementation may take
  advantage of this permission to perform internal buffering. However, all the
  calls on the Read and Write operations of the stream type needed to implement
  an explicit invocation of a stream-oriented attribute
  @Chg{Version=[3],New=[shall],Old=[must]} take place before
  this invocation returns. An explicit invocation is one appearing explicitly
  in the program text, possibly through a generic instantiation (see
  @RefSecNum{Generic Instantiation}).]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0023-1],ARef=[AI05-0264-1]}
  @ChgAdded{Version=[3],Text=[If @i<T> is a discriminated type and its
  discriminants have defaults, then in two cases an execution of the default
  implementation of S'Read is not required to create an anonymous object of
  type @i<T>: If the discriminant values that are read in are equal to the
  corresponding discriminant values of @i<Item>, then no object of type @i<T>
  need be created and @i<Item> may be used instead. If they are not equal and
  @i<Item> is a constrained variable, then Constraint_Error may be raised at
  that point, before any further values are read from the stream and before
  the object of type @i<T> is created.]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0023-1]}
  @ChgAdded{Version=[3],Text=[A default implementation of S'Input that calls the default implementation
  of S'Read may create a constrained anonymous object with discriminants
  that match those in the stream.]}
@begin{ImplNote}
@ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Keepnext=[T],Text=[This allows
    the combined executions of S'Input and S'Read to create one
    object of type @i<T> instead of two. If this option is exercised, then:]}
@begin{Itemize}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The discriminants are read from the stream by
    S'Input, not S'Read.]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[S'Input declares an object of type @i<T>
    constrained by the discriminants read from the stream, not an
    unconstrained object.]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The discriminant values that S'Read would
    normally have read from the stream are read from Item instead.]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[The permissions of the preceding paragraph
    then apply and no object of type @i<T> need be created by the execution
    of S'Read.]}
@end{Itemize}
@end{ImplNote}

@end{ImplPerm}

@begin{Notes}
For a definite subtype S of a type @i(T), only @i(T)'Write and @i(T)'Read
are needed to pass
an arbitrary value of the subtype through a stream.
For an indefinite subtype S of a type @i(T), @i(T)'Output and @i(T)'Input
will normally be needed, since @i(T)'Write and @i(T)'Read do not
pass bounds, discriminants, or tags.

User-specified attributes of S'Class are not inherited by other
class-wide types descended from S.
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of user-defined Write attribute:}
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00441-01]}
@key[procedure] My_Write(
  Stream : @key[@Chg{Version=[2],New=[not null ],Old=[]}access] Ada.Streams.Root_Stream_Type'Class;@Chg{Version=[2],New=[
  ],Old=[]}Item@Chg{Version=[2],New=[  ],Old=[]} : My_Integer'Base);
@key(for) My_Integer'Write @key(use) My_Write;
@end{Example}
@begin{Discussion}
@leading@keepnext@i{Example of network input/output using input output attributes:}
@begin{Example}
@key(with) Ada.Streams; @key(use) Ada.Streams;
@key(generic)
    @key(type) Msg_Type(<>) @key(is private);
@key(package) Network_IO @key(is)
    --@RI[ Connect/Disconnect are used to establish the stream]
    @key(procedure) Connect(...);
    @key(procedure) Disconnect(...);

    --@RI[ Send/Receive transfer messages across the network]
    @key(procedure) Send(X : @key[in] Msg_Type);
    @key(function) Receive @key(return) Msg_Type;
@key(private)
    @key(type) Network_Stream @key(is new) Root_Stream_Type @key(with) ...
    @key(procedure) Read(...);  --@RI[ define Read/Write for Network_Stream]
    @key(procedure) Write(...);
@key(end) Network_IO;

@key(with) Ada.Streams; @key(use) Ada.Streams;
@key(package body) Network_IO @key(is)
    Current_Stream : @key(aliased) Network_Stream;
    . . .
    @key(procedure) Connect(...) @key(is) ...;
    @key(procedure) Disconnect(...) @key(is) ...;

    @key(procedure) Send(X : @key[in] Msg_Type) @key(is)
    @key(begin)
        Msg_Type'Output(Current_Stream'Access, X);
    @key(end) Send;

    @key(function) Receive @key(return) Msg_Type @key(is)
    @key(begin)
        @key(return) Msg_Type'Input(Current_Stream'Access);
    @key(end) Receive;
@key(end) Network_IO;
@end{Example}
@end{Discussion}
@end{Examples}

@begin{Inconsistent95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0040],ARef=[AI95-00108-01]}
  @ChgAdded{Version=[2],Text=[@Defn{inconsistencies with Ada 95}
  @b<Corrigendum:> Clarified how the default
  implementation for stream attributes is determined (eliminating conflicting
  language). The new wording provides that attributes for type extensions are
  created by composing the parent's attribute with those for the extension
  components if any. If a program was written assuming that the extension
  components were not included in the stream (as in original Ada 95), it
  would fail to work in the language as corrected by the Corrigendum.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00195-01]}
  @ChgAdded{Version=[2],Text=[@B[Amendment Correction:] Explicitly provided a
  permission that the number of calls to the underlying stream Read and Write
  operations may differ from the number determined by the canonical operations.
  If Ada 95 code somehow depended on the number of calls to Read or Write, it
  could fail with an Ada 2005 implementation. Such code is likely to be very
  rare; moreover, such code is really wrong, as the permission applies to Ada
  95 as well.]}
@end{Inconsistent95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00270-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The Stream_Size attribute is new. It allows specifying the number of bits
  that will be streamed for a type. The @ImplAdviceTitle involving this
  also was changed; this is not incompatible because @ImplAdviceTitle does
  not have to be followed.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0040],ARef=[AI95-00108-01],ARef=[AI95-00195-01],ARef=[AI95-00444-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Limited types may have default
  constructed attributes if all of the parent and (for extensions) extension
  components have available attributes. Ada 2005 adds the notion of
  availability to patch up some holes in the Corrigendum model.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0009],ARef=[AI95-00137-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added wording to specify that
  these are operational attributes.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0045],ARef=[AI95-00132-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that End_Error
  is raised by the default implementation of Read and Input if the end of
  the stream is reached. (The result could have been abnormal without this
  clarification, thus this is not an inconsistency, as the programmer could
  not have depended on the previous behavior.)]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00195-01]}
  @ChgAdded{Version=[2],Text=[Clarified that the default implementation of
  S'Input does normal initialization on the object that it passes to S'Read.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00195-01]}
  @ChgAdded{Version=[2],Text=[Explicitly stated that what is read from a
  stream when a required check fails is unspecified.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Text=[Defined availability and default implementations
  for types with progenitors.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00279-01]}
  @ChgAdded{Version=[2],Text=[Specified that Constraint_Error is raised if
  the internal tag retrieved for S'Class'Input is for some type not covered
  by S'Class or is abstract. We also explicitly state that the program is
  erroneous if the tag has not been created or does not currently exist in
  the partition. (Ada 95 did not specify what happened in these
  cases; it's very unlikely to have provided some useful result, so this is
  not considered an inconsistency.)]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00344-01]}
  @ChgAdded{Version=[2],Text=[Added wording to support nested type extensions.
  S'Input and S'Output always raise Tag_Error for such extensions, and such
  extensions were not permitted in Ada 95, so this is neither an extension
  nor an incompatibility.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00366-01]}
  @ChgAdded{Version=[2],Text=[Defined @i<supports external streaming> to
  put all of the rules about @lquotes@;good@rquotes stream attributes in one
  place. This is used for distribution and for defining pragma Pure.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00441-01]}
  @ChgAdded{Version=[2],Text=[Added the @key[not null] qualifier to the
  first parameter of all of the stream attributes, so that the semantics
  doesn't change between Ada 95 and Ada 2005. This change is compatible,
  because mode conformance is required for subprograms specified as
  stream attributes, and @nt{null_exclusion}s are not considered for
  mode conformance.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00444-01]}
  @ChgAdded{Version=[2],Text=[Improved the wording to make it clear that
  we don't define the default implementations of attributes that cannot be
  called (that is, aren't @lquotes@;available@rquotes@;). Also clarified when
  inheritance takes place.]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0039-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Added a requirement that stream
  attributes be specified by a static subprogram name rather than a
  dynamic expression. Expressions cannot provide any useful functionality
  because of the freezing rules, and the possibility of them complicates
  implementations. Only pathological programs should be affected.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0007-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}@b<Correction:>
  Stream attributes
  for scalar types can be specified with subprograms that take the first
  subtype as well as the base type. This eliminates confusion about which
  subtype is appropriate for attributes specified for partial views whose
  full type is a scalar type. It also eliminates a common user error
  (forgetting 'Base).]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0023-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected the definition
  of the default version S'Read and S'Input to be well-defined if
  S is a discriminated type with defaulted discriminants and
  some components require initialization and/or finalizations.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0065-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Defined remote access
  types to support external streaming, since that is their purpose.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0109-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Removed a misleading phrase
  which implies that Constraint_Error is raised for internal tags of the
  wrong type, when Tag_Error should be raised for such tags.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
  @ChgAdded{Version=[3],Text=[Clarified that arrays with convention Fortran
  are written in column-major order, rather then row-major order. This
  is necessary in order that streaming of Fortran arrays is efficient.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0192-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified that the profile
  of an inherited stream attribute is as defined for an inherited primitive
  subprogram, while the default implementation of the same attribute might
  have a different profile.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0194-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified that Stream_Size
  has no effect on and is not effected by user-defined stream attributes.]}
@end{DiffWord2005}



@ISOOnlyRMNewPageVer{Version=[3]}@Comment{For ISO version of Ada 2012 Standard}
@LabeledClause{Freezing Rules}

@begin{Intro}
@redundant[This clause defines
a place in the program text where each declared entity becomes
@lquotes@;frozen.@rquotes@;
A use of an entity, such as a reference to it by name,
or (for a type) an expression of the type,
causes freezing of the entity in some contexts,
as described below.
The @LegalityTitle forbid certain kinds of uses of an entity
in the region of text where it is frozen.]
@begin{Reason}
This concept has two purposes: a compile-time one and a run-time one.

The compile-time purpose of the freezing rules comes from the fact that
the evaluation of static expressions depends on overload resolution,
and overload resolution sometimes depends on the value of a
static expression.
(The dependence of static evaluation upon overload resolution is
obvious. The dependence in the other direction is more subtle.
There are three rules that require static expressions in contexts that
can appear in declarative places: The expression in an
@nt{attribute_designator} shall be static.
In a record aggregate, variant-controlling discriminants shall be static.
In an array aggregate with more than one named association,
the choices shall be static.
The compiler needs to know the value of these expressions in order to
perform overload resolution and legality checking.)
We wish to allow a compiler to evaluate static expressions when it sees
them in a single pass over the @nt{compilation_unit}.
The freezing rules ensure that.

The run-time purpose of the freezing rules is called the @lquotes@;linear
elaboration model.@rquotes@;
This means that declarations are elaborated in the order in which they
appear in the program text,
and later elaborations can depend on the results of earlier ones.
The elaboration of the declarations of certain entities requires run-time
information about the implementation details of other entities.
The freezing rules ensure that this information has been calculated by
the time it is used.
For example, suppose the initial value of a constant is the result of
a function call that takes a parameter of type @i(T).
In order to pass that parameter, the size of type @i(T) has to be known.
If @i(T) is composite, that size might be known only at run time.

(Note that in these discussions, words like @lquotes@;before@rquotes@; and @lquotes@;after@rquotes@;
generally refer to places in the program text,
as opposed to times at run time.)
@end{Reason}
@begin{Discussion}
@leading@;The @lquotes@;implementation details@rquotes@; we're talking
about above are:
@begin{Itemize}
For a tagged type,
the implementations of all the primitive subprograms of the type
@em that is (in the canonical implementation model),
the contents of the type descriptor, which contains pointers to the code for
each primitive subprogram.

For a type, the full type declaration of any parts (including the type itself)
that are private.

For a deferred constant,
the full constant declaration, which gives the constant's value.
(Since this information necessarily comes after the constant's type
and subtype are fully
known, there's no need to worry about its type or subtype.)

For any entity,
representation information specified by the user
via representation items.
Most representation items are for types
or subtypes; however, various other kinds of entities,
such as objects and subprograms, are possible.
@end{Itemize}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
Similar issues arise for incomplete types.
However, we do not use freezing @Chg{Version=[3],
New=[to prevent premature access],Old=[there]};
incomplete types have different, more severe, restrictions.
Similar issues also arise for subprograms, protected operations,
tasks and generic units.
However, we do not use freezing @Chg{Version=[3],
New=[to prevent premature access for those,],Old=[there]} either;
@RefSecNum{Declarative Parts} prevents problems with run-time
Elaboration_Checks.@Chg{Version=[3],New=[ Even so, freezing is used for these
entities to prevent giving representation items too late (that is, after uses
that require representation information, such as calls).],Old=[]}
@end{Discussion}
@end{Intro}

@begin{MetaRules}
An evaluable construct should freeze anything that's needed to evaluate
it.

However, if the construct is not evaluated where it appears,
let it cause freezing later, when it is evaluated.
This is the case for @nt{default_expression}s and @nt{default_name}s.
(Formal parameters, generic formal parameters, and components can have
@nt{default_expression}s or @nt{default_name}s.)

The compiler should be allowed to evaluate static expressions without
knowledge of their context. (I.e. there should not be any special
rules for static expressions that happen to occur in a context that
requires a static expression.)

Compilers should be allowed to evaluate static expressions (and record
the results) using the run-time representation of the type.
For example, suppose Color'Pos(Red) = 1, but the internal code for Red
is 37.
If the value of a static expression is Red,
some compilers might store 1 in their symbol table,
and other compilers might store 37.
Either compiler design should be feasible.

Compilers should never be required to detect erroneousness or exceptions at
compile time (although it's very nice if they do).
This implies that we should not require
code-generation for a nonstatic expression of type @i(T) too early,
even if we can prove that that expression will be erroneous,
or will raise an exception.

@Leading@;Here's an example (modified from AI83-00039, Example 3):
@begin{example}
@key[type] T @key[is]
    @key[record]
        ...
    @key[end] @key[record];
@key[function] F @key[return] T;
@key[function] G(X : T) @key[return] Boolean;
Y : Boolean := G(F); --@RI{ doesn't force T in Ada 83}
@key[for] T @key[use]
    @key[record]
        ...
    @key[end] @key[record];
@end{example}

@Leading@;AI83-00039 says this is legal.
Of course, it raises Program_Error because the function bodies aren't
elaborated yet. A one-pass compiler has to generate code for an expression of
type T before it knows the representation of T.
Here's a similar example, which AI83-00039 also says is legal:
@begin{example}
@key[package] P @key[is]
    @key[type] T @key[is] @key[private];
    @key[function] F @key[return] T;
    @key[function] G(X : T) @key[return] Boolean;
    Y : Boolean := G(F); --@RI{ doesn't force T in Ada 83}
@key[private]
    @key[type] T @key[is]
        @key[record]
            ...
        @key[end] @key[record];
@key[end] P;
@end{example}

If T's size were dynamic, that size would be stored in some
compiler-generated dope; this dope would be initialized at the place
of the full type declaration.
However, the generated code for the function calls
would most likely allocate a temp of the size specified by the dope
@i{before} checking for Program_Error.
That dope would contain uninitialized junk,
resulting in disaster.
To avoid doing that,
the compiler would have to determine,
at compile time, that the expression will raise Program_Error.

This is silly. If we're going to require compilers to detect the exception at
compile time, we might as well formulate the rule as a legality rule.

Compilers should not be required to generate code to load the value
of a variable before the address of the variable has been determined.

After an entity has been frozen, no further requirements may be
placed on its representation (such as by a representation item
or a @nt{full_type_declaration}).
@end{MetaRules}

@begin{Intro}
@RootDefn2{Term=[freezing], Sec=(entity)}
@Defn2{Term=[freezing points], Sec=(entity)}
The @i(freezing) of an entity occurs at one or more places
(@i{freezing points}) in the program
text where the representation for the entity has to be
fully determined. Each entity is frozen from its first
freezing point to the end of the program text
(given the ordering of compilation units defined in
@RefSecNum{The Compilation Process}).
@begin{Ramification}
  The @lquotes@;representation@rquotes@; for a subprogram includes its calling convention
  and means for referencing the subprogram body, either a @lquotes@;link-name@rquotes@; or
  specified address. It does
  not include the code for the subprogram body itself, nor its
  address if a link-name is used to reference the body.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0019-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[freezing], Sec=(profile)}This clause
also defines a place in the program text where the profile of each declared
callable entity becomes @i{frozen}. A use of a callable entity causes freezing
of its profile in some contexts, as described below. At the place where the
profile of a callable entity becomes frozen, the entity itself becomes
frozen.]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0014]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0017-1],ARef=[AI05-0019-1]}
@Defn2{Term=[freezing],
  Sec=(entity caused by the end of an enclosing construct)}
The end of a @nt{declarative_part}, @nt{protected_body},
or a declaration of a library package or generic library package,
causes @i(freezing) of each
entity @Chg{Version=[3],New=[and profile ],Old=[]}declared within it,
except for incomplete types.
@Defn2{Term=[freezing], Sec=(entity caused by a body)}
A noninstance body@Chg{New=[ other than a renames-as-body],Old=[]} causes
freezing of each entity @Chg{Version=[3],New=[and profile ],Old=[]}declared
before it within the same @nt{declarative_part}@Chg{Version=[3],
New=[ that is not an incomplete type; it only causes
freezing of an incomplete type if the body is within the immediate scope of the
incomplete type],Old=[]}.
@begin{Discussion}
  This is worded carefully to handle nested packages and private types.
  Entities declared in a nested @nt{package_specification}
  will be frozen by some containing construct.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0017-1]}
  An incomplete type declared in the private part of
  a library @nt{package_specification}
  can be completed in the body.@Chg{Version=[3],New=[ For other
  incomplete types (and in the bodies of library packages),
  the completion of the type will be frozen at the end of
  the package or @nt{declarative_part}, and that will freeze the
  incomplete view as well.],Old=[]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0017-1]}
@ChgAdded{Version=[3],Text=[The reason we have to worry about
  freezing of incomplete types is to prevent premature uses of
  the types in dispatching calls. Such uses may need access to
  the tag of the type, and the type has to be frozen to know
  where the tag is stored.]}
@end{Discussion}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
  The part about bodies does not say @i{immediately} within.
  A renaming-as-body does not have this property.
  Nor does @Chg{Version=[3],New=[an imported body],Old=[a @nt{pragma} Import.]}
@end{Ramification}
@begin{Reason}
  The reason bodies cause freezing is because we want
  @ntf{proper_bodies} and @nt{body_stub}s to be interchangeable @em one
  should be able to move a @nt{proper_body} to a @nt{subunit}, and
  vice-versa, without changing the semantics.
  Clearly, anything that should cause freezing should do so even if
  it's inside a @nt{proper_body}.
  However, if we make it a @nt{body_stub}, then the compiler can't
  see that thing that should cause freezing.
  So we make @nt{body_stub}s cause freezing, just in case they
  contain something that should cause freezing.
  But that means we need to do the same for @ntf{proper_bodies}.

  Another reason for bodies to cause freezing,
  there could be an added implementation burden if an entity
  declared in an enclosing @nt<declarative_part> is frozen
  within a nested body,
  since some compilers look at bodies after looking
  at the containing @nt{declarative_part}.

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0177-1]}
  @ChgAdded{Version=[3],Text=[Note that @ldquote@;body@rdquote includes
  @nt{null_procedure_declaration}s and @nt{expression_function_declaration}s
  when those are used as completions, as well as @nt{entry_body}s (see
  @RefSecNum{Completions of Declarations}). These all cause freezing,
  along with @nt{proper_body}s and @nt{body_stub}s.]}

@end{Reason}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0046],ARef=[AI95-00106-01]}
@Leading@RootDefn2{Term=[freezing], Sec=(entity caused by a construct)}
A construct that (explicitly or implicitly) references an
entity can cause the @i(freezing) of the entity, as defined by
subsequent paragraphs.
@PDefn2{Term=[freezing], Sec=(by a constituent of a construct)}
At the place where a construct causes freezing,
each @nt<name>, @Chg{New=[@nt<expression>, @nt<implicit_dereference>],
Old=[expression]}@Redundant[, or @nt{range}] within the construct causes
freezing:
@begin{Ramification}
Note that in the sense of this paragraph,
a @nt{subtype_mark} @lquotes@;references@rquotes@; the denoted subtype,
but not the type.
@end{Ramification}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0213-1]}
@PDefn2{Term=[freezing], Sec=(generic_instantiation)}
The occurrence of a @nt{generic_instantiation} causes
freezing@Chg{Version=[3],New=[, except that
a @nt{name} which is a generic actual parameter whose corresponding
generic formal parameter is a formal incomplete type
(see @RefSecNum{Formal Private and Derived Types})
does not cause freezing. In addition, if],Old=[;
also, if]} a parameter of the instantiation is defaulted,
the @nt{default_expression} or @nt{default_name} for that parameter
causes freezing.

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0213-1]}
  @ChgAdded{Version=[3],Text=[Thus, an actual parameter corresponding to a
  formal incomplete type parameter may denote an incomplete or private type
  which is not completely defined at the point of the @nt{generic_instantiation}.]}
@end{Ramification}

@PDefn2{Term=[freezing], Sec=(object_declaration)}
The occurrence of an @nt<object_declaration> that has no corresponding
completion causes freezing.
@begin{Ramification}
  Note that this does not include a @nt{formal_object_declaration}.
@end{Ramification}

@PDefn2{Term=[freezing], Sec=(subtype caused by a record extension)}
The declaration of a record extension causes freezing of the parent
subtype.
@begin{Ramification}
This combined with another rule specifying
that primitive subprogram declarations shall precede freezing
ensures that all descendants of a tagged type implement all of
its dispatching operations.

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
  The declaration of a private
  extension does not cause freezing. The freezing is deferred
  until the full type declaration, which will necessarily be
  for a record extension@Chg{Version=[2],New=[, task, or protected type (the
  latter only for a limited private extension derived from an interface)],
  Old=[]}.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
@ChgAdded{Version=[2],Text=[The declaration of a record extension,
interface type, task unit, or protected unit causes freezing of any
progenitor types specified in the declaration.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[This rule has the same purpose as the one
above: ensuring that all descendants of an interface tagged type implement all
of its dispatching operations. As with the previous rule, a private extension
does not freeze its progenitors; the full type declaration (which must have the
same progenitors) will do that.]}
@end{Reason}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[An interface type can be a parent as well as a
progenitor; these rules are similar so that the location of an interface
in a record extension does not have an effect on the freezing of the interface
type.]}
@end{Ramification}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0183-1]}
@ChgAdded{Version=[3],Text=[At the freezing point of the entity associated with
an @nt{aspect_specification}, any @nt{expression}s or @nt{name}s within the
@nt{aspect_specification} cause freezing. Any static expressions within an
@nt{aspect_specification} also cause freezing
at the end of the immediately enclosing declaration list.]}
@end{Itemize}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0046],ARef=[AI95-00106-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0177-1],ARef=[AI05-0183-1]}
@PDefn2{Term=[freezing], Sec=(by an expression)}
A static expression @Chg{Version=[3],New=[(other than within an
@nt{aspect_specification}) ],Old=[]}causes freezing where it occurs.
@Chg{New=[@PDefn2{Term=[freezing], Sec=(by an object name)}
An object name or],Old=[A]} nonstatic expression causes freezing where it
occurs, unless the @Chg{New=[name or ],Old=[]}expression is part of a
@nt<default_expression>, a @nt<default_name>, @Chg{Version=[3],New=[the
@nt{expression} of an expression function, an @nt{aspect_specification},
],Old=[]}or a per-object expression of a component's @nt{constraint}, in which
case, the freezing occurs later as part of another construct@Chg{Version=[3],New=[
or at the freezing point of an associated entity],Old=[]}.

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0046],ARef=[AI95-00106-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0019-1]}
@ChgAdded{Version=[1],Text=[@PDefn2{Term=[freezing], Sec=(by an implicit call)}
An implicit call freezes the same
entities @Chg{Version=[3],New=[and profiles ],Old=[]}that would be frozen by an
explicit call. This is true even if the implicit call is removed via
implementation permissions.]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0046],ARef=[AI95-00106-01]}
@ChgAdded{Version=[1],Text=[@PDefn2{Term=[freezing], Sec=(subtype caused by an implicit conversion)}
If an expression is implicitly converted to a type or subtype @i(T),
then at the place where the expression causes freezing, @i(T) is frozen.]}

@Leading@;The following rules define which entities are frozen at the place where
a construct causes freezing:
@begin{Itemize}
@Leading@PDefn2{Term=[freezing], Sec=(type caused by an expression)}
At the place where an expression causes freezing,
the type of the expression is frozen,
unless the expression is an enumeration literal used as a
@nt{discrete_choice} of the @nt{array_@!aggregate} of an
@nt{enumeration_@!representation_@!clause}.
@begin{Reason}
We considered making enumeration literals never cause freezing,
which would be more upward compatible,
but examples like the variant record aggregate (Discrim => Red, ...)
caused us to change our mind.
Furthermore, an enumeration literal is a static expression,
so the implementation should be allowed to represent it using its
representation.
@end{Reason}
@begin{Ramification}
@Leading@;The following pathological example was legal in Ada 83,
but is illegal in Ada 95:
@begin{Example}
@key[package] P1 @key[is]
    @key[type] T @key[is] @key[private];
    @key[package] P2 @key[is]
        @key[type] Composite(D : Boolean) @key[is]
            @key[record]
                @key[case] D @key[is]
                    @key[when] False => Cf : Integer;
                    @key[when] True  => Ct : T;
                @key[end] @key[case];
            @key[end] @key[record];
    @key[end] P2;
    X : Boolean := P2."="( (False,1), (False,1) );
@key[private]
    @key[type] T @key[is] @key[array](1..Func_Call) @key[of] Integer;
@key[end];
@end{Example}

In Ada 95, the declaration of X freezes Composite
(because it contains an expression of that type),
which in turn freezes T (even though Ct does not exist in this
particular case).
But type T is not completely defined at that point,
violating the rule that a type shall be completely defined before it
is frozen.
In Ada 83, on the other hand, there is no occurrence of the name T,
hence no forcing occurrence of T.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0019-1],ARef=[AI05-0177-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=[freezing], Sec=(profile of a function call)}
@PDefn2{Term=[freezing], Sec=(expression of an expression function by a call)}
At the place where a function call causes freezing, the profile of the function is
frozen. Furthermore, if a parameter of the call is defaulted, the
@nt{default_expression} for that parameter causes freezing. If the function call
is to an expression function, the @nt{expression} of the expression function
causes freezing.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0019-1]}
  @ChgAdded{Version=[3],Text=[This is the important rule for profile freezing: a
  call freezes the profile. That's because generating the call will need to know
  how the parameters are passed, and that will require knowing details of the
  types. Other uses of subprograms do not need to know about the parameters, and
  thus only freeze the subprogram, and not the profile.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Note that we don't
  need to consider procedure or entry calls, since a body freezes
  everything that precedes it, and the end of a declarative part freezes
  everything in the declarative part.]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0177-1]}
  @ChgAdded{Version=[3],Text=[Freezing of the @nt{expression} of an expression
  function only needs to be considered when the expression function is in the
  same compilation unit and there are no intervening bodies; the end of a
  declarative_part or library package freezes everything in it, and a body
  freezes everything declared before it.]}
@end{Ramification}


@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0019-1],ARef=[AI05-0177-1],ARef=[AI05-0296-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=[freezing], Sec=(profile of a callable entity by an instantiation)}
@PDefn2{Term=[freezing], Sec=(expression of an expression function by an instantiation)}
At the place where a @nt{generic_instantiation} causes freezing of a callable
entity, the profile of that entity is frozen
unless the formal subprogram corresponding to the callable entity has a
parameter or result of a formal untagged incomplete type;
if the callable entity is
an expression function, the @nt{expression} of the expression function causes
freezing.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Elaboration of the generic might call the actual
  for one of its formal subprograms, so we need to know the profile and
  (for an expression function) @nt{expression}.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0177-1]}
@ChgAdded{Version=[3],Text=[@PDefn2{Term=[freezing], Sec=(expression of an expression function by Access attribute)}
At the place where a use of the Access or Unchecked_Access attribute whose
@nt{prefix} denotes an expression function causes freezing, the @nt{expression}
of the expression function causes freezing.]}

@begin{Reason}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Leading],Text=[This is needed to avoid calls to
unfrozen expressions. Consider:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[package] Pack @key[is]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] Flub @key[is range] 0 .. 100;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] Foo (A : @key[in] Natural) @key[return] Natural @key[is]
      (A + Flub'Size); -- @Examcom[The expression is not frozen here.]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] Bar @key[is access function] Foo (A : @key[in] Natural) @key[return] Natural;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   P : Bar := Foo'Access; -- @Examcom[(A)]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   Val : Natural := P.@key[all](5); -- @Examcom[(B)]]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[end Pack;]}
@end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[If point (A) did not freeze the expression of Foo
  (which freezes Flub), then the call at point (B) would be depending on the
  aspects of the unfrozen type Flub. That would be bad.]}
@end{Reason}

@Leading@PDefn2{Term=[freezing], Sec=(entity caused by a name)}
At the place where a @nt<name> causes freezing,
the entity denoted by the @nt<name> is frozen, unless
the @nt<name> is a @nt<prefix> of an expanded name;
@PDefn2{Term=[freezing], Sec=(nominal subtype caused by a name)}
at the place where an object @nt{name} causes freezing, the
nominal subtype associated with the @nt<name> is frozen.
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
This only matters in the presence of deferred constants or
access types; an @nt{object_declaration} other than a
@Chg{Version=[2],New=[deferred constant declaration],Old=[@ntf{deferred_constant_declaration}]}
causes freezing of the nominal subtype, plus all component junk.

@ChgRef{Version=[1],Kind=[Deleted],Ref=[8652/0046],ARef=[AI95-00106-01]}
@ChgDeleted{Version=[1],Text=[@nt{Implicit_dereference}s are covered by
@nt{expression}.]}
@Comment{This statement is just plain wrong.}
@end{Ramification}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0046],ARef=[AI95-00106-01]}
@ChgAdded{Version=[1],Type=[Leading],Text=[@PDefn2{Term=[freezing], Sec=(subtype caused by an implicit dereference)}
At the place where an @nt{implicit_dereference} causes freezing,
the nominal subtype associated with the @nt{implicit_dereference} is frozen.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This rule ensures that X.D freezes the same
  entities that X.@key{all}.D does. Note that an @nt{implicit_dereference} is
  neither a @nt{name} nor @nt{expression} by itself, so it isn't covered by
  other rules.]}
@end{Discussion}

@Leading@Redundant[@PDefn2{Term=[freezing], Sec=(type caused by a range)}
At the place where a @nt{range} causes freezing, the type of the
@nt<range> is frozen.]
@begin{TheProof}
This is consequence of the facts that expressions freeze their type,
and the Range attribute is defined to be equivalent
to a pair of expressions separated by @lquotes@;..@rquotes@;.}
@end{TheProof}

@Leading@PDefn2{Term=[freezing],
  Sec=(designated subtype caused by an allocator)}
At the place where an @nt<allocator> causes freezing,
the designated subtype of its type is frozen.
If the type of the @nt<allocator> is a derived type,
then all ancestor types are also frozen.
@begin{Ramification}
  @nt{Allocator}s also freeze the named subtype,
  as a consequence of other rules.

  @Leading@;The ancestor types are frozen to prevent things like this:
@begin{Example}
@key[type] Pool_Ptr @key[is] @key[access] System.Storage_Pools.Root_Storage_Pool'Class;
@key[function] F @key[return] Pool_Ptr;

@key[package] P @key[is]
    @key[type] A1 @key[is] @key[access] Boolean;
    @key[type] A2 @key[is] @key[new] A1;
    @key[type] A3 @key[is] @key[new] A2;
    X : A3 := @key[new] Boolean; --@RI{ Don't know what pool yet!}
    @key[for] A1'Storage_Pool @key[use] F.all;
@key[end] P;
@end{Example}

  This is necessary because derived access types share their parent's pool.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0019-1]}
@Leading@PDefn2{Term=[freezing], Sec=(subtypes of the profile of a callable entity)}
At the place where a @Chg{Version=[3],New=[profile],Old=[callable entity]}
is frozen, each subtype of @Chg{Version=[3],New=[the],Old=[its]} profile is frozen.
If the @Chg{Version=[3],New=[corresponding ],Old=[]}callable entity is
a member of an entry family, the index subtype of the family is
frozen.@Chg{Version=[3],New=[],Old=[
@PDefn2{Term=[freezing], Sec=(function call)}
At the place where a function call
causes freezing, if a parameter of the call is defaulted,
the @nt{default_@!expression} for that parameter causes freezing.]}
@begin{Discussion}
@ChgRef{Version=[3],Kind=[Deleted]}@Comment{Moved above with rule}
@ChgDeleted{Version=[3],Text=[We don't worry about freezing for procedure calls
  or entry calls, since a body freezes everything that precedes it, and the end
  of a declarative part freezes everything in the declarative part.]}
@end{Discussion}

@Leading@PDefn2{Term=[freezing],
  Sec=[type caused by the freezing of a subtype]}
At the place where a subtype is frozen,
its type is frozen.
@PDefn2{Term=[freezing], Sec=(constituents of a full type definition)}
@PDefn2{Term=[freezing],
  Sec=(first subtype caused by the freezing of the type)}
At the place where a type is frozen, any expressions or @nt<name>s within
the full type definition cause freezing;
the first subtype, and
any component subtypes,
index subtypes, and parent subtype
of the type are frozen as well.
@PDefn2{Term=[freezing],
  Sec=(class-wide type caused by the freezing of the specific type)}
@PDefn2{Term=[freezing],
  Sec=(specific type caused by the freezing of the class-wide type)}
For a specific tagged type,
the corresponding class-wide type is frozen as well.
For a class-wide type,
the corresponding specific type is frozen as well.
@begin{Ramification}
  Freezing a type needs to freeze its first subtype in order to
  preserve the property that the subtype-specific aspects of statically
  matching subtypes are the same.

  Freezing an access type does not freeze its designated subtype.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00341-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0019-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[At the place where a specific
tagged type is frozen, the primitive subprograms of the type are
frozen.@Chg{Version=[3],New=[ At the place where a type is frozen,
any subprogram named in an @nt{attribute_definition_clause} for the type
is frozen.],Old=[]}]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We have a language design principle that all of
  the details of a specific tagged type are known at its freezing point.
  But that is only true if the primitive subprograms are frozen at this
  point as well. Late changes of Import and address clauses violate the
  principle.]}
@end{Reason}
@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This rule means that no implicit call to
  Initialize or Adjust can freeze a subprogram (the type and thus subprograms
  would have been frozen at worst at the same point).]}
@end{ImplNote}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0019-1]}
  @ChgAdded{Version=[3],Text=[The second sentence is the rule that makes
  it possible to check that only subprograms with convention Ada are
  specified in @nt{attribute_definition_clause}s without jumping through hoops.]}
@end{Discussion}
@end{Itemize}

@end{Intro}

@begin{Legality}

@Comment{"Leading" below is to get the last paragraph onto the page with everything else.}
@Leading@Redundant[The explicit declaration of a primitive subprogram of a
tagged type shall occur before the type is frozen
(see @RefSecNum{Dispatching Operations of Tagged Types}).]
@begin{Reason}
This rule is needed
because (1) we don't want people dispatching to things that haven't
been declared yet, and (2) we want to allow tagged type descriptors
to be static (allocated statically, and initialized to link-time-known
symbols). Suppose T2 inherits primitive P from T1, and then
overrides P. Suppose P is called @i{before} the declaration of the
overriding P. What should it dispatch to? If the answer is the new
P, we've violated the first principle above. If the answer is the
old P, we've violated the second principle. (A call
to the new one necessarily raises Program_Error, but that's
beside the point.)

Note that a call upon a dispatching operation of type @i(T) will freeze @i(T).

We considered applying this rule to all derived types, for uniformity.
However, that would be upward incompatible, so we rejected the idea.
As in Ada 83, for an untagged type, the above call upon P will call the
old P (which is arguably confusing).
@end{Reason}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0222-1]}
  @ChgAdded{Version=[3],Text=[This rule only applies to "original" declarations
  and not to the completion of a primitive subprogram, even though a completion
  is technically an explicit declaration, and it may declare a primitive
  subprogram.]}
@end{Honest}

@Leading@Redundant[A type shall be completely defined before it is frozen
(see @RefSecNum{Completions of Declarations} and
@RefSecNum{Private Types and Private Extensions}).]

@Leading@Redundant[The completion of a deferred constant declaration shall occur
before the constant is frozen
(see @RefSecNum{Deferred Constants}).]

@begin{TheProof}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00114-01]}
@ChgAdded{Version=[2],Text=[The above @LegalityTitle are stated
@lquotes@;officially@rquotes@; in the referenced clauses.]}
@end{TheProof}

@Leading@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@Chg{New=[An operational or],Old=[A]} representation item that
directly specifies an aspect of an entity shall appear before the entity is
frozen (see @RefSecNum{Operational and Representation Aspects}).
@ChgNote{A last minute change (requested by WG9) moved this rule to 13.1(9).
However, the rule there only covers types and subtypes. So this rule is not
redundant, and I removed the @Redundant for it. I don't have a way to mark
that as a change, so it is just gone. RLB-29-08-00}

@begin{Discussion}

@comment{The following is a "fix" to keep consistent with v. 5.95;
appearently 6.0 is different.
@ChgRef{Version=[1],Kind=[Deleted]}
@ChgDeleted{Version=[1],Text=[Old @b{Change}.]}}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
From RM83-13.1(7). The wording here forbids freezing
within the @Chg{New=[@nt{aspect_clause}],Old=[@nt{representation_clause}]}
itself, which was not true of the Ada 83 wording.
The wording of this rule is carefully written to
work properly for type-related representation items.
For example, an @nt{enumeration_@!representation_@!clause} is illegal after the
type is frozen, even though the @ntf{_clause} refers to the first subtype.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00114-01]}
@ChgAdded{Version=[2],Text=[The above @LegalityName is stated
for types and subtypes in @RefSecNum{Operational and Representation Aspects},
but the rule here covers all other entities as well.]}
@end{Discussion}
@begin{TheProof}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00114-01]}
@ChgDeleted{Version=[2],Text=[The above @LegalityTitle
are stated @lquotes@;officially@rquotes@;
in the referenced clauses]}.
@end{TheProof}
@begin{Discussion}
@Leading@;Here's an example that illustrates when freezing occurs in the
presence of defaults:
@begin{Example}
@key[type] T @key[is] ...;
@key[function] F @key[return] T;
@key[type] R @key[is]
    @key[record]
        C : T := F;
        D : Boolean := F = F;
    @key[end] @key[record];
X : R;
@end{Example}

Since the elaboration of R's declaration does not allocate component C,
there is no need to freeze C's subtype at that place.
Similarly, since the elaboration of R does not evaluate the
@nt{default_expression} @lquotes@;F = F@rquotes@;, there is no need to freeze the types
involved at that point.
However, the declaration of X @i{does} need to freeze these things.
Note that even if component C did not exist, the elaboration of the
declaration of X would still need information about T @em even though
D is not of type T,
its @nt{default_expression} requires that information.
@end{Discussion}
@begin{Ramification}
Although we define freezing in terms of the program text as a whole
(i.e. after applying the rules of Section 10),
the freezing rules actually have no effect beyond compilation unit
boundaries.
@end{Ramification}
@begin{Reason}
That is important, because Section 10 allows some
implementation definedness in the order of things,
and we don't want the freezing rules to be implementation defined.
@end{Reason}
@begin{Ramification}
These rules also have no effect in @nt{statement}s @em they only apply within
a single @nt{declarative_part}, @nt{package_specification},
@nt{task_definition},
@nt{protected_definition},
or @nt{protected_body}.
@end{Ramification}
@begin{ImplNote}
An implementation may choose to generate code for
@nt{default_expression}s and @nt{default_name}s in
line at the place of use.
@Defn{thunk}
Alternatively, an implementation may choose to generate thunks
(subprograms implicitly generated by the compiler)
for evaluation of defaults.
Thunk generation cannot, in general, be done at the place of the
declaration that includes the default.
Instead, they can be generated at the first freezing point of the
type(s) involved.
(It is impossible to write a purely one-pass Ada compiler,
for various reasons.
This is one of them @em the compiler needs to store a representation of
defaults in its symbol table, and then walk that representation later,
no earlier than the first freezing point.)

In implementation terms, the linear elaboration model can be thought of
as preventing uninitialized dope.
For example, the implementation might generate dope to contain the size
of a private type.
This dope is initialized at the place where the type becomes
completely defined.
It cannot be initialized earlier,
because of the order-of-elaboration rules.
The freezing rules prevent elaboration of earlier declarations from
accessing the size dope for a private type before it is initialized.

@RefSecNum{Pragmas} overrides the freezing rules in the case of
unrecognized @nt{pragma}s.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@Chg{New=[An @nt{aspect_clause}],Old=[A @nt{representation_clause}]} for
an entity should most certainly @i{not} be a freezing point for the entity.
@end{ImplNote}
@end{Legality}

@begin{RunTime}
@ChgNote{This is the last normative paragraph in the clause}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00279-01]}
@ChgAdded{Version=[2],Text=[The tag (see
@RefSecNum{Tagged Types and Type Extensions}) of a tagged type T
is created at the point where T is frozen.@PDefn2{Term=[creation],Sec=[of a tag]}]}
@end{RunTime}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
RM83 defines a forcing occurrence of a type as follows:
@lquotes@;A forcing occurrence is any occurrence [of the name of the type,
subtypes of the type, or types or subtypes with subcomponents of the type]
other than in a type or subtype
declaration, a subprogram specification, an entry declaration, a deferred
constant declaration, a @nt{pragma}, or a @ntf{representation_clause} for the type
itself. In any case, an occurrence within an expression is always forcing.@rquotes@;

@Leading@;It seems like the wording allows things like this:
@begin{Example}
@key[type] A @key[is] @key[array](Integer @key[range] 1..10) @key[of] Boolean;
@key[subtype] S @key[is] Integer @key[range] A'Range;
    --@RI{ not forcing for A}
@end{Example}

Occurrences within @nt{pragma}s can cause freezing in Ada 95.
(Since such @nt{pragma}s are ignored in Ada 83,
this will probably fix more bugs than it causes.)
@end{Incompatible83}

@begin{Extend83}
@Leading@;@Defn{extensions to Ada 83}
In Ada 95, @nt{generic_formal_parameter_declaration}s
do not normally freeze the entities from which they are defined.
For example:
@begin{example}
@key[package] Outer @key[is]
    @key[type] T @key[is] @key[tagged] @key[limited] @key[private];
    @key[generic]
        @key[type] T2 @key[is]
            @key[new] T @key[with] @key[private]; --@RI{ Does not freeze T}
                                --@RI{ in Ada 95.}
    @key[package] Inner @key[is]
        ...
    @key[end] Inner;
@key[private]
    @key[type] T @key[is] ...;
@key[end] Outer;
@end{example}

This is important for the usability of generics.
The above example uses the Ada 95 feature of formal derived types.
Examples using the kinds of formal parameters already allowed in
Ada 83 are well known.
See, for example, comments 83-00627 and 83-00688.
The extensive use expected for formal derived types
makes this issue even more compelling
than described by those comments.
Unfortunately, we are unable to solve the problem that
@nt{explicit_generic_actual_parameter}s cause freezing, even though a package
equivalent to the instance would not cause freezing.
This is primarily because such an equivalent package would have its
body in the body of the containing program unit, whereas an instance
has its body right there.
@end{Extend83}

@begin{DiffWord83}
The concept of freezing is based on Ada 83's concept of @lquotes@;forcing
occurrences.@rquotes@;
The first freezing point of an entity
corresponds roughly to the place of the first forcing occurrence, in Ada
83 terms.
The reason for changing the terminology is that the new rules do not
refer to any particular @lquotes@;occurrence@rquotes@; of a name of an entity.
Instead, we refer to @lquotes@;uses@rquotes@; of an entity,
which are sometimes implicit.

In Ada 83, forcing occurrences were used only in rules about
@ntf{representation_clause}s.
We have expanded the concept to cover private types,
because the rules stated in RM83-7.4.1(4) are almost identical to the
forcing occurrence rules.

@Leading@;The Ada 83 rules are changed in Ada 95 for the following reasons:
@begin{Itemize}
The Ada 83 rules do not work right for subtype-specific aspects.
In an earlier version of Ada 9X, we considered allowing representation
items to apply to subtypes other than the first subtype.
This was part of the reason for changing the Ada 83 rules.
However, now that we have dropped that functionality,
we still need the rules to be different from the Ada 83 rules.

The Ada 83 rules do not achieve the intended effect.
In Ada 83, either with or without the AIs,
it is possible to force the compiler to generate code that references
uninitialized dope, or force it to detect erroneousness and exception
raising at compile time.

It was a goal of Ada 83 to avoid uninitialized access values.
However, in the case of deferred constants, this goal was not achieved.

The Ada 83 rules are not only too weak @em they are also too strong.
They allow loopholes (as described above),
but they also prevent certain kinds of @nt{default_expression}s that are
harmless,
and certain kinds of @nt{generic_declaration}s that are both harmless
and very useful.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
Ada 83 had a case where @Chg{Version=[2],New=[an @nt{aspect_clause}],
Old=[a @nt{representation_clause}]} had a strong
effect on the semantics of the program @em 'Small.
This caused certain semantic anomalies.
There are more cases in Ada 95,
because the @Chg{Version=[2],New=[@nt{attribute_definition_clause}],
Old=[@ntf{attribute_representation_clause}]} has been generalized.
@end{Itemize}
@end{DiffWord83}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0046],ARef=[AI95-00106-01],ARef=[AI95-00341-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @b<Corrigendum:> Various freezing rules were added to fix holes in the rules.
  Most importantly, implicit calls are now freezing, which make some
  representation clauses illegal in Ada 2005 that were legal (but dubious) in
  Ada 95. @b[Amendment Correction:] Similarly, the primitive subprograms of a
  specific tagged type are frozen when the type is frozen, preventing dubious
  convention changes (and address clauses) after the freezing point. In both
  cases, the code is dubious and the workaround is easy.]}
@end{Incompatible95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0009],ARef=[AI95-00137-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added wording to specify that
  both operational and representation attributes must be specified before
  the type is frozen.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Text=[Added wording that declaring a specific
  descendant of an interface type freezes the interface type.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00279-01]}
  @ChgAdded{Version=[2],Text=[Added wording that defines when a tag is created
  for a type (at the freezing point of the type). This is used to specify
  checking for uncreated tags (see @RefSecNum{Tagged Types and Type Extensions}).]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0019-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 95}@b<Correction:>
  Separated the freezing of the profile from the rest of a subprogram, in order
  to reduce the impact of the Ada 95 incompatibility noted above. (The effects
  were much more limiting than expected.)]}
@end{Incompatible2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0017-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Reworded so that
  incomplete types with a deferred completion aren't prematurely
  frozen.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0177-1]}
  @ChgAdded{Version=[3],Text=[Added freezing rules for expression functions;
  these are frozen at the point of call, not the point of declaration, like
  default expressions.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[Added freezing rules for @nt{aspect_specification}s;
  these are frozen at the freezing point of the associated entity, not the
  point of declaration.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0213-1]}
  @ChgAdded{Version=[3],Text=[Added freezing rules for formal incomplete types;
  the corresponding actual is not frozen.]}
@end{DiffWord2005}

