@comment{ $Source: e:\\cvsroot/ARM/Source/pre_big.mss,v $ }
@comment{ $Revision: 1.1 $ $Date: 2019/04/09 04:56:52 $ $Author: randy $ }
@Part(predefbignum, Root="ada.mss")

@Comment{$Date: 2019/04/09 04:56:52 $}

@LabeledAddedSubclause{Version=[5],Name=[Big Numbers]}

@begin{Intro}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],Text=[Support is provided for integer arithmetic involving
values larger than than those supported by the target machine, and for
arbitrary-precision reals.]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],KeepNext=[T],Type=[Leading],Text=[The library package
Numerics.Big_Numbers has the following declaration:]}

@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key{package} Ada.Numerics.Big_Numbers@ChildUnit{Parent=[Ada.Numerics.Big_Numbers],Child=[Big_Integers]}
   @key{with} Pure, Nonblocking, Global => @key{null is}
   @key{subtype} @AdaSubtypeDefn{Name=[Field],Of=[Integer]} @key{is} Integer @key{range} 0 .. @RI{implementation-defined};
   @key{subtype} @AdaSubtypeDefn{Name=[Number_Base],Of=[Integer]} @key{is} Integer @key{range} 2 .. 16;
@key{end} Ada.Numerics.Big_Numbers;]}
@end{Example}

@end{StaticSem}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}
  The package Numerics.Big_Numbers is new.]}
@end{Extend2012}


@LabeledAddedSubclause{Version=[5],Name=[Big Integers]}

@begin{StaticSem}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],KeepNext=[T],Type=[Leading],Text=[The library package
Numerics.Big_Numbers.Big_Integers has the following declaration:]}

@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key{with} Ada.Streams;
@key{package} Ada.Numerics.Big_Numbers.Big_Integers@ChildUnit{Parent=[Ada.Numerics.Big_Numbers],Child=[Big_Integers]}
   @key{with} Preelaborate, Nonblocking,
        Global => @key{in out synchronized} Big_Integers
   @key{is}]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{type} @AdaTypeDefn{Optional_Big_Integer} @key{is private}
     @key{with} Default_Initial_Condition => @key{not} Is_Valid (Optional_Big_Integer),
          Integer_Literal => From_String,
          Put_Image => Put_Image;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Is_Valid} (Arg : Optional_Big_Integer) @key{return} Boolean;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{subtype} @AdaSubtypeDefn{Name=[Big_Integer],Of=[Optional_Big_Integer]} @key{is} Optional_Big_Integer
      @key{with} Dynamic_Predicate => Is_Valid (Big_Integer),
           Predicate_Failure => (@key{raise} Constraint_Error);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Invalid_Big_Integer} @key{return} Optional_Big_Integer
      @key{with} Post => @key{not} Is_Valid (Invalid_Big_Integer'Result);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} "=" (L, R : Big_Integer) @key{return} Boolean;
   @key{function} "<" (L, R : Big_Integer) @key{return} Boolean;
   @key{function} "<=" (L, R : Big_Integer) @key{return} Boolean;
   @key{function} ">" (L, R : Big_Integer) @key{return} Boolean;
   @key{function} ">=" (L, R : Big_Integer) @key{return} Boolean;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_Big_Integer} (Arg : Integer) @key{return} Big_Integer;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{subtype} @AdaSubtypeDefn{Name=[Optional_Big_Positive],Of=[Optional_Big_Integer]} @key{is} Optional_Big_Integer
      @key{with} Dynamic_Predicate => (@key{not} Is_Valid (Optional_Big_Positive))
                                 @key{or else} (Optional_Big_Positive > 0),
           Predicate_Failure => (@key{raise} Constraint_Error);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{subtype} @AdaSubtypeDefn{Name=[Optional_Big_Natural],Of=[Optional_Big_Integer]} @key{is} Optional_Big_Integer
      @key{with} Dynamic_Predicate => (@key{not} Is_Valid (Optional_Big_Natural))
                                 @key{or else} (Optional_Big_Natural >= 0),
           Predicate_Failure => (@key{raise} Constraint_Error);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{subtype} @AdaSubtypeDefn{Name=[Big_Positive],Of=[Big_Integer]} @key{is} Big_Integer
      @key{with} Dynamic_Predicate => Big_Positive > 0,
           Predicate_Failure => (@key{raise} Constraint_Error);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{subtype} @AdaSubtypeDefn{Name=[Big_Natural],Of=[Big_Integer]} @key{is} Big_Integer
      @key{with} Dynamic_Predicate => Big_Natural >= 0,
           Predicate_Failure => (@key{raise} Constraint_Error);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{In_Range} (Arg, Low, High : Big_Integer) @key{return} Boolean @key{is}
     ((Low <= Arg) @key{and} (Arg <= High));]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_Integer} (Arg : Big_Integer) @key{return} Integer
      @key{with} Pre => In_Range (Arg,
                            Low  => To_Big_Integer (Integer'First),
                            High => To_Big_Integer (Integer'Last))
                   @key{or else} (@key{raise} Constraint_Error);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{generic}
      @key{type} Int @key{is range} <>;
   @key{package} @AdaPackDefn{Signed_Conversions} @key{is}
      @key{function} @AdaSubDefn{To_Big_Integer} (Arg : Int) @key{return} Big_Integer;
      @key{function} @AdaSubDefn{From_Big_Integer} (Arg : Big_Integer) @key{return} Int
         @key{with} Pre => In_Range (Arg,
                               Low  => To_Big_Integer (Int'First),
                               High => To_Big_Integer (Int'Last))
                      @key{or else} (@key{raise} Constraint_Error);
   @key{end} Signed_Conversions;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{generic}
      @key{type} Int @key{is mod} <>;
   @key{package} @AdaPackDefn{Unsigned_Conversions} @key{is}
      @key{function} @AdaSubDefn{To_Big_Integer} (Arg : Int) @key{return} Big_Integer;
      @key{function} @AdaSubDefn{From_Big_Integer} (Arg : Big_Integer) @key{return} Int
         @key{with} Pre => In_Range (Arg,
                               Low  => To_Big_Integer (Int'First),
                               High => To_Big_Integer (Int'Last))
                      @key{or else} (@key{raise} Constraint_Error);
   @key{end} Unsigned_Conversions;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_String} (Arg : Big_Integer;
                       Width : Field := 0;
                       Base  : Number_Base := 10) @key{return} String
      @key{with} Post => To_String'Result'First = 1;
   @key{function} @AdaSubDefn{From_String} (Arg : String) @key{return} Big_Integer;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Put_Image}
     (Stream : @key{not null access} Ada.Streams.Root_Stream_Type'Class;
      Arg    : Big_Integer);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} "+" (L : Big_Integer) @key{return} Big_Integer;
   @key{function} "-" (L : Big_Integer) @key{return} Big_Integer;
   @key{function} "abs" (L : Big_Integer) @key{return} Big_Integer;
   @key{function} "+" (L, R : Big_Integer) @key{return} Big_Integer;
   @key{function} "-" (L, R : Big_Integer) @key{return} Big_Integer;
   @key{function} "*" (L, R : Big_Integer) @key{return} Big_Integer;
   @key{function} "/" (L, R : Big_Integer) @key{return} Big_Integer;
   @key{function} "mod" (L, R : Big_Integer) @key{return} Big_Integer;
   @key{function} "rem" (L, R : Big_Integer) @key{return} Big_Integer;
   @key{function} "**" (L : Big_Integer; R : Natural)
      @key{return} Big_Integer;
   @key{function} @AdaSubDefn{Min} (L, R : Big_Integer) @key{return} Big_Integer;
   @key{function} @AdaSubDefn{Max} (L, R : Big_Integer) @key{return} Big_Integer;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Greatest_Common_Divisor}
     (L, R : Big_Integer) @key{return} Big_Positive
     @key{with} Pre => (L /= 0 @key{and} R /= 0) @key{or else} (@key{raise} Constraint_Error);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key{private}
   ... -- @examcom{not specified by the language}
@key[end] Ada.Numerics.Big_Numbers.Big_Integers;]}

@end{Example}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],Text=[To_String and From_String behave analogously to the
Put and Get procedures defined in Text_IO.Integer_IO (in particular, with
respect to the interpretation of the Width and Base parameters) except that
Constraint_Error, not Data_Error, is propagated in error cases and the result of
a call To_String with a Width parameter of 0 and a nonnegative Arg parameter
does not include a leading blank. Put_Image calls To_String (passing in the
default values for the Width and Base parameters), prepends a leading blank if
the argument is nonnegative, converts that String to a Wide_Wide_String using
To_Wide_Wide_String, and writes the resulting value to the stream using
Wide_Wide_String'Write.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],Text=[The other functions have their usual mathematical
meanings.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],Text=[The type Optional_Big_Integer needs
finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}
@end{StaticSem}

@begin{ImplReq}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],Text=[No storage associated with an Optional_Big_Integer
object shall be lost upon assignment or scope exit.]}

@begin{ImplNote}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
  @ChgAdded{Version=[5],Text=[The @ldquote@;No storage ... shall be lost@rdquote
  requirement does not preclude implementation techniques such as caching or
  unique number tables.]}
@end{ImplNote}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],Text=[For purposes of determining whether predicate checks
are performed as part of default initialization, the type Optional_Big_Integer
shall be considered to have a subcomponent that has a @nt{default_expression}.]}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
  @ChgAdded{Version=[5],Type=[Leading],Text=[This means that the elaboration of]}
@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[Default_Initialized_Object : Big_Integer;]}
@end{Example}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[is required to propagate Assertion_Error.]}
@end{Ramification}
@end{ImplReq}


@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}
  The package Numerics.Big_Numbers.Big_Integers is new.]}
@end{Extend2012}


@LabeledAddedSubclause{Version=[5],Name=[Big Reals]}

@begin{StaticSem}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],KeepNext=[T],Type=[Leading],Text=[The library package
Numerics.Big_Numbers.Big_Reals has the following declaration:]}

@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key{with} Ada.Numerics.Big_Numbers.Big_Integers;
@key{with} Ada.Streams;
@key{package} Ada.Numerics.Big_Numbers.Big_Reals@ChildUnit{Parent=[Ada.Numerics.Big_Numbers],Child=[Big_Reals]}
   @key{with} Preelaborate, Nonblocking,
        Global => @key{in out synchronized} Big_Reals
   @key{is}]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{type} @AdaTypeDefn{Optional_Big_Real} @key{is private}
      @key{with} Default_Initial_Condition => @key{not} Is_Valid (Optional_Big_Real),
           Real_Literal => From_String,
           Put_Image => Put_Image;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Is_Valid} (Arg : Optional_Big_Real) @key{return} Boolean;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{No_Big_Real} @key{return} Optional_Big_Real
      @key{with} Post => @key{not} Is_Valid (No_Big_Real'Result);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{subtype} @AdaSubtypeDefn{Name=[Big_Real],Of=[Optional_Big_Real]} @key{is} Optional_Big_Real
      @key{with} Dynamic_Predicate => Is_Valid (Big_Real),
           Predicate_Failure => (@key{raise} Constraint_Error);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} "/" (Num, Den : Big_Integer) @key{return} Big_Real
      @key{with} Pre => (@key{if} Big_Integers."=" (Den, 0)
                   @key{then raise} Constraint_Error);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Numerator} (Arg : Big_Real) @key{return} Big_Integer;
   @key{function} @AdaSubDefn{Denominator} (Arg : Big_Real) @key{return} Big_Positive
      @key{with} Post =>
        (Arg = 0.0) @key{or else}
        (Big_Integers.Greatest_Common_Divisor
           (Numerator (Arg), Denominator'Result) = 1);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_Big_Real} (Arg : Big_Integer) @key{return} Big_Real @key{is}
      (Arg / 1);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_Real} (Arg : Integer) @key{return} Big_Real @key{is}
      (Big_Integers.To_Big_Integer (Arg) / 1);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} "=" (L, R : Big_Real) @key{return} Boolean;
   @key{function} "<" (L, R : Big_Real) @key{return} Boolean;
   @key{function} "<=" (L, R : Big_Real) @key{return} Boolean;
   @key{function} ">" (L, R : Big_Real) @key{return} Boolean;
   @key{function} ">=" (L, R : Big_Real) @key{return} Boolean;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{In_Range} (Arg, Low, High : Big_Real) @key{return} Boolean @key{is}
      ((Low <= Arg) @key{and} (Arg <= High));]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{generic}
      @key{type} Num @key{is digits} <>;
   @key{package} @AdaPackDefn{Float_Conversions} @key{is}
      @key{function} @AdaSubDefn{To_Big_Real} (Arg : Num) @key{return} Big_Real;
      @key{function} @AdaSubDefn{From_Big_Real} (Arg : Big_Real) @key{return} Num
         @key{with} Pre => In_Range (Arg,
                               Low  => To_Big_Real (Num'First),
                               High => To_Big_Real (Num'Last))
                     @key{or else} (@key{raise} Constraint_Error);
   @key{end} Float_Conversions;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{generic}
      @key{type} Num @key{is delta} <>;
   @key{package} @AdaPackDefn{Fixed_Conversions} @key{is}
      @key{function} @AdaSubDefn{To_Big_Real} (Arg : Num) @key{return} Big_Real;
      @key{function} @AdaSubDefn{From_Big_Real} (Arg : Big_Real) @key{return} Num
         @key{with} Pre => In_Range (Arg,
                               Low  => To_Big_Real (Num'First),
                               High => To_Big_Real (Num'Last))
                     @key{or else} (@key{raise} Constraint_Error);
   @key{end} Fixed_Conversions;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_String} (Arg  : Big_Real;
                       Fore : Field := 2;
                       Aft  : Field := 3;
                       Exp  : Field := 0) @key{return} String
      @key{with} Post => To_String'Result'First = 1;
   @key{function} @AdaSubDefn{From_String} (Arg   : String) @key{return} Big_Real;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_Quotient_String} (Arg : Big_Real) @key{return} String @key{is}
      (To_String (Numerator (Arg)) & " / " & To_String (Denominator (Arg)));
   @key{function} @AdaSubDefn{From_Quotient_String} (Arg : String) @key{return} Big_Real;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Put_Image}
     (Stream : @key{not null access} Ada.Streams.Root_Stream_Type'Class;
      Arg    : Big_Real);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} "+" (L : Big_Real) @key{return} Big_Real;
   @key{function} "-" (L : Big_Real) @key{return} Big_Real;
   @key{function} "abs" (L : Big_Real) @key{return} Big_Real;
   @key{function} "+" (L, R : Big_Real) @key{return} Big_Real;
   @key{function} "-" (L, R : Big_Real) @key{return} Big_Real;
   @key{function} "*" (L, R : Big_Real) @key{return} Big_Real;
   @key{function} "/" (L, R : Big_Real) @key{return} Big_Real;
   @key{function} "**" (L : Big_Real; R : Integer)
      @key{return} Big_Real;
   @key{function} @AdaSubDefn{Min} (L, R : Big_Real) @key{return} Big_Real;
   @key{function} @AdaSubDefn{Max} (L, R : Big_Real) @key{return} Big_Real;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key{private}
   ... -- @examcom{not specified by the language}
@key[end] Ada.Numerics.Big_Numbers.Big_Reals;]}

@end{Example}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],Text=[To_String and From_String behave analogously to the
Put and Get procedures defined in Text_IO.Float_IO (in particular, with respect
to the interpretation of the Fore, Aft, and Exp parameters), except that
Constraint_Error (not Data_Error) is propagated in error cases.
From_Quotient_String implements the inverse function of To_Quotient_String;
Constraint_Error is propagated in error cases. Put_Image calls To_String,
converts that String to a Wide_Wide_String using To_Wide_Wide_String, and the
resulting value to the stream using Wide_Wide_String'Write.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],Text=[For an instance of Float_Conversions or
Fixed_Conversions, To_Big_Real is exact (that is, the result represents exactly
the same mathematical value as the argument) and From_Big_Real is subject to the
same precision rules as a type conversion of a value of type T to the target
type Num, where T is a hypothetical floating point type whose model numbers
include all of the model numbers of Num as well as the exact mathematical value
of the argument.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],Text=[The other functions have their usual mathematical
meanings.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],Text=[The type Optional_Big_Real needs
finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}

@end{StaticSem}

@begin{ImplReq}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],Text=[No storage associated with an Optional_Big_Real
object shall be lost upon assignment or scope exit.]}

@begin{ImplNote}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
  @ChgAdded{Version=[5],Text=[The @ldquote@;No storage ... shall be lost@rdquote
  requirement does not preclude implementation techniques such as caching or
  unique number tables.]}
@end{ImplNote}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],Text=[For purposes of determining whether predicate checks
are performed as part of default initialization, the type Optional_Big_Real
shall be considered to have a subcomponent that has a @nt{default_expression}.]}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
  @ChgAdded{Version=[5],Type=[Leading],Text=[This means that the elaboration of]}
@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[Default_Initialized_Object : Big_Real;]}
@end{Example}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[is required to propagate Assertion_Error.]}
@end{Ramification}

@end{ImplReq}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}
  The package Numerics.Big_Numbers.Big_Reals is new.]}
@end{Extend2012}

