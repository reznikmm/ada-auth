@comment{ $Source: e:\\cvsroot/ARM/Source/pre_big.mss,v $ }
@comment{ $Revision: 1.2 $ $Date: 2020/06/03 00:09:01 $ $Author: randy $ }
@Part(predefbignum, Root="ada.mss")

@Comment{$Date: 2020/06/03 00:09:01 $}

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

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1],ARef=[AI12-0366-1]}
@ChgAdded{Version=[5],KeepNext=[T],Type=[Leading],Text=[The library package
Numerics.Big_Numbers.Big_Integers has the following declaration:]}

@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key{with} Ada.Strings.Text_Buffers;
@key{package} Ada.Numerics.Big_Numbers.Big_Integers@ChildUnit{Parent=[Ada.Numerics.Big_Numbers],Child=[Big_Integers]}
   @key{with} Preelaborate, Nonblocking, Global => @key{null} @key{is}]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{type} @AdaTypeDefn{Big_Integer} @key{is private}
     @key{with} Integer_Literal => From_String,
          Put_Image => Put_Image;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Is_Valid} (Arg : Big_Integer) @key{return} Boolean
      @key{with} Convention => Intrinsic;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{subtype} @AdaSubtypeDefn{Name=[Valid_Big_Integer],Of=[Big_Integer]} @key{is} Big_Integer
      @key{with} Dynamic_Predicate => Is_Valid (Valid_Big_Integer),
           Predicate_Failure => (@key{raise} Program_Error);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} "=" (L, R : Valid_Big_Integer) @key{return} Boolean;
   @key{function} "<" (L, R : Valid_Big_Integer) @key{return} Boolean;
   @key{function} "<=" (L, R : Valid_Big_Integer) @key{return} Boolean;
   @key{function} ">" (L, R : Valid_Big_Integer) @key{return} Boolean;
   @key{function} ">=" (L, R : Valid_Big_Integer) @key{return} Boolean;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_Big_Integer} (Arg : Integer) @key{return} Valid_Big_Integer;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{subtype} @AdaSubtypeDefn{Name=[Big_Positive],Of=[Big_Integer]} @key{is} Big_Integer
      @key{with} Dynamic_Predicate => (@key{if} Is_Valid (Big_Positive) @key{then} Big_Positive > 0),
           Predicate_Failure => (@key{raise} Constraint_Error);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{subtype} @AdaSubtypeDefn{Name=[Big_Natural],Of=[Big_Integer]} @key{is} Big_Integer
      @key{with} Dynamic_Predicate => (@key{if} Is_Valid (Big_Natural) @key{then} Big_Natural => 0),
           Predicate_Failure => (@key{raise} Constraint_Error);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{In_Range} (Arg, Low, High : Valid_Big_Integer) @key{return} Boolean @key{is}
     (Low <= Arg @key{and} Arg <= High);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_Integer} (Arg : Valid_Big_Integer) @key{return} Integer
      @key{with} Pre => In_Range (Arg,
                            Low  => To_Big_Integer (Integer'First),
                            High => To_Big_Integer (Integer'Last))
                   @key{or else raise} Constraint_Error;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{generic}
      @key{type} Int @key{is range} <>;
   @key{package} @AdaPackDefn{Signed_Conversions} @key{is}
      @key{function} @AdaSubDefn{To_Big_Integer} (Arg : Int) @key{return} Valid_Big_Integer;
      @key{function} @AdaSubDefn{From_Big_Integer} (Arg : Valid_Big_Integer) @key{return} Int
         @key{with} Pre => In_Range (Arg,
                               Low  => To_Big_Integer (Int'First),
                               High => To_Big_Integer (Int'Last))
                      @key{or else raise} Constraint_Error;
   @key{end} Signed_Conversions;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{generic}
      @key{type} Int @key{is mod} <>;
   @key{package} @AdaPackDefn{Unsigned_Conversions} @key{is}
      @key{function} @AdaSubDefn{To_Big_Integer} (Arg : Int) @key{return} Valid_Big_Integer;
      @key{function} @AdaSubDefn{From_Big_Integer} (Arg : Valid_Big_Integer) @key{return} Int
         @key{with} Pre => In_Range (Arg,
                               Low  => To_Big_Integer (Int'First),
                               High => To_Big_Integer (Int'Last))
                      @key{or else raise} Constraint_Error;
   @key{end} Unsigned_Conversions;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_String} (Arg : Valid_Big_Integer;
                       Width : Field := 0;
                       Base  : Number_Base := 10) @key{return} String
      @key{with} Post => To_String'Result'First = 1;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{From_String} (Arg : String) @key{return} Valid_Big_Integer;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Put_Image}
     (Buffer : @key{in out} Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : @key{in} Valid_Big_Integer);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} "+" (L : Valid_Big_Integer) @key{return} Valid_Big_Integer;
   @key{function} "-" (L : Valid_Big_Integer) @key{return} Valid_Big_Integer;
   @key{function} "abs" (L : Valid_Big_Integer) @key{return} Valid_Big_Integer;
   @key{function} "+" (L, R : Valid_Big_Integer) @key{return} Valid_Big_Integer;
   @key{function} "-" (L, R : Valid_Big_Integer) @key{return} Valid_Big_Integer;
   @key{function} "*" (L, R : Valid_Big_Integer) @key{return} Valid_Big_Integer;
   @key{function} "/" (L, R : Valid_Big_Integer) @key{return} Valid_Big_Integer;
   @key{function} "mod" (L, R : Valid_Big_Integer) @key{return} Valid_Big_Integer;
   @key{function} "rem" (L, R : Valid_Big_Integer) @key{return} Valid_Big_Integer;
   @key{function} "**" (L : Valid_Big_Integer; R : Natural)
      @key{return} Valid_Big_Integer;
   @key{function} @AdaSubDefn{Min} (L, R : Valid_Big_Integer) @key{return} Valid_Big_Integer;
   @key{function} @AdaSubDefn{Max} (L, R : Valid_Big_Integer) @key{return} Valid_Big_Integer;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Greatest_Common_Divisor}
     (L, R : Valid_Big_Integer) @key{return} Big_Positive
     @key{with} Pre => (L /= 0 @key{and} R /= 0) @key{or else raise} Constraint_Error;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key{private}
   ... -- @examcom{not specified by the language}
@key[end] Ada.Numerics.Big_Numbers.Big_Integers;]}

@end{Example}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1],ARef=[AI12-0366-1]}
@ChgAdded{Version=[5],Text=[To_String and From_String behave analogously to the
Put and Get procedures defined in Text_IO.Integer_IO (in particular, with
respect to the interpretation of the Width and Base parameters) except that
Constraint_Error, not Data_Error, is propagated in error cases and the result of
a call To_String with a Width parameter of 0 and a nonnegative Arg parameter
does not include a leading blank. Put_Image calls To_String (passing in the
default values for the Width and Base parameters), prepends a leading blank if
the argument is nonnegative, and writes the resulting value to the buffer using 
Text_Buffers.Put.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
@ChgAdded{Version=[5],Text=[The other functions have their usual mathematical
meanings.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1],ARef=[AI12-0366-1]}
@ChgAdded{Version=[5],Text=[The type Big_Integer needs
finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}
@end{StaticSem}

@begin{Runtime}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1],ARef=[AI12-0366-1]}
@ChgAdded{Version=[5],Text=[For purposes of determining whether predicate checks
are performed as part of default initialization, the type Big_Integer
is considered to have a subcomponent that has a @nt{default_expression}.]}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
  @ChgAdded{Version=[5],Type=[Leading],Text=[This means that the elaboration of]}
@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[Default_Initialized_Object : Valid_Big_Integer;]}
@end{Example}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[is required to propagate Program_Error.]}
@end{Ramification}
@end{Runtime}

@begin{ImplReq}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1],ARef=[AI12-0366-1]}
@ChgAdded{Version=[5],Text=[No storage associated with a Big_Integer
object shall be lost upon assignment or scope exit.]}

@begin{ImplNote}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
  @ChgAdded{Version=[5],Text=[The @ldquote@;No storage ... shall be lost@rdquote
  requirement does not preclude implementation techniques such as caching or
  unique number tables.]}
@end{ImplNote}
@end{ImplReq}


@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1],ARef=[AI12-0366-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}
  The package Numerics.Big_Numbers.Big_Integers is new.]}
@end{Extend2012}


@LabeledAddedSubclause{Version=[5],Name=[Big Reals]}

@begin{StaticSem}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1],ARef=[AI12-0366-1]}
@ChgAdded{Version=[5],KeepNext=[T],Type=[Leading],Text=[The library package
Numerics.Big_Numbers.Big_Reals has the following declaration:]}

@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key{with} Ada.Numerics.Big_Numbers.Big_Integers;
   @key{use all type} Big_Integers.Big_Integer;
@key{with} Ada.Strings.Text_Buffers;
@key{package} Ada.Numerics.Big_Numbers.Big_Reals@ChildUnit{Parent=[Ada.Numerics.Big_Numbers],Child=[Big_Reals]}
   @key{with} Preelaborate, Nonblocking, Global => @key{null} @key{is}]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{type} @AdaTypeDefn{Big_Real} @key{is private}
      @key{with} Real_Literal => From_String,
           Put_Image => Put_Image;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Is_Valid} (Arg : Big_Real) @key{return} Boolean
      @key{with} Convention => Intrinsic;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{subtype} @AdaSubtypeDefn{Name=[Valid_Big_Real],Of=[Big_Real]} @key{is} Big_Real
      @key{with} Dynamic_Predicate => Is_Valid (Valid_Big_Real),
           Predicate_Failure => @key{raise} Program_Error;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} "/" (Num, Den : Big_Integers.Valid_Big_Integer) 
      @key{return} Valid_Big_Real
      @key{with} Pre => Den /= 0
                   @key{or else raise} Constraint_Error;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Numerator}
      (Arg : Valid_Big_Real) @key{return} Big_Integers.Valid_Big_Integer
     @key{with} Post => (@key{if} Arg = 0.0 @key{then} Numerator'Result = 0);]}

@begin{Reason}
   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[The postcondition of Numerator cannot be complete
   as it cannot mention Denominator. Since the postcondition of Denominator uses
   Numerator, we would get an infinite mutual recursion if both postconditions 
   are enabled. The postcondition of Denominator serves as the postcondition for
   Numerator as well unless Arg = 0.0.]}
@end{Reason}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{Denominator} (Arg : Valid_Big_Real) 
      @key{return} Big_Integers.Big_Positive
      @key{with} Post =>
        (@key{if} Arg = 0.0 @key{then} Denominator'Result = 1 
         @key{else} Big_Integers.Greatest_Common_Divisor
                (Numerator (Arg), Denominator'Result) = 1);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_Big_Real} (Arg : Big_Integers.Valid_Big_Integer)
      @key{return} Valid_Big_Real @key{is} (Arg / 1);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_Real} (Arg : Integer) @key{return} Valid_Big_Real @key{is}
      (Big_Integers.To_Big_Integer (Arg) / 1);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} "=" (L, R : Valid_Big_Real) @key{return} Boolean;
   @key{function} "<" (L, R : Valid_Big_Real) @key{return} Boolean;
   @key{function} "<=" (L, R : Valid_Big_Real) @key{return} Boolean;
   @key{function} ">" (L, R : Valid_Big_Real) @key{return} Boolean;
   @key{function} ">=" (L, R : Valid_Big_Real) @key{return} Boolean;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{In_Range} (Arg, Low, High : Valid_Big_Real) @key{return} Boolean @key{is}
      (Low <= Arg @key{and} Arg <= High);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{generic}
      @key{type} Num @key{is digits} <>;
   @key{package} @AdaPackDefn{Float_Conversions} @key{is}
      @key{function} @AdaSubDefn{To_Big_Real} (Arg : Num) @key{return} Valid_Big_Real;
      @key{function} @AdaSubDefn{From_Big_Real} (Arg : Valid_Big_Real) @key{return} Num
         @key{with} Pre => In_Range (Arg,
                               Low  => To_Big_Real (Num'First),
                               High => To_Big_Real (Num'Last))
                     @key{or else} (@key{raise} Constraint_Error);
   @key{end} Float_Conversions;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{generic}
      @key{type} Num @key{is delta} <>;
   @key{package} @AdaPackDefn{Fixed_Conversions} @key{is}
      @key{function} @AdaSubDefn{To_Big_Real} (Arg : Num) @key{return} Valid_Big_Real;
      @key{function} @AdaSubDefn{From_Big_Real} (Arg : Valid_Big_Real) @key{return} Num
         @key{with} Pre => In_Range (Arg,
                               Low  => To_Big_Real (Num'First),
                               High => To_Big_Real (Num'Last))
                     @key{or else} (@key{raise} Constraint_Error);
   @key{end} Fixed_Conversions;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_String} (Arg  : Valid_Big_Real;
                       Fore : Field := 2;
                       Aft  : Field := 3;
                       Exp  : Field := 0) @key{return} String
      @key{with} Post => To_String'Result'First = 1;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{From_String} (Arg   : String) @key{return} Valid_Big_Real;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} @AdaSubDefn{To_Quotient_String} (Arg : Valid_Big_Real) @key{return} String @key{is}
      (To_String (Numerator (Arg)) & " / " & To_String (Denominator (Arg)));
   @key{function} @AdaSubDefn{From_Quotient_String} (Arg : String) @key{return} Valid_Big_Real;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{procedure} @AdaSubDefn{Put_Image}
     (Buffer : @key{in out} Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Arg    : @key{in} Valid_Big_Real);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   @key{function} "+" (L : Valid_Big_Real) @key{return} Valid_Big_Real;
   @key{function} "-" (L : Valid_Big_Real) @key{return} Valid_Big_Real;
   @key{function} "abs" (L : Valid_Big_Real) @key{return} Valid_Big_Real;
   @key{function} "+" (L, R : Valid_Big_Real) @key{return} Valid_Big_Real;
   @key{function} "-" (L, R : Valid_Big_Real) @key{return} Valid_Big_Real;
   @key{function} "*" (L, R : Valid_Big_Real) @key{return} Valid_Big_Real;
   @key{function} "/" (L, R : Valid_Big_Real) @key{return} Valid_Big_Real;
   @key{function} "**" (L : Valid_Big_Real; R : Integer)
      @key{return} Valid_Big_Real;
   @key{function} @AdaSubDefn{Min} (L, R : Valid_Big_Real) @key{return} Valid_Big_Real;
   @key{function} @AdaSubDefn{Max} (L, R : Valid_Big_Real) @key{return} Valid_Big_Real;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key{private}
   ... -- @examcom{not specified by the language}
@key[end] Ada.Numerics.Big_Numbers.Big_Reals;]}

@end{Example}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1],ARef=[AI12-0366-1]}
@ChgAdded{Version=[5],Text=[To_String and From_String behave analogously to the
Put and Get procedures defined in Text_IO.Float_IO (in particular, with respect
to the interpretation of the Fore, Aft, and Exp parameters), except that
Constraint_Error (not Data_Error) is propagated in error cases.
From_Quotient_String implements the inverse function of To_Quotient_String;
Constraint_Error is propagated in error cases. Put_Image calls To_String,
and writes the resulting value to the buffer using Text_Buffers.Put.]}

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

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1],ARef=[AI12-0366-1]}
@ChgAdded{Version=[5],Text=[The type Big_Real needs
finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}

@end{StaticSem}

@begin{Runtime}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1],ARef=[AI12-0366-1]}
@ChgAdded{Version=[5],Text=[For purposes of determining whether predicate checks
are performed as part of default initialization, the type Big_Real
is considered to have a subcomponent that has a @nt{default_expression}.]}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
  @ChgAdded{Version=[5],Type=[Leading],Text=[This means that the elaboration of]}
@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[Default_Initialized_Object : Valid_Big_Real;]}
@end{Example}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[is required to propagate Program_Error.]}
@end{Ramification}
@end{Runtime}

@begin{ImplReq}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1],ARef=[AI12-0366-1]}
@ChgAdded{Version=[5],Text=[No storage associated with a Big_Real
object shall be lost upon assignment or scope exit.]}

@begin{ImplNote}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1]}
  @ChgAdded{Version=[5],Text=[The @ldquote@;No storage ... shall be lost@rdquote
  requirement does not preclude implementation techniques such as caching or
  unique number tables.]}
@end{ImplNote}
@end{ImplReq}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0208-1],ARef=[AI12-0366-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}
  The package Numerics.Big_Numbers.Big_Reals is new.]}
@end{Extend2012}

