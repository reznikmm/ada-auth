@comment{ $Source: e:\\cvsroot/ARM/Source/numerics.mss,v $ }
@comment{ $Revision: 1.54 $ $Date: 2006/10/19 06:40:29 $ $Author: Randy $ }
@Part(numerics, Root="ada.mss")

@Comment{$Date: 2006/10/19 06:40:29 $}

@LabeledNormativeAnnex{Numerics}
@begin{Intro}
@Defn{numerics}
The Numerics Annex specifies
@begin{itemize}
   features for complex arithmetic, including complex I/O;

   a mode (@lquotes@;strict mode@rquotes@;), in which the predefined arithmetic operations of
   floating point and fixed point types and the functions and operations of
   various predefined packages have to provide guaranteed accuracy or conform
   to other numeric performance requirements, which the Numerics Annex also
   specifies;

   a mode (@lquotes@;relaxed mode@rquotes@;), in which no accuracy or other numeric performance
   requirements need be satisfied, as for implementations not conforming to the
   Numerics Annex;

   @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00296-01]}
   models of floating point and fixed point arithmetic on which the accuracy
   requirements of strict mode are based;@Chg{Version=[2],New=[],Old=[ and]}

   @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00296-01]}
   the definitions of the model-oriented attributes of floating point types
   that apply in the strict mode@Chg{Version=[2],New=[; and],Old=[.]}

   @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00296-01]}@ChgAdded{Version=[2],
   Text=[features for the manipulation of real and complex vectors and
   matrices.]}
@end{itemize}
@end{Intro}

@begin{ImplAdvice}
If Fortran (respectively, C) is widely supported in the target environment,
implementations supporting the Numerics Annex should provide the child package
Interfaces.Fortran (respectively, Interfaces.C) specified in
@RefSecNum{Interface to Other Languages}
and should support a @i{convention_}@nt{identifier} of
Fortran (respectively, C) in the interfacing pragmas
(see @RefSecNum{Interface to Other Languages}),
thus allowing Ada programs to interface with programs written in
that language.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[If Fortran (respectively, C) is supported in the target environment,
then interfacing to Fortran (respectively, C) should be supported as
specified in @RefSecNum{Interface to Other Languages}.]}]}

@end{ImplAdvice}

@begin{Extend83}
@Defn{extensions to Ada 83}
This Annex is new to Ada 95.
@end{Extend83}

@LabeledClause{Complex Arithmetic}

@begin{Intro}
Types and arithmetic operations for complex arithmetic are provided in
Generic_Complex_Types, which is defined in @RefSecNum{Complex Types}.
Implementation-defined approximations to the complex analogs of the mathematical
functions known as the @lquotes@;elementary functions@rquotes@; are provided by
the subprograms in Generic_@!Complex_@!Elementary_@!Functions, which is defined in
@RefSecNum{Complex Elementary Functions}. Both of these library units are generic
children of the predefined package Numerics (see @RefSecNum{The Numerics Packages}).
Nongeneric equivalents of these generic packages for each of the predefined
floating point types are also provided as children of Numerics.
@ImplDef{The accuracy actually achieved by the complex elementary
functions and by other complex arithmetic
operations.}
@begin{Discussion}
   Complex arithmetic is defined in the Numerics Annex, rather than in the
   core, because it is considered to be a specialized need of (some) numeric
   applications.
@end{Discussion}
@end{Intro}

@LabeledSubClause{Complex Types}

@begin{StaticSem}
@Leading@;The generic library package
Numerics.Generic_Complex_Types has the following declaration:
@begin{Example}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0020],ARef=[AI95-00126-01]}
@key{generic}@ChildUnit{Parent=[Ada.Numerics],Child=[Generic_@!Complex_@!Types]}
   @key{type} Real @key{is} @key{digits} <>;
@key{package} Ada.Numerics.Generic_Complex_Types @key{is}
   @Chg{New=[@key{pragma}],Old=[pragma]} Pure(Generic_Complex_Types);

   @key{type} @AdaTypeDefn{Complex} @key{is}
      @key{record}
         Re, Im : Real'Base;
      @key{end} @key{record};

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00161-01]}
   @key{type} @AdaTypeDefn{Imaginary} @key{is} @key{private};@Chg{Version=[2],New=[
   @key{pragma} Preelaborable_Initialization(Imaginary);],Old=[]}

   @AdaObjDefn{i} : @key{constant} Imaginary;
   @AdaObjDefn{j} : @key{constant} Imaginary;


   @key{function} @AdaSubDefn{Re} (X : Complex)   @key{return} Real'Base;
   @key{function} @AdaSubDefn{Im} (X : Complex)   @key{return} Real'Base;
   @key{function} @AdaSubDefn{Im} (X : Imaginary) @key{return} Real'Base;

   @key{procedure} @AdaSubDefn{Set_Re} (X  : @key{in} @key{out} Complex;
                     Re : @key{in}     Real'Base);
   @key{procedure} @AdaSubDefn{Set_Im} (X  : @key{in} @key{out} Complex;
                     Im : @key{in}     Real'Base);
   @key{procedure} @AdaSubDefn{Set_Im} (X  :    @key{out} Imaginary;
                     Im : @key{in}     Real'Base);

   @key{function} @AdaSubDefn{Compose_From_Cartesian} (Re, Im : Real'Base) @key{return} Complex;
   @key{function} @AdaSubDefn{Compose_From_Cartesian} (Re     : Real'Base) @key{return} Complex;
   @key{function} @AdaSubDefn{Compose_From_Cartesian} (Im     : Imaginary) @key{return} Complex;

   @key{function} @AdaSubDefn{Modulus} (X     : Complex) @key{return} Real'Base;
   @key{function} "@key{abs}"   (Right : Complex) @key{return} Real'Base @key{renames} Modulus;

   @key{function} @AdaSubDefn{Argument} (X     : Complex)   @key{return} Real'Base;
   @key{function} @AdaSubDefn{Argument} (X     : Complex;
                      Cycle : Real'Base) @key{return} Real'Base;

   @key{function} @AdaSubDefn{Compose_From_Polar} (Modulus, Argument        : Real'Base)
      @key{return} Complex;
   @key{function} @AdaSubDefn{Compose_From_Polar} (Modulus, Argument, Cycle : Real'Base)
      @key{return} Complex;


   @key{function} "+"       (Right : Complex) @key{return} Complex;
   @key{function} "-"       (Right : Complex) @key{return} Complex;
   @key{function} @AdaSubDefn{Conjugate} (X     : Complex) @key{return} Complex;


   @key{function} "+" (Left, Right : Complex) @key{return} Complex;
   @key{function} "-" (Left, Right : Complex) @key{return} Complex;
   @key{function} "*" (Left, Right : Complex) @key{return} Complex;
   @key{function} "/" (Left, Right : Complex) @key{return} Complex;


   @key{function} "**" (Left : Complex; Right : Integer) @key{return} Complex;


   @key{function} "+"       (Right : Imaginary) @key{return} Imaginary;
   @key{function} "-"       (Right : Imaginary) @key{return} Imaginary;
   @key{function} @AdaSubDefn{Conjugate} (X     : Imaginary) @key{return} Imaginary @key{renames} "-";
   @key{function} "@key{abs}"     (Right : Imaginary) @key{return} Real'Base;


   @key{function} "+" (Left, Right : Imaginary) @key{return} Imaginary;
   @key{function} "-" (Left, Right : Imaginary) @key{return} Imaginary;
   @key{function} "*" (Left, Right : Imaginary) @key{return} Real'Base;
   @key{function} "/" (Left, Right : Imaginary) @key{return} Real'Base;


   @key{function} "**" (Left : Imaginary; Right : Integer) @key{return} Complex;


   @key{function} "<"  (Left, Right : Imaginary) @key{return} Boolean;
   @key{function} "<=" (Left, Right : Imaginary) @key{return} Boolean;
   @key{function} ">"  (Left, Right : Imaginary) @key{return} Boolean;
   @key{function} ">=" (Left, Right : Imaginary) @key{return} Boolean;


   @key{function} "+" (Left : Complex;   Right : Real'Base) @key{return} Complex;
   @key{function} "+" (Left : Real'Base; Right : Complex)   @key{return} Complex;
   @key{function} "-" (Left : Complex;   Right : Real'Base) @key{return} Complex;
   @key{function} "-" (Left : Real'Base; Right : Complex)   @key{return} Complex;
   @key{function} "*" (Left : Complex;   Right : Real'Base) @key{return} Complex;
   @key{function} "*" (Left : Real'Base; Right : Complex)   @key{return} Complex;
   @key{function} "/" (Left : Complex;   Right : Real'Base) @key{return} Complex;
   @key{function} "/" (Left : Real'Base; Right : Complex)   @key{return} Complex;


   @key{function} "+" (Left : Complex;   Right : Imaginary) @key{return} Complex;
   @key{function} "+" (Left : Imaginary; Right : Complex)   @key{return} Complex;
   @key{function} "-" (Left : Complex;   Right : Imaginary) @key{return} Complex;
   @key{function} "-" (Left : Imaginary; Right : Complex)   @key{return} Complex;
   @key{function} "*" (Left : Complex;   Right : Imaginary) @key{return} Complex;
   @key{function} "*" (Left : Imaginary; Right : Complex)   @key{return} Complex;
   @key{function} "/" (Left : Complex;   Right : Imaginary) @key{return} Complex;
   @key{function} "/" (Left : Imaginary; Right : Complex)   @key{return} Complex;


   @key{function} "+" (Left : Imaginary; Right : Real'Base) @key{return} Complex;
   @key{function} "+" (Left : Real'Base; Right : Imaginary) @key{return} Complex;
   @key{function} "-" (Left : Imaginary; Right : Real'Base) @key{return} Complex;
   @key{function} "-" (Left : Real'Base; Right : Imaginary) @key{return} Complex;
   @key{function} "*" (Left : Imaginary; Right : Real'Base) @key{return} Imaginary;
   @key{function} "*" (Left : Real'Base; Right : Imaginary) @key{return} Imaginary;
   @key{function} "/" (Left : Imaginary; Right : Real'Base) @key{return} Imaginary;
   @key{function} "/" (Left : Real'Base; Right : Imaginary) @key{return} Imaginary;


@key[private]

   @key{type} Imaginary @key{is} @key{new} Real'Base;
   i : @key{constant} Imaginary := 1.0;
   j : @key{constant} Imaginary := 1.0;

@key{end} Ada.Numerics.Generic_Complex_Types;
@end{Example}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0020],ARef=[AI95-00126-01]}
@ChildUnit{Parent=[Ada.Numerics],Child=[Complex_@!Types]}
The library package Numerics.Complex_Types
@Chg{New=[is declared pure and ],Old=[]}defines the same types, constants,
and subprograms as Numerics.Generic_Complex_Types, except that the
predefined type Float is systematically substituted for Real'Base throughout.
Nongeneric equivalents of Numerics.Generic_Complex_Types for each of the other
predefined floating point types are defined similarly, with the names
Numerics.@!Short_@!Complex_@!Types, Numerics.@!Long_@!Complex_@!Types, etc.
@begin{Reason}
   The nongeneric equivalents are provided to allow the programmer to
   construct simple mathematical applications without being required to
   understand and use generics.
@end{Reason}
@begin{Reason}
   The nongeneric equivalents all export the types Complex and Imaginary and
   the constants i and j (rather than uniquely named types and constants, such
   as Short_Complex, Long_Complex, etc.) to preserve their equivalence to
   actual instantiations of the generic package and to allow the programmer to
   change the precision of an application globally by changing a single
   context clause.
@end{Reason}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
@Redundant{Complex is a visible type with
@Chg{Version=[2],New=[Cartesian],Old=[cartesian]} components.}
@begin{Reason}
   The @Chg{Version=[2],New=[Cartesian],Old=[cartesian]} representation is
   far more common than the polar
   representation, in practice. The accuracy of the results of the complex
   arithmetic operations and of the complex elementary functions
   is dependent on the representation; thus, implementers need to know that
   representation. The type is visible so that complex @lquotes@;literals@rquotes@; can be
   written in aggregate notation, if desired.
@end{Reason}

@Redundant{Imaginary is a private type; its full type is derived from
Real'Base.}
@begin{Reason}
   @Leading@;The Imaginary type and the constants i and j are provided for two reasons:
   @begin{itemize}
      They allow complex @lquotes@;literals@rquotes@; to be written in the alternate form of
      @RI{a} + @RI{b}*i (or @RI{a} + @RI{b}*j), if desired. Of course,
      in some contexts the sum will need to be parenthesized.

      When an Ada binding to IEC 559:1989 that provides (signed) infinities
      as the result of operations that overflow becomes available, it will be
      important to allow arithmetic between pure-imaginary and complex operands
      without requiring the former to be represented as (or promoted to)
      complex values with a real component of zero. For example, the
      multiplication of @RI{a} + @RI{b}*i by @RI{d}*i should yield
      @en@RI{b}@Times @RI{d} + @RI{a}@Times @RI{d}*i, but if one cannot avoid representing the
      pure-imaginary value @RI{d}*i as the complex value
      0.0 + @RI{d}*i, then a NaN ("Not-a-Number") could be produced
      as the result of multiplying @RI{a} by 0.0 (e.g., when @RI{a} is
      infinite); the NaN could later trigger an exception.
      Providing the Imaginary type and overloadings of the
      arithmetic operators for mixtures of Imaginary and Complex operands
      gives the programmer the same control over avoiding premature coercion of
      pure-imaginary values to complex as is already provided for pure-real
      values.
   @end{itemize}
@end{Reason}
@begin{Reason}
   @Leading@;The Imaginary type is private, rather than being visibly derived
   from Real'Base, for two reasons:
   @begin{itemize}
      to preclude implicit conversions of real literals to the Imaginary type
      (such implicit conversions would make many common arithmetic expressions
      ambiguous); and

      to suppress the implicit derivation of the multiplication, division, and
      absolute value operators with Imaginary operands and an Imaginary result
      (the result type would be incorrect).
   @end{itemize}
@end{Reason}
@begin{Reason}
   The base subtype Real'Base is used for the component type of Complex, the
   parent type of Imaginary, and the parameter and result types of some of the
   subprograms to maximize the chances of being able to pass meaningful values
   into the subprograms and receive meaningful results back. The generic
   formal parameter Real therefore plays only one role, that of providing the
   precision to be maintained in complex arithmetic calculations. Thus, the
   subprograms in Numerics.Generic_Complex_Types share with those in
   Numerics.Generic_Elementary_Functions, and indeed even with the predefined
   arithmetic operations (see @RefSecNum{Operators and Expression Evaluation}),
   the property of being free of range checks on input
   and output, i.e., of being able to exploit the base range of the relevant
   floating point type fully. As a result, the user loses the ability to
   impose application-oriented bounds on the range of values that the
   components of a complex variable can acquire; however, it can be argued that
   few, if any, applications have a naturally square domain (as opposed to a
   circular domain) anyway.
@end{Reason}

@Leading@;The arithmetic operations and the Re, Im, Modulus, Argument, and Conjugate
functions have their usual mathematical meanings. When applied to a parameter
of pure-imaginary type, the @lquotes@;imaginary-part@rquotes@; function Im yields the value of
its parameter, as the corresponding real value.
The remaining subprograms have the following meanings:
@begin{Reason}
   The middle case can be understood by considering the parameter of
   pure-imaginary type to represent a complex value with a zero real part.
@end{Reason}
@begin{Itemize}
   The Set_Re and Set_Im procedures replace the designated component of a
   complex parameter with the given real value; applied to a parameter of
   pure-imaginary type, the Set_Im procedure replaces the value of that
   parameter with the imaginary value corresponding to the given real value.

   The Compose_From_Cartesian function constructs a complex value from the
   given real and imaginary components. If only one component is given, the
   other component is implicitly zero.

   The Compose_From_Polar function constructs a complex value from the given
   modulus (radius) and argument (angle). When the value of the parameter
   Modulus is positive (resp., negative), the result is the complex value
   represented by the point in the complex plane lying at a distance from the
   origin given by the absolute value of Modulus and forming an angle measured
   counterclockwise from the positive (resp., negative) real axis given by the
   value of the parameter Argument.
@end(Itemize)

When the Cycle parameter is specified, the result of the Argument function and
the parameter Argument of the Compose_From_Polar function are measured in units
such that a full cycle of revolution has the given value; otherwise, they are
measured in radians.

@Leading@;The computed results of the mathematically multivalued functions are rendered
single-valued by the following conventions, which are meant to imply the
principal branch:
@begin{Itemize}
   The result of the Modulus function is nonnegative.

   The result of the Argument function is in the quadrant containing the point
   in the complex plane represented by the parameter X. This may be any
   quadrant (I through IV); thus, the range of the Argument function is
   approximately @en@Pi to @Pi
   (@en@R[Cycle]/2.0 to @R[Cycle]/2.0, if the parameter Cycle is
   specified). When the point represented by the parameter X lies on the
   negative real axis, the result approximates
   @begin{InnerItemize}
      @Pi (resp., @en@Pi) when the sign of the imaginary
      component of X is positive (resp., negative), if Real'Signed_Zeros is
      True;

      @Pi, if Real'Signed_Zeros is False.
   @end{InnerItemize}

   Because a result lying on or near one of the axes may not be exactly
   representable, the approximation inherent in computing the result may place
   it in an adjacent quadrant, close to but on the wrong side of the axis.
@end{Itemize}
@end{StaticSem}

@begin{RunTime}
The exception Numerics.Argument_Error is raised by the Argument and
Compose_From_Polar functions with specified cycle, signaling a parameter value
outside the domain of the corresponding mathematical function, when the value
of the parameter Cycle is zero or negative.

@IndexCheck{Division_Check}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
The exception Constraint_Error is raised by the division operator when the
value of the right operand is zero, and by the exponentiation operator when
the value of the left operand is zero and the value of the exponent is
negative, provided that Real'Machine_Overflows is True; when
Real'Machine_Overflows is False, the result is
unspecified.
@PDefn{unspecified}
@Redundant[Constraint_Error
can also be raised when a finite result overflows
(see @RefSecNum{Accuracy Requirements for Complex Arithmetic}).]
@begin{Discussion}
   It is anticipated that an Ada binding to IEC 559:1989 will be developed in
   the future. As part of such a binding, the Machine_Overflows attribute of a
   conformant floating point type will be specified to yield False, which will
   permit implementations of the complex arithmetic operations to deliver
   results with an infinite component (and set the overflow flag defined by the
   binding) instead of raising Constraint_Error in overflow situations, when
   traps are disabled. Similarly, it is appropriate for the complex arithmetic
   operations to deliver results with infinite components (and set the
   zero-divide flag defined by the binding) instead of raising Constraint_Error
   in the situations defined above, when traps are disabled. Finally, such a
   binding should also specify the behavior of the complex arithmetic
   operations, when sensible, given operands with infinite components.
@end{Discussion}
@end{RunTime}

@begin{ImplReq}
In the implementation of Numerics.Generic_Complex_Types,
the range of intermediate values allowed during the calculation
of a final result shall not be affected by any
range constraint of the subtype Real.
@begin{ImplNote}
   Implementations of Numerics.Generic_Complex_Types written in Ada should
   therefore avoid declaring local variables of subtype Real; the subtype
   Real'Base should be used instead.
@end{ImplNote}

@Leading
@Defn2{Term=[prescribed result],
        Sec=[for the evaluation of a complex arithmetic operation]}
In the following cases, evaluation of a complex arithmetic operation shall
yield the @i{prescribed result},
provided that the preceding rules do not call for an exception to be
raised:
@begin{Itemize}
   The results of the Re, Im, and Compose_From_Cartesian functions are exact.

   The real (resp., imaginary) component of the result of a binary addition
   operator that yields a result of complex type is exact when either of its
   operands is of pure-imaginary (resp., real) type.
   @begin{Ramification}
      The result of the addition operator is exact when one of its operands is
      of real type and the other is of pure-imaginary type. In this particular
      case, the operator is analogous to the Compose_From_Cartesian function;
      it performs no arithmetic.
   @end{Ramification}

   The real (resp., imaginary) component of the result of a binary subtraction
   operator that yields a result of complex type is exact when its right
   operand is of pure-imaginary (resp., real) type.

   The real component of the result of the Conjugate function for the complex
   type is exact.

   When the point in the complex plane represented by the parameter X lies on
   the nonnegative real axis, the Argument function yields a result of zero.
   @begin{Discussion}
      Argument(X + i*Y) is analogous to @i{EF}.Arctan(Y, X), where @i{EF} is an
      appropriate instance of Numerics.Generic_Elementary_Functions, except
      when X and Y are both zero, in which case the former yields the value
      zero while the latter raises Numerics.Argument_Error.
   @end{Discussion}

   When the value of the parameter Modulus is zero, the Compose_From_Polar
   function yields a result of zero.

   When the value of the parameter Argument is equal to a multiple of the
   quarter cycle, the result of the Compose_From_Polar function with specified
   cycle lies on one of the axes. In this case, one of its components is zero,
   and the other has the magnitude of the parameter Modulus.

   Exponentiation by a zero exponent yields the value one. Exponentiation by
   a unit exponent yields the value of the left operand. Exponentiation of
   the value one yields the value one. Exponentiation of the value zero
   yields the value zero, provided that the exponent is nonzero. When the
   left operand is of pure-imaginary type, one component of the result of the
   exponentiation operator is zero.
@end{Itemize}

When the result, or a result component, of any operator of
Numerics.Generic_Complex_Types has a mathematical definition in terms of a
single arithmetic or relational operation, that result or result component
exhibits the accuracy of the corresponding operation of the type Real.

Other accuracy requirements for the Modulus, Argument, and Compose_From_Polar
functions, and accuracy requirements for the multiplication of a pair of
complex operands or for division by a complex operand, all of which apply
only in the strict mode, are given in
@RefSecNum{Accuracy Requirements for Complex Arithmetic}.

The sign of a zero result or zero result component yielded by a complex
arithmetic operation or function is implementation defined when
Real'Signed_Zeros is True.
@ImplDef{The sign of a zero result (or a component thereof) from any operator
or function in Numerics.Generic_Complex_Types, when Real'Signed_Zeros is True.}
@end{ImplReq}

@begin{ImplPerm}
The nongeneric equivalent packages may, but need not, be actual
instantiations of the generic package for the appropriate predefined type.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0091]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
Implementations may obtain the result of exponentiation of a complex
or pure-imaginary operand by repeated complex multiplication, with arbitrary
association of the factors and with a possible final complex reciprocation
(when the exponent is negative). Implementations are also permitted to obtain
the result of exponentiation of a complex operand, but not of a pure-imaginary
operand, by converting the left operand to a polar representation;
exponentiating the modulus by the given exponent; multiplying the argument by
the given exponent@Chg{New=[],Old=[, when the exponent is positive, or dividing
the argument by the absolute value of the given exponent, when the exponent is
negative]}; and reconverting to a @Chg{Version=[2],New=[Cartesian],Old=[cartesian]}
representation. Because of this
implementation freedom, no accuracy requirement is imposed on complex
exponentiation (except for the prescribed results given above, which apply
regardless of the implementation method chosen).
@end{ImplPerm}

@begin{ImplAdvice}
Because the usual mathematical meaning of multiplication of a complex operand
and a real operand is that of the scaling of both components of the former by
the latter, an implementation should not perform this operation by first
promoting the real operand to complex type and then performing a full complex
multiplication. In systems that, in the future, support an Ada binding to IEC
559:1989, the latter technique will not generate the required result when one
of the components of the complex operand is infinite. (Explicit multiplication
of the infinite component by the zero component obtained during promotion
yields a NaN that propagates into the final result.) Analogous advice applies
in the case of multiplication of a complex operand and a pure-imaginary
operand, and in the case of division of a complex operand by a real or
pure-imaginary operand.
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Mixed real and complex operations (as well as pure-imaginary and complex
operations) should not be performed by converting
the real (resp. pure-imaginary) operand to complex.]}]}

Likewise, because the usual mathematical meaning of addition of a complex
operand and a real operand is that the imaginary operand remains unchanged, an
implementation should not perform this operation by first promoting the real
operand to complex type and then performing a full complex addition. In
implementations in which the Signed_Zeros attribute of the component type is
True (and which therefore conform to IEC 559:1989 in regard to the handling of
the sign of zero in predefined arithmetic operations), the latter technique
will not generate the required result when the imaginary component of the
complex operand is a negatively signed zero. (Explicit addition of the
negative zero to the zero obtained during promotion yields a positive zero.)
Analogous advice applies in the case of addition of a complex operand and a
pure-imaginary operand, and in the case of subtraction of a complex operand and
a real or pure-imaginary operand.

Implementations in which Real'Signed_Zeros is True should attempt to provide a
rational treatment of the signs of zero results and result components. As one
example, the result of the Argument function should have the sign of the
imaginary component of the parameter X when the point represented by that
parameter lies on the positive real axis; as another, the sign of the imaginary
component of the Compose_@!From_@!Polar function should be the same as (resp., the
opposite of) that of the Argument parameter when that parameter has a value of
zero and the Modulus parameter has a nonnegative (resp., negative) value.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[If Real'Signed_Zeros is true for Numerics.@!Generic_@!Complex_@!Types,
a rational treatment of the signs of
zero results and result components should be provided.]}]}
@end{ImplAdvice}

@begin{DiffWord83}
@Leading@;The semantics of Numerics.Generic_Complex_Types differs from
Generic_Complex_Types as defined in ISO/IEC CD 13813
(for Ada 83) in the following ways:
@begin{itemize}
   The generic package is a child of the package defining the
   Argument_Error exception.

   The nongeneric equivalents export types and constants with the same names
   as those exported by the generic package, rather than with names unique to
   the package.

   Implementations are not allowed to impose an optional restriction that the
   generic actual parameter associated with Real be unconstrained. (In view of
   the ability to declare variables of subtype Real'Base in implementations of
   Numerics.Generic_Complex_Types, this flexibility is no longer needed.)

   The dependence of the Argument function on the sign of a zero parameter
   component is tied to the value of Real'Signed_Zeros.

   Conformance to accuracy requirements is conditional.
@end{itemize}
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00161-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @b[Amendment Correction:] Added a @nt{pragma} Preelaborable_Initialization to
  type Imaginary, so that it can be used in preelaborated units.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0020],ARef=[AI95-00126-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Explicitly stated that the
  nongeneric equivalents of Generic_Complex_Types are pure.]}
@end{DiffWord95}


@LabeledSubClause{Complex Elementary Functions}

@begin{StaticSem}
@Leading@;The generic library package
Numerics.Generic_Complex_Elementary_Functions has the following declaration:
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
@key[with] Ada.Numerics.Generic_Complex_Types;
@key[generic]@ChildUnit{Parent=[Ada.Numerics],Child=[Generic_@!Complex_@!Elementary_@!Functions]}
   @key[with] @key[package] Complex_Types @key[is]
         @key[new] Ada.Numerics.Generic_Complex_Types (<>);
   @key[use] Complex_Types;
@key[package] Ada.Numerics.Generic_Complex_Elementary_Functions @key[is]
   @Chg{Version=[2],New=[@key{pragma}],Old=[pragma]} Pure(Generic_Complex_Elementary_Functions);

   @key[function] @AdaSubDefn{Sqrt} (X : Complex)   @key[return] Complex;
   @key[function] @AdaSubDefn{Log}  (X : Complex)   @key[return] Complex;
   @key[function] @AdaSubDefn{Exp}  (X : Complex)   @key[return] Complex;
   @key[function] @AdaSubDefn{Exp}  (X : Imaginary) @key[return] Complex;
   @key[function] "**" (Left : Complex;   Right : Complex)   @key[return] Complex;
   @key[function] "**" (Left : Complex;   Right : Real'Base) @key[return] Complex;
   @key[function] "**" (Left : Real'Base; Right : Complex)   @key[return] Complex;


   @key[function] @AdaSubDefn{Sin} (X : Complex) @key[return] Complex;
   @key[function] @AdaSubDefn{Cos} (X : Complex) @key[return] Complex;
   @key[function] @AdaSubDefn{Tan} (X : Complex) @key[return] Complex;
   @key[function] @AdaSubDefn{Cot} (X : Complex) @key[return] Complex;


   @key[function] @AdaSubDefn{Arcsin} (X : Complex) @key[return] Complex;
   @key[function] @AdaSubDefn{Arccos} (X : Complex) @key[return] Complex;
   @key[function] @AdaSubDefn{Arctan} (X : Complex) @key[return] Complex;
   @key[function] @AdaSubDefn{Arccot} (X : Complex) @key[return] Complex;


   @key[function] @AdaSubDefn{Sinh} (X : Complex) @key[return] Complex;
   @key[function] @AdaSubDefn{Cosh} (X : Complex) @key[return] Complex;
   @key[function] @AdaSubDefn{Tanh} (X : Complex) @key[return] Complex;
   @key[function] @AdaSubDefn{Coth} (X : Complex) @key[return] Complex;


   @key[function] @AdaSubDefn{Arcsinh} (X : Complex) @key[return] Complex;
   @key[function] @AdaSubDefn{Arccosh} (X : Complex) @key[return] Complex;
   @key[function] @AdaSubDefn{Arctanh} (X : Complex) @key[return] Complex;
   @key[function] @AdaSubDefn{Arccoth} (X : Complex) @key[return] Complex;

@key[end] Ada.Numerics.Generic_Complex_Elementary_Functions;
@end{Example}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0020],ARef=[AI95-00126-01]}
@ChildUnit{Parent=[Ada.Numerics],Child=[Complex_@!Elementary_@!Functions]}
The library package Numerics.Complex_Elementary_Functions
@Chg{New=[is declared pure and ],Old=[]}defines the same subprograms as
Numerics.@!Generic_@!Complex_@!Elementary_@!Functions,
except that the predefined type Float is systematically substituted for
Real'Base, and the Complex and Imaginary types exported by Numerics.@!Complex_@!Types
are systematically substituted for Complex and Imaginary, throughout.
Nongeneric equivalents of Numerics.@!Generic_@!Complex_@!Elementary_@!Functions
corresponding to each of the other predefined floating point types are
defined similarly, with the names Numerics.@!Short_@!Complex_@!Elementary_@!Functions,
Numerics.@!Long_@!Complex_@!Elementary_@!Functions, etc.
@begin{Reason}
   The nongeneric equivalents are provided to allow the programmer to
   construct simple mathematical applications without being required to
   understand and use generics.
@end{Reason}

The overloading of the Exp function for the pure-imaginary type is provided
to give the user an alternate way to compose a complex value from a given
modulus and argument. In addition to Compose_@!From_@!Polar(Rho, Theta)
(see @RefSecNum{Complex Types}), the programmer may write Rho * Exp(i * Theta).

The imaginary (resp., real) component of the parameter X of the forward
hyperbolic (resp., trigonometric) functions and of the Exp function (and the
parameter X, itself, in the case of the overloading of the Exp function for the
pure-imaginary type) represents an angle measured in radians, as does the
imaginary (resp., real) component of the result of the Log and inverse
hyperbolic (resp., trigonometric) functions.

@Leading@;The functions have their usual mathematical meanings. However, the
arbitrariness inherent in the placement of branch cuts, across which some of
the complex elementary functions exhibit discontinuities, is eliminated by the
following conventions:
@begin{Itemize}
   The imaginary component of the result of the Sqrt and Log functions is
   discontinuous as the parameter X crosses the negative real axis.

   The result of the exponentiation operator when the left operand is of
   complex type is discontinuous as that operand crosses the negative real
   axis.

   @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00185-01]}
   The @Chg{Version=[2],New=[],Old=[real (resp., ]}imaginary@Chg{Version=[2],
   New=[],Old=[)]} component of the result of the Arcsin@Chg{Version=[2],
   New=[,],Old=[ and]} Arccos@Chg{Version=[2],New=[],Old=[(resp.]},
   @Chg{Version=[2],New=[and ],Old=[]}Arctanh@Chg{Version=[2],
   New=[],Old=[)]} functions is discontinuous as the parameter X crosses the
   real axis to the left of @en@;1.0 or the right of 1.0.

   @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00185-01]}
   The real @Chg{Version=[2],New=[],Old=[(resp., imaginary) ]}component of the
   result of the Arctan @Chg{Version=[2],New=[and],Old=[(resp.,]}
   Arcsinh@Chg{Version=[2], New=[ functions],Old=[) function]} is discontinuous
   as the parameter X crosses the imaginary axis below @en@RI{i} or above @RI{i}.

   @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00185-01]}
   The real component of the result of the Arccot function is discontinuous as
   the parameter X crosses the imaginary axis @Chg{Version=[2],New=[below],
   Old=[between]} @en@RI{i} @Chg{Version=[2],New=[or above],Old=[and]} @RI{i}.

   The imaginary component of the Arccosh function is discontinuous as the
   parameter X crosses the real axis to the left of 1.0.

   The imaginary component of the result of the Arccoth function is
   discontinuous as the parameter X crosses the real axis between @en@;1.0
   and 1.0.
@end{Itemize}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00185-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The branch cuts come from the
fact that the functions in question are
really multi-valued in the complex domain, and that we have to pick one
@i{principal value} to be the result of the function. Evidently we have
much freedom in choosing where the branch cuts lie. However, we are
adhering to the following principles which seem to lead to the more
@i{natural} definitions:]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A branch cut should not intersect the real axis
at a place where the
corresponding real function is well-defined (in other words, the complex
function should be an extension of the corresponding real function).]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Because all the functions in question are
analytic, to ensure power
series validity for the principal value, the branch cuts should be
invariant by complex conjugation.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[For odd functions, to ensure that the principal
value remains an odd
function, the branch cuts should be invariant by reflection in the origin.]}
@end{Itemize}
@end{Discussion}

@Leading@;@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00185-01]}The
computed results of the mathematically multivalued functions
are rendered single-valued by the following conventions, which are meant to
imply @Chg{Version=[2],New=[that ],Old=[]}the
principal branch@Chg{Version=[2],New=[ is an analytic continuation of
the corresponding real-valued function in
Numerics.Generic_Elementary_Functions. (For Arctan and Arccot,
the single-argument function in question is that obtained from the two-argument
version by fixing the second argument to be its default value.)],Old=[:]}
@begin{Itemize}
   The real component of the result of the Sqrt and Arccosh functions is
   nonnegative.

   The same convention applies to the imaginary component of the result of the
   Log function as applies to the result of the natural-cycle version of the
   Argument function of Numerics.Generic_Complex_Types
   (see @RefSecNum{Complex Types}).

   The range of the real (resp., imaginary) component of the result of the
   Arcsin and Arctan (resp., Arcsinh and Arctanh) functions is
   approximately @en@Pi/2.0 to @Pi/2.0.

   The real (resp., imaginary) component of the result of the Arccos and Arccot
   (resp., Arccoth) functions ranges from 0.0 to approximately @Pi.

   The range of the imaginary component of the result of the Arccosh function
   is approximately @en@Pi to @Pi.
@end{Itemize}

In addition, the exponentiation operator inherits the single-valuedness of the
Log function.
@end{StaticSem}

@begin{RunTime}
The exception Numerics.Argument_Error is raised by the exponentiation operator,
signaling a parameter value outside the domain of the corresponding
mathematical function, when the value of the left operand is zero and the real
component of the exponent (or the exponent itself, when it is of real type) is
zero.

@Leading@IndexCheck{Division_Check}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
The exception Constraint_Error is raised, signaling a pole of the mathematical
function (analogous to dividing by zero), in the following cases, provided that
Complex_Types.Real'Machine_Overflows is True:
@begin{Itemize}
   by the Log, Cot, and Coth functions, when the value of the parameter X is
   zero;

   by the exponentiation operator, when the value of the left operand is zero
   and the real component of the exponent (or the exponent itself, when it is
   of real type) is negative;

   by the Arctan and Arccot functions, when the value of the parameter X is
   @PorM @RI{i};

   by the Arctanh and Arccoth functions, when the value of the parameter X is
   @PorM 1.0.
@end{Itemize}

@redundant[Constraint_Error can also be raised
when a finite result overflows
(see @RefSecNum{Accuracy Requirements for Complex Arithmetic}); this may
occur for parameter values sufficiently @i{near} poles, and, in the case of
some of the functions, for parameter values having components of sufficiently
large magnitude.]
@PDefn{unspecified}
When Complex_Types.Real'Machine_Overflows is False, the result at poles is
unspecified.
@begin{Reason}
   The purpose of raising Constraint_Error (rather than
   Numerics.Argument_Error) at the poles of a function, when
   Float_Type'Machine_Overflows is True, is to provide continuous behavior as
   the actual parameters of the function approach the pole and finally reach
   it.
@end{Reason}
@begin{Discussion}
   It is anticipated that an Ada binding to IEC 559:1989 will be developed
   in the future. As part of such a binding, the Machine_Overflows attribute
   of a conformant floating point type will be specified to yield False, which
   will permit implementations of the complex elementary functions to deliver
   results with an infinite component (and set the overflow flag defined by the
   binding) instead of raising Constraint_Error in overflow situations, when
   traps are disabled. Similarly, it is appropriate for the complex elementary
   functions to deliver results with an infinite component (and set the
   zero-divide flag defined by the binding) instead of raising Constraint_Error
   at poles, when traps are disabled. Finally, such a binding should also
   specify the behavior of the complex elementary functions, when sensible,
   given parameters with infinite components.
@end{Discussion}
@end{RunTime}

@begin{ImplReq}
In the implementation of Numerics.Generic_Complex_Elementary_Functions,
the range of intermediate values allowed during the calculation
of a final result shall not be affected by any
range constraint of the subtype Complex_Types.Real.
@begin{ImplNote}
   Implementations of Numerics.Generic_Complex_Elementary_Functions written in Ada
   should therefore avoid declaring local variables of subtype Complex_Types.Real; the
   subtype Complex_Types.Real'Base should be used instead.
@end{ImplNote}

@Leading
@Defn2{Term=[prescribed result],
        Sec=[for the evaluation of a complex elementary function]}
In the following cases, evaluation of a complex elementary function shall
yield the @i{prescribed result} (or a result having the prescribed component),
provided that the preceding rules do not call for an exception to be
raised:
@begin{Itemize}
   When the parameter X has the value zero, the Sqrt, Sin, Arcsin, Tan, Arctan,
   Sinh, Arcsinh, Tanh, and Arctanh functions yield a result of zero; the
   Exp, Cos, and Cosh functions yield a result of one; the Arccos and Arccot
   functions yield a real result; and the Arccoth function yields an imaginary
   result.

   When the parameter X has the value one, the Sqrt function yields a result
   of one; the Log, Arccos, and Arccosh functions yield a result of zero; and
   the Arcsin function yields a real result.

   When the parameter X has the value @en@;1.0, the Sqrt function yields the
   result
   @begin{InnerItemize}
      @RI{i} (resp., @en@RI{i}), when the sign of the imaginary component of
      X is positive (resp., negative), if Complex_Types.Real'Signed_Zeros is
      True;

      @RI{i}, if Complex_Types.Real'Signed_Zeros is False;
   @end{InnerItemize}

   @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
   @Chg{Version=[2],New=[When the parameter X has the value @en@;1.0, ],
   Old=[]}the Log function yields an imaginary result; and the Arcsin and Arccos
   functions yield a real result.

   When the parameter X has the value @PorM @RI{i}, the Log function yields
   an imaginary result.

   Exponentiation by a zero exponent yields the value one. Exponentiation by
   a unit exponent yields the value of the left operand (as a complex value).
   Exponentiation of the value one yields the value one. Exponentiation of
   the value zero yields the value zero.
@end{Itemize}
@begin{Discussion}
   It is possible to give many other prescribed results restricting the result
   to the real or imaginary axis when the parameter X is appropriately
   restricted to easily testable portions of the domain. We follow the
   proposed ISO/IEC standard for Generic_Complex_Elementary_Functions (for Ada
   83), CD 13813,
   in not doing so, however.
@end{Discussion}

Other accuracy requirements for the complex elementary functions, which apply
only in the strict mode, are given in
@RefSecNum{Accuracy Requirements for Complex Arithmetic}.

The sign of a zero result or zero result component yielded by a complex
elementary function is implementation defined when
Complex_Types.Real'Signed_Zeros is True.
@ImplDef{The sign of a zero result (or a component thereof) from any operator
or function in Numerics.Generic_Complex_Elementary_Functions, when
Complex_Types.Real'Signed_Zeros is True.}
@end{ImplReq}

@begin{ImplPerm}
The nongeneric equivalent packages may, but need not, be actual
instantiations of the generic package with the appropriate predefined
nongeneric equivalent of Numerics.Generic_Complex_Types; if they are, then the
latter shall have been obtained by actual instantiation of
Numerics.Generic_Complex_Types.

The exponentiation operator may be implemented in terms of the Exp and Log
functions. Because this implementation yields poor accuracy in some parts of
the domain, no accuracy requirement is imposed on complex exponentiation.

@PDefn{unspecified}
The implementation of the Exp function of a complex parameter X is allowed to
raise the exception Constraint_Error, signaling overflow, when the real
component of X exceeds an unspecified threshold that
is approximately
@Log(@R[Complex_Types.Real'Safe_Last]).
This permission recognizes the impracticality of avoiding overflow in
the marginal
case that the exponential of the real component of X exceeds the safe range of
Complex_Types.Real but both components of the final result do not. Similarly,
the Sin and Cos (resp., Sinh and Cosh) functions are allowed to raise the
exception Constraint_Error, signaling overflow, when the absolute value of the
imaginary (resp., real) component of the parameter X exceeds an
unspecified threshold that is approximately
@Log(@R[Complex_Types.Real'Safe_Last]) +
@Log(2.0).
@PDefn{unspecified}
This permission
recognizes the impracticality of avoiding overflow in the marginal case that
the hyperbolic sine or cosine of the imaginary (resp., real) component of X
exceeds the safe range of Complex_Types.Real but both components of the final
result do not.
@end{ImplPerm}

@begin{ImplAdvice}
Implementations in which Complex_Types.Real'Signed_Zeros is True should attempt
to provide a rational treatment of the signs of zero results and result
components. For example, many of the complex elementary functions have
components that are odd functions of one of the parameter components; in these
cases, the result component should have the sign of the parameter component at
the origin. Other complex elementary functions have zero components whose sign
is opposite that of a parameter component at the origin, or is always positive
or always negative.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[If Complex_Types.Real'Signed_Zeros is true for
Numerics.@!Generic_@!Complex_@!Elementary_@!Functions,
a rational treatment of the signs of
zero results and result components should be provided.]}]}
@end{ImplAdvice}

@begin{DiffWord83}
@Leading@;The semantics of Numerics.@!Generic_@!Complex_@!Elementary_@!Functions
differs from Generic_@!Complex_@!Elementary_@!Functions as defined in
ISO/IEC CD 13814 (for Ada 83) in the following ways:
@begin{itemize}
   The generic package is a child unit of the package defining the
   Argument_Error exception.

   The proposed Generic_Complex_Elementary_Functions standard (for Ada 83)
   specified names for the nongeneric equivalents, if provided. Here, those
   nongeneric equivalents are required.

   The generic package imports an instance of Numerics.Generic_Complex_Types rather
   than a long list of individual types and operations exported by such an
   instance.

   The dependence of the imaginary component of the Sqrt and Log functions on
   the sign of a zero parameter component is tied to the value of
   Complex_Types.Real'Signed_Zeros.

   Conformance to accuracy requirements is conditional.
@end{itemize}
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0020],ARef=[AI95-00126-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Explicitly stated that the
  nongeneric equivalents of Generic_Complex_Elementary_Functions are pure.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00185-01]}
  @ChgAdded{Version=[2],Text=[Corrected various inconsistencies in the
  definition of the branch cuts.]}
@end{DiffWord95}


@LabeledSubClause{Complex Input-Output}

@begin{Intro}
The generic package Text_IO.Complex_IO defines procedures for the
formatted input and output of complex values. The generic actual parameter in
an instantiation of Text_IO.Complex_IO is an instance of
Numerics.Generic_Complex_Types for some floating point subtype. Exceptional
conditions are reported by raising the appropriate exception defined in
Text_IO.
@begin{ImplNote}
  An implementation of Text_IO.Complex_IO can
  be built around an instance of
  Text_IO.Float_IO for the base subtype of Complex_Types.Real, where
  Complex_Types is the generic formal package parameter of Text_IO.Complex_IO.
  There is no need for an implementation of Text_IO.Complex_IO to parse
  real values.
@end{ImplNote}
@end{Intro}

@begin{StaticSem}
@Leading@;The generic library package
Text_IO.Complex_IO has the following declaration:
@begin{Ramification}
  Because this is a child of Text_IO, the declarations of the visible
  part of Text_IO are directly visible within it.
@end{Ramification}
@begin{Example}
@key[with] Ada.Numerics.Generic_Complex_Types;
@key[generic]@ChildUnit{Parent=[Ada.Text_IO],Child=[Complex_IO]}
   @key[with] @key[package] Complex_Types @key[is]
         @key[new] Ada.Numerics.Generic_Complex_Types (<>);
@key[package] Ada.Text_IO.Complex_IO @key[is]


   @key[use] Complex_Types;

   @AdaObjDefn{Default_Fore} : Field := 2;
   @AdaObjDefn{Default_Aft}  : Field := Real'Digits - 1;
   @AdaObjDefn{Default_Exp}  : Field := 3;


   @key[procedure] @AdaSubDefn{Get} (File  : @key[in]  File_Type;
                  Item  : @key[out] Complex;
                  Width : @key[in]  Field := 0);
   @key[procedure] @AdaSubDefn{Get} (Item  : @key[out] Complex;
                  Width : @key[in]  Field := 0);


   @key[procedure] @AdaSubDefn{Put} (File : @key[in] File_Type;
                  Item : @key[in] Complex;
                  Fore : @key[in] Field := Default_Fore;
                  Aft  : @key[in] Field := Default_Aft;
                  Exp  : @key[in] Field := Default_Exp);
   @key[procedure] @AdaSubDefn{Put} (Item : @key[in] Complex;
                  Fore : @key[in] Field := Default_Fore;
                  Aft  : @key[in] Field := Default_Aft;
                  Exp  : @key[in] Field := Default_Exp);


@trailing@;   @key[procedure] @AdaSubDefn{Get} (From : @key[in]  String;
                  Item : @key[out] Complex;
                  Last : @key[out] Positive);
   @key[procedure] @AdaSubDefn{Put} (To   : @key[out] String;
                  Item : @key[in]  Complex;
                  Aft  : @key[in]  Field := Default_Aft;
                  Exp  : @key[in]  Field := Default_Exp);

@key[end] Ada.Text_IO.Complex_IO;
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00328-01]}
@ChgAdded{Version=[2],Text=[
@ChildUnit{Parent=[Ada],Child=[Complex_@!Text_IO]}
The library package Complex_Text_IO defines the
same subprograms as Text_IO.Complex_IO, except that the predefined type Float
is systematically substituted for Real, and the type
Numerics.Complex_Types.Complex is systematically substituted for Complex
throughout. Non-generic equivalents of Text_IO.Complex_IO corresponding to each
of the other predefined floating point types are defined similarly, with the
names Short_Complex_Text_IO, Long_Complex_Text_IO, etc.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[
   The nongeneric equivalents are provided to allow the programmer to
   construct simple mathematical applications without being required to
   understand and use generics.]}
@end{Reason}

The semantics of the Get and Put procedures are as follows:
@begin{DescribeCode}
@begin{Example}
@key[procedure] Get (File  : @key[in]  File_Type;
               Item  : @key[out] Complex;
               Width : @key[in]  Field := 0);
@key[procedure] Get (Item  : @key[out] Complex;
               Width : @key[in]  Field := 0);
@end{Example}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0092],ARef=[AI95-00029-01]}
@Leading@;The input sequence is a pair of
optionally signed real literals representing
the real and imaginary components of a complex value@Chg{New=[ These components
have the format defined for the corresponding Get procedure of an instance of
Text_IO.Float_IO (see @RefSecNum{Input-Output for Real Types}) for the base
subtype of Complex_Types.Real. T],Old=[; optionally, t]}he pair of components
may be separated by a comma @Chg{New=[],Old=[and/]}or surrounded by a pair of
parentheses@Chg{New=[ or both],Old=[]}. Blanks are freely allowed before each
of the components and before the parentheses and comma, if either is used.
If the value of the parameter Width is zero, then
@begin{Itemize}
   line and page terminators are also allowed in these places;

   the components shall be separated by at least one blank or line terminator
   if the comma is omitted; and

   reading stops when the right parenthesis has been read, if the input
   sequence includes a left parenthesis, or when the imaginary component has
   been read, otherwise.
@end{Itemize}

@ChgNote{The following paragraph is missing a number in the original version.
To give it a number in the new version, it is marked as an insertion.}
@ChgRef{Version=[0],Kind=[Added]}@Leading
@Chg{New=[],Old=[@Noparanum@;]}If a nonzero value of Width is supplied, then

@begin{Itemize}
   the components shall be separated by at least one blank if the comma is
   omitted; and

   exactly Width characters are read, or the characters (possibly none) up to
   a line terminator, whichever comes first (blanks are included in the count).
@end{Itemize}
@begin{Reason}
   The parenthesized and comma-separated form is the form produced by Put
   on output (see below), and also by list-directed output in Fortran. The
   other allowed forms match several common styles of edit-directed output in
   Fortran, allowing most preexisting Fortran data files containing complex
   data to be read easily. When such files contain complex values with no
   separation between the real and imaginary components, the user will have to
   read those components separately, using an instance of
   Text_IO.Float_IO.
@end{Reason}

Returns, in the parameter Item, the value of type Complex that corresponds to
the input sequence.

@Trailing@;The exception Text_IO.Data_Error
is raised if the input sequence
does not have the required syntax or if the components of the complex value
obtained are not of the base subtype of Complex_Types.Real.

@begin{Example}
@key[procedure] Put (File : @key[in] File_Type;
               Item : @key[in] Complex;
               Fore : @key[in] Field := Default_Fore;
               Aft  : @key[in] Field := Default_Aft;
               Exp  : @key[in] Field := Default_Exp);
@key[procedure] Put (Item : @key[in] Complex;
               Fore : @key[in] Field := Default_Fore;
               Aft  : @key[in] Field := Default_Aft;
               Exp  : @key[in] Field := Default_Exp);
@end{Example}

Outputs the value of the parameter Item as a pair of decimal literals
representing the real and imaginary components of the complex value,
using the syntax
of an aggregate.
More specifically,
@begin{itemize}
   outputs a left parenthesis;

   outputs the value of the real component of the parameter Item with the
   format defined by the corresponding Put procedure of an instance of
   Text_IO.Float_IO for the base subtype of Complex_Types.Real, using the given values of
   Fore, Aft, and Exp;

   outputs a comma;

   outputs the value of the imaginary component of the parameter Item with the
   format defined by the corresponding Put procedure of an instance of
   Text_IO.Float_IO for the base subtype of Complex_Types.Real, using the given values of
   Fore, Aft, and Exp;

@Trailing@;outputs a right parenthesis.
@end{itemize}
@begin{Discussion}
   If the file has a bounded line length, a line terminator may be output
   implicitly before any element of the sequence itemized above.
@end{Discussion}
@begin{Discussion}
   The option of outputting the complex value as a pair of reals without
   additional punctuation
   is not provided, since it can be
   accomplished by outputting the real and imaginary components of the complex
   value separately.
@end{Discussion}

@begin{Example}
@key[procedure] Get (From : @key[in]  String;
               Item : @key[out] Complex;
               Last : @key[out] Positive);
@end{Example}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
Reads a complex value from the beginning of the given string, following the
same rule as the Get procedure that reads a complex value from a file, but
treating the end of the string as a @Chg{Version=[2],New=[file],Old=[line]}
terminator. Returns, in the parameter
Item, the value of type Complex that corresponds to the input sequence.
Returns in Last the index value such that From(Last) is the last character
read.

@Trailing@;The exception Text_IO.Data_Error is raised if the input sequence
does not have the required syntax or if the components of the complex value
obtained are not of the base subtype of Complex_Types.Real.

@begin{Example}
@key[procedure] Put (To   : @key[out] String;
               Item : @key[in]  Complex;
               Aft  : @key[in]  Field := Default_Aft;
               Exp  : @key[in]  Field := Default_Exp);
@end{Example}

Outputs the value of the parameter Item to the given string as a pair of
decimal literals representing the real and imaginary components of the complex
value, using the syntax of an aggregate.
More specifically,
@begin{itemize}
   a left parenthesis, the real component, and a comma are left justified in
   the given string, with the real component having the format defined by the
   Put procedure (for output to a file) of an instance of Text_IO.Float_IO
   for the base subtype of Complex_Types.Real, using a value of zero for Fore and the given
   values of Aft and Exp;

   the imaginary component and a right parenthesis are right justified in the
   given string, with the imaginary component having the format defined by the
   Put procedure (for output to a file) of an instance of Text_IO.Float_IO
   for the base subtype of Complex_Types.Real, using a value for Fore that completely fills
   the remainder of the string, together with the given values of Aft and Exp.
@end{itemize}
@begin{Reason}
   This rule is the one proposed in LSN-1051. Other rules were considered,
   including one that would have read @lquotes@;Outputs the value of the parameter Item
   to the given string, following the same rule as for output to a file, using
   a value for Fore such that the sequence of characters output exactly fills,
   or comes closest to filling, the string; in the latter case, the string is
   filled by inserting one extra blank immediately after the comma.@rquotes@; While
   this latter rule might be considered the closest analogue to the rule for
   output to a string in Text_IO.Float_IO, it requires a more difficult and
   inefficient implementation involving special cases when the integer part of
   one component is substantially longer than that of the other and the string
   is too short to allow both to be preceded by blanks. Unless such a special
   case applies, the latter rule might produce better columnar output if
   several such strings are ultimately output to a file, but very nearly the
   same output can be produced by outputting to the file directly, with the
   appropriate value of Fore; in any case, it might validly be assumed that
   output to a string is intended for further computation rather than for
   display, so that the precise formatting of the string to achieve a
   particular appearance is not the major concern.
@end{Reason}

The exception Text_IO.Layout_Error is raised if the given string is
too short to hold the formatted output.
@end{DescribeCode}
@end{StaticSem}

@begin{ImplPerm}
Other exceptions declared (by renaming)
in Text_IO may be raised by the preceding procedures
in the appropriate circumstances, as for the corresponding
procedures of Text_IO.Float_IO.
@end{ImplPerm}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00328-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Nongeneric equivalents for Text_IO.Complex_IO are added, to be consistent
  with all other language-defined Numerics generic packages.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0092],ARef=[AI95-00029-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that the syntax
  of values read by Complex_IO is the same as that read by Text_IO.Float_IO.]}
@end{DiffWord95}


@LabeledSubClause{The Package Wide_Text_IO.Complex_IO}

@begin{StaticSem}
@Defn{Ada.Wide_@!Text_IO.Complex_IO}
@ChildUnit{Parent=[Ada.Wide_@!Text_IO],Child=[Complex_IO]}
Implementations shall also provide the generic library package
Wide_Text_IO.Complex_IO. Its declaration is obtained from that of
Text_IO.Complex_IO by systematically replacing Text_IO by Wide_Text_IO and
String by Wide_String; the description of its behavior is obtained by
additionally replacing references to particular characters (commas,
parentheses, etc.) by those for the corresponding wide characters.
@end{StaticSem}


@LabeledAddedSubClause{Version=[2],Name=[The Package Wide_Wide_Text_IO.Complex_IO]}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn{Ada.Wide_Wide_@!Text_IO.Complex_IO}
@ChildUnit{Parent=[Ada.Wide_Wide_@!Text_IO],Child=[Complex_IO]}
Implementations shall also provide the generic library package
Wide_Wide_Text_IO.Complex_IO. Its declaration is obtained from that of
Text_IO.Complex_IO by systematically replacing Text_IO by Wide_Wide_Text_IO and
String by Wide_Wide_String; the description of its behavior is obtained by
additionally replacing references to particular characters (commas,
parentheses, etc.) by those for the corresponding wide wide characters.]}
@end{StaticSem}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Package Wide_Wide_Text_IO.Complex_IO is new. (At least it wasn't called
  Incredibly_Wide_Text_IO.Complex_IO; maybe next time.)]}
@end{Extend95}


@LabeledClause{Numeric Performance Requirements}

@begin{ImplReq}
@Defn{accuracy}
@Defn{strict mode}
Implementations shall provide a user-selectable mode in which the accuracy
and other numeric performance requirements detailed in the following subclauses
are observed.
This mode,
referred to as the @i{strict mode}, may or may not be the default mode;
it directly affects the results of the predefined arithmetic operations of real
types and the results of the subprograms in
children of the Numerics package, and indirectly affects the operations in
other language defined packages.
@Defn{relaxed mode}
Implementations shall also provide the opposing mode, which is known as the
@i{relaxed mode}.
@begin{Reason}
   On the assumption that the users of an implementation that does not support
   the Numerics Annex have no particular need for numerical performance, such
   an implementation has no obligation to meet any particular requirements in
   this area. On the other hand, users of an implementation that does support
   the Numerics Annex are provided with a way of ensuring that their programs
   achieve a known level of numerical performance and that the performance is
   portable to other such implementations. The relaxed mode is provided to
   allow implementers to offer an efficient but not fully accurate alternative
   in the case that the strict mode entails a time overhead that some users may
   find excessive. In some of its areas of impact, the relaxed mode may be
   fully equivalent to the strict mode.
@end{Reason}
@begin{ImplNote}
   The relaxed mode may, for example, be used to exploit the implementation of
   (some of) the elementary functions in hardware, when available. Such
   implementations often do not meet the accuracy requirements of the strict
   mode, or do not meet them over the specified range of parameter values,
   but compensate in other ways that may be important to the user, such as
   their extreme speed.
@end{ImplNote}
@begin{Ramification}
   For implementations supporting the Numerics Annex,
   the choice of mode has no
   effect on the selection of a representation for a real type or on the values
   of attributes of a real type.
@end{Ramification}
@end{ImplReq}

@begin{ImplPerm}
Either mode may be the default mode.
@ImplDef{Whether the strict mode or the relaxed mode is the default.}

The two modes need not actually be different.
@end{ImplPerm}

@begin{Extend83}
@Defn{extensions to Ada 83}
The choice between strict and relaxed numeric performance was not available in
Ada 83.
@end{Extend83}


@RMNewPage@Comment{For printed version of Ada 2007 RM}
@LabeledSubClause{Model of Floating Point Arithmetic}

@begin{Intro}
In the strict mode, the predefined operations of a floating point type shall
satisfy the accuracy requirements specified here and shall avoid or signal
overflow in the situations described. This behavior is presented in terms of
a model of floating point arithmetic that builds on the concept of the
canonical form (see @RefSecNum{Attributes of Floating Point Types}).
@end{Intro}

@begin{StaticSem}
Associated with each floating point type is an infinite set of
model numbers. The model numbers of a type are used to define the
accuracy requirements that have to be satisfied by certain predefined
operations of the type; through certain attributes of
the model numbers, they are also used to explain
the meaning of a user-declared floating point type declaration.
The model numbers of a derived type are those of the
parent type; the model numbers of a subtype are those of its type.

@Defn{model number}
The @i{model numbers} of a floating point type T are zero and all the values
expressible in the canonical form (for the type T),
in which @i{mantissa} has T'Model_Mantissa digits
and @i{exponent} has a value greater than or equal
to T'Model_Emin.
(These attributes are defined in
@RefSecNum{Model-Oriented Attributes of Floating Point Types}.)
@begin{Discussion}
   The model is capable of describing the behavior of most existing hardware
   that has a mantissa-exponent representation. As applied to a type T, it is
   parameterized by the values of T'Machine_Radix, T'Model_Mantissa,
   T'Model_Emin, T'Safe_First, and T'Safe_Last. The values of these
   attributes are determined by how, and how well, the hardware behaves.
   They in turn determine the set of model numbers and the safe range of the
   type, which figure in the accuracy and range (overflow avoidance)
   requirements.

   In hardware that is free of arithmetic anomalies, T'Model_Mantissa,
   T'Model_Emin, T'Safe_First, and T'Safe_Last will yield the same values as
   T'Machine_Mantissa, T'Machine_Emin, T'Base'First, and T'Base'Last,
   respectively, and the
   model numbers in the safe range of the type T will coincide with the machine
   numbers of the type T. In less perfect hardware, it is not possible for the
   model-oriented attributes to have these optimal values, since the hardware,
   by definition, and therefore the implementation, cannot conform to the
   stringencies of the resulting model; in this case, the values yielded by the
   model-oriented parameters have to be made more conservative (i.e., have to
   be penalized), with the result that the model numbers are more widely
   separated than the machine numbers, and the safe range is a subrange of the
   base range. The implementation will then be able to conform to the
   requirements of the weaker model defined by the sparser set of model numbers
   and the smaller safe range.
@end{Discussion}

@Defn{model interval}
A @i(model interval) of a floating point type is any interval whose bounds
are model numbers of the type.
@Defn2{Term=[model interval],
        Sec=[associated with a value]}
The @i{model interval} of a type T @i{associated with a value} @i{v} is the
smallest model interval of T that includes @i{v}. (The model interval
associated with a model number of a type consists of that number only.)
@end{StaticSem}

@begin{ImplReq}
The accuracy requirements for the evaluation of certain predefined
operations of floating point types are as follows.
@begin{Discussion}
This subclause does not cover the accuracy of an operation of a static
expression; such operations
have to be evaluated exactly
(see @RefSecNum(Static Expressions and Static Subtypes)).
It also does not cover the accuracy of the predefined attributes of a
floating point subtype that yield a value of the type;
such operations also yield exact results
(see @RefSecNum(Operations of Floating Point Types)
and @RefSecNum(Attributes of Floating Point Types)).
@end{Discussion}

@Defn{operand interval}
An @i(operand interval) is the model interval, of the type specified for the
operand of an operation, associated with the value of the operand.

@Leading@;For any predefined arithmetic operation
that yields a result of a
floating point type T, the required bounds on the result are given by
a model interval of T (called the @i(result interval)) defined in terms of the
operand values as follows:
@begin(Itemize)
   @Defn2{Term=[result interval],
           Sec=[for the evaluation of a predefined arithmetic operation]}
   The result interval is the smallest model interval of T that includes
   the minimum and the maximum of all the values obtained by applying the
   (exact) mathematical operation to values arbitrarily selected from the
   respective operand intervals.
@end(Itemize)

The result interval of an exponentiation is obtained by applying the above rule
to the sequence of multiplications defined by the exponent, assuming arbitrary
association of the factors, and to the final division in the case of a negative
exponent.

The result interval of a conversion of a numeric value to a floating point type
T is the model interval of T associated with the operand value, except when the
source expression is of a fixed point type
with a @i(small) that is not a power
of T'Machine_Radix or is a fixed point multiplication or division either of
whose operands has a @i(small) that is not a power of T'Machine_Radix;
in these cases, the result interval is implementation defined.
@ImplDef{The result interval in certain cases of fixed-to-float conversion.}

@IndexCheck{Overflow_Check}
For any of the foregoing operations, the implementation shall deliver a value
that belongs to the result interval when both bounds of the result interval are
in the safe range of the result type T, as determined by the values of
T'Safe_First and T'Safe_Last; otherwise,
@begin(itemize)
   @Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
   if T'Machine_Overflows is True, the implementation shall either deliver a
   value that belongs to the result interval or raise Constraint_Error;

   if T'Machine_Overflows is False, the result is implementation defined.
   @ImplDef{The result of a floating point arithmetic operation in
   overflow situations, when the Machine_Overflows attribute of the result
   type is False.}
@end(itemize)

For any predefined relation on operands of a floating point type T, the
implementation may deliver any value (i.e., either True or False) obtained by
applying the (exact) mathematical comparison to values arbitrarily chosen from
the respective operand intervals.

The result of a membership test is defined in terms of comparisons of the
operand value with the lower and upper bounds of the given range or type
mark (the usual rules apply to these comparisons).
@end{ImplReq}

@begin{ImplPerm}
If the underlying floating point hardware implements division
as multiplication by a reciprocal, the result interval
for division (and exponentiation by a negative exponent) is
implementation
defined.
@ImplDef{The result interval for division (or exponentiation by a
negative exponent), when the floating point hardware implements division as
multiplication by a reciprocal.}
@end{ImplPerm}

@begin{DiffWord83}
The Ada 95 model numbers of a floating point type that are in the safe range of
the type are comparable to the Ada 83 safe numbers of the type. There is no
analog of the Ada 83 model numbers. The Ada 95 model numbers, when not
restricted to the safe range, are an infinite set.
@end{DiffWord83}

@begin{Inconsistent83}
@Defn{inconsistencies with Ada 83}
Giving the model numbers the hardware radix, instead of always a radix of two,
allows (in conjunction with other changes) some borderline declared
types to be represented with less precision than in Ada 83 (i.e., with single
precision, whereas Ada 83 would have used double precision). Because the lower
precision satisfies the requirements of the model (and did so in Ada 83 as
well), this change is viewed as a desirable correction of an anomaly, rather
than a worrisome inconsistency. (Of course, the wider representation chosen in
Ada 83 also remains eligible for selection in Ada 95.)

As an example of this phenomenon, assume that Float is represented in single
precision and that a double precision type is also available. Also assume
hexadecimal hardware with clean properties, for example certain IBM hardware.
Then,
@begin{Example}
@key[type] T @key[is] @key[digits] Float'Digits @key[range] -Float'Last .. Float'Last;
@end{Example}

results in T being represented in double precision in Ada 83 and in single
precision in Ada 95. The latter is intuitively correct; the former is
counterintuitive. The reason why the double precision type is used in Ada 83
is that Float has model and safe numbers (in Ada 83) with 21 binary digits in
their mantissas, as is required to model the hypothesized
hexadecimal hardware using a binary
radix; thus Float'Last, which is not a model number, is slightly outside the
range of safe numbers of the single precision type, making that type ineligible
for selection as the representation of T even though it provides adequate
precision. In Ada 95, Float'Last (the same value as before) is a model number
and is in the safe range of Float on the hypothesized hardware, making Float
eligible for the representation of T.
@end{Inconsistent83}

@begin{Extend83}
@Defn{extensions to Ada 83}
Giving the model numbers the hardware radix allows for practical
implementations on decimal hardware.
@end{Extend83}

@begin{DiffWord83}
The wording of the model of floating point arithmetic has been simplified to a
large extent.
@end{DiffWord83}


@LabeledSubClause{Model-Oriented Attributes of Floating Point Types}

@begin{Intro}
In implementations that support the Numerics Annex, the model-oriented
attributes of floating point types shall yield the values defined here,
in both the strict and the relaxed modes.
These definitions add conditions to those in
@RefSecNum{Attributes of Floating Point Types}.
@end{Intro}

@begin{StaticSem}
@Leading@keepnext@;For every subtype S of a floating point type @i{T}:
@begin{Description}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00256-01]}
S'@Attr{Model_Mantissa} @\Yields the number of digits in the mantissa of
the canonical form of the model numbers of @i{T}
(see @RefSecNum{Attributes of Floating Point Types}). The
value of this attribute shall be greater than or equal to@Chg{Version=[2],New=[],
Old=[ @Ceiling{@RI{d} @Times @Log(10) / @Log(@RI{T}'@R{Machine_Radix})} + 1, where @RI{d}
is the requested decimal precision of @i{T}. In addition, it
shall be less than or equal to the value of
@i{T}'Machine_Mantissa. This attribute yields a value of the
type @i{universal_integer}.]}
@begin{Display}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@Ceiling{@RI{d} @Times @Log(10) / @Log(@RI{T}'@R{Machine_Radix})} + @RI{g}]}
@end{Display}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],NoPrefix=[T],Text=[where @RI{d}
is the requested decimal precision of @i{T}, and @RI{g} is 0 if
@i{T}'Machine_Radix
is a positive power of 10 and 1 otherwise. In addition, @i{T}'Model_Mantissa
shall be less than or equal to the value of
@i{T}'Machine_Mantissa. This attribute yields a value of the
type @i{universal_integer}.]}
  @begin{Ramification}
     S'Model_Epsilon, which is defined in terms of S'Model_Mantissa
     (see @RefSecNum{Attributes of Floating Point Types}), yields the
     absolute value of the difference between one and the next model number of
     the type @i{T} above one.
     It is equal to or larger than the absolute value of the difference
     between one and the next machine number of the type @i{T} above
     one.
  @end{Ramification}

S'@Attr{Model_Emin} @\Yields the minimum exponent of the canonical form
of the model numbers of @i{T}
(see @RefSecNum{Attributes of Floating Point Types}). The value of this
attribute shall be greater than or equal to the value of
@i{T}'Machine_Emin. This attribute yields a value of the type
@i{universal_integer}.
  @begin{Ramification}
     S'Model_Small, which is defined in terms of S'Model_Emin
     (see @RefSecNum{Attributes of Floating Point Types}), yields the
     smallest positive (nonzero) model number of the type @i{T}.
  @end{Ramification}

S'@Attr{Safe_First} @\Yields the lower bound of the safe range of @i{T}.
The value of this attribute shall be a model number of @i{T} and greater
than or equal to the lower bound of the base range of @i{T}.
In addition, if @i{T} is declared by a
@nt{floating_point_definition} or is derived from such a type,
and the @nt{floating_point_definition} includes a
@nt{real_range_specification} specifying a lower bound of @RI{lb},
then the value of this attribute shall be less than or
equal to @RI{lb}; otherwise, it shall be less than or equal to
@en@;10.0 @+[4 @Times @RI{d}], where @RI{d} is the requested decimal precision
of @i{T}. This attribute yields a value of the type
@i{universal_real}.

S'@Attr{Safe_Last} @\Yields the upper bound of the safe range of @i{T}.
The value of this attribute shall be a model number of @i{T} and less
than or equal to the upper bound of the base range of @i{T}.
In addition, if @i{T} is declared by a
@nt{floating_point_definition} or is derived from such a type,
and the @nt{floating_point_definition} includes a
@nt{real_range_specification} specifying an upper bound of @RI{ub},
then the value of this attribute shall be greater than or
equal to @RI{ub}; otherwise, it shall be greater than or equal
to 10.0 @+[4 @Times @RI{d}], where d is the requested decimal
precision of @i{T}. This attribute yields a value of the type
@i{universal_real}.

@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}S'@Attr{Model} @\Denotes a function (of a parameter @i{X}) whose specification
is given in
@RefSecNum{Attributes of Floating Point Types}.
If @i{X} is a model number of @i{T}, the
function yields @i{X}; otherwise, it yields the value obtained
by rounding or truncating @i{X} to either one of the adjacent
model numbers of @i{T}.
@IndexCheck{Overflow_Check}Constraint_Error is raised if the
resulting model number is outside the safe range of S. A
zero result has the sign of @i{X} when S'Signed_Zeros is True.
@end{Description}

@Leading@;Subject to the constraints given above, the values of S'Model_Mantissa and
S'Safe_Last are to be maximized, and the values of S'Model_Emin and
S'Safe_First minimized, by the implementation as follows:
@begin{Itemize}
First, S'Model_Mantissa is set to the largest value for which values
of S'Model_Emin, S'Safe_First, and S'Safe_Last can be chosen so that
the implementation satisfies the strict-mode requirements of
@RefSecNum{Model of Floating Point Arithmetic} in
terms of the model numbers and safe range induced by these attributes.

Next, S'Model_Emin is set to the smallest value for which values of
S'Safe_First and S'Safe_Last can be chosen so that the implementation
satisfies the strict-mode requirements of
@RefSecNum{Model of Floating Point Arithmetic} in terms of the model
numbers and safe range induced by these attributes and the previously
determined value of S'Model_Mantissa.

Finally, S'Safe_First and S'Safe_last are set (in either order) to the
smallest and largest values, respectively, for which the
implementation satisfies the strict-mode requirements of
@RefSecNum{Model of Floating Point Arithmetic} in
terms of the model numbers and safe range induced by these attributes
and the previously determined values of S'Model_Mantissa and
S'Model_Emin.
@end{Itemize}

@begin{Ramification}

@Defn{IEEE floating point arithmetic}
@Defn{IEC 559:1989}
The following table shows appropriate attribute values for IEEE basic
single and double precision types (ANSI/IEEE Std 754-1985, IEC 559:1989).
Here, we use the names IEEE_Float_32 and IEEE_Float_64,
the names that would typically be declared in package Interfaces,
in an implementation that supports IEEE arithmetic.
In such an implementation,
the attributes would typically be the same for Standard.Float and
Long_Float, respectively.
@begin{Example}
Attribute                        IEEE_Float_32                 IEEE_Float_64

'Machine_Radix                               2                             2
'Machine_Mantissa                           24                            53
'Machine_Emin                             -125                         -1021
'Machine_Emax                              128                          1024
'Denorm                                   True                          True
'Machine_Rounds                           True                          True
'Machine_Overflows                  True/False                    True/False
'Signed_Zeros                   should be True                should be True

'Model_Mantissa    (same as 'Machine_Mantissa)   (same as 'Machine_Mantissa)
'Model_Emin            (same as 'Machine_Emin)       (same as 'Machine_Emin)
'Model_Epsilon                      2.0**(-23)                    2.0**(-52)
'Model_Small                       2.0**(-126)                  2.0**(-1022)
'Safe_First         -2.0**128*(1.0-2.0**(-24))   -2.0**1024*(1.0-2.0**(-53))
'Safe_Last           2.0**128*(1.0-2.0**(-24))    2.0**1024*(1.0-2.0**(-53))

'Digits                                      6                            15
'Base'Digits                 (same as 'Digits)             (same as 'Digits)

'First                   (same as 'Safe_First)         (same as 'Safe_First)
'Last                     (same as 'Safe_Last)          (same as 'Safe_Last)
'Size                                       32                            64
@end{Example}

Note: 'Machine_Overflows can be True or False, depending on whether the Ada
implementation raises Constraint_Error or delivers a signed infinity in
overflow and zerodivide situations (and at poles of the elementary functions).

@end{Ramification}
@end{StaticSem}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00256-01]}
  @ChgAdded{Version=[2],Text=[Corrected the definition of Model_Mantissa to
  match that given in @RefSecNum{Operations of Floating Point Types}.]}
@end{DiffWord95}



@LabeledSubClause{Model of Fixed Point Arithmetic}

@begin{Intro}
In the strict mode, the predefined arithmetic operations of a fixed point type
shall satisfy the accuracy requirements specified here and shall avoid or
signal overflow in the situations described.
@end{Intro}

@begin{ImplReq}
The accuracy requirements for the predefined fixed point arithmetic operations
and conversions, and the results of relations on fixed point operands, are
given below.
@begin{Discussion}
This subclause does not cover the accuracy of an operation of a static
expression; such operations
have to be evaluated exactly
(see @RefSecNum(Static Expressions and Static Subtypes)).
@end{Discussion}

The operands of the fixed point adding operators, absolute value,
and comparisons have the same type. These operations are required to
yield exact results, unless they overflow.

Multiplications and divisions are allowed between operands of any two
fixed point types; the result has to be (implicitly or explicitly)
converted to some other numeric type.
For purposes of defining the accuracy rules, the multiplication or
division and the conversion are treated
as a single operation whose accuracy depends on three types (those
of the operands and the result).
For decimal fixed point types, the
attribute T'Round may be used to imply explicit conversion with
rounding (see @RefSecNum(Operations of Fixed Point Types)).

When the result type is a floating point type, the accuracy is
as given in @RefSecNum(Model of Floating Point Arithmetic).
@Defn{perfect result set}
For some combinations of the operand and result types in the remaining cases,
the result is required to belong to a small set of values called the
@i(perfect result set);
@Defn{close result set}
for other combinations, it is required merely to belong to a
generally larger and implementation-defined set of values called the
@i(close result set).
When the result type is a decimal fixed point type, the perfect result set
contains a single value; thus, operations on decimal types are always
fully specified.
@ImplDef{The definition of @i{close result set}, which determines the
accuracy of certain fixed point multiplications and
divisions.}

When one operand of a fixed-fixed multiplication or division is of type
@i(universal_real), that operand is not implicitly converted in the usual sense,
since the context does not determine a unique target type, but the accuracy of
the result of the multiplication or division (i.e., whether the result has to
belong to the perfect result set or merely the close result set) depends on the
value of the operand of type @i(universal_real) and on the types of the other
operand and of the result.
@begin{Discussion}
We need not consider here the multiplication or
division of two such operands, since in that case either the operation is
evaluated exactly (i.e., it is an operation of a static expression all of whose
operators are of a root numeric type) or it is considered to be an operation of
a floating point type.
@end{Discussion}

@Leading@;For a fixed point multiplication or division whose (exact)
mathematical result is @RI{v}, and for the conversion of a value
@RI{v} to a fixed point type, the perfect result set and close result set
are defined as follows:
@begin(itemize)
      @Leading@Keepnext@;If the result type is an ordinary fixed point
      type with a @i(small) of @RI{s},
      @begin(InnerItemize)
         if @RI{v} is an integer multiple of
         @RI{s},
         then the perfect result set contains only the value
         @RI{v};

         otherwise, it contains the integer multiple of
         @RI{s} just below
         @RI{v} and the
         integer multiple of @RI{s} just above
         @RI{v}.
      @end(InnerItemize)

      @NoPrefix@;The close result set is an implementation-defined set of consecutive
      integer multiples of @RI{s} containing the perfect
      result set as a subset.

      @Leading@Keepnext@;If the result type is a decimal type with a @i(small) of
      @RI{s},
      @begin(InnerItemize)
         if @RI{v} is an integer multiple of
         @RI{s},
         then the perfect result set contains
         only the value @RI{v};

         otherwise, if truncation applies then it contains only the integer
         multiple of @RI{s} in the direction toward zero,
         whereas if rounding
         applies then it contains only the nearest integer multiple of
         @RI{s} (with
         ties broken by rounding away from zero).
      @end(InnerItemize)

      @NoPrefix@;The close result set is an implementation-defined set of consecutive
      integer multiples of @RI{s} containing the perfect
      result set as a subset.
      @begin{Ramification}
        As a consequence of subsequent rules, this case does not arise
        when the operand types are also decimal types.
      @end{Ramification}

      @Leading@Keepnext@;If the result type is an integer type,
      @begin(InnerItemize)
         if @RI{v} is an integer,
         then the perfect result set contains only the
         value @RI{v};

         otherwise, it contains the integer nearest to the value
         @RI{v} (if @RI{v} lies
         equally distant from two consecutive integers, the perfect result set
         contains the one that is further from zero).
      @end(InnerItemize)

      @NoPrefix@;The close result set is an implementation-defined set of consecutive
      integers containing the perfect result set as a subset.
@end(itemize)

The result of a fixed point multiplication or division shall belong either to
the perfect result set or to the close result set, as described below, if
overflow does not occur. In the following
cases, if the result type is a fixed point type,
let @RI{s} be its @i(small);
otherwise, i.e. when the result type is an integer type,
let @RI{s} be 1.0.
@begin(itemize)
   For a multiplication or division neither of whose operands is of type
   @i(universal_real), let @RI{l} and @RI{r}
   be the @i(smalls) of the left and right
   operands. For a multiplication, if (@RI{l} @Times @RI{r}) / @RI{s}
   is an integer or the
   reciprocal of an integer (the @i(smalls) are said to be @lquotes@;compatible@rquotes@; in
   this case), the result shall belong to the perfect result set; otherwise, it
   belongs to the close result set. For a division, if
   @RI{l} / (@RI{r} @Times @RI{s}) is an
   integer or the reciprocal of an integer (i.e., the @i(smalls) are
   compatible), the result shall belong to the perfect result set; otherwise,
   it belongs to the close result set.
   @begin{Ramification}
      When the operand and result types are all decimal types, their @i(smalls)
      are necessarily compatible; the same is true when they are all ordinary
      fixed point types with binary @i(smalls).
   @end{Ramification}

   For a multiplication or division having one @i(universal_real) operand with
   a value of @RI{v},
   note that it is always possible to factor
   @RI{v} as an integer
   multiple of a @lquotes@;compatible@rquotes@; @i(small), but the integer multiple may be
   @lquotes@;too big.@rquotes@;
   If there exists a factorization in which that multiple is less than some
   implementation-defined limit, the result shall belong to the perfect result
   set; otherwise, it belongs to the close result set.
   @ImplDef{Conditions on a @i{universal_real} operand of a fixed point
   multiplication or division for which the result shall be in the @i{perfect
   result set}.}
@end(itemize)

A multiplication P * Q of an operand of a fixed point type F by an operand of
an integer type I, or vice-versa, and a division P / Q of an operand of a
fixed point type F by an operand of an integer type I, are also allowed.
In these cases, the result has a type of F; explicit conversion of the
result is never required. The accuracy required in these cases is the same as
that required for a multiplication F(P * Q) or a division F(P / Q) obtained by
interpreting the operand of the integer type to have a fixed point type with a
@i(small) of 1.0.

The accuracy of the result of a conversion from an integer or fixed point type
to a fixed point type, or from a fixed point type to an integer
type, is the same as that of a fixed point multiplication of the source value
by a fixed point operand having a @i(small) of 1.0 and a value of 1.0, as given
by the foregoing rules. The result of a conversion from a floating point type
to a fixed point type shall belong to the close result set.
The result of a conversion of a @i(universal_real) operand to a fixed point
type shall belong to the perfect result set.

The possibility of overflow in the result of a predefined arithmetic operation
or conversion yielding a result of a fixed point type T is analogous to that
for floating point types, except for being related to the base range instead of
the safe range.
@IndexCheck{Overflow_Check}
If all of the permitted results belong to the base range of T,
then the implementation shall deliver one of the permitted results; otherwise,
@begin(itemize)
   @Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
   if T'Machine_Overflows is True, the implementation shall either deliver one
   of the permitted results or raise Constraint_Error;

   if T'Machine_Overflows is False, the result is implementation defined.
   @ImplDef{The result of a fixed point arithmetic operation in overflow
   situations, when the Machine_Overflows attribute of the result type is
   False.}
@end(itemize)
@end{ImplReq}

@begin{Inconsistent83}
@Defn{inconsistencies with Ada 83}
Since the values of a fixed point type are now just the integer multiples of
its @i{small}, the possibility of using extra bits available in the chosen
representation for extra accuracy rather than for increasing the base range
would appear to be removed, raising the possibility that some fixed point
expressions will yield less accurate results than in Ada 83. However, this is
partially offset by the ability of an implementation to choose a smaller
default @i{small} than before. Of course, if it does so for a type T then
T'Small will have a different value than it previously had.

The accuracy requirements in the case of incompatible @i{smalls} are relaxed to
foster wider support for non-binary @i{smalls}. If this relaxation is
exploited for a type that was previously supported, lower accuracy could
result; however, there is no particular incentive to exploit the relaxation in
such a case.
@end{Inconsistent83}

@begin{DiffWord83}
@Leading@;The fixed point accuracy requirements are now expressed without reference to
model or safe numbers, largely because the full generality of the former model
was never exploited in the case of fixed point types (particularly in regard to
operand perturbation). Although the new formulation in terms of perfect result
sets and close result sets is still verbose, it can be seen to distill down to
two cases:
@begin{Itemize}
   a case where the result must be the exact result, if the exact result is
   representable, or, if not, then either one of the adjacent values of the
   type (in some subcases only one of those adjacent values is allowed);

   a case where the accuracy is not specified by the language.
@end{Itemize}
@end{DiffWord83}


@LabeledSubClause{Accuracy Requirements for the Elementary Functions}

@begin{Intro}
In the strict mode, the performance of Numerics.Generic_Elementary_Functions
shall be as specified here.
@end{Intro}

@begin{ImplReq}
@Defn2{Term=[result interval],
        Sec=[for the evaluation of an elementary function]}
@Defn2{Term=[maximum relative error],
        Sec=[for the evaluation of an elementary function]}
When an exception is not raised, the result of evaluating a function in an
instance @i{EF} of Numerics.Generic_Elementary_Functions belongs to a @i{result
interval}, defined as the smallest model interval of @i{EF}.Float_Type that
contains all the values of the form @RI{f} @Times (1.0 + @RI{d}), where @RI{f} is the
exact value of the corresponding mathematical function at the given parameter
values, @RI{d} is a real number, and @Abs[@RI{d}] is less than or equal to
the function's @i{maximum relative error}.
@IndexCheck{Overflow_Check}
The function delivers a value that belongs to the result interval when both of
its bounds belong to the safe range of @i{EF}.Float_Type; otherwise,
@begin{Itemize}
   @Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
   if @i{EF}.Float_Type'Machine_Overflows is True, the function either delivers
   a value that belongs to the result interval or raises Constraint_Error,
   signaling overflow;

   @Trailing@;if @i{EF}.Float_Type'Machine_Overflows is False, the result is
   implementation defined.
   @ImplDef{The result of an elementary function reference in overflow
   situations, when the Machine_Overflows attribute of the result type is
   False.}
@end{Itemize}

@Leading@;The maximum relative error exhibited by each function is as follows:
@begin{Itemize}
   2.0 @Times @RI{EF}@R[.Float_Type'Model_Epsilon], in the case of the Sqrt, Sin,
   and Cos functions;

   4.0 @Times @RI{EF}@R[.Float_Type'Model_Epsilon], in the case of the Log, Exp,
   Tan, Cot, and inverse trigonometric functions; and

   8.0 @Times @RI{EF}@R[.Float_Type'Model_Epsilon], in the case of the forward and
   inverse hyperbolic functions.
@end{Itemize}

The maximum relative error exhibited by the exponentiation operator, which
depends on the values of the operands, is
(4.0 + @Abs{@R[Right] @Times @Log(@R[Left])} / 32.0) @Times
@RI{EF}@R[.Float_Type'Model_Epsilon].

The maximum relative error given above applies throughout the domain of
the forward trigonometric functions when the Cycle parameter is specified.
@Defn{angle threshold}
When the Cycle parameter is omitted, the maximum relative error given above
applies only when the absolute value of the angle parameter X is less than or
equal to some implementation-defined @i{angle threshold}, which shall be at
least
@RI{EF}@R[.Float_@!Type'Machine_@!Radix] @+<@Floor(@RI{EF}@R[.Float_@!Type'Machine_@!Mantissa]/2)>.
Beyond the angle threshold, the accuracy of the forward trigonometric functions
is implementation defined.
@ImplDef{The value of the @i{angle threshold}, within which certain elementary
functions, complex arithmetic operations, and complex elementary functions
yield results conforming to a maximum relative error bound.}
@ImplDef{The accuracy of certain elementary functions for parameters beyond the
angle threshold.}
@begin{ImplNote}
   The angle threshold indirectly determines the amount of precision that the
   implementation has to maintain during argument reduction.
@end{ImplNote}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
The prescribed results specified in @RefSecNum{Elementary Functions} for certain
functions at particular parameter values take precedence over the maximum
relative error bounds; effectively, they narrow to a single value the result
interval allowed by the maximum relative error bounds. Additional rules with a
similar effect are given by @Chg{Version=[2],New=[],Old=[the ]}table
@Chg{Version=[2],New=[G-1],Old=[below]} for the inverse trigonometric
functions, at particular parameter values for which the mathematical result is
possibly not a model number of @i{EF}.Float_Type (or is, indeed, even
transcendental). In each table entry, the values of the parameters are such
that the result lies on the axis between two quadrants; the corresponding
accuracy rule, which takes precedence over the maximum relative error bounds,
is that the result interval is the model interval of @i{EF}.Float_Type
associated with the exact mathematical result given in the table.

@ChgRef{Version=[1], Kind=[Deleted]}
@Chg[New=<>,Old=<@ @;@comment{Empty paragraph to hang junk paragraph number (12) from original RM}>]

The last line of the table is meant to apply when
@i{EF}.Float_Type'Signed_Zeros is False; the two lines just above it, when
@i{EF}.Float_Type'Signed_Zeros is True and the parameter Y has a zero value
with the indicated sign.

@Table[Columns=<5>,Alignment=<AllCenter>,FirstColWidth=[2],LastColWidth=[1],
NoBreak=[T],Border=[T],SmallSize=[F],
Caption=<@b{@Chg{Version=[2],New=[Table G-1: ],Old=[]}Tightly Approximated Elementary Function Results}>,
Headers=<@b{Function}@\@b{Value of X}@\@b{Value of Y}@\@b{Exact Result @*when Cycle @*Specified}@\@b{Exact Result @*when Cycle @*Omitted}>,
Body=<Arcsin@\1.0@\n.a.@\Cycle/4.0@\@Pi/2.0
Arcsin@\@en@;1.0@\n.a.@\@en@R[Cycle]/4.0@\@en@Pi/2.0
Arccos@\0.0@\n.a.@\Cycle/4.0@\@Pi/2.0
Arccos@\@en@;1.0@\n.a.@\Cycle/2.0@\@Pi
Arctan and Arccot@\0.0@\positive@\Cycle/4.0@\@Pi/2.0
Arctan and Arccot@\0.0@\negative@\@en@R[Cycle]/4.0@\@en@Pi/2.0
Arctan and Arccot@\negative@\+0.0@\Cycle/2.0@\@Pi
Arctan and Arccot@\negative@\@en@;0.0@\@en@R[Cycle]/2.0@\@en@Pi@Last
Arctan and Arccot@\negative@\0.0@\Cycle/2.0@\@Pi>]
The amount by which the result of an inverse trigonometric function is allowed
to spill over into a quadrant adjacent to the one corresponding to the
principal branch, as given in @RefSecNum{Elementary Functions}, is limited.
The rule is that the result belongs to the smallest model interval of
@i{EF}.Float_Type that contains both boundaries of the quadrant corresponding
to the principal branch. This rule also takes precedence over the maximum
relative error bounds, effectively narrowing the result interval allowed by
them.

@Comment{For Ada 95 with Corr, table G-1 appears here}

@Leading@;Finally, the following specifications also take precedence over the maximum
relative error bounds:
@begin{Itemize}
   The absolute value of the result of the Sin, Cos, and Tanh functions never
   exceeds one.

   The absolute value of the result of the Coth function is never less than
   one.

   The result of the Cosh function is never less than one.
@end{Itemize}
@end{ImplReq}

@begin{ImplAdvice}
The versions of the forward trigonometric functions without a Cycle parameter
should not be implemented by calling the corresponding version with a Cycle
parameter of 2.0*Numerics.Pi, since this will not provide the required accuracy
in some portions of the domain. For the same reason, the version of Log
without a Base parameter should not be implemented by calling the corresponding
version with a Base parameter of Numerics.e.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[For elementary functions, the forward trigonometric functions without a
Cycle parameter should not be implemented by calling the corresponding version
with a Cycle parameter. Log without a Base parameter should not be implemented
by calling Log with a Base parameter.]}]}
@end{ImplAdvice}

@begin{DiffWord83}
@Leading@;The semantics of Numerics.Generic_Elementary_Functions differs from
Generic_Elementary_Functions as defined in ISO/IEC DIS 11430 (for Ada 83) in
the following ways related to the accuracy specified for strict mode:
@begin{Itemize}
   The maximum relative error bounds use the Model_Epsilon attribute instead of
   the Base'Epsilon attribute.

   The accuracy requirements are expressed in terms of result intervals that
   are model intervals. On the one hand, this facilitates the description of
   the required results in the presence of underflow; on the other hand, it
   slightly relaxes the requirements expressed in ISO/IEC DIS 11430.
@end{Itemize}
@end{DiffWord83}


@LabeledSubClause{Performance Requirements for Random Number Generation}

@begin{Intro}
In the strict mode, the performance of Numerics.Float_Random and
Numerics.Discrete_Random shall be as specified here.
@end{Intro}

@begin{ImplReq}
Two different calls to the time-dependent Reset procedure shall reset the
generator to different states, provided that the calls are separated in time by
at least one second and not more than fifty
years.

The implementation's representations of generator states and its algorithms for
generating random numbers shall yield a period of at least 2@+{31}@en@;2;
much longer periods are desirable but not required.

The implementations of Numerics.Float_Random.Random and
Numerics.Discrete_Random.Random shall pass at least 85% of the individual
trials in a suite of statistical tests. For Numerics.Float_Random, the tests
are applied directly to the floating point values generated (i.e., they are not
converted to integers first), while for Numerics.Discrete_Random they are
applied to the generated values of various discrete types. Each test suite
performs 6 different tests, with each test repeated 10 times, yielding a total
of 60 individual trials. An individual trial is deemed to pass if the
chi-square value (or other statistic) calculated for the observed counts or
distribution falls within the range of values corresponding to the 2.5 and 97.5
percentage points for the relevant degrees of freedom (i.e., it shall be
neither too high nor too low). For the purpose of determining the degrees of
freedom, measurement categories are combined whenever the expected counts are
fewer than 5.
@begin{ImplNote}
   In the floating point random number test suite, the generator is reset to a
   time-dependent state at the beginning of the run. The test suite
   incorporates the following tests, adapted from D. E. Knuth, @i{The Art of
   Computer Programming, vol. 2: Seminumerical Algorithms.}  In the
   descriptions below, the given number of degrees of freedom is the number
   before reduction due to any necessary combination of measurement categories
   with small expected counts; it is one less than the number of measurement
   categories.
   @begin{itemize}
      Proportional Distribution Test (a variant of the Equidistribution Test).
      The interval 0.0 .. 1.0 is partitioned into @RI{K} subintervals.
      @RI{K} is chosen randomly between 4 and 25 for each repetition of the
      test, along with the boundaries of the subintervals (subject to the
      constraint that at least 2 of the subintervals have a width of 0.001 or
      more). 5000 random floating point numbers are generated. The counts of
      random numbers falling into each subinterval are tallied and compared
      with the expected counts, which are proportional to the widths of the
      subintervals. The number of degrees of freedom for the chi-square test
      is @RI{K}@en@;1.

      Gap Test. The bounds of a range @RI{A} .. @RI{B}, with
      0.0 @leq @RI{A} @Lt @RI{B} @leq 1.0, are chosen randomly for each repetition
      of the test, subject to the constraint that 0.2 @leq @RI{B}@en@RI{A} @leq 0.6.
      Random floating point numbers are generated until 5000 falling into the
      range @RI{A} .. @RI{B} have been encountered. Each of these 5000 is
      preceded by a @lquotes@;gap@rquotes@; (of length greater than or equal to 0) of
      consecutive random numbers not falling into the range
      @RI{A} .. @RI{B}. The counts of gaps of each length from 0 to 15,
      and of all lengths greater than 15 lumped together, are tallied and
      compared with the expected counts. Let @RI{P} = @RI{B}@en@RI{A}. The
      probability that a gap has a length of @RI{L} is (1@en@RI{P}) @+[@RI{L}]
      @Times @RI{P} for @RI{L} @leq 15, while the probability that a gap has a
      length of 16 or more is (1@en@RI{P}) @+[16]. The number of degrees of
      freedom for the chi-square test is 16.

      Permutation Test. 5000 tuples of 4 different random floating point
      numbers are generated. (An entire 4-tuple is discarded in the unlikely
      event that it contains any two exactly equal components.) The counts of
      each of the 4! = 24 possible relative orderings of the
      components of the 4-tuples are tallied and compared with the expected
      counts. Each of the possible relative orderings has an equal
      probability. The number of degrees of freedom for the chi-square test
      is 23.

      Increasing-Runs Test. Random floating point numbers are generated until
      5000 increasing runs have been observed. An @lquotes@;increasing run@rquotes@; is a
      sequence of random numbers in strictly increasing order; it is followed
      by a random number that is strictly smaller than the preceding random
      number. (A run under construction is entirely discarded in the unlikely
      event that one random number is followed immediately by an exactly equal
      random number.) The decreasing random number that follows an increasing
      run is discarded and not included with the next increasing run. The
      counts of increasing runs of each length from 1 to 4, and of all lengths
      greater than 4 lumped together, are tallied and compared with the
      expected counts. The probability that an increasing run has a length of
      @RI{L} is 1/@RI{L}! @en 1/(@RI{L}+1)! for @RI{L} @leq 4, while
      the probability that an increasing run has a length of 5 or more is
      1/5!. The number of degrees of freedom for the chi-square test
      is 4.

      Decreasing-Runs Test. The test is similar to the Increasing Runs Test,
      but with decreasing runs.

      Maximum-of-@RI{t} Test (with @RI{t} = 5). 5000 tuples of
      5 random floating point numbers are generated. The maximum of the
      components of each 5-tuple is determined and raised to the 5th power.
      The uniformity of the resulting values over the range 0.0 .. 1.0 is
      tested as in the Proportional Distribution Test.
   @end{itemize}
@end{ImplNote}
@begin{ImplNote}
   In the discrete random number test suite, Numerics.Discrete_Random is
   instantiated as described below. The generator is reset to a time-dependent
   state after each instantiation. The test suite incorporates the following
   tests, adapted from D. E. Knuth (@i{op. cit.}) and other sources. The given
   number of degrees of freedom for the chi-square test is reduced by any
   necessary combination of measurement categories with small expected counts,
   as described above.
   @begin{Itemize}
      Equidistribution Test. In each repetition of the test, a number @RI{R}
      between 2 and 30 is chosen randomly, and Numerics.Discrete_Random is
      instantiated with an integer subtype whose range is 1 .. @RI{R}. 5000
      integers are generated randomly from this range. The counts of
      occurrences of each integer in the range are tallied and compared with
      the expected counts, which have equal probabilities. The number of
      degrees of freedom for the chi-square test is @RI{R}@en@;1.

      Simplified Poker Test. Numerics.Discrete_Random is instantiated once
      with an enumeration subtype representing the 13 denominations (Two
      through Ten, Jack, Queen, King, and Ace) of an infinite deck of playing
      cards. 2000 @lquotes@;poker@rquotes@; hands (5-tuples of values of this subtype) are
      generated randomly. The counts of hands containing exactly @RI{K}
      different denominations (1 @leq @RI{K} @leq 5) are tallied and compared
      with the expected counts. The probability that a hand contains exactly
      @RI{K} different denominations is given by a formula in Knuth. The
      number of degrees of freedom for the chi-square test is 4.

      Coupon Collector's Test. Numerics.Discrete_Random is instantiated in
      each repetition of the test with an integer subtype whose range is
      1 .. @RI{R}, where @RI{R} varies systematically from 2 to 11.
      Integers are generated randomly from this range until each value in the
      range has occurred, and the number @RI{K} of integers generated is
      recorded. This constitutes a @lquotes@;coupon collector's segment@rquotes@; of length
      @RI{K}. 2000 such segments are generated. The counts of segments of
      each length from @RI{R} to @RI{R}+29, and of all lengths greater than
      @RI{R}+29 lumped together, are tallied and compared with the expected
      counts. The probability that a segment has any given length is given by
      formulas in Knuth. The number of degrees of freedom for the chi-square
      test is 30.

      Craps Test (Lengths of Games). Numerics.Discrete_Random is instantiated
      once with an integer subtype whose range is 1 .. 6 (representing the six
      numbers on a die). 5000 craps games are played, and their lengths are
      recorded. (The length of a craps game is the number of rolls of the pair
      of dice required to produce a win or a loss.
      A game is won on the first roll if
      the dice show 7 or 11; it is lost if they show 2, 3, or 12. If the dice
      show some other sum on the first roll, it is called the @i{point}, and
      the game is won if and only if the point is rolled again before a 7 is
      rolled.) The counts of games of each length from 1 to 18, and of all
      lengths greater than 18 lumped together, are tallied and compared with
      the expected counts. For 2 @leq @RI{S} @leq 12, let
      @RI{D} @-{@RI{S}} be the probability that a roll of a pair of dice shows
      the sum @RI{S}, and let
      @RI{Q} @-[@RI{S}](@RI{L}) = @RI{D} @-[@RI{S}] @Times
      (1 @en (@RI{D} @-[@RI{S}] + @RI{D} @-[7])) @+[@RI{L}@en@;2] @Times
      (@RI{D} @-[@RI{S}] + @RI{D} @-[7]). Then, the probability that a
      game has a length of 1 is @RI{D} @-[7] +
      @RI{D} @-[11] + @RI{D} @-[2] +
      @RI{D} @-[3] + @RI{D} @-[12]
      and, for @RI{L} @Gt 1, the probability that a game has a length of
      @RI{L} is @RI{Q} @-[4](@RI{L}) +
      @RI{Q} @-[5](@RI{L}) + @RI{Q} @-[6](@RI{L}) + @RI{Q} @-[8](@RI{L})
      + @RI{Q} @-[9](@RI{L}) + @RI{Q}
      @-[10](@RI{L}). The number of degrees of freedom for the chi-square test
      is 18.

      Craps Test (Lengths of Passes). This test is similar to the last, but
      enough craps games are played for 3000 losses to occur. A string of wins
      followed by a loss is called a @i{pass}, and its length is the
      number of wins preceding the loss. The counts of passes of each length
      from 0 to 7, and of all lengths greater than 7 lumped together, are
      tallied and compared with the expected counts. For @RI{L} @geq 0, the
      probability that a pass has a length of @RI{L} is
      @RI{W} @+[@RI{L}] @Times (1@en@RI{W}), where @RI{W}, the probability that a game
      ends in a win, is 244.0/495.0. The number of degrees of freedom for the
      chi-square test is 8.

      Collision Test. Numerics.Discrete_Random is instantiated once with an
      integer or enumeration type representing binary bits. 15 successive
      calls on the Random function are used to obtain the bits of a 15-bit
      binary integer between 0 and 32767. 3000 such integers are generated,
      and the number of collisions (integers previously generated) is counted
      and compared with the expected count. A chi-square test is not used to
      assess the number of collisions; rather, the limits on the number of
      collisions, corresponding to the 2.5 and 97.5 percentage points, are
      (from formulas in Knuth) 112 and 154. The test passes if and only if the
      number of collisions is in this range.
   @end{Itemize}
@end{ImplNote}
@end{ImplReq}

@LabeledSubClause{Accuracy Requirements for Complex Arithmetic}

@begin{Intro}
In the strict mode, the performance of Numerics.Generic_@!Complex_Types and
Numerics.Generic_@!Complex_@!Elementary_@!Functions shall be as specified here.
@end{Intro}

@begin{ImplReq}
When an exception is not raised, the result of evaluating a real function of
an instance @i{CT} of Numerics.Generic_Complex_Types (i.e., a function that
yields a value of subtype @i{CT}.Real'Base or @i{CT}.Imaginary) belongs to a
result interval defined as for a real elementary function
(see @RefSecNum{Accuracy Requirements for the Elementary Functions}).

@Defn2{Term=[result interval],
        Sec=[for a component of the result of evaluating a complex function]}
When an exception is not raised, each component of the result of evaluating a
complex function of such an instance, or of an instance of
Numerics.Generic_Complex_Elementary_Functions obtained by instantiating the
latter with @i{CT} (i.e., a function that yields a value of subtype
@i{CT}.Complex), also belongs to a @i{result interval}. The result intervals
for the components of the result are either defined by a
@i{maximum relative error} bound or by a @i{maximum box error} bound.
@Defn2{Term=[maximum relative error],
        Sec=[for a component of the result of evaluating a complex function]}
When the result interval for the real (resp., imaginary) component is defined by
maximum relative error, it is defined as for that of a real function, relative
to the exact value of the real (resp., imaginary) part of the result of the
corresponding mathematical function.
@Defn2{Term=[maximum box error],
        Sec=[for a component of the result of evaluating a complex function]}
When defined by maximum box error, the result interval for a component of the
result is the smallest model interval of @i{CT}.Real that contains all the
values of the corresponding part of @RI{f} @Times (1.0 + @RI{d}), where @RI{f} is the
exact complex value of the corresponding mathematical function at the given
parameter values, @RI{d} is complex, and @Abs{@RI{d}} is less than or equal
to the given maximum box error.
@IndexCheck{Overflow_Check}
The function delivers a value that belongs to the result interval (or a value
both of whose components belong to their respective result intervals) when both
bounds of the result interval(s) belong to the safe range of @i{CT}.Real;
otherwise,
@begin{Discussion}
   The maximum relative error could be specified separately for each
   component, but we do not take advantage of that freedom here.
@end{Discussion}
@begin{Discussion}
  Note that
  @RI{f} @Times (1.0 + @RI{d}) defines a small circular region of the complex
  plane centered at @RI{f}, and the result intervals for
  the real and imaginary
  components of the result define a small rectangular box containing that
  circle.
@end{Discussion}
@begin{Reason}
   Box error is used when the computation of the result risks loss of
   significance in a component due to cancellation.
@end{Reason}
@begin{Ramification}
   The components of a complex function that exhibits bounded relative error in
   each component have to have the correct sign. In contrast, one of
   the components of a complex function that exhibits bounded box error may
   have the wrong sign, since the dimensions of the box containing the result
   are proportional to the modulus of the mathematical result and not to either
   component of the mathematical result individually. Thus, for example, the
   box containing the computed result of a complex function whose mathematical
   result has a large modulus but lies very close to the imaginary axis might
   well straddle that axis, allowing the real component of the computed result
   to have the wrong sign. In this case, the distance between the computed
   result and the mathematical result is, nevertheless, a small fraction of the
   modulus of the mathematical result.
@end{Ramification}
@begin{itemize}
   @Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
   if @i{CT}.Real'Machine_Overflows is True, the function either delivers a
   value that belongs to the result interval (or a value both of whose
   components belong to their respective result intervals) or raises
   Constraint_Error, signaling overflow;

   if @i{CT}.Real'Machine_Overflows is False, the result is
   implementation defined.
   @ImplDef{The result of a complex arithmetic operation or complex
   elementary function reference in overflow situations, when the
   Machine_Overflows attribute of the corresponding real type is
   False.}
@end{itemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
The error bounds for particular complex functions are tabulated @Chg{Version=[2],New=[in table G-2],Old=[below]}.
In the table, the error bound is given as the coefficient of
@i{CT}.Real'Model_Epsilon.

@ChgRef{Version=[1], Kind=[Deleted]}
@Chg[New=<>,Old=<@ @;@comment{Empty paragraph to hang junk paragraph number (7) from original RM}>]

@Table[Columns=<4>,Alignment=<AllCenter>,FirstColWidth=[2],LastColWidth=[1],
NoBreak=[T],Border=[T],SmallSize=[F],
Caption=<@b{@Chg{Version=[2],New=[Table G-2: ],Old=[]}Error Bounds for Particular Complex Functions}>,
Headers=<@b{Function or Operator}@\@b{Nature of @*Result}@\@b{Nature of @*Bound}@\@b{Error Bound}>,
Body=<Modulus@\real@\max. rel. error@\3.0
Argument@\real@\max. rel. error@\4.0
Compose_From_Polar@\complex@\max. rel. error@\3.0
"*" (both operands complex)@\complex@\max. box error@\5.0
"/" (right operand complex)@\complex@\max. box error@\13.0
Sqrt@\complex@\max. rel. error@\6.0
Log@\complex@\max. box error@\13.0
Exp (complex parameter)@\complex@\max. rel. error@\7.0
Exp (imaginary parameter)@\complex@\max. rel. error@\2.0
Sin, Cos, Sinh, and Cosh@\complex@\max. rel. error@\11.0
Tan, Cot, Tanh, and Coth@\complex@\max. rel. error@\35.0
inverse trigonometric@\complex@\max. rel. error@\14.0@Last
inverse hyperbolic@\complex@\max. rel. error@\14.0>]

The maximum relative error given above applies throughout the domain of the
Compose_From_Polar function when the Cycle parameter is specified. When the
Cycle parameter is omitted, the maximum relative error applies only when the
absolute value of the parameter Argument is less than or equal to the angle
threshold (see @RefSecNum{Accuracy Requirements for the Elementary Functions}).
For the Exp function, and for the forward hyperbolic (resp., trigonometric)
functions, the maximum relative error given above likewise applies only when
the absolute value of the imaginary (resp., real) component of the parameter X
(or the absolute value of the parameter itself, in the case of the Exp function
with a parameter of pure-imaginary type) is less than or equal to the angle
threshold. For larger angles, the accuracy is
implementation defined.
@ImplDef{The accuracy of certain complex arithmetic operations and certain
complex elementary functions for parameters (or components thereof) beyond
the angle threshold.}

@Comment{For Ada 95 with Corr, table G-2 appears here}

@Leading@;The prescribed results specified in
@RefSecNum{Complex Elementary Functions} for certain functions at particular
parameter values take precedence over the error bounds;
effectively, they narrow to a single value the result interval allowed by
the error bounds for a component of the result. Additional rules with a similar
effect are given below for certain inverse trigonometric and inverse hyperbolic
functions, at particular parameter values for which a component of the
mathematical result is transcendental. In each case, the accuracy rule,
which takes precedence over the error bounds, is that the result interval
for the stated result component is the model interval of @i{CT}.Real
associated with the component's exact mathematical value. The cases in
question are as follows:
@begin{Itemize}
   When the parameter X has the value zero, the real (resp., imaginary)
   component of the result of the Arccot (resp., Arccoth) function is in the
   model interval of @i{CT}.Real associated with the value @Pi/2.0.

   When the parameter X has the value one, the real component of the result of
   the Arcsin function is in the model interval of @i{CT}.Real associated with
   the value @Pi/2.0.

   When the parameter X has the value @en@;1.0, the real component of the
   result of the Arcsin (resp., Arccos) function is in the model interval of
   @i{CT}.Real associated with the value @en@Pi/2.0 (resp.,
   @Pi).
@end{Itemize}
@begin{Discussion}
   It is possible to give many other prescribed results in which a component of
   the parameter is restricted to a similar model interval when the parameter X
   is appropriately restricted to an easily testable portion of the domain.
   We follow the proposed ISO/IEC standard for
   Generic_Complex_Elementary_Functions (for Ada 83) in not doing so, however.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
The amount by which a component of the result of an inverse trigonometric or
inverse hyperbolic function is allowed to spill over into a quadrant adjacent
to the one corresponding to the principal branch, as given in
@RefSecNum{Complex Elementary Functions}, is limited. The rule is that the
result belongs to the smallest model interval of @i{CT}.Real that contains both
boundaries of the quadrant corresponding to the principal branch. This rule
also takes precedence @Chg{Version=[2],New=[over],Old=[to]} the maximum error
bounds, effectively narrowing the result interval allowed by them.

Finally, the results allowed by the error bounds are narrowed by one further
rule: The absolute value of each component of the result of the Exp function,
for a pure-imaginary parameter, never exceeds one.
@end{ImplReq}

@begin{ImplAdvice}
The version of the Compose_From_Polar function without a Cycle parameter should
not be implemented by calling the corresponding version with a Cycle parameter
of 2.0*Numerics.Pi, since this will not provide the required accuracy in some
portions of the domain.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[For complex arithmetic, the Compose_From_Polar function without a Cycle
parameter should not be implemented by calling Compose_From_Polar with a Cycle
parameter.]}]}
@end{ImplAdvice}

@begin{DiffWord83}
The semantics of Numerics.Generic_Complex_Types and
Numerics.Generic_Complex_Elementary_Functions differs from
Generic_Complex_Types and Generic_Complex_Elementary_Functions as defined in
ISO/IEC CDs
13813 and 13814 (for Ada 83) in ways analogous to
those identified for the elementary functions in
@RefSecNum{Accuracy Requirements for the Elementary Functions}.
In addition,
we do not generally specify the signs of zero results (or result
components), although those proposed standards do.
@end{DiffWord83}


@LabeledAddedClause{Version=[2],Name=[Vector and Matrix Manipulation]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Types and operations for the manipulation
of real vectors and matrices are provided in Generic_Real_Arrays, which is
defined in @RefSecNum[Real Vectors and Matrices]. Types and
operations for the manipulation of complex vectors and matrices are provided
in Generic_Complex_Arrays, which is defined in
@RefSecNum[Complex Vectors and Matrices]. Both of these library units
are generic children of the predefined package Numerics (see
@RefSecNum[The Numerics Packages]). Nongeneric
equivalents of these packages for each of the predefined floating point types
are also provided as children of Numerics.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Vector and matrix manipulation is defined in
  the Numerics Annex, rather than in the core, because it is considered
  to be a specialized need of (some) numeric applications.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[These packages provide facilities that are
  similar to and replace those found in ISO/IEC 13813:1998
  @i{Information technology @em Programming languages @em
  Generic packages of real and complex type declarations and basic
  operations for Ada (including vector and matrix types)}.
  (The other facilities provided by that Standard were already provided in Ada
  95.) In addition to the main facilities of that Standard, these packages also
  include subprograms for the solution of linear equations, matrix inversion,
  determinants, and the determination of the eigenvalues and eigenvectors of
  real symmetric matrices and Hermitian matrices.]}
@end{Discussion}
@end{Intro}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  This clause is new. It just provides an introduction to the following
  subclauses.]}
@end{Extend95}


@LabeledAddedSubClause{Version=[2],Name=[Real Vectors and Matrices]}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01],ARef=[AI95-00418-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
package Numerics.Generic_Real_Arrays has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{generic}
   @key{type} Real @key{is digits} <>;
@key{package} Ada.Numerics.Generic_Real_Arrays @key{is}@ChildUnit{Parent=[Ada.Numerics],Child=[Generic_Real_Arrays]}
   @key{pragma} Pure(Generic_Real_Arrays);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Types}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Real_Vector} @key{is array} (Integer @key{range} <>) @key{of} Real'Base;
   @key{type} @AdaTypeDefn{Real_Matrix} @key{is array} (Integer @key{range} <>, Integer @key{range} <>)
                                                   @key{of} Real'Base;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Subprograms for Real_Vector types}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Real_Vector arithmetic operations}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "+"   (Right : Real_Vector)       @key{return} Real_Vector;
   @key{function} "-"   (Right : Real_Vector)       @key{return} Real_Vector;
   @key{function} "@key{abs}" (Right : Real_Vector)       @key{return} Real_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "+"   (Left, Right : Real_Vector) @key{return} Real_Vector;
   @key{function} "-"   (Left, Right : Real_Vector) @key{return} Real_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*"   (Left, Right : Real_Vector) @key{return} Real'Base;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "@key{abs}" (Right : Real_Vector)       @key{return} Real'Base;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[Real_Vector scaling operations]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*" (Left : Real'Base;   Right : Real_Vector)
      @key{return} Real_Vector;
   @key{function} "*" (Left : Real_Vector; Right : Real'Base)
      @key{return} Real_Vector;
   @key{function} "/" (Left : Real_Vector; Right : Real'Base)
      @key{return} Real_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[Other Real_Vector operations]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Unit_Vector} (Index : Integer;
                         Order : Positive;
                         First : Integer := 1) @key{return} Real_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[Subprograms for Real_Matrix types]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[Real_Matrix arithmetic operations]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "+"       (Right : Real_Matrix) @key{return} Real_Matrix;
   @key{function} "-"       (Right : Real_Matrix) @key{return} Real_Matrix;
   @key{function} "@key{abs}"     (Right : Real_Matrix) @key{return} Real_Matrix;
   @key{function} @AdaSubDefn{Transpose} (X     : Real_Matrix) @key{return} Real_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "+" (Left, Right : Real_Matrix) @key{return} Real_Matrix;
   @key{function} "-" (Left, Right : Real_Matrix) @key{return} Real_Matrix;
   @key{function} "*" (Left, Right : Real_Matrix) @key{return} Real_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*" (Left, Right : Real_Vector) @key{return} Real_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*" (Left : Real_Vector; Right : Real_Matrix)
      @key{return} Real_Vector;
   @key{function} "*" (Left : Real_Matrix; Right : Real_Vector)
      @key{return} Real_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[Real_Matrix scaling operations]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*" (Left : Real'Base;   Right : Real_Matrix)
      @key{return} Real_Matrix;
   @key{function} "*" (Left : Real_Matrix; Right : Real'Base)
      @key{return} Real_Matrix;
   @key{function} "/" (Left : Real_Matrix; Right : Real'Base)
      @key{return} Real_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[Real_Matrix inversion and related operations]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Solve} (A : Real_Matrix; X : Real_Vector) @key{return} Real_Vector;
   @key{function} @AdaSubDefn{Solve} (A, X : Real_Matrix) @key{return} Real_Matrix;
   @key{function} @AdaSubDefn{Inverse} (A : Real_Matrix) @key{return} Real_Matrix;
   @key{function} @AdaSubDefn{Determinant} (A : Real_Matrix) @key{return} Real'Base;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[Eigenvalues and vectors of a real symmetric matrix]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Eigenvalues} (A : Real_Matrix) @key{return} Real_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Eigensystem} (A       : @key{in}  Real_Matrix;
                          Values  : @key{out} Real_Vector;
                          Vectors : @key{out} Real_Matrix);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[Other Real_Matrix operations]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Unit_Matrix} (Order            : Positive;
                         First_1, First_2 : Integer := 1)
                                            @key{return} Real_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{end} Ada.Numerics.Generic_Real_Arrays;]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[@ChildUnit{Parent=[Ada.Numerics],Child=[Real_@!Arrays]}
The library package Numerics.Real_Arrays is declared pure and defines the
same types and subprograms as Numerics.Generic_Real_Arrays, except that
the predefined type Float is systematically substituted for Real'Base
throughout. Nongeneric equivalents for each of the other predefined floating
point types are defined similarly, with the names Numerics.Short_Real_Arrays,
Numerics.Long_Real_Arrays, etc.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[
   The nongeneric equivalents are provided to allow the programmer to
   construct simple mathematical applications without being required to
   understand and use generics, and to be consistent with other
   Numerics packages.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Two types are defined and exported by
Numerics.Generic_Real_Arrays. The composite type Real_Vector is provided to
represent a vector with components of type Real; it is defined as an
unconstrained, one-dimensional array with an index of type Integer. The
composite type Real_Matrix is provided to represent a matrix with components of
type Real; it is defined as an unconstrained, two-dimensional array with
indices of type Integer.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[The effect of the various subprograms is as described
below. In most cases the subprograms are described in terms of corresponding
scalar operations of the type Real; any exception raised by those operations is
propagated by the array operation. Moreover, the accuracy of the result for
each individual component is as defined for the scalar operation unless stated
otherwise.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[In the case of those operations which are defined
to @i{involve an inner product}, Constraint_Error may be raised if an intermediate
result is outside the range of Real'Base even though the mathematical final
result would not be.@Defn2{Term=[involve an inner product],Sec=[real]}]}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "+"   (Right : Real_Vector) @key{return} Real_Vector;
@key{function} "-"   (Right : Real_Vector) @key{return} Real_Vector;
@key{function} "@key{abs}" (Right : Real_Vector) @key{return} Real_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Each operation returns the result
of applying the corresponding operation of the type Real to each component of
Right. The index range of the result is Right'Range.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "+" (Left, Right : Real_Vector) @key{return} Real_Vector;
@key{function} "-" (Left, Right : Real_Vector) @key{return} Real_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation of the type Real to each component of Left and the
matching component of Right. The index range of the result is Left'Range.
Constraint_Error is raised if Left'Length is not equal to Right'Length.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left, Right : Real_Vector) @key{return} Real'Base;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation returns the inner product of Left
and Right. Constraint_Error is raised if Left'Length is not equal to
Right'Length. This operation involves an inner product.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "@key{abs}" (Right : Real_Vector) @key{return} Real'Base;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00418-01]}
@ChgAdded{Version=[2],Text=[This operation returns the L2-norm of Right (the
square root of the inner product of the vector with itself).]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Normalization of vectors is a frequent enough
  operation that it is useful to provide the norm as a basic operation.
  Furthermore, implementing the norm is not entirely straightforward, because
  the inner product might overflow while the final norm does not. An
  implementation cannot merely return Sqrt (X * X), it has to cope with a
  possible overflow of the inner product.]}
@end{Discussion}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[While the definition is given in terms of
  an inner product, the norm doesn't @lquotes@;involve an inner product@rquotes@;
  in the technical sense. The reason is that it has accuracy requirements
  substantially different from those applicable to inner products; and that
  cancellations cannot occur, because all the terms are positive, so there
  is no possibility of intermediate overflow.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Real'Base; Right : Real_Vector) @key{return} Real_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation returns the result of multiplying
each component of Right by the scalar Left using the "*" operation of the type
Real. The index range of the result is Right'Range.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Real_Vector; Right : Real'Base) @key{return} Real_Vector;
@key{function} "/" (Left : Real_Vector; Right : Real'Base) @key{return} Real_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation of the type Real to each component of Left and to the
scalar Right. The index range of the result is Left'Range.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Unit_Vector (Index : Integer;
                      Order : Positive;
                      First : Integer := 1) @key{return} Real_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns a @i<unit vector>@Defn2{Term=[unit vector],Sec=[real vector]}
with Order components and a lower bound of First. All components are set to 0.0
except for the Index component which is set to 1.0. Constraint_Error is raised
if Index < First, Index > First + Order @en 1 or if First + Order @en 1 >
Integer'Last.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "+"   (Right : Real_Matrix) @key{return} Real_Matrix;
@key{function} "-"   (Right : Real_Matrix) @key{return} Real_Matrix;
@key{function} "@key{abs}" (Right : Real_Matrix) @key{return} Real_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation of the type Real to each component of Right. The index
ranges of the result are those of Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Transpose (X : Real_Matrix) @key{return} Real_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns the transpose of a matrix X.
The first and second index ranges of the result are X'Range(2) and X'Range(1)
respectively.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "+" (Left, Right : Real_Matrix) @key{return} Real_Matrix;
@key{function} "-" (Left, Right : Real_Matrix) @key{return} Real_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation of the type Real to each component of Left and the
matching component of Right. The index ranges of the result are those of Left.
Constraint_Error is raised if Left'Length(1) is not equal to Right'Length(1) or
Left'Length(2) is not equal to Right'Length(2).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left, Right : Real_Matrix) @key{return} Real_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation provides the standard mathematical
operation for matrix multiplication. The first and second index ranges of the
result are Left'Range(1) and Right'Range(2) respectively. Constraint_Error is
raised if Left'Length(2) is not equal to Right'Length(1). This operation
involves inner products.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left, Right : Real_Vector) @key{return} Real_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation returns the outer product of a
(column) vector Left by a (row) vector Right using the operation "*" of the
type Real for computing the individual components. The first and second index
ranges of the result are Left'Range and Right'Range respectively.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Real_Vector; Right : Real_Matrix) @key{return} Real_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation provides the standard mathematical
operation for multiplication of a (row) vector Left by a matrix Right. The
index range of the (row) vector result is Right'Range(2). Constraint_Error is
raised if Left'Length is not equal to Right'Length(1). This operation involves
inner products.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Real_Matrix; Right : Real_Vector) @key{return} Real_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation provides the standard mathematical
operation for multiplication of a matrix Left by a (column) vector Right. The
index range of the (column) vector result is Left'Range(1). Constraint_Error is
raised if Left'Length(2) is not equal to Right'Length. This operation involves
inner products.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Real'Base; Right : Real_Matrix) @key{return} Real_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation returns the result of multiplying
each component of Right by the scalar Left using the "*" operation of the type
Real. The index ranges of the result are those of Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Real_Matrix; Right : Real'Base) @key{return} Real_Matrix;
@key{function} "/" (Left : Real_Matrix; Right : Real'Base) @key{return} Real_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation of the type Real to each component of Left and to the
scalar Right. The index ranges of the result are those of Left.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Solve (A : Real_Matrix; X : Real_Vector) @key{return} Real_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns a vector Y such that X is
(nearly) equal to A * Y. This is the standard mathematical operation for
solving a single set of linear equations. The index range of the result is
A'Range(2). Constraint_Error is raised if A'Length(1), A'Length(2), and X'Length
are not equal. Constraint_Error is raised if the matrix A is ill-conditioned.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The text says that Y is such that @lquotes@;X is
  (nearly) equal to A * Y@rquotes rather than @lquotes@;X is equal to A *
  Y@rquotes because rounding errors may mean that there is no value of Y such
  that X is exactly equal to A * Y. On the other hand it does not mean that any
  old rough value will do. The algorithm given under @ImplAdviceTitle
  should be followed.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The requirement to raise Constraint_Error if the
  matrix is ill-conditioned is really a reflection of what will happen if the
  matrix is ill-conditioned. See @ImplAdviceTitle.
  We do not make any attempt to define ill-conditioned formally.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[These remarks apply to all versions of Solve and
  Inverse.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Solve (A, X : Real_Matrix) @key{return} Real_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns a matrix Y such that X is
(nearly) equal to A * Y. This is the standard mathematical operation for
solving several sets of linear equations. The index ranges of the result are
A'Range(2) and X'Range(2). Constraint_Error is raised if A'Length(1), A'Length(2), and
X'Length(1) are not equal. Constraint_Error is raised if the matrix A is
ill-conditioned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Inverse (A : Real_Matrix) @key{return} Real_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns a matrix B such that A * B is
(nearly) equal to the unit matrix. The index ranges of the result are A'Range(2)
and A'Range(1). Constraint_Error is raised if A'Length(1) is not equal to
A'Length(2). Constraint_Error is raised if the matrix A is ill-conditioned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Determinant (A : Real_Matrix) @key{return} Real'Base;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns the determinant of the matrix
A. Constraint_Error is raised if A'Length(1) is not equal to A'Length(2).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Eigenvalues(A : Real_Matrix) @key{return} Real_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns the eigenvalues of the
symmetric matrix A as a vector sorted into order with the largest first.
Constraint_Error is raised if A'Length(1) is not equal to A'Length(2). The
index range of the result is A'Range(1). Argument_Error is raised if the matrix
A is not symmetric.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Eigensystem(A       : @key{in}  Real_Matrix;
                      Values  : @key{out} Real_Vector;
                      Vectors : @key{out} Real_Matrix);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This procedure computes both the eigenvalues and
eigenvectors of the symmetric matrix A. The out parameter Values is the same as
that obtained by calling the function Eigenvalues. The out parameter Vectors is
a matrix whose columns are the eigenvectors of the matrix A. The order of the
columns corresponds to the order of the eigenvalues. The eigenvectors are
normalized and mutually orthogonal (they are orthonormal), including when there
are repeated eigenvalues. Constraint_Error is raised if A'Length(1) is not
equal to A'Length(2). The index ranges of the parameter Vectors are those of A.
Argument_Error is raised if the matrix A is not symmetric.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Unit_Matrix (Order            : Positive;
                      First_1, First_2 : Integer := 1) @key{return} Real_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns a square
@i{unit matrix}@Defn2{Term=[unit matrix],Sec=[real matrix]}
with Order**2 components and lower bounds of
First_1 and First_2 (for the first and second index ranges respectively). All
components are set to 0.0 except for the main diagonal, whose components are
set to 1.0. Constraint_Error is raised if First_1 + Order @en 1 > Integer'Last or
First_2 + Order @en 1 > Integer'Last.]}

@end{DescribeCode}

@end{StaticSem}

@begin{ImplReq}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Accuracy requirements for the subprograms Solve,
Inverse, Determinant, Eigenvalues and Eigensystem are implementation defined.]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[The
accuracy requirements for the subprograms Solve,
Inverse, Determinant, Eigenvalues and Eigensystem for type Real_Matrix.]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[For operations not involving an inner product, the
accuracy requirements are those of the corresponding operations of the type
Real in both the strict mode and the relaxed mode
(see @RefSecNum{Numeric Performance Requirements}).]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For operations involving an inner
product, no
requirements are specified in the relaxed mode. In the strict mode the modulus
of the absolute error of the inner product @i<X>*@i<Y> shall not exceed
@i<g>*@b<abs>(@i<X>)*@b<abs>(@i<Y>) where @i<g> is defined as]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@i<g> = @i<X>'Length * Real'Machine_Radix**(1 @en@; Real'Model_Mantissa)]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00418-01]}
@ChgAdded{Version=[2],Text=[For the L2-norm, no accuracy
requirements are specified in the relaxed mode. In the strict mode the relative
error on the norm shall not exceed @i<g> / 2.0 + 3.0 * Real'Model_Epsilon where
@i<g> is defined as above.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is simply the combination of the error on
  the inner product with the error on Sqrt. A first order computation would
  lead to 2.0 * Real'Model_Epsilon above, but we are adding an extra
  Real'Model_Epsilon to account for higher order effects.]}
@end{Reason}
@end{ImplReq}

@begin{DocReq}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Implementations shall document any techniques used
to reduce cancellation errors such as extended precision arithmetic.]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[Any
techniques used to reduce cancellation errors in
Numerics.Generic_Real_Arrays shall be documented.]}]}
@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The above accuracy requirement is met by the
  canonical implementation of the inner product by multiplication and addition
  using the corresponding operations of type Real'Base and performing the
  cumulative addition using ascending indices. Note however, that some hardware
  provides special operations for the computation of the inner product and
  although these may be fast they may not meet the accuracy requirement
  specified. See Accuracy and Stability of Numerical Algorithms By N J Higham
  (ISBN 0-89871-355-2), Section 3.1.]}
@end{ImplNote}
@end{DocReq}

@begin{ImplPerm}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[The nongeneric equivalent packages may, but need
not, be actual instantiations of the generic package for the appropriate
predefined type.]}

@end{ImplPerm}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Implementations should implement the Solve and
Inverse functions using established techniques such as LU decomposition with
row interchanges followed by back and forward substitution. Implementations are
recommended to refine the result by performing an iteration on the residuals;
if this is done then it should be documented.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Solve and Inverse for Numerics.Generic_Real_Arrays should be
implemented using established techniques such as LU decomposition and
the result should be refined by an iteration on the residuals.]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[It is not the intention that any special provision
should be made to determine whether a matrix is ill-conditioned or not. The
naturally occurring overflow (including division by zero) which will result
from executing these functions with an ill-conditioned matrix and thus raise
Constraint_Error is sufficient.]}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[There isn't any advice for the implementation to
document with this paragraph.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The test that a matrix is symmetric should be
performed by using the equality operator to compare the relevant components.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The equality operator should be used to test that a matrix in
Numerics.Generic_Real_Matrix is symmetric.]}]}

@end{ImplAdvice}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The package Numerics.Generic_Real_Arrays and its nongeneric equivalents
  are new.]}
@end{Extend95}


@LabeledAddedSubClause{Version=[2],Name=[Complex Vectors and Matrices]}


@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
package Numerics.Generic_Complex_Arrays has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{with} Ada.Numerics.Generic_Real_Arrays, Ada.Numerics.Generic_Complex_Types;
@key{generic}
   @key{with package} Real_Arrays   @key{is new}
      Ada.Numerics.Generic_Real_Arrays   (<>);
   @key{use} Real_Arrays;
   @key{with package} Complex_Types @key{is new}
      Ada.Numerics.Generic_Complex_Types (Real);
   @key{use} Complex_Types;
@key{package} Ada.Numerics.Generic_Complex_Arrays @key{is}@ChildUnit{Parent=[Ada.Numerics],Child=[Generic_@!Complex_@!Arrays]}
   @key{pragma} Pure(Generic_Complex_Arrays);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Types}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Complex_Vector} @key{is array} (Integer @key{range} <>) @key{of} Complex;
   @key{type} @AdaTypeDefn{Complex_Matrix} @key{is array} (Integer @key{range} <>,
                                 Integer @key{range} <>) @key{of} Complex;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Subprograms for Complex_Vector types}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Complex_Vector selection, conversion and composition operations}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Re} (X : Complex_Vector) @key{return} Real_Vector;
   @key{function} @AdaSubDefn{Im} (X : Complex_Vector) @key{return} Real_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Set_Re} (X  : @key{in out} Complex_Vector;
                     Re : @key{in}     Real_Vector);
   @key{procedure} @AdaSubDefn{Set_Im} (X  : @key{in out} Complex_Vector;
                     Im : @key{in}     Real_Vector);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Compose_From_Cartesian} (Re     : Real_Vector)
      @key{return} Complex_Vector;
   @key{function} @AdaSubDefn{Compose_From_Cartesian} (Re, Im : Real_Vector)
      @key{return} Complex_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Modulus}  (X     : Complex_Vector) @key{return} Real_Vector;
   @key{function} "@key{abs}"    (Right : Complex_Vector) @key{return} Real_Vector
                                                 @key{renames} Modulus;
   @key{function} @AdaSubDefn{Argument} (X     : Complex_Vector) @key{return} Real_Vector;
   @key{function} @AdaSubDefn{Argument} (X     : Complex_Vector;
                      Cycle : Real'Base)      @key{return} Real_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Compose_From_Polar} (Modulus, Argument : Real_Vector)
      @key{return} Complex_Vector;
   @key{function} @AdaSubDefn{Compose_From_Polar} (Modulus, Argument : Real_Vector;
                                Cycle             : Real'Base)
      @key{return} Complex_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Complex_Vector arithmetic operations}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "+"       (Right  : Complex_Vector) @key{return} Complex_Vector;
   @key{function} "-"       (Right  : Complex_Vector) @key{return} Complex_Vector;
   @key{function} @AdaSubDefn{Conjugate} (X      : Complex_Vector) @key{return} Complex_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "+"  (Left, Right : Complex_Vector) @key{return} Complex_Vector;
   @key{function} "-"  (Left, Right : Complex_Vector) @key{return} Complex_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*"  (Left, Right : Complex_Vector) @key{return} Complex;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "@key{abs}"     (Right : Complex_Vector) @key{return} Complex;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Mixed Real_Vector and Complex_Vector arithmetic operations}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "+" (Left  : Real_Vector;
                 Right : Complex_Vector) @key{return} Complex_Vector;
   @key{function} "+" (Left  : Complex_Vector;
                 Right : Real_Vector)    @key{return} Complex_Vector;
   @key{function} "-" (Left  : Real_Vector;
                 Right : Complex_Vector) @key{return} Complex_Vector;
   @key{function} "-" (Left  : Complex_Vector;
                 Right : Real_Vector)    @key{return} Complex_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*" (Left  : Real_Vector;    Right : Complex_Vector)
      @key{return} Complex;
   @key{function} "*" (Left  : Complex_Vector; Right : Real_Vector)
      @key{return} Complex;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Complex_Vector scaling operations}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*" (Left  : Complex;
                 Right : Complex_Vector) @key{return} Complex_Vector;
   @key{function} "*" (Left  : Complex_Vector;
                 Right : Complex)        @key{return} Complex_Vector;
   @key{function} "/" (Left  : Complex_Vector;
                 Right : Complex)        @key{return} Complex_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*" (Left  : Real'Base;
                 Right : Complex_Vector) @key{return} Complex_Vector;
   @key{function} "*" (Left  : Complex_Vector;
                 Right : Real'Base)      @key{return} Complex_Vector;
   @key{function} "/" (Left  : Complex_Vector;
                 Right : Real'Base)      @key{return} Complex_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Other Complex_Vector operations}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Unit_Vector} (Index : Integer;
                         Order : Positive;
                         First : Integer := 1) @key{return} Complex_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Subprograms for Complex_Matrix types}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Complex_Matrix selection, conversion and composition operations}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Re} (X : Complex_Matrix) @key{return} Real_Matrix;
   @key{function} @AdaSubDefn{Im} (X : Complex_Matrix) @key{return} Real_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Set_Re} (X  : @key{in out} Complex_Matrix;
                     Re : @key{in}     Real_Matrix);
   @key{procedure} @AdaSubDefn{Set_Im} (X  : @key{in out} Complex_Matrix;
                     Im : @key{in}     Real_Matrix);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Compose_From_Cartesian} (Re     : Real_Matrix)
      @key{return} Complex_Matrix;
   @key{function} @AdaSubDefn{Compose_From_Cartesian} (Re, Im : Real_Matrix)
      @key{return} Complex_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Modulus}  (X     : Complex_Matrix) @key{return} Real_Matrix;
   @key{function} "@key{abs}"    (Right : Complex_Matrix) @key{return} Real_Matrix
                                                 @key{renames} Modulus;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Argument} (X     : Complex_Matrix) @key{return} Real_Matrix;
   @key{function} @AdaSubDefn{Argument} (X     : Complex_Matrix;
                      Cycle : Real'Base)      @key{return} Real_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Compose_From_Polar} (Modulus, Argument : Real_Matrix)
      @key{return} Complex_Matrix;
   @key{function} @AdaSubDefn{Compose_From_Polar} (Modulus, Argument : Real_Matrix;
                                Cycle             : Real'Base)
      @key{return} Complex_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Complex_Matrix arithmetic operations}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "+"       (Right : Complex_Matrix) @key{return} Complex_Matrix;
   @key{function} "-"       (Right : Complex_Matrix) @key{return} Complex_Matrix;
   @key{function} @AdaSubDefn{Conjugate} (X     : Complex_Matrix) @key{return} Complex_Matrix;
   @key{function} @AdaSubDefn{Transpose} (X     : Complex_Matrix) @key{return} Complex_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "+" (Left, Right : Complex_Matrix) @key{return} Complex_Matrix;
   @key{function} "-" (Left, Right : Complex_Matrix) @key{return} Complex_Matrix;
   @key{function} "*" (Left, Right : Complex_Matrix) @key{return} Complex_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*" (Left, Right : Complex_Vector) @key{return} Complex_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*" (Left  : Complex_Vector;
                 Right : Complex_Matrix) @key{return} Complex_Vector;
   @key{function} "*" (Left  : Complex_Matrix;
                 Right : Complex_Vector) @key{return} Complex_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Mixed Real_Matrix and Complex_Matrix arithmetic operations}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "+" (Left  : Real_Matrix;
                 Right : Complex_Matrix) @key{return} Complex_Matrix;
   @key{function} "+" (Left  : Complex_Matrix;
                 Right : Real_Matrix)    @key{return} Complex_Matrix;
   @key{function} "-" (Left  : Real_Matrix;
                 Right : Complex_Matrix) @key{return} Complex_Matrix;
   @key{function} "-" (Left  : Complex_Matrix;
                 Right : Real_Matrix)    @key{return} Complex_Matrix;
   @key{function} "*" (Left  : Real_Matrix;
                 Right : Complex_Matrix) @key{return} Complex_Matrix;
   @key{function} "*" (Left  : Complex_Matrix;
                 Right : Real_Matrix)    @key{return} Complex_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*" (Left  : Real_Vector;
                 Right : Complex_Vector) @key{return} Complex_Matrix;
   @key{function} "*" (Left  : Complex_Vector;
                 Right : Real_Vector)    @key{return} Complex_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*" (Left  : Real_Vector;
                 Right : Complex_Matrix) @key{return} Complex_Vector;
   @key{function} "*" (Left  : Complex_Vector;
                 Right : Real_Matrix)    @key{return} Complex_Vector;
   @key{function} "*" (Left  : Real_Matrix;
                 Right : Complex_Vector) @key{return} Complex_Vector;
   @key{function} "*" (Left  : Complex_Matrix;
                 Right : Real_Vector)    @key{return} Complex_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Complex_Matrix scaling operations}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*" (Left  : Complex;
                 Right : Complex_Matrix) @key{return} Complex_Matrix;
   @key{function} "*" (Left  : Complex_Matrix;
                 Right : Complex)        @key{return} Complex_Matrix;
   @key{function} "/" (Left  : Complex_Matrix;
                 Right : Complex)        @key{return} Complex_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "*" (Left  : Real'Base;
                 Right : Complex_Matrix) @key{return} Complex_Matrix;
   @key{function} "*" (Left  : Complex_Matrix;
                 Right : Real'Base)      @key{return} Complex_Matrix;
   @key{function} "/" (Left  : Complex_Matrix;
                 Right : Real'Base)      @key{return} Complex_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Complex_Matrix inversion and related operations}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Solve} (A : Complex_Matrix; X : Complex_Vector)
      @key{return} Complex_Vector;
   @key{function} @AdaSubDefn{Solve} (A, X : Complex_Matrix) @key{return} Complex_Matrix;
   @key{function} @AdaSubDefn{Inverse} (A : Complex_Matrix) @key{return} Complex_Matrix;
   @key{function} @AdaSubDefn{Determinant} (A : Complex_Matrix) @key{return} Complex;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Eigenvalues and vectors of a Hermitian matrix}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Eigenvalues}(A : Complex_Matrix) @key{return} Real_Vector;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Eigensystem}(A       : @key{in}  Complex_Matrix;
                         Values  : @key{out} Real_Vector;
                         Vectors : @key{out} Complex_Matrix);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Other Complex_Matrix operations}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Unit_Matrix} (Order            : Positive;
                         First_1, First_2 : Integer := 1)
                                            @key{return} Complex_Matrix;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{end} Ada.Numerics.Generic_Complex_Arrays;]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[@ChildUnit{Parent=[Ada.Numerics],Child=[Complex_@!Arrays]}
The library package Numerics.Complex_Arrays is declared pure and defines
the same types and subprograms as Numerics.Generic_Complex_Arrays, except
that the predefined type Float is systematically substituted for Real'Base,
and the Real_Vector and Real_Matrix types exported by Numerics.Real_Arrays
are systematically substituted for Real_Vector and Real_Matrix, and the
Complex type exported by Numerics.Complex_Types is systematically
substituted for Complex, throughout. Nongeneric equivalents for each of
the other predefined floating point types are defined similarly, with the
names Numerics.Short_Complex_Arrays, Numerics.Long_Complex_Arrays, etc.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Two types are defined and exported by
Numerics.Generic_Complex_Arrays. The composite type Complex_Vector is
provided to represent a vector with components of type Complex; it is defined
as an unconstrained one-dimensional array with an index of type Integer. The
composite type Complex_Matrix is provided to represent a matrix with components
of type Complex; it is defined as an unconstrained, two-dimensional array with
indices of type Integer.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[The effect of the various subprograms is as
described below. In many cases they are described in terms of corresponding
scalar operations in Numerics.Generic_Complex_Types. Any exception raised by
those operations is propagated by the array subprogram. Moreover, any
constraints on the parameters and the accuracy of the result for each
individual component are as defined for the scalar operation.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[In the case of those operations which are defined
to @i{involve an inner product}, Constraint_Error may be raised if an intermediate
result has a component outside the range of Real'Base even though the final
mathematical result would not.@Defn2{Term=[involve an inner product],Sec=[complex]}]}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Re (X : Complex_Vector) @key{return} Real_Vector;
@key{function} Im (X : Complex_Vector) @key{return} Real_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each function returns a vector of the specified
Cartesian components of X. The index range of the result is X'Range.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Set_Re (X  : @key{in out} Complex_Vector; Re : @key{in} Real_Vector);
@key{procedure} Set_Im (X  : @key{in out} Complex_Vector; Im : @key{in} Real_Vector);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each procedure replaces the specified (Cartesian)
component of each of the components of X by the value of the matching component
of Re or Im; the other (Cartesian) component of each of the components is
unchanged. Constraint_Error is raised if X'Length is not equal to Re'Length or
Im'Length.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Compose_From_Cartesian (Re     : Real_Vector)
   @key{return} Complex_Vector;
@key{function} Compose_From_Cartesian (Re, Im : Real_Vector)
   @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each function constructs a vector of Complex
results (in Cartesian representation) formed from given vectors of Cartesian
components; when only the real components are given, imaginary components of
zero are assumed. The index range of the result is Re'Range. Constraint_Error
is raised if Re'Length is not equal to Im'Length.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Modulus  (X     : Complex_Vector) @key{return} Real_Vector;
@key{function} "@key{abs}"    (Right : Complex_Vector) @key{return} Real_Vector
                                              @key{renames} Modulus;
@key{function} Argument (X     : Complex_Vector) @key{return} Real_Vector;
@key{function} Argument (X     : Complex_Vector;
                   Cycle : Real'Base)      @key{return} Real_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each function calculates and returns a vector of
the specified polar components of X or Right using the corresponding function
in numerics.@!generic_complex_types. The index range of the result is X'Range or
Right'Range.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Compose_From_Polar (Modulus, Argument : Real_Vector)
   @key{return} Complex_Vector;
@key{function} Compose_From_Polar (Modulus, Argument : Real_Vector;
                             Cycle             : Real'Base)
   @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each function constructs a vector of Complex
results (in Cartesian representation) formed from given vectors of polar
components using the corresponding function in numerics.@!generic_complex_types
on matching components of Modulus and Argument. The index range of the result
is Modulus'Range. Constraint_Error is raised if Modulus'Length is not equal to
Argument'Length.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "+" (Right : Complex_Vector) @key{return} Complex_Vector;
@key{function} "-" (Right : Complex_Vector) @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation in numerics.@!generic_complex_types to each component of
Right. The index range of the result is Right'Range.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Conjugate (X : Complex_Vector) @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns the result of applying the
appropriate function Conjugate in numerics.@!generic_complex_types to each
component of X. The index range of the result is X'Range.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "+" (Left, Right : Complex_Vector) @key{return} Complex_Vector;
@key{function} "-" (Left, Right : Complex_Vector) @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation in numerics.@!generic_complex_types to each component of
Left and the matching component of Right. The index range of the result is
Left'Range. Constraint_Error is raised if Left'Length is not equal to
Right'Length.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left, Right : Complex_Vector) @key{return} Complex;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation returns the inner product of Left
and Right. Constraint_Error is raised if Left'Length is not equal to
Right'Length. This operation involves an inner product.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "@key{abs}" (Right : Complex_Vector) @key{return} Complex;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00418-01]}
@ChgAdded{Version=[2],Text=[This operation returns the Hermitian L2-norm of
Right (the square root of the inner product of the vector with its
conjugate).]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[While the definition is given in terms of
  an inner product, the norm doesn't @lquotes@;involve an inner product@rquotes
  in the technical sense. The reason is that it has accuracy requirements
  substantially different from those applicable to inner products; and that
  cancellations cannot occur, because all the terms are positive, so there
  is no possibility of intermediate overflow.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "+" (Left  : Real_Vector;
              Right : Complex_Vector) @key{return} Complex_Vector;
@key{function} "+" (Left  : Complex_Vector;
              Right : Real_Vector)    @key{return} Complex_Vector;
@key{function} "-" (Left  : Real_Vector;
              Right : Complex_Vector) @key{return} Complex_Vector;
@key{function} "-" (Left  : Complex_Vector;
              Right : Real_Vector)    @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation in numerics.@!generic_complex_types to each component of
Left and the matching component of Right. The index range of the result is
Left'Range. Constraint_Error is raised if Left'Length is not equal to
Right'Length.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Real_Vector;    Right : Complex_Vector) @key{return} Complex;
@key{function} "*" (Left : Complex_Vector; Right : Real_Vector)    @key{return} Complex;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the inner product of Left
and Right. Constraint_Error is raised if Left'Length is not equal to
Right'Length. These operations involve an inner product.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Complex; Right : Complex_Vector) @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation returns the result of multiplying
each component of Right by the complex number Left using the appropriate
operation "*" in numerics.@!generic_complex_types. The index range of the result
is Right'Range.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Complex_Vector; Right : Complex) @key{return} Complex_Vector;
@key{function} "/" (Left : Complex_Vector; Right : Complex) @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation in numerics.@!generic_complex_types to each component of
the vector Left and the complex number Right. The index range of the result is
Left'Range.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Real'Base;
              Right : Complex_Vector) @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation returns the result of multiplying
each component of Right by the real number Left using the appropriate operation
"*" in numerics.@!generic_complex_types. The index range of the result is
Right'Range.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Complex_Vector;
              Right : Real'Base) @key{return} Complex_Vector;
@key{function} "/" (Left : Complex_Vector;
              Right : Real'Base) @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation in numerics.@!generic_complex_types to each component of
the vector Left and the real number Right. The index range of the result is
Left'Range.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Unit_Vector (Index : Integer;
                      Order : Positive;
                      First : Integer := 1) @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns a @i{unit
vector}@Defn2{Term=[unit vector],Sec=[complex vector]} with Order components
and a lower bound of First. All components are set to (0.0, 0.0) except for the
Index component which is set to (1.0, 0.0). Constraint_Error is raised if Index
< First, Index > First + Order @en 1, or if First + Order @en 1 > Integer'Last.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Re (X : Complex_Matrix) @key{return} Real_Matrix;
@key{function} Im (X : Complex_Matrix) @key{return} Real_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each function returns a matrix of the specified
Cartesian components of X. The index ranges of the result are those of X.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Set_Re (X : @key{in out} Complex_Matrix; Re : @key{in} Real_Matrix);
@key{procedure} Set_Im (X : @key{in out} Complex_Matrix; Im : @key{in} Real_Matrix);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each procedure replaces the specified (Cartesian)
component of each of the components of X by the value of the matching component
of Re or Im; the other (Cartesian) component of each of the components is
unchanged. Constraint_Error is raised if X'Length(1) is not equal to
Re'Length(1) or Im'Length(1) or if X'Length(2) is not equal to Re'Length(2) or
Im'Length(2).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Compose_From_Cartesian (Re     : Real_Matrix)
   @key{return} Complex_Matrix;
@key{function} Compose_From_Cartesian (Re, Im : Real_Matrix)
   @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each function constructs a matrix of Complex
results (in Cartesian representation) formed from given matrices of Cartesian
components; when only the real components are given, imaginary components of
zero are assumed. The index ranges of the result are those of Re.
Constraint_Error is raised if Re'Length(1) is not equal to Im'Length(1) or
Re'Length(2) is not equal to Im'Length(2).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Modulus  (X     : Complex_Matrix) @key{return} Real_Matrix;
@key{function} "@key{abs}"    (Right : Complex_Matrix) @key{return} Real_Matrix
                                              @key{renames} Modulus;
@key{function} Argument (X     : Complex_Matrix) @key{return} Real_Matrix;
@key{function} Argument (X     : Complex_Matrix;
                   Cycle : Real'Base)      @key{return} Real_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each function calculates and returns a matrix of
the specified polar components of X or Right using the corresponding function
in numerics.@!generic_complex_types. The index ranges of the result are those of
X or Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Compose_From_Polar (Modulus, Argument : Real_Matrix)
   @key{return} Complex_Matrix;
@key{function} Compose_From_Polar (Modulus, Argument : Real_Matrix;
                             Cycle             : Real'Base)
   @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each function constructs a matrix of Complex
results (in Cartesian representation) formed from given matrices of polar
components using the corresponding function in numerics.@!generic_complex_types
on matching components of Modulus and Argument. The index ranges of the result
are those of Modulus. Constraint_Error is raised if Modulus'Length(1) is not
equal to Argument'Length(1) or Modulus'Length(2) is not equal to
Argument'Length(2).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "+" (Right : Complex_Matrix) @key{return} Complex_Matrix;
@key{function} "-" (Right : Complex_Matrix) @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation in numerics.@!generic_complex_types to each component of
Right. The index ranges of the result are those of Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Conjugate (X : Complex_Matrix) @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns the result of applying the
appropriate function Conjugate in numerics.@!generic_complex_types to each
component of X. The index ranges of the result are those of X.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Transpose (X : Complex_Matrix) @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns the transpose of a matrix X.
The first and second index ranges of the result are X'Range(2) and X'Range(1)
respectively.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "+" (Left, Right : Complex_Matrix) @key{return} Complex_Matrix;
@key{function} "-" (Left, Right : Complex_Matrix) @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation in numerics.@!generic_complex_types to each component of
Left and the matching component of Right. The index ranges of the result are
those of Left. Constraint_Error is raised if Left'Length(1) is not equal to
Right'Length(1) or Left'Length(2) is not equal to Right'Length(2).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left, Right : Complex_Matrix) @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation provides the standard mathematical
operation for matrix multiplication. The first and second index ranges of the
result are Left'Range(1) and Right'Range(2) respectively. Constraint_Error is
raised if Left'Length(2) is not equal to Right'Length(1). This operation
involves inner products.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left, Right : Complex_Vector) @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation returns the outer product of a
(column) vector Left by a (row) vector Right using the appropriate operation
"*" in numerics.@!generic_complex_types for computing the individual components.
The first and second index ranges of the result are Left'Range and
Right'Range respectively.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left  : Complex_Vector;
              Right : Complex_Matrix) @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation provides the standard mathematical
operation for multiplication of a (row) vector Left by a matrix Right. The
index range of the (row) vector result is Right'Range(2). Constraint_Error is
raised if Left'Length is not equal to Right'Length(1). This operation involves
inner products.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left  : Complex_Matrix;
              Right : Complex_Vector) @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation provides the standard mathematical
operation for multiplication of a matrix Left by a (column) vector Right. The
index range of the (column) vector result is Left'Range(1). Constraint_Error is
raised if Left'Length(2) is not equal to Right'Length. This operation involves
inner products.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "+" (Left  : Real_Matrix;
              Right : Complex_Matrix) @key{return} Complex_Matrix;
@key{function} "+" (Left  : Complex_Matrix;
              Right : Real_Matrix)    @key{return} Complex_Matrix;
@key{function} "-" (Left  : Real_Matrix;
              Right : Complex_Matrix) @key{return} Complex_Matrix;
@key{function} "-" (Left  : Complex_Matrix;
              Right : Real_Matrix)    @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation in numerics.@!generic_complex_types to each component of
Left and the matching component of Right. The index ranges of the result are
those of Left. Constraint_Error is raised if Left'Length(1) is
not equal to Right'Length(1) or Left'Length(2) is not equal to
Right'Length(2).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left  : Real_Matrix;
              Right : Complex_Matrix) @key{return} Complex_Matrix;
@key{function} "*" (Left  : Complex_Matrix;
              Right : Real_Matrix)    @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation provides the standard mathematical
operation for matrix multiplication. The first and second index ranges of the
result are Left'Range(1) and Right'Range(2) respectively. Constraint_Error is
raised if Left'Length(2) is not equal to Right'Length(1). These operations
involve inner products.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left  : Real_Vector;
              Right : Complex_Vector) @key{return} Complex_Matrix;
@key{function} "*" (Left  : Complex_Vector;
              Right : Real_Vector)    @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the outer product of a
(column) vector Left by a (row) vector Right using the appropriate operation
"*" in numerics.@!generic_complex_types for computing the individual components.
The first and second index ranges of the result are Left'Range and
Right'Range respectively.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left  : Real_Vector;
              Right : Complex_Matrix) @key{return} Complex_Vector;
@key{function} "*" (Left  : Complex_Vector;
              Right : Real_Matrix)    @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation provides the standard mathematical
operation for multiplication of a (row) vector Left by a matrix Right. The
index range of the (row) vector result is Right'Range(2). Constraint_Error is
raised if Left'Length is not equal to Right'Length(1). These operations involve
inner products.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left  : Real_Matrix;
              Right : Complex_Vector) @key{return} Complex_Vector;
@key{function} "*" (Left  : Complex_Matrix;
              Right : Real_Vector)    @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation provides the standard mathematical
operation for multiplication of a matrix Left by a (column) vector Right. The
index range of the (column) vector result is Left'Range(1). Constraint_Error is
raised if Left'Length(2) is not equal to Right'Length. These operations involve
inner products.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Complex; Right : Complex_Matrix) @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation returns the result of multiplying
each component of Right by the complex number Left using the appropriate
operation "*" in numerics.@!generic_complex_types. The index ranges of the result
are those of Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Complex_Matrix; Right : Complex) @key{return} Complex_Matrix;
@key{function} "/" (Left : Complex_Matrix; Right : Complex) @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation in numerics.@!generic_complex_types to each component of
the matrix Left and the complex number Right. The index ranges of the result
are those of Left.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Real'Base;
              Right : Complex_Matrix) @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This operation returns the result of multiplying
each component of Right by the real number Left using the appropriate operation
"*" in numerics.@!generic_complex_types. The index ranges of the result are those
of Right.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} "*" (Left : Complex_Matrix;
              Right : Real'Base) @key{return} Complex_Matrix;
@key{function} "/" (Left : Complex_Matrix;
              Right : Real'Base) @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Each operation returns the result of applying the
corresponding operation in numerics.@!generic_complex_types to each component of
the matrix Left and the real number Right. The index ranges of the result are
those of Left.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Solve (A : Complex_Matrix; X : Complex_Vector) @key{return} Complex_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns a vector Y such that X is
(nearly) equal to A * Y. This is the standard mathematical operation for
solving a single set of linear equations. The index range of the result is
A'Range(2). Constraint_Error is raised if A'Length(1), A'Length(2), and X'Length
are not equal. Constraint_Error is raised if the matrix A is ill-conditioned.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The text says that Y is such that @lquotes@;X is
  (nearly) equal to A * Y@rquotes rather than @lquotes@;X is equal to A *
  Y@rquotes because rounding errors may mean that there is no value of Y such
  that X is exactly equal to A * Y. On the other hand it does not mean that any
  old rough value will do. The algorithm given under @ImplAdviceTitle
  should be followed.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The requirement to raise Constraint_Error if the
  matrix is ill-conditioned is really a reflection of what will happen if the
  matrix is ill-conditioned. See @ImplAdviceTitle.
  We do not make any attempt to define ill-conditioned formally.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[These remarks apply to all versions of Solve and
  Inverse.]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Solve (A, X : Complex_Matrix) @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns a matrix Y such that X is
(nearly) equal to A * Y. This is the standard mathematical operation for
solving several sets of linear equations. The index ranges of the result are
A'Range(2) and X'Range(2). Constraint_Error is raised if A'Length(1), A'Length(2), and
X'Length(1) are not equal. Constraint_Error is raised if the matrix A is
ill-conditioned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Inverse (A : Complex_Matrix) @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns a matrix B such that A * B is
(nearly) equal to the unit matrix. The index ranges of the result are A'Range(2)
and A'Range(1). Constraint_Error is raised if A'Length(1) is not equal to
A'Length(2). Constraint_Error is raised if the matrix A is ill-conditioned.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Determinant (A : Complex_Matrix) @key{return} Complex;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns the determinant of the matrix
A. Constraint_Error is raised if A'Length(1) is not equal to A'Length(2).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Eigenvalues(A : Complex_Matrix) @key{return} Real_Vector;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns the eigenvalues of the
Hermitian matrix A as a vector sorted into order with the largest first.
Constraint_Error is raised if A'Length(1) is not equal to A'Length(2). The
index range of the result is A'Range(1). Argument_Error is raised if the matrix
A is not Hermitian.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[A Hermitian matrix is one whose transpose is
  equal to its complex conjugate. The eigenvalues of a Hermitian matrix are
  always real. We only support this case because algorithms for solving the
  general case are inherently unstable.]}
@end{Discussion}


@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Eigensystem(A       : @key{in}  Complex_Matrix;
                      Values  :  @key{out} Real_Vector;
                      Vectors :  @key{out} Complex_Matrix);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This procedure computes both the eigenvalues and
eigenvectors of the Hermitian matrix A. The out parameter Values is the same as
that obtained by calling the function Eigenvalues. The out parameter Vectors is
a matrix whose columns are the eigenvectors of the matrix A. The order of the
columns corresponds to the order of the eigenvalues. The eigenvectors are
mutually orthonormal, including when there are repeated eigenvalues.
Constraint_Error is raised if A'Length(1) is not equal to A'Length(2). The
index ranges of the parameter Vectors are those of A. Argument_Error is raised
if the matrix A is not Hermitian.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Unit_Matrix (Order            : Positive;
                      First_1, First_2 : Integer := 1)
                                         @key{return} Complex_Matrix;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[This function returns a square
@i{unit matrix}@Defn2{Term=[unit matrix],Sec=[complex matrix]}
with Order**2 components and
lower bounds of First_1 and First_2 (for the first and second index ranges
respectively). All components are set to (0.0, 0.0) except for the main diagonal,
whose components are set to (1.0, 0.0). Constraint_Error is raised
if First_1 + Order @en 1 > Integer'Last or First_2 + Order @en 1 > Integer'Last.]}

@end{DescribeCode}

@end{StaticSem}

@begin{ImplReq}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Accuracy requirements for the subprograms Solve,
Inverse, Determinant, Eigenvalues and Eigensystem are implementation defined.]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[The
accuracy requirements for the subprograms Solve,
Inverse, Determinant, Eigenvalues and Eigensystem for type Complex_Matrix.]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[For operations not involving an inner product, the
accuracy requirements are those of the corresponding operations of the type
Real'Base and Complex in both the strict mode and the relaxed mode
(see @RefSecNum{Numeric Performance Requirements}).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[For operations involving an inner product, no
requirements are specified in the relaxed mode. In the strict mode the modulus
of the absolute error of the inner product @i{X}*@i{Y} shall not exceed
@i{g}*@key{abs}(@i{X})*@key{abs}(@i{Y}) where @i{g} is defined as]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@i{g} = @i{X}'Length * Real'Machine_Radix**(1 @en@; Real'Model_Mantissa)
    for mixed complex and real operands]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@i{g} = sqrt(2.0) * @i{X}'Length * Real'Machine_Radix**(1 @en@; Real'Model_Mantissa)
    for two complex operands]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00418-01]}
@ChgAdded{Version=[2],Text=[For the L2-norm, no accuracy requirements are
specified in the relaxed mode. In
the strict mode the relative error on the norm shall not
exceed @i<g> / 2.0 + 3.0 * Real'Model_Epsilon
where @i<g> has the definition appropriate for two complex operands.]}


@end{ImplReq}

@begin{DocReq}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Implementations shall document any techniques used
to reduce cancellation errors such as extended precision arithmetic.]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[Any
techniques used to reduce cancellation errors in
Numerics.Generic_Complex_Arrays shall be documented.]}]}
@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The above accuracy requirement is met by the
  canonical implementation of the
  inner product by multiplication and addition using the corresponding
  operations of type Complex and performing the cumulative addition using
  ascending indices. Note however, that some hardware provides special
  operations for the computation of the inner product and although these may be
  fast they may not meet the accuracy requirement specified. See Accuracy and
  Stability of Numerical Algorithms by N J Higham (ISBN 0-89871-355-2),
  Sections 3.1 and 3.6.]}
@end{ImplNote}
@end{DocReq}

@begin{ImplPerm}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[The nongeneric equivalent packages may, but need
not, be actual instantiations of the generic package for the appropriate
predefined type.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Although many operations are defined in terms of
operations from numerics.@!generic_complex_types, they need not be implemented by
calling those operations provided that the effect is the same.]}
@end{ImplPerm}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Implementations should implement the Solve and
Inverse functions using established techniques. Implementations are recommended
to refine the result by performing an iteration on the residuals; if this is
done then it should be documented.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Solve and Inverse for Numerics.Generic_Complex_Arrays should be
implemented using established techniques and the result should be refined
by an iteration on the residuals.]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[It is not the intention that any special provision
should be made to determine whether a matrix is ill-conditioned or not. The
naturally occurring overflow (including division by zero) which will result
from executing these functions with an ill-conditioned matrix and thus raise
Constraint_Error is sufficient.]}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[There isn't any advice for the implementation to
document with this paragraph.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[The test that a matrix is Hermitian should use the
equality operator to compare the real components and negation followed by
equality to compare the imaginary components
(see @RefSecNum{Model of Floating Point Arithmetic}).]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The equality and negation operators should be used to test that a matrix is
Hermitian.]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
@ChgAdded{Version=[2],Text=[Implementations should not perform operations on
mixed complex and real operands by first converting the real operand to
complex. See @RefSecNum{Complex Types}.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Mixed real and complex operations should not be performed by converting
the real operand to complex.]}]}

@end{ImplAdvice}


@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00296-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The package Numerics.Generic_Complex_Arrays and its nongeneric equivalents
  are new.@Comment{ It would be better if this was called
  "Ada.Numerics.Generic_Imitation_Arrays", 'cause that's the opposite of Real. :-)
  Just checking if anyone reads this stuff.}]}
@end{Extend95}


