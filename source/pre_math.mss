@comment{ $Source: e:\\cvsroot/ARM/Source/pre_math.mss,v $ }
@comment{ $Revision: 1.47 $ $Date: 2019/04/09 04:56:52 $ $Author: randy $ }
@Part(predefmath, Root="ada.mss")

@Comment{$Date: 2019/04/09 04:56:52 $}

@LabeledClause{The Numerics Packages}

@begin{Intro}
The library package Numerics is the parent of several child units that provide
facilities for mathematical computation. One child, the generic package
Generic_Elementary_Functions, is defined in @RefSecNum{Elementary Functions},
together with nongeneric equivalents;
two others,
the package Float_Random and the generic package Discrete_Random,
are defined in @RefSecNum{Random Number Generation}.
Additional (optional)
children are defined in @RefSec{Numerics}.
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[1], Kind=[Deleted]}
@ChgDeleted[Version=[1],Text=<@ @;@comment{Empty paragraph to hang junk paragraph number from original RM}>]
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00388-01]}
@key[package] Ada.Numerics @key[is]@ChildUnit{Parent=[Ada],Child=[Numerics]}
   @key[pragma] Pure(Numerics);
   @AdaExcDefn{Argument_Error} : @key[exception];
   @AdaObjDefn{Pi} : @key[constant] :=
          3.14159_26535_89793_23846_26433_83279_50288_41971_69399_37511;@Chg{Version=[2],New=[
   @pi  : @key[constant] := Pi;],Old=[]}
   @AdaObjDefn{e}  : @key[constant] :=
          2.71828_18284_59045_23536_02874_71352_66249_77572_47093_69996;
@key[end] Ada.Numerics;
@end{Example}

The Argument_Error exception is raised by a subprogram in
a child unit of Numerics
to signal that one or more of the actual subprogram parameters are outside the
domain of the corresponding mathematical function.

@end{StaticSem}

@begin{ImplPerm}
The implementation may specify the values of Pi and e to a larger number of
significant digits.
@begin{Reason}
   51 digits seem more than adequate for all present computers; converted to
   binary, the values given above are accurate to more than 160 bits.
   Nevertheless, the permission allows implementations to accommodate
   unforeseen hardware advances.
@end{Reason}
@end{ImplPerm}

@begin{Extend83}
@Defn{extensions to Ada 83}
Numerics and its children were not predefined in Ada 83.
@end{Extend83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00388-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The alternative declaration of @pi is new.]}
@end{Extend95}


@LabeledSubClause{Elementary Functions}

@begin{Intro}
Implementation-defined
approximations to the mathematical functions known as the @lquotes@;elementary
functions@rquotes@; are provided by the subprograms in
Numerics.@!Generic_@!Elementary_@!Functions. Nongeneric equivalents of this generic
package for each of the predefined floating point types are also provided as
children of Numerics.
@ImplDef{The accuracy actually achieved by the elementary functions.}
@end{Intro}

@begin{StaticSem}
@Leading@;The generic library package
Numerics.Generic_Elementary_Functions has the following declaration:
@begin{Example}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0241-1]}
@key{generic}
   @key{type} Float_Type @key{is} @key{digits} <>;
@ChildUnit{Parent=[Ada.Numerics],Child=[Generic_@!Elementary_@!Functions]}
@key{package} Ada.Numerics.Generic_Elementary_Functions@Chg{Version=[5],New=[],Old=[ @key{is}]}
   @Chg{Version=[5],New=[@key[with]],Old=[@key[pragma]]} Pure@Chg{Version=[5],New=[, Nonblocking @key[is]],Old=[(Generic_Elementary_Functions);]}

   @key{function} @AdaSubDefn{Sqrt}    (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Log}     (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Log}     (X, Base     : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Exp}     (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} "**"    (Left, Right : Float_Type'Base) @key{return} Float_Type'Base;

   @key{function} @AdaSubDefn{Sin}     (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Sin}     (X, Cycle    : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Cos}     (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Cos}     (X, Cycle    : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Tan}     (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Tan}     (X, Cycle    : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Cot}     (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Cot}     (X, Cycle    : Float_Type'Base) @key{return} Float_Type'Base;

   @key{function} @AdaSubDefn{Arcsin}  (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Arcsin}  (X, Cycle    : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Arccos}  (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Arccos}  (X, Cycle    : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Arctan}  (Y           : Float_Type'Base;
                     X           : Float_Type'Base := 1.0)
                                                    @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Arctan}  (Y           : Float_Type'Base;
                     X           : Float_Type'Base := 1.0;
                     Cycle       : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Arccot}  (X           : Float_Type'Base;
                     Y           : Float_Type'Base := 1.0)
                                                    @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Arccot}  (X           : Float_Type'Base;
                     Y           : Float_Type'Base := 1.0;
                     Cycle       : Float_Type'Base) @key{return} Float_Type'Base;

   @key{function} @AdaSubDefn{Sinh}    (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Cosh}    (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Tanh}    (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Coth}    (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Arcsinh} (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Arccosh} (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Arctanh} (X           : Float_Type'Base) @key{return} Float_Type'Base;
   @key{function} @AdaSubDefn{Arccoth} (X           : Float_Type'Base) @key{return} Float_Type'Base;

@key{end} Ada.Numerics.Generic_Elementary_Functions;
@end{Example}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0020],ARef=[AI95-00126-01]}
@ChildUnit{Parent=[Ada.Numerics],Child=[Elementary_@!Functions]}
The library package Numerics.Elementary_Functions
@Chg{New=[is declared pure and ],Old=[]}defines the same subprograms as
Numerics.@!Generic_@!Elementary_@!Functions,
except that the predefined type Float is systematically substituted for
Float_Type'Base throughout. Nongeneric equivalents of
Numerics.@!Generic_@!Elementary_@!Functions for each of the other predefined
floating point types are defined similarly, with the names
Numerics.@!Short_@!Elementary_@!Functions, Numerics.@!Long_@!Elementary_@!Functions, etc.
@begin{Reason}
   The nongeneric equivalents are provided to allow the programmer to
   construct simple mathematical applications without being required to
   understand and use generics.
@end{Reason}

The functions have their usual mathematical meanings. When the Base parameter
is specified, the Log function computes the logarithm to the given base;
otherwise, it computes the natural logarithm. When the Cycle parameter is
specified, the parameter X of the forward trigonometric functions (Sin, Cos,
Tan, and Cot) and the results of the inverse trigonometric functions (Arcsin,
Arccos, Arctan, and Arccot) are measured in units such that a full cycle of
revolution has the given value; otherwise, they are measured in radians.

@Leading@;The computed results of the mathematically multivalued functions are
rendered single-valued by the following conventions, which are meant to imply
the principal branch:
@begin{Itemize}
   The results of the Sqrt and Arccosh functions and that of the exponentiation
   operator are nonnegative.

   The result of the Arcsin function is in the quadrant containing the point
   (1.0, @i[x]), where @i[x] is the value of the parameter X. This quadrant is
   I or IV; thus, the range of the Arcsin function is approximately
   @en@Pi/2.0 to @Pi/2.0
   (@en@R[Cycle]/4.0 to @R[Cycle]/4.0,
   if the parameter Cycle is specified).

   The result of the Arccos function is in the quadrant containing the point
   (@i{x}, 1.0), where @i[x] is the value of the parameter X. This quadrant is
   I or II; thus, the Arccos function ranges from 0.0 to approximately
   @Pi (@R[Cycle]/2.0, if the parameter Cycle is specified).

   The results of the Arctan and Arccot functions are in the quadrant
   containing the point (@i[x], @i[y]), where @i[x] and @i[y] are the values of
   the parameters X and Y, respectively. This may be any quadrant (I through
   IV) when the parameter X (resp., Y) of Arctan (resp., Arccot) is specified,
   but it is restricted to quadrants I and IV (resp., I and II) when that
   parameter is omitted. Thus, the range when that parameter is specified is
   approximately @en@Pi to @Pi
   (@en@R[Cycle]/2.0 to @R[Cycle]/2.0,
   if the parameter Cycle is specified); when omitted, the range of Arctan
   (resp., Arccot) is that of Arcsin (resp., Arccos), as given above. When the
   point (@i[x], @i[y]) lies on the negative x-axis, the result approximates
   @begin{Itemize}
      @Pi (resp., @en@Pi) when the sign of the parameter Y is
      positive (resp., negative), if Float_Type'Signed_Zeros is True;

      @Pi, if Float_Type'Signed_Zeros is False.
   @end{Itemize}
@end{Itemize}

(In the case of the inverse trigonometric functions, in which a result lying on
or near one of the axes may not be exactly representable, the approximation
inherent in computing the result may place it in an adjacent quadrant, close to
but on the wrong side of the axis.)
@end{StaticSem}

@begin{RunTime}
@Leading@;The exception Numerics.Argument_Error is raised, signaling a
parameter value outside the domain of the corresponding mathematical function,
in the following cases:
@begin{Itemize}
   by any forward or inverse trigonometric function with specified cycle, when
   the value of the parameter Cycle is zero or negative;

   by the Log function with specified base, when the value of the parameter
   Base is zero, one, or negative;

   by the Sqrt and Log functions, when the value of the parameter X is
   negative;

   by the exponentiation operator, when the value of the left operand is
   negative or when both operands have the value zero;

   by the Arcsin, Arccos, and Arctanh functions, when the absolute value of the
   parameter X exceeds one;

   by the Arctan and Arccot functions, when the parameters X and Y both have
   the value zero;

   by the Arccosh function, when the value
   of the parameter X is less than one; and

   by the Arccoth function, when the absolute value
   of the parameter X is less than one.
@end{Itemize}

@Leading
The exception Constraint_Error is raised, signaling a pole of the mathematical
function (analogous to dividing by zero), in the following cases, provided that
Float_Type'Machine_Overflows is True:
@begin{Itemize}
   by the Log, Cot, and Coth functions, when the value of the parameter X is
   zero;

   by the exponentiation operator, when the value of the left operand is zero
   and the value of the exponent is negative;

   by the Tan function with specified cycle, when the value of the parameter X
   is an odd multiple of the quarter cycle;

   by the Cot function with specified cycle, when the value of the parameter X
   is zero or a multiple of the half cycle; and

   by the Arctanh and Arccoth functions, when the absolute value of the
   parameter X is one.
@end{Itemize}

@redundant[Constraint_Error can also be raised
when a finite result overflows
(see @RefSecNum{Accuracy Requirements for the Elementary Functions}); this may
occur for parameter values sufficiently @i{near} poles, and, in the case of
some of the functions, for parameter values with sufficiently large
magnitudes.]@PDefn{unspecified} When Float_Type'Machine_Overflows is False,
the result at poles is unspecified.
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
   will permit both the predefined arithmetic operations and implementations of
   the elementary functions to deliver signed infinities (and set the overflow
   flag defined by the binding) instead of raising Constraint_Error in overflow
   situations, when traps are disabled. Similarly, it is appropriate for the
   elementary functions to deliver signed infinities (and set the zero-divide
   flag defined by the binding) instead of raising Constraint_Error at poles,
   when traps are disabled. Finally, such a binding should also specify the
   behavior of the elementary functions, when sensible, given parameters with
   infinite values.
@end{Discussion}

When one parameter of a function with multiple parameters represents a pole and
another is outside the function's domain, the latter takes precedence (i.e.,
Numerics.Argument_Error is raised).
@end{RunTime}

@begin{ImplReq}
In the implementation of Numerics.Generic_Elementary_Functions,
the range of intermediate values allowed during the calculation
of a final result shall not be affected by any
range constraint of the subtype Float_Type.
@begin{ImplNote}
   Implementations of Numerics.Generic_Elementary_Functions written in Ada
   should therefore avoid declaring local variables of subtype Float_Type; the
   subtype Float_Type'Base should be used instead.
@end{ImplNote}

@Leading@Defn2{Term=[prescribed result],
Sec=[for the evaluation of an elementary function]}
In the following cases, evaluation of an elementary function shall yield the
@i{prescribed result},
provided that the preceding rules do not call for an exception to be
raised:
@begin{Itemize}
   When the parameter X has the value zero, the Sqrt, Sin, Arcsin, Tan, Sinh,
   Arcsinh, Tanh, and Arctanh functions yield a result of zero, and the Exp,
   Cos, and Cosh functions yield a result of one.

   When the parameter X has the value one, the Sqrt function yields a result
   of one, and the Log, Arccos, and Arccosh functions yield a result of zero.

   When the parameter Y has the value zero and the parameter X has a positive
   value, the Arctan and Arccot functions yield a result of zero.

   The results of the Sin, Cos, Tan, and Cot functions with specified cycle are
   exact when the mathematical result is zero; those of the first two are also
   exact when the mathematical result is @PorM 1.0.

   Exponentiation by a zero exponent yields the value one. Exponentiation by
   a unit exponent yields the value of the left operand. Exponentiation of
   the value one yields the value one. Exponentiation of the value zero
   yields the value zero.
@end{Itemize}

Other accuracy requirements for the elementary functions, which apply only in
implementations conforming to the Numerics Annex, and then only in the
@lquotes@;strict@rquotes@; mode defined there (see @RefSecNum{Numeric Performance Requirements}),
are given in @RefSecNum{Accuracy Requirements for the Elementary Functions}.

@Leading@;When Float_Type'Signed_Zeros is True, the sign of a zero result
shall be as follows:
@begin{itemize}
   A prescribed zero result delivered
   @i{at the origin} by one of the odd functions (Sin, Arcsin, Sinh,
   Arcsinh, Tan, Arctan or Arccot as a function of Y when X is fixed and
   positive, Tanh, and Arctanh) has the sign of the parameter X (Y, in the case
   of Arctan or Arccot).

   A prescribed zero result delivered by one of the odd functions @i{away from
   the origin}, or by some other elementary function, has
   an implementation-defined sign.
   @ImplDef{The sign of a zero result from some of the operators or functions
   in Numerics.Generic_Elementary_Functions, when Float_Type'Signed_Zeros is
   True.}

   @redundant[A zero result that is not a prescribed result
   (i.e., one that results from
   rounding or underflow) has the correct mathematical sign.]
   @begin{Reason}
      This is a consequence of the rules specified in IEC 559:1989 as they
      apply to underflow situations with traps disabled.
   @end{Reason}
@end{itemize}
@end{ImplReq}

@begin{ImplPerm}
The nongeneric equivalent packages may, but need not, be actual
instantiations of the generic package for the appropriate predefined type.
@end{ImplPerm}

@begin{DiffWord83}
@Leading@;The semantics of Numerics.Generic_Elementary_Functions differs from
Generic_Elementary_Functions as defined in ISO/IEC DIS 11430 (for Ada 83)
in the following ways:
@begin{itemize}
   The generic package is a child unit of the package defining the
   Argument_Error exception.

   DIS 11430 specified names for the nongeneric equivalents, if provided.
   Here, those nongeneric equivalents are required.

   Implementations are not allowed to impose an optional restriction that the
   generic actual parameter associated with Float_Type be unconstrained.
   (In view of the ability to declare variables of subtype Float_Type'Base in
   implementations of Numerics.Generic_Elementary_Functions, this flexibility
   is no longer needed.)

   The sign of a prescribed zero result at the origin of the odd functions is
   specified, when Float_Type'Signed_Zeros is True. This conforms with
   recommendations of Kahan and other numerical analysts.

   The dependence of Arctan and Arccot on the sign of a parameter value of zero
   is tied to the value of Float_Type'Signed_Zeros.

   Sqrt is prescribed to yield a result of one when its parameter has the
   value one. This guarantee makes it easier to achieve certain prescribed
   results of the complex elementary functions
   (see @RefSec{Complex Elementary Functions}).

   Conformance to accuracy requirements is conditional.
@end{itemize}
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0020],ARef=[AI95-00126-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Explicitly stated that the
  nongeneric equivalents of Generic_Elementary_Functions are pure.]}
@end{DiffWord95}


@LabeledSubClause{Random Number Generation}

@begin{Intro}
@redundant[Facilities for the generation of pseudo-random floating point numbers are
provided in the package Numerics.Float_Random; the generic package
Numerics.Discrete_Random provides similar facilities for the generation of
pseudo-random integers and pseudo-random values of enumeration
types.
@Defn{random number}
For brevity, pseudo-random values of any of these types are called @i{random
numbers}.

Some of the facilities provided are basic to all applications of random
numbers. These include a limited private type each of whose objects serves as
the generator of a (possibly distinct) sequence of random numbers; a function
to obtain the @lquotes@;next@rquotes@; random number from a given sequence of random numbers
(that is, from its generator); and subprograms to initialize or reinitialize a
given generator to a time-dependent state or a state denoted by a single
integer.

Other facilities are provided specifically for advanced applications. These
include subprograms to save and restore the state of a given generator; a
private type whose objects can be used to hold the saved state of a
generator;
and subprograms to obtain a string representation of a given generator state,
or, given such a string representation, the corresponding state.]
@begin{Discussion}
These facilities support a variety of requirements ranging from repeatable
sequences (for debugging) to unique sequences in each execution of a program.
@end{Discussion}
@end{Intro}

@begin{StaticSem}
@Leading@;The library package Numerics.Float_Random has the following declaration:
@begin{Example}
@key[package] Ada.Numerics.Float_Random @key[is]@ChildUnit{Parent=[Ada.Numerics],Child=[Float_@!Random]}

   -- @RI{Basic facilities}

   @key[type] @AdaTypeDefn{Generator} @key[is] @key[limited] @key[private];


   @key[subtype] @AdaSubtypeDefn{Name=[Uniformly_Distributed],Of=[Float]} @key[is] Float @key[range] 0.0 .. 1.0;
   @key[function] @AdaSubDefn{Random} (Gen : Generator) @key[return] Uniformly_Distributed;


   @key[procedure] @AdaSubDefn{Reset} (Gen       : @key[in] Generator;
                    Initiator : @key[in] Integer);
   @key[procedure] @AdaSubDefn{Reset} (Gen       : @key[in] Generator);


   -- @RI{Advanced facilities}

   @key[type] @AdaTypeDefn{State} @key[is] @key[private];


   @key[procedure] @AdaSubDefn{Save}  (Gen        : @key[in]  Generator;
                    To_State   : @key[out] State);
   @key[procedure] @AdaSubDefn{Reset} (Gen        : @key[in]  Generator;
                    From_State : @key[in]  State);


   @AdaObjDefn{Max_Image_Width} : @key[constant] := @RI{implementation-defined integer value};


   @key[function] @AdaSubDefn{Image} (Of_State    : State)  @key[return] String;
   @key[function] @AdaSubDefn{Value} (Coded_State : String) @key[return] State;


@key[private]
   ... -- @RI{not specified by the language}
@key[end] Ada.Numerics.Float_Random;
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00360-01]}
@ChgAdded{Version=[2],Text=[The type Generator
needs finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}

The generic library package Numerics.Discrete_Random has the following
declaration:
@begin{Example}
@ChildUnit{Parent=[Ada.Numerics],Child=[Discrete_@!Random]}
@key[generic]
   @key[type] Result_Subtype @key[is] (<>);
@key[package] Ada.Numerics.Discrete_Random @key[is]

   -- @RI{Basic facilities}

   @key[type] @AdaTypeDefn{Generator} @key[is] @key[limited] @key[private];


   @key[function] @AdaSubDefn{Random} (Gen : Generator) @key[return] Result_Subtype;

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0144-1]}
@ChgAdded{Version=[5],Text=[   @key[function] Random (Gen   : Generator;
                    First : Result_Subtype;
                    Last  : Result_Subtype) @key[return] Result_Subtype
      @key[with] Post => Random'Result @key[in] First .. Last;]}

   @key[procedure] @AdaSubDefn{Reset} (Gen       : @key[in] Generator;
                    Initiator : @key[in] Integer);
   @key[procedure] @AdaSubDefn{Reset} (Gen       : @key[in] Generator);


   -- @RI{Advanced facilities}

   @key[type] @AdaTypeDefn{State} @key[is] @key[private];


   @key[procedure] @AdaSubDefn{Save}  (Gen        : @key[in]  Generator;
                    To_State   : @key[out] State);
   @key[procedure] @AdaSubDefn{Reset} (Gen        : @key[in]  Generator;
                    From_State : @key[in]  State);


   @AdaObjDefn{Max_Image_Width} : @key[constant] := @RI{implementation-defined integer value};


   @key[function] @AdaSubDefn{Image} (Of_State    : State)  @key[return] String;
   @key[function] @AdaSubDefn{Value} (Coded_State : String) @key[return] State;


@key[private]
   ... -- @RI{not specified by the language}
@key[end] Ada.Numerics.Discrete_Random;
@end{Example}
@ImplDef{The value of Numerics.Float_Random.Max_Image_Width.}
@ImplDef{The value of Numerics.Discrete_Random.Max_Image_Width.}
@begin{ImplNote}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0097],ARef=[AI95-00115-01]}
@Leading@;
The following is a possible implementation of the private part of
@Chg{New=[Numerics.Float_Random], Old=[each package]} (assuming the presence
of @lquotes@;@key[with] Ada.Finalization;@rquotes@; as a context clause):
@begin{example}
@key[type] State @key[is] ...;
@key[type] Access_State @key[is] @key[access] State;
@key[type] Generator @key[is] @key[new] Finalization.Limited_Controlled @key[with]
   @key[record]
      S : Access_State := @key[new] State'(...);
   @key[end] @key[record];
@key[procedure] Finalize (G : @key[in] @key[out] Generator);
@end{Example}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0097],ARef=[AI95-00115-01]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00344-01]}
@ChgAdded{Version=[1],Text=[@Chg{Version=[2],New=[],
Old=[Unfortunately, ]}Numerics.Discrete_Random.Generator @Chg{Version=[2],New=[also can],Old=[cannot]} be
implemented this way@Chg{Version=[2],New=[],Old=[, as Numerics.Discrete_Random can be instantiated at any
nesting depth. However, Generator could have a component of a controlled type,
as long as that type is declared in some other (nongeneric) package. One
possible solution would be to implement Numerics.@!Discrete_@!Random in terms
of Numerics.@!Float_@!Random, using a component of Numerics.@!Float_@!Random.Generator
to implement Numerics.@!Float_@!Random.@!Generator]}.]}

Clearly some level of indirection is required in the implementation of a
Generator, since the parameter mode is @key(in) for all operations on a
Generator. For this reason, Numerics.Float_Random and Numerics.Discrete_Random
cannot be declared pure.
@end{ImplNote}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00360-01]}
@ChgAdded{Version=[2],Text=[The type Generator
needs finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization})
in every instantiation of Numerics.Discrete_Random.]}

An object of the limited private type Generator is associated with a sequence
of random numbers. Each generator has a hidden (internal) state, which the
operations on generators use to determine the position in the associated
sequence.
@PDefn{unspecified}
All generators are implicitly initialized to an unspecified state
that does not vary from one program execution to another;
they may also be explicitly initialized, or reinitialized, to a time-dependent
state, to a previously saved state, or to a state uniquely denoted by an
integer value.
@begin{Discussion}
The repeatability provided by the implicit initialization may be exploited for
testing or debugging purposes.
@end{Discussion}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0280-1]}
An object of the private type State can be used to hold the internal state of a
generator. Such objects are only needed if the application is designed to
save and restore generator states or to examine or manufacture
them.@Chg{Version=[3],New=[ The implicit initial value of type State
corresponds to the implicit initial value of all generators.],Old=[]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0280-1]}
  @ChgAdded{Version=[3],Text=[All generators are implicitly initialized to
  the same unchanging value, and using Reset on a default initialized object
  of type State will produce a generator with that same value.]}
@end{Discussion}

@Trailing@;The operations on generators affect the state and therefore the future values
of the associated sequence. The semantics of the operations on generators and
states are defined below.
@begin{DescribeCode}
@begin{Example}
@key[function] Random (Gen : Generator) @key[return] Uniformly_Distributed;
@key[function] Random (Gen : Generator) @key[return] Result_Subtype;
@end{Example}
@Trailing@;@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0144-1]}
Obtains the @lquotes@;next@rquotes@; random number from the given generator,
relative to its current state, according to an implementation-defined
algorithm.@Chg{Version=[5],New=[],Old=[ The result of the function in
Numerics.Float_Random is delivered as a value of
the subtype Uniformly_Distributed, which is a subtype of the predefined type
Float having a range of
0.0 .. 1.0.
The result of the function in an
instantiation of Numerics.Discrete_Random is delivered as a value of the
generic formal subtype Result_Subtype.]}
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],Text=[The
algorithms for random number generation.]}]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[The algorithm is the subject of a @DocReqName@;,
  so we don't separately summarize this implementation-defined item.]}
@end{Discussion}
@begin{Reason}
   The requirement for a level of indirection in accessing the internal state
   of a generator arises from the desire to make Random a function, rather than
   a procedure.
@end{Reason}

@begin{Example}
@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0144-1]}
@ChgAdded{Version=[5],Text=[@key[function] Random (Gen   : Generator;
                 First : Result_Subtype;
                 Last  : Result_Subtype) @key[return] Result_Subtype
   @key[with] Post => Random'Result @key[in] First .. Last;]}
@end{Example}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0144-1]}
@ChgAdded{Version=[5],Type=[Trailing],Text=[Obtains the @lquotes@;next@rquotes@;
random number from the given generator, relative to its current state, according
to an implementation-defined algorithm. If the range First .. Last is a null
range, Constraint_Error is
raised.]}

@begin{Example}
@key[procedure] Reset (Gen       : @key[in] Generator;
                 Initiator : @key[in] Integer);
@key[procedure] Reset (Gen       : @key[in] Generator);
@end{Example}
@Trailing@PDefn{unspecified}
Sets the state of the specified generator to one that is an unspecified
function of the value of the parameter Initiator (or to a time-dependent state,
if only a generator parameter is
specified).
@Defn2{Term=[Time-dependent Reset procedure],Sec=(of the random number generator)}
The latter form of the procedure is known as the @i{time-dependent Reset
procedure}.
@begin{ImplNote}
   The time-dependent Reset procedure can be implemented by mapping the current
   time and date as determined by the system clock into a state, but other
   implementations are possible. For example, a white-noise generator or
   a radioactive source can be used to generate time-dependent states.
@end{ImplNote}

@begin{Example}
@key[procedure] Save  (Gen        : @key[in]  Generator;
                 To_State   : @key[out] State);
@key[procedure] Reset (Gen        : @key[in]  Generator;
                 From_State : @key[in]  State);
@end{Example}
@Trailing@;Save obtains the current state of a generator. Reset gives a
generator the specified state. A generator that is reset to a state previously
obtained by invoking Save is restored to the state it had when Save was
invoked.

@begin{Example}
@key[function] Image (Of_State    : State)  @key[return] String;
@key[function] Value (Coded_State : String) @key[return] State;
@end{Example}
Image provides a representation of a state coded (in an implementation-defined
way) as a string whose length is bounded by the value of Max_Image_Width.
Value is the inverse of Image: Value(Image(S)) = S for each state S that can be
obtained from a generator by invoking Save.
@ImplDef{The string representation of a random number generator's state.}
@end{DescribeCode}
@end{StaticSem}

@begin{RunTime}
Instantiation of Numerics.Discrete_Random with a subtype having a null range
raises Constraint_Error.

@ChgRef{Version=[1],Kind=[Deleted],Ref=[8652/0050],ARef=[AI95-00089]}
@ChgDeleted{Version=[1],Text=[Invoking Value with a string that is not the
image of any generator state raises Constraint_Error.]}
@end{RunTime}

@begin{Bounded}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0050],ARef=[AI95-00089]}
@ChgAdded{Version=[1],Text=[It is a bounded error to invoke Value with a
string that is not the image of any generator
state.@PDefn2{Term=(bounded error),Sec=(cause)}
If the error is detected, Constraint_Error or
Program_Error is raised. Otherwise, a call to Reset with the resulting state
will produce a generator such that calls to Random with this generator will
produce a sequence of values of the appropriate subtype, but which might not
be random in character. That is, the sequence of values might not fulfill the
implementation requirements of this subclause.]}
@end{Bounded}

@begin{ImplReq}
@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0144-1]}
@ChgAdded{Version=[5],Text=[Each call of a Random function has a @i<result
range>;@Defn2{Term=[result range],Sec=[of a Random function]} this is the
range First .. Last for the version of Random with First and Last parameters
and the range of the result subtype of the function otherwise.]}

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0144-1]}
A sufficiently long sequence of random numbers obtained by
@Chg{Version=[5],New=[consecutive],Old=[successive]} calls to
Random @Chg{Version=[5],New=[that have the same generator and result
range ],Old=[]}is approximately uniformly distributed over the
@Chg{Version=[5],New=[result ],Old=[]}range@Chg{Version=[5],New=[],Old=[ of
the result subtype]}.

@begin{Discussion}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0144-1]}
  @ChgAdded{Version=[5],Text=[In this rule, @lquotes@;consecutive@rquotes@;
  means at least that there are
  no intervening explicit calls involving the same generator. This restricts
  the rule to only applying to cases where just the Random function changes
  the generator. We don't mean to impose a requirement if there are
  intervening calls to Reset, to Random with the same generator but a different
  result range, or any other case that would affect the sequence of values
  returned. Operations which use the resulting random values (for instance,
  to store them somewhere) are not considered in determining if calls are
  consecutive.]}
@end{Discussion}

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0144-1]}
@Chg{Version=[5],New=[A],Old=[The]} Random function in an instantiation of
Numerics.Discrete_Random is
guaranteed to yield each value in its result @Chg{Version=[5],New=[range],
Old=[subtype]} in a finite number of calls, provided that the number of such
values does not exceed 2 @+[15].

Other performance requirements for the random number generator, which apply
only in implementations conforming to the Numerics Annex, and then only in the
@lquotes@;strict@rquotes@; mode defined there (see @RefSecNum{Numeric Performance Requirements}),
are given in @RefSecNum{Performance Requirements for Random Number Generation}.
@end{ImplReq}

@begin{DocReq}
No one algorithm for random number generation is best for all applications. To
enable the user to determine the suitability of the random number generators
for the intended application, the implementation shall describe the algorithm
used and shall give its period, if known exactly, or a lower bound on the
period, if the exact period is unknown. Periods that are so long that the
periodicity is unobservable in practice can be described in such terms, without
giving a numerical bound.
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[The
algorithm used for random number generation, including a description of its
period.]}]}

The implementation also shall document the minimum time interval between calls
to the time-dependent Reset procedure that are guaranteed to initiate
different sequences, and it shall document the nature of the strings that
Value will accept without raising Constraint_Error.
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],Text=[The
minimum time interval between
calls to the time-dependent Reset procedure that are guaranteed to initiate
different random number sequences.]}]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[The
minimum time interval between
calls to the time-dependent Reset procedure that is guaranteed to initiate
different random number sequences.]}]}
@end{DocReq}

@begin{ImplAdvice}
Any storage associated with an object of type Generator should be
reclaimed on exit from the scope of the object.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],Text=[Any
storage associated with an object of type Generator of the random
number packages should be reclaimed on exit from the scope of the object.]}]}
@begin{Ramification}
  A level of indirection is implicit in the semantics of the
  operations, given that they all take parameters of mode @key(in).
  This implies that the full type of Generator probably should be a controlled
  type, with appropriate finalization to reclaim any heap-allocated storage.
@end{Ramification}

If the generator period is sufficiently long in relation to the number of
distinct initiator values, then each possible value of Initiator passed to
Reset should initiate a sequence of random numbers that does not, in a
practical sense, overlap the sequence initiated by any other value. If this
is not possible, then the mapping between initiator values and generator states
should be a rapidly varying function of the initiator value.
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[Each
value of Initiator passed to Reset for the random number packages should
initiate a distinct sequence of random numbers, or, if that is not possible,
be at least a rapidly varying function of the initiator value.]}]}
@end{ImplAdvice}

@begin{Notes}
If two or more tasks are to share the same generator, then the tasks have to
synchronize their access to the generator as for any shared variable
(see @RefSecNum(Shared Variables)).

Within a given implementation, a repeatable random number sequence can be
obtained by relying on the implicit initialization of generators or by
explicitly initializing a generator with a repeatable initiator value.
Different sequences of random numbers can be obtained from a given generator in
different program executions by explicitly initializing the generator to a
time-dependent state.

A given implementation of the Random function in Numerics.Float_Random may or
may not be capable of delivering the values 0.0 or 1.0. Portable applications
should assume that these values, or values sufficiently close to them to behave
indistinguishably from them, can occur. If a sequence of random integers from
some fixed range is needed, the application should use the Random function in
an appropriate instantiation of Numerics.Discrete_Random, rather than
transforming the result of the Random function in Numerics.Float_Random.
However, some applications with unusual requirements, such as for a sequence of
random integers each drawn from a different range, will find it more convenient
to transform the result of the floating point Random function. For
@R[M] @geq 1, the expression
@begin{Example}
   Integer(Float(M) * Random(G)) mod M
@end{Example}

@NoPrefix@;transforms the result of Random(G) to an integer uniformly distributed over the
range 0 .. @R[M]@en@;1; it is valid even if Random delivers 0.0 or 1.0.
Each value of the result range is possible, provided that M is not too large.
Exponentially distributed (floating point) random numbers with mean and
standard deviation 1.0 can be obtained by the transformation
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
   -Log(Random(G) + Float'Model_Small)@Chg{Version=[2],New=[],Old=[)]}
@end{Example}

@NoPrefix@;where Log comes from Numerics.Elementary_Functions
(see @RefSecNum{Elementary Functions});
in this expression, the addition of Float'Model_Small avoids the
exception that would be raised were Log to be given the value zero, without
affecting the result (in most implementations) when Random returns a nonzero
value.
@end{Notes}

@begin{Examples}
@Leading@Keepnext@i{Example of a program that plays a simulated dice game:}
@begin{Example}@Trailing
@key[with] Ada.Numerics.Discrete_Random;
@key[procedure] Dice_Game @key[is]@Softpage
   @key[subtype] Die @key[is] Integer @key[range] 1 .. 6;
   @key[subtype] Dice @key[is] Integer @key[range] 2*Die'First .. 2*Die'Last;
   @key[package] Random_Die @key[is] @key[new] Ada.Numerics.Discrete_Random (Die);
   @key[use] Random_Die;
   G : Generator;
   D : Dice;@Softpage
@key[begin]@Softpage
   Reset (G);  -- @RI{Start the generator in a unique state in each run}
   @key[loop]
      -- @RI{Roll a pair of dice; sum and process the results}
      D := Random(G) + Random(G);
      ...
   @key[end] @key[loop];@Softpage
@key[end] Dice_Game;
@end{Example}

@Leading@Keepnext@i{Example of a program that simulates coin tosses:}
@begin{Example}@Trailing
@key[with] Ada.Numerics.Discrete_Random;
@key[procedure] Flip_A_Coin @key[is]@Softpage
   @key[type] Coin @key[is] (Heads, Tails);
   @key[package] Random_Coin @key[is] @key[new] Ada.Numerics.Discrete_Random (Coin);
   @key[use] Random_Coin;
   G : Generator;@Softpage
@key[begin]@Softpage
   Reset (G);  -- @RI{Start the generator in a unique state in each run}
   @key[loop]
      -- @RI{Toss a coin and process the result}
      @key[case] Random(G) @key[is]
          @key[when] Heads =>
             ...
          @key[when] Tails =>
             ...
      @key[end] @key[case];
   ...
   @key[end] @key[loop];@Softpage
@key[end] Flip_A_Coin;
@end{Example}

@Leading@Keepnext@i{Example of a parallel simulation of a physical system,
with a separate generator of event probabilities in each task:}
@begin{Example}
@key[with] Ada.Numerics.Float_Random;
@key[procedure] Parallel_Simulation @key[is]@Softpage
   @key[use] Ada.Numerics.Float_Random;
   @key[task] @key[type] Worker @key[is]
      @key[entry] Initialize_Generator (Initiator : @key[in] Integer);
      ...
   @key[end] Worker;
   W : @key[array] (1 .. 10) @key[of] Worker;@Softpage
   @key[task] @key[body] Worker @key[is]
      G : Generator;
      Probability_Of_Event : Uniformly_Distributed;
   @key[begin]@Softpage
      @key[accept] Initialize_Generator (Initiator : @key[in] Integer) @key[do]
         Reset (G, Initiator);
      @key[end] Initialize_Generator;
      @key[loop]
         ...
         Probability_Of_Event := Random(G);
         ...
      @key[end] @key[loop];
   @key[end] Worker;@Softpage
@key[begin]@Softpage
   -- @RI{Initialize the generators in the Worker tasks to different states}
   @key[for] I @key[in] W'Range @key[loop]
      W(I).Initialize_Generator (I);
   @key[end] @key[loop];
   ... -- @RI{Wait for the Worker tasks to terminate}@Softpage
@key[end] Parallel_Simulation;
@end{Example}
@end{Examples}

@begin{Notes}
@i{Notes on the last example:}
Although each Worker task initializes its generator to a different state, those
states will be the same in every execution of the program. The generator
states can be initialized uniquely in each program execution by instantiating
Ada.Numerics.Discrete_Random for the type Integer in the main procedure,
resetting the generator obtained from that instance to a time-dependent state,
and then using random integers obtained from that generator to initialize the
generators in each Worker task.
@end{Notes}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00360-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @b[Amendment Correction:] Type Generator in Numerics.Float_Random and in an
  instance of Numerics.Discrete_Random is defined to need finalization. If the
  restriction No_Nested_Finalization (see @RefSecNum{Tasking Restrictions})
  applies to the partition, and Generator does not have a controlled part, it
  will not be allowed in local objects in Ada 2005 whereas it would be allowed
  in original Ada 95. Such code is not portable, as another Ada compiler may
  have a controlled part in Generator, and thus would be illegal.]}
@end{Incompatible95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0050],ARef=[AI95-00089-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Made the passing of an incorrect
  Image of a generator a bounded error, as it @Chg{Version=[3],New=[might],Old=[may]}
  not be practical to check
  for problems (if a generator consists of several related values).]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0280-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Specified the implicit initial
  value for (sub)type State. This was unspecified in Ada 95 and Ada 2005,
  so a program depending on some other initial value is very unlikely
  and certainly was not portable. An implementation can use default expressions,
  aspect Default_Value, or aspect Default_Component_Value to keep the
  representation of the type unchanged while meeting this new requirement.]}
@end{DiffWord2005}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0144-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012} The function Random
  with First and Last parameters is new.]}
@end{Extend2012}
