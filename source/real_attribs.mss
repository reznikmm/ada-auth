@Part(realattribs, Root="ada.mss")

@Comment{$Date: 2005/08/17 00:07:41 $}

@comment{$Source: e:\\cvsroot/ARM/Source/real_attribs.mss,v $}
@comment{$Revision: 1.25 $}

@LabeledSubClause{Attributes of Floating Point Types}

@begin{StaticSem}
@Leading@Defn2{Term=[representation-oriented attributes],
               Sec=[of a floating point subtype]}
The following @i{representation-oriented attributes} are defined for
@PrefixType{every subtype S of a floating point type @i{T}}.
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<Machine_Radix>,
  Text=[Yields the radix of the hardware
   representation of the type @i{T}. The value of this attribute is of
   the type @i{universal_integer}.]}
@end{Description}

@Defn{canonical form}
The values of other representation-oriented attributes of a floating point
subtype, and of the @lquotes@;primitive function@rquotes@; attributes of a floating point
subtype described later, are defined in terms of a particular representation
of nonzero values called the @i{canonical form}.
The canonical form (for the type @i{T}) is the form@*
@ @ @ @ @PorM @RI{mantissa} @Times @RI{T}@R['Machine_Radix]@+{@RI{exponent}}@*
where
@begin{Itemize}
   @i{mantissa} is a fraction in the number base @i{T}'Machine_Radix,
   the first digit of which is nonzero, and

   @i{exponent} is an integer.
@end{Itemize}
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<Machine_Mantissa>,
  Text=[Yields the largest value of @RI{p} such that every value expressible
   in the canonical form (for the type @i{T}), having a @RI{p}-digit
   @i{mantissa} and an @i{exponent} between @i{T}'Machine_Emin and
   @i{T}'Machine_Emax, is a machine number (see @RefSecNum{Floating Point Types})
   of the type @i{T}. This attribute yields a value of the type
   @i{universal_integer}.]}
@begin{Ramification}
   Values of a type held in an extended register are, in general, not machine
   numbers of the type, since they cannot be expressed in the canonical form
   with a sufficiently short @i{mantissa}.
@end{Ramification}

@Attribute{Prefix=<S>, AttrName=<Machine_Emin>,
  Text=[Yields the smallest (most negative) value of @i{exponent} such
   that every value expressible in the canonical form (for the type @i{T}),
   having a @i{mantissa} of @i{T}'Machine_Mantissa digits, is a machine number
   (see @RefSecNum{Floating Point Types}) of the type
   @i{T}.
   This attribute yields
   a value of the type @i{universal_integer}.]}

@Attribute{Prefix=<S>, AttrName=<Machine_Emax>,
  Text=[Yields the largest (most positive) value of @i{exponent} such that
   every value expressible in the canonical form (for the type @i{T}), having
   a @i{mantissa} of @i{T}'Machine_Mantissa digits, is a machine number
   (see @RefSecNum{Floating Point Types}) of the type @i{T}. This attribute
   yields a value of the type @i{universal_integer}.]}
@begin{Ramification}

  Note that the above definitions do not determine unique values for the
  representation-oriented attributes of floating point types.
  The implementation may choose any set of values that collectively
  satisfies the definitions.

@end{Ramification}

@Attribute{Prefix=<S>, AttrName=<Denorm>,
  Text=[Yields the value True if every value expressible in the form@*
   @ @ @ @ @PorM @RI{mantissa} @Times @RI{T}@R{'Machine_Radix}@+{@RI{T}@R{'Machine_Emin}}@*
   where @i{mantissa} is a nonzero @i{T}'Machine_Mantissa-digit fraction in the
   number base @i{T}'Machine_Radix, the first digit of which is zero,
   is a machine number (see @RefSecNum{Floating Point Types}) of the type
   @i{T}; yields the value False otherwise.
   The value
   of this attribute is of the predefined type Boolean.]}
@end{Description}

@Defn{denormalized number}
The values described by the formula in the definition of S'Denorm are called
@i{denormalized numbers}.
@Defn{normalized number}
A nonzero machine number that is not a denormalized number is a
@i{normalized number}.
@Defn{represented in canonical form}
@Defn{canonical-form representation}
A normalized number @RI{x} of a given type @i{T} is said to be
@i{represented in canonical form} when it is expressed in the
canonical form (for the type @i{T}) with a @i{mantissa}
having @i{T}'Machine_Mantissa digits;
the resulting form is the @i{canonical-form representation} of @RI{x}.
@begin{Discussion}
   The intent is that S'Denorm be True when such denormalized numbers exist
   and are generated in the circumstances defined by IEC 559:1989,
   though the latter requirement is not formalized here.
@end{Discussion}
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<Machine_Rounds>,
  Text=[Yields the value True if rounding is performed on inexact
   results of every predefined operation that yields a result of the
   type @i{T}; yields the value False otherwise.
   The value
   of this attribute is of the predefined type Boolean.]}
@begin{Discussion}
   @Leading@;It is difficult to be more precise about what it means to round
   the result of a predefined operation. If the implementation does not use
   extended registers, so that every arithmetic result is necessarily a machine
   number, then rounding seems to imply two things:
   @begin{Itemize}
      S'Model_Mantissa = S'Machine_Mantissa, so that operand preperturbation
      never occurs;

      when the exact mathematical result is not a machine number, the result of
      a predefined operation must be the nearer of the two adjacent machine
      numbers.
   @end{Itemize}

   Technically, this attribute should yield False when extended registers are
   used, since a few computed results will cross over the half-way point as
   a result of double rounding, if and when a value held in an extended
   register has to be reduced in precision to that of the machine numbers. It
   does not seem desirable to preclude the use of extended registers when
   S'Machine_Rounds could otherwise be True.
@end{Discussion}

@Attribute{Prefix=<S>, AttrName=<Machine_Overflows>,
  Text=[Yields the value True if overflow and
   divide-by-zero are detected and
   reported by raising Constraint_Error for every predefined operation that
   yields a result of the type @i{T};
   yields the value False otherwise.
   The value of this
   attribute is of the predefined type Boolean.]}

@Attribute{Prefix=<S>, AttrName=<Signed_Zeros>,
  Text=[Yields the value True if the hardware representation for the
   type @i{T} has the capability of representing both positively and negatively
   signed zeros, these being generated and used by the predefined operations of
   the type @i{T} as specified in IEC 559:1989; yields the value False
   otherwise.
   The value of this attribute
   is of the predefined type Boolean.]}
@end{Description}

@Leading@Defn{normalized exponent}
For every value @RI{x} of a floating point type @i{T}, the
@i{normalized exponent} of @RI{x} is defined as follows:
@begin{Itemize}
   the normalized exponent of zero is (by convention) zero;

   for nonzero @RI{x}, the normalized exponent of @RI{x} is the unique
   integer @RI{k} such that
   @RI{T}@R['Machine_Radix]@+{@RI{k}@en@;1} @leq @Abs{@RI{x}} @Lt
   @RI{T}@R['Machine_Radix]@+{@RI{k}}.
@end{Itemize}
@begin{Ramification}
   The normalized exponent of a normalized number @RI{x} is the value
   of @i{exponent} in the canonical-form representation of @RI{x}.

   The normalized exponent of a denormalized number is less than the value of
   @i{T}'Machine_Emin.
@end{Ramification}

@begin{Wide}
@Leading@Defn{primitive function}
The following @i{primitive function attributes} are defined for any subtype
S of a floating point type @i{T}.
@end{Wide}
@begin(Description)
@AttributeLeading{Prefix=<S>, AttrName=<Exponent>,
  Text=[S'Exponent denotes a function with the following
   specification:
@begin(DescExample)
@key(function) S'Exponent (@RI(X) : @RI(T))
  @key(return) @RI(universal_integer)
@end(DescExample)

   @NoPrefix@;The function yields the normalized exponent of @i{X}.]}

@AttributeLeading{Prefix=<S>, AttrName=<Fraction>,
  Text=[S'Fraction denotes a function with the following
   specification:
@begin(DescExample)
@key(function) S'Fraction (@RI(X) : @RI(T))
  @key(return) @RI(T)
@end(DescExample)

   @NoPrefix@;The function yields the value @RI(X) @Times @RI(T)@R('Machine_Radix)@+(@en@RI(k)), where
   @RI(k) is the normalized exponent of @i(X). A zero result@Redundant(, which
   can only occur when @i(X) is zero,) has the sign of @i(X).]}
@begin{Discussion}
   Informally, when @i{X} is a normalized number, the result is the value
   obtained by replacing the @i{exponent} by zero in the canonical-form
   representation of @i{X}.
@end{Discussion}
@begin{Ramification}
   Except when @i{X} is zero, the magnitude of the result is
   greater than or equal to the reciprocal of @i{T}'Machine_Radix and less than
   one; consequently, the result is always a normalized number, even when @i{X}
   is a denormalized number.
@end{Ramification}
@begin{ImplNote}
   When @i{X} is a denormalized number, the result is the value obtained by
   replacing the @i{exponent} by zero in the canonical-form representation of
   the result of scaling @i{X} up sufficiently to normalize it.
@end{ImplNote}

@AttributeLeading{Prefix=<S>, AttrName=<Compose>,
  Text=[S'Compose denotes a function with the following
   specification:
@begin{DescExample}
@key(function) S'Compose (@RI{Fraction} : @RI{T};
                    @RI{Exponent} : @RI{universal_integer})
  @key(return) @RI{T}
@end{DescExample}

   @NoPrefix@;@Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
   Let @RI{v} be the value @RI{Fraction} @Times
   @RI{T}@R('Machine_Radix)@+{@RI{Exponent}@en@RI{k}},
   where @RI{k} is the normalized exponent of @i{Fraction}.
   If @RI{v} is a machine number of the type @i{T}, or if
   @Abs{@RI{v}} @geq @RI{T}@R('Model_Small), the function yields @RI{v};
   otherwise,
   it yields either one of the machine numbers of the type @i{T} adjacent to
   @RI{v}.
   @IndexCheck{Range_Check}Constraint_Error is optionally raised if
   @RI{v} is outside the base range of S.
   A zero result has the sign of @i{Fraction} when S'Signed_Zeros is True.]}
@begin{Discussion}
   Informally, when @i{Fraction} and @RI{v} are both normalized numbers, the
   result is the value obtained by replacing the @i{exponent} by @i{Exponent}
   in the canonical-form representation of @i{Fraction}.
@end{Discussion}
@begin{Ramification}
   If @i{Exponent} is less than @i{T}'Machine_Emin and
   @i{Fraction} is nonzero, the result is either zero, @i{T}'Model_Small, or
   (if @i{T}'Denorm is True) a denormalized number.
@end{Ramification}

@AttributeLeading{Prefix=<S>, AttrName=<Scaling>,
  Text=[S'Scaling denotes a function with the following
   specification:
@begin{DescExample}
@key(function) S'Scaling (@RI{X} : @RI{T};
                    @RI{Adjustment} : @RI{universal_integer})
  @key(return) @RI{T}
@end{DescExample}

   @NoPrefix@Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
   Let @RI{v} be the value @RI{X} @Times @RI{T}@R('Machine_Radix)@+{@RI{Adjustment}}.
   If @RI{v} is a machine number of the type @i{T}, or if
   @Abs{@RI{v}} @geq @RI{T}@R('Model_Small), the function yields @RI{v};
   otherwise,
   it yields either one of the machine numbers of the type @i{T} adjacent to
   @RI{v}.
@IndexCheck{Range_Check}Constraint_Error is optionally raised if
@RI{v} is outside the base range of S.
   A zero result has the sign of @i{X} when S'Signed_Zeros is True.]}
@begin{Discussion}
   Informally, when @i{X} and @RI{v} are both normalized numbers, the result
   is the value obtained by increasing the @i{exponent} by @i{Adjustment} in the
   canonical-form representation of @i{X}.
@end{Discussion}
@begin{Ramification}
   If @i{Adjustment} is sufficiently small (i.e., sufficiently negative), the
   result is
   either zero, @i{T}'Model_Small, or (if @i{T}'Denorm is True) a denormalized
   number.
@end{Ramification}

@AttributeLeading{Prefix=<S>, AttrName=<Floor>,
  Text=[S'Floor denotes a function with the following
   specification:
@begin{DescExample}
@key(function) S'Floor (@RI{X} : @RI{T})
  @key(return) @RI{T}
@end{DescExample}

   @NoPrefix@;The function yields the value @Floor{@RI{X}},
   i.e., the largest (most positive) integral value less than or equal to
   @i{X}.
   When @i{X} is zero, the
   result has the sign of @i{X}; a zero result otherwise has a positive
   sign.]}

@AttributeLeading{Prefix=<S>, AttrName=<Ceiling>,
  Text=[S'Ceiling denotes a function with the following
   specification:
@begin{DescExample}
@key(function) S'Ceiling (@RI{X} : @RI{T})
  @key(return) @RI{T}
@end{DescExample}

   @NoPrefix@;The function yields the value @Ceiling{@RI{X}},
   i.e., the smallest (most negative) integral value greater than or equal to
   @i{X}.
   When @i{X} is zero, the
   result has the sign of @i{X}; a zero result otherwise has a negative sign
   when S'Signed_Zeros is True.]}

@AttributeLeading{Prefix=<S>, AttrName=<Rounding>,
  Text=[S'Rounding denotes a function with the following
   specification:
@begin{DescExample}
@key(function) S'Rounding (@RI{X} : @RI{T})
  @key(return) @RI{T}
@end{DescExample}

   @NoPrefix@;The function yields the integral value nearest to @i{X},
   rounding away from zero if @i{X} lies exactly halfway between two integers.
   A zero result has the sign of @i{X} when S'Signed_Zeros is True.]}

@AttributeLeading{Prefix=<S>, AttrName=<Unbiased_Rounding>,
  Text=[S'Unbiased_Rounding denotes a function with
   the following specification:
@begin{DescExample}
@key(function) S'Unbiased_Rounding (@RI{X} : @RI{T})
  @key(return) @RI{T}
@end{DescExample}

   @NoPrefix@;The function yields the integral value nearest to @i{X},
   rounding toward the even integer if @i{X} lies exactly halfway between
   two integers. A zero result has the sign of @i{X} when S'Signed_Zeros is
   True.]}

@ChgAttribute{Version=[2],Kind=[Added],ChginAnnex=[T],
  Leading=<T>, Prefix=<S>, AttrName=<Machine_Rounding>, ARef=[AI95-00267-01],
  Text=[@Chg{Version=[2],New=[S'Machine_Rounding denotes a function
     with the following specification:],Old=[]}
@begin(Descexample)
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@key(function) S'Machine_Rounding (@RI{X} : @RI{T})
  @key(return) @RI{T}]}
@end(Descexample)

   @ChgRef{Version=[2],Kind=[Added]}
   @ChgAdded{Version=[2],NoPrefix=[T],Text=[The function yields the
   integral value nearest to @i{X}. If @i{X} lies exactly halfway between two
   integers, one of those integers is returned, but which of them is returned
   is unspecified. A zero result has the sign of @i{X} when S'Signed_Zeros is
   True. This function provides access to the rounding behavior which is most
   efficient on the target processor.@PDefn{unspecified}]}]}

   @begin{Discussion}
     @ChgRef{Version=[2],Kind=[Added]}
     @ChgAdded{Version=[2],Text=[We leave the rounding unspecified, so that
     users cannot depend on a particular rounding. This attribute is intended
     for use in cases where the particular rounding chosen is irrelevant. If
     there is a need to know which way values halfway between two integers are
     rounded, one of the other rounding attributes should be used.]}
   @end{Discussion}

@AttributeLeading{Prefix=<S>, AttrName=<Truncation>,
  Text=[S'Truncation denotes a function with the following
   specification:
@begin{DescExample}
@key(function) S'Truncation (@RI{X} : @RI{T})
  @key(return) @RI{T}
@end{DescExample}

   @NoPrefix@;The function yields the value @Ceiling{@RI{X}} when @i{X} is negative,
   and @Floor{@RI{X}} otherwise. A zero result has the sign of @i{X} when
   S'Signed_Zeros is True.]}

@AttributeLeading{Prefix=<S>, AttrName=<Remainder>,
  Text=[S'Remainder denotes a function with the following
   specification:
@begin{DescExample}
@key(function) S'Remainder (@RI{X}, @RI{Y} : @RI{T})
  @key(return) @RI{T}
@end{DescExample}

   @NoPrefix@Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
   For nonzero @i{Y}, let @RI{v} be the value
   @RI{X} @en @RI{n} @Times @RI{Y}, where @RI{n} is the integer nearest to
   the exact value of @RI{X}/@RI{Y}; if @Abs{@RI{n} @en @RI{X}/@RI{Y}} = 1/2,
   then @RI(n) is chosen to be even. If @RI{v} is a machine number of
   the type @i{T}, the function yields @RI{v}; otherwise, it yields zero.
@IndexCheck{Division_Check}Constraint_Error is raised if @i{Y} is zero.
   A zero result has the sign of @i{X} when S'Signed_Zeros is True.]}
@begin{Ramification}
   The magnitude of the result is less than or equal to one-half the magnitude
   of @i{Y}.
@end{Ramification}
@begin{Discussion}
   Given machine numbers @i{X} and @i{Y} of the type @i{T}, @RI{v} is
   necessarily a machine number of the type @i{T}, except when @i{Y} is in the
   neighborhood of zero, @i{X} is sufficiently close to a multiple of @i{Y},
   and @i{T}'Denorm is False.
@end{Discussion}

@AttributeLeading{Prefix=<S>, AttrName=<Adjacent>,
  Text=[S'Adjacent denotes a function with the following
   specification:
@begin{DescExample}
@key(function) S'Adjacent (@RI{X}, @RI{Towards} : @RI{T})
  @key(return) @RI{T}
@end{DescExample}

   @NoPrefix@;@Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
   If @RI{Towards} = @RI{X}, the function yields @i{X}; otherwise, it
   yields the machine number of the type @i{T} adjacent to @i{X} in the
   direction of @i{Towards}, if that machine number exists.
   @IndexCheck{Range_Check}If the result would
   be outside the base range of S, Constraint_Error is raised.
   When @i{T}'Signed_Zeros is True, a zero result has the sign of @i{X}.
   When @i{Towards} is zero, its sign has no bearing on the result.]}
@begin{Ramification}
   The value of S'Adjacent(0.0, 1.0) is the smallest normalized positive number
   of the type @i{T} when @i{T}'Denorm is False and the smallest denormalized
   positive number of the type @i{T} when @i{T}'Denorm is True.
@end{Ramification}

@AttributeLeading{Prefix=<S>, AttrName=<Copy_Sign>,
  Text=[S'Copy_Sign denotes a function with the following specification:
@begin{DescExample}
@key(function) S'Copy_Sign (@RI{Value}, @RI{Sign} : @RI{T})
  @key(return) @RI{T}
@end{DescExample}

   @NoPrefix@Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
   If the value of @i{Value} is nonzero, the function yields a result whose
   magnitude is that of @i{Value} and whose sign is that of @i{Sign};
   otherwise, it yields the value zero.
@IndexCheck{Range_Check}Constraint_Error is optionally raised if the result
is outside the base range of S.
   A zero result has the sign of @i{Sign} when S'Signed_Zeros is True.]}
@begin{Discussion}
   S'Copy_Sign is provided for convenience in restoring the sign to a quantity
   from which it has been temporarily removed, or to a related quantity. When
   S'Signed_Zeros is True, it is also instrumental in determining the sign
   of a zero quantity, when required. (Because negative and positive zeros
   compare equal in systems conforming to IEC 559:1989, a negative zero does
   @i{not} appear to be negative when compared to zero.) The sign
   determination is accomplished by transferring the sign of the zero quantity
   to a nonzero quantity and then testing for a negative result.
@end{Discussion}

@AttributeLeading{Prefix=<S>, AttrName=<Leading_Part>,
  Text=[S'Leading_Part denotes a function with the following specification:
@begin{DescExample}
@key(function) S'Leading_Part (@RI{X} : @RI{T};
                         @RI{Radix_Digits} : @RI{universal_integer})
  @key(return) @RI{T}
@end{DescExample}

   @NoPrefix@;Let @RI{v} be the value @RI{T}@R('Machine_Radix)@+{@RI{k}@en@RI{Radix_Digits}},
   where @RI{k} is the normalized exponent of @i{X}. The function yields
   the value
   @begin{Itemize}
      @Floor{@RI{X}/@RI{v}} @Times @RI{v},
      when @i{X} is nonnegative and @i{Radix_Digits} is positive;

      @Ceiling{@RI{X}/@RI{v}} @Times @RI{v},
      when @i{X} is negative and @i{Radix_Digits} is positive.
   @end{Itemize}

   @NoPrefix@Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
   @IndexCheck{Range_Check}Constraint_Error is raised when @i{Radix_Digits}
   is zero or negative.
   A zero result@Redundant{, which can only occur when @i{X} is zero,} has the
   sign of @i{X}.]}
@begin{Discussion}
   Informally, if @i{X} is nonzero, the result is the value
   obtained by retaining only the specified number of (leading) significant
   digits of @i{X} (in the machine radix), setting all other digits to zero.
@end{Discussion}
@begin{ImplNote}
   The result can be obtained by first scaling @i{X} up, if necessary to
   normalize it, then masking the mantissa so as to retain only the specified
   number of leading digits, then scaling the result back down if @i{X} was
   scaled up.
@end{ImplNote}

@AttributeLeading{Prefix=<S>, AttrName=<Machine>,
  Text=[S'Machine denotes a function with the following specification:
@begin{DescExample}
@key(function) S'Machine (@RI{X} : @RI{T})
  @key(return) @RI{T}
@end{DescExample}

   @NoPrefix@Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
   If @i{X} is a machine number of the type @i{T}, the function yields @i{X};
   otherwise, it yields the value obtained by rounding or truncating @i{X} to
   either one of the adjacent machine numbers of the type @i(T).
@IndexCheck{Range_Check}Constraint_Error is raised if rounding or
truncating @i{X} to the precision
   of the machine numbers results in a value outside the base range of S.
   A zero result has the sign of @i{X} when S'Signed_Zeros is True.]}
@begin{Discussion}
   All of the primitive function attributes except Rounding and Machine
   correspond to subprograms in the Generic_Primitive_Functions
   generic package proposed as a separate ISO standard (ISO/IEC DIS 11729) for
   Ada 83. The Scaling, Unbiased_Rounding, and Truncation attributes
   correspond to the Scale, Round, and Truncate functions, respectively, in
   Generic_Primitive_Functions. The Rounding attribute rounds away from zero;
   this functionality was not provided in Generic_Primitive_Functions. The
   name Round was not available for either of the primitive function attributes
   that perform rounding, since an attribute of that name is used for a
   different purpose for decimal fixed point types. Likewise, the name Scale
   was not available, since an attribute of that name is also used for a
   different purpose for decimal fixed point types. The functionality of the
   Machine attribute was also not provided in Generic_Primitive_Functions. The
   functionality of the Decompose procedure of Generic_Primitive_Functions is
   only provided in the form of the separate attributes Exponent and Fraction.
   The functionality of the Successor and Predecessor functions of
   Generic_Primitive_Functions is provided by the extension of the existing
   Succ and Pred attributes.
@end{Discussion}
@begin{ImplNote}
   The primitive function attributes may be implemented either with appropriate
   floating point arithmetic operations or with integer and logical operations
   that act on parts of the representation directly. The latter is strongly
   encouraged when it is more efficient than the former; it is mandatory when
   the former cannot deliver the required accuracy due to limitations of the
   implementation's arithmetic operations.
@end{ImplNote}
@end(Description)

@begin{Wide}
@Leading@Defn2{Term=[model-oriented attributes],
                Sec=[of a floating point subtype]}
The following @i{model-oriented attributes} are defined for any subtype S of
a floating point type @i{T}.
@end{Wide}
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<Model_Mantissa>,
  Text=[If the Numerics Annex is not supported,
   this attribute yields an implementation defined value that is
   greater than or equal to
   @Ceiling{@RI{d} @Times @Log(10) / @Log(@RI{T}'@R{Machine_Radix})} + 1, where
   @RI{d} is the requested decimal precision of @i{T},
   and less than or equal to the value of
   @i{T}'Machine_Mantissa.
   See @RefSecNum{Model-Oriented Attributes of Floating Point Types}
   for further requirements
   that apply to implementations supporting the Numerics
   Annex.
   The value of this attribute is of the type
   @i{universal_integer}.]}

@Attribute{Prefix=<S>, AttrName=<Model_Emin>,
  Text=[If the Numerics Annex is not supported,
  this attribute yields an implementation defined value
  that is greater than or equal to the value
  of @i{T}'Machine_Emin.
  See @RefSecNum{Model-Oriented Attributes of Floating Point Types}
  for further requirements
  that apply to implementations supporting the Numerics
  Annex.
  The value of this attribute is of the type
  @i{universal_integer}.]}

@Attribute{Prefix=<S>, AttrName=<Model_Epsilon>,
  Text=[Yields the value
   @RI{T}@R('Machine_Radix)@+{1 @en @RI{T}@R('Model_Mantissa)}. The value of this
   attribute is of the type @i{universal_real}.]}
@begin{Discussion}
   In most implementations, this attribute yields the absolute value of the
   difference between one and the smallest machine number of the type @i{T}
   above one which, when added to one, yields a machine number different from
   one. Further discussion can be found in
   @RefSecNum{Model-Oriented Attributes of Floating Point Types}.
@end{Discussion}

@Attribute{Prefix=<S>, AttrName=<Model_Small>,
  Text=[Yields the value
   @RI{T}@R('Machine_Radix)@+{@RI{T}@R('Model_Emin) @en 1}. The value of this
   attribute is of the type @i{universal_real}.]}
@begin{Discussion}
   In most implementations, this attribute yields the
   smallest positive normalized number of the type @i{T},
   i.e. the number corresponding to the positive underflow
   threshold. In some implementations employing a radix-complement
   representation for the type @i{T}, the positive underflow threshold is
   closer to zero than is the negative underflow threshold, with the
   consequence that the smallest positive normalized number does not coincide with
   the positive underflow threshold (i.e., it exceeds the latter). Further
   discussion can be found in
   @RefSecNum{Model-Oriented Attributes of Floating Point Types}.
@end{Discussion}

@AttributeLeading{Prefix=<S>, AttrName=<Model>,
  Text=[S'Model denotes a function with the following specification:
@begin{DescExample}
@key(function) S'Model (@RI{X} : @RI{T})
  @key(return) @RI{T}
@end{DescExample}

   @NoPrefix@;If the Numerics Annex is not supported,
   the meaning of this attribute is implementation
   defined;
   see @RefSecNum{Model-Oriented Attributes of Floating Point Types}
   for the definition that applies to implementations supporting
   the Numerics Annex.]}

@Attribute{Prefix=<S>, AttrName=<Safe_First>,
  Text=[Yields the lower bound of the safe range
  (see @RefSecNum{Floating Point Types}) of the type @i{T}.
  If the Numerics Annex is not supported, the value
  of this attribute is implementation defined;
  see @RefSecNum{Model-Oriented Attributes of Floating Point Types}
  for the definition that applies to
  implementations supporting the Numerics Annex. The value of this attribute
  is of the type @i{universal_real}.]}

@Attribute{Prefix=<S>, AttrName=<Safe_Last>,
  Text=[Yields the upper bound of the safe range
  (see @RefSecNum{Floating Point Types}) of the type @i{T}.
  If the Numerics Annex is not supported, the value
  of this attribute is implementation defined;
  see @RefSecNum{Model-Oriented Attributes of Floating Point Types}
  for the definition that applies to
  implementations supporting the Numerics Annex. The value of this attribute
  is of the type @i{universal_real}.]}
@begin{Discussion}
   A predefined floating point arithmetic operation that yields a value in the
   safe range of its result type is guaranteed not to overflow.
@end{Discussion}
@begin{Honest}

An exception is made for exponentiation by a negative exponent in
@RefSecNum{Highest Precedence Operators}.

@end{Honest}
@ImplDef{The values of the Model_Mantissa, Model_Emin, Model_Epsilon,
Model, Safe_First, and Safe_Last attributes, if the Numerics
Annex is not supported.}
@end{Description}
@EndPrefixType{}
@end{StaticSem}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
The Epsilon and Mantissa attributes of floating point types are removed
from the language and replaced by Model_Epsilon and
Model_Mantissa, which may have different values (as a result of changes in the
definition of model numbers); the replacement of one set of attributes by
another is intended to convert what would be an inconsistent change into an
incompatible change.

The Emax, Small, Large, Safe_Emax, Safe_Small, and Safe_Large attributes of
floating point types are removed from the language. Small and Safe_Small are
collectively replaced by Model_Small, which is functionally equivalent to
Safe_Small, though it may have a slightly different value. The others are
collectively replaced by Safe_First and Safe_Last. Safe_Last is functionally
equivalent to Safe_Large, though it may have a different value; Safe_First is
comparable to the negation of Safe_Large but may differ slightly from it as
well as from the negation of Safe_Last. Emax and Safe_Emax had relatively few
uses in Ada 83; T'Safe_Emax can be computed in the revised language as
Integer'Min(T'Exponent(T'Safe_First), T'Exponent(T'Safe_Last)).

Implementations are encouraged to eliminate the incompatibilities discussed
here by retaining the old attributes, during a transition period,
in the form of implementation-defined attributes with their former values.
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
The Model_Emin attribute is new. It is conceptually similar to the negation of
Safe_Emax attribute of Ada 83, adjusted for the fact that the model numbers now
have the hardware radix. It is a fundamental determinant, along with
Model_Mantissa, of the set of model numbers of a type
(see @RefSecNum{Model of Floating Point Arithmetic}).

The Denorm and Signed_Zeros attributes are new, as are all of the
primitive function attributes.
@end{Extend83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00388-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The Machine_Rounding attribute is new.]}
@end{Extend95}


@LabeledSubClause{Attributes of Fixed Point Types}

@begin{StaticSem}
@Leading@Defn2{Term=[representation-oriented attributes],
                Sec=[of a fixed point subtype]}
The following @i{representation-oriented} attributes are defined for
@PrefixType{every subtype S of a fixed point type @i{T}}.
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<Machine_Radix>,
  Text=[Yields the radix of the hardware representation of the type
   @i{T}. The value of this attribute is of the type @i{universal_integer}.]}

@Attribute{Prefix=<S>, AttrName=<Machine_Rounds>,
  Text=[Yields the value True if rounding is performed on inexact
   results of every predefined operation that yields a result of the
   type @i{T}; yields the value False otherwise.
   The value
   of this attribute is of the predefined type Boolean.]}

@Attribute{Prefix=<S>, AttrName=<Machine_Overflows>,
  Text=[Yields the value True if overflow and divide-by-zero are detected and
   reported by raising Constraint_Error for every predefined operation that
   yields a result of the type @i{T};
   yields the value False otherwise.
   The value of
   this attribute is of the predefined type Boolean.]}
@end{Description}
@EndPrefixType{}
@end{StaticSem}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
The Mantissa, Large, Safe_Small, and Safe_Large attributes of fixed
point types are removed from the language.

Implementations are encouraged to eliminate the resulting incompatibility by
retaining these attributes, during a transition period,
in the form of implementation-defined attributes with their former values.
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
The Machine_Radix attribute
is now allowed for fixed point types. It is
also specifiable in an attribute definition clause
(see @RefSecNum{Machine_Radix Attribute Definition Clause}).
@end{Extend83}

