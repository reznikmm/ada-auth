@comment{ $Source: e:\\cvsroot/ARM/Source/infosys.mss,v $ }
@comment{ $Revision: 1.19 $ $Date: 2000/08/12 00:40:17 $ $Author: Randy $ }
@Part(infosys, Root="ada.mss")

@Comment{$Date: 2000/08/12 00:40:17 $}
@LabeledNormativeAnnex{Information Systems}

@begin{Intro}
@Defn{information systems}
@Leading@;This Annex provides a set of facilities relevant to
Information Systems programming. These fall into several
categories:
@begin{itemize}
an attribute definition clause specifying Machine_Radix for a decimal subtype;

the package Decimal, which declares a set of constants defining the
implementation's capacity for decimal types, and  a
generic procedure
for decimal division; and

the child packages Text_IO.Editing and Wide_Text_IO.Editing,
which
support formatted and localized output of decimal data, based on
@lquotes@;picture String@rquotes@; values.
@end{itemize}

See also: @RefSec{Fixed Point Types}; @RefSec{Operations of Fixed Point Types};
@RefSec{Type Conversions};
@RefSec{Representation Attributes};
@RefSec(Input-Output for Real Types);
@RefSec{Interfacing with COBOL};
@RefSec{Interfacing with C};
@RefSec{Numerics}.

The character and string handling packages in
@RefSec{Predefined Language Environment}
 are also relevant for Information Systems.
@end{Intro}

@begin{ImplAdvice}
If COBOL (respectively, C) is widely supported in the target environment,
implementations supporting the Information Systems
 Annex should provide the child package
Interfaces.COBOL (respectively, Interfaces.C) specified in
@RefSecNum{Interface to Other Languages}
and should support a @i{convention_}@nt{identifier} of
COBOL (respectively, C) in the interfacing pragmas
(see @RefSecNum{Interface to Other Languages}),
thus allowing Ada programs to interface with programs written in
that language.
@end{ImplAdvice}

@begin{Extend83}
This Annex is new to Ada 95.
@end{Extend83}


@LabeledClause{Machine_Radix Attribute Definition Clause}

@begin{StaticSem}
@PDefn2{Term=[specifiable], Sec=(of Machine_Radix for decimal first subtypes)}
@Defn{Machine_Radix clause}
Machine_Radix may be specified for a decimal first subtype
(see @RefSecNum{Fixed Point Types})
via an @nt{attribute_definition_clause};
the expression of such a clause shall be static,
and its value shall be 2 or 10.
A value of 2 implies a binary base range; a
value of 10 implies a decimal base range.
@begin{Ramification}
In the absence of a Machine_Radix clause, the choice
of 2 versus 10 for S'Machine_Radix is not specified.
@end{Ramification}
@end{StaticSem}

@begin{ImplAdvice}
Packed decimal should be used as the internal representation
for objects of subtype S when S'Machine_Radix = 10.
@end{ImplAdvice}

@begin{Discussion}

The intent of a decimal Machine_Radix attribute definition clause
is to allow the programmer to declare an Ada decimal data object
whose representation matches a particular COBOL implementation's
representation of packed decimal items.
The Ada object may then be passed to an interfaced COBOL program
that takes a packed decimal data item as a parameter,
assuming that convention COBOL has been specified for the Ada
object's type in a @nt[pragma] Convention.

Additionally, the Ada compiler may choose to generate arithmetic
instructions that exploit the packed decimal representation.

@end{discussion}

@begin{Examples}
@Leading@Keepnext@i{Example of Machine_Radix attribute definition clause:}
@begin{example}
@key[type] Money @key[is] @key[delta] 0.01 @key[digits] 15;
@key[for] Money'Machine_Radix @key[use] 10;
@end{example}
@end{Examples}

@LabeledClause{The Package Decimal}

@begin{StaticSem}
@Leading@Keepnext@;The library package Decimal has the following declaration:
@begin{Example}
@ChildUnit{Parent=[Ada],Child=[Decimal]}
@key(package) Ada.Decimal @key(is)
   @key(pragma) Pure(Decimal);

   @AdaDefn{Max_Scale} : @key(constant) := @RI{implementation-defined};
   @AdaDefn{Min_Scale} : @key(constant) := @RI{implementation-defined};

   @AdaDefn{Min_Delta} : @key(constant) := 10.0**(-Max_Scale);
   @AdaDefn{Max_Delta} : @key(constant) := 10.0**(-Min_Scale);

   @AdaDefn{Max_Decimal_Digits} : @key(constant) := @RI{implementation-defined};

   @key(generic)
      @key(type) Dividend_Type  @key(is) @key(delta) <> @key(digits) <>;
      @key(type) Divisor_Type   @key(is) @key(delta) <> @key(digits) <>;
      @key(type) Quotient_Type  @key(is) @key(delta) <> @key(digits) <>;
      @key(type) Remainder_Type @key(is) @key(delta) <> @key(digits) <>;
   @key(procedure) @AdaSubDefn{Divide} (Dividend  : @key(in) Dividend_Type;
                     Divisor   : @key(in) Divisor_Type;
                     Quotient  : @key(out) Quotient_Type;
                     Remainder : @key(out) Remainder_Type);
   @key[pragma] Convention(Intrinsic, Divide);

@key(end) Ada.Decimal;
@end{Example}
@ImplDef{The values of named numbers in the package Decimal.}

Max_Scale is the largest N such that 10.0**(-N) is allowed as a decimal
  type's delta. Its type is @i{universal_integer}.

Min_Scale is the smallest N such that 10.0**(-N) is allowed as a decimal
  type's delta. Its type is @i{universal_integer}.

Min_Delta is the smallest value allowed for @i{delta} in a
@nt{decimal_fixed_point_definition}. Its type is @i{universal_real}.

Max_Delta is the largest value allowed for @i{delta} in a
@nt{decimal_fixed_point_definition}. Its type is @i{universal_real}.

Max_Decimal_Digits is the largest value allowed for @i{digits} in a
@nt{decimal_fixed_point_definition}. Its type is @i{universal_integer}.
@begin{reason}
The name is Max_Decimal_Digits versus Max_Digits, in order to avoid
confusion with the named number System.Max_Digits relevant to floating
point.
@end{reason}
@end{StaticSem}

@begin{StaticSem}
The effect of Divide is as follows.
The value of Quotient is Quotient_Type(Dividend/Divisor).
The value of Remainder is Remainder_Type(Intermediate),
where Intermediate is the difference between Dividend and the product of
Divisor and Quotient;  this result is computed exactly.
@end{StaticSem}

@begin(ImplReq)
Decimal.Max_Decimal_Digits shall be at least 18.

Decimal.Max_Scale shall be at least 18.

Decimal.Min_Scale shall be at most 0.
@end{ImplReq}

@begin(Notes)
The effect of division yielding a quotient with control over rounding
versus truncation is obtained by applying either the
function attribute Quotient_Type'Round or the conversion Quotient_Type
to the expression Dividend/Divisor.
@end(Notes)

@LabeledClause{Edited Output for Decimal Types}
@begin{Intro}
@Leading@;The child packages Text_IO.Editing and Wide_Text_IO.Editing
provide localizable formatted text
output, known as @i{edited output} @Defn{edited output},
for decimal types. An edited
output string is a function of a numeric value, program-specifiable
locale elements, and a format control value. The numeric value is of
some decimal type. The locale elements are:
@begin{itemize}
    the currency string;

    the digits group separator character;

    the radix mark character; and

    the fill character that replaces leading zeros of the numeric value.
@end{itemize}

For Text_IO.Editing the edited output and currency strings are of
type String, and the locale characters are of type Character.
For Wide_Text_IO.Editing their types are Wide_String and Wide_Character,
respectively.

Each of the locale elements has a default value that can be replaced or
explicitly overridden.

A format-control
value is of the private type Picture; it determines
the composition of the edited output string and controls the form and
placement of the sign, the position of the locale elements and the
decimal digits, the presence or absence of a radix mark, suppression
of leading zeros, and insertion of particular character values.

@Leading@;A Picture object is composed from a String value, known as a
@i{picture String}, that serves as a template for the
edited output string, and a
Boolean value that controls whether a string of all space characters is
produced when the number's value is zero. A picture String comprises a
sequence of one- or two-Character symbols, each serving as a placeholder
for a character or string at a corresponding position in the edited
output string. The picture String symbols fall into several categories
based on their effect on the edited output string:
@TabClear()
@begin{Display}
@TabSet{3, 21, 27, 33, 39, 45, 51}
@\Decimal Digit: @\'9'
@\Radix Control: @\'.' @\'V'
@\Sign Control: @\'+' @\'-' @\'<' @\'>' @\"CR" @\"DB"
@\Currency Control: @\'$' @\'#'
@\Zero Suppression: @\'Z' @\'*'
@\Simple Insertion: @\'_' @\'B' @\'0' @\'/'
@end{Display}

The entries are not case-sensitive. Mixed- or lower-case forms
for "CR" and "DB", and lower-case forms for 'V', 'Z', and 'B',
have the same effect as the upper-case symbols shown.

An occurrence of a '9' Character in the picture String represents a
decimal digit position in the edited output string.

A radix control Character in the picture String indicates the position
of the radix mark in the edited output string: an actual character
position for '.', or an assumed position for 'V'.

A sign control Character in the picture String affects the form of the
sign in the edited output string. The '<' and '>' Character values indicate
parentheses for negative values. A Character '+', '-', or '<' appears
either singly, signifying a fixed-position sign in the edited output,
or repeated, signifying a floating-position sign that is preceded by
zero or more space characters and that replaces a leading 0.

A currency control Character in the picture String indicates an
occurrence of the currency string in the edited output string. The
'$' Character represents the complete currency string; the '#'
Character represents one character of the currency string. A '$'
Character appears either singly, indicating a fixed-position currency
string in the edited output, or repeated, indicating a
floating-position currency string that occurs in place of a leading 0.
A sequence of '#' Character values indicates either a fixed- or
floating-position currency string, depending on context.

A zero suppression Character in the picture String allows a leading
zero to be replaced by either the space character (for 'Z') or
the fill character (for '*').

A simple insertion Character in the picture String represents, in
general, either itself (if '/' or '0'), the space character (if 'B'),
or the digits group separator character (if '_'). In some contexts it
is treated as part of a floating sign, floating currency, or zero
suppression string.

An example of a picture String is "<###Z_ZZ9.99>". If the currency string
is "FF", the separator character is ',', and the radix mark is '.' then
the edited output string values for the decimal values 32.10 and @en@;5432.10 are
"bbFFbbb32.10b" and "(bFF5,432.10)", respectively, where 'b' indicates
the space character.

The generic packages Text_IO.Decimal_IO and Wide_Text_IO.Decimal_IO
(see @RefSec(Input-Output for Real Types))
provide text input and non-edited text output for decimal types.
@end{Intro}

@begin{Notes}
A picture String is of type Standard.String, both for Text_IO.Editing
and Wide_Text_IO.Editing.
@end{Notes}

@LabeledSubClause{Picture String Formation}
@begin{Intro}
@Defn2{term=[picture String],sec=[for edited output]}
@Defn2{term=[well-formed picture String],sec=[for edited output]}
A @i{well-formed picture String}, or simply @i{picture String},
is a String value that conforms to the
syntactic rules, composition constraints, and character replication
conventions specified in this clause.
@end{Intro}

@begin{RunTime}
@TabClear()
@begin{Display}
@nt{picture_string} ::=
   @nt{fixed_$_picture_string}
 | @nt{fixed_#_picture_string}
 | @nt{floating_currency_picture_string}
 | @nt{non_currency_picture_string}

@end{Display}
@begin{display}
@nt{fixed_$_picture_string} ::=
   [@nt{fixed_LHS_sign}] @nt{fixed_$_char} {@nt{direct_insertion}} [@nt{zero_suppression}]
     @nt{number} [@nt{RHS_sign}]

 | [@nt{fixed_LHS_sign} {@nt{direct_insertion}}] [@nt{zero_suppression}]
     @nt{number} @nt{fixed_$_char} {@nt{direct_insertion}} [@nt{RHS_sign}]

 | @nt{floating_LHS_sign} @nt{number} @nt{fixed_$_char} {@nt{direct_insertion}} [@nt{RHS_sign}]

 | [@nt{fixed_LHS_sign}] @nt{fixed_$_char} {@nt{direct_insertion}}
     @nt{all_zero_suppression_number} {@nt{direct_insertion}}  [@nt{RHS_sign}]

 | [@nt{fixed_LHS_sign} {@nt{direct_insertion}}] @nt{all_zero_suppression_number} {@nt{direct_insertion}}
     @nt{fixed_$_char} {@nt{direct_insertion}} [@nt{RHS_sign}]

 | @nt{all_sign_number} {@nt{direct_insertion}} @nt{fixed_$_char} {@nt{direct_insertion}} [@nt{RHS_sign}]

@end{display}
@begin{display}
@nt{fixed_#_picture_string} ::=
   [@nt{fixed_LHS_sign}] @nt{single_#_currency} {@nt{direct_insertion}}
     [@nt{zero_suppression}] @nt{number} [@nt{RHS_sign}]

 | [@nt{fixed_LHS_sign}] @nt{multiple_#_currency} {@nt{direct_insertion}}
     @nt{zero_suppression} @nt{number} [@nt{RHS_sign}]

 | [@nt{fixed_LHS_sign} {@nt{direct_insertion}}] [@nt{zero_suppression}]
     @nt{number} @nt{fixed_#_currency} {@nt{direct_insertion}} [@nt{RHS_sign}]

 | @nt{floating_LHS_sign} @nt{number} @nt{fixed_#_currency} {@nt{direct_insertion}} [@nt{RHS_sign}]

 | [@nt{fixed_LHS_sign}] @nt{single_#_currency} {@nt{direct_insertion}}
     @nt{all_zero_suppression_number} {@nt{direct_insertion}} [@nt{RHS_sign}]

 | [@nt{fixed_LHS_sign}] @nt{multiple_#_currency} {@nt{direct_insertion}}
     @nt{all_zero_suppression_number} {@nt{direct_insertion}} [@nt{RHS_sign}]

 | [@nt{fixed_LHS_sign} {@nt{direct_insertion}}] @nt{all_zero_suppression_number} {@nt{direct_insertion}}
     @nt{fixed_#_currency} {@nt{direct_insertion}} [@nt{RHS_sign}]

 | @nt{all_sign_number} {@nt{direct_insertion}} @nt{fixed_#_currency} {@nt{direct_insertion}} [@nt{RHS_sign}]

@end{display}
@begin{display}
@nt{floating_currency_picture_string} ::=
   [@nt{fixed_LHS_sign}] {@nt{direct_insertion}} @nt{floating_$_currency} @nt{number} [@nt{RHS_sign}]
 | [@nt{fixed_LHS_sign}] {@nt{direct_insertion}} @nt{floating_#_currency} @nt{number} [@nt{RHS_sign}]
 | [@nt{fixed_LHS_sign}] {@nt{direct_insertion}} @nt{all_currency_number} {@nt{direct_insertion}} [@nt{RHS_sign}]

@end{display}
@begin{display}
@nt{non_currency_picture_string} ::=
   [@nt{fixed_LHS_sign} {@nt{direct_insertion}}] @nt{zero_suppression} @nt{number} [@nt{RHS_sign}]
 | [@nt{floating_LHS_sign}] @nt{number} [@nt{RHS_sign}]
 | [@nt{fixed_LHS_sign} {@nt{direct_insertion}}] @nt{all_zero_suppression_number} {@nt{direct_insertion}} [@nt{RHS_sign}]
 | @nt{all_sign_number} {@nt{direct_insertion}}
 | @nt{fixed_LHS_sign} @nt{direct_insertion} {@nt{direct_insertion}} @nt{number} [@nt{RHS_sign}]

@end{display}
@begin{display}
@nt{fixed_LHS_sign} ::=  @nt{LHS_Sign}
@end{display}
@begin{display}
@nt{LHS_Sign} ::=  + | @en | <

@end{display}
@begin{display}
@nt{fixed_$_char} ::= $

@end{display}
@begin{display}
@nt{direct_insertion} ::=  @nt{simple_insertion}
@end{display}
@begin{display}
@nt{simple_insertion} ::=  _ | B | 0 | /

@end{display}
@begin{display}
@nt{zero_suppression} ::=  Z {Z | @nt{context_sensitive_insertion}} | @nt{fill_string}
@end{display}
@begin{display}
@nt{context_sensitive_insertion} ::=  @nt{simple_insertion}

@end{display}
@begin{display}
@nt{fill_string} ::=  * {* | @nt{context_sensitive_insertion}}

@end{display}
@begin{display}
@nt{number} ::=
   @nt{fore_digits} [@nt{radix} [@nt{aft_digits}] {@nt{direct_insertion}}]
 | @nt{radix} @nt{aft_digits} {@nt{direct_insertion}}
@end{display}
@begin{display}
@nt{fore_digits} ::= 9 {9 | @nt{direct_insertion}}
@end{display}
@begin{display}
@nt{aft_digits} ::=  {9 | @nt{direct_insertion}} 9
@end{display}
@begin{display}
@nt{radix} ::= . | V

@end{display}
@begin{display}
@nt{RHS_sign} ::= + | @en | > | CR | DB

@end{display}
@begin{display}
@nt{floating_LHS_sign} ::=
   @nt{LHS_Sign} {@nt{context_sensitive_insertion}} @nt{LHS_Sign} {@nt{LHS_Sign} | @nt{context_sensitive_insertion}}

@end{display}
@begin{display}
@nt{single_#_currency} ::= #
@end{display}
@begin{display}
@nt{multiple_#_currency} ::= ## {#}

@end{display}
@begin{display}
@nt{fixed_#_currency} ::= @nt{single_#_currency} | @nt{multiple_#_currency}

@end{display}
@begin{display}
@nt{floating_$_currency} ::=
   $ {@nt{context_sensitive_insertion}} $ {$ | @nt{context_sensitive_insertion}}

@end{display}
@begin{display}
@nt{floating_#_currency} ::=
   # {@nt{context_sensitive_insertion}} # {# | @nt{context_sensitive_insertion}}

@end{display}
@begin{display}
@nt{all_sign_number} ::=  @nt{all_sign_fore} [@nt{radix} [@nt{all_sign_aft}]] [>]
@end{display}
@begin{display}
@nt{all_sign_fore} ::=
   @nt{sign_char} {@nt{context_sensitive_insertion}} @nt{sign_char} {@nt{sign_char} | @nt{context_sensitive_insertion}}
@end{display}
@begin{display}
@nt{all_sign_aft} ::= {@nt{all_sign_aft_char}} @nt{sign_char}

@nt{all_sign_aft_char} ::=  @nt{sign_char} | @nt{context_sensitive_insertion}
@end{display}
@begin{display}
@nt{sign_char} ::= + | - | <

@end{display}
@begin{display}
@nt{all_currency_number} ::=  @nt{all_currency_fore} [@nt{radix} [@nt{all_currency_aft}]]
@end{display}
@begin{display}
@nt{all_currency_fore} ::=
   @nt{currency_char} {@nt{context_sensitive_insertion}}
     @nt{currency_char} {@nt{currency_char} | @nt{context_sensitive_insertion}}
@end{display}
@begin{display}
@nt{all_currency_aft} ::= {@nt{all_currency_aft_char}} @nt{currency_char}

@nt{all_currency_aft_char} ::= @nt{currency_char} | @nt{context_sensitive_insertion}
@end{display}
@begin{display}
@nt{currency_char} ::= $ | #

@end{display}
@begin{display}
@nt{all_zero_suppression_number} ::=  @nt{all_zero_suppression_fore} [ @nt{radix} [@nt{all_zero_suppression_aft}]]
@end{display}
@begin{display}
@nt{all_zero_suppression_fore} ::=
   @nt{zero_suppression_char} {@nt{zero_suppression_char} | @nt{context_sensitive_insertion}}
@end{display}
@begin{display}
@nt{all_zero_suppression_aft} ::= {@nt{all_zero_suppression_aft_char}} @nt{zero_suppression_char}

@nt{all_zero_suppression_aft_char} ::=  @nt{zero_suppression_char} | @nt{context_sensitive_insertion}
@end{display}
@begin{display}
@nt{zero_suppression_char} ::= Z | *
@end{Display}

@Leading@;The following composition constraints apply to a picture String:
@begin{Itemize}
A @nt{floating_LHS_sign} does not have occurrences of different @nt{LHS_Sign}
Character values.

If a picture String has '<' as @nt{fixed_LHS_sign}, then it has '>' as
@nt{RHS_sign}.

If a picture String has '<' in a @nt{floating_LHS_sign} or in an
@nt{all_sign_number}, then it has an occurrence of '>'.

If a picture String has '+' or '-' as @nt{fixed_LHS_sign}, in a
@nt{floating_LHS_sign}, or in an @nt{all_sign_number}, then it has no
@nt{RHS_sign}.

An instance of @nt{all_sign_number} does not have occurrences of
different @nt{sign_char} Character values.

An instance of @nt{all_currency_number} does not have occurrences of
different @nt{currency_char} Character values.

An instance of @nt{all_zero_suppression_number} does not have occurrences
of different @nt{zero_suppression_char} Character values, except for possible
case differences between 'Z' and 'z'.
@end{Itemize}

A @i{replicable Character} is a Character that, by the
above rules, can occur in two consecutive positions in a picture String.

A @i{Character replication} is a String
@begin{example}
@RI{char} & '(' & @RI{spaces} & @RI{count_string} & ')'
@end{example}

where @i{char} is a replicable Character,
@i{spaces} is a String (possibly empty) comprising only space Character values,
and @i{count_string} is a String of one or more decimal digit Character
values.
A Character
replication  in a picture String has the same effect
as (and is said to be @i{equivalent to}) a String comprising @i[n]
consecutive occurrences of @i{char}, where
@i[n]=Integer'Value(@i{count_string}).

An @i{expanded picture String} is a picture String containing no
Character replications.
@begin{Discussion}

Since 'B' is not allowed after a RHS sign, there is no need
for a special rule to disallow "9.99DB(2)" as an abbreviation for
"9.99DBB"
@end{discussion}
@end{RunTime}

@begin{Notes}
Although a sign to the left of the number can float, a sign to the right
of the number is in a fixed position.
@end{Notes}

@LabeledSubClause{Edited Output Generation}
@begin{RunTime}
@Leading@;The contents of an edited output string are based on:
@begin{Itemize}
     A value, Item, of some decimal type Num,

     An expanded picture String Pic_String,

     A Boolean value, Blank_When_Zero,

     A Currency string,

     A Fill character,

     A Separator character, and

     A Radix_Mark character.
@end{Itemize}

The combination of a True value for Blank_When_Zero and a '*' character
in Pic_String is inconsistent; no edited output string is defined.

A layout error is identified in the rules below if leading
non-zero digits of Item, character values of the Currency string,
or a negative sign would be
truncated; in such cases no edited output string is defined.

The edited output string has lower bound 1 and upper bound N where
     N = Pic_String'Length + Currency_Length_Adjustment -
         Radix_Adjustment, and
@begin{Itemize}
        Currency_Length_Adjustment =
           Currency'Length @en@; 1 if there is some occurrence of '$' in
             Pic_String, and
           0 otherwise.

        Radix_Adjustment =
           1 if there is an occurrence of 'V' or 'v' in Pic_Str, and
           0 otherwise.
@end{Itemize}

@Leading@defn{displayed magnitude (of a decimal value)}
Let the magnitude of Item be expressed as a base-10 number
I@-(p)@Times@Times@Times@;I@-(1).F@-(1)@Times@Times@Times@;F@-(q),
called the @i{displayed} @i{magnitude} of Item,
where:
@begin{Itemize}
     q = Min(Max(Num'Scale, 0), n) where n is 0 if Pic_String
         has no @nt{radix} and is otherwise the number of digit
         positions following @nt{radix} in Pic_String,
         where a digit position corresponds to an occurrence of
         '9', a @nt{zero_suppression_char} (for an
         @nt{all_zero_suppression_number}), a @nt{currency_char}
         (for an @nt{all_currency_number}), or a @nt{sign_char}
         (for an @nt{all_sign_number}).

     I@-(p) /= 0 if p>0.
@end{Itemize}

If n < Num'Scale, then the above number is the result of rounding
(away from 0 if exactly midway between values).

If Blank_When_Zero = True and the displayed magnitude of Item
is zero,
then the edited output string
comprises all space character values. Otherwise, the picture String is
treated as a sequence of instances of syntactic categories based on
the rules in @RefSecNum[Picture String Formation],
and the edited output string is the concatenation of
string values derived from these categories according to the
following mapping rules.

Table F-1 shows the mapping from a sign control symbol to a
corresponding character or string in the edited output. In the
columns showing the edited output, a lower-case 'b' represents the
space character.
If there is no sign control symbol but the value of Item
is negative, a layout error occurs and no edited output string
is produced.

@Table{Columns=<3>,
Caption=<@b{Table F-1: Edited Output for Sign Control Symbols}>,
Headers=<@b{Sign Control Symbol}@\@b{Edited Output for @*Non-Negative Number}@\@b{Edited Output for @*Negative Number}>,
Body=['+'@\'+'@\'-'
'-'@\'b'@\'-'
'<'@\'b'@\'('
'>'@\'b'@\')'
"CR"@\"bb"@\"CR"@Last
"DB"@\"bb"@\"DB"]}

An instance of @nt{fixed_LHS_sign} maps to a character as shown in
Table F-1.

An instance of @nt{fixed_$_char} maps to Currency.

An instance of @nt{direct_insertion} maps to
       Separator if @nt{direct_insertion} = '_', and to
       the @nt{direct_insertion} Character otherwise.


@Leading@;An instance of @nt{number} maps to a string
   @i{integer_part} & @i{radix_part} & @i{fraction_part}
where:
@begin{Itemize}
   The string for @i{integer_part} is obtained as follows:
@begin{Enumerate}
                 Occurrences of '9'
                  in @nt{fore_digits} of @nt{number} are replaced
                  from right to left
             with the decimal digit character values for I@-(1), ..., I@-(p),
                  respectively.

                 Each occurrence of '9' in @nt{fore_digits}
                  to the left of the leftmost '9' replaced according
                  to rule 1 is replaced with '0'.

            If p exceeds the number of occurrences of '9' in
            @nt{fore_digits} of @nt{number}, then the excess leftmost
            digits are eligible for use in the mapping of
            an instance of @nt{zero_suppression}, @nt{floating_LHS_sign},
            @nt{floating_$_currency},
            or @nt{floating_#_currency} to the left of @nt{number};
            if there is no such instance, then a layout error
            occurs and no edited output string is produced.
@end{Enumerate}

   @leading@;The @i{radix_part} is:
@begin{Itemize}
"" if @nt{number} does not include a @nt{radix}, if @nt{radix} = 'V',
                      or if @nt{radix} = 'v'

                Radix_Mark if @nt{number} includes '.' as @nt{radix}
@end{Itemize}

@leading@;The string for @i{fraction_part} is obtained as follows:
@begin{Enumerate}
                Occurrences of '9'
                 in @nt{aft_digits} of @nt{number} are replaced
                from left to right
                 with the decimal digit character values for F@-(1), ... F@-(q).

                Each occurrence of '9' in @nt{aft_digits}
                 to the right of the rightmost '9' replaced according
                to rule 1
                is replaced by '0'.
@end{Enumerate}
@end{Itemize}

@leading@;An instance of @nt{zero_suppression} maps to the string obtained as follows:
@begin{Enumerate}
                  The rightmost 'Z', 'z', or '*' Character values
                  are replaced
                   with the excess digits (if any)
                   from the @i{integer_part} of the mapping of the @nt{number}
                   to the right of the @nt{zero_suppression} instance,

                  A @nt{context_sensitive_insertion} Character is replaced
                   as though it were a @nt{direct_insertion} Character, if
                   it occurs to the right of some 'Z', 'z', or '*' in
                   @nt{zero_suppression} that has been mapped to an excess
                   digit,

                  @leading@;Each Character to the left of the
                   leftmost Character replaced according to rule 1 above
                  is replaced by:
@begin{InnerItemize}
                     the space character if the zero suppression Character is
                         'Z' or 'z', or

                     the Fill character if the zero suppression Character is '*'.
@end{InnerItemize}

                   A layout error occurs if some excess digits remain
                   after all 'Z', 'z', and '*' Character values in
                   @nt{zero_suppression} have been replaced via rule 1; no
                   edited output string is produced.
@end{Enumerate}

An instance of @nt{RHS_sign} maps to a character or string
as shown in Table F-1.


An instance of @nt{floating_LHS_sign} maps to the string obtained as follows.
@begin{Enumerate}
                   Up to all but one of the rightmost @nt{LHS_Sign}
                   Character values are replaced by the excess digits (if any)
                   from the @i{integer_part} of the mapping of the @nt{number}
                   to the right of the @nt{floating_LHS_sign} instance.

                  The next Character to the left is replaced
                   with the character given by the entry in Table F-1
                   corresponding to the @nt{LHS_Sign} Character.

                  A @nt{context_sensitive_insertion} Character is replaced
                   as though it were a @nt{direct_insertion} Character, if
                   it occurs to the right of the leftmost @nt{LHS_Sign}
                   character replaced according to rule 1.

                  Any other Character is replaced by the space character..

                   A layout error occurs if some excess digits remain
                   after replacement via rule 1; no edited output string is
                   produced.
@end{Enumerate}

An instance of @nt{fixed_#_currency} maps to the Currency string with n
space character values concatenated on the left (if the instance does
not follow a @nt{radix}) or on the right (if the instance does follow a
@nt{radix}), where n is the difference between the length of the
@nt{fixed_#_currency} instance and Currency'Length. A layout error
occurs if Currency'Length exceeds the length of the
@nt{fixed_#_currency} instance; no edited output string is produced.


@Leading@;An instance of @nt{floating_$_currency} maps to the string
obtained as follows:
@begin{Enumerate}
                  Up to all but one of the rightmost '$'
                   Character values are replaced
                 with the excess digits (if any)
                   from the @i{integer_part} of the mapping of the @nt{number}
                   to the right of the @nt{floating_$_currency} instance.

                  The next Character to the left is replaced
                   by the Currency string.

                  A @nt{context_sensitive_insertion} Character is replaced
                   as though it were a @nt{direct_insertion} Character, if
                   it occurs to the right of the leftmost '$' Character
                   replaced via rule 1.

                  Each other Character is replaced by the space character.

                   A layout error occurs if some excess digits remain
                   after replacement by rule 1; no edited output string is
                   produced.
@end{Enumerate}

@leading@;An instance of @nt{floating_#_currency} maps to the string obtained
as follows:
@begin{Enumerate}
                  Up to all but one of the rightmost '#'
                   Character values are replaced
                  with the excess digits (if any)
                   from the @i{integer_part} of the mapping of the @nt{number}
                   to the right of the
                   @nt{floating_#_currency} instance.

  The substring whose last Character occurs at the position immediately
   preceding the leftmost Character replaced via rule 1, and whose
   length is Currency'Length, is replaced by the Currency string.

                  A @nt{context_sensitive_insertion} Character is replaced
                   as though it were a @nt{direct_insertion} Character, if
                   it occurs to the right of the leftmost '#'
                   replaced via rule 1.

                  Any other Character is replaced by the space character.

                   A layout error occurs if some excess digits remain
                   after replacement rule 1, or if there is no substring
                   with the required length for replacement rule 2; no edited
                   output string is produced.
@end{Enumerate}

@leading@;An instance of @nt{all_zero_suppression_number} maps to:
@begin{Itemize}
              a string of all spaces if
              the displayed magnitude of Item is zero,
              the @nt{zero_suppression_char}
              is 'Z' or 'z', and the instance of
              @nt{all_zero_suppression_number} does not have
              a @nt{radix} at its last character position;

              a string containing the Fill character in each position except
              for the character (if any) corresponding to @nt{radix}, if
              @nt{zero_suppression_char} = '*' and the
              displayed magnitude of Item is zero;

              otherwise,
              the same result as if each @nt{zero_suppression_char} in
              @nt{all_zero_suppression_aft} were '9', interpreting
              the instance of @nt{all_zero_suppression_number} as
              either @nt{zero_suppression} @nt{number} (if a @nt{radix}
              and @nt{all_zero_suppression_aft} are present), or
              as @nt{zero_suppression} otherwise.
@end{Itemize}

@leading@;An instance of @nt{all_sign_number} maps to:
@begin{Itemize}
              a string of all spaces if
        the displayed magnitude of Item is zero and the
        instance of @nt{all_sign_number} does not have a @nt{radix}
        at its last character position;

              otherwise,
              the same result as if each @nt{sign_char} in
              @nt{all_sign_number_aft} were '9', interpreting
              the instance of @nt{all_sign_number} as
              either @nt{floating_LHS_sign} @nt{number} (if a @nt{radix}
              and @nt{all_sign_number_aft} are present), or
              as @nt{floating_LHS_sign} otherwise.
@end{Itemize}

@leading@;An instance of @nt{all_currency_number} maps to:
@begin{Itemize}
              a string of all spaces if
        the displayed magnitude of Item is zero and the
        instance of @nt{all_currency_number} does not have a @nt{radix}
        at its last character position;

              otherwise,
              the same result as if each @nt{currency_char} in
              @nt{all_currency_number_aft} were '9', interpreting
              the instance of @nt{all_currency_number} as
               @nt{floating_$_currency} @nt{number} or
               @nt{floating_#_currency} @nt{number}
              (if a @nt{radix}
              and @nt{all_currency_number_aft} are present), or
              as @nt{floating_$_currency} or @nt{floating_#_currency}
              otherwise.
@end{Itemize}
@end{RunTime}

@begin{Examples}
In the result string values shown below, 'b' represents the space character.
@begin{Example}
Item:         Picture and Result Strings:

123456.78     Picture:  "-###**_***_**9.99"
                        "bbb$***123,456.78"
                        "bbFF***123.456,78" (currency = "FF",
                                             separator = '.',
                                             radix mark = ',')

123456.78     Picture:  "-$$$**_***_**9.99"
              Result:   "bbb$***123,456.78"
                       "bbbFF***123.456,78" (currency = "FF",
                                             separator = '.',
                                             radix mark = ',')

0.0          Picture: "-$$$$$$.$$"
             Result:  "bbbbbbbbbb"

0.20         Picture: "-$$$$$$.$$"
             Result:  "bbbbbb$.20"

-1234.565    Picture: "<<<<_<<<.<<###>"
             Result:  "bb(1,234.57DMb)"  (currency = "DM")

12345.67     Picture: "###_###_##9.99"
             Result:  "bbCHF12,345.67"   (currency = "CHF")
@end{Example}
@end{Examples}

@LabeledSubClause{The Package Text_IO.Editing}
@begin{Intro}
The package Text_IO.Editing provides a private type Picture with associated
operations, and a generic package Decimal_Output.
An object of type Picture is composed from a well-formed picture String
(see @RefSecNum(Picture String Formation)) and a Boolean item indicating
whether a zero numeric value will result in an edited output string
of all space characters.
The package Decimal_Output
contains edited output subprograms implementing the effects defined
in @RefSecNum(Edited Output Generation).
@end{Intro}

@begin{StaticSem}
@leading@;The library package Text_IO.Editing has the following declaration:
@begin{Example}
@ChildUnit{Parent=[Ada.Text_IO],Child=[Editing]}
@key(package) Ada.Text_IO.Editing @key(is)

   @key(type) Picture @key(is) @key(private);
@LangDefType{Package=[Ada.Text_IO.Editing],
             Type=[Picture]}

   @key(function) @AdaSubDefn{Valid} (Pic_String      : @key(in) String;
                   Blank_When_Zero : @key(in) Boolean := False) @key(return) Boolean;

   @key(function) @AdaSubDefn{To_Picture} (Pic_String      : @key(in) String;
                        Blank_When_Zero : @key(in) Boolean := False)
      @key(return) Picture;

   @key(function) @AdaSubDefn{Pic_String}      (Pic : @key(in) Picture) @key(return) String;
   @key(function) @AdaSubDefn{Blank_When_Zero} (Pic : @key(in) Picture) @key(return) Boolean;

   @AdaDefn{Max_Picture_Length}  : @key(constant) := @RI{implementation_defined};

   @AdaDefn{Picture_Error}       : @key(exception);

   @AdaDefn{Default_Currency}    : @key(constant) String    := "$";
   @AdaDefn{Default_Fill}        : @key(constant) Character := '*';
   @AdaDefn{Default_Separator}   : @key(constant) Character := ',';
   @AdaDefn{Default_Radix_Mark}  : @key(constant) Character := '.';

   @key(generic)
      @key(type) Num @key(is) @key(delta) <> @key(digits) <>;
      Default_Currency   : @key(in) String    :=  Text_IO.Editing.Default_Currency;
      Default_Fill       : @key(in) Character :=  Text_IO.Editing.Default_Fill;
      Default_Separator  : @key(in) Character :=  Text_IO.Editing.Default_Separator;
      Default_Radix_Mark : @key(in) Character :=  Text_IO.Editing.Default_Radix_Mark;
   @key(package) @AdaDefn{Decimal_Output} @key(is)
      @key(function) @AdaSubDefn{Length} (Pic      : @key(in) Picture;
                       Currency : @key(in) String := Default_Currency)
         @key(return) Natural;

      @key(function) @AdaSubDefn{Valid} (Item     : @key(in) Num;
                      Pic      : @key(in) Picture;
                      Currency : @key(in) String := Default_Currency)
         @key(return) Boolean;

      @key(function) @AdaSubDefn{Image} (Item       : @key(in) Num;
                      Pic        : @key(in) Picture;
                      Currency   : @key(in) String    := Default_Currency;
                      Fill       : @key(in) Character := Default_Fill;
                      Separator  : @key(in) Character := Default_Separator;
                      Radix_Mark : @key(in) Character := Default_Radix_Mark)
         @key(return) String;

      @key(procedure) @AdaSubDefn{Put} (File       : @key(in) File_Type;
                     Item       : @key(in) Num;
                     Pic        : @key(in) Picture;
                     Currency   : @key(in) String    := Default_Currency;
                     Fill       : @key(in) Character := Default_Fill;
                     Separator  : @key(in) Character := Default_Separator;
                     Radix_Mark : @key(in) Character := Default_Radix_Mark);

      @key(procedure) @AdaSubDefn{Put} (Item       : @key(in) Num;
                     Pic        : @key(in) Picture;
                     Currency   : @key(in) String    := Default_Currency;
                     Fill       : @key(in) Character := Default_Fill;
                     Separator  : @key(in) Character := Default_Separator;
                     Radix_Mark : @key(in) Character := Default_Radix_Mark);

      @key(procedure) @AdaSubDefn{Put} (To         : @key(out) String;
                     Item       : @key(in) Num;
                     Pic        : @key(in) Picture;
                     Currency   : @key(in) String    := Default_Currency;
                     Fill       : @key(in) Character := Default_Fill;
                     Separator  : @key(in) Character := Default_Separator;
                     Radix_Mark : @key(in) Character := Default_Radix_Mark);
   @key(end) Decimal_Output;
@key(private)
   ... @RI{-- not specified by the language}
@key(end) Ada.Text_IO.Editing;
@end{Example}
@ImplDef{The value of Max_Picture_Length in the package Text_IO.Editing}

The exception Constraint_Error is raised
if the Image function or any of the
Put procedures is invoked with a null string for Currency.
@begin{DescribeCode}
@begin{Example}
@key(function) Valid (Pic_String      : @key(in) String;
                Blank_When_Zero : @key(in) Boolean := False) @key(return) Boolean;
@end{Example}

Valid returns True if Pic_String is a well-formed picture String
(see @RefSecNum(Picture String Formation)) the
length of whose expansion does not exceed Max_Picture_Length, and if
either Blank_When_Zero is False or Pic_String contains no '*'.

@begin{Example}
@key(function) To_Picture (Pic_String      : @key(in) String;
                     Blank_When_Zero : @key(in) Boolean := False)
   @key(return) Picture;
@end{Example}

To_Picture returns a result Picture such that the application of the
function Pic_String to this result
yields an expanded picture String equivalent to Pic_String, and such
that Blank_When_Zero applied to the result Picture is the same value as the
parameter Blank_When_Zero.
 Picture_Error is raised if not
Valid(Pic_String, Blank_When_Zero).

@begin{Example}
@key(function) Pic_String      (Pic : @key(in) Picture) @key(return) String;

@key(function) Blank_When_Zero (Pic : @key(in) Picture) @key(return) Boolean;
@end{Example}

If Pic is To_Picture(String_Item, Boolean_Item) for some String_Item and
Boolean_Item, then:
@begin[itemize]
Pic_String(Pic) returns an expanded picture String
equivalent to String_Item and with any lower-case letter
replaced with its corresponding upper-case form, and

Blank_When_Zero(Pic) returns Boolean_Item.
@end[Itemize]

If Pic_1 and Pic_2 are objects of type Picture, then "="(Pic_1, Pic_2)
is True when
@begin[itemize]
Pic_String(Pic_1) = Pic_String(Pic_2), and

Blank_When_Zero(Pic_1) = Blank_When_Zero(Pic_2).
@end[Itemize]

@begin{Example}
@key(function) Length (Pic      : @key(in) Picture;
                 Currency : @key(in) String := Default_Currency)
   @key(return) Natural;
@end{Example}

Length returns Pic_String(Pic)'Length + Currency_Length_Adjustment -
Radix_Adjustment where
@begin[itemize]
        Currency_Length_Adjustment =
@begin[Inneritemize]
           Currency'Length @en@; 1 if there is some occurrence of '$' in
             Pic_String(Pic), and

           0 otherwise.
@end[Inneritemize]

        Radix_Adjustment =
@begin[Inneritemize]
           1 if there is an occurrence of 'V' or 'v' in Pic_Str(Pic), and

           0 otherwise.
@end[Inneritemize]
@end[itemize]

@begin{Example}
@key(function) Valid (Item     : @key(in) Num;
                Pic      : @key(in) Picture;
                Currency : @key(in) String := Default_Currency)
   @key(return) Boolean;
@end{Example}

Valid returns True if Image(Item, Pic, Currency) does not raise
Layout_Error, and returns False otherwise.

@begin{Example}
@key(function) Image (Item       : @key(in) Num;
                Pic        : @key(in) Picture;
                Currency   : @key(in) String    := Default_Currency;
                Fill       : @key(in) Character := Default_Fill;
                Separator  : @key(in) Character := Default_Separator;
                Radix_Mark : @key(in) Character := Default_Radix_Mark)
   @key(return) String;
@end{Example}

Image returns the edited output String as defined in
@RefSecNum(Edited Output Generation) for Item,
Pic_String(Pic), Blank_When_Zero(Pic),
Currency, Fill, Separator,
and Radix_Mark. If these rules identify a layout error, then Image
raises the exception Layout_Error.

@begin{Example}
@key(procedure) Put (File       : @key(in) File_Type;
               Item       : @key(in) Num;
               Pic        : @key(in) Picture;
               Currency   : @key(in) String    := Default_Currency;
               Fill       : @key(in) Character := Default_Fill;
               Separator  : @key(in) Character := Default_Separator;
               Radix_Mark : @key(in) Character := Default_Radix_Mark);

@key(procedure) Put (Item       : @key(in) Num;
               Pic        : @key(in) Picture;
               Currency   : @key(in) String    := Default_Currency;
               Fill       : @key(in) Character := Default_Fill;
               Separator  : @key(in) Character := Default_Separator;
               Radix_Mark : @key(in) Character := Default_Radix_Mark);
@end{Example}

Each of these Put procedures
outputs Image(Item, Pic, Currency, Fill, Separator, Radix_Mark)
consistent with the conventions for Put for other real types in case
of bounded line length (see @RefSec{Get and Put Procedures}).

@begin{Example}
@key(procedure) Put (To         : @key(out) String;
               Item       : @key(in) Num;
               Pic        : @key(in) Picture;
               Currency   : @key(in) String    := Default_Currency;
               Fill       : @key(in) Character := Default_Fill;
               Separator  : @key(in) Character := Default_Separator;
               Radix_Mark : @key(in) Character := Default_Radix_Mark);
@end{Example}

Put copies Image(Item, Pic, Currency, Fill, Separator, Radix_Mark)
to the given string, right justified. Otherwise unassigned Character values
in To are assigned the space character. If To'Length is less than
the length of the string resulting from Image, then Layout_Error is raised.
@end{DescribeCode}
@end{StaticSem}

@begin{ImplReq}
Max_Picture_Length shall be at least 30.
The implementation shall support currency strings of length
up to at least 10, both for Default_Currency in an instantiation of
Decimal_Output, and for Currency in an invocation of Image or any
of the Put procedures.
@begin{Discussion}
This implies that a picture string with character replications
need not be supported (i.e., To_Picture will raise Picture_Error)
if its expanded form exceeds 30 characters.
@end{Discussion}
@end{ImplReq}

@begin{Notes}
@Leading@;The rules for edited output are based on COBOL (ANSI X3.23:1985,
endorsed by ISO as ISO 1989-1985), with the
following differences:
@begin{Itemize}
   The COBOL provisions for picture string localization and for 'P' format
   are absent from Ada.

   @Leading@;The following Ada facilities are not in COBOL:
@begin{InnerItemize}
      currency symbol placement after the number,

      localization of edited output string for multi-character
      currency string values, including support for both length-preserving
      and length-expanding currency symbols in picture strings

      localization of the radix mark, digits separator, and
      fill character, and

      parenthesization of negative values.
@end{InnerItemize}
@end{Itemize}
The value of 30 for Max_Picture_Length is the same limit as in COBOL.
@end{Notes}

@begin{Reason}
There are several reasons we have not adopted the COBOL-style permission to
provide a single-character replacement in the picture string for the `$' as
currency symbol, or to interchange the roles of `.' and `,' in picture strings
@begin{Itemize}
It would have introduced considerable complexity into Ada, as well as
confusion between run-time and compile-time character interpretation, since
picture Strings are dynamically computable in Ada, in contrast with COBOL

Ada's rules for real literals provide a natural interpretation of `_' as
digits separator and `.' for radix mark; it is not essential to allow these to
be localized in picture strings, since Ada does not allow them to be localized
in real literals.

The COBOL restriction for the currency symbol in a picture string to be
replaced by
 a single character currency symbol is a compromise
solution. For general international usage
 a mechanism is needed to localize the edited output to
be a multi-character currency string. Allowing a single-Character
localization for the picture Character, and a multiple-character localization
for the currency string, would be an unnecessary complication.
@end{Itemize}
@end{Reason}

@LabeledSubClause{The Package Wide_Text_IO.Editing}

@begin{StaticSem}
@Leading@;@Defn{Ada.Wide_Text_IO.Editing}
@ChildUnit{Parent=[Ada.Wide_Text_IO],Child=[Editing]}
@LangDefType{Package=[Ada.Wide_Text_IO.Editing],
             Type=[Picture]}
The child package Wide_Text_IO.Editing has
the same contents as Text_IO.Editing, except that:
@begin{Itemize}
each occurrence of Character
is replaced by Wide_Character,

each occurrence of Text_IO is replaced by Wide_Text_IO,

the subtype of Default_Currency is Wide_String rather than String, and

each occurrence of String
in the generic package Decimal_Output is replaced by
Wide_String.
@end{Itemize}
@ImplDef{The value of Max_Picture_Length in the package Wide_Text_IO.Editing}
@end{StaticSem}

@begin{Notes}
Each of the functions Wide_Text_IO.Editing.Valid, To_Picture, and Pic_String has String (versus
Wide_String) as its parameter or result subtype, since a picture String
is not localizable.
@end{Notes}
