@Part(02, Root="ada.mss")

@Comment{$Date: 2000/05/29 05:47:19 $}
@LabeledSection{Lexical Elements}

@Comment{$Source: e:\\cvsroot/ARM/Source/02.mss,v $}
@Comment{$Revision: 1.16 $}

@begin{Intro}
@redundant[The text of a program consists of the texts of one or more
@nt<compilation>s.  The text of a @nt<compilation> is a sequence of
lexical elements, each composed of characters; the rules of
composition are given in this section.
@nt{Pragma}s, which provide certain information for the compiler, are also
described in this section.]
@end{Intro}

@LabeledClause{Character Set}

@begin{Intro}
@Defn{character set}
The only characters allowed outside of
@nt{comment}s are the @nt{graphic_character}s and
@nt{format_effector}s.
@begin{Ramification}
Any character, including an @nt<other_control_function>, is allowed in a
comment.

Note that this rule doesn't really have much force,
since the implementation can represent characters in the source in any
way it sees fit.
For example, an implementation could simply define that what seems to be
a non-graphic, non-format-effector character is actually a
representation of the space character.
@end{Ramification}
@begin(Discussion)
It is our intent to follow the terminology of ISO 10646 BMP where
appropriate, and to remain compatible with the character
classifications defined in @RefSec{Character Handling}.
Note that our definition for @nt<graphic_character> is more
inclusive than that of ISO 10646-1.
@end(Discussion)
@end{Intro}

@begin{Syntax}
@Syn{lhs=<character>,
  rhs="@Syn2{graphic_character} | @Syn2{format_effector} | @Syn2{other_control_function}"}


@Syn{lhs=<graphic_character>,rhs="@Syn2{identifier_letter} | @Syn2{digit} | @Syn2{space_character} | @Syn2{special_character}"}

@end{Syntax}

@begin{StaticSem}
The character repertoire for the text of
an Ada program consists of the
collection of characters called
the Basic Multilingual Plane (BMP) of the
ISO 10646 Universal Multiple-Octet Coded Character Set, plus a set
of @nt<format_effector>s and, in comments only,
a set of @nt<other_control_function>s; the coded representation for these
characters is implementation defined @Redundant[(it need not be a
representation defined within ISO-10646-1)].
@ImplDef{The coded representation for the text of an Ada program.}

The description of the
language definition in this International Standard uses the graphic symbols
defined for Row 00: Basic Latin and Row 00: Latin-1 Supplement
of the ISO 10646 BMP; these correspond to the graphic symbols of
ISO 8859-1 (Latin-1); no graphic symbols are used in this International Standard for
characters outside of Row 00 of the BMP.
The actual set of graphic symbols used by an implementation
for the visual representation of
the text of an Ada program is not specified.
@PDefn{unspecified}

The categories of characters are defined as follows:
@begin{Description}
@Defn{identifier_letter}@nt<identifier_letter> @\@nt{upper_case_identifier_letter} | @nt{lower_case_identifier_letter}
@begin{Discussion}
  We use @nt<identifier_letter> instead of simply @nt<letter> because
  ISO 10646 BMP includes many other characters that would generally
  be considered "letters."
@end{Discussion}

@Defn{upper_case_identifier_letter}@nt<upper_case_identifier_letter> @\Any character of Row 00 of ISO 10646 BMP whose
name begins ``Latin Capital Letter''.

@Defn{lower_case_identifier_letter}@nt<lower_case_identifier_letter> @\Any character of Row 00 of ISO 10646 BMP whose
name begins ``Latin Small Letter''.
@begin{Honest}

The above rules do not include the ligatures
@latin1(198) and @latin1(230).
However, the intent is to include these characters as identifier letters.
This problem was pointed out by a comment from the Netherlands.

@end{Honest}

@Defn{digit}@nt<digit> @\One of the characters 0, 1, 2, 3, 4, 5, 6, 7, 8, or 9.

@Defn{space_character}@nt<space_character> @\The character of ISO
10646 BMP named ``Space''.

@Defn{special_character}@nt<special_character> @\Any character of
  the ISO 10646 BMP that is not reserved for a control function, and
  is not the @nt<space_character>, an @nt<identifier_letter>, or a @nt<digit>.
@begin{Ramification}
Note that the no break space and soft hyphen are @nt<special_character>s,
and therefore @nt<graphic_character>s.
They are not the same characters as space and hyphen-minus.
@end{Ramification}

@Defn{format_effector}@nt<format_effector> @\The control functions of ISO 6429 called
  character tabulation (HT), line tabulation (VT), carriage return (CR),
  line feed (LF), and form feed (FF).
@IndexSeeAlso{Term=[control character],See=(format_effector)}

@Defn{other_control_function}@nt<other_control_function> @\Any control
function,
other than a @nt<format_effector>, that is allowed in a comment; the set of
@nt<other_control_function>s allowed in comments is implementation defined.
@ImplDef{The control functions allowed in comments.}
@IndexSeeAlso{Term=[control character],See=(other_control_function)}
@end{Description}

@Defn{names of @nt{special_character}s}
@Defn2{Term=[special_character],Sec=(names)}
The following names are used when referring to certain
@nt{special_character}s:
@Defn{quotation mark}
@Defn{number sign}
@Defn{ampersand}
@Defn{apostrophe}
@Defn{tick}
@Defn{left parenthesis}
@Defn{right parenthesis}
@Defn{asterisk}
@Defn{multiply}
@Defn{plus sign}
@Defn{comma}
@Defn{hyphen-minus}
@Defn{minus}
@Defn{full stop}
@Defn{dot}
@Defn{point}
@Defn{solidus}
@Defn{divide}
@Defn{colon}
@Defn{semicolon}
@Defn{less-than sign}
@Defn{equals sign}
@Defn{greater-than sign}
@Defn{low line}
@Defn{underline}
@Defn{vertical line}
@Defn{left square bracket}
@Defn{right square bracket}
@Defn{left curly bracket}
@Defn{right curly bracket}
@begin{Discussion}
These are the ones that play a special role in the syntax of Ada 9X,
or in the syntax rules;
we don't bother to define names for all characters.
The first name given is the name from ISO 10646-1; the subsequent
names, if any, are those used within the
standard, depending on context.
@end{Discussion}
@begin{DisplayWithoutParaNum}
@TabClear()@TabSet(P9)
@begin(TwoCol)
symbol @\name

  " @\quotation mark
  # @\number sign
  & @\ampersand
  ' @\apostrophe, tick
  ( @\left parenthesis
  ) @\right parenthesis
  * @\asterisk, multiply
  + @\plus sign
  , @\comma
  @en@; @\hyphen-minus, minus
  . @\full stop, dot, point
  / @\solidus, divide
@NewColumn
symbol @\name

  : @\colon
  ; @\semicolon
  < @\less-than sign
  = @\equals sign
  > @\greater-than sign
  _ @\low line, underline
  | @\vertical line
  [ @\left square bracket
  ] @\right square bracket
  { @\left curly bracket
  } @\right curly bracket
@end(TwoCol)
@end{DisplayWithoutParaNum}
@end{StaticSem}

@begin{ImplPerm}
In a nonstandard mode, the implementation
may support a different character repertoire@Redundant[; in particular,
the set of characters that are considered @nt<identifier_letter>s
can be extended
or changed to conform to local conventions].
@begin{Ramification}
If an implementation supports other character sets,
it defines which characters fall into each category,
such as ``@nt{identifier_letter},''
and what the corresponding rules of this section are,
such as which characters are allowed in the text of a program.
@end{Ramification}
@end{ImplPerm}

@begin{Notes}
Every code position of ISO 10646 BMP that is not reserved for a control
function is defined to be a @nt<graphic_character> by this International Standard.
This includes all code positions other than 0000 - 001F, 007F - 009F,
and FFFE - FFFF.

The language does not specify the source representation of programs.
@begin(Discussion)
Any source representation is valid so long as the
implementer can produce an (information-preserving)
algorithm for translating both directions
between the representation and the standard character set.
(For example, every character in the standard character set has to be
representable, even if the output devices attached to a given computer
cannot print all of those characters properly.)
From a practical point of view, every implementer will have to provide
some way to process the ACVC.
It is the intent to allow source representations, such as parse trees,
that are not even linear sequences of characters.
It is also the intent to allow different fonts:
reserved words might be in bold face,
and that should be irrelevant to the semantics.
@end(Discussion)
@end{Notes}

@begin{Extend83}
Ada 9X allows 8-bit and 16-bit characters,
as well as implementation-specified character sets.
@end{Extend83}

@begin{DiffWord83}
The syntax rules in this clause are modified to remove the emphasis
on basic characters vs. others.
(In this day and age, there is no need to point out that you can write
programs without using (for example) lower case letters.)
In particular, @nt{character} (representing all characters usable outside
comments) is added, and @nt{basic_graphic_character},
@nt{other_special_character},
and @nt{basic_character} are removed.
@nt{Special_character} is expanded to include Ada 83's
@nt{other_special_character}, as well as new 8-bit characters not
present in Ada 83.
Note that the term ``basic letter'' is used
in @RefSec{Character Handling}
to refer to letters without diacritical marks.

Character names now come from ISO 10646.

We use @nt<identifier_letter> rather than @nt<letter> since
ISO 10646 BMP includes many "letters' that are not permitted in
identifiers (in the standard mode).
@end{DiffWord83}

@LabeledClause{Lexical Elements, Separators, and Delimiters}

@begin{StaticSem}
@Defn{text of a program}
The text of a program consists of the texts of one or more
@nt<compilation>s.
@Defn{lexical element}
@IndexSee{Term=[token],See=(lexical element)}
The text of each @nt<compilation> is a sequence
of separate @i(lexical elements).
Each lexical element is formed from a
sequence of characters, and is
either a delimiter, an @nt<identifier>, a reserved word,
a @nt<numeric_literal>, a @nt<character_literal>, a @nt<string_literal>,
or a comment.
The meaning of a program depends only on the particular sequences of
lexical elements that form its @nt{compilation}s, excluding
@nt{comment}s.

The text of a @nt<compilation> is divided into @Defn{line}@i{lines}.
@Defn{end of a line}
In general, the representation for an end of line is implementation defined.
@ImplDef{The representation for an end of line.}
However, a sequence of one or more @nt<format_effector>s other
than character tabulation (HT) signifies at least one end of line.

@Defn{separator}
@Redundant[In some cases an explicit @i(separator) is required
to separate adjacent lexical elements.]
A separator is
any of a space character, a format
effector, or the end of a line, as follows:
@begin(Discussion)
It might be useful to define ``white space'' and use it here.
@end(Discussion)
@begin{Itemize}
A space character is a separator except within a @nt{comment}, a
@nt{string_literal}, or a @nt{character_literal}.

Character tabulation (HT) is a
separator except within a @nt{comment}.

The end of a line is always a separator.
@end{Itemize}

One or more separators are allowed between any two adjacent lexical
elements, before the first of each @nt{compilation}, or after the
last.
At least one separator is required between an @nt{identifier},
a reserved word, or a @nt{numeric_literal} and an adjacent
@nt{identifier}, reserved word, or @nt{numeric_literal}.

@Defn{delimiter}
A @i{delimiter} is either one of the following special characters
@begin{Display}
&@ @ @ @ '@ @ @ @ (@ @ @ @ )@ @ @ @ *@ @ @ @ +@ @ @ @ ,@ @ @ @ -@ @ @ @ .@ @ @ @ /@ @ @ @ :@ @ @ @ ;@ @ @ @ <@ @ @ @ =@ @ @ @ >@ @ @ @ |
@end{Display}

@Defn{compound delimiter}
or one of the following @i{compound delimiters} each composed of two
adjacent special characters
@begin{Display}
=>@ @ @ @ ..@ @ @ @ **@ @ @ @ :=@ @ @ @ /=@ @ @ @ >=@ @ @ @ <=@ @ @ @ <<@ @ @ @ >>@ @ @ @ <>
@end{Display}

Each of the special characters listed for single character delimiters
is a single delimiter except if this character is used as a character
of a compound delimiter, or as a character of a @nt{comment},
@nt{string_literal}, @nt{character_literal}, or
@nt{numeric_literal}.

The following names are used when referring to compound
delimiters:
@begin{Display}
@TabClear()@TabSet(P11)delimiter @\name

   => @\arrow
   .. @\double dot
   ** @\double star, exponentiate
   := @\assignment (pronounced: ``becomes'')
   /= @\inequality (pronounced: ``not equal'')
   >= @\greater than or equal
   <= @\less than or equal
   << @\left label bracket
   >> @\right label bracket
   <> @\box
@end{Display}
@end{StaticSem}

@begin{ImplReq}
An implementation shall support lines of
at least 200 characters in length, not counting any characters used to signify
the end of a line.
An implementation shall support lexical elements of at least
200 characters in length.
The maximum supported line length and lexical element length are
implementation defined.
@ImplDef{Maximum supported line length and lexical element length.}
@begin{Discussion}
From URG recommendation.
@end{Discussion}
@end{ImplReq}

@LabeledClause{Identifiers}

@begin{Intro}
@nt<Identifier>s are used as names.
@end{Intro}

@begin{Syntax}
@Syn{lhs=<identifier>,rhs="
   @Syn2{identifier_letter} {[@Syn2{underline}] @Syn2{letter_or_digit}}"}

@Syn{lhs=<letter_or_digit>,rhs="@Syn2{identifier_letter} | @Syn2{digit}"}

@begin{SyntaxText}
An @nt{identifier} shall not be a reserved word.
@end{SyntaxText}
@end{Syntax}

@begin{StaticSem}
All characters of an @nt{identifier} are significant, including any
underline character.
@Defn{case insensitive}
@nt{Identifier}s differing only in the use of
corresponding upper and lower case letters are considered the
same.
@begin(Discussion)
  Two of the letters of ISO 8859-1 appear only as lower case,
  "sharp s" and "y with diaeresis."  These two letters have
  no corresponding upper case letter (in particular, they
  are not considered equivalent to one another).
@end(Discussion)
@end{StaticSem}

@begin{ImplPerm}
In a nonstandard mode,
an implementation may support other upper/lower case equivalence
rules for @nt<identifier>s@Redundant[,
to accommodate local conventions].
@end{ImplPerm}

@begin{Examples}
@i{Examples of identifiers:}
@begin{Display}
Count      X    Get_Symbol   Ethelyn   Marion

Snobol_4   X1   Page_Count    Store_Next_Item
@end{Display}
@end{Examples}

@begin{DiffWord83}
We no longer include reserved words as @nt<identifier>s.
This is not a language change.
In Ada 83, @nt{identifier} included reserved words.
However, this complicated several other
rules (for example, regarding implementation-defined
attributes and pragmas, etc.).
We now explicitly allow certain reserved words for attribute designators,
to make up for the loss.
@begin{Ramification}
Because syntax rules are relevant to overload resolution,
it means that if it looks like a reserved word,
it is not an @nt<identifier>.
As a side effect, implementations cannot use reserved words as
implementation-defined attributes or pragma names.
@end{Ramification}
@end{DiffWord83}

@LabeledClause{Numeric Literals}

@begin{Intro}
@Defn2{Term=[literal], Sec=(numeric)}
There are two kinds of @nt<numeric_literal>s, @i(real literals) and @i(integer
literals).
@Defn{real literal}
A real literal is a @nt{numeric_literal} that includes a point;
@Defn{integer literal}
an integer literal is a @nt{numeric_literal} without a point.
@end{Intro}

@begin{Syntax}
@Syn{lhs=<numeric_literal>,rhs="@Syn2{decimal_literal} | @Syn2{based_literal}"}
@end{Syntax}

@begin{Notes}
The type of an integer literal is @i{universal_integer}.
The type of a real literal is @i{universal_real}.
@end{Notes}

@LabeledSubClause{Decimal Literals}

@begin{Intro}
@Defn2{Term=[literal], Sec=(decimal)}
A @nt<decimal_literal> is a @nt<numeric_literal> in the conventional
decimal notation (that is, the base is ten).
@end{Intro}

@begin{Syntax}
@Syn{lhs=<decimal_literal>,rhs="@Syn2{numeral} [.@Syn2{numeral}] [@Syn2{exponent}]"}


@Syn{lhs=<numeral>,rhs="@Syn2{digit} {[@Syn2{underline}] @Syn2{digit}}"}

@Syn{lhs=<exponent>,rhs="E [+] @Syn2{numeral} | E @en@; @Syn2{numeral}"}

@begin{SyntaxText}
An @nt{exponent} for an integer literal shall not have a minus sign.
@begin{Ramification}
Although
this rule is in this subclause, it applies also to the next subclause.
@end{Ramification}
@end{SyntaxText}
@end{Syntax}

@begin{StaticSem}
An underline character in a
@nt{numeric_literal} does not affect its meaning.
The letter E of an @nt{exponent} can be written either in lower
case or in upper case, with the same meaning.
@begin{Ramification}
Although these rules are in this subclause,
they apply also to the next subclause.
@end{Ramification}

An @nt{exponent} indicates the power of ten by which the value of the
@nt{decimal_literal} without the @nt{exponent} is to be multiplied to
obtain the value of the @nt{decimal_literal} with the @nt{exponent}.
@end{StaticSem}

@begin{Examples}
@i{Examples of decimal literals:}
@begin{Display}
@tabclear()@tabset(P31)
12        0      1E6    123_456 @\--@i{  integer literals}

12.0      0.0    0.456  3.14159_26 @\--@i{  real literals}
@end{Display}
@end{Examples}

@begin{DiffWord83}
We have changed the syntactic category name @nt{integer} to be @nt{numeral}.
We got this idea from ACID.
It avoids the confusion between this and integers.
(Other places don't offer similar confusions.
For example, a @nt{string_literal} is different from a string.)
@end{DiffWord83}

@LabeledSubClause{Based Literals}

@begin{Intro}
@Redundant[
@Defn2{Term=[literal], Sec=(based)}
@Defn{binary literal}
@Defn{base 2 literal}
@Defn2{Term=[binary], Sec=(literal)}
@Defn{octal literal}
@Defn{base 8 literal}
@Defn2{Term=[octal], Sec=(literal)}
@Defn{hexadecimal literal}
@Defn{base 16 literal}
@Defn2{Term=[hexadecimal], Sec=(literal)}
A @nt<based_literal> is a @nt<numeric_literal> expressed in a form
that specifies the base explicitly.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<based_literal>,rhs="
   @Syn2{base} # @Syn2{based_numeral} [.@Syn2{based_numeral}] # [@Syn2{exponent}]"}


@Syn{lhs=<base>,rhs="@Syn2{numeral}"}

@Syn{lhs=<based_numeral>,rhs="
   @Syn2{extended_digit} {[@Syn2{underline}] @Syn2{extended_digit}}"}

@Syn{lhs=<extended_digit>,rhs="@Syn2{digit} | A | B | C | D | E | F"}

@end{Syntax}

@begin{Legality}
@Defn{base}
The @i(base) (the numeric value of the decimal @nt<numeral> preceding
the first #) shall be at least two and at most sixteen.
The @nt{extended_digit}s A through F represent the digits ten through
fifteen, respectively.
The value of each @nt{extended_digit} of a @nt{based_literal}
shall be less than the base.
@end{Legality}

@begin{StaticSem}
The conventional meaning of based notation is assumed.
An @nt{exponent} indicates the
power of the base by which the value of the @nt{based_literal}
without the @nt{exponent} is to be multiplied to obtain the value of
the @nt{based_literal} with the @nt{exponent}.
The @nt{base} and the @nt{exponent}, if any, are in decimal notation.

The @nt{extended_digit}s A through F can be written either in
lower case or in upper case, with the same meaning.
@end{StaticSem}

@begin{Examples}
@i{Examples of based literals:}
@begin{Display}
@tabclear()@tabset(P16, P45)
2#1111_1111# @\16#FF#       016#0ff# @\--@i{  integer literals of value 255}
16#E#E1     @\2#1110_0000#     @\--@i{  integer literals of value 224}
16#F.FF#E+2 @\2#1.1111_1111_1110#E11 @\--@i{  real literals of value 4095.0}
@end{Display}
@end{Examples}

@begin{DiffWord83}
The rule about which letters are allowed is now encoded in BNF,
as suggested by Mike Woodger.
This is clearly more readable.
@end{DiffWord83}

@LabeledClause{Character Literals}

@begin{Intro}
@Redundant[A @nt<character_literal> is formed by enclosing a graphic
character between two apostrophe characters.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<character_literal>,rhs="@SingleQuote@Syn2{graphic_character}@SingleQuote"}
@end{Syntax}

@begin{Notes}
A @nt{character_literal} is an enumeration literal
of a character type.  See @RefSecNum(Character Types).
@end{Notes}

@begin{Examples}
@i{Examples of character literals:}
@begin{Display}
'A'@ @ @ @ @ '*'@ @ @ @ @ '''@ @ @ @ @ '@ '
@end{Display}
@end{Examples}

@begin{DiffWord83}
The definitions of the values of literals are in
Sections 3 and 4, rather than here,
since it requires knowledge of types.
@end{DiffWord83}

@LabeledClause{String Literals}

@begin{Intro}
@redundant[A @nt<string_literal> is formed by a sequence of graphic characters
(possibly none) enclosed between two quotation marks used as
string brackets.  They are used to represent @nt<operator_symbol>s
(see @RefSecNum(Subprogram Declarations)), values of a string type
(see @RefSecNum(Literals)), and array subaggregates
(see @RefSecNum(Array Aggregates)).
@IndexSee{Term=[quoted string],See=(string_literal)}]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<string_literal>,rhs=<"{@Syn2{string_element}}">}

@Syn{lhs=<string_element>,
  rhs=<"" | @SynI{non_quotation_mark_}@Syn2{graphic_character}>}

@begin{SyntaxText}
A @nt{string_element} is either a pair of quotation marks (""),
or a single @nt{graphic_character} other than a quotation mark.
@end{SyntaxText}
@end{Syntax}

@begin{StaticSem}
@Defn2{Term=[sequence of characters], Sec=(of a @nt<string_literal>)}
The @i(sequence of characters) of a @nt<string_literal> is formed
from the sequence of @nt<string_element>s between the bracketing
quotation marks, in the given order,
with a @nt<string_element> that is "" becoming a single quotation
mark in the sequence of characters,
and any other @nt<string_element> being reproduced
in the sequence.

@Defn{null string literal}
A @i(null string literal) is a @nt<string_literal> with no
@nt<string_element>s between the quotation marks.

@end{StaticSem}

@begin{Notes}
An end of line cannot appear in a @nt{string_literal}.
@end{Notes}

@begin{Examples}
@i{Examples of string literals:}
@begin{Display}
@tabclear()@tabset(P16)
"Message of the day:"

""   @\--@i{  a null string literal}
"@ "   "A"   """"     @\--@i{  three string literals of length 1}

"Characters such as $, %, and } are allowed in string literals"
@end{Display}
@end{Examples}

@begin{DiffWord83}
The wording has been changed to be strictly lexical.
No mention is made of string or character values, since
@nt<string_literal>s are also used to represent @nt<operator_symbol>s,
which don't have a defined value.

The syntax is described differently.
@end{DiffWord83}

@LabeledClause{Comments}

@begin{Intro}
A @nt{comment} starts with two adjacent hyphens and extends up to the end
of the line.
@end{Intro}

@begin{Syntax}
@Syn{lhs=<comment>,
  rhs=<--{@SynI{non_end_of_line_}@Syn2{character}}>}

@begin{SyntaxText}
A @nt{comment} may appear on any line of a program.
@end{SyntaxText}
@end{Syntax}

@begin{StaticSem}
The presence or absence of @nt{comment}s has no influence on whether a program
is legal or illegal.  Furthermore, @nt{comment}s do not influence the meaning
of a program; their sole purpose is the enlightenment of the human reader.
@end{StaticSem}

@begin{Examples}
@i{Examples of comments:}
@begin{Display}
--@i{  the last sentence above echoes the Algol 68 report }

@key[end];  --@i{  processing of Line is complete }

--@i{  a long comment may be split onto}
--@i{  two or more consecutive lines   }

----------------@i{  the first two hyphens start the comment  }
@end{Display}
@end{Examples}

@LabeledClause{Pragmas}

@begin{Intro}
@ToGlossaryAlso{Term=<Pragma>,
  Text=<A pragma is a compiler directive.
  There are language-defined pragmas that give instructions
  for optimization, listing control, etc.
  An implementation may support additional
  (implementation-defined) pragmas.>}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<pragma>,rhs="
   @key{pragma} @Syn2{identifier} [(@Syn2{pragma_argument_association} {, @Syn2{pragma_argument_association}})];"}


@Syn{lhs=<pragma_argument_association>,rhs="
     [@SynI{pragma_argument_}@Syn2{identifier} =>] @Syn2{name}
   | [@SynI{pragma_argument_}@Syn2{identifier} =>] @Syn2{expression}"}

@begin{SyntaxText}
In a @nt<pragma>, any @nt<pragma_argument_association>s without a
@i{pragma_argument_}@nt<identifier> shall precede any
associations with a
@i{pragma_argument_}@nt<identifier>.

@nt{Pragma}s are only allowed at the following places in a program:
@begin{Itemize}
After a semicolon delimiter, but not within a
@nt{formal_part}
or @nt{discriminant_part}.


At any place where the syntax rules allow a construct defined by a
syntactic category whose name ends with "@nt{declaration}", "@nt{statement}",
"@nt{clause}", or "@nt{alternative}", or one of the syntactic categories
@nt{variant} or @nt{exception_handler};
but not in place of such a construct.
Also at any place where a @nt{compilation_unit} would be allowed.
@end{Itemize}

Additional syntax rules and placement restrictions
exist for specific pragmas.
@end{SyntaxText}
@begin{Discussion}
The above rule is written in text,
rather than in BNF;
the syntactic category @nt{pragma} is not used in any BNF syntax rule.
@end{Discussion}
@begin{Ramification}
A @nt{pragma} is allowed where a
@nt<generic_formal_parameter_declaration> is allowed.
@end{Ramification}
@end{Syntax}

@begin{Intro}
@Defn2{Term=[name], Sec=(of a @nt{pragma})}
@Defn{pragma name}
The @i{name} of a @nt{pragma} is
the identifier following the reserved word @key{pragma}.
@Defn{pragma argument}
@Defn{argument of a pragma}
The @nt{name} or @nt{expression} of a @nt{pragma_argument_association}
is a @i{pragma argument}.

@Defn{identifier specific to a pragma}
@Defn{pragma, identifier specific to}
An @i{identifier specific to a pragma} is
an identifier that is used in a pragma argument with special meaning
for that pragma.
@begin{Honest}
Whenever the syntax rules for a given pragma allow
"@nt{identifier}" as an argument of the @nt{pragma},
that @nt{identifier} is an identifier specific to that
pragma.
@end{Honest}
@end{Intro}

@begin{StaticSem}
If an implementation does not recognize the name of a
@nt{pragma}, then it has no effect on the semantics
of the program.
Inside such a @nt{pragma}, the only rules that apply are the
@SyntaxName@;s.
@begin{Honest}
This rule takes precedence over any other rules that imply otherwise.
@end{Honest}
@begin{Ramification}
Note well: this rule applies only to @nt{pragma}s whose name is not
recognized.
If anything else is wrong with a @nt{pragma} (at compile time),
the @nt{pragma} is illegal.
This is true whether the @nt{pragma} is
language defined or implementation defined.

For example, an expression in an unrecognized @nt{pragma} does
not cause freezing, even though the rules in
@RefSec{Freezing Rules} say it does;
the above rule overrules those other rules.
On the other hand, an expression in a recognized @nt{pragma} causes
freezing, even if this makes something illegal.

For another example, an expression that would be ambiguous is not
illegal if it is inside an unrecognized @nt{pragma}.

Note, however, that implementations have to recognize @key[pragma]
Inline(Foo) and freeze things accordingly, even if they choose to never
do inlining.

Obviously, the contradiction needs to be resolved one way or the
other.  The reasons for resolving it this way are:  The
implementation is simple @em the compiler can just ignore the
@nt{pragma} altogether.
The interpretation of constructs appearing inside
implementation-defined @nt{pragma}s is implementation defined.  For
example: ``@key[pragma] Mumble(X);''.  If the current implementation has
never heard of Mumble, then it doesn't know whether X is a name,
an expression, or an identifier specific to the pragma Mumble.
@end{Ramification}
@begin{Honest}
The syntax of individual pragmas overrides the general
syntax for @nt{pragma}.
@end{Honest}
@begin{Ramification}
Thus, an identifier specific to a pragma is not a @nt{name},
syntactically; if it were, the visibility rules would be invoked,
which is not what we want.

This also implies that named associations do not allow one to give the
arguments in an arbitrary order @em the order given in the syntax rule
for each individual pragma must be obeyed.
However, it is generally possible to leave out earlier arguments when
later ones are given; for example, this is allowed by the syntax rule
for pragma Import (see @RefSec{Interfacing Pragmas}).
As for subprogram calls, positional notation precedes named
notation.

Note that Ada 83 had no pragmas for which the order of named
associations mattered, since there was never more than one argument
that allowed named associations.
@end{Ramification}
@begin{Honest}
The interpretation of the arguments of implementation-defined pragmas
is implementation defined.
However, the syntax rules have to be obeyed.
@end{Honest}
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[execution], Sec=(pragma)}
@PDefn2{Term=[elaboration], Sec=(pragma)}
Any @nt{pragma} that appears at the place of an executable construct is
executed.
Unless otherwise specified for a particular pragma,
this execution consists of the evaluation of each evaluable pragma
argument in an arbitrary order.
@begin{Ramification}
For a @nt{pragma} that appears at the place of an elaborable
construct, execution is elaboration.

An identifier specific to a pragma is neither a @nt{name} nor an
@nt{expression} @em such identifiers are not evaluated (unless an
implementation defines them to be evaluated in the case of an
implementation-defined @nt{pragma}).

The ``unless otherwise specified'' part allows us (and
implementations) to make exceptions, so a @nt{pragma} can contain an
expression that is not evaluated.
Note that @nt{pragma}s in @nt{type_definition}s may contain expressions
that depend on discriminants.

When we wish to define a pragma with some run-time effect,
we usually make sure that it appears in an executable context;
otherwise, special rules are needed to define the run-time effect
and when it happens.
@end{Ramification}
@end{RunTime}

@begin{ImplReq}
The implementation shall give a warning message for
an unrecognized pragma name.
@begin{Ramification}
An implementation is also allowed to have modes in which a warning
message is suppressed, or in which the presence of an unrecognized
@nt{pragma} is a compile-time error.
@end{Ramification}
@end{ImplReq}

@begin{ImplPerm}
An implementation may provide implementation-defined pragmas;
the name of an implementation-defined pragma shall differ
from those of the language-defined pragmas.
@ImplDef{Implementation-defined pragmas.}
@begin{Ramification}
The semantics of implementation-defined pragmas, and any associated
rules (such as restrictions on their placement or arguments),
are, of course, implementation defined.
Implementation-defined pragmas may have run-time effects.
@end{Ramification}

An implementation may ignore an unrecognized pragma even if
it violates some of the @SyntaxName@;s, if detecting the
syntax error is too complex.
@begin{Reason}
  Many compilers use extra post-parsing checks to enforce the syntax
  rules, since the Ada syntax rules are not LR(k) (for any k).
  (The grammar is ambiguous, in fact.)
  This paragraph allows them to ignore an unrecognized pragma, without
  having to perform such post-parsing checks.
@end{Reason}
@end{ImplPerm}

@begin{ImplAdvice}
Normally,
implementation-defined pragmas should have no semantic effect
for error-free programs;
that is, if the implementation-defined pragmas are removed from a
working program,
the program should still be legal, and should still have the same
semantics.
@begin{Ramification}
Note that ``semantics'' is not the same as ``effect;''
as explained in
@RefSecNum{Conformity of an Implementation With the Standard},
the semantics defines a set of possible effects.

Note that adding a @nt{pragma} to a program might cause an error
(either at compile time or at run time).
On the other hand, if the language-specified semantics for a
feature are in part implementation defined,
it makes sense to support pragmas that control the feature,
and that have real semantics; thus,
this paragraph is merely a recommendation.
@end{Ramification}

Normally,
an implementation should not define pragmas that can make an illegal
program legal, except as follows:
@begin(Itemize)
  A @nt<pragma> used to complete a declaration, such as a @nt{pragma} Import;

  A @nt<pragma> used to configure the environment
  by adding, removing, or replacing @nt{library_item}s.
@end(Itemize)
@begin(Ramification)
For example, it is OK to support Interface,
System_Name, Storage_Unit, and Memory_Size @nt{pragma}s
for upward compatibility reasons,
even though all of these @nt{pragma}s can make an illegal program legal.
(The latter three can affect legality in a rather subtle way:
They affect the value of named numbers in System,
and can therefore affect the legality in cases where
static expressions are required.)

On the other hand,
adding implementation-defined pragmas to a legal program can make it illegal.
For example, a common kind of implementation-defined pragma is one that
promises some property that allows more efficient code to be generated.
If the promise is a lie,
it is best if the user gets an error message.
@end(Ramification)
@end{ImplAdvice}

@begin{Incompatible83}
In Ada 83, ``bad'' @nt{pragma}s are ignored.
In Ada 9X, they are illegal,
except in the case where the name of the @nt{pragma} itself
is not recognized by the implementation.
@end{Incompatible83}

@begin{Extend83}
Implementation-defined @nt{pragma}s may affect the legality of a program.
@end{Extend83}

@begin{DiffWord83}
Implementation-defined @nt{pragma}s may affect the run-time semantics of
the program.
This was always true in Ada 83 (since it was not explicitly forbidden by
RM83), but it was not clear, because there was no definition of
``executing'' or ``elaborating'' a @nt{pragma}.
@end{DiffWord83}

@begin{Syntax}
@begin{SyntaxText}
The forms of List, Page, and Optimize @nt{pragma}s are as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(List)(@Syn2{identifier});'

@PragmaSyn`@key{pragma} @prag(Page);'

@PragmaSyn`@key{pragma} @prag(Optimize)(@Syn2{identifier});'

@begin{SyntaxText}
@Redundant[Other pragmas are defined throughout this International Standard,
and are summarized in
@RefSecNum{Language-Defined Pragmas}.]
@begin{Ramification}
The language-defined pragmas
are supported by every implementation,
although ``supporting'' some of
them (for example, Inline) requires nothing more than checking the
arguments, since they act only as advice to the implementation.
@end{Ramification}

@end{SyntaxText}
@end{Syntax}

@begin{StaticSem}
A @nt{pragma} List
takes one of the @nt{identifier}s On or Off as the single
argument.  This pragma is allowed anywhere a
@nt{pragma} is allowed.  It specifies that listing of the
compilation is to be continued or suspended until a List
@nt{pragma} with the opposite argument is given within the
same compilation.  The @nt{pragma} itself is always listed if
the compiler is producing a listing.

A @nt{pragma} Page
is allowed anywhere a
@nt{pragma} is allowed.  It specifies that the program text
which follows the @nt{pragma} should start on a new page (if
the compiler is currently producing a listing).

A @nt{pragma} Optimize
takes one of the @nt{identifier}s Time, Space, or Off as the
single argument.  This @nt{pragma} is allowed anywhere a @nt<pragma>
is allowed, and it applies until the end of the immediately enclosing
declarative region,
or for a @nt{pragma} at the place of a @nt{compilation_unit},
to the end of the @nt<compilation>.
It gives advice to the implementation as to
whether time or space is the primary optimization criterion, or
that optional optimizations should be turned off.
@Redundant[It is implementation defined how this advice is followed.]
@ImplDef{Effect of pragma Optimize.}
@begin{Discussion}

For example, a compiler might use Time vs. Space to control whether
generic instantiations are implemented with a macro-expansion model,
versus a shared-generic-body model.


We don't define what constitutes an ``optimization''
@em in fact, it cannot be formally defined in the context of Ada.
One compiler might call something an optional optimization,
whereas another compiler might consider that same thing
to be a normal part of code generation.
Thus, the programmer cannot rely on this pragma having any
particular portable effect on the generated code.
Some compilers might even ignore the pragma altogether.
@end{Discussion}
@end{StaticSem}

@begin{Examples}
@i{Examples of pragmas:}
@begin{Example}
@key[pragma] List(Off); --@RI{ turn off listing generation}
@key[pragma] Optimize(Off); --@RI{ turn off optional optimizations}
@key[pragma] Inline(Set_Mask); --@RI{ generate code for Set_Mask inline}
@key[pragma] Suppress(Range_Check, On => Index); --@RI{ turn off range checking on Index}
@end{Example}
@end{Examples}

@begin{Extend83}
The Optimize @nt<pragma> now allows the identifier Off
to request that normal optimization be turned off.

An Optimize @nt<pragma> may appear anywhere pragmas are allowed.
@end{Extend83}

@begin{DiffWord83}
We now describe the pragmas Page, List, and Optimize here,
to act as examples, and to remove the normative material from
@RefSec{Language-Defined Pragmas}, so it can be entirely an
informative annex.
@end{DiffWord83}

@NewPage
@LabeledClause{Reserved Words}

@begin{Syntax}
@begin{Bundle}
@begin{SyntaxText}
@ChgRef{Version=[1], Kind=[Deleted]}
@Chg[New=<>,Old=<@ @;@comment{Empty paragraph to hang junk paragraph number from original RM}>]

@Defn{reserved word}
The following are the @i{reserved words}
(ignoring upper/lower case distinctions):
@end{SyntaxText}
@begin{Discussion}
  Reserved words have special meaning in the syntax.
  In addition, certain reserved words are used as attribute names.

  The syntactic category @nt{identifier}
  no longer allows reserved words.  We have added the few reserved
  words that are legal explicitly to the syntax for @nt{attribute_reference}.
  Allowing identifier to include reserved words has been a source
  of confusion for some users, and differs from the way they
  are treated in the C and Pascal language definitions.
@end{Discussion}
@*

@begin{DisplayWithoutParanum}
@begin{FourCol}
@key{abort}
@key{abs}
@key{abstract}
@key{accept}
@key{access}
@key{aliased}
@key{all}
@key{and}
@key{array}
@key{at}

@key{begin}
@key{body}

@key{case}
@key{constant}

@key{declare}
@key{delay}
@key{delta}
@key{digits}
@key{do}
@NewColumn

@key{else}
@key{elsif}
@key{end}
@key{entry}
@key{exception}
@key{exit}

@key{for}
@key{function}

@key{generic}
@key{goto}

@key{if}
@key{in}
@key{is}


@key{limited}
@key{loop}

@key{mod}
@NewColumn

@key{new}
@key{not}
@key{null}


@key{of}
@key{or}
@key{others}
@key{out}

@key{package}
@key{pragma}
@key{private}
@key{procedure}
@key{protected}

@key{raise}
@key{range}
@key{record}
@key{rem}
@key{renames}
@key{requeue}
@NewColumn

@key{return}
@key{reverse}

@key{select}
@key{separate}
@key{subtype}

@key{tagged}
@key{task}
@key{terminate}
@key{then}
@key{type}


@key{until}
@key{use}

@key{when}
@key{while}
@key{with}

@key{xor}
@end{FourCol}
@end{DisplayWithoutParanum}
@end{Bundle}
@end{Syntax}

@begin{Notes}
The reserved words appear in @key{lower case boldface}
in this International Standard,
except when used in the @nt{designator} of an attribute
(see @RefSecNum(Attributes)).

Lower case boldface is also used

for a reserved word in a
@nt{string_literal} used as an @nt{operator_symbol}.
This is merely a convention @em programs may be written in whatever
typeface is desired and available.
@end{Notes}

@begin{Incompatible83}
The following words are not reserved in Ada 83, but are reserved in Ada
9X: @key{abstract}, @key{aliased}, @key{protected}, @key{requeue},
@key{tagged}, @key{until}.
@end{Incompatible83}

@begin{DiffWord83}
The clause entitled ``Allowed Replacements of Characters'' has been moved
to @RefSec(Obsolescent Features).
@end{DiffWord83}