@Part(02, Root="ada.mss")

@Comment{$Date: 2012/04/03 20:37:01 $}
@LabeledSection{Lexical Elements}

@Comment{$Source: e:\\cvsroot/ARM/Source/02.mss,v $}
@Comment{$Revision: 1.84 $}

@begin{Intro}
@redundant[The text of a program consists of the texts of one or more
@nt<compilation>s. The text of a @nt<compilation> is a sequence of
lexical elements, each composed of characters; the rules of
composition are given in this section.
@nt{Pragma}s, which provide certain information for the compiler, are also
described in this section.]
@end{Intro}

@LabeledClause{Character Set}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0266-1]}
@Defn{character set}
The @Chg{Version=[2],New=[character repertoire for the text of an Ada
program consists of the
entire coding space described by the ISO/IEC 10646:@Chg{Version=[3],New=[2011],Old=[2003]}
Universal
Multiple-Octet Coded Character Set. This coding space is organized in
@i<planes>, each plane comprising 65536 characters.@Defn2{Term=[plane],Sec=[character]}
@Defn{character plane}],Old=[only characters
allowed outside of @nt{comment}s are the @nt{graphic_character}s and
@ntf{format_effector}s.]}

@begin{Ramification}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00285-01]}
@ChgDeleted{Version=[2],Text=[Any character, including an
@ntf<other_control_function>, is allowed in a comment.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00285-01]}@ChgNote{Moved below}
@ChgDeleted{Version=[2],Text=[Note that this rule doesn't really have much
force, since the implementation can represent characters in the source in any
way it sees fit.
For example, an implementation could simply define that what seems to be
a nongraphic, non-format-effector character is actually a
representation of the space character.]}
@end{Ramification}
@begin(Discussion)
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0266-1]}
It is our intent to follow the terminology of
@Chg{Version=[2],New=[ISO/IEC 10646:@Chg{Version=[3],New=[2011],Old=[2003]}],Old=[ISO 10646 BMP]} where
appropriate, and to remain compatible with the character
classifications defined in @RefSec{Character Handling}.@Chg{Version=[2],
New=[],Old=[Note that our definition for
@nt<graphic_character> is more inclusive than that of ISO 10646-1.]}
@end(Discussion)
@end{Intro}

@begin{Syntax}
@begin{NotIso}
@ChgAdded{Version=[2],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 2 and 3
were deleted.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01]}
@DeletedSyn{Version=[2],lhs=<@Chg{Version=[2],New=<>,Old=<character>}>,
rhs="@Chg{Version=[2],New=<>,Old=<@Syn2{graphic_character} | @Synf{format_effector} | @Synf{other_control_function}>}"}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01]}
@DeletedSyn{Version=[2],lhs=<@Chg{Version=[2],New=<>,Old=<graphic_character>}>,
rhs="@Chg{Version=[2],New=<>,Old=<@Synf{identifier_letter} | @Synf{digit} | @Synf{space_character} | @Synf{special_character}>}"}

@begin{SyntaxText}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0266-1]}
@ChgAdded{Version=[2],Text=[A @ntf{character} is defined by this International
Standard for each cell in the coding space described by ISO/IEC 10646:@Chg{Version=[3],New=[2011],Old=[2003]},
regardless of whether or not ISO/IEC 10646:@Chg{Version=[3],New=[2011],Old=[2003]} allocates a character to that
cell.]}
@end{SyntaxText}
@end{Syntax}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0079-1],ARef=[AI05-0262-1],ARef=[AI05-0266-1]}
The@Chg{Version=[2],New=[],Old=[ character repertoire for the text of
an Ada program consists of the
collection of characters
@Chg{Version=[2],New=[described by the ISO/IEC 10646:@Chg{Version=[3],New=[2011],Old=[2003]}],
Old=[called the Basic Multilingual Plane (BMP) of the
ISO 10646]} Universal Multiple-Octet Coded Character Set, plus a set
of @ntf<format_effector>s and, in comments only,
a set of @ntf<other_control_function>s; the]} coded representation for
@Chg{Version=[2],New=[],Old=[these ]}characters is implementation defined
@Redundant[(it need not be a
representation defined within @Chg{Version=[2],New=[ISO/IEC 10646:@Chg{Version=[3],New=[2011],Old=[2003]}],
Old=[ISO-10646-1]})].@Chg{Version=[2],New=[ A character whose relative
code @Chg{Version=[3],New=[point],Old=[position]} in its plane
is 16#FFFE# or 16#FFFF# is not allowed anywhere
in the text of a program.],Old=[]}@Chg{Version=[3],New=[ The only
characters allowed outside of comments are those in categories
@ntf{other_format}, @ntf{format_effector}, and @ntf{graphic_character}.],Old=[]}
@ImplDef{The coded representation for the text of an Ada program.}

@begin{Ramification}@ChgNote{Moved from above}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[Note that this rule doesn't really have
  much force, since the implementation can represent characters in the
  source in any way it sees fit.
  For example, an implementation could simply define that what seems to be
  an @ntf{other_private_use} character is actually a
  representation of the space character.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0266-1]}
@ChgAdded{Version=[2],Text=[The semantics of an Ada program whose text is not
in Normalization Form KC (as defined by section @Chg{Version=[3],New=[21],Old=[24]}
of ISO/IEC 10646:@Chg{Version=[3],New=[2011],Old=[2003]})
is implementation defined.]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[The
semantics of an Ada program whose text is not in Normalization Form KC.]}]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0266-1]}
The description of the
language definition in this International Standard uses the @Chg{Version=[2],
New=[character properties General Category, Simple Uppercase Mapping,
Uppercase Mapping, and Special Case Condition of the documents referenced by
the note in section 1 of ISO/IEC 10646:@Chg{Version=[3],New=[2011],Old=[2003]}],Old=[graphic symbols
defined for Row 00: Basic Latin and Row 00: Latin-1 Supplement
of the ISO 10646 BMP; these correspond to the graphic symbols of
ISO 8859-1 (Latin-1); no graphic symbols are used in this International Standard for
characters outside of Row 00 of the BMP]}.
The actual set of graphic symbols used by an implementation
for the visual representation of
the text of an Ada program is not specified.
@PDefn{unspecified}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0266-1]}
@Leading@keepnext@;@Chg{Version=[2],New=[Characters],Old=[The categories of
characters]} are @Chg{Version=[2],New=[categorized],Old=[defined]} as follows:
@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1],ARef=[AI05-0262-1],ARef=[AI05-0266-1]}
@ChgAdded{Version=[2],Text=[Our character classification considers that the
cells not allocated in ISO/IEC 10646:@Chg{Version=[3],New=[2011],Old=[2003]} are graphic characters, except for
those whose relative code @Chg{Version=[3],New=[point],Old=[position]}
in their plane is 16#FFFE# or 16#FFFF#. This
seems to provide the best compatibility with future versions of ISO/IEC 10646,
as future characters can @Chg{Version=[3],New=[],Old=[be ]}already be
used in Ada character and string literals.]}
@end{Discussion}
@begin{Description}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00285-01]}
@ChgDeleted{Version=[2],Text=[@Defn{identifier_letter}@ntf<identifier_letter>@\@ntf{upper_case_identifier_letter} | @ntf{lower_case_identifier_letter}]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[2],Text=[We use @ntf<identifier_letter>
  instead of simply @ntf<letter> because
  ISO 10646 BMP includes many other characters that would generally
  be considered "letters."]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@Chg{Version=[2],New=[@Defn{letter_uppercase}@ntf{letter_uppercase}],
Old=[@Defn{upper_case_identifier_letter}@ntf<upper_case_identifier_letter>]}@\Any
character @Chg{Version=[2],New=[whose General Category is defined
to be @lquotes@;Letter, Uppercase@rquotes@;],
Old=[of Row 00 of ISO 10646 BMP whose name begins
@lquotes@;Latin Capital Letter@rquotes@;]}.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@Chg{Version=[2],New=[@Defn{letter_lowercase}@ntf{letter_lowercase}],
Old=[@Defn{lower_case_identifier_letter}@ntf<lower_case_identifier_letter>]}@\Any
character @Chg{Version=[2],New=[whose General Category is defined
to be @lquotes@;Letter, Lowercase@rquotes@;],
Old=[of Row 00 of ISO 10646 BMP whose name begins
@lquotes@;Latin Small Letter@rquotes@;]}.
@begin{Honest}
@ChgRef{Version=[1],Kind=[Deleted],Ref=[8652/0001],ARef=[AI95-00124-01]}
@Comment{The change in clause 1.3 for 8652/0001 handles this problem.}
@ChgDeleted{Version=[1],Text=[The above rules do not include the ligatures
@latin1(198) and @latin1(230).
However, the intent is to include these characters as identifier letters.
This problem was pointed out by a comment from the Netherlands.]}
@end{Honest}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn{letter_titlecase}@ntf{letter_titlecase}@\Any
character whose General Category is defined to be @lquotes@;Letter, Titlecase@rquotes@;.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn{letter_modifier}@ntf{letter_modifier}@\Any
character whose General Category is defined to be @lquotes@;Letter, Modifier@rquotes@;.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn{letter_other}@ntf{letter_other}@\Any
character whose General Category is defined to be @lquotes@;Letter, Other@rquotes@;.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn{mark_non_spacing}@ntf{mark_non_spacing}@\Any
character whose General Category is defined to be @lquotes@;Mark, Non-Spacing@rquotes@;.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn{mark_non_spacing}@ntf{mark_spacing_combining}@\Any
character whose General Category is defined to be @lquotes@;Mark, Spacing Combining@rquotes@;.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@Chg{Version=[2],New=[@Defn{number_decimal}@ntf{number_decimal}],
Old=[@Defn{digit}@nt{digit}]}@\@Chg{Version=[2],New=[Any
character whose General Category is defined
to be @lquotes@;Number, Decimal@rquotes@;],
Old=[One of the characters 0, 1, 2, 3, 4, 5, 6, 7, 8, or 9]}.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn{number_letter}@ntf{number_letter}@\Any
character whose General Category is defined to be @lquotes@;Number, Letter@rquotes@;.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn{punctuation_connector}@ntf{punctuation_connector}@\Any
character whose General Category is defined to be @lquotes@;Punctuation, Connector@rquotes@;.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn{other_format}@ntf{other_format}@\Any
character whose General Category is defined to be @lquotes@;Other, Format@rquotes@;.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@Chg{Version=[2],New=[@Defn{separator_space}@ntf<separator_space>],
Old=[@Defn{space_character}@ntf{space_character}]}@\@Chg{Version=[2],New=[Any
character whose General Category is defined to be @lquotes@;Separator,
Space@rquotes@;.],Old=[The character of ISO 10646 BMP named
@lquotes@;Space@rquotes@;.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@Chg{Version=[2],New=[@Defn{separator_line}@ntf{separator_line}],
Old=[@Defn{special_character}@ntf{special_character}]}@\Any character
@Chg{Version=[2],New=[whose General Category is defined to be
@lquotes@;Separator, Line@rquotes@;.],
Old=[of the ISO 10646 BMP that is not reserved for a control function, and
is not the @ntf<space_character>, an @ntf<identifier_letter>, or a @ntf<digit>.]}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[Note that the no break space and soft hyphen
are @ntf<special_character>s,
and therefore @nt<graphic_character>s.
They are not the same characters as space and hyphen-minus.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn{separator_paragraph}@ntf{separator_paragraph}@\Any
character whose General Category is defined to be @lquotes@;Separator, Paragraph@rquotes@;.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@Defn{format_effector}@ntf<format_effector>@\The
@Chg{Version=[2],New=[characters whose code @Chg{Version=[3],New=[points],Old=[positions]} are
16#09# (CHARACTER TABULATION), 16#0A# (LINE FEED), 16#0B# (LINE TABULATION),
16#0C# (FORM FEED), 16#0D# (CARRIAGE RETURN), 16#85# (NEXT LINE),
and the characters in categories @ntf{separator_line} and
@ntf{separator_paragraph}],
Old=[control functions of ISO 6429 called
  character tabulation (HT), line tabulation (VT), carriage return (CR),
  line feed (LF), and form feed (FF)]}.
@IndexSeeAlso{Term=[control character],See=(format_effector)}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[ISO/IEC 10646:2003 does not define the names
  of control characters, but rather refers to the names defined by
  ISO/IEC 6429:1992. These are the names that we use
  here.@Comment{10646:2011 gives a list of the long names from 6429:1992,
  so I'm not sure the above is true anymore. Best leave the old reference.}]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn{other_control}@ntf{other_control}@\Any
character whose General Category is defined
to be @lquotes@;Other, Control@rquotes@;, and which is not defined to be a
@ntf<format_effector>.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn{other_private_use}@ntf{other_private_use}@\Any
character whose General Category is defined to be @lquotes@;Other, Private Use@rquotes@;.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn{other_surrogate}@ntf{other_surrogate}@\Any
character whose General Category is defined to be @lquotes@;Other, Surrogate@rquotes@;.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@Chg{Version=[2],New=[@Defn{graphic_character}@ntf{graphic_character}],
Old=[@Defn{other_control_function}@ntf<other_control_function>]}@\@Chg{Version=[2],
New=[Any character that is not in the categories @ntf{other_control},
@ntf{other_private_use}, @ntf{other_surrogate},
@ntf{format_effector}, and whose relative code
@Chg{Version=[3],New=[point],Old=[position]} in its plane is neither
16#FFFE# nor 16#FFFF#.],
Old=[Any control function,
other than a @ntf<format_effector>, that is allowed in a comment; the set of
@ntf<other_control_function>s allowed in comments is implementation defined.
@IndexSeeAlso{Term=[control character],See=(other_control_function)}]}
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],Text=[The control functions allowed in comments.]}]}

@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=`We considered basing the definition of
lexical elements on Annex A of ISO/IEC
TR 10176 (4th edition), which lists the characters which should be supported in
identifiers for all programming languages, but we finally decided against this
option. Note that it is not our intent to diverge from ISO/IEC TR 10176,
except to the extent that ISO/IEC TR 10176 itself diverges from ISO/IEC
10646:2003 (which is the case at the time of this writing [January 2005]).'}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[More precisely, we intend to align
strictly with ISO/IEC 10646:2003. It must be
noted that ISO/IEC TR 10176 is a Technical Report while ISO/IEC 10646:2003 is a
Standard. If one has to make a choice, one should conform with the Standard
rather than with the Technical Report. And, it turns out that one @i<must>
make a choice because there are important differences between the two:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[ISO/IEC TR 10176 is still based on
ISO/IEC 10646:2000 while ISO/IEC 10646:2003 has already been published
for a year. We cannot afford to delay
the adoption of our amendment until ISO/IEC TR 10176 has been revised.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[There are considerable differences between the
two editions of ISO/IEC 10646,
notably in supporting characters beyond the BMP (this might be significant for
some languages, e.g. Korean).]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[ISO/IEC TR 10176 does not define case conversion
tables, which are essential
for a case-insensitive language like Ada. To get case conversion tables, we
would have to reference either ISO/IEC 10646:2003 or Unicode, or we would have
to invent our own.]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For the purpose of defining the
lexical elements of the language, we need
character properties like categorization, as well as case conversion tables.
These are mentioned in ISO/IEC 10646:2003 as useful for implementations, with a
reference to Unicode. Machine-readable tables are available on the web at URLs:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@URLLink{URL=[http://www.unicode.org/Public/4.0-Update/UnicodeData-4.0.0.txt],
Text=[http://www.unicode.org/Public/4.0-Update/UnicodeData-4.0.0.txt]}
@URLLink{URL=[http://www.unicode.org/Public/4.0-Update/CaseFolding-4.0.0.txt],
Text=[http://www.unicode.org/Public/4.0-Update/CaseFolding-4.0.0.txt]}]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[with an explanatory document found at URL:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@URLLink{URL=[http://www.unicode.org/Public/4.0-Update/UCD-4.0.0.html],
Text=[http://www.unicode.org/Public/4.0-Update/UCD-4.0.0.html]}]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The actual text of the standard only makes
specific references to the
corresponding clauses of ISO/IEC 10646:2003, not to Unicode.]}

@end{Discussion}
@end{Description}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0266-1]}
@Leading@Chg{Version=[2],New=[],Old=[@Defn{names of @ntf{special_character}s}
@Defn2{Term=[special_character],Sec=(names)}]}The
following names are used when referring to certain
@Chg{Version=[2],New=[characters (the first name is that given in
ISO/IEC 10646:@Chg{Version=[3],New=[2011],Old=[2003]})],Old=[@ntf{special_character}s]}:
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
@Chg{Version=[2],New=[@Defn{exclamation point}
@Defn{percent sign}],
Old=[@Defn{left square bracket}
@Defn{right square bracket}
@Defn{left curly bracket}
@Defn{right curly bracket}]}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0266-1]}
@Chg{Version=[2],New=[@Defn{graphic symbols}@Defn{glyphs}This table
serves to show the correspondence between
ISO/IEC 10646:@Chg{Version=[3],New=[2011],Old=[2003]} names and the graphic symbols (glyphs) used in this
International Standard. These are the characters],
Old=[These are the ones]}
that play a special role in the syntax of Ada@Chg{Version=[2],New=[],
Old=[ 95, or in the syntax rules;
we don't bother to define names for all characters.
The first name given is the name from ISO 10646-1; the subsequent
names, if any, are those used within the
standard, depending on context]}.
@end{Discussion}
@Comment{The original version follows here (commented out)
@begin{Display}
@TabClear()@TabSet(P9)
@begin(TwoCol)
@NoParanum@;symbol @\name

@NoParanum@;  " @\quotation mark
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
@NoParanum@;symbol @\name

@NoParanum@;  : @\colon
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
@end{Display}}
@Comment{We use this (weird) four column version to make the HTML look much better.
WARNING: The items that go together are in separate columns!!}
@begin(FourCol)
@NoParanum@;@ @ @Chg{Version=[2],New=[graphic ],Old=[@ @ @ @ ]}symbol

@NoParanum@;@ @ @ @ @ @ @ @ @ "@*
@ @ @ @ @ @ @ @ @ #@*
@ @ @ @ @ @ @ @ @ &@*
@ @ @ @ @ @ @ @ @ '@*
@ @ @ @ @ @ @ @ @ (@*
@ @ @ @ @ @ @ @ @ )@*
@ @ @ @ @ @ @ @ @ *@*
@ @ @ @ @ @ @ @ @ +@*
@ @ @ @ @ @ @ @ @ ,@*
@ @ @ @ @ @ @ @ @ @en@;@*
@ @ @ @ @ @ @ @ @ .@Chg{Version=[2],New=[],Old=[@*]}@Chg{Version=[2],New=[],Old=[@ @ @ @ @ @ @ @ @ /]}@ChgNote{Funny layout so HTML and RTF look right}
@NewColumn
@NoParanum@;name

@NoParanum@;quotation mark@*
number sign@*
ampersand@*
apostrophe, tick@*
left parenthesis@*
right parenthesis@*
asterisk, multiply@*
plus sign@*
comma@*
hyphen-minus, minus@*
full stop, dot, point@Chg{Version=[2],New=[],Old=[@*]}@Chg{Version=[2],New=[],Old=[solidus, divide]}@ChgNote{Funny layout so HTML and RTF look right}

@NewColumn
@NoParanum@;@ @ @Chg{Version=[2],New=[graphic ],Old=[@ @ @ @ ]}symbol

@NoParanum@;@ @ @ @ @ @ @ @ @ :@*
@ @ @ @ @ @ @ @ @ ;@*
@ @ @ @ @ @ @ @ @ <@*
@ @ @ @ @ @ @ @ @ =@*
@ @ @ @ @ @ @ @ @ >@*
@ @ @ @ @ @ @ @ @ _@*
@ @ @ @ @ @ @ @ @ |@*
@ @ @ @ @ @ @ @ @ @Chg{Version=[2],New=[/],Old=<[>}@*
@ @ @ @ @ @ @ @ @ @Chg{Version=[2],New=[!],Old=<]>}@*
@ @ @ @ @ @ @ @ @ @Chg<Version=[2],New=[%],Old=<{>>@*
@Chg<Version=[2],New=[],Old=<@ @ @ @ @ @ @ @ @ }>>
@NewColumn
@NoParanum@;name

@NoParanum@;colon@*
semicolon@*
less-than sign@*
equals sign@*
greater-than sign@*
low line, underline@*
vertical line@*
@Chg{Version=[2],New=[solidus, divide],Old=[left square bracket]}@*
@Chg{Version=[2],New=[exclamation point],Old=[right square bracket]}@*
@Chg{Version=[2],New=[percent sign],Old=[left curly bracket]}@*
@Chg{Version=[2],New=[],Old=[right curly bracket]}
@end(FourCol)
@end{StaticSem}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0286-1]}
@ChgAdded{Version=[3],Text=[An Ada implementation shall accept Ada source code
in UTF-8 encoding, with or without a BOM (see @RefSecNum{String Encoding}),
where every character is represented by its code point. The character pair
CARRIAGE RETURN/LINE FEED (code points 16#0D# 16#0A#) signifies a single end of
line (see @RefSecNum{Lexical Elements, Separators, and Delimiters}); every other
occurrence of a @ntf{format_effector} other than the character whose code point
position is 16#09# (CHARACTER TABULATION) also signifies a single end of line.]}

@begin{Reason}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0079-1],ARef=[AI05-0286-1]}
@ChgAdded{Version=[3],Text=[This is simply requiring that an Ada implementation
be able to directly process the ACATS, which is provided in the described
format. Note that files that only contain characters with code points in the
first 128 (which is the majority of the ACATS) are represented in the same way
in both UTF-8 and in "plain" string format. The ACATS includes a BOM in files
that have any characters with code points greater than 127. Note that the BOM
contains characters not legal in Ada source code, so an implementation can use
that to automatically distinguish between files formatted as plain Latin-1
strings and UTF-8 with BOM.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[We allow line endings to be both represented as the
pair CR LF (as in Windows and the ACATS), and as single @ntf{format_effector}
characters (usually LF, as in Linux), in order that files created by standard
tools on most operating systems will meet the standard format. We specify how
many line endings each represent so that compilers use the same line numbering
for standard source files.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[This requirement increases portability by having a
format that is accepted by all Ada compilers. Note that implementations can
support other source representations, including structured representations like
a parse tree.]}

@end{Reason}
@end{ImplReq}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0266-1]}
@Chg{Version=[3],New=[The categories defined above, as well as case mapping and
folding, may be based on an implementation-defined version of ISO/IEC 10646
(2003 edition or later).],Old=[@Chg{Version=[2],New=[],Old=[In a nonstandard
mode, the implementation may support a different character
repertoire@Redundant[; in particular,
the set of characters that are considered @ntf<identifier_letter>s
can be extended
or changed to conform to local conventions].]}]}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[If an implementation supports
other character sets,
it defines which characters fall into each category,
such as @lquotes@;@ntf{identifier_letter},@rquotes@;
and what the corresponding rules of this section are,
such as which characters are allowed in the text of a program.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=<The exact categories, case mapping, and case folding chosen
affects identifiers, the result of '[[Wide_]Wide_]Image, and packages
Wide_Characters.Handling and Wide_Wide_Characters.Handling.>}
@end{Ramification}
@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[This permission allows implementations to upgrade
to using a newer character set standard whenever that makes sense, rather
than having to wait for the next Ada Standard.
But the character set standard used cannot be older than ISO/IEC 10646:2003
(which is essentially similar to Unicode 4.0).]}
@end{Discussion}
@end{ImplPerm}

@begin{Notes}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@Chg{Version=[2],New=[The characters in categories @ntf{other_control},
@ntf{other_private_use}, and @ntf{other_surrogate} are only allowed in comments],
Old=[Every code position of ISO 10646 BMP that is not reserved for a control
function is defined to be a @nt<graphic_character> by this International Standard.
This includes all code positions other than 0000 - 001F, 007F - 009F,
and FFFE - FFFF]}.

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0286-1]}
@ChgDeleted{Version=[3],Text=[The language does not specify the source
representation of programs.]}
@begin(Discussion)
@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0286-1]}
@ChgDeleted{Version=[3],Text=[Any source representation is valid so long
as the implementer can produce an (information-preserving)
algorithm for translating both directions
between the representation and the standard character set.
(For example, every character in the standard character set has to be
representable, even if the output devices attached to a given computer
cannot print all of those characters properly.)
From a practical point of view, every implementer will have to provide
some way to process the @Chg{Version=[2],New=[ACATS],Old=[ACVC]}.
It is the intent to allow source representations, such as parse trees,
that are not even linear sequences of characters.
It is also the intent to allow different fonts:
reserved words might be in bold face,
and that should be irrelevant to the semantics.]}
@end(Discussion)
@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
Ada 95 allows 8-bit and 16-bit characters,
as well as implementation-specified character sets.
@end{Extend83}

@begin{DiffWord83}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
The syntax rules in this clause are modified to remove the emphasis
on basic characters vs. others.
(In this day and age, there is no need to point out that you can write
programs without using (for example) lower case letters.)
In particular, @ntf{character} (representing all characters usable outside
comments) is added, and @ntf{basic_graphic_character},
@ntf{other_special_character},
and @ntf{basic_character} are removed.
@ntf{Special_character} is expanded to include Ada 83's
@ntf{other_special_character}, as well as new 8-bit characters not
present in Ada 83.@Chg{Version=[2],New=[ Ada 2005 removes
@ntf{special_character} altogether; we want to stick to ISO/IEC 10646:2003
character classifications.],Old=[]}
Note that the term @lquotes@;basic letter@rquotes@; is used
in @RefSec{Character Handling}
to refer to letters without diacritical marks.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
Character names now come from
@Chg{Version=[2],New=[ISO/IEC 10646:2003],Old=[ISO 10646]}.

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00285-01]}
@ChgDeleted{Version=[2],Text=[We use @ntf<identifier_letter> rather than
@ntf<letter> since ISO 10646 BMP includes many "letters' that are not
permitted in identifiers (in the standard mode).]}
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Program text can use most characters defined by ISO-10646:2003. This
  clause has been rewritten to use the categories defined in that Standard.
  This should ease programming in languages other than English.]}
@end{Extend95}

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0266-1]}
  @ChgAdded{Version=[3],Text=<@Defn{inconsistencies with Ada 2005}An
  implementation is allowed (but not required) to use a newer character
  set standard to determine the categories, case mapping, and case folding.
  Doing so will change the results of attributes '[[Wide_]Wide_]Image and the
  packages [Wide_]Wide_Characters.Handling in the case of a few rarely used
  characters. (This also could make some identifiers illegal, for characters
  that are no longer classified as letters.) This is unlikely to be a problem
  in practice. Moreover, truly portable Ada 2012 programs should avoid using
  in these contexts any characters that would have different classifications in
  any character set standards issued since 10646:2003 (since the compiler can
  use any such standard as the basis for its classifications).>}
@end{Inconsistent2005}

@begin{Diffword2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0079-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified that only
  characters in the categories defined here are allowed in the source
  of an Ada program. This was clear in Ada 95, but Amendment 1 dropped
  the wording instead of correcting it.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0286-1]}
  @ChgAdded{Version=[3],Text=[A standard source representation is defined
  that all compilers are expected to process. Since this is the same format
  as the ACATS, it seems unlikely that there are any implementations that
  don't meet this requirement. Moreover, other representations are still
  permitted, and the "impossible or impractical" loophole (see
  @RefSecnum{Conformity of an Implementation with the Standard}) can be
  invoked for any implementations that cannot directly process the ACATS.]}
@end{Diffword2005}


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

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
The text of a @nt<compilation> is divided into @Defn{line}@i{lines}.
@Defn{end of a line}
In general, the representation for an end of line is implementation defined.
However, a sequence of one or more @ntf<format_effector>s other
than @Chg{Version=[2],New=[the character whose code
@Chg{Version=[3],New=[point],Old=[position]}
is 16#09# (CHARACTER TABULATION)],Old=[character tabulation (HT)]}
signifies at least one end of line.
@ImplDef{The representation for an end of line.}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@Leading@Defn{separator}
@Redundant[In some cases an explicit @i(separator) is required
to separate adjacent lexical elements.]
A separator is
any of a @Chg{Version=[2],New=[@ntf{separator_space}],Old=[space character]},
a @Chg{Version=[2],New=[@ntf{format_effector}],Old=[format effector]},
or the end of a line, as follows:
@begin(Discussion)
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[It might be useful to define @lquotes@;white space@rquotes@;
and use it here.]}@ChgNote{No one can figure out why, and it wouldn't match the
Unicode def of whitespace.}
@end(Discussion)
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
A @Chg{Version=[2],New=[@ntf{separator_space}],Old=[space character]} is
a separator except within a @nt{comment}, a @nt{string_literal},
or a @nt{character_literal}.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@Chg{Version=[2],New=[The character whose code @Chg{Version=[3],New=[point],Old=[position]}
is 16#09# (CHARACTER TABULATION)],Old=[Character tabulation (HT)]} is a
separator except within a @nt{comment}.

The end of a line is always a separator.
@end{Itemize}

One or more separators are allowed between any two adjacent lexical
elements, before the first of each @nt{compilation}, or after the
last.
At least one separator is required between an @nt{identifier},
a reserved word, or a @nt{numeric_literal} and an adjacent
@nt{identifier}, reserved word, or @nt{numeric_literal}.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0079-1]}
@ChgAdded{Version=[3],Text=[One or more @ntf{other_format} characters
are allowed anywhere that a separator is@Redundant[; any such characters
have no effect on the meaning of an Ada program].]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@Defn{delimiter}
A @i{delimiter} is either one of the following@Chg{Version=[2],New=[],Old=[ special]}
characters@Chg{Version=[2],New=[:],Old=[]}
@begin{Display}
&@ @ @ @ '@ @ @ @ (@ @ @ @ )@ @ @ @ *@ @ @ @ +@ @ @ @ ,@ @ @ @ @en@ @ @ @ .@ @ @ @ /@ @ @ @ :@ @ @ @ ;@ @ @ @ <@ @ @ @ =@ @ @ @ >@ @ @ @ |
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

@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}

@Leading@keepnext@;The following names are used when referring to compound
delimiters:@table{Columns=[2],
Alignment=[Allleft],FirstColWidth=[1],LastColWidth=[4],
NoBreak=[F],Border=[F],SmallSize=[F],Caption=[],
Headers=[delimiter@ @\name],
Body=[=>@\arrow
..@\double dot
**@\double star, exponentiate
:=@\assignment (pronounced: @lquotes@;becomes@rquotes@;)
/=@\inequality (pronounced: @lquotes@;not equal@rquotes@;)
>=@\greater than or equal
<=@\less than or equal
<<@\left label bracket
>>@\right label bracket
<>@\box]}
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

@begin{Diffword95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[The wording was updated to use the new character
  categories defined in the preceding clause.]}
@end{Diffword95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0079-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}@b<Correction:>
  Clarified that
  @ntf{other_format} characters are allowed anywhere that separators
  are allowed. This was intended in Ada 2005, but didn't actually
  make it into the wording.]}
@end{Extend2005}



@LabeledClause{Identifiers}

@begin{Intro}
@nt<Identifier>s are used as names.
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@Syn{lhs=<identifier>,rhs="
   @Chg{Version=[2],New=<@Syn2{identifier_start} {@Syn2{identifier_start} | @Syn2{identifier_extend}}>,
   Old=<@Synf{identifier_letter} {[@Synf{underline}] @Syn2{letter_or_digit}}>}"}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@Syn{lhs=<@Chg{Version=[2],New=<identifier_start>,Old=<letter_or_digit>}>,
rhs="@Chg{Version=[2],New=<
     @Synf{letter_uppercase}
   | @Synf{letter_lowercase}
   | @Synf{letter_titlecase}
   | @Synf{letter_modifier}
   | @Synf{letter_other}
   | @Synf{number_letter}>,Old=<@Synf{identifier_letter} | @Synf{digit}>}"}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0091-1]}
@AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=<identifier_extend>,Old=<>}>,
rhs="@Chg{Version=[2],New=<
     @Synf{mark_non_spacing}
   | @Synf{mark_spacing_combining}
   | @Synf{number_decimal}
   | @Synf{punctuation_connector}@Chg{Version=[3],New=<>,Old=<
   | @Synf{other_format}>}>,Old=<>}"}

@begin{SyntaxText}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00395-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0091-1]}
@Chg{Version=[2],New=[@Chg{Version=[3],New=[An],Old=[After eliminating the
characters in category @ntf{other_format}, an]} @nt{identifier} shall not
contain two consecutive characters in category
@Chg{Version=[3],New=[@ntf{punctuation_connector}],Old=[punctuation_connector]},
or end with a character in that category.],Old=[An @nt{identifier} shall not be
a reserved word.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised]}
  @ChgAdded{Version=[2],Text=[This rule was stated in the syntax in Ada 95,
  but that has gotten too complex in Ada 2005.@Chg{Version=[3],New=[],
  Old=[ Since @ntf{other_format}
  characters usually do not display, we do not want to count them as separating
  two underscores.]}]}
@end{Reason}
@end{SyntaxText}
@end{Syntax}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0091-1],ARef=[AI05-0227-1],ARef=[AI05-0266-1]}
@Comment{Removed the "Type=[Leading]" along with the bullets. Don't have
a way to make it doubly conditional (only in Version=[2]), and since it is
mainly for spacing, we just forget it.}
@ChgAdded{Version=[2],Text=[Two @nt{identifier}s are ]}@Chg{Version=[2],
New=[considered the same if they consist of the same sequence of characters
after applying @Chg{Version=[3],New=[locale-independent simple case folding,
as defined by documents referenced in the note in section 1 of
ISO/IEC 10646:2011.@Defn{case insensitive}],Old=[the following transformations
(in this order):]}],Old=[All characters of an @nt{identifier} are significant,
including any underline character.
@Defn{case insensitive}
@nt{Identifier}s differing only in the use of
corresponding upper and lower case letters are considered the same.]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg],ARef=[AI05-0091-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[The characters
in category @ntf{other_format} are eliminated.]}]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg],ARef=[AI05-0091-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[The remaining sequence of
characters is converted to upper case.
@Defn{case insensitive}]}]}
@end{Itemize}
@begin(Discussion)
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0227-1]}
  @Chg{Version=[3],New=[Simple case folding is a mapping to lower case, so this
  is matching the defining (lower case) version of a reserved word. We could
  have mentioned case folding of the reserved words, but as that is an identity
  function, it would have no effect.],Old=[@Chg{Version=[2],New=[],Old=[Two of
  the letters of ISO 8859-1 appear only as lower case,
  "sharp s" and "y with diaeresis." These two letters have
  no corresponding upper case letter (in particular, they
  are not considered equivalent to one another).]}]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0227-1]}
  @ChgAdded{Version=[3],Text=[The @ldquote@;documents referenced@rdquote means
  Unicode. Note that simple case folding is supposed to be compatible between
  Unicode versions, so the Unicode version used doesn't
  matter.]}
@end(Discussion)
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00395-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0091-1],ARef=[AI05-0227-1]}
@ChgAdded{Version=[2],Text=[After applying @Chg{Version=[3],New=[simple
case folding],Old=[these transformations]}, an
@nt{identifier} shall not be identical to a reserved
word@Chg{Version=[3],New=[],Old=[ (in upper case)]}.]}
@begin(ImplNote)
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised]}
  @ChgAdded{Version=[2],Text=[We match the reserved words after
  @Chg{Version=[3],New=[applying case folding],Old=[doing these
  transformations]} so that the rules for @nt{identifier}s and reserved words are
  the same. @Chg{Version=[3],New=[],Old=[(This allows @ntf{other_format}
  characters, which usually don't display, in a reserved word
  without changing it to an @nt{identifier}.) ]}Since
  a compiler usually will lexically process @nt{identifier}s and reserved words the
  same way (often with the same code), this will prevent a lot of headaches.]}
@end(ImplNote)
@begin(Ramification)
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0227-1]}
  @ChgAdded{Version=[2],Text=[The rules for reserved words differ in one way:
  they define case conversion on letters rather than sequences. This means that
  @Chg{Version=[3],New=[it is possible that there exist ],Old=[]}some
  unusual sequences @Chg{Version=[3],New=[that ],Old=[]}are neither
  @nt{identifier}s nor reserved words.
  @Chg{Version=[3],New=[We are not aware of any such sequences so long as we use
  simple case folding (as opposed to full case folding), but we have defined the
  rules in case any are introduced in future character set standards. This
  originally was a problem when converting to upper case:],Old=[For instance,]}
  @lquotes@;@smldotlessi@;f@rquotes@; and
  @lquotes@;acce@latin1(223)@rquotes@; have upper case conversions of
  @lquotes@;IF@rquotes@; and @lquotes@;ACCESS@rquotes@; respectively.
  @Chg{Version=[3],New=[We would not want these to be treated as reserved words.
  But neither of these cases exist when using simple case folding.], Old=[These
  are not @nt{identifier}s, because the transformed values are
  identical to a reserved word. But they are not reserved words, either, because
  the original values do not match any reserved word as defined or with any number
  of characters of the reserved word in upper case. Thus, these odd
  constructions are just illegal, and should not appear in the source of
  a program.]}]}
@end(Ramification)
@end{StaticSem}

@begin{ImplPerm}
In a nonstandard mode,
an implementation may support other upper/lower case equivalence
rules for @nt<identifier>s@Redundant[,
to accommodate local conventions].
@end{ImplPerm}

@begin(Discussion)
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0227-1]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[For instance, in most languages,
  the @Chg{Version=[3],New=[simple case folded],Old=[uppercase]}
  equivalent of LATIN @Chg{Version=[3],New=[CAPITAL],Old=[SMALL]} LETTER I
  (@Chg{Version=[3],New=[an upper],Old=[a lower]} case letter
  @Chg{Version=[3],New=[without],Old=[with]} a dot above) is
  LATIN @Chg{Version=[3],New=[SMALL],Old=[CAPITAL]} LETTER I
  (@Chg{Version=[3],New=[a lower],Old=[an upper]} case letter
  @Chg{Version=[3],New=[with],Old=[without]} a dot above). In
  Turkish, though, LATIN @Chg{Version=[3],New=[CAPITAL],Old=[SMALL]} LETTER I
  and LATIN @Chg{Version=[3],New=[CAPITAL],Old=[SMALL]} LETTER
  @Chg{Version=[3],New=[],Old=[DOTLESS ]}I@Chg{Version=[3],New=[ WITH DOT
  ABOVE],Old=[]} are two distinct letters, so the @Chg{Version=[3],New=[case
  folded],Old=[upper case]} equivalent of LATIN
  @Chg{Version=[3],New=[CAPITAL],Old=[SMALL]} LETTER I is LATIN
  @Chg{Version=[3],New=[SMALL],Old=[CAPITAL]} LETTER
  @Chg{Version=[3],New=[DOTLESS ],Old=[]}I@Chg{Version=[3],New=[],Old=[ WITH DOT
  ABOVE]}, and the @Chg{Version=[3],New=[case folded],Old=[upper case]}
  equivalent of LATIN @Chg{Version=[3],New=[CAPITAL],Old=[SMALL]} LETTER
  @Chg{Version=[3],New=[],Old=[DOTLESS ]}I@Chg{Version=[3],New=[ WITH DOT
  ABOVE],Old=[]} is LATIN @Chg{Version=[3],New=[SMALL],Old=[CAPITAL]} LETTER I.
  Take for instance the following identifier (which is the name of a city on the
  Tigris river in Eastern Anatolia):]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[D@capdottedi@;YARBAKIR],Old=[diyarbak@smldotlessi@;r]} -- @RI[The first i is dotted, the second isn't.]]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[@Chg{Version=[3],New=[A Turkish
  reader would expect that the above identifier is equivalent to],Old=[Locale-independent conversion to
  upper case results in]}:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[diyarbak@smldotlessi@;r],Old=[DIYARBAKIR -- @RI[Both Is are dotless.]]}]}
@end{Example}
  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[However, locale-independent simple case folding (and thus Ada)
  maps this to:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[d@capdottedi@;yarbakir]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[@Chg{Version=[3],New=[which is
  different from any of the following identifiers],Old=[This means that the four
  following sequences of characters represent the same identifier, even though
  for a locutor of Turkish they would probably be considered distinct words]}:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[diyarbakir
diyarbak@smldotlessi@;r
d@smldotlessi@;yarbakir
d@smldotlessi@;yarbak@smldotlessi@;r]}
@end{Example}
  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=<including the @ldquote@;correct@rdquote matching
  identifier for Turkish. Upper case conversion (used in '[Wide_]Wide_Image)
  introduces additional problems.>}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised]}
  @Comment{Removed the "Type=[Leading]" along with the example. Don't have
  a way to make it doubly conditional (only in Version=[2]), and since it is
  mainly for spacing, we just forget it.}
  @ChgAdded{Version=[2],Text=[An implementation targeting the
  Turkish market is allowed (in fact, expected) to provide a nonstandard mode
  where case folding is appropriate for Turkish.@Chg{Version=[3],New=[],Old=[ This
  would cause the original identifier to be converted to:]}]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[D@capdottedi@;YARBAKIR -- @RI[The first I is dotted, the second isn't.]]}]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[and the four
  sequences of characters shown above would represent four distinct
  identifiers.]}]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Lithuanian and Azeri are two other languages that
  present similar idiosyncrasies.]}
@end{Discussion}

@begin{Notes}
  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[@nt{Identifier}s differing only in the use of
  corresponding upper and lower case letters are considered the same.]}
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Examples of identifiers:}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@ChgNote{Old version:@begin{Display}
Count      X    Get_Symbol   Ethelyn   Marion
@Comment{Blank line}
Snobol_4   X1   Page_Count    Store_Next_Item
@end{Display}}
@begin{Example}
Count      X    Get_Symbol   Ethelyn   Marion
Snobol_4   X1   Page_Count   Store_Next_Item@Chg{Version=[2],New=[
@unicode(928)@unicode(955)@unicode(940)@unicode(964)@unicode(969)@unicode(957)      --@RI[ Plato]
@unicode(1063)@unicode(1072)@unicode(1081)@unicode(1082)@unicode(1086)@unicode(1074)@unicode(1089)@unicode(1082)@unicode(1080)@unicode(1081)  --@RI[ Tchaikovsky]
@unicode(952)  @unicode(966)        --@RI[ Angles]],Old=[]}
@end{Example}
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

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  An @nt{identifier} can use any letter defined by ISO-10646:2003, along
  with several other categories. This should ease programming in languages
  other than English.]}
@end{Extend95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0091-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@B<Correction:>
  @ntf{other_format} characters were removed from identifiers as the Unicode
  recommendations have changed. This change can only affect programs
  written for the original Ada 2005, so there should be few such programs.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0227-1]}
  @ChgAdded{Version=[3],Text=[@B<Correction:> We now specify simple case
  folding rather than full case folding. That potentially could change
  identifier equivalence, although it is more likely that identifiers that
  are considered the same in original Ada 2005 will now be considered different.
  This change was made because the original Ada 2005 definition was incompatible
  (and even inconsistent in unusual cases) with the Ada 95 identifier
  equivalence rules. As such, the Ada 2005 rules were rarely fully implemented,
  and in any case, only Ada 2005 identifiers containing wide characters could
  be affected.]}
@end{Incompatible2005}


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


@Syn{lhs=<numeral>,rhs="@Syn2{digit} {[@Synf{underline}] @Syn2{digit}}"}

@Syn{lhs=<exponent>,rhs="E [+] @Syn2{numeral} | E @en@; @Syn2{numeral}"}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=<digit>,Old=<>}>,
rhs="@Chg{Version=[2],New=<0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9>,Old=<>}"}

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
@Leading@keepnext@i{Examples of decimal literals:}
@ChgNote{ Old version: @begin{Display}
@tabclear()@tabset(P31)
12        0      1E6    123_456 @\--@i{  integer literals}
@Comment{Blank line}
12.0      0.0    0.456  3.14159_26 @\--@i{  real literals}
@end{Display}}
@begin{Example}
12        0      1E6    123_456    --@RI{  integer literals}
@Comment{Blank line}
12.0      0.0    0.456  3.14159_26 --@RI{  real literals}
@end{Example}
@end{Examples}

@begin{DiffWord83}
We have changed the syntactic category name @ntf{integer} to be @nt{numeral}.
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
   @Syn2{extended_digit} {[@Synf{underline}] @Syn2{extended_digit}}"}

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
@Leading@keepnext@i{Examples of based literals:}
@ChgNote{ Old version: @begin{Display}
@tabclear()@tabset(P16, P45)
2#1111_1111# @\16#FF#       016#0ff# @\--@i{  integer literals of value 255}
16#E#E1     @\2#1110_0000#     @\--@i{  integer literals of value 224}
16#F.FF#E+2 @\2#1.1111_1111_1110#E11 @\--@i{  real literals of value 4095.0}
@end{Display}}
@begin{Example}
2#1111_1111#  16#FF#       016#0ff#   --@RI{  integer literals of value 255}
16#E#E1       2#1110_0000#            --@RI{  integer literals of value 224}
16#F.FF#E+2   2#1.1111_1111_1110#E11  --@RI{  real literals of value 4095.0}
@end{Example}
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
@Syn{lhs=<character_literal>,rhs="@SingleQuote@Synf{graphic_character}@SingleQuote"}
@end{Syntax}@Comment{graphic_character is defined syntactically in Ada 95, but not in Ada 2007}

@begin{Notes}
A @nt{character_literal} is an enumeration literal
of a character type. See @RefSecNum(Character Types).
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Examples of character literals:}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@ChgNote{ Orginal version: @begin{Display}
'A'@ @ @ @ @ '*'@ @ @ @ @ '''@ @ @ @ @ '@ '
@end{Display}}
@begin{Example}
'A'     '*'     '''     ' '@Chg{Version=[2],New=[
'L'     '@Unicode(1051)'     '@Unicode(923)'    --@RI[ Various els.]
'@Unicode(8734)'     '@Unicode(1488)'            --@RI[ Big numbers - infinity and aleph.]],Old=[]}
@end{Example}
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
string brackets. They are used to represent @nt<operator_symbol>s
(see @RefSecNum(Subprogram Declarations)), values of a string type
(see @RefSecNum(Literals)), and array subaggregates
(see @RefSecNum(Array Aggregates)).
@IndexSee{Term=[quoted string],See=(string_literal)}]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<string_literal>,rhs=<"{@Syn2{string_element}}">}

@Syn{lhs=<string_element>,
  rhs=<"" | @SynI{non_quotation_mark_}@Synf{graphic_character}>}

@begin{SyntaxText}
A @nt{string_element} is either a pair of quotation marks (""),
or a single @ntf{graphic_character} other than a quotation mark.
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

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[No transformation is performed on the
sequence of characters of a @nt{string_literal}.]}
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Examples of string literals:}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@ChgNote< Original version: @begin{Display}
@tabclear()@tabset(P16)
"Message of the day:"
@comment{Blank line}
""   @\--@i{  a null string literal}
"@ "   "A"   """"     @\--@i{  three string literals of length 1}
@comment{Blank line}
"Characters such as $, %, and } are allowed in string literals"
@end{Display}>
@begin{Example}
"Message of the day:"
@comment{Blank line}
""                    --@RI{  a null string literal}
" "   "A"   """"      --@RI{  three string literals of length 1}
@comment{Blank line}
"Characters such as $, %, and } are allowed in string literals"
@Chg{Version=[2],New=["Archimedes said ""@unicode(917)@unicode(973)@unicode(961)@unicode(951)@unicode(954)@unicode(945)"""
"Volume of cylinder (@pi@;r@latin1(178)h) = "],Old=[]}
@end{Example}
@end{Examples}

@begin{DiffWord83}
The wording has been changed to be strictly lexical.
No mention is made of string or character values, since
@nt<string_literal>s are also used to represent @nt<operator_symbol>s,
which don't have a defined value.

The syntax is described differently.
@end{DiffWord83}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[We explicitly say that the characters of a
@nt{string_literal} should be used as is. In particular, no normalization
or folding should be performed on a @nt{string_literal}.]}
@end{DiffWord95}


@LabeledClause{Comments}

@begin{Intro}
A @nt{comment} starts with two adjacent hyphens and extends up to the end
of the line.
@end{Intro}

@begin{Syntax}
@Syn{lhs=<comment>,
  rhs=<--{@SynI{non_end_of_line_}@Synf{character}}>}@Comment{character is defined syntactically in Ada 95,
but not in Ada 2007. We assume the worst here.}

@begin{SyntaxText}
A @nt{comment} may appear on any line of a program.
@end{SyntaxText}
@end{Syntax}

@begin{StaticSem}
The presence or absence of @nt{comment}s has no influence on whether a program
is legal or illegal. Furthermore, @nt{comment}s do not influence the meaning
of a program; their sole purpose is the enlightenment of the human reader.
@end{StaticSem}

@begin{Examples}
@Leading@keepnext@i{Examples of comments:}
@ChgNote{Original version: @begin{Display}
--@i{  the last sentence above echoes the Algol 68 report }
@comment{Blank line}
@key[end];  --@i{  processing of Line is complete }
@comment{Blank line}
--@i{  a long comment may be split onto}
--@i{  two or more consecutive lines   }
@comment{Blank line}
----------------@i{  the first two hyphens start the comment  }
@end{Display}}
@begin{Example}
--@RI[  the last sentence above echoes the Algol 68 report ]
@comment{Blank line}
@key[end];  --@RI[  processing of Line is complete ]
@comment{Blank line}
--@RI[  a long comment may be split onto]
--@RI[  two or more consecutive lines   ]
@comment{Blank line}
----------------@RI[  the first two hyphens start the comment  ]
@end{Example}
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

@begin{MetaRules}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0100-1],ARef=[AI05-0163-1]}
  @ChgAdded{Version=[3],Text=[In general, if all @nt{pragma}s are treated as
  unrecognized @nt{pragma}s, the program should remain both syntactically and
  semantically legal. There are a few exceptions to this general principle (for
  example, @nt{pragma} Import can eliminate the need for a completion), but the
  principle remains, and is strictly true at the syntactic level. Certainly any
  implementation-defined @nt{pragma}s should obey this principle both
  syntactically and semantically, so that if the @nt{pragma}s are not recognized
  by some other implementation, the program will remain legal.]}
@end{MetaRules}

@begin{Syntax}
@Syn{lhs=<pragma>,rhs="
   @key{pragma} @Syn2{identifier} [(@Syn2{pragma_argument_association} {, @Syn2{pragma_argument_association}})];"}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0290-1]}
@Syn{lhs=<pragma_argument_association>,rhs="
     [@SynI{pragma_argument_}@Syn2{identifier} =>] @Syn2{name}
   | [@SynI{pragma_argument_}@Syn2{identifier} =>] @Syn2{expression}@Chg{Version=[3],New=[
   | @SynI{pragma_argument_}@Syn2{aspect_mark} =>  @Syn2{name}
   | @SynI{pragma_argument_}@Syn2{aspect_mark} =>  @Syn2{expression}],Old=[]}"}

@begin{SyntaxText}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0290-1]}
In a @nt<pragma>, any @nt<pragma_argument_association>s without a
@SynI{pragma_argument_}@nt<identifier> @Chg{Version=[3],New=[or
@SynI{pragma_argument_}@nt<aspect_mark> ],Old=[]}shall
precede any associations with a
@i{pragma_argument_}@nt<identifier>@Chg{Version=[3],New=[ or
@SynI{pragma_argument_}@nt<aspect_mark>],Old=[]}.

@Leading@keepnext@nt{Pragma}s are only allowed at the following places in a program:
@begin{Itemize}
After a semicolon delimiter, but not within a @nt{formal_part}
or @nt{discriminant_part}.


@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0100-1],ARef=[AI05-0163-1]}
At any place where the syntax rules allow a construct defined by a
syntactic category whose name ends with @ldquote@ntf{declaration}@rdquote,
@Chg{Version=[3],New=[@ldquote@ntf{item}@rdquote, ],Old=[]}@ldquote@ntf{statement}@rdquote,
@ldquote@ntf{clause}@rdquote, or
@ldquote@ntf{alternative}@rdquote, or one of the syntactic categories
@nt{variant} or @nt{exception_handler};
but not in place of such a construct@Chg{Version=[3],New=[ if the construct
is required, or is part of a list that is required to have at least one such
construct],Old=[. Also at any place where a @nt{compilation_unit} would be
allowed]}.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0163-1]}
@ChgAdded{Version=[3],Text=[In place of a @nt{statement} in a
@nt{sequence_of_statements}.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0100-1]}
@ChgAdded{Version=[3],Text=[At any place where a @nt{compilation_unit}
is allowed.]}
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

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00284-02]}
  @ChgAdded{Version=[2],Text=[For compatibility with Ada 83, the name of a
  @nt{pragma} may also be @lquotes@key{interface}@rquotes@;, which is not an
  identifier (because it is a reserved word).
  See @RefSecNum{Pragma Interface}.]}
@end{Honest}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0272-1]}
@Defn{identifier specific to a pragma}
@Defn{pragma, identifier specific to}
An @i{identifier specific to a pragma} is
an identifier @Chg{Version=[3],New=[or reserved word ],Old=[]}that is
used in a pragma argument with special meaning
for that pragma.
@begin{Honest}
Whenever the syntax rules for a given pragma allow
"@nt{identifier}" as an argument of the @nt{pragma},
that @nt{identifier} is an identifier specific to that
pragma.

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0272-1]}
  @ChgAdded{Version=[3],Text=[In a few cases, a reserved word is
  allowed as "an identifier specific to a pragma". Even in these cases,
  the syntax still is written as @nt{identifier} (the reserved word(s)
  are not shown). For example, the restriction No_Use_Of_Attribute
  (see @RefSecNum{Language-Defined Restrictions and Profiles}) allows the
  reserved words which can be attribute designators, but the syntax for
  a restriction does not include these reserved words.]}
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
other. The reasons for resolving it this way are: The
implementation is simple @em the compiler can just ignore the
@nt{pragma} altogether.
The interpretation of constructs appearing inside
implementation-defined @nt{pragma}s is implementation defined. For
example: @lquotes@;@key[pragma] Mumble(X);@rquotes@;. If the current implementation has
never heard of Mumble, then it doesn't know whether X is a name,
an expression, or an identifier specific to the pragma Mumble.
@end{Ramification}
@begin{Honest}
The syntax of individual pragmas overrides the general
syntax for @nt{pragma}.
@end{Honest}
@begin{Ramification}
Thus, an identifier specific to a @nt{pragma} is not a @nt{name},
syntactically; if it were, the visibility rules would be invoked,
which is not what we want.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
This also implies that named associations do not allow one to give the
arguments in an arbitrary order @em the order given in the syntax rule
for each individual pragma must be obeyed.
However, it is generally possible to leave out earlier arguments when
later ones are given; for example, this is allowed by the syntax rule
for pragma Import (see @Chg{Version=[3],New=[@RefSec{Interfacing Pragmas}],
Old=[@RefSec{Interfacing Aspects}]}).
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
argument in an arbitrary order.@PDefn2{Term=[arbitrary order],Sec=[allowed]}
@begin{Ramification}
For a @nt{pragma} that appears at the place of an elaborable
construct, execution is elaboration.

An identifier specific to a pragma is neither a @nt{name} nor an
@nt{expression} @em such identifiers are not evaluated (unless an
implementation defines them to be evaluated in the case of an
implementation-defined @nt{pragma}).

The @lquotes@;unless otherwise specified@rquotes@; part allows us (and
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
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0163-1]}
Normally,
implementation-defined pragmas should have no semantic effect
for error-free programs;
that is, if the implementation-defined pragmas
@Chg{Version=[3],New=[in a working program are replaced
with unrecognized pragmas],Old=[are removed from a working program]},
the program should still be legal, and should still have the same
semantics.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Implementation-defined pragmas should have no semantic effect
for error-free programs.]}]}
@begin{Ramification}
Note that @lquotes@;semantics@rquotes@; is not the same as @lquotes@;effect;@rquotes@;
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

@Leading@;Normally,
an implementation should not define pragmas that can make an illegal
program legal, except as follows:
@begin(Itemize)
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
  A @nt<pragma> used to complete a
  declaration@Chg{Version=[3],New=[],Old=[, such as a @nt{pragma} Import]};

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[There are no language-defined pragmas which
  can be completions; @nt{pragma} Import was defined this way in Ada 95
  and Ada 2005, but in Ada 2012 @nt{pragma} Import just sets aspect Import
  which disallows having any completion.]}
@end{Discussion}

  A @nt<pragma> used to configure the environment
  by adding, removing, or replacing @nt{library_item}s.
@end(Itemize)
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Implementation-defined pragmas should not make an illegal program
legal, unless they complete a declaration or configure the @nt{library_item}s
in an environment.]}]}
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
@Defn{incompatibilities with Ada 83}
In Ada 83, @lquotes@;bad@rquotes@; @nt{pragma}s are ignored.
In Ada 95, they are illegal,
except in the case where the name of the @nt{pragma} itself
is not recognized by the implementation.
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
Implementation-defined @nt{pragma}s may affect the legality of a program.
@end{Extend83}

@begin{DiffWord83}
Implementation-defined @nt{pragma}s may affect the run-time semantics of
the program.
This was always true in Ada 83 (since it was not explicitly forbidden by
RM83), but it was not clear, because there was no definition of
@lquotes@;executing@rquotes@; or @lquotes@;elaborating@rquotes@; a @nt{pragma}.
@end{DiffWord83}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0163-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}@b<Correction:>
  Allow @nt{pragma}s in place of a @nt{statement}, even if there are no
  other @nt{statement}s in a @nt{sequence_of_statements}.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0272-1]}
  @ChgAdded{Version=[3],Text=[Identifiers specific to a pragma can be
  reserved words.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0290-1]}
  @ChgAdded{Version=[3],Text=[Pragma arguments can be identified with
  @nt{aspect_mark}s; this allows @nt{identifier}'Class in this context.
  As usual, this is only allowed if specifically allowed by a particular
  pragma.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0100-1]}
  @ChgAdded{Version=[3],Text=[@b{Correction:} Clarified where @nt{pragma}s
  are (and are not) allowed.]}
@end{DiffWord2005}


@begin{Syntax}
@Leading@keepnext@;The forms of List, Page, and Optimize @nt{pragma}s are as follows:

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
although @lquotes@;supporting@rquotes@; some of
them (for example, Inline) requires nothing more than checking the
arguments, since they act only as advice to the implementation.
@end{Ramification}

@end{SyntaxText}
@end{Syntax}

@begin{StaticSem}
A @nt{pragma} List
takes one of the @nt{identifier}s On or Off as the single
argument. This pragma is allowed anywhere a
@nt{pragma} is allowed. It specifies that listing of the
compilation is to be continued or suspended until a List
@nt{pragma} with the opposite argument is given within the
same compilation. The @nt{pragma} itself is always listed if
the compiler is producing a listing.

A @nt{pragma} Page
is allowed anywhere a
@nt{pragma} is allowed. It specifies that the program text
which follows the @nt{pragma} should start on a new page (if
the compiler is currently producing a listing).

A @nt{pragma} Optimize
takes one of the @nt{identifier}s Time, Space, or Off as the
single argument. This @nt{pragma} is allowed anywhere a @nt<pragma>
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


We don't define what constitutes an @lquotes@;optimization@rquotes@;
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
@Leading@keepnext@i{Examples of pragmas:}
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@key[pragma] List(Off); -- @Examcom{turn off listing generation}
@key[pragma] Optimize(Off); -- @Examcom{turn off optional optimizations}
@Chg{Version=[3],New=[@key[pragma] Pure(Rational_Numbers); -- @Examcom{set categorization for package}
@key[pragma] Assert(Exists(File_Name),
              Message => "Nonexistent file"); -- @Examcom{assert file exists}],
Old=[@key[pragma] Inline(Set_Mask); --@Examcom{ generate code for Set_Mask inline}
@Chg{Version=[2],New=[@key[pragma] Import(C, Put_Char, External_Name => "putchar"); --@Examcom{ import C putchar function}],
Old=[@key[pragma] Suppress(Range_Check, On => Index); -- @Examcom{turn off range checking on Index}]}]}
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
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

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
  @ChgAdded{Version=[2],Text=[Updated the example of named pragma parameters,
  because the second parameter of @nt{pragma} Suppress is obsolescent.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[Updated the example of pragmas, because both
  @nt{pragma}s Inline and Import are obsolescent.]}
@end{DiffWord2005}


@LabeledClause{Reserved Words}

@begin{Syntax}
@begin{Bundle}
@ChgRef{Version=[1], Kind=[Deleted]}
@ChgDeleted[Version=[1],Type=[Leading],Text=<@ @;@comment{Empty paragraph to hang junk paragraph number from original RM}>]

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00284-02],ARef=[AI95-00395-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0091-1]}
@ChgNote{The table of words has no paragraph number, so we need to put the
change here}
@Leading
@Defn{reserved word}
The following are the @i{reserved words}@Chg{Version=[2],New=[. Within a program,
some or all of the letters of a reserved word may be in
upper case@Chg{Version=[3],New=[],Old=[, and one or
more characters in category @ntf{other_format} may be inserted within or at the
end of the reserved word]}.],Old=[ (ignoring upper/lower case distinctions):]}
@begin{Discussion}
  Reserved words have special meaning in the syntax.
  In addition, certain reserved words are used as attribute names.

  The syntactic category @nt{identifier}
  no longer allows reserved words. We have added the few reserved
  words that are legal explicitly to the syntax for @nt{attribute_reference}.
  Allowing identifier to include reserved words has been a source
  of confusion for some users, and differs from the way they
  are treated in the C and Pascal language definitions.
@end{Discussion}
@*

@begin{Display}
@begin{FourCol}
@noparanum@key{abort}
@key{abs}
@key{abstract}
@key{accept}
@key{access}
@key{aliased}
@key{all}
@key{and}
@key{array}
@key{at}

@noparanum@key{begin}
@key{body}

@noparanum@key{case}
@key{constant}

@noparanum@key{declare}
@key{delay}
@key{delta}
@key{digits}
@key{do}
@NewColumn

@noparanum@key{else}
@key{elsif}
@key{end}
@key{entry}
@key{exception}
@key{exit}

@noparanum@key{for}
@key{function}

@noparanum@key{generic}
@key{goto}

@noparanum@key{if}
@key{in}
@Chg{Version=[2],New=[@key{interface}],Old=[]}
@key{is}


@noparanum@key{limited}
@key{loop}

@noparanum@key{mod}
@NewColumn

@noparanum@key{new}
@key{not}
@key{null}


@noparanum@key{of}
@key{or}
@key{others}
@key{out}
@Chg{Version=[2],New=[@key{overriding}],Old=[]}

@noparanum@key{package}
@key{pragma}
@key{private}
@key{procedure}
@key{protected}

@noparanum@key{raise}
@key{range}
@key{record}
@key{rem}
@key{renames}
@key{requeue}
@NewColumn

@noparanum@key{return}
@key{reverse}

@noparanum@key{select}
@key{separate}
@Chg{Version=[3],New=[@key{some}],Old=[]}
@key{subtype}
@Chg{Version=[2],New=[@key{synchronized}],Old=[]}

@noparanum@key{tagged}
@key{task}
@key{terminate}
@key{then}
@key{type}

@noparanum@key{until}
@key{use}

@noparanum@key{when}
@key{while}
@key{with}

@noparanum@key{xor}
@end{FourCol}
@end{Display}
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
@Defn{incompatibilities with Ada 83}
The following words are not reserved in Ada 83, but are reserved in Ada
95: @key{abstract}, @key{aliased}, @key{protected}, @key{requeue},
@key{tagged}, @key{until}.
@end{Incompatible83}

@begin{DiffWord83}
The clause entitled @lquotes@;Allowed Replacements of Characters@rquotes@; has been moved
to @RefSec(Obsolescent Features).
@end{DiffWord83}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00284-02]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  The following words are not reserved in Ada 95, but are reserved in Ada
  2005: @key{interface}, @key{overriding}, @key{synchronized}. A special
  allowance is made for @key{pragma} Interface (see @RefSecNum{Pragma Interface}).
  Uses of these words as identifiers will need to be changed, but we do not
  expect them to be common.]}
@end{Incompatible95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
  @ChgAdded{Version=[2],Text=[The definition of upper case equivalence has
  been modified to allow identifiers using all of the characters of ISO 10646.
  This change has no effect on the character sequences that are reserved
  words, but does make some unusual sequences of characters illegal.]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0091-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Removed @ntf{other_format}
  characters from reserved words in order to be compatible with the latest
  Unicode recommendations. This change can only affect programs
  written for original Ada 2005, and there is little reason to put
  @ntf{other_format} characters into reserved words in the first place,
  so there should be very few such programs.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0176-1]}
  @ChgAdded{Version=[3],Text=[The following word is not reserved in Ada 2005, but is
  reserved in Ada 2012: @key{some}. Uses of this word as an identifier will need
  to be changed, but we do not expect them to be common.]}
@end{Incompatible2005}

