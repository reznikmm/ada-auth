@comment{ $Source: e:\\cvsroot/ARM/Source/pre_chars.mss,v $ }
@comment{ $Revision: 1.12 $ $Date: 2000/05/19 04:12:07 $ $Author: Randy $ }
@Part(predefchars, Root="ada.mss")

@Comment{$Date: 2000/05/19 04:12:07 $}

@LabeledClause{Character Handling}
@begin{Intro}
This clause presents the packages related to character processing:
an empty pure package Characters and child packages
Characters.Handling and Characters.Latin_1.
The package Characters.Handling
provides classification and conversion
functions for Character data, and some simple functions for
dealing with Wide_Character data.
The child package Characters.Latin_1 declares a set of
constants initialized to values of type Character.
@end{Intro}

@begin{Extend83}
This clause is new to Ada 9X.
@end{Extend83}

@LabeledSubClause(The Package Characters)

@begin{StaticSem}
The library package Characters has the following declaration:
@begin{example}
@ChildUnit{Parent=[Ada],Child=[Characters],Expanded=[Ada.Characters]}
@key(package) Ada.Characters @key[is]
  @key[pragma] Pure(Characters);
@key(end) Ada.Characters;
@end{example}

@end{StaticSem}

@LabeledSubClause{The Package Characters.Handling}
@begin{StaticSem}
The library package Characters.Handling has the following declaration:
@begin{example}
@key[package] Ada.Characters.Handling @key[is]
@ChildUnit{Parent=[Ada.Characters],Child=[Handling],Expanded=[Ada.Characters.Handling]}
  @key[pragma] Preelaborate(Handling);

--@i{Character classification functions}

  @key[function] Is_Control           (Item : @key[in] Character) @key[return] Boolean;
  @key[function] Is_Graphic           (Item : @key[in] Character) @key[return] Boolean;
  @key[function] Is_Letter            (Item : @key[in] Character) @key[return] Boolean;
  @key[function] Is_Lower             (Item : @key[in] Character) @key[return] Boolean;
  @key[function] Is_Upper             (Item : @key[in] Character) @key[return] Boolean;
  @key[function] Is_Basic             (Item : @key[in] Character) @key[return] Boolean;
  @key[function] Is_Digit             (Item : @key[in] Character) @key[return] Boolean;
  @key[function] Is_Decimal_Digit     (Item : @key[in] Character) @key[return] Boolean @key[renames] Is_Digit;
  @key[function] Is_Hexadecimal_Digit (Item : @key[in] Character) @key[return] Boolean;
  @key[function] Is_Alphanumeric      (Item : @key[in] Character) @key[return] Boolean;
  @key[function] Is_Special           (Item : @key[in] Character) @key[return] Boolean;


--@i{Conversion functions for Character and String}

  @key[function] To_Lower (Item : @key[in] Character) @key[return] Character;
  @key[function] To_Upper (Item : @key[in] Character) @key[return] Character;
  @key[function] To_Basic (Item : @key[in] Character) @key[return] Character;

  @key[function] To_Lower (Item : @key[in] String) @key[return] String;
  @key[function] To_Upper (Item : @key[in] String) @key[return] String;
  @key[function] To_Basic (Item : @key[in] String) @key[return] String;


--@i{Classifications of and conversions between Character and ISO 646}

  @key[subtype] ISO_646 @key[is]
    Character @key[range] Character'Val(0) .. Character'Val(127);

  @key[function] Is_ISO_646 (Item : @key[in] Character) @key[return] Boolean;
  @key[function] Is_ISO_646 (Item : @key[in] String)    @key[return] Boolean;

  @key[function] To_ISO_646 (Item       : @key[in] Character;
                       Substitute : @key[in] ISO_646 := ' ')
    @key[return] ISO_646;

  @key[function] To_ISO_646 (Item       : @key[in] String;
                       Substitute : @key[in] ISO_646 := ' ')
    @key[return] String;


--@i{Classifications of and conversions between Wide_Character and Character.}

  @key[function] Is_Character (Item : @key[in] Wide_Character) @key[return] Boolean;
  @key[function] Is_String    (Item : @key[in] Wide_String)    @key[return] Boolean;


  @key[function] To_Character (Item       : @key[in] Wide_Character;
                         Substitute : @key[in] Character := ' ')
    @key[return] Character;

  @key[function] To_String    (Item       : @key[in] Wide_String;
                         Substitute : @key[in] Character := ' ')
    @key[return] String;


  @key[function] To_Wide_Character (Item : @key[in] Character) @key[return] Wide_Character;

  @key[function] To_Wide_String    (Item : @key[in] String)    @key[return] Wide_String;

@key[end] Ada.Characters.Handling;
@end{Example}

In the description below for each function that returns a Boolean
result, the effect is described in terms of the conditions under which
the value True is returned.  If these conditions are not met, then the
function returns False.

Each of the following classification functions has a formal Character
parameter, Item, and returns a Boolean result.
@begin{description}
@Defn2{term=[control character], sec=[a category of Character]}Is_Control @\True if Item is a control character.
A @i{control character} is a character whose position is
in one of the ranges 0..31 or 127..159.

@Defn2{term=[graphic character], sec=[a category of Character]}Is_Graphic @\True if Item is a graphic character.  A @i[graphic character]
is a character whose position is in one of the ranges
32..126 or 160..255.

@Defn2{term=[letter], sec=[a category of Character]}Is_Letter @\True if Item is a letter.
A @i[letter] is a character that is in one of the ranges
'A'..'Z' or 'a'..'z', or whose position is in one of the ranges
192..214, 216..246, or 248..255.

@Defn2{term=[lower-case letter], sec=[a category of Character]}Is_Lower @\True if Item is a lower-case letter.
A @i[lower-case letter] is a character that is in
the range 'a'..'z', or whose position is in one of the ranges
223..246 or 248..255.

@Defn2{term=[upper-case letter], sec=[a category of Character]}Is_Upper@\True if Item is an upper-case letter.
An @i[upper-case letter] is a character that is in the range
'A'..'Z' or whose position is in one of the ranges
192..214 or 216.. 222.

@Defn2{term=[basic letter], sec=[a category of Character]}Is_Basic @\True if Item is a basic letter.
A @i[basic letter] is a character that is in one of the
ranges 'A'..'Z' and 'a'..'z', or that is one of the
following:
'@latin1(198)', '@latin1(230)', '@latin1(208)', '@latin1(240)', '@latin1(222)', '@latin1(254)', or '@latin1(223)'.

@comment[These characters are at the positions
198 and 230, 208 and 240, 222 and 254, and 223.]

@Defn2{term=[decimal digit], sec=[a category of Character]}Is_Digit @\True if Item is a decimal digit.
A @i[decimal digit] is a character in the range '0'..'9'.

Is_Decimal_Digit @\A renaming of Is_Digit.

@Defn2{term=[hexadecimal digit], sec=[a category of Character]}Is_Hexadecimal_Digit @\True if Item is a hexadecimal digit.
A @i[hexadecimal digit] is a character that is either a
decimal digit or that is in one of the ranges 'A' .. 'F' or 'a' .. 'f'.

@Defn2{term=[alphanumeric character], sec=[a category of Character]}Is_Alphanumeric @\True if Item is an alphanumeric character.
An @i[alphanumeric character] is a character that is either
a letter or a  decimal digit.

@Defn2{term=[special graphic character], sec=[a category of Character]}Is_Special @\True if Item is a special graphic character.
A @i[special graphic character] is a  graphic character that is
not alphanumeric.
@end{description}

Each of the names
To_Lower, To_Upper, and To_Basic refers to two
functions: one that converts from Character to Character, and the
other that converts from String to String.  The result of each
Character-to-Character function is described below, in terms of the
conversion applied to Item, its formal Character parameter.  The
result of each  String-to-String conversion is  obtained by applying
to each element of the function's
 String parameter the corresponding Character-to-Character conversion;
the result is the null String if the value of the formal parameter
is the null String.
The lower bound of the result String is 1.
@begin{description}
To_Lower@\Returns the corresponding lower-case value for Item if
Is_Upper(Item), and returns Item otherwise.

To_Upper@\Returns the corresponding upper-case value for Item if
Is_Lower(Item) and Item has an upper-case form, and returns Item otherwise.
The lower case letters
'@latin1(223)' and '@latin1(255)'
@comment{These are at positions 223 and 255}
do not have upper case forms.

To_Basic@\Returns the letter corresponding to Item
but with no diacritical mark,
if Item is a letter but not a basic letter;
returns Item otherwise.
@end{Description}

The following set of functions test for membership in the ISO 646
character range, or convert between ISO 646 and Character.
@begin{description}
Is_ISO_646@\The function whose formal parameter, Item, is of type
Character returns True if Item is in the subtype ISO_646.

Is_ISO_646@\The function whose formal parameter, Item, is of type
String returns True if Is_ISO_646(Item(I)) is True for each I in
Item'Range.

To_ISO_646@\The function whose first formal parameter, Item, is of type
Character returns Item if Is_ISO_646(Item), and returns the Substitute
ISO_646 character otherwise.

To_ISO_646@\The function whose first formal parameter, Item, is of type
String returns the String whose Range is 1..Item'Length and each of
whose elements is given by To_ISO_646 of the corresponding element in
Item.
@end{description}

The following set of functions test Wide_Character values
for membership in Character,
or convert between corresponding characters of
Wide_Character and Character.
@begin{description}
Is_Character@\Returns True if Wide_Character'Pos(Item) <=
Character'Pos(Character'Last).

Is_String@\Returns True if Is_Character(Item(I)) is True for each I in
Item'Range.

To_Character@\Returns the Character corresponding to
Item if Is_Character(Item), and returns the Substitute
Character otherwise.

To_String@\Returns the String whose range is 1..Item'Length and each of
whose elements is given by To_Character of the corresponding element in
Item.

To_Wide_Character@\Returns the Wide_Character X such that
Character'Pos(Item) = Wide_Character'Pos(X).

To_Wide_String@\Returns the Wide_String whose range is 1..Item'Length and each of
whose elements is given by To_Wide_Character of the corresponding element in
Item.
@end{description}
@end{StaticSem}

@begin{ImplAdvice}
If an implementation provides a localized definition of Character
or Wide_Character,
then the effects of the subprograms in Characters.Handling should
reflect the  localizations.  See also @RefSecNum(Character Types).
@end{ImplAdvice}

@begin{Notes}
 A basic letter is a letter without a diacritical mark.

Except for the hexadecimal digits, basic letters, and ISO_646
characters, the categories identified in the classification functions
form a strict hierarchy:
@begin{itemize}
Control characters

Graphic characters
@begin{itemize}
Alphanumeric characters
@begin{Itemize}
Letters
@begin{itemize}
Upper-case letters

Lower-case letters
@end{Itemize}

Decimal digits
@end{Itemize}

Special graphic characters
@end{Itemize}
@end{Itemize}
@begin{Ramification}
Thus each Character value is either a control character or
a graphic character but not both; each graphic character is either
an alphanumeric or special graphic but not both; each alphanumeric
is either a letter or decimal digit but not both; each letter is
either upper case or lower case but not both.@end{ramification}
@end{Notes}

@LabeledSubClause{The Package Characters.Latin_1}
@begin{Intro}
The package Characters.Latin_1 declares constants for
characters in ISO 8859-1.
@begin{reason}
The constants for the ISO 646 characters could have
been declared as renamings of objects declared in package ASCII, as
opposed to explicit constants.  The main reason for explicit constants
was for consistency of style with the upper-half constants, and to avoid
emphasizing the package ASCII.@end{reason}
@end{Intro}

@begin{StaticSem}
The library package Characters.Latin_1 has the following
declaration:
@begin{Example}
@ChildUnit{Parent=[Ada.Characters],Child=[Latin_1],Expanded=[Ada.Characters.Latin_1]}
@key[package] Ada.Characters.Latin_1 @key[is]
    @key[pragma] Pure(Latin_1);

@i{-- Control characters:}@PDefn2{term=[control character],
sec=[a category of Character]}

    NUL                  : @key[constant] Character := Character'Val(0);
    SOH                  : @key[constant] Character := Character'Val(1);
    STX                  : @key[constant] Character := Character'Val(2);
    ETX                  : @key[constant] Character := Character'Val(3);
    EOT                  : @key[constant] Character := Character'Val(4);
    ENQ                  : @key[constant] Character := Character'Val(5);
    ACK                  : @key[constant] Character := Character'Val(6);
    BEL                  : @key[constant] Character := Character'Val(7);
    BS                   : @key[constant] Character := Character'Val(8);
    HT                   : @key[constant] Character := Character'Val(9);
    LF                   : @key[constant] Character := Character'Val(10);
    VT                   : @key[constant] Character := Character'Val(11);
    FF                   : @key[constant] Character := Character'Val(12);
    CR                   : @key[constant] Character := Character'Val(13);
    SO                   : @key[constant] Character := Character'Val(14);
    SI                   : @key[constant] Character := Character'Val(15);

    DLE                  : @key[constant] Character := Character'Val(16);
    DC1                  : @key[constant] Character := Character'Val(17);
    DC2                  : @key[constant] Character := Character'Val(18);
    DC3                  : @key[constant] Character := Character'Val(19);
    DC4                  : @key[constant] Character := Character'Val(20);
    NAK                  : @key[constant] Character := Character'Val(21);
    SYN                  : @key[constant] Character := Character'Val(22);
    ETB                  : @key[constant] Character := Character'Val(23);
    CAN                  : @key[constant] Character := Character'Val(24);
    EM                   : @key[constant] Character := Character'Val(25);
    SUB                  : @key[constant] Character := Character'Val(26);
    ESC                  : @key[constant] Character := Character'Val(27);
    FS                   : @key[constant] Character := Character'Val(28);
    GS                   : @key[constant] Character := Character'Val(29);
    RS                   : @key[constant] Character := Character'Val(30);
    US                   : @key[constant] Character := Character'Val(31);

@i{-- ISO 646 graphic characters:}

    Space                : @key[constant] Character := ' ';  @i{-- Character'Val(32)}
    Exclamation          : @key[constant] Character := '!';  @i{-- Character'Val(33)}
    Quotation            : @key[constant] Character := '"';  @i{-- Character'Val(34)}
    Number_Sign          : @key[constant] Character := '#';  @i{-- Character'Val(35)}
    Dollar_Sign          : @key[constant] Character := '$';  @i{-- Character'Val(36)}
    Percent_Sign         : @key[constant] Character := '%';  @i{-- Character'Val(37)}
    Ampersand            : @key[constant] Character := '&';  @i{-- Character'Val(38)}
    Apostrophe           : @key[constant] Character := ''';  @i{-- Character'Val(39)}
    Left_Parenthesis     : @key[constant] Character := '(';  @i{-- Character'Val(40)}
    Right_Parenthesis    : @key[constant] Character := ')';  @i{-- Character'Val(41)}
    Asterisk             : @key[constant] Character := '*';  @i{-- Character'Val(42)}
    Plus_Sign            : @key[constant] Character := '+';  @i{-- Character'Val(43)}
    Comma                : @key[constant] Character := ',';  @i{-- Character'Val(44)}
    Hyphen               : @key[constant] Character := '-';  @i{-- Character'Val(45)}
    Minus_Sign           : Character @key[renames] Hyphen;
    Full_Stop            : @key[constant] Character := '.';  @i{-- Character'Val(46)}
    Solidus              : @key[constant] Character := '/';  @i{-- Character'Val(47)}

    @i{-- Decimal digits '0' though '9' are at positions 48 through 57}

    Colon                : @key[constant] Character := ':';  @i{-- Character'Val(58)}
    Semicolon            : @key[constant] Character := ';';  @i{-- Character'Val(59)}
    Less_Than_Sign       : @key[constant] Character := '<';  @i{-- Character'Val(60)}
    Equals_Sign          : @key[constant] Character := '=';  @i{-- Character'Val(61)}
    Greater_Than_Sign    : @key[constant] Character := '>';  @i{-- Character'Val(62)}
    Question             : @key[constant] Character := '?';  @i{-- Character'Val(63)}
    Commercial_At        : @key[constant] Character := '@@';  @i{-- Character'Val(64)}

    @i{-- Letters 'A' through 'Z' are at positions 65 through 90}

    Left_Square_Bracket  : @key[constant] Character := '[';  @i{-- Character'Val(91)}
    Reverse_Solidus      : @key[constant] Character := '\';  @i{-- Character'Val(92)}
    Right_Square_Bracket : @key[constant] Character := ']';  @i{-- Character'Val(93)}
    Circumflex           : @key[constant] Character := '^';  @i{-- Character'Val(94)}
    Low_Line             : @key[constant] Character := '_';  @i{-- Character'Val(95)}

    Grave                : @key[constant] Character := '`';  @i{-- Character'Val(96)}
    LC_A                 : @key[constant] Character := 'a';  @i{-- Character'Val(97)}
    LC_B                 : @key[constant] Character := 'b';  @i{-- Character'Val(98)}
    LC_C                 : @key[constant] Character := 'c';  @i{-- Character'Val(99)}
    LC_D                 : @key[constant] Character := 'd';  @i{-- Character'Val(100)}
    LC_E                 : @key[constant] Character := 'e';  @i{-- Character'Val(101)}
    LC_F                 : @key[constant] Character := 'f';  @i{-- Character'Val(102)}
    LC_G                 : @key[constant] Character := 'g';  @i{-- Character'Val(103)}
    LC_H                 : @key[constant] Character := 'h';  @i{-- Character'Val(104)}
    LC_I                 : @key[constant] Character := 'i';  @i{-- Character'Val(105)}
    LC_J                 : @key[constant] Character := 'j';  @i{-- Character'Val(106)}
    LC_K                 : @key[constant] Character := 'k';  @i{-- Character'Val(107)}
    LC_L                 : @key[constant] Character := 'l';  @i{-- Character'Val(108)}
    LC_M                 : @key[constant] Character := 'm';  @i{-- Character'Val(109)}
    LC_N                 : @key[constant] Character := 'n';  @i{-- Character'Val(110)}
    LC_O                 : @key[constant] Character := 'o';  @i{-- Character'Val(111)}

    LC_P                 : @key[constant] Character := 'p';  @i{-- Character'Val(112)}
    LC_Q                 : @key[constant] Character := 'q';  @i{-- Character'Val(113)}
    LC_R                 : @key[constant] Character := 'r';  @i{-- Character'Val(114)}
    LC_S                 : @key[constant] Character := 's';  @i{-- Character'Val(115)}
    LC_T                 : @key[constant] Character := 't';  @i{-- Character'Val(116)}
    LC_U                 : @key[constant] Character := 'u';  @i{-- Character'Val(117)}
    LC_V                 : @key[constant] Character := 'v';  @i{-- Character'Val(118)}
    LC_W                 : @key[constant] Character := 'w';  @i{-- Character'Val(119)}
    LC_X                 : @key[constant] Character := 'x';  @i{-- Character'Val(120)}
    LC_Y                 : @key[constant] Character := 'y';  @i{-- Character'Val(121)}
    LC_Z                 : @key[constant] Character := 'z';  @i{-- Character'Val(122)}
    Left_Curly_Bracket   : @key[constant] Character := '{';  @i{-- Character'Val(123)}
    Vertical_Line        : @key[constant] Character := '|';  @i{-- Character'Val(124)}
    Right_Curly_Bracket  : @key[constant] Character := '}';  @i{-- Character'Val(125)}
    Tilde                : @key[constant] Character := '~';  @i{-- Character'Val(126)}
    DEL                  : @key[constant] Character := Character'Val(127);


@i{-- ISO 6429 control characters:}@PDefn2{term=[control character],
sec=[a category of Character]}

    IS4                  : Character @key[renames] FS;
    IS3                  : Character @key[renames] GS;
    IS2                  : Character @key[renames] RS;
    IS1                  : Character @key[renames] US;

    Reserved_128         : @key[constant] Character := Character'Val(128);
    Reserved_129         : @key[constant] Character := Character'Val(129);
    BPH                  : @key[constant] Character := Character'Val(130);
    NBH                  : @key[constant] Character := Character'Val(131);
    Reserved_132         : @key[constant] Character := Character'Val(132);
    NEL                  : @key[constant] Character := Character'Val(133);
    SSA                  : @key[constant] Character := Character'Val(134);
    ESA                  : @key[constant] Character := Character'Val(135);
    HTS                  : @key[constant] Character := Character'Val(136);
    HTJ                  : @key[constant] Character := Character'Val(137);
    VTS                  : @key[constant] Character := Character'Val(138);
    PLD                  : @key[constant] Character := Character'Val(139);
    PLU                  : @key[constant] Character := Character'Val(140);
    RI                   : @key[constant] Character := Character'Val(141);
    SS2                  : @key[constant] Character := Character'Val(142);
    SS3                  : @key[constant] Character := Character'Val(143);

    DCS                  : @key[constant] Character := Character'Val(144);
    PU1                  : @key[constant] Character := Character'Val(145);
    PU2                  : @key[constant] Character := Character'Val(146);
    STS                  : @key[constant] Character := Character'Val(147);
    CCH                  : @key[constant] Character := Character'Val(148);
    MW                   : @key[constant] Character := Character'Val(149);
    SPA                  : @key[constant] Character := Character'Val(150);
    EPA                  : @key[constant] Character := Character'Val(151);

    SOS                  : @key[constant] Character := Character'Val(152);
    Reserved_153         : @key[constant] Character := Character'Val(153);
    SCI                  : @key[constant] Character := Character'Val(154);
    CSI                  : @key[constant] Character := Character'Val(155);
    ST                   : @key[constant] Character := Character'Val(156);
    OSC                  : @key[constant] Character := Character'Val(157);
    PM                   : @key[constant] Character := Character'Val(158);
    APC                  : @key[constant] Character := Character'Val(159);

@i{-- Other graphic characters:}

@i{-- Character positions 160 (16#A0#) .. 175 (16#AF#):}
    No_Break_Space              : @key[constant] Character := ' ';  @i{--Character'Val(160)}
    NBSP                        : Character @key[renames] No_Break_Space;
    Inverted_Exclamation        : @key[constant] Character := '@latin1(161)';  @i{--Character'Val(161)}
    Cent_Sign                   : @key[constant] Character := '@latin1(162)';  @i{--Character'Val(162)}
    Pound_Sign                  : @key[constant] Character := '@latin1(163)';  @i{--Character'Val(163)}
    Currency_Sign               : @key[constant] Character := '@latin1(164)';  @i{--Character'Val(164)}
    Yen_Sign                    : @key[constant] Character := '@latin1(165)';  @i{--Character'Val(165)}
    Broken_Bar                  : @key[constant] Character := '@latin1(166)';  @i{--Character'Val(166)}
    Section_Sign                : @key[constant] Character := '@latin1(167)';  @i{--Character'Val(167)}
    Diaeresis                   : @key[constant] Character := '@latin1(168)';  @i{--Character'Val(168)}
    Copyright_Sign              : @key[constant] Character := '@latin1(169)';  @i{--Character'Val(169)}
    Feminine_Ordinal_Indicator  : @key[constant] Character := '@latin1(170)';  @i{--Character'Val(170)}
    Left_Angle_Quotation        : @key[constant] Character := '@latin1(171)';  @i{--Character'Val(171)}
    Not_Sign                    : @key[constant] Character := '@latin1(172)';  @i{--Character'Val(172)}
    Soft_Hyphen                 : @key[constant] Character := '@latin1(173)';  @i{--Character'Val(173)}
    Registered_Trade_Mark_Sign  : @key[constant] Character := '@latin1(174)';  @i{--Character'Val(174)}
    Macron                      : @key[constant] Character := '@latin1(175)';  @i{--Character'Val(175)}

@i{-- Character positions 176 (16#B0#) .. 191 (16#BF#):}
    Degree_Sign                 : @key[constant] Character := '@latin1(176)';  @i{--Character'Val(176)}
    Ring_Above                  : Character @key[renames] Degree_Sign;
    Plus_Minus_Sign             : @key[constant] Character := '@latin1(177)';  @i{--Character'Val(177)}
    Superscript_Two             : @key[constant] Character := '@latin1(178)';  @i{--Character'Val(178)}
    Superscript_Three           : @key[constant] Character := '@latin1(179)';  @i{--Character'Val(179)}
    Acute                       : @key[constant] Character := '@latin1(180)';  @i{--Character'Val(180)}
    Micro_Sign                  : @key[constant] Character := '@latin1(181)';  @i{--Character'Val(181)}
    Pilcrow_Sign                : @key[constant] Character := '@latin1(182)';  @i{--Character'Val(182)}
    Paragraph_Sign              : Character @key[renames] Pilcrow_Sign;
    Middle_Dot                  : @key[constant] Character := '@latin1(183)';  @i{--Character'Val(183)}
    Cedilla                     : @key[constant] Character := '@latin1(184)';  @i{--Character'Val(184)}
    Superscript_One             : @key[constant] Character := '@latin1(185)';  @i{--Character'Val(185)}
    Masculine_Ordinal_Indicator : @key[constant] Character := '@latin1(186)';  @i{--Character'Val(186)}
    Right_Angle_Quotation       : @key[constant] Character := '@latin1(187)';  @i{--Character'Val(187)}
    Fraction_One_Quarter        : @key[constant] Character := '@latin1(188)';  @i{--Character'Val(188)}
    Fraction_One_Half           : @key[constant] Character := '@latin1(189)';  @i{--Character'Val(189)}
    Fraction_Three_Quarters     : @key[constant] Character := '@latin1(190)';  @i{--Character'Val(190)}
    Inverted_Question           : @key[constant] Character := '@latin1(191)';  @i{--Character'Val(191)}

@i{-- Character positions 192 (16#C0#) .. 207 (16#CF#):}
    UC_A_Grave                  : @key[constant] Character := '@latin1(192)';  @i{--Character'Val(192)}
    UC_A_Acute                  : @key[constant] Character := '@latin1(193)';  @i{--Character'Val(193)}
    UC_A_Circumflex             : @key[constant] Character := '@latin1(194)';  @i{--Character'Val(194)}
    UC_A_Tilde                  : @key[constant] Character := '@latin1(195)';  @i{--Character'Val(195)}
    UC_A_Diaeresis              : @key[constant] Character := '@latin1(196)';  @i{--Character'Val(196)}
    UC_A_Ring                   : @key[constant] Character := '@latin1(197)';  @i{--Character'Val(197)}
    UC_AE_Diphthong             : @key[constant] Character := '@latin1(198)';  @i{--Character'Val(198)}
    UC_C_Cedilla                : @key[constant] Character := '@latin1(199)';  @i{--Character'Val(199)}
    UC_E_Grave                  : @key[constant] Character := '@latin1(200)';  @i{--Character'Val(200)}
    UC_E_Acute                  : @key[constant] Character := '@latin1(201)';  @i{--Character'Val(201)}
    UC_E_Circumflex             : @key[constant] Character := '@latin1(202)';  @i{--Character'Val(202)}
    UC_E_Diaeresis              : @key[constant] Character := '@latin1(203)';  @i{--Character'Val(203)}
    UC_I_Grave                  : @key[constant] Character := '@latin1(204)';  @i{--Character'Val(204)}
    UC_I_Acute                  : @key[constant] Character := '@latin1(205)';  @i{--Character'Val(205)}
    UC_I_Circumflex             : @key[constant] Character := '@latin1(206)';  @i{--Character'Val(206)}
    UC_I_Diaeresis              : @key[constant] Character := '@latin1(207)';  @i{--Character'Val(207)}

@i{-- Character positions 208 (16#D0#) .. 223 (16#DF#):}
    UC_Icelandic_Eth            : @key[constant] Character := '@latin1(208)';  @i{--Character'Val(208)}
    UC_N_Tilde                  : @key[constant] Character := '@latin1(209)';  @i{--Character'Val(209)}
    UC_O_Grave                  : @key[constant] Character := '@latin1(210)';  @i{--Character'Val(210)}
    UC_O_Acute                  : @key[constant] Character := '@latin1(211)';  @i{--Character'Val(211)}
    UC_O_Circumflex             : @key[constant] Character := '@latin1(212)';  @i{--Character'Val(212)}
    UC_O_Tilde                  : @key[constant] Character := '@latin1(213)';  @i{--Character'Val(213)}
    UC_O_Diaeresis              : @key[constant] Character := '@latin1(214)';  @i{--Character'Val(214)}
    Multiplication_Sign         : @key[constant] Character := '@latin1(215)';  @i{--Character'Val(215)}
    UC_O_Oblique_Stroke         : @key[constant] Character := '@latin1(216)';  @i{--Character'Val(216)}
    UC_U_Grave                  : @key[constant] Character := '@latin1(217)';  @i{--Character'Val(217)}
    UC_U_Acute                  : @key[constant] Character := '@latin1(218)';  @i{--Character'Val(218)}
    UC_U_Circumflex             : @key[constant] Character := '@latin1(219)';  @i{--Character'Val(219)}
    UC_U_Diaeresis              : @key[constant] Character := '@latin1(220)';  @i{--Character'Val(220)}
    UC_Y_Acute                  : @key[constant] Character := '@latin1(221)';  @i{--Character'Val(221)}
    UC_Icelandic_Thorn          : @key[constant] Character := '@latin1(222)';  @i{--Character'Val(222)}
    LC_German_Sharp_S           : @key[constant] Character := '@latin1(223)';  @i{--Character'Val(223)}

@i{-- Character positions 224 (16#E0#) .. 239 (16#EF#):}
    LC_A_Grave                  : @key[constant] Character := '@latin1(224)';  @i{--Character'Val(224)}
    LC_A_Acute                  : @key[constant] Character := '@latin1(225)';  @i{--Character'Val(225)}
    LC_A_Circumflex             : @key[constant] Character := '@latin1(226)';  @i{--Character'Val(226)}
    LC_A_Tilde                  : @key[constant] Character := '@latin1(227)';  @i{--Character'Val(227)}
    LC_A_Diaeresis              : @key[constant] Character := '@latin1(228)';  @i{--Character'Val(228)}
    LC_A_Ring                   : @key[constant] Character := '@latin1(229)';  @i{--Character'Val(229)}
    LC_AE_Diphthong             : @key[constant] Character := '@latin1(230)';  @i{--Character'Val(230)}
    LC_C_Cedilla                : @key[constant] Character := '@latin1(231)';  @i{--Character'Val(231)}
    LC_E_Grave                  : @key[constant] Character := '@latin1(232)';  @i{--Character'Val(232)}
    LC_E_Acute                  : @key[constant] Character := '@latin1(233)';  @i{--Character'Val(233)}
    LC_E_Circumflex             : @key[constant] Character := '@latin1(234)';  @i{--Character'Val(234)}
    LC_E_Diaeresis              : @key[constant] Character := '@latin1(235)';  @i{--Character'Val(235)}
    LC_I_Grave                  : @key[constant] Character := '@latin1(236)';  @i{--Character'Val(236)}
    LC_I_Acute                  : @key[constant] Character := '@latin1(237)';  @i{--Character'Val(237)}
    LC_I_Circumflex             : @key[constant] Character := '@latin1(238)';  @i{--Character'Val(238)}
    LC_I_Diaeresis              : @key[constant] Character := '@latin1(239)';  @i{--Character'Val(239)}

@i{-- Character positions 240 (16#F0#) .. 255 (16#FF#):}
    LC_Icelandic_Eth            : @key[constant] Character := '@latin1(240)';  @i{--Character'Val(240)}
    LC_N_Tilde                  : @key[constant] Character := '@latin1(241)';  @i{--Character'Val(241)}
    LC_O_Grave                  : @key[constant] Character := '@latin1(242)';  @i{--Character'Val(242)}
    LC_O_Acute                  : @key[constant] Character := '@latin1(243)';  @i{--Character'Val(243)}
    LC_O_Circumflex             : @key[constant] Character := '@latin1(244)';  @i{--Character'Val(244)}
    LC_O_Tilde                  : @key[constant] Character := '@latin1(245)';  @i{--Character'Val(245)}
    LC_O_Diaeresis              : @key[constant] Character := '@latin1(246)';  @i{--Character'Val(246)}
    Division_Sign               : @key[constant] Character := '@latin1(247)';  @i{--Character'Val(247)}
    LC_O_Oblique_Stroke         : @key[constant] Character := '@latin1(248)';  @i{--Character'Val(248)}
    LC_U_Grave                  : @key[constant] Character := '@latin1(249)';  @i{--Character'Val(249)}
    LC_U_Acute                  : @key[constant] Character := '@latin1(250)';  @i{--Character'Val(250)}
    LC_U_Circumflex             : @key[constant] Character := '@latin1(251)';  @i{--Character'Val(251)}
    LC_U_Diaeresis              : @key[constant] Character := '@latin1(252)';  @i{--Character'Val(252)}
    LC_Y_Acute                  : @key[constant] Character := '@latin1(253)';  @i{--Character'Val(253)}
    LC_Icelandic_Thorn          : @key[constant] Character := '@latin1(254)';  @i{--Character'Val(254)}
    LC_Y_Diaeresis              : @key[constant] Character := '@latin1(255)';  @i{--Character'Val(255)}
@key[end] Ada.Characters.Latin_1;
@end{Example}

@end{StaticSem}

@begin{ImplPerm}
An implementation may provide additional  packages as children of
Ada.Characters,  to declare names for the  symbols of the local character set
or other character sets.
@end{ImplPerm}
