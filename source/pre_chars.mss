@comment{ $Source: e:\\cvsroot/ARM/Source/pre_chars.mss,v $ }
@comment{ $Revision: 1.19 $ $Date: 2005/01/29 07:15:07 $ $Author: Randy $ }
@Part(predefchars, Root="ada.mss")

@Comment{$Date: 2005/01/29 07:15:07 $}

@LabeledClause{Character Handling}
@begin{Intro}
This clause presents the packages related to character processing:
an empty pure package Characters and child packages
Characters.Handling and Characters.Latin_1.
The package Characters.Handling provides classification and conversion
functions for Character data, and some simple functions for
dealing with Wide_Character data.
The child package Characters.Latin_1 declares a set of
constants initialized to values of type Character.
@end{Intro}

@begin{Extend83}
@Defn{extensions to Ada 83}
This clause is new to Ada 95.
@end{Extend83}

@LabeledSubClause(The Package Characters)

@begin{StaticSem}
@leading@keepnext@;The library package Characters has the following declaration:
@begin{example}
@ChildUnit{Parent=[Ada],Child=[Characters]}@key(package) Ada.Characters @key[is]
  @key[pragma] Pure(Characters);
@key(end) Ada.Characters;
@end{example}

@end{StaticSem}

@LabeledSubClause{The Package Characters.Handling}
@begin{StaticSem}
@leading@keepnext@;The library package Characters.Handling has the following declaration:
@begin{example}
@key[package] Ada.Characters.Handling @key[is]@ChildUnit{Parent=[Ada.Characters],Child=[Handling]}
  @key[pragma] Preelaborate(Handling);

@keepnext--@RI{Character classification functions}

  @key[function] @AdaSubDefn{Is_Control}           (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_Graphic}           (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_Letter}            (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_Lower}             (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_Upper}             (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_Basic}             (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_Digit}             (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_Decimal_Digit}     (Item : @key[in] Character) @key[return] Boolean
                     @key[renames] Is_Digit;
  @key[function] @AdaSubDefn{Is_Hexadecimal_Digit} (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_Alphanumeric}      (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_Special}           (Item : @key[in] Character) @key[return] Boolean;


@keepnext--@RI{Conversion functions for Character and String}

  @key[function] @AdaSubDefn{To_Lower} (Item : @key[in] Character) @key[return] Character;
  @key[function] @AdaSubDefn{To_Upper} (Item : @key[in] Character) @key[return] Character;
  @key[function] @AdaSubDefn{To_Basic} (Item : @key[in] Character) @key[return] Character;

  @key[function] @AdaSubDefn{To_Lower} (Item : @key[in] String) @key[return] String;
  @key[function] @AdaSubDefn{To_Upper} (Item : @key[in] String) @key[return] String;
  @key[function] @AdaSubDefn{To_Basic} (Item : @key[in] String) @key[return] String;


@keepnext--@RI{Classifications of and conversions between Character and ISO 646}

  @key[subtype] @AdaDefn{ISO_646} @key[is]
    Character @key[range] Character'Val(0) .. Character'Val(127);

  @key[function] @AdaSubDefn{Is_ISO_646} (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_ISO_646} (Item : @key[in] String)    @key[return] Boolean;

  @key[function] @AdaSubDefn{To_ISO_646} (Item       : @key[in] Character;
                       Substitute : @key[in] ISO_646 := ' ')
    @key[return] ISO_646;

  @key[function] @AdaSubDefn{To_ISO_646} (Item       : @key[in] String;
                       Substitute : @key[in] ISO_646 := ' ')
    @key[return] String;


@Keepnext--@RI{Classifications of and conversions between Wide_Character and Character.}

  @key[function] @AdaSubDefn{Is_Character} (Item : @key[in] Wide_Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_String}    (Item : @key[in] Wide_String)    @key[return] Boolean;


  @key[function] @AdaSubDefn{To_Character} (Item       : @key[in] Wide_Character;
                         Substitute : @key[in] Character := ' ')
    @key[return] Character;

  @key[function] @AdaSubDefn{To_String}    (Item       : @key[in] Wide_String;
                         Substitute : @key[in] Character := ' ')
    @key[return] String;


  @key[function] @AdaSubDefn{To_Wide_Character} (Item : @key[in] Character) @key[return] Wide_Character;

  @key[function] @AdaSubDefn{To_Wide_String}    (Item : @key[in] String)    @key[return] Wide_String;

@key[end] Ada.Characters.Handling;
@end{Example}

In the description below for each function that returns a Boolean
result, the effect is described in terms of the conditions under which
the value True is returned. If these conditions are not met, then the
function returns False.

Each of the following classification functions has a formal Character
parameter, Item, and returns a Boolean result.
@begin{description}
@Defn2{term=[control character], sec=[a category of Character]}Is_Control @\True if Item is a control character.
A @i{control character} is a character whose position is
in one of the ranges 0..31 or 127..159.

@Defn2{term=[graphic character], sec=[a category of Character]}Is_Graphic @\True if Item is a graphic character. A @i[graphic character]
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
other that converts from String to String. The result of each
Character-to-Character function is described below, in terms of the
conversion applied to Item, its formal Character parameter. The
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
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00285-01]}
@ChgDeleted{Version=[2],Text=[If an implementation provides a localized
definition of Character or Wide_Character, then the effects of the subprograms
in Characters.Handling should reflect the localizations. See also
@RefSecNum(Character Types).]}
@end{ImplAdvice}

@begin{Notes}
 A basic letter is a letter without a diacritical mark.

@Leading@;Except for the hexadecimal digits, basic letters, and ISO_646
characters, the categories identified in the classification functions
form a strict hierarchy:
@begin{Display}
@TabClear{}@Comment{We use "Leading" below to make this list closely packed}
@TabSet{4, 8, 12, 16}
@Leading@em Control characters

@Leading@em Graphic characters

@Leading@\@em Alphanumeric characters

@Leading@\@\@em Letters

@Leading@\@\@\@em Upper-case letters

@Leading@\@\@\@em Lower-case letters

@Leading@\@\@em Decimal digits

@Leading@\@em Special graphic characters
@end{Display}

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
opposed to explicit constants. The main reason for explicit constants
was for consistency of style with the upper-half constants, and to avoid
emphasizing the package ASCII.@end{reason}
@end{Intro}

@begin{StaticSem}
@leading@keepnext@;The library package Characters.Latin_1 has the following
declaration:
@begin{Example}
@key[package] Ada.Characters.Latin_1 @key[is]@ChildUnit{Parent=[Ada.Characters],Child=[Latin_1]}
    @key[pragma] Pure(Latin_1);

@keepnext@RI{-- Control characters:}@PDefn2{term=[control character],
sec=[a category of Character]}

    @AdaDefn{NUL}                  : @key[constant] Character := Character'Val(0);
    @AdaDefn{SOH}                  : @key[constant] Character := Character'Val(1);
    @AdaDefn{STX}                  : @key[constant] Character := Character'Val(2);
    @AdaDefn{ETX}                  : @key[constant] Character := Character'Val(3);
    @AdaDefn{EOT}                  : @key[constant] Character := Character'Val(4);
    @AdaDefn{ENQ}                  : @key[constant] Character := Character'Val(5);
    @AdaDefn{ACK}                  : @key[constant] Character := Character'Val(6);
    @AdaDefn{BEL}                  : @key[constant] Character := Character'Val(7);
    @AdaDefn{BS}                   : @key[constant] Character := Character'Val(8);
    @AdaDefn{HT}                   : @key[constant] Character := Character'Val(9);
    @AdaDefn{LF}                   : @key[constant] Character := Character'Val(10);
    @AdaDefn{VT}                   : @key[constant] Character := Character'Val(11);
    @AdaDefn{FF}                   : @key[constant] Character := Character'Val(12);
    @AdaDefn{CR}                   : @key[constant] Character := Character'Val(13);
    @AdaDefn{SO}                   : @key[constant] Character := Character'Val(14);
    @AdaDefn{SI}                   : @key[constant] Character := Character'Val(15);

    @AdaDefn{DLE}                  : @key[constant] Character := Character'Val(16);
    @AdaDefn{DC1}                  : @key[constant] Character := Character'Val(17);
    @AdaDefn{DC2}                  : @key[constant] Character := Character'Val(18);
    @AdaDefn{DC3}                  : @key[constant] Character := Character'Val(19);
    @AdaDefn{DC4}                  : @key[constant] Character := Character'Val(20);
    @AdaDefn{NAK}                  : @key[constant] Character := Character'Val(21);
    @AdaDefn{SYN}                  : @key[constant] Character := Character'Val(22);
    @AdaDefn{ETB}                  : @key[constant] Character := Character'Val(23);
    @AdaDefn{CAN}                  : @key[constant] Character := Character'Val(24);
    @AdaDefn{EM}                   : @key[constant] Character := Character'Val(25);
    @AdaDefn{SUB}                  : @key[constant] Character := Character'Val(26);
    @AdaDefn{ESC}                  : @key[constant] Character := Character'Val(27);
    @AdaDefn{FS}                   : @key[constant] Character := Character'Val(28);
    @AdaDefn{GS}                   : @key[constant] Character := Character'Val(29);
    @AdaDefn{RS}                   : @key[constant] Character := Character'Val(30);
    @AdaDefn{US}                   : @key[constant] Character := Character'Val(31);

@keepnext@RI{-- ISO 646 graphic characters:}

    @AdaDefn{Space}                : @key[constant] Character := ' ';  @RI{-- Character'Val(32)}
    @AdaDefn{Exclamation}          : @key[constant] Character := '!';  @RI{-- Character'Val(33)}
    @AdaDefn{Quotation}            : @key[constant] Character := '"';  @RI{-- Character'Val(34)}
    @AdaDefn{Number_Sign}          : @key[constant] Character := '#';  @RI{-- Character'Val(35)}
    @AdaDefn{Dollar_Sign}          : @key[constant] Character := '$';  @RI{-- Character'Val(36)}
    @AdaDefn{Percent_Sign}         : @key[constant] Character := '%';  @RI{-- Character'Val(37)}
    @AdaDefn{Ampersand}            : @key[constant] Character := '&';  @RI{-- Character'Val(38)}
    @AdaDefn{Apostrophe}           : @key[constant] Character := ''';  @RI{-- Character'Val(39)}
    @AdaDefn{Left_Parenthesis}     : @key[constant] Character := '(';  @RI{-- Character'Val(40)}
    @AdaDefn{Right_Parenthesis}    : @key[constant] Character := ')';  @RI{-- Character'Val(41)}
    @AdaDefn{Asterisk}             : @key[constant] Character := '*';  @RI{-- Character'Val(42)}
    @AdaDefn{Plus_Sign}            : @key[constant] Character := '+';  @RI{-- Character'Val(43)}
    @AdaDefn{Comma}                : @key[constant] Character := ',';  @RI{-- Character'Val(44)}
    @AdaDefn{Hyphen}               : @key[constant] Character := '-';  @RI{-- Character'Val(45)}
    @AdaDefn{Minus_Sign}           : Character @key[renames] Hyphen;
    @AdaDefn{Full_Stop}            : @key[constant] Character := '.';  @RI{-- Character'Val(46)}
    @AdaDefn{Solidus}              : @key[constant] Character := '/';  @RI{-- Character'Val(47)}

@keepnext    @RI{-- Decimal digits '0' though '9' are at positions 48 through 57}

    @AdaDefn{Colon}                : @key[constant] Character := ':';  @RI{-- Character'Val(58)}
    @AdaDefn{Semicolon}            : @key[constant] Character := ';';  @RI{-- Character'Val(59)}
    @AdaDefn{Less_Than_Sign}       : @key[constant] Character := '<';  @RI{-- Character'Val(60)}
    @AdaDefn{Equals_Sign}          : @key[constant] Character := '=';  @RI{-- Character'Val(61)}
    @AdaDefn{Greater_Than_Sign}    : @key[constant] Character := '>';  @RI{-- Character'Val(62)}
    @AdaDefn{Question}             : @key[constant] Character := '?';  @RI{-- Character'Val(63)}
    @AdaDefn{Commercial_At}        : @key[constant] Character := '@@';  @RI{-- Character'Val(64)}

@keepnext    @RI{-- Letters 'A' through 'Z' are at positions 65 through 90}

    @AdaDefn{Left_Square_Bracket}  : @key[constant] Character := '[';  @RI{-- Character'Val(91)}
    @AdaDefn{Reverse_Solidus}      : @key[constant] Character := '\';  @RI{-- Character'Val(92)}
    @AdaDefn{Right_Square_Bracket} : @key[constant] Character := ']';  @RI{-- Character'Val(93)}
    @AdaDefn{Circumflex}           : @key[constant] Character := '^';  @RI{-- Character'Val(94)}
    @AdaDefn{Low_Line}             : @key[constant] Character := '_';  @RI{-- Character'Val(95)}

    @AdaDefn{Grave}                : @key[constant] Character := '`';  @RI{-- Character'Val(96)}
    @AdaDefn{LC_A}                 : @key[constant] Character := 'a';  @RI{-- Character'Val(97)}
    @AdaDefn{LC_B}                 : @key[constant] Character := 'b';  @RI{-- Character'Val(98)}
    @AdaDefn{LC_C}                 : @key[constant] Character := 'c';  @RI{-- Character'Val(99)}
    @AdaDefn{LC_D}                 : @key[constant] Character := 'd';  @RI{-- Character'Val(100)}
    @AdaDefn{LC_E}                 : @key[constant] Character := 'e';  @RI{-- Character'Val(101)}
    @AdaDefn{LC_F}                 : @key[constant] Character := 'f';  @RI{-- Character'Val(102)}
    @AdaDefn{LC_G}                 : @key[constant] Character := 'g';  @RI{-- Character'Val(103)}
    @AdaDefn{LC_H}                 : @key[constant] Character := 'h';  @RI{-- Character'Val(104)}
    @AdaDefn{LC_I}                 : @key[constant] Character := 'i';  @RI{-- Character'Val(105)}
    @AdaDefn{LC_J}                 : @key[constant] Character := 'j';  @RI{-- Character'Val(106)}
    @AdaDefn{LC_K}                 : @key[constant] Character := 'k';  @RI{-- Character'Val(107)}
    @AdaDefn{LC_L}                 : @key[constant] Character := 'l';  @RI{-- Character'Val(108)}
    @AdaDefn{LC_M}                 : @key[constant] Character := 'm';  @RI{-- Character'Val(109)}
    @AdaDefn{LC_N}                 : @key[constant] Character := 'n';  @RI{-- Character'Val(110)}
    @AdaDefn{LC_O}                 : @key[constant] Character := 'o';  @RI{-- Character'Val(111)}

    @AdaDefn{LC_P}                 : @key[constant] Character := 'p';  @RI{-- Character'Val(112)}
    @AdaDefn{LC_Q}                 : @key[constant] Character := 'q';  @RI{-- Character'Val(113)}
    @AdaDefn{LC_R}                 : @key[constant] Character := 'r';  @RI{-- Character'Val(114)}
    @AdaDefn{LC_S}                 : @key[constant] Character := 's';  @RI{-- Character'Val(115)}
    @AdaDefn{LC_T}                 : @key[constant] Character := 't';  @RI{-- Character'Val(116)}
    @AdaDefn{LC_U}                 : @key[constant] Character := 'u';  @RI{-- Character'Val(117)}
    @AdaDefn{LC_V}                 : @key[constant] Character := 'v';  @RI{-- Character'Val(118)}
    @AdaDefn{LC_W}                 : @key[constant] Character := 'w';  @RI{-- Character'Val(119)}
    @AdaDefn{LC_X}                 : @key[constant] Character := 'x';  @RI{-- Character'Val(120)}
    @AdaDefn{LC_Y}                 : @key[constant] Character := 'y';  @RI{-- Character'Val(121)}
    @AdaDefn{LC_Z}                 : @key[constant] Character := 'z';  @RI{-- Character'Val(122)}
    @AdaDefn{Left_Curly_Bracket}   : @key[constant] Character := '{';  @RI{-- Character'Val(123)}
    @AdaDefn{Vertical_Line}        : @key[constant] Character := '|';  @RI{-- Character'Val(124)}
    @AdaDefn{Right_Curly_Bracket}  : @key[constant] Character := '}';  @RI{-- Character'Val(125)}
    @AdaDefn{Tilde}                : @key[constant] Character := '~';  @RI{-- Character'Val(126)}
    @AdaDefn{DEL}                  : @key[constant] Character := Character'Val(127);


@keepnext@RI{-- ISO 6429 control characters:}@PDefn2{term=[control character],
sec=[a category of Character]}

    @AdaDefn{IS4}                  : Character @key[renames] FS;
    @AdaDefn{IS3}                  : Character @key[renames] GS;
    @AdaDefn{IS2}                  : Character @key[renames] RS;
    @AdaDefn{IS1}                  : Character @key[renames] US;

    @AdaDefn{Reserved_128}         : @key[constant] Character := Character'Val(128);
    @AdaDefn{Reserved_129}         : @key[constant] Character := Character'Val(129);
    @AdaDefn{BPH}                  : @key[constant] Character := Character'Val(130);
    @AdaDefn{NBH}                  : @key[constant] Character := Character'Val(131);
    @AdaDefn{Reserved_132}         : @key[constant] Character := Character'Val(132);
    @AdaDefn{NEL}                  : @key[constant] Character := Character'Val(133);
    @AdaDefn{SSA}                  : @key[constant] Character := Character'Val(134);
    @AdaDefn{ESA}                  : @key[constant] Character := Character'Val(135);
    @AdaDefn{HTS}                  : @key[constant] Character := Character'Val(136);
    @AdaDefn{HTJ}                  : @key[constant] Character := Character'Val(137);
    @AdaDefn{VTS}                  : @key[constant] Character := Character'Val(138);
    @AdaDefn{PLD}                  : @key[constant] Character := Character'Val(139);
    @AdaDefn{PLU}                  : @key[constant] Character := Character'Val(140);
    @AdaDefn{RI}                   : @key[constant] Character := Character'Val(141);
    @AdaDefn{SS2}                  : @key[constant] Character := Character'Val(142);
    @AdaDefn{SS3}                  : @key[constant] Character := Character'Val(143);

    @AdaDefn{DCS}                  : @key[constant] Character := Character'Val(144);
    @AdaDefn{PU1}                  : @key[constant] Character := Character'Val(145);
    @AdaDefn{PU2}                  : @key[constant] Character := Character'Val(146);
    @AdaDefn{STS}                  : @key[constant] Character := Character'Val(147);
    @AdaDefn{CCH}                  : @key[constant] Character := Character'Val(148);
    @AdaDefn{MW}                   : @key[constant] Character := Character'Val(149);
    @AdaDefn{SPA}                  : @key[constant] Character := Character'Val(150);
    @AdaDefn{EPA}                  : @key[constant] Character := Character'Val(151);

    @AdaDefn{SOS}                  : @key[constant] Character := Character'Val(152);
    @AdaDefn{Reserved_153}         : @key[constant] Character := Character'Val(153);
    @AdaDefn{SCI}                  : @key[constant] Character := Character'Val(154);
    @AdaDefn{CSI}                  : @key[constant] Character := Character'Val(155);
    @AdaDefn{ST}                   : @key[constant] Character := Character'Val(156);
    @AdaDefn{OSC}                  : @key[constant] Character := Character'Val(157);
    @AdaDefn{PM}                   : @key[constant] Character := Character'Val(158);
    @AdaDefn{APC}                  : @key[constant] Character := Character'Val(159);

@keepnext@RI{-- Other graphic characters:}

@RI{-- Character positions 160 (16#A0#) .. 175 (16#AF#):}
    @AdaDefn{No_Break_Space}              : @key[constant] Character := ' ';  @RI{--Character'Val(160)}
    @AdaDefn{NBSP}                        : Character @key[renames] No_Break_Space;
    @AdaDefn{Inverted_Exclamation}        : @key[constant] Character := '@latin1(161)';  @RI{--Character'Val(161)}
    @AdaDefn{Cent_Sign}                   : @key[constant] Character := '@latin1(162)';  @RI{--Character'Val(162)}
    @AdaDefn{Pound_Sign}                  : @key[constant] Character := '@latin1(163)';  @RI{--Character'Val(163)}
    @AdaDefn{Currency_Sign}               : @key[constant] Character := '@latin1(164)';  @RI{--Character'Val(164)}
    @AdaDefn{Yen_Sign}                    : @key[constant] Character := '@latin1(165)';  @RI{--Character'Val(165)}
    @AdaDefn{Broken_Bar}                  : @key[constant] Character := '@latin1(166)';  @RI{--Character'Val(166)}
    @AdaDefn{Section_Sign}                : @key[constant] Character := '@latin1(167)';  @RI{--Character'Val(167)}
    @AdaDefn{Diaeresis}                   : @key[constant] Character := '@latin1(168)';  @RI{--Character'Val(168)}
    @AdaDefn{Copyright_Sign}              : @key[constant] Character := '@latin1(169)';  @RI{--Character'Val(169)}
    @AdaDefn{Feminine_Ordinal_Indicator}  : @key[constant] Character := '@latin1(170)';  @RI{--Character'Val(170)}
    @AdaDefn{Left_Angle_Quotation}        : @key[constant] Character := '@latin1(171)';  @RI{--Character'Val(171)}
    @AdaDefn{Not_Sign}                    : @key[constant] Character := '@latin1(172)';  @RI{--Character'Val(172)}
    @AdaDefn{Soft_Hyphen}                 : @key[constant] Character := '@latin1(173)';  @RI{--Character'Val(173)}
    @AdaDefn{Registered_Trade_Mark_Sign}  : @key[constant] Character := '@latin1(174)';  @RI{--Character'Val(174)}
    @AdaDefn{Macron}                      : @key[constant] Character := '@latin1(175)';  @RI{--Character'Val(175)}

@RI{-- Character positions 176 (16#B0#) .. 191 (16#BF#):}
    @AdaDefn{Degree_Sign}                 : @key[constant] Character := '@latin1(176)';  @RI{--Character'Val(176)}
    @AdaDefn{Ring_Above}                  : Character @key[renames] Degree_Sign;
    @AdaDefn{Plus_Minus_Sign}             : @key[constant] Character := '@latin1(177)';  @RI{--Character'Val(177)}
    @AdaDefn{Superscript_Two}             : @key[constant] Character := '@latin1(178)';  @RI{--Character'Val(178)}
    @AdaDefn{Superscript_Three}           : @key[constant] Character := '@latin1(179)';  @RI{--Character'Val(179)}
    @AdaDefn{Acute}                       : @key[constant] Character := '@latin1(180)';  @RI{--Character'Val(180)}
    @AdaDefn{Micro_Sign}                  : @key[constant] Character := '@latin1(181)';  @RI{--Character'Val(181)}
    @AdaDefn{Pilcrow_Sign}                : @key[constant] Character := '@latin1(182)';  @RI{--Character'Val(182)}
    @AdaDefn{Paragraph_Sign}              : Character @key[renames] Pilcrow_Sign;
    @AdaDefn{Middle_Dot}                  : @key[constant] Character := '@latin1(183)';  @RI{--Character'Val(183)}
    @AdaDefn{Cedilla}                     : @key[constant] Character := '@latin1(184)';  @RI{--Character'Val(184)}
    @AdaDefn{Superscript_One}             : @key[constant] Character := '@latin1(185)';  @RI{--Character'Val(185)}
    @AdaDefn{Masculine_Ordinal_Indicator} : @key[constant] Character := '@latin1(186)';  @RI{--Character'Val(186)}
    @AdaDefn{Right_Angle_Quotation}       : @key[constant] Character := '@latin1(187)';  @RI{--Character'Val(187)}
    @AdaDefn{Fraction_One_Quarter}        : @key[constant] Character := '@latin1(188)';  @RI{--Character'Val(188)}
    @AdaDefn{Fraction_One_Half}           : @key[constant] Character := '@latin1(189)';  @RI{--Character'Val(189)}
    @AdaDefn{Fraction_Three_Quarters}     : @key[constant] Character := '@latin1(190)';  @RI{--Character'Val(190)}
    @AdaDefn{Inverted_Question}           : @key[constant] Character := '@latin1(191)';  @RI{--Character'Val(191)}

@RI{-- Character positions 192 (16#C0#) .. 207 (16#CF#):}
    @AdaDefn{UC_A_Grave}                  : @key[constant] Character := '@latin1(192)';  @RI{--Character'Val(192)}
    @AdaDefn{UC_A_Acute}                  : @key[constant] Character := '@latin1(193)';  @RI{--Character'Val(193)}
    @AdaDefn{UC_A_Circumflex}             : @key[constant] Character := '@latin1(194)';  @RI{--Character'Val(194)}
    @AdaDefn{UC_A_Tilde}                  : @key[constant] Character := '@latin1(195)';  @RI{--Character'Val(195)}
    @AdaDefn{UC_A_Diaeresis}              : @key[constant] Character := '@latin1(196)';  @RI{--Character'Val(196)}
    @AdaDefn{UC_A_Ring}                   : @key[constant] Character := '@latin1(197)';  @RI{--Character'Val(197)}
    @AdaDefn{UC_AE_Diphthong}             : @key[constant] Character := '@latin1(198)';  @RI{--Character'Val(198)}
    @AdaDefn{UC_C_Cedilla}                : @key[constant] Character := '@latin1(199)';  @RI{--Character'Val(199)}
    @AdaDefn{UC_E_Grave}                  : @key[constant] Character := '@latin1(200)';  @RI{--Character'Val(200)}
    @AdaDefn{UC_E_Acute}                  : @key[constant] Character := '@latin1(201)';  @RI{--Character'Val(201)}
    @AdaDefn{UC_E_Circumflex}             : @key[constant] Character := '@latin1(202)';  @RI{--Character'Val(202)}
    @AdaDefn{UC_E_Diaeresis}              : @key[constant] Character := '@latin1(203)';  @RI{--Character'Val(203)}
    @AdaDefn{UC_I_Grave}                  : @key[constant] Character := '@latin1(204)';  @RI{--Character'Val(204)}
    @AdaDefn{UC_I_Acute}                  : @key[constant] Character := '@latin1(205)';  @RI{--Character'Val(205)}
    @AdaDefn{UC_I_Circumflex}             : @key[constant] Character := '@latin1(206)';  @RI{--Character'Val(206)}
    @AdaDefn{UC_I_Diaeresis}              : @key[constant] Character := '@latin1(207)';  @RI{--Character'Val(207)}

@RI{-- Character positions 208 (16#D0#) .. 223 (16#DF#):}
    @AdaDefn{UC_Icelandic_Eth}            : @key[constant] Character := '@latin1(208)';  @RI{--Character'Val(208)}
    @AdaDefn{UC_N_Tilde}                  : @key[constant] Character := '@latin1(209)';  @RI{--Character'Val(209)}
    @AdaDefn{UC_O_Grave}                  : @key[constant] Character := '@latin1(210)';  @RI{--Character'Val(210)}
    @AdaDefn{UC_O_Acute}                  : @key[constant] Character := '@latin1(211)';  @RI{--Character'Val(211)}
    @AdaDefn{UC_O_Circumflex}             : @key[constant] Character := '@latin1(212)';  @RI{--Character'Val(212)}
    @AdaDefn{UC_O_Tilde}                  : @key[constant] Character := '@latin1(213)';  @RI{--Character'Val(213)}
    @AdaDefn{UC_O_Diaeresis}              : @key[constant] Character := '@latin1(214)';  @RI{--Character'Val(214)}
    @AdaDefn{Multiplication_Sign}         : @key[constant] Character := '@latin1(215)';  @RI{--Character'Val(215)}
    @AdaDefn{UC_O_Oblique_Stroke}         : @key[constant] Character := '@latin1(216)';  @RI{--Character'Val(216)}
    @AdaDefn{UC_U_Grave}                  : @key[constant] Character := '@latin1(217)';  @RI{--Character'Val(217)}
    @AdaDefn{UC_U_Acute}                  : @key[constant] Character := '@latin1(218)';  @RI{--Character'Val(218)}
    @AdaDefn{UC_U_Circumflex}             : @key[constant] Character := '@latin1(219)';  @RI{--Character'Val(219)}
    @AdaDefn{UC_U_Diaeresis}              : @key[constant] Character := '@latin1(220)';  @RI{--Character'Val(220)}
    @AdaDefn{UC_Y_Acute}                  : @key[constant] Character := '@latin1(221)';  @RI{--Character'Val(221)}
    @AdaDefn{UC_Icelandic_Thorn}          : @key[constant] Character := '@latin1(222)';  @RI{--Character'Val(222)}
    @AdaDefn{LC_German_Sharp_S}           : @key[constant] Character := '@latin1(223)';  @RI{--Character'Val(223)}

@RI{-- Character positions 224 (16#E0#) .. 239 (16#EF#):}
    @AdaDefn{LC_A_Grave}                  : @key[constant] Character := '@latin1(224)';  @RI{--Character'Val(224)}
    @AdaDefn{LC_A_Acute}                  : @key[constant] Character := '@latin1(225)';  @RI{--Character'Val(225)}
    @AdaDefn{LC_A_Circumflex}             : @key[constant] Character := '@latin1(226)';  @RI{--Character'Val(226)}
    @AdaDefn{LC_A_Tilde}                  : @key[constant] Character := '@latin1(227)';  @RI{--Character'Val(227)}
    @AdaDefn{LC_A_Diaeresis}              : @key[constant] Character := '@latin1(228)';  @RI{--Character'Val(228)}
    @AdaDefn{LC_A_Ring}                   : @key[constant] Character := '@latin1(229)';  @RI{--Character'Val(229)}
    @AdaDefn{LC_AE_Diphthong}             : @key[constant] Character := '@latin1(230)';  @RI{--Character'Val(230)}
    @AdaDefn{LC_C_Cedilla}                : @key[constant] Character := '@latin1(231)';  @RI{--Character'Val(231)}
    @AdaDefn{LC_E_Grave}                  : @key[constant] Character := '@latin1(232)';  @RI{--Character'Val(232)}
    @AdaDefn{LC_E_Acute}                  : @key[constant] Character := '@latin1(233)';  @RI{--Character'Val(233)}
    @AdaDefn{LC_E_Circumflex}             : @key[constant] Character := '@latin1(234)';  @RI{--Character'Val(234)}
    @AdaDefn{LC_E_Diaeresis}              : @key[constant] Character := '@latin1(235)';  @RI{--Character'Val(235)}
    @AdaDefn{LC_I_Grave}                  : @key[constant] Character := '@latin1(236)';  @RI{--Character'Val(236)}
    @AdaDefn{LC_I_Acute}                  : @key[constant] Character := '@latin1(237)';  @RI{--Character'Val(237)}
    @AdaDefn{LC_I_Circumflex}             : @key[constant] Character := '@latin1(238)';  @RI{--Character'Val(238)}
    @AdaDefn{LC_I_Diaeresis}              : @key[constant] Character := '@latin1(239)';  @RI{--Character'Val(239)}

@RI{-- Character positions 240 (16#F0#) .. 255 (16#FF#):}
    @AdaDefn{LC_Icelandic_Eth}            : @key[constant] Character := '@latin1(240)';  @RI{--Character'Val(240)}
    @AdaDefn{LC_N_Tilde}                  : @key[constant] Character := '@latin1(241)';  @RI{--Character'Val(241)}
    @AdaDefn{LC_O_Grave}                  : @key[constant] Character := '@latin1(242)';  @RI{--Character'Val(242)}
    @AdaDefn{LC_O_Acute}                  : @key[constant] Character := '@latin1(243)';  @RI{--Character'Val(243)}
    @AdaDefn{LC_O_Circumflex}             : @key[constant] Character := '@latin1(244)';  @RI{--Character'Val(244)}
    @AdaDefn{LC_O_Tilde}                  : @key[constant] Character := '@latin1(245)';  @RI{--Character'Val(245)}
    @AdaDefn{LC_O_Diaeresis}              : @key[constant] Character := '@latin1(246)';  @RI{--Character'Val(246)}
    @AdaDefn{Division_Sign}               : @key[constant] Character := '@latin1(247)';  @RI{--Character'Val(247)}
    @AdaDefn{LC_O_Oblique_Stroke}         : @key[constant] Character := '@latin1(248)';  @RI{--Character'Val(248)}
    @AdaDefn{LC_U_Grave}                  : @key[constant] Character := '@latin1(249)';  @RI{--Character'Val(249)}
    @AdaDefn{LC_U_Acute}                  : @key[constant] Character := '@latin1(250)';  @RI{--Character'Val(250)}
    @AdaDefn{LC_U_Circumflex}             : @key[constant] Character := '@latin1(251)';  @RI{--Character'Val(251)}
    @AdaDefn{LC_U_Diaeresis}              : @key[constant] Character := '@latin1(252)';  @RI{--Character'Val(252)}
    @AdaDefn{LC_Y_Acute}                  : @key[constant] Character := '@latin1(253)';  @RI{--Character'Val(253)}
    @AdaDefn{LC_Icelandic_Thorn}          : @key[constant] Character := '@latin1(254)';  @RI{--Character'Val(254)}
    @AdaDefn{LC_Y_Diaeresis}              : @key[constant] Character := '@latin1(255)';  @RI{--Character'Val(255)}
@key[end] Ada.Characters.Latin_1;
@end{Example}

@end{StaticSem}

@begin{ImplPerm}
An implementation may provide additional packages as children of
Ada.Characters, to declare names for the symbols of the local character set
or other character sets.
@end{ImplPerm}