@comment{ $Source: e:\\cvsroot/ARM/Source/pre_chars.mss,v $ }
@comment{ $Revision: 1.42 $ $Date: 2011/11/01 23:14:15 $ $Author: randy $ }
@Part(predefchars, Root="ada.mss")

@Comment{$Date: 2011/11/01 23:14:15 $}

@LabeledClause{Character Handling}
@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0243-1]}
This clause presents the packages related to character processing:
an empty @Chg{Version=[3],New=[declared ],Old=[]}pure package Characters
and child packages Characters.Handling and Characters.Latin_1.
The package Characters.Handling provides classification and conversion
functions for Character data, and some simple functions for
dealing with Wide_Character @Chg{Version=[2],New=[and Wide_Wide_Character ],
Old=[]}data.
The child package Characters.Latin_1 declares a set of
constants initialized to values of type Character.
@end{Intro}

@begin{Extend83}
@Defn{extensions to Ada 83}
This clause is new to Ada 95.
@end{Extend83}

@begin{Diffword95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[Included Wide_Wide_Character in this description;
  the individual changes are documented as extensions as needed.]}
@end{Diffword95}


@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
@RMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledRevisedSubClause{Version=[2],New=[The Packages Characters, Wide_Characters, and Wide_Wide_Characters],Old=[The Package Characters]}

@begin{StaticSem}
@leading@keepnext@;The library package Characters has the following declaration:
@begin{example}
@ChildUnit{Parent=[Ada],Child=[Characters]}@key(package) Ada.Characters @key[is]
  @key[pragma] Pure(Characters);
@key(end) Ada.Characters;
@end{example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[The library package
Wide_Characters has the following declaration:]}
@begin{example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@ChildUnit{Parent=[Ada],Child=[Wide_Characters]}@key(package) Ada.Wide_Characters @key[is]
  @key[pragma] Pure(Wide_Characters);
@key[end] Ada.Wide_Characters;]}
@end{example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[The library package
Wide_Wide_Characters has the following declaration:]}
@begin{example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@ChildUnit{Parent=[Ada],Child=[Wide_Wide_Characters]}@key(package) Ada.Wide_Wide_Characters @key[is]
  @key[pragma] Pure(Wide_Wide_Characters);
@key[end] Ada.Wide_Wide_Characters;]}
@end{example}
@end{StaticSem}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0185-1]}
@ChgAdded{Version=[2],Text=[If an implementation chooses to provide
implementation-defined operations on Wide_Character or Wide_String (such as
@Chg{Version=[3],New=[],Old=[case mapping, classification, ]}collating and
sorting, etc.) it should do so by
providing child units of Wide_Characters. Similarly if it chooses to
provide implementation-defined operations on Wide_Wide_Character or
Wide_Wide_String it should do so by providing child units of
Wide_Wide_Characters.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Implementation-defined operations on Wide_Character, Wide_String,
Wide_Wide_Character, and Wide_Wide_String should be child units of
Wide_Characters or Wide_Wide_Characters.]}]}
@end{ImplAdvice}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The packages Wide_Characters and Wide_Wide_Characters are new.]}
@end{Extend95}


@LabeledSubClause{The Package Characters.Handling}
@begin{StaticSem}
@leading@keepnext@;The library package Characters.Handling has the following declaration:
@begin{example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00362-01],ARef=[AI95-00395-01]}
@Chg{Version=[2],New=[@key[with] Ada.Characters.Conversions;
],Old=[]}@key[package] Ada.Characters.Handling @key[is]@ChildUnit{Parent=[Ada.Characters],Child=[Handling]}
  @key[pragma] @Chg{Version=[2],New=[Pure],Old=[Preelaborate]}(Handling);

@keepnext--@RI{Character classification functions}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0185-1]}
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
  @key[function] @AdaSubDefn{Is_Special}           (Item : @key[in] Character) @key[return] Boolean;@Chg{Version=[3],New=[
  @key[function] @AdaSubDefn{Is_Line_Terminator}   (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_Mark}              (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_Other_Format}      (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_Punctuation_Connector} (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_Space}             (Item : @key[in] Character) @key[return] Boolean;],Old=[]}

@keepnext--@RI{Conversion functions for Character and String}

  @key[function] @AdaSubDefn{To_Lower} (Item : @key[in] Character) @key[return] Character;
  @key[function] @AdaSubDefn{To_Upper} (Item : @key[in] Character) @key[return] Character;
  @key[function] @AdaSubDefn{To_Basic} (Item : @key[in] Character) @key[return] Character;

  @key[function] @AdaSubDefn{To_Lower} (Item : @key[in] String) @key[return] String;
  @key[function] @AdaSubDefn{To_Upper} (Item : @key[in] String) @key[return] String;
  @key[function] @AdaSubDefn{To_Basic} (Item : @key[in] String) @key[return] String;


@keepnext--@RI{Classifications of and conversions between Character and ISO 646}

  @key[subtype] @AdaSubtypeDefn{Name=[ISO_646],Of=[Character]} @key[is]
    Character @key[range] Character'Val(0) .. Character'Val(127);

  @key[function] @AdaSubDefn{Is_ISO_646} (Item : @key[in] Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_ISO_646} (Item : @key[in] String)    @key[return] Boolean;

  @key[function] @AdaSubDefn{To_ISO_646} (Item       : @key[in] Character;
                       Substitute : @key[in] ISO_646 := ' ')
    @key[return] ISO_646;

  @key[function] @AdaSubDefn{To_ISO_646} (Item       : @key[in] String;
                       Substitute : @key[in] ISO_646 := ' ')
    @key[return] String;


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@Keepnext--@RI{@Chg{Version=[2],New=[ The functions Is_Character, Is_String, To_Character, To_String, To_Wide_Character,],Old=[Classifications of and conversions between Wide_Character and Character.]}}@Chg{Version=[2],New=[
--@RI{ and To_Wide_String are obsolescent; see @RefSecnum{Character and Wide_Character Conversion Functions}.}],Old=[]}

@begin{NotIso}
@ChgAdded{Version=[2],Noprefix=[T],Noparanum=[T],Text=[@roman{@Shrink{@i<Paragraphs 14 through 18 were deleted.>}}]}@Comment{This
message should be deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00395-01]}
@ChgDeleted{Version=[2],Text=[  @key[function] @AdaSubDefn{Is_Character} (Item : @key[in] Wide_Character) @key[return] Boolean;
  @key[function] @AdaSubDefn{Is_String}    (Item : @key[in] Wide_String)    @key[return] Boolean;]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00395-01]}
@ChgDeleted{Version=[2],Text=[  @key[function] @AdaSubDefn{To_Character} (Item       : @key[in] Wide_Character;
                         Substitute : @key[in] Character := ' ')
    @key[return] Character;]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00395-01]}
@ChgDeleted{Version=[2],Text=[  @key[function] @AdaSubDefn{To_String}    (Item       : @key[in] Wide_String;
                         Substitute : @key[in] Character := ' ')
    @key[return] String;]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00395-01]}
@ChgDeleted{Version=[2],Text=[  @key[function] @AdaSubDefn{To_Wide_Character} (Item : @key[in] Character) @key[return] Wide_Character;]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00395-01]}
@ChgDeleted{Version=[2],Text=[  @key[function] @AdaSubDefn{To_Wide_String}    (Item : @key[in] String)    @key[return] Wide_String;]}

@key[end] Ada.Characters.Handling;
@end{Example}

@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Text=[The @nt{with_clause} for Ada.Characters.Conversions
is needed for the definition of the obsolescent functions (see
@RefSecnum{Character and Wide_Character Conversion Functions}). It would
be odd to put this clause
into @RefSecnum{Character and Wide_Character Conversion Functions}
as it was not present in Ada 95,
and @nt{with_clause}s are semantically neutral to clients anyway.]}
@end{Discussion}

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
a letter or a decimal digit.

@Defn2{term=[special graphic character], sec=[a category of Character]}Is_Special @\True if Item is a special graphic character.
A @i[special graphic character] is a graphic character that is
not alphanumeric.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Text=[Is_Line_Terminator@\True if Item is a character
with position 10 .. 13 (Line_Feed, Line_Tabulation, Form_Feed, Carriage_Return)
or 133 (Next_Line).]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Text=[Is_Mark@\Never True (no value of type Character
has categories Mark, Non-Spacing or Mark, Spacing Combining).]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Text=[Is_Other_Format@\True if Item is a character
with position 173 (Soft_Hyphen).]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Text=[Is_Punctuation_Connector@\True if Item is a
character with position 95 ('_', known as Low_Line or Underscore).]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Text=[Is_Space@\True if Item is a character with
position 32 (' ') or 160 (No_Break_Space).]}
@end{description}

Each of the names
To_Lower, To_Upper, and To_Basic refers to two
functions: one that converts from Character to Character, and the
other that converts from String to String. The result of each
Character-to-Character function is described below, in terms of the
conversion applied to Item, its formal Character parameter. The
result of each  String-to-String conversion is obtained by applying
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

@begin{NotIso}
@ChgAdded{Version=[2],Noprefix=[T],Noparanum=[T],Text=[@roman{@Shrink{@i<Paragraphs 42
through 48 were deleted.>}}]}@Comment{This message should be deleted if the
paragraphs are ever renumbered.}
@end{NotIso}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgDeleted{Version=[2],Text=[The following set of functions test
Wide_Character values for membership in Character,
or convert between corresponding characters of
Wide_Character and Character.]}
@begin{description}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgDeleted{Version=[2],Text=[Is_Character@\Returns True if
Wide_Character'Pos(Item) <= Character'Pos(Character'Last).]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgDeleted{Version=[2],Text=[Is_String@\Returns True if Is_Character(Item(I))
is True for each I in Item'Range.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgDeleted{Version=[2],Text=[To_Character@\Returns the Character corresponding
to Item if Is_Character(Item), and returns the Substitute Character otherwise.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgDeleted{Version=[2],Text=[To_String@\Returns the String whose range is
1..Item'Length and each of whose elements is given by To_Character of the
corresponding element in Item.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgDeleted{Version=[2],Text=[To_Wide_Character@\Returns the Wide_Character X
such that Character'Pos(Item) = Wide_Character'Pos(X).]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgDeleted{Version=[2],Text=[To_Wide_String@\Returns the Wide_String whose
range is 1..Item'Length and each of whose elements is given by
To_Wide_Character of the corresponding element in Item.]}

@end{description}
@end{StaticSem}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00285-01]}
@ChgDeleted{Version=[2],Text=[If an implementation provides a localized
definition of Character or Wide_Character, then the effects of the subprograms
in Characters.Handling should reflect the localizations. See also
@RefSecNum(Character Types).]}
@ChgNote{No @ChgImplAdvice here, because we'd have to insert and delete the
item in the same version.}
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

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0114-1]}
@ChgAdded{Version=[3],Text=[There are certain characters which are defined to be
lower case letters by ISO 10646 and are therefore allowed in identifiers, but
are not considered lower case letters by Ada.Characters.Handling.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is to maintain runtime compatibility with
  the Ada 95 definitions of these functions. We don't list the exact characters
  involved because they're likely to change in future character set standards;
  the list for ISO 10646:2003 can be found in
  @AILink{AI=[AI05-0114-1],Text=[AI05-0114-1]}.]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[No version of Characters.Handling is intended
  to do portable (Ada-version independent) manipulation of Ada identifiers.
  Specifically for Ada 2012, Wide_Characters.Handling has the correct
  classification of characters, but that is unlikely to be true in future
  Ada standards (it will have to remain tied to the classifications of
  ISO 10646:2003 forever in order to avoid breaking programs at runtime, while
  future Ada standards will move to newer character set standards.]}
@end{Ramification}
@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00362-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Characters.Handling is now Pure, so it can be used in pure units.]}
@end{Extend95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
  Added additional classification routines so that Characters.Handling has
  all of the routines available in Wide_Characters.Handling. If
  Characters.Handling is referenced in a @nt{use_clause}, and an
  entity @i<E> with a @nt{defining_identifier} that is the same as one of
  the new functions is
  defined in a package that is also referenced in a @nt{use_clause}, the entity
  @i<E> may no longer be use-visible, resulting in errors. This should be rare
  and is easily fixed if it does occur.]}
@end{Incompatible2005}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
  @ChgAdded{Version=[2],Text=[The conversion functions are made obsolescent;
  a more complete set is available in Characters.Conversions @em
  see @RefSecNum{The Package Characters.Conversions}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[2],Text=[We no longer talk about localized character
  sets; these are a @Chg{Version=[3],New=[nonstandard],Old=[non-standard]} mode,
  which is none of our business.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0114-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a note to clarify that
  these functions don't have any relationship to the characters allowed in
  identifiers.]}
@end{DiffWord2005}


@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
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

@keepnext--@RI{ Control characters:}@PDefn2{term=[control character],
sec=[a category of Character]}

    @AdaObjDefn{NUL}                  : @key[constant] Character := Character'Val(0);
    @AdaObjDefn{SOH}                  : @key[constant] Character := Character'Val(1);
    @AdaObjDefn{STX}                  : @key[constant] Character := Character'Val(2);
    @AdaObjDefn{ETX}                  : @key[constant] Character := Character'Val(3);
    @AdaObjDefn{EOT}                  : @key[constant] Character := Character'Val(4);
    @AdaObjDefn{ENQ}                  : @key[constant] Character := Character'Val(5);
    @AdaObjDefn{ACK}                  : @key[constant] Character := Character'Val(6);
    @AdaObjDefn{BEL}                  : @key[constant] Character := Character'Val(7);
    @AdaObjDefn{BS}                   : @key[constant] Character := Character'Val(8);
    @AdaObjDefn{HT}                   : @key[constant] Character := Character'Val(9);
    @AdaObjDefn{LF}                   : @key[constant] Character := Character'Val(10);
    @AdaObjDefn{VT}                   : @key[constant] Character := Character'Val(11);
    @AdaObjDefn{FF}                   : @key[constant] Character := Character'Val(12);
    @AdaObjDefn{CR}                   : @key[constant] Character := Character'Val(13);
    @AdaObjDefn{SO}                   : @key[constant] Character := Character'Val(14);
    @AdaObjDefn{SI}                   : @key[constant] Character := Character'Val(15);

    @AdaObjDefn{DLE}                  : @key[constant] Character := Character'Val(16);
    @AdaObjDefn{DC1}                  : @key[constant] Character := Character'Val(17);
    @AdaObjDefn{DC2}                  : @key[constant] Character := Character'Val(18);
    @AdaObjDefn{DC3}                  : @key[constant] Character := Character'Val(19);
    @AdaObjDefn{DC4}                  : @key[constant] Character := Character'Val(20);
    @AdaObjDefn{NAK}                  : @key[constant] Character := Character'Val(21);
    @AdaObjDefn{SYN}                  : @key[constant] Character := Character'Val(22);
    @AdaObjDefn{ETB}                  : @key[constant] Character := Character'Val(23);
    @AdaObjDefn{CAN}                  : @key[constant] Character := Character'Val(24);
    @AdaObjDefn{EM}                   : @key[constant] Character := Character'Val(25);
    @AdaObjDefn{SUB}                  : @key[constant] Character := Character'Val(26);
    @AdaObjDefn{ESC}                  : @key[constant] Character := Character'Val(27);
    @AdaObjDefn{FS}                   : @key[constant] Character := Character'Val(28);
    @AdaObjDefn{GS}                   : @key[constant] Character := Character'Val(29);
    @AdaObjDefn{RS}                   : @key[constant] Character := Character'Val(30);
    @AdaObjDefn{US}                   : @key[constant] Character := Character'Val(31);

@keepnext--@RI{ ISO 646 graphic characters:}

    @AdaObjDefn{Space}                : @key[constant] Character := ' ';  --@RI{ Character'Val(32)}
    @AdaObjDefn{Exclamation}          : @key[constant] Character := '!';  --@RI{ Character'Val(33)}
    @AdaObjDefn{Quotation}            : @key[constant] Character := '"';  --@RI{ Character'Val(34)}
    @AdaObjDefn{Number_Sign}          : @key[constant] Character := '#';  --@RI{ Character'Val(35)}
    @AdaObjDefn{Dollar_Sign}          : @key[constant] Character := '$';  --@RI{ Character'Val(36)}
    @AdaObjDefn{Percent_Sign}         : @key[constant] Character := '%';  --@RI{ Character'Val(37)}
    @AdaObjDefn{Ampersand}            : @key[constant] Character := '&';  --@RI{ Character'Val(38)}
    @AdaObjDefn{Apostrophe}           : @key[constant] Character := ''';  --@RI{ Character'Val(39)}
    @AdaObjDefn{Left_Parenthesis}     : @key[constant] Character := '(';  --@RI{ Character'Val(40)}
    @AdaObjDefn{Right_Parenthesis}    : @key[constant] Character := ')';  --@RI{ Character'Val(41)}
    @AdaObjDefn{Asterisk}             : @key[constant] Character := '*';  --@RI{ Character'Val(42)}
    @AdaObjDefn{Plus_Sign}            : @key[constant] Character := '+';  --@RI{ Character'Val(43)}
    @AdaObjDefn{Comma}                : @key[constant] Character := ',';  --@RI{ Character'Val(44)}
    @AdaObjDefn{Hyphen}               : @key[constant] Character := '-';  --@RI{ Character'Val(45)}
    @AdaObjDefn{Minus_Sign}           : Character @key[renames] Hyphen;
    @AdaObjDefn{Full_Stop}            : @key[constant] Character := '.';  --@RI{ Character'Val(46)}
    @AdaObjDefn{Solidus}              : @key[constant] Character := '/';  --@RI{ Character'Val(47)}

@keepnext    --@RI{ Decimal digits '0' though '9' are at positions 48 through 57}

    @AdaObjDefn{Colon}                : @key[constant] Character := ':';  --@RI{ Character'Val(58)}
    @AdaObjDefn{Semicolon}            : @key[constant] Character := ';';  --@RI{ Character'Val(59)}
    @AdaObjDefn{Less_Than_Sign}       : @key[constant] Character := '<';  --@RI{ Character'Val(60)}
    @AdaObjDefn{Equals_Sign}          : @key[constant] Character := '=';  --@RI{ Character'Val(61)}
    @AdaObjDefn{Greater_Than_Sign}    : @key[constant] Character := '>';  --@RI{ Character'Val(62)}
    @AdaObjDefn{Question}             : @key[constant] Character := '?';  --@RI{ Character'Val(63)}
    @AdaObjDefn{Commercial_At}        : @key[constant] Character := '@@';  --@RI{ Character'Val(64)}

@keepnext    --@RI{ Letters 'A' through 'Z' are at positions 65 through 90}

    @AdaObjDefn{Left_Square_Bracket}  : @key[constant] Character := '[';  --@RI{ Character'Val(91)}
    @AdaObjDefn{Reverse_Solidus}      : @key[constant] Character := '\';  --@RI{ Character'Val(92)}
    @AdaObjDefn{Right_Square_Bracket} : @key[constant] Character := ']';  --@RI{ Character'Val(93)}
    @AdaObjDefn{Circumflex}           : @key[constant] Character := '^';  --@RI{ Character'Val(94)}
    @AdaObjDefn{Low_Line}             : @key[constant] Character := '_';  --@RI{ Character'Val(95)}

    @AdaObjDefn{Grave}                : @key[constant] Character := '`';  --@RI{ Character'Val(96)}
    @AdaObjDefn{LC_A}                 : @key[constant] Character := 'a';  --@RI{ Character'Val(97)}
    @AdaObjDefn{LC_B}                 : @key[constant] Character := 'b';  --@RI{ Character'Val(98)}
    @AdaObjDefn{LC_C}                 : @key[constant] Character := 'c';  --@RI{ Character'Val(99)}
    @AdaObjDefn{LC_D}                 : @key[constant] Character := 'd';  --@RI{ Character'Val(100)}
    @AdaObjDefn{LC_E}                 : @key[constant] Character := 'e';  --@RI{ Character'Val(101)}
    @AdaObjDefn{LC_F}                 : @key[constant] Character := 'f';  --@RI{ Character'Val(102)}
    @AdaObjDefn{LC_G}                 : @key[constant] Character := 'g';  --@RI{ Character'Val(103)}
    @AdaObjDefn{LC_H}                 : @key[constant] Character := 'h';  --@RI{ Character'Val(104)}
    @AdaObjDefn{LC_I}                 : @key[constant] Character := 'i';  --@RI{ Character'Val(105)}
    @AdaObjDefn{LC_J}                 : @key[constant] Character := 'j';  --@RI{ Character'Val(106)}
    @AdaObjDefn{LC_K}                 : @key[constant] Character := 'k';  --@RI{ Character'Val(107)}
    @AdaObjDefn{LC_L}                 : @key[constant] Character := 'l';  --@RI{ Character'Val(108)}
    @AdaObjDefn{LC_M}                 : @key[constant] Character := 'm';  --@RI{ Character'Val(109)}
    @AdaObjDefn{LC_N}                 : @key[constant] Character := 'n';  --@RI{ Character'Val(110)}
    @AdaObjDefn{LC_O}                 : @key[constant] Character := 'o';  --@RI{ Character'Val(111)}

    @AdaObjDefn{LC_P}                 : @key[constant] Character := 'p';  --@RI{ Character'Val(112)}
    @AdaObjDefn{LC_Q}                 : @key[constant] Character := 'q';  --@RI{ Character'Val(113)}
    @AdaObjDefn{LC_R}                 : @key[constant] Character := 'r';  --@RI{ Character'Val(114)}
    @AdaObjDefn{LC_S}                 : @key[constant] Character := 's';  --@RI{ Character'Val(115)}
    @AdaObjDefn{LC_T}                 : @key[constant] Character := 't';  --@RI{ Character'Val(116)}
    @AdaObjDefn{LC_U}                 : @key[constant] Character := 'u';  --@RI{ Character'Val(117)}
    @AdaObjDefn{LC_V}                 : @key[constant] Character := 'v';  --@RI{ Character'Val(118)}
    @AdaObjDefn{LC_W}                 : @key[constant] Character := 'w';  --@RI{ Character'Val(119)}
    @AdaObjDefn{LC_X}                 : @key[constant] Character := 'x';  --@RI{ Character'Val(120)}
    @AdaObjDefn{LC_Y}                 : @key[constant] Character := 'y';  --@RI{ Character'Val(121)}
    @AdaObjDefn{LC_Z}                 : @key[constant] Character := 'z';  --@RI{ Character'Val(122)}
    @AdaObjDefn{Left_Curly_Bracket}   : @key[constant] Character := '{';  --@RI{ Character'Val(123)}
    @AdaObjDefn{Vertical_Line}        : @key[constant] Character := '|';  --@RI{ Character'Val(124)}
    @AdaObjDefn{Right_Curly_Bracket}  : @key[constant] Character := '}';  --@RI{ Character'Val(125)}
    @AdaObjDefn{Tilde}                : @key[constant] Character := '~';  --@RI{ Character'Val(126)}
    @AdaObjDefn{DEL}                  : @key[constant] Character := Character'Val(127);


@keepnext--@RI{ ISO 6429 control characters:}@PDefn2{term=[control character],
sec=[a category of Character]}

    @AdaObjDefn{IS4}                  : Character @key[renames] FS;
    @AdaObjDefn{IS3}                  : Character @key[renames] GS;
    @AdaObjDefn{IS2}                  : Character @key[renames] RS;
    @AdaObjDefn{IS1}                  : Character @key[renames] US;

    @AdaObjDefn{Reserved_128}         : @key[constant] Character := Character'Val(128);
    @AdaObjDefn{Reserved_129}         : @key[constant] Character := Character'Val(129);
    @AdaObjDefn{BPH}                  : @key[constant] Character := Character'Val(130);
    @AdaObjDefn{NBH}                  : @key[constant] Character := Character'Val(131);
    @AdaObjDefn{Reserved_132}         : @key[constant] Character := Character'Val(132);
    @AdaObjDefn{NEL}                  : @key[constant] Character := Character'Val(133);
    @AdaObjDefn{SSA}                  : @key[constant] Character := Character'Val(134);
    @AdaObjDefn{ESA}                  : @key[constant] Character := Character'Val(135);
    @AdaObjDefn{HTS}                  : @key[constant] Character := Character'Val(136);
    @AdaObjDefn{HTJ}                  : @key[constant] Character := Character'Val(137);
    @AdaObjDefn{VTS}                  : @key[constant] Character := Character'Val(138);
    @AdaObjDefn{PLD}                  : @key[constant] Character := Character'Val(139);
    @AdaObjDefn{PLU}                  : @key[constant] Character := Character'Val(140);
    @AdaObjDefn{RI}                   : @key[constant] Character := Character'Val(141);
    @AdaObjDefn{SS2}                  : @key[constant] Character := Character'Val(142);
    @AdaObjDefn{SS3}                  : @key[constant] Character := Character'Val(143);

    @AdaObjDefn{DCS}                  : @key[constant] Character := Character'Val(144);
    @AdaObjDefn{PU1}                  : @key[constant] Character := Character'Val(145);
    @AdaObjDefn{PU2}                  : @key[constant] Character := Character'Val(146);
    @AdaObjDefn{STS}                  : @key[constant] Character := Character'Val(147);
    @AdaObjDefn{CCH}                  : @key[constant] Character := Character'Val(148);
    @AdaObjDefn{MW}                   : @key[constant] Character := Character'Val(149);
    @AdaObjDefn{SPA}                  : @key[constant] Character := Character'Val(150);
    @AdaObjDefn{EPA}                  : @key[constant] Character := Character'Val(151);

    @AdaObjDefn{SOS}                  : @key[constant] Character := Character'Val(152);
    @AdaObjDefn{Reserved_153}         : @key[constant] Character := Character'Val(153);
    @AdaObjDefn{SCI}                  : @key[constant] Character := Character'Val(154);
    @AdaObjDefn{CSI}                  : @key[constant] Character := Character'Val(155);
    @AdaObjDefn{ST}                   : @key[constant] Character := Character'Val(156);
    @AdaObjDefn{OSC}                  : @key[constant] Character := Character'Val(157);
    @AdaObjDefn{PM}                   : @key[constant] Character := Character'Val(158);
    @AdaObjDefn{APC}                  : @key[constant] Character := Character'Val(159);

@keepnext--@RI{ Other graphic characters:}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0181-1]}
--@RI{ Character positions 160 (16#A0#) .. 175 (16#AF#):}
    @AdaObjDefn{No_Break_Space}             : @key[constant] Character := ' '; --@RI{Character'Val(160)}
    @AdaObjDefn{NBSP}                       : Character @key[renames] No_Break_Space;
    @AdaObjDefn{Inverted_Exclamation}       : @key[constant] Character := '@latin1(161)'; --@RI{Character'Val(161)}
    @AdaObjDefn{Cent_Sign}                  : @key[constant] Character := '@latin1(162)'; --@RI{Character'Val(162)}
    @AdaObjDefn{Pound_Sign}                 : @key[constant] Character := '@latin1(163)'; --@RI{Character'Val(163)}
    @AdaObjDefn{Currency_Sign}              : @key[constant] Character := '@latin1(164)'; --@RI{Character'Val(164)}
    @AdaObjDefn{Yen_Sign}                   : @key[constant] Character := '@latin1(165)'; --@RI{Character'Val(165)}
    @AdaObjDefn{Broken_Bar}                 : @key[constant] Character := '@latin1(166)'; --@RI{Character'Val(166)}
    @AdaObjDefn{Section_Sign}               : @key[constant] Character := '@latin1(167)'; --@RI{Character'Val(167)}
    @AdaObjDefn{Diaeresis}                  : @key[constant] Character := '@latin1(168)'; --@RI{Character'Val(168)}
    @AdaObjDefn{Copyright_Sign}             : @key[constant] Character := '@latin1(169)'; --@RI{Character'Val(169)}
    @AdaObjDefn{Feminine_Ordinal_Indicator} : @key[constant] Character := '@latin1(170)'; --@RI{Character'Val(170)}
    @AdaObjDefn{Left_Angle_Quotation}       : @key[constant] Character := '@latin1(171)'; --@RI{Character'Val(171)}
    @AdaObjDefn{Not_Sign}                   : @key[constant] Character := '@latin1(172)'; --@RI{Character'Val(172)}
    @AdaObjDefn{Soft_Hyphen}                : @key[constant] Character := @Chg{Version=[3],New=[Character'Val(173);],Old=['@latin1(173)'; --@RI{Character'Val(173)}]}
    @AdaObjDefn{Registered_Trade_Mark_Sign} : @key[constant] Character := '@latin1(174)'; --@RI{Character'Val(174)}
    @AdaObjDefn{Macron}                     : @key[constant] Character := '@latin1(175)'; --@RI{Character'Val(175)}

--@RI{ Character positions 176 (16#B0#) .. 191 (16#BF#):}
    @AdaObjDefn{Degree_Sign}                : @key[constant] Character := '@latin1(176)'; --@RI{Character'Val(176)}
    @AdaObjDefn{Ring_Above}                 : Character @key[renames] Degree_Sign;
    @AdaObjDefn{Plus_Minus_Sign}            : @key[constant] Character := '@latin1(177)'; --@RI{Character'Val(177)}
    @AdaObjDefn{Superscript_Two}            : @key[constant] Character := '@latin1(178)'; --@RI{Character'Val(178)}
    @AdaObjDefn{Superscript_Three}          : @key[constant] Character := '@latin1(179)'; --@RI{Character'Val(179)}
    @AdaObjDefn{Acute}                      : @key[constant] Character := '@latin1(180)'; --@RI{Character'Val(180)}
    @AdaObjDefn{Micro_Sign}                 : @key[constant] Character := '@latin1(181)'; --@RI{Character'Val(181)}
    @AdaObjDefn{Pilcrow_Sign}               : @key[constant] Character := '@latin1(182)'; --@RI{Character'Val(182)}
    @AdaObjDefn{Paragraph_Sign}             : Character @key[renames] Pilcrow_Sign;
    @AdaObjDefn{Middle_Dot}                 : @key[constant] Character := '@latin1(183)'; --@RI{Character'Val(183)}
    @AdaObjDefn{Cedilla}                    : @key[constant] Character := '@latin1(184)'; --@RI{Character'Val(184)}
    @AdaObjDefn{Superscript_One}            : @key[constant] Character := '@latin1(185)'; --@RI{Character'Val(185)}
    @AdaObjDefn{Masculine_Ordinal_Indicator}: @key[constant] Character := '@latin1(186)'; --@RI{Character'Val(186)}
    @AdaObjDefn{Right_Angle_Quotation}      : @key[constant] Character := '@latin1(187)'; --@RI{Character'Val(187)}
    @AdaObjDefn{Fraction_One_Quarter}       : @key[constant] Character := '@latin1(188)'; --@RI{Character'Val(188)}
    @AdaObjDefn{Fraction_One_Half}          : @key[constant] Character := '@latin1(189)'; --@RI{Character'Val(189)}
    @AdaObjDefn{Fraction_Three_Quarters}    : @key[constant] Character := '@latin1(190)'; --@RI{Character'Val(190)}
    @AdaObjDefn{Inverted_Question}          : @key[constant] Character := '@latin1(191)'; --@RI{Character'Val(191)}

--@RI{ Character positions 192 (16#C0#) .. 207 (16#CF#):}
    @AdaObjDefn{UC_A_Grave}                 : @key[constant] Character := '@latin1(192)'; --@RI{Character'Val(192)}
    @AdaObjDefn{UC_A_Acute}                 : @key[constant] Character := '@latin1(193)'; --@RI{Character'Val(193)}
    @AdaObjDefn{UC_A_Circumflex}            : @key[constant] Character := '@latin1(194)'; --@RI{Character'Val(194)}
    @AdaObjDefn{UC_A_Tilde}                 : @key[constant] Character := '@latin1(195)'; --@RI{Character'Val(195)}
    @AdaObjDefn{UC_A_Diaeresis}             : @key[constant] Character := '@latin1(196)'; --@RI{Character'Val(196)}
    @AdaObjDefn{UC_A_Ring}                  : @key[constant] Character := '@latin1(197)'; --@RI{Character'Val(197)}
    @AdaObjDefn{UC_AE_Diphthong}            : @key[constant] Character := '@latin1(198)'; --@RI{Character'Val(198)}
    @AdaObjDefn{UC_C_Cedilla}               : @key[constant] Character := '@latin1(199)'; --@RI{Character'Val(199)}
    @AdaObjDefn{UC_E_Grave}                 : @key[constant] Character := '@latin1(200)'; --@RI{Character'Val(200)}
    @AdaObjDefn{UC_E_Acute}                 : @key[constant] Character := '@latin1(201)'; --@RI{Character'Val(201)}
    @AdaObjDefn{UC_E_Circumflex}            : @key[constant] Character := '@latin1(202)'; --@RI{Character'Val(202)}
    @AdaObjDefn{UC_E_Diaeresis}             : @key[constant] Character := '@latin1(203)'; --@RI{Character'Val(203)}
    @AdaObjDefn{UC_I_Grave}                 : @key[constant] Character := '@latin1(204)'; --@RI{Character'Val(204)}
    @AdaObjDefn{UC_I_Acute}                 : @key[constant] Character := '@latin1(205)'; --@RI{Character'Val(205)}
    @AdaObjDefn{UC_I_Circumflex}            : @key[constant] Character := '@latin1(206)'; --@RI{Character'Val(206)}
    @AdaObjDefn{UC_I_Diaeresis}             : @key[constant] Character := '@latin1(207)'; --@RI{Character'Val(207)}

--@RI{ Character positions 208 (16#D0#) .. 223 (16#DF#):}
    @AdaObjDefn{UC_Icelandic_Eth}           : @key[constant] Character := '@latin1(208)'; --@RI{Character'Val(208)}
    @AdaObjDefn{UC_N_Tilde}                 : @key[constant] Character := '@latin1(209)'; --@RI{Character'Val(209)}
    @AdaObjDefn{UC_O_Grave}                 : @key[constant] Character := '@latin1(210)'; --@RI{Character'Val(210)}
    @AdaObjDefn{UC_O_Acute}                 : @key[constant] Character := '@latin1(211)'; --@RI{Character'Val(211)}
    @AdaObjDefn{UC_O_Circumflex}            : @key[constant] Character := '@latin1(212)'; --@RI{Character'Val(212)}
    @AdaObjDefn{UC_O_Tilde}                 : @key[constant] Character := '@latin1(213)'; --@RI{Character'Val(213)}
    @AdaObjDefn{UC_O_Diaeresis}             : @key[constant] Character := '@latin1(214)'; --@RI{Character'Val(214)}
    @AdaObjDefn{Multiplication_Sign}        : @key[constant] Character := '@latin1(215)'; --@RI{Character'Val(215)}
    @AdaObjDefn{UC_O_Oblique_Stroke}        : @key[constant] Character := '@latin1(216)'; --@RI{Character'Val(216)}
    @AdaObjDefn{UC_U_Grave}                 : @key[constant] Character := '@latin1(217)'; --@RI{Character'Val(217)}
    @AdaObjDefn{UC_U_Acute}                 : @key[constant] Character := '@latin1(218)'; --@RI{Character'Val(218)}
    @AdaObjDefn{UC_U_Circumflex}            : @key[constant] Character := '@latin1(219)'; --@RI{Character'Val(219)}
    @AdaObjDefn{UC_U_Diaeresis}             : @key[constant] Character := '@latin1(220)'; --@RI{Character'Val(220)}
    @AdaObjDefn{UC_Y_Acute}                 : @key[constant] Character := '@latin1(221)'; --@RI{Character'Val(221)}
    @AdaObjDefn{UC_Icelandic_Thorn}         : @key[constant] Character := '@latin1(222)'; --@RI{Character'Val(222)}
    @AdaObjDefn{LC_German_Sharp_S}          : @key[constant] Character := '@latin1(223)'; --@RI{Character'Val(223)}

--@RI{ Character positions 224 (16#E0#) .. 239 (16#EF#):}
    @AdaObjDefn{LC_A_Grave}                 : @key[constant] Character := '@latin1(224)'; --@RI{Character'Val(224)}
    @AdaObjDefn{LC_A_Acute}                 : @key[constant] Character := '@latin1(225)'; --@RI{Character'Val(225)}
    @AdaObjDefn{LC_A_Circumflex}            : @key[constant] Character := '@latin1(226)'; --@RI{Character'Val(226)}
    @AdaObjDefn{LC_A_Tilde}                 : @key[constant] Character := '@latin1(227)'; --@RI{Character'Val(227)}
    @AdaObjDefn{LC_A_Diaeresis}             : @key[constant] Character := '@latin1(228)'; --@RI{Character'Val(228)}
    @AdaObjDefn{LC_A_Ring}                  : @key[constant] Character := '@latin1(229)'; --@RI{Character'Val(229)}
    @AdaObjDefn{LC_AE_Diphthong}            : @key[constant] Character := '@latin1(230)'; --@RI{Character'Val(230)}
    @AdaObjDefn{LC_C_Cedilla}               : @key[constant] Character := '@latin1(231)'; --@RI{Character'Val(231)}
    @AdaObjDefn{LC_E_Grave}                 : @key[constant] Character := '@latin1(232)'; --@RI{Character'Val(232)}
    @AdaObjDefn{LC_E_Acute}                 : @key[constant] Character := '@latin1(233)'; --@RI{Character'Val(233)}
    @AdaObjDefn{LC_E_Circumflex}            : @key[constant] Character := '@latin1(234)'; --@RI{Character'Val(234)}
    @AdaObjDefn{LC_E_Diaeresis}             : @key[constant] Character := '@latin1(235)'; --@RI{Character'Val(235)}
    @AdaObjDefn{LC_I_Grave}                 : @key[constant] Character := '@latin1(236)'; --@RI{Character'Val(236)}
    @AdaObjDefn{LC_I_Acute}                 : @key[constant] Character := '@latin1(237)'; --@RI{Character'Val(237)}
    @AdaObjDefn{LC_I_Circumflex}            : @key[constant] Character := '@latin1(238)'; --@RI{Character'Val(238)}
    @AdaObjDefn{LC_I_Diaeresis}             : @key[constant] Character := '@latin1(239)'; --@RI{Character'Val(239)}

--@RI{ Character positions 240 (16#F0#) .. 255 (16#FF#):}
    @AdaObjDefn{LC_Icelandic_Eth}           : @key[constant] Character := '@latin1(240)'; --@RI{Character'Val(240)}
    @AdaObjDefn{LC_N_Tilde}                 : @key[constant] Character := '@latin1(241)'; --@RI{Character'Val(241)}
    @AdaObjDefn{LC_O_Grave}                 : @key[constant] Character := '@latin1(242)'; --@RI{Character'Val(242)}
    @AdaObjDefn{LC_O_Acute}                 : @key[constant] Character := '@latin1(243)'; --@RI{Character'Val(243)}
    @AdaObjDefn{LC_O_Circumflex}            : @key[constant] Character := '@latin1(244)'; --@RI{Character'Val(244)}
    @AdaObjDefn{LC_O_Tilde}                 : @key[constant] Character := '@latin1(245)'; --@RI{Character'Val(245)}
    @AdaObjDefn{LC_O_Diaeresis}             : @key[constant] Character := '@latin1(246)'; --@RI{Character'Val(246)}
    @AdaObjDefn{Division_Sign}              : @key[constant] Character := '@latin1(247)'; --@RI{Character'Val(247)}
    @AdaObjDefn{LC_O_Oblique_Stroke}        : @key[constant] Character := '@latin1(248)'; --@RI{Character'Val(248)}
    @AdaObjDefn{LC_U_Grave}                 : @key[constant] Character := '@latin1(249)'; --@RI{Character'Val(249)}
    @AdaObjDefn{LC_U_Acute}                 : @key[constant] Character := '@latin1(250)'; --@RI{Character'Val(250)}
    @AdaObjDefn{LC_U_Circumflex}            : @key[constant] Character := '@latin1(251)'; --@RI{Character'Val(251)}
    @AdaObjDefn{LC_U_Diaeresis}             : @key[constant] Character := '@latin1(252)'; --@RI{Character'Val(252)}
    @AdaObjDefn{LC_Y_Acute}                 : @key[constant] Character := '@latin1(253)'; --@RI{Character'Val(253)}
    @AdaObjDefn{LC_Icelandic_Thorn}         : @key[constant] Character := '@latin1(254)'; --@RI{Character'Val(254)}
    @AdaObjDefn{LC_Y_Diaeresis}             : @key[constant] Character := '@latin1(255)'; --@RI{Character'Val(255)}
@key[end] Ada.Characters.Latin_1;
@end{Example}

@end{StaticSem}

@begin{ImplPerm}
An implementation may provide additional packages as children of
Ada.Characters, to declare names for the symbols of the local character set
or other character sets.
@end{ImplPerm}

@begin{DiffWord2005}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0181-1]}
@ChgAdded{Version=[3],Text=[@b<Correction:> Soft_Hyphen is not a graphic
character, and thus a character literal for it is illegal. So we have to use the
position value. This makes no semantic change to users of the constant.]}
@end{DiffWord2005}


@RMNewPage@LabeledAddedSubClause{Version=[2],Name=[The Package Characters.Conversions]}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[The library package
Characters.Conversions has the following declaration:]}
@begin{example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[package] Ada.Characters.Conversions @key[is]@ChildUnit{Parent=[Ada.Characters],Child=[Conversions]}
   @key[pragma] Pure(Conversions);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key[function] @AdaSubDefn{Is_Character} (Item : @key[in] Wide_Character)      @key[return] Boolean;
   @key[function] @AdaSubDefn{Is_String}    (Item : @key[in] Wide_String)         @key[return] Boolean;
   @key[function] @AdaSubDefn{Is_Character} (Item : @key[in] Wide_Wide_Character) @key[return] Boolean;
   @key[function] @AdaSubDefn{Is_String}    (Item : @key[in] Wide_Wide_String)    @key[return] Boolean;
   @key[function] @AdaSubDefn{Is_Wide_Character} (Item : @key[in] Wide_Wide_Character)
      @key[return] Boolean;
   @key[function] @AdaSubDefn{Is_Wide_String}    (Item : @key[in] Wide_Wide_String)
      @key[return] Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key[function] @AdaSubDefn{To_Wide_Character} (Item : @key[in] Character) @key[return] Wide_Character;
   @key[function] @AdaSubDefn{To_Wide_String}    (Item : @key[in] String)    @key[return] Wide_String;
   @key[function] @AdaSubDefn{To_Wide_Wide_Character} (Item : @key[in] Character)
      @key[return] Wide_Wide_Character;
   @key[function] @AdaSubDefn{To_Wide_Wide_String}    (Item : @key[in] String)
      @key[return] Wide_Wide_String;
   @key[function] @AdaSubDefn{To_Wide_Wide_Character} (Item : @key[in] Wide_Character)
      @key[return] Wide_Wide_Character;
   @key[function] @AdaSubDefn{To_Wide_Wide_String}    (Item : @key[in] Wide_String)
      @key[return] Wide_Wide_String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key[function] @AdaSubDefn{To_Character} (Item       : @key[in] Wide_Character;
                         Substitute : @key[in] Character := ' ')
      @key[return] Character;
   @key[function] @AdaSubDefn{To_String}    (Item       : @key[in] Wide_String;
                          Substitute : @key[in] Character := ' ')
      @key[return] String;
   @key[function] @AdaSubDefn{To_Character} (Item :       @key[in] Wide_Wide_Character;
                          Substitute : @key[in] Character := ' ')
      @key[return] Character;
   @key[function] @AdaSubDefn{To_String}    (Item :       @key[in] Wide_Wide_String;
                          Substitute : @key[in] Character := ' ')
      @key[return] String;
   @key[function] @AdaSubDefn{To_Wide_Character} (Item :       @key[in] Wide_Wide_Character;
                               Substitute : @key[in] Wide_Character := ' ')
      @key[return] Wide_Character;
   @key[function] @AdaSubDefn{To_Wide_String}    (Item :       @key[in] Wide_Wide_String;
                               Substitute : @key[in] Wide_Character := ' ')
      @key[return] Wide_String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[end] Ada.Characters.Conversions;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Text=[The functions in package Characters.Conversions
test Wide_Wide_Character or Wide_Character values for membership in
Wide_Character or Character, or convert between corresponding characters of
Wide_Wide_Character, Wide_Character, and Character.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] Is_Character (Item : @key[in] Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns True if Wide_Character'Pos(Item) <= Character'Pos(Character'Last).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] Is_Character (Item : @key[in] Wide_Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns True if Wide_Wide_Character'Pos(Item) <= Character'Pos(Character'Last).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] Is_Wide_Character (Item : @key[in] Wide_Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns True if Wide_Wide_Character'Pos(Item) <=
Wide_Character'Pos(Wide_Character'Last).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] Is_String (Item : @key[in] Wide_String)      @key[return] Boolean;
@key[function] Is_String (Item : @key[in] Wide_Wide_String) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns True if Is_Character(Item(I)) is True for each I in Item'Range.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] Is_Wide_String (Item : @key[in] Wide_Wide_String) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns True if Is_Wide_Character(Item(I)) is True for each I in Item'Range.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] To_Character (Item :       @key[in] Wide_Character;
                       Substitute : @key[in] Character := ' ') @key[return] Character;
@key[function] To_Character (Item :       @key[in] Wide_Wide_Character;
                       Substitute : @key[in] Character := ' ') @key[return] Character;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the Character corresponding to Item if Is_Character(Item), and returns
the Substitute Character otherwise.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] To_Wide_Character (Item : @key[in] Character) @key[return] Wide_Character;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the Wide_Character X such that Character'Pos(Item) = Wide_Character'Pos
(X).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] To_Wide_Character (Item :       @key[in] Wide_Wide_Character;
                            Substitute : @key[in] Wide_Character := ' ')
   @key[return] Wide_Character;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the Wide_Character corresponding to Item if Is_Wide_Character(Item),
and returns the Substitute Wide_Character otherwise.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] To_Wide_Wide_Character (Item : @key[in] Character)
   @key[return] Wide_Wide_Character;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the Wide_Wide_Character X such that Character'Pos(Item) =
Wide_Wide_Character'Pos (X).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] To_Wide_Wide_Character (Item : @key[in] Wide_Character)
   @key[return] Wide_Wide_Character;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the Wide_Wide_Character X such that Wide_Character'Pos(Item) =
Wide_Wide_Character'Pos (X).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] To_String (Item :       @key[in] Wide_String;
                    Substitute : @key[in] Character := ' ') @key[return] String;
@key[function] To_String (Item :       @key[in] Wide_Wide_String;
                    Substitute : @key[in] Character := ' ') @key[return] String;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the String whose range is 1..Item'Length and each of whose elements is
given by To_Character of the corresponding element in Item.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] To_Wide_String (Item : @key[in] String) @key[return] Wide_String;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the Wide_String whose range is 1..Item'Length and each of whose
elements is given by To_Wide_Character of the corresponding element in Item.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[function] To_Wide_String (Item :       @key[in] Wide_Wide_String;
                         Substitute : @key[in] Wide_Character := ' ')
   @key[return] Wide_String;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the Wide_String whose range is 1..Item'Length and each of whose
elements is given by To_Wide_Character of the corresponding element in Item
with the given Substitute Wide_Character.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key[function] To_Wide_Wide_String (Item : @key[in] String) @key[return] Wide_Wide_String;
@key[function] To_Wide_Wide_String (Item : @key[in] Wide_String)
   @key[return] Wide_Wide_String;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the Wide_Wide_String whose range is 1..Item'Length and each of whose
elements is given by To_Wide_Wide_Character of the corresponding element in
Item.]}

@end{DescribeCode}
@end{StaticSem}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The package Characters.Conversions is new, replacing functions
  previously found in Characters.Handling.]}
@end{Extend95}


@LabeledAddedSubClause{Version=[3],Name=[The Package Wide_Characters.Handling]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Text=[The package Wide_Characters.Handling provides
operations for classifying Wide_Characters and case folding for Wide_Characters.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Leading],Keepnext=[T],Text=[The library package
Wide_Characters.Handling has the following declaration:]}

@begin{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[package] Ada.Wide_Characters.Handling @key[is]@ChildUnit{Parent=[Ada.Wide_Characters],Child=[Handling]}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Control} (Item : Wide_Character) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Letter} (Item : Wide_Character) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Lower} (Item : Wide_Character) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Upper} (Item : Wide_Character) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Digit} (Item : Wide_Character) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Decimal_Digit} (Item : Wide_Character) @key[return] Boolean
      @key[renames] Is_Digit;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Hexadecimal_Digit} (Item : Wide_Character) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Alphanumeric} (Item : Wide_Character) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Special} (Item : Wide_Character) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Line_Terminator} (Item : Wide_Character) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Mark} (Item : Wide_Character) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Other_Format} (Item : Wide_Character) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Punctuation_Connector} (Item : Wide_Character) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Space} (Item : Wide_Character) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Is_Graphic} (Item : Wide_Character) @key[return] Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{To_Lower} (Item : Wide_Character) @key[return] Wide_Character;
   @key[function] @AdaSubDefn{To_Upper} (Item : Wide_Character) @key[return] Wide_Character;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{To_Lower} (Item : Wide_String) @key[return] Wide_String;
   @key[function] @AdaSubDefn{To_Upper} (Item : Wide_String) @key[return] Wide_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[end] Ada.Wide_Characters.Handling;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Text=[The subprograms defined in Wide_Characters.Handling are locale independent.]}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Is_Control (Item : Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the Wide_Character
designated by Item is categorized as @ntf{other_control}, otherwise returns False.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Is_Letter (Item : Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the Wide_Character
designated by Item is categorized as @ntf{letter_uppercase},
@ntf{letter_lowercase}, @ntf{letter_titlecase}, @ntf{letter_modifier},
@ntf{letter_other}, or @ntf{number_letter}; otherwise returns False.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Is_Lower (Item : Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the Wide_Character
designated by Item is categorized as @ntf{letter_lowercase}, otherwise returns
False.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Is_Upper (Item : Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the Wide_Character
designated by Item is categorized as @ntf{letter_uppercase}, otherwise returns
False.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Is_Digit (Item : Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the Wide_Character
designated by Item is categorized as @ntf{number_decimal}, otherwise returns
False.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Is_Hexadecimal_Digit (Item : Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the Wide_Character
designated by Item is categorized as @ntf{number_decimal}, or is in the range
'A' .. 'F' or 'a' .. 'f', otherwise returns False.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Is_Alphanumeric (Item : Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the Wide_Character
designated by Item is categorized as @ntf{letter_uppercase},
@ntf{letter_lowercase}, @ntf{letter_titlecase}, @ntf{letter_modifier},
@ntf{letter_other}, @ntf{number_letter}, or @ntf{number_decimal}; otherwise
returns False.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Is_Special (Item : Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the Wide_Character
designated by Item is categorized as @ntf{graphic_character}, but not categorized as
@ntf{letter_uppercase}, @ntf{letter_lowercase}, @ntf{letter_titlecase},
@ntf{letter_modifier}, @ntf{letter_other}, @ntf{number_letter}, or
@ntf{number_decimal}; otherwise returns False.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Is_Line_Terminator (Item : Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the Wide_Character
designated by Item is categorized as @ntf{separator_line} or
@ntf{separator_paragraph}, or if Item is a conventional line terminator
character (Line_Feed, Line_Tabulation, Form_Feed,
Carriage_Return, Next_Line); otherwise returns False.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Is_Mark (Item : Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the Wide_Character
designated by Item is categorized as @ntf{mark_non_spacing} or
@ntf{mark_spacing_combining}, otherwise returns False.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Is_Other_Format (Item : Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the Wide_Character
designated by Item is categorized as @ntf{other_format}, otherwise returns False.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Is_Punctuation_Connector (Item : Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the Wide_Character
designated by Item is categorized as @ntf{punctuation_connector}, otherwise
returns False.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Is_Space (Item : Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the Wide_Character
designated by Item is categorized as @ntf{separator_space}, otherwise returns
False.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Is_Graphic (Item : Wide_Character) @key[return] Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns True if the Wide_Character
designated by Item is categorized as @ntf{graphic_character}, otherwise returns
False.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] To_Lower (Item : Wide_Character) @key[return] Wide_Character;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the Simple Lowercase Mapping
as defined by documents referenced in the note in section 1 of ISO/IEC 10646:2003
of the Wide_Character designated by Item. If the Simple Lowercase Mapping does
not exist for the Wide_Character designated by Item, then the value of Item is
returned.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The case mappings come from Unicode as
  ISO/IEC 10646:2003 does not include case mappings (but rather references
  the Unicode ones as above).]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] To_Lower (Item : Wide_String) @key[return] Wide_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the result of applying the
To_Lower conversion to each Wide_Character element of the
Wide_String designated by Item. The result is the null Wide_String if the value
of the formal parameter is the null Wide_String. The lower bound of the result
Wide_String is 1.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] To_Upper (Item : Wide_Character) @key[return] Wide_Character;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the Simple Uppercase Mapping
as defined by documents referenced in the note in section 1 of ISO/IEC 10646:2003
of the Wide_Character designated by Item. If the Simple Uppercase
Mapping does not exist for the Wide_Character designated by Item, then the value
of Item is returned.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] To_Upper (Item : Wide_String) @key[return] Wide_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the result of applying the
To_Upper conversion to each Wide_Character element of the
Wide_String designated by Item. The result is the null Wide_String if the value
of the formal parameter is the null Wide_String. The lower bound of the result
Wide_String is 1.]}

@end{DescribeCode}
@end{StaticSem}


@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The package Wide_Characters.Handling is new.]}
@end{Extend2005}


@LabeledAddedSubClause{Version=[3],Name=[The Package Wide_Wide_Characters.Handling]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
@ChgAdded{Version=[3],Text=[The package Wide_Wide_Characters.Handling
@ChildUnit{Parent=[Ada.Wide_Wide_Characters],Child=[Handling]}has the same contents as
Wide_Characters.Handling except that each occurrence of Wide_Character is
replaced by Wide_Wide_Character, and each occurrence of Wide_String is replaced
by Wide_Wide_String.]}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0185-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The package Wide_Wide_Characters.Handling is new.]}
@end{Extend2005}



