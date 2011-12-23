@comment{ $Source: e:\\cvsroot/ARM/Source/pre_strings.mss,v $ }
@comment{ $Revision: 1.71 $ $Date: 2011/11/01 23:14:15 $ $Author: randy $ }
@Part(predefstrings, Root="ada.mss")
@Comment{$Date: 2011/11/01 23:14:15 $}

@RMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledClause{String Handling}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
This clause presents the specifications of the package Strings and
several child packages, which provide facilities for dealing with
 string data. Fixed-length,
bounded-length, and unbounded-length strings are supported, for @Chg{Version=[2],New=[],Old=[both]}
String@Chg{Version=[2],New=[,],Old=[ and]} Wide_String@Chg{Version=[2],New=[,
and Wide_Wide_String],Old=[]}.
The string-handling subprograms include searches for pattern strings
and for characters in program-specified sets,
translation (via a character-to-character mapping), and transformation
(replacing, inserting, overwriting, and deleting of substrings).
@end{Intro}

@begin{Extend83}
@Defn{extensions to Ada 83}
This clause is new to Ada 95.
@end{Extend83}

@begin{Diffword95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[Included Wide_Wide_String in this description;
  the individual changes are documented as extensions as needed.]}
@end{Diffword95}


@LabeledSubClause{The Package Strings}
@begin{Intro}
The package Strings provides declarations
common to the string handling packages.
@end{Intro}

@begin{StaticSem}
@Leading@;The library package Strings has the following declaration:
@begin{example}
@ChildUnit{Parent=[Ada],Child=[Strings]}@key[package] Ada.Strings @key[is]
   @key[pragma] Pure(Strings);

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
   @AdaObjDefn{Space}      : @key[constant] Character      := ' ';
   @AdaObjDefn{Wide_Space} : @key[constant] Wide_Character := ' ';@Chg{Version=[2],New=[
   @AdaObjDefn{Wide_Wide_Space} : @key[constant] Wide_Wide_Character := ' ';],Old=[]}

   @AdaExcDefn{Length_Error}, @AdaExcDefn{Pattern_Error}, @AdaExcDefn{Index_Error}, @AdaExcDefn{Translation_Error} : @key[exception];

   @key[type] @AdaTypeDefn{Alignment}  @key[is] (Left, Right, Center);
   @key[type] @AdaTypeDefn{Truncation} @key[is] (Left, Right, Error);
   @key[type] @AdaTypeDefn{Membership} @key[is] (Inside, Outside);
   @key[type] @AdaTypeDefn{Direction}  @key[is] (Forward, Backward);
   @key[type] @AdaTypeDefn{Trim_End}   @key[is] (Left, Right, Both);
@key[end] Ada.Strings;
@end{example}
@end{StaticSem}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  Constant Wide_Wide_Space is newly added to Ada.Strings. If Ada.Strings is
  referenced in a @nt{use_clause}, and an entity @i<E> with a
  @nt{defining_identifier} of Wide_Wide_Space is defined in a package that is
  also referenced in a @nt{use_clause}, the entity @i<E> may no longer be
  use-visible, resulting in errors. This should be rare and is easily fixed if
  it does occur.]}
@end{Incompatible95}



@RMNewPageVer{Version=[1]}@Comment{Insert page break so printed Ada 95 w/ Corr RM looks better.}
@LabeledSubClause{The Package Strings.Maps}
@begin{Intro}
The package Strings.Maps defines the types, operations, and other
entities needed for character sets and character-to-character mappings.
@end{Intro}

@begin{StaticSem}
@Leading@;The library package Strings.Maps has the following declaration:
@begin{example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00362-01]}
@ChildUnit{Parent=[Ada.Strings],Child=[Maps]}@key[package] Ada.Strings.Maps @key[is]
   @key[pragma] @Chg{Version=[2],New=[Pure],Old=[Preelaborate]}(Maps);

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00161-01]}
   --@Examcom{ Representation for a set of character values:}
   @key[type] @AdaTypeDefn{Character_Set} @key[is] @key[private];@Chg{Version=[2],New=[
   @key[pragma] Preelaborable_Initialization(Character_Set);],Old=[]}

   @AdaObjDefn{Null_Set} : @key[constant] Character_Set;

   @key[type] @AdaTypeDefn{Character_Range} @key[is]
     @Key[record]
        Low  : Character;
        High : Character;
     @key[end] @key[record];
   -- @Examcom[Represents Character range Low..High]

   @key[type] @AdaTypeDefn{Character_Ranges} @key[is] @key[array] (Positive @key[range] <>) @key[of] Character_Range;

   @key[function] @AdaSubDefn{To_Set}    (Ranges : @key[in] Character_Ranges)@key[return] Character_Set;

   @key[function] @AdaSubDefn{To_Set}    (Span   : @key[in] Character_Range)@key[return] Character_Set;

   @key[function] @AdaSubDefn{To_Ranges} (Set    : @key[in] Character_Set)  @key[return] Character_Ranges;

   @key[function] "="   (Left, Right : @key[in] Character_Set) @key[return] Boolean;

   @key[function] "@key[not]" (Right : @key[in] Character_Set)       @key[return] Character_Set;
   @key[function] "@key[and]" (Left, Right : @key[in] Character_Set) @key[return] Character_Set;
   @key[function] "@key[or]"  (Left, Right : @key[in] Character_Set) @key[return] Character_Set;
   @key[function] "@key[xor]" (Left, Right : @key[in] Character_Set) @key[return] Character_Set;
   @key[function] "-"   (Left, Right : @key[in] Character_Set) @key[return] Character_Set;

   @key[function] @AdaSubDefn{Is_In} (Element : @key[in] Character;
                   Set     : @key[in] Character_Set)
      @key[return] Boolean;

   @key[function] @AdaSubDefn{Is_Subset} (Elements : @key[in] Character_Set;
                       Set      : @key[in] Character_Set)
      @key[return] Boolean;

   @key[function] "<=" (Left  : @key[in] Character_Set;
                  Right : @key[in] Character_Set)
      @key[return] Boolean @key[renames] Is_Subset;


   --@Examcom{ Alternative representation for a set of character values:}
   @key[subtype] @AdaSubtypeDefn{Name=[Character_Sequence],Of=[String]} @key[is] String;

   @key[function] @AdaSubDefn{To_Set} (Sequence  : @key[in] Character_Sequence)@key[return] Character_Set;

   @key[function] @AdaSubDefn{To_Set} (Singleton : @key[in] Character)     @key[return] Character_Set;

   @key[function] @AdaSubDefn{To_Sequence} (Set  : @key[in] Character_Set) @key[return] Character_Sequence;


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00161-01]}
   --@Examcom{ Representation for a character to character mapping:}
   @key[type] @AdaTypeDefn{Character_Mapping} @key[is] @key[private];@Chg{Version=[2],New=[
   @key[pragma] Preelaborable_Initialization(Character_Mapping);],Old=[]}

   @key[function] @AdaSubDefn{Value} (Map     : @key[in] Character_Mapping;
                   Element : @key[in] Character)
      @key[return] Character;

   @AdaObjDefn{Identity} : @key[constant] Character_Mapping;

   @key[function] @AdaSubDefn{To_Mapping} (From, To : @key[in] Character_Sequence)
      @key[return] Character_Mapping;

   @key[function] @AdaSubDefn{To_Domain} (Map : @key[in] Character_Mapping)
      @key[return] Character_Sequence;
   @key[function] @AdaSubDefn{To_Range}  (Map : @key[in] Character_Mapping)
      @key[return] Character_Sequence;

   @key{type} @AdaTypeDefn{Character_Mapping_Function} @key{is}
      @key{access} @key{function} (From : @key{in} Character) @key{return} Character;

@key[private]
   ... -- @Examcom{not specified by the language}
@key[end] Ada.Strings.Maps;
@end{example}

An object of type Character_Set represents a set of characters.

Null_Set represents the set containing no characters.

An object Obj of type Character_Range represents the set of characters
in the range Obj.Low .. Obj.High.

An object Obj of type Character_Ranges represents the
union of the sets corresponding to Obj(I) for I in Obj'Range.
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[function] To_Set (Ranges : @key[in] Character_Ranges) @key[return] Character_Set;
@end{Example}
@Trailing@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
If Ranges'Length=0 then Null_Set is returned;
otherwise@Chg{Version=[3],New=[,],Old=[]}
the returned value represents the set corresponding to Ranges.

@begin{Example}@Keepnext
@key[function] To_Set (Span : @key[in] Character_Range) @key[return] Character_Set;
@end{Example}

The returned value represents the set containing each character in Span.
@begin{Example}@Keepnext
@key[function] To_Ranges (Set : @key[in] Character_Set) @key[return] Character_Ranges;
@end{Example}
@Trailing@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
If Set = Null_Set@Chg{Version=[3],New=[,],Old=[]}
then an empty Character_Ranges array is returned;
otherwise@Chg{Version=[3],New=[,],Old=[]} the shortest array of contiguous
ranges of Character values in Set, in increasing order of Low, is returned.

@begin{Example}@Keepnext
@key[function] "=" (Left, Right : @key[in] Character_Set) @key[return] Boolean;
@end{Example}
@Trailing@;The function "=" returns True if Left and Right represent identical sets,
and False otherwise.
@end{DescribeCode}

@Trailing@;Each of the logical operators "@key[not]", "@key[and]", "@key[or]",
and "@key[xor]" returns
a Character_Set value that represents the set obtained by applying
the corresponding operation to the set(s) represented by the parameter(s)
of the operator.
"@en"(Left, Right) is equivalent to "and"(Left, "not"(Right)).
@begin{reason}
The set minus operator is provided for efficiency.@end{reason}

@begin{DescribeCode}
@begin{Example}@Keepnext
@key[function] Is_In (Element : @key[in] Character;
                Set     : @key[in] Character_Set);
   @key[return] Boolean;
@end{Example}
@Trailing@;Is_In returns True if Element is in Set, and False otherwise.

@begin{Example}@Keepnext
@key[function] Is_Subset (Elements : @key[in] Character_Set;
                    Set      : @key[in] Character_Set)
   @key[return] Boolean;
@end{Example}
@Trailing@;Is_Subset returns True if
Elements is a subset of Set, and False otherwise.

@begin{Example}@Keepnext
@key[subtype] Character_Sequence @key[is] String;
@end{Example}
@Trailing@;The Character_Sequence subtype is used to portray a set of character
values and also to identify the domain and range of a character
mapping.
@begin{reason}
Although a named subtype is redundant @em the predefined type String
could have been used for the parameter to To_Set and To_Mapping
below @em the use of a differently named subtype identifies the intended
purpose of the parameter.
@end{reason}

@begin{Example}@Keepnext
@key[function] To_Set (Sequence  : @key[in] Character_Sequence) @key[return] Character_Set;@*
@key[function] To_Set (Singleton : @key[in] Character)          @key[return] Character_Set;
@end{Example}
@Trailing@;Sequence portrays the set of character values that it explicitly
contains (ignoring duplicates).
Singleton portrays the set comprising a single Character.
Each of the To_Set functions
returns a Character_Set value that represents
the set portrayed by Sequence or Singleton.

@begin{Example}@Keepnext
@key[function] To_Sequence (Set : @key[in] Character_Set) @key[return] Character_Sequence;
@end{Example}
@Trailing@;The function To_Sequence returns a Character_Sequence value
containing each of the characters in the set represented by Set, in
ascending order with no duplicates.

@begin{Example}@Keepnext
@key[type] Character_Mapping @key[is] @key[private];
@end{Example}
@Trailing@;An object of type Character_Mapping represents a
Character-to-Character mapping.

@begin{Example}@Keepnext
@key[function] Value (Map     : @key[in] Character_Mapping;
                Element : @key[in] Character)
   @key[return] Character;
@end{Example}
@Trailing@;The function Value returns the Character value to which Element maps
with respect to the mapping represented by Map.
@end{DescribeCode}

@Defn2{Term=[match], Sec=(a character to a pattern character)}
A character C @i{matches} a pattern character P
with respect to a given Character_Mapping value Map if
Value(Map, C) = P.
@Defn2{Term=[match], Sec=(a string to a pattern string)}
A string S @i{matches} a pattern string P with respect to a
given Character_Mapping if their lengths are the same
and if each character in S matches its corresponding character in
the pattern string P.
@begin{Discussion}
  In an earlier version of the string handling packages,
  the definition of matching was symmetrical, namely
  C matches P if Value(Map,C) = Value(Map,P).
 However, applying the mapping
  to the pattern was confusing according to some reviewers.
  Furthermore, if the symmetrical version is needed, it can
  be achieved by applying the mapping to the pattern (via translation) prior to
  passing it as a parameter.
@end{Discussion}

String handling subprograms that deal with character mappings have
parameters whose type is Character_Mapping.
@begin{DescribeCode}
@begin{Example}@Keepnext
Identity : @key[constant] Character_Mapping;
@end{Example}
  @Trailing@;Identity maps each Character to itself.

@begin{Example}@Keepnext
@key[function] To_Mapping (From, To : @key[in] Character_Sequence)
    @key[return] Character_Mapping;
@end{Example}
  @Trailing@;To_Mapping produces a Character_Mapping such that
  each element of From maps to the corresponding element of To,
  and each other character maps to itself.
    If From'Length /= To'Length, or
     if some character is repeated in From, then Translation_Error
     is propagated.

@begin{Example}@Keepnext
@key[function] To_Domain (Map : @key[in] Character_Mapping) @key[return] Character_Sequence;
@end{Example}
@Trailing@;To_Domain returns the shortest Character_Sequence value D such that
each character not in D maps to itself, and such that
the characters in D are in ascending order.
The lower bound of D is 1.

@begin{Example}@Keepnext
@key[function] To_Range  (Map : @key[in] Character_Mapping) @key[return] Character_Sequence;
@end{Example}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0048],ARef=[AI95-00151-01]}
@Trailing@;To_Range returns the Character_Sequence value R,
@Chg{New=[],Old=[with lower bound 1 and upper bound Map'Length,]}
such that if D = To_Domain(Map)@Chg{New=[, then R has the same bounds as D,
and],Old=[ then]} D(I) maps to R(I) for each I in D'Range.
@end{DescribeCode}

An object F of type Character_Mapping_Function maps a Character
value C to the Character value F.@key{all}(C), which is said to
@i{match} C with respect to mapping function F.
@Defn2[term=<match>,sec=<a character to a pattern character, with
respect to a character mapping function>]
@end{StaticSem}

@begin{Notes}
Character_Mapping and Character_Mapping_Function
are used both for character equivalence
mappings in the search subprograms (such as for case insensitivity) and
as transformational mappings in the Translate subprograms.

To_Domain(Identity) and To_Range(Identity) each returns the null string.
@begin{Reason}
Package Strings.Maps is not pure, since it declares an
access-to-subprogram type.
@end{Reason}
@end{Notes}

@begin{Examples}
To_Mapping("ABCD", "ZZAB") returns a Character_Mapping that maps 'A'
and 'B' to 'Z', 'C' to 'A', 'D' to 'B', and each other Character to
itself.
@end{Examples}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00161-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @b[Amendment Correction:] Added @nt{pragma} Preelaborable_Initialization to
  types Character_Set and Character_Mapping, so that they can be used
  to declare default-initialized objects in preelaborated units.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00362-01]}
  @ChgAdded{Version=[2],Text=[Strings.Maps is now Pure,
  so it can be used in pure units.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0048],ARef=[AI95-00151-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the definition of
  the range of the result of To_Range, since the Ada 95 definition makes no
  sense.]}
@end{DiffWord95}


@LabeledSubClause{Fixed-Length String Handling}
@begin{Intro}
The language-defined package Strings.Fixed provides string-handling subprograms
 for fixed-length strings;
that is, for values of type Standard.String.
Several of these subprograms are procedures that modify the contents of
a String that is passed as an @key[out] or an @key[in] @key[out] parameter;
 each has additional
parameters to control the effect when the logical length of the result
differs from the parameter's length.

For each function that returns a String, the lower bound of the returned
value is 1.
@begin{Discussion}
@Chgref{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
Most operations that @Chg{Version=[2],New=[yield],Old=[yields]} a String
are provided both as a
function and as a procedure. The functional form is possibly a more aesthetic
style but may introduce overhead due to extra copying or dynamic memory
usage in some implementations. Thus a procedural form, with an @key[in]
@key[out] parameter so that all copying is done `in place', is also
supplied.@end{discussion}

The basic model embodied in the package is that a fixed-length string
comprises significant characters and possibly padding
(with space characters)
on either or both
ends. When a shorter string is copied to a longer string, padding
is inserted, and when a longer string is copied to a shorter one,
padding is stripped. The Move procedure in Strings.Fixed, which takes a
String as an @key[out] parameter, allows the programmer to control these
effects. Similar control is provided by the string transformation
procedures.
@end{Intro}

@begin{StaticSem}
@Leading@keepnext
@Leading@;The library package Strings.Fixed has the following declaration:
@begin{example}
@key[with] Ada.Strings.Maps;
@ChildUnit{Parent=[Ada.Strings],Child=[Fixed]}@key[package] Ada.Strings.Fixed @key[is]
   @key[pragma] Preelaborate(Fixed);


--@Examcom{ "Copy" procedure for strings of possibly different lengths}

   @key[procedure] @AdaSubDefn{Move} (Source  : @key[in]  String;
                   Target  : @key[out] String;
                   Drop    : @key[in]  Truncation := Error;
                   Justify : @key[in]  Alignment  := Left;
                   Pad     : @key[in]  Character  := Space);


--@Examcom{ Search subprograms}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[   @key[function] @AdaSubDefn{Index} (Source  : @key[in] String;
                   Pattern : @key[in] String;
                   From    : @key[in] Positive;
                   Going   : @key[in] Direction := Forward;
                   Mapping : @key[in] Maps.Character_Mapping := Maps.Identity)
      @key[return] Natural;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[   @key[function] @AdaSubDefn{Index} (Source  : @key[in] String;
                   Pattern : @key[in] String;
                   From    : @key[in] Positive;
                   Going   : @key[in] Direction := Forward;
                   Mapping : @key[in] Maps.Character_Mapping_Function)
      @key[return] Natural;]}

   @key[function] @AdaSubDefn{Index} (Source   : @key[in] String;
                   Pattern  : @key[in] String;
                   Going    : @key[in] Direction := Forward;
                   Mapping  : @key[in] Maps.Character_Mapping
                                := Maps.Identity)
      @key[return] Natural;

   @key[function] @AdaSubDefn{Index} (Source   : @key[in] String;
                   Pattern  : @key[in] String;
                   Going    : @key[in] Direction := Forward;
                   Mapping  : @key[in] Maps.Character_Mapping_Function)
      @key[return] Natural;

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[   @key[function] @AdaSubDefn{Index} (Source  : @key[in] String;
                   Set     : @key[in] Maps.Character_Set;
                   From    : @key[in] Positive;
                   Test    : @key[in] Membership := Inside;
                   Going   : @key[in] Direction := Forward)
      @key[return] Natural;]}

   @key[function] @AdaSubDefn{Index} (Source : @key[in] String;
                   Set    : @key[in] Maps.Character_Set;
                   Test   : @key[in] Membership := Inside;
                   Going  : @key[in] Direction  := Forward)
      @key[return] Natural;


@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[   @key[function] @AdaSubDefn{Index_Non_Blank} (Source : @key[in] String;
                             From   : @key[in] Positive;
                             Going  : @key[in] Direction := Forward)
      @key[return] Natural;]}

   @key[function] @AdaSubDefn{Index_Non_Blank} (Source : @key[in] String;
                             Going  : @key[in] Direction := Forward)
      @key[return] Natural;


   @key[function] @AdaSubDefn{Count} (Source   : @key[in] String;
                   Pattern  : @key[in] String;
                   Mapping  : @key[in] Maps.Character_Mapping
                                 := Maps.Identity)
      @key[return] Natural;

   @key[function] @AdaSubDefn{Count} (Source   : @key[in] String;
                   Pattern  : @key[in] String;
                   Mapping  : @key[in] Maps.Character_Mapping_Function)
      @key[return] Natural;

   @key[function] @AdaSubDefn{Count} (Source   : @key[in] String;
                   Set      : @key[in] Maps.Character_Set)
      @key[return] Natural;


@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0031-1]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Find_Token} (Source : @key[in] String;
                         Set    : @key[in] Maps.Character_Set;
                         From   : @key[in] Positive;
                         Test   : @key[in] Membership;
                         First  : @key[out] Positive;
                         Last   : @key[out] Natural);]}

   @key[procedure] @AdaSubDefn{Find_Token} (Source : @key[in] String;
                         Set    : @key[in] Maps.Character_Set;
                         Test   : @key[in] Membership;
                         First  : @key[out] Positive;
                         Last   : @key[out] Natural);


@Keepnext@;--@Examcom{ String translation subprograms}

   @key[function] @AdaSubDefn{Translate} (Source  : @key[in] String;
                       Mapping : @key[in] Maps.Character_Mapping)
      @key[return] String;

   @key[procedure] @AdaSubDefn{Translate} (Source  : @key[in] @key[out] String;
                        Mapping : @key[in] Maps.Character_Mapping);


   @key[function] @AdaSubDefn{Translate} (Source  : @key[in] String;
                       Mapping : @key[in] Maps.Character_Mapping_Function)
      @key[return] String;

   @key[procedure] @AdaSubDefn{Translate} (Source  : @key[in] @key[out] String;
                        Mapping : @key[in] Maps.Character_Mapping_Function);

@Keepnext@;--@Examcom{ String transformation subprograms}

   @key[function] @AdaSubDefn{Replace_Slice} (Source   : @key[in] String;
                           Low      : @key[in] Positive;
                           High     : @key[in] Natural;
                           By       : @key[in] String)
      @key[return] String;

   @key[procedure] @AdaSubDefn{Replace_Slice} (Source   : @key[in] @key[out] String;
                            Low      : @key[in] Positive;
                            High     : @key[in] Natural;
                            By       : @key[in] String;
                            Drop     : @key[in] Truncation := Error;
                            Justify  : @key[in] Alignment  := Left;
                            Pad      : @key[in] Character  := Space);


   @key[function] @AdaSubDefn{Insert} (Source   : @key[in] String;
                    Before   : @key[in] Positive;
                    New_Item : @key[in] String)
      @key[return] String;

   @key[procedure] @AdaSubDefn{Insert} (Source   : @key[in] @key[out] String;
                     Before   : @key[in] Positive;
                     New_Item : @key[in] String;
                     Drop     : @key[in] Truncation := Error);


   @key[function] @AdaSubDefn{Overwrite} (Source   : @key[in] String;
                       Position : @key[in] Positive;
                       New_Item : @key[in] String)
      @key[return] String;

   @key[procedure] @AdaSubDefn{Overwrite} (Source   : @key[in] @key[out] String;
                        Position : @key[in] Positive;
                        New_Item : @key[in] String;
                        Drop     : @key[in] Truncation := Right);


   @key[function] @AdaSubDefn{Delete} (Source  : @key[in] String;
                    From    : @key[in] Positive;
                    Through : @key[in] Natural)
      @key[return] String;

   @key[procedure] @AdaSubDefn{Delete} (Source  : @key[in] @key[out] String;
                     From    : @key[in] Positive;
                     Through : @key[in] Natural;
                     Justify : @key[in] Alignment := Left;
                     Pad     : @key[in] Character := Space);

 --@Examcom{String selector subprograms}
   @key[function] @AdaSubDefn{Trim} (Source : @key[in] String;
                  Side   : @key[in] Trim_End)
      @key[return] String;

   @key[procedure] @AdaSubDefn{Trim} (Source  : @key[in] @key[out] String;
                   Side    : @key[in] Trim_End;
                   Justify : @key[in] Alignment := Left;
                   Pad     : @key[in] Character := Space);

   @key[function] @AdaSubDefn{Trim} (Source : @key[in] String;
                  Left   : @key[in] Maps.Character_Set;
                  Right  : @key[in] Maps.Character_Set)
      @key[return] String;

   @key[procedure] @AdaSubDefn{Trim} (Source  : @key[in] @key[out] String;
                   Left    : @key[in] Maps.Character_Set;
                   Right   : @key[in] Maps.Character_Set;
                   Justify : @key[in] Alignment := Strings.Left;
                   Pad     : @key[in] Character := Space);


   @key[function] @AdaSubDefn{Head} (Source : @key[in] String;
                  Count  : @key[in] Natural;
                  Pad    : @key[in] Character := Space)
      @key[return] String;

   @key[procedure] @AdaSubDefn{Head} (Source  : @key[in] @key[out] String;
                   Count   : @key[in] Natural;
                   Justify : @key[in] Alignment := Left;
                   Pad     : @key[in] Character := Space);

   @key[function] @AdaSubDefn{Tail} (Source : @key[in] String;
                  Count  : @key[in] Natural;
                  Pad    : @key[in] Character := Space)
      @key[return] String;

   @key[procedure] @AdaSubDefn{Tail} (Source  : @key[in] @key[out] String;
                   Count   : @key[in] Natural;
                   Justify : @key[in] Alignment := Left;
                   Pad     : @key[in] Character := Space);

@keepnext@;--@Examcom{String constructor functions}

   @key[function] "*" (Left  : @key[in] Natural;
                 Right : @key[in] Character) @key[return] String;

   @key[function] "*" (Left  : @key[in] Natural;
                 Right : @key[in] String) @key[return] String;

@key[end] Ada.Strings.Fixed;
@end{example}

The effects of the above subprograms are as follows.
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[procedure] Move (Source  : @key[in]  String;
                Target  : @key[out] String;
                Drop    : @key[in]  Truncation := Error;
                Justify : @key[in]  Alignment  := Left;
                Pad     : @key[in]  Character  := Space);
@end{Example}
@Leading@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
The Move procedure copies characters from Source to Target.
If Source has the same length as Target, then the effect is
to assign Source to Target.
If Source is shorter than Target@Chg{Version=[3],New=[,],Old=[]} then:
@begin{itemize}
If Justify=Left, then Source is copied into the first Source'Length
 characters of Target.

If Justify=Right, then Source is copied into the last Source'Length
 characters of Target.

If Justify=Center, then Source is copied into
 the middle Source'Length characters of Target.
In this case, if the difference in length between
 Target and Source is odd, then the extra Pad character
  is on the right.

Pad is copied to each Target character not otherwise assigned.
@end{itemize}

If Source is longer than Target, then the effect is based on
Drop.
@begin{itemize}
If Drop=Left, then the rightmost Target'Length characters
of Source are copied into Target.

If Drop=Right, then the leftmost Target'Length characters
of Source are copied into Target.

@Leading@;If Drop=Error, then the effect depends on the value of the Justify
parameter and also on whether any characters in Source other than
Pad would fail to be copied:
@begin{inneritemize}
If Justify=Left, and if each of the rightmost Source'Length-Target'Length
characters in Source is Pad, then the leftmost Target'Length characters
of Source are copied to Target.

If Justify=Right, and if each of the leftmost Source'Length-Target'Length
characters in Source is Pad, then the rightmost Target'Length characters
of Source are copied to Target.

@Trailing@;Otherwise, Length_Error is propagated.
@end{inneritemize}
@end{itemize}
@begin{ramification}
The Move procedure will work even if Source and Target
overlap.@end{ramification}
@begin{reason}
The order of parameters (Source before Target)
corresponds to the order in COBOL's MOVE verb.@end{reason}

@begin{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@key[function] Index (Source  : @key[in] String;
                Pattern : @key[in] String;
                From    : @key[in] Positive;
                Going   : @key[in] Direction := Forward;
                Mapping : @key[in] Maps.Character_Mapping := Maps.Identity)
   @key[return] Natural;@*
@key[function] Index (Source  : @key[in] String;
                Pattern : @key[in] String;
                From    : @key[in] Positive;
                Going   : @key[in] Direction := Forward;
                Mapping : @key[in] Maps.Character_Mapping_Function)
   @key[return] Natural;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0056-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Each Index function searches, starting from From, for a slice of
Source, with length Pattern'Length, that matches Pattern with respect to
Mapping; the parameter Going indicates the direction of the lookup.
@Chg{Version=[3],New=[If Source is the null string, Index returns 0;
otherwise, if],Old=[If]}
From is not in Source'Range, then Index_Error is propagated. If Going =
Forward, then Index returns the smallest index I which is greater than or equal
to From such that the slice of Source starting at I matches Pattern. If Going =
Backward, then Index returns the largest index I such that the slice of Source
starting at I matches Pattern and has an upper bound less than or equal to
From. If there is no such slice, then 0 is returned. If Pattern is the null
string, then Pattern_Error is propagated.]}

@begin{Discussion}
   @ChgRef{Version=[2],Kind=[AddedNormal]}
   @ChgAdded{Version=[2],Text=[There is no default parameter for From; the
   default value would need to depend on other parameters (the bounds of Source
   and the direction Going). It is better to use overloaded functions rather
   than a special value to represent the default.]}

   @ChgRef{Version=[2],Kind=[AddedNormal]}
   @ChgAdded{Version=[2],Text=[There is no default value for the Mapping
   parameter that is a Character_Mapping_Function; if there were, a call would
   be ambiguous since there is also a default for the Mapping parameter that is
   a Character_Mapping.]}

   @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0056-1]}
   @ChgAdded{Version=[3],Text=[The language does not define when the
   Pattern_Error check is made. (That's because many common searching
   implementations require a non-empty pattern) That means that the result for
   a call like @f{Index ("", "")} could be 0 or could raise Pattern_Error.
   Similarly, in the call @f{Index ("", "", From => 2)}, the language does not
   define whether Pattern_Error or Index_Error is raised.]}

@end{Discussion}

@begin{Example}@Keepnext
@key[function] Index (Source   : @key[in] String;
                Pattern  : @key[in] String;
                Going    : @key[in] Direction := Forward;
                Mapping  : @key[in] Maps.Character_Mapping
                              := Maps.Identity)
   @key[return] Natural;@*
@key[function] Index (Source   : @key[in] String;
                Pattern  : @key[in] String;
                Going    : @key[in] Direction := Forward;
                Mapping  : @key[in] Maps.Character_Mapping_Function)
   @key[return] Natural;
@end{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00301-01]}
@Comment{@ChgDeleted{Version=[2],Type=[Trailing],Text=[]}@Comment{Fake to hold conditional format.}Can't have both.}
@ChgAdded{Version=[2],Type=[Leading],Text=[]}@Comment{Fake to hold conditional format.}
@Chg{Version=[2],New=[If Going = Forward, returns],
Old=[Each Index function searches for a slice of Source, with length
Pattern'Length, that matches Pattern
with respect to Mapping;
the parameter Going indicates the direction of the lookup.
 If Going = Forward, then Index
 returns the smallest index I such that
    the slice of Source starting at I matches Pattern.
  If Going = Backward, then Index
 returns the largest index I such that
    the slice of Source starting at I matches Pattern.
  If there is no such slice, then 0 is returned.
  If Pattern is the null string then Pattern_Error is propagated.]}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[      Index (Source, Pattern, Source'First, Forward, Mapping);]}
@end{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[otherwise@Chg{Version=[3],New=[,],Old=[]} returns]}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[      Index (Source, Pattern, Source'Last, Backward, Mapping);]}
@end{Example}
@begin{discussion}
   @ChgRef{Version=[2],Kind=[Deleted]}@ChgNote{Moved up}
   @ChgAdded{Version=[2],Text=[There is no default value for the Mapping
   parameter that is a Character_Mapping_Function; if there were, a call would
   be ambiguous since there is also a default for the Mapping parameter that is
   a Character_Mapping.]}
@end{discussion}

@begin{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@key[function] Index (Source  : @key[in] String;
                Set     : @key[in] Maps.Character_Set;
                From    : @key[in] Positive;
                Test    : @key[in] Membership := Inside;
                Going   : @key[in] Direction := Forward)
   @key[return] Natural;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0056-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Index searches for the first or
last occurrence of any of a set of characters (when Test=Inside), or any of the
complement of a set of characters (when Test=Outside).
@Chg{Version=[3],New=[If Source is the null string, Index returns 0;
otherwise, if],Old=[If]} From is not in
Source'Range, then Index_Error is propagated. Otherwise, it returns the
smallest index I >= From (if Going=Forward) or the largest index I <= From (if
Going=Backward) such that Source(I) satisfies the Test condition with respect
to Set; it returns 0 if there is no such Character in Source.]}

@begin{Example}@Keepnext
@key[function] Index (Source : @key[in] String;
                Set    : @key[in] Maps.Character_Set;
                Test   : @key[in] Membership := Inside;
                Going  : @key[in] Direction  := Forward)
   @key[return] Natural;
@end{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00301-01]}
@Comment{@ChgDeleted{Version=[2],Type=[Trailing],Text=[]}@Comment{Fake to hold conditional format.}Can't have both.}
@ChgAdded{Version=[2],Type=[Leading],Text=[]}@Comment{Fake to hold conditional format.}
@Chg{Version=[2],New=[If Going = Forward, returns],
Old=[Index searches for the first or last occurrence of any of a set of
characters (when Test=Inside),
or any of the complement of a set of characters (when Test=Outside).
It returns the smallest index I (if Going=Forward) or the largest index I
(if Going=Backward) such that
Source(I) satisfies the Test condition with respect to Set;
it returns 0 if there is no such Character in Source.]}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[      Index (Source, Set, Source'First, Test, Forward);]}
@end{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[otherwise@Chg{Version=[3],New=[,],Old=[]} returns]}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[      Index (Source, Set, Source'Last, Test, Backward);]}
@end{Example}

@begin{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@key[function] Index_Non_Blank (Source : @key[in] String;
                          From   : @key[in] Positive;
                          Going  : @key[in] Direction := Forward)
   @key[return] Natural;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns Index (Source, Maps.To_Set(Space), From, Outside, Going);]}

@begin{Example}@Keepnext
@key[function] Index_Non_Blank (Source : @key[in] String;
                          Going  : @key[in] Direction := Forward)
   @key[return] Natural;
@end{Example}
@Trailing@;Returns Index(Source, Maps.To_Set(Space), Outside, Going)

@begin{Example}@Keepnext
@key[function] Count (Source   : @key[in] String;
                Pattern  : @key[in] String;
                Mapping  : @key[in] Maps.Character_Mapping
                             := Maps.Identity)
   @key[return] Natural;@*
@key[function] Count (Source   : @key[in] String;
                Pattern  : @key[in] String;
                Mapping  : @key[in] Maps.Character_Mapping_Function)
   @key[return] Natural;
@end{Example}
@Trailing@;Returns the maximum number of nonoverlapping slices of Source that
match Pattern with respect to Mapping.
If Pattern is the null string then Pattern_Error is propagated.
@begin{reason}
We say `maximum number' because it is possible to slice a source
string in different ways yielding different numbers of matches. For
example if Source is "ABABABA" and Pattern is "ABA", then Count yields
2, although there is a partitioning of Source that yields just 1 match,
for the middle slice. Saying `maximum number' is equivalent to saying
that the pattern match starts either at the low index or the high index
position.
@end{reason}

@begin{Example}@Keepnext
@key[function] Count (Source   : @key[in] String;
                Set      : @key[in] Maps.Character_Set)
   @key[return] Natural;
@end{Example}
@Trailing@;Returns the number of occurrences in Source of characters that
are in Set.

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key[procedure] Find_Token (Source : @key[in] String;
                      Set    : @key[in] Maps.Character_Set;
                      From   : @key[in] Positive;
                      Test   : @key[in] Membership;
                      First  : @key[out] Positive;
                      Last   : @key[out] Natural);]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0031-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If Source is not the null string and
From is not in Source'Range, then Index_Error is raised. Otherwise, First is set
to the index of the first character in Source(From..Source'Last) that satisfies
the Test condition. Last is set to the largest index such that all characters in
Source(First..Last) satisfy the Test condition. If no characters in
Source(From..Source'Last) satisfy the Test condition, First is set to From, and
Last is set to 0.]}

@begin{Example}@Keepnext
@key[procedure] Find_Token (Source : @key[in] String;
                      Set    : @key[in] Maps.Character_Set;
                      Test   : @key[in] Membership;
                      First  : @key[out] Positive;
                      Last   : @key[out] Natural);
@end{Example}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0049],ARef=[AI95-00128-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0031-1]}
@Trailing@;@Chg{Version=[3],New=[Equivalent to Find_Token (Source, Set,
Source'First, Test, First, Last).],Old=[Find_Token returns in
First and Last the indices of the beginning
and end of the first slice of Source all of whose elements
satisfy the Test condition, and such that the elements
(if any) immediately before and after the slice do not
satisfy the Test condition.
If no such slice exists, then the value returned for Last is zero, and
the value returned for First is Source'First@Chg{New=[; however, if
Source'First is not in Positive then Constraint_Error
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
is raised],Old=[]}.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0031-1]}
  @ChgAdded{Version=[3],Text=[If Source'First is not in Positive, which can only
  happen for an empty string, this will raise Constraint_Error.]}
@end{Ramification}

@begin{Example}@Keepnext
@key[function] Translate (Source  : @key[in] String;
                    Mapping : @key[in] Maps.Character_Mapping)
   @key[return] String;@*
@key[function] Translate (Source  : @key[in] String;
                    Mapping : @key[in] Maps.Character_Mapping_Function)
   @key[return] String;
@end{Example}
@Trailing@;Returns the string S whose length is Source'Length and such
that S(I) is the character to which Mapping maps the corresponding
element of Source, for I in 1..Source'Length.

@begin{Example}@Keepnext
@key[procedure] Translate (Source  : @key[in] @key[out] String;
                     Mapping : @key[in] Maps.Character_Mapping);@*
@key[procedure] Translate (Source  : @key[in] @key[out] String;
                     Mapping : @key[in] Maps.Character_Mapping_Function);
@end{Example}
@Trailing@;Equivalent to Source := Translate(Source, Mapping).

@begin{Example}@Keepnext
@key[function] Replace_Slice (Source   : @key[in] String;
                        Low      : @key[in] Positive;
                        High     : @key[in] Natural;
                        By       : @key[in] String)
   @key[return] String;
@end{Example}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0049],ARef=[AI95-00128-01]}
@Chg{New=[@Leading],Old=[@Trailing]}If Low > Source'Last+1, or
High < Source'First@en@;1, then Index_Error is propagated.
Otherwise@Chg{New=[:],Old=[, if High >= Low then the returned string
comprises Source(Source'First..Low@en@;1) & By & Source(High+1..Source'Last),
and if High < Low then the returned string is
Insert(Source, Before=>Low, New_Item=>By).]}

@begin{Itemize}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0049],ARef=[AI95-00128-01]}
@ChgAdded{Version=[1],Text=[If High >= Low, then the returned string comprises
Source(Source'First..Low@en@;1) & By & Source(High+1..Source'Last), but with
lower bound 1.@Trailing]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0049],ARef=[AI95-00128-01]}
@ChgAdded{Version=[1],Text=[If High < Low, then the returned string is
Insert(Source, Before=>Low, New_Item=>By).]}
@end{Itemize}

@begin{Example}@Keepnext
@key[procedure] Replace_Slice (Source   : @key[in] @key[out] String;
                         Low      : @key[in] Positive;
                         High     : @key[in] Natural;
                         By       : @key[in] String;
                         Drop     : @key[in] Truncation := Error;
                         Justify  : @key[in] Alignment  := Left;
                         Pad      : @key[in] Character  := Space);
@end{Example}
@Trailing@;Equivalent to Move(Replace_Slice(Source, Low, High,
By), Source, Drop, Justify, Pad).

@begin{Example}@Keepnext
@key[function] Insert (Source   : @key[in] String;
                 Before   : @key[in] Positive;
                 New_Item : @key[in] String)
   @key[return] String;
@end{Example}
@Trailing@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
Propagates Index_Error if Before is not in Source'First .. Source'Last+1;
otherwise@Chg{Version=[3],New=[,],Old=[]}
returns Source(Source'First..Before@en@;1) & New_Item &
Source(Before..Source'Last), but with lower bound 1.


@begin{Example}@Keepnext
@key[procedure] Insert (Source   : @key[in] @key[out] String;
                  Before   : @key[in] Positive;
                  New_Item : @key[in] String;
                  Drop     : @key[in] Truncation := Error);
@end{Example}
@Trailing@;Equivalent to Move(Insert(Source, Before, New_Item), Source, Drop).

@begin{Example}@Keepnext
@key[function] Overwrite (Source   : @key[in] String;
                    Position : @key[in] Positive;
                    New_Item : @key[in] String)
   @key[return] String;
@end{Example}
@Trailing@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
Propagates Index_Error if Position is not in Source'First .. Source'Last+1;
otherwise@Chg{Version=[3],New=[,],Old=[]}
returns the string obtained from Source by consecutively replacing
characters starting at Position with corresponding characters from
New_Item. If the end of Source is reached before the characters in
New_Item are exhausted, the remaining characters from New_Item are
appended to the string.

@begin{Example}@Keepnext
@key[procedure] Overwrite (Source   : @key[in] @key[out] String;
                     Position : @key[in] Positive;
                     New_Item : @key[in] String;
                     Drop     : @key[in] Truncation := Right);
@end{Example}
@Trailing@;Equivalent to Move(Overwrite(Source, Position,
New_Item), Source, Drop).

@begin{Example}@Keepnext
@key[function] Delete (Source  : @key[in] String;
                 From    : @key[in] Positive;
                 Through : @key[in] Natural)
   @key[return] String;
@end{Example}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0049],ARef=[AI95-00128-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@Trailing@;If From <= Through, the returned string is Replace_Slice(Source, From,
Through, "")@Chg{Version=[3],New=[;],Old=[,]}
otherwise@Chg{Version=[3],New=[,],Old=[]} it is Source@Chg{New=[ with lower
bound 1],Old=[]}.

@begin{Example}@Keepnext
@key[procedure] Delete (Source  : @key[in] @key[out] String;
                  From    : @key[in] Positive;
                  Through : @key[in] Natural;
                  Justify : @key[in] Alignment := Left;
                  Pad     : @key[in] Character := Space);
@end{Example}
@Trailing@;Equivalent to Move(Delete(Source, From, Through),
Source, Justify => Justify, Pad => Pad).

@begin{Example}@Keepnext
@key[function] Trim (Source : @key[in] String;
               Side   : @key[in] Trim_End)
  @key[return] String;
@end{Example}
@Trailing@;Returns the string obtained by removing from Source all leading
Space characters (if Side = Left), all trailing Space characters
(if Side = Right), or all leading and trailing Space characters
(if Side = Both).

@begin{Example}@Keepnext
@key[procedure] Trim (Source  : @key[in] @key[out] String;
                Side    : @key[in] Trim_End;
                Justify : @key[in] Alignment := Left;
                Pad     : @key[in] Character := Space);
@end{Example}
@Trailing@;Equivalent to Move(Trim(Source, Side), Source, Justify=>Justify, Pad=>Pad).

@begin{Example}@Keepnext
@key[function] Trim (Source : @key[in] String;
               Left   : @key[in] Maps.Character_Set;
               Right  : @key[in] Maps.Character_Set)
   @key[return] String;
@end{Example}
@Trailing@;Returns the string obtained by removing from Source all leading
characters in Left and all trailing characters in Right.

@begin{Example}@Keepnext
@key[procedure] Trim (Source  : @key[in] @key[out] String;
                Left    : @key[in] Maps.Character_Set;
                Right   : @key[in] Maps.Character_Set;
                Justify : @key[in] Alignment := Strings.Left;
                Pad     : @key[in] Character := Space);
@end{Example}
@Trailing@;Equivalent to Move(Trim(Source, Left, Right), Source,
Justify => Justify, Pad=>Pad).

@begin{Example}@Keepnext
@key[function] Head (Source : @key[in] String;
               Count  : @key[in] Natural;
               Pad    : @key[in] Character := Space)
   @key[return] String;
@end{Example}
@Trailing@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
Returns a string of length Count. If Count <= Source'Length, the
string comprises the first Count characters of Source.
Otherwise@Chg{Version=[3],New=[,],Old=[]} its contents
are Source concatenated with Count@en@;Source'Length Pad characters.

@begin{Example}@Keepnext
@key[procedure] Head (Source  : @key[in] @key[out] String;
                Count   : @key[in] Natural;
                Justify : @key[in] Alignment := Left;
                Pad     : @key[in] Character := Space);
@end{Example}
@Trailing@;Equivalent to Move(Head(Source, Count, Pad), Source, Drop=>Error,
Justify=>Justify, Pad=>Pad).

@begin{Example}@Keepnext
@key[function] Tail (Source : @key[in] String;
               Count  : @key[in] Natural;
               Pad    : @key[in] Character := Space)
   @key[return] String;
@end{Example}
@Trailing@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
Returns a string of length Count. If Count <= Source'Length, the
string comprises the last Count characters of Source.
Otherwise@Chg{Version=[3],New=[,],Old=[]} its contents
are Count-Source'Length Pad characters concatenated with Source.

@begin{Example}@Keepnext
@key[procedure] Tail (Source  : @key[in] @key[out] String;
                Count   : @key[in] Natural;
                Justify : @key[in] Alignment := Left;
                Pad     : @key[in] Character := Space);
@end{Example}
@Trailing@;Equivalent to Move(Tail(Source, Count, Pad), Source, Drop=>Error,
Justify=>Justify, Pad=>Pad).

@begin{Example}@Keepnext
@key[function] "*" (Left  : @key[in] Natural;
              Right : @key[in] Character) @key[return] String;@*
@key[function] "*" (Left  : @key[in] Natural;
              Right : @key[in] String) @key[return] String;
@end{Example}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0049],ARef=[AI95-00128-01]}
These functions replicate a character or string a specified number
of times. The first function returns a string whose length is Left and each
of whose elements is Right. The second function returns a string whose
length is Left*Right'Length and whose value is the null
string if Left = 0 and @Chg{New=[otherwise ],Old=[]}is
(Left@en@;1)*Right & Right @Chg{New=[with lower bound 1],Old=[otherwise]}.
@end{DescribeCode}
@end{StaticSem}

@begin{Notes}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
In the Index and Count functions taking Pattern and Mapping parameters,
the actual String parameter passed to Pattern should comprise characters
occurring as target characters of the mapping.
Otherwise@Chg{Version=[3],New=[,],Old=[]} the pattern will not match.

In the Insert subprograms, inserting at the end of a string is obtained
by passing Source'Last+1 as the Before parameter.

@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
If a null Character_Mapping_Function is passed to any of the
string handling subprograms, Constraint_Error is propagated.
@end{Notes}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  Overloaded versions of Index and Index_Non_Blank are newly added to
  Strings.Fixed. If Strings.Fixed is referenced in a @nt{use_clause}, and an
  entity @i<E> with a @nt{defining_identifier} of Index or Index_Non_Blank is
  defined in a package that is also referenced in a @nt{use_clause}, the entity
  @i<E> may no longer be use-visible, resulting in errors. This should be rare
  and is easily fixed if it does occur.]}
@end{Incompatible95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0049],ARef=[AI95-00128-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that Find_Token
  may raise Constraint_Error if Source'First is not in Positive (which is
  only possible for a null string).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0049],ARef=[AI95-00128-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that Replace_Slice,
  Delete, and "*" always return a string with lower bound 1.]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0031-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
  An overloaded version of Find_Token is newly added to
  Strings.Fixed. If Strings.Fixed is referenced in a @nt{use_clause}, and an
  entity @i<E> with a @nt{defining_identifier} of Find_Token is
  defined in a package that is also referenced in a @nt{use_clause}, the entity
  @i<E> may no longer be use-visible, resulting in errors. This should be rare
  and is easily fixed if it does occur.]}
@end{Incompatible2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0056-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified that Index
  never raises Index_Error if the source string is null.]}
@end{DiffWord2005}


@LabeledSubClause{Bounded-Length String Handling}
@begin{Intro}
The language-defined package Strings.Bounded provides a generic package
each of whose instances yields a private type Bounded_String and a
set of operations. An object of a particular Bounded_String type
represents a String whose low bound is 1 and whose length
can vary conceptually
between 0 and a maximum size established at the
generic instantiation.
The subprograms for fixed-length string handling
are either overloaded directly for Bounded_String, or are modified as
needed to reflect the variability in length. Additionally, since the
Bounded_String type is private, appropriate constructor and selector
operations are provided.
@begin{reason}
Strings.Bounded declares an inner generic package, versus itself
being directly a generic child of Strings, in order to retain
compatibility with a version of the string-handling packages that
is generic with respect to the character and string types.@end{reason}
@begin{reason}
The bound of a bounded-length string is specified as a parameter
to a generic, versus as the value for a discriminant, because of the
inappropriateness of assignment and equality of discriminated types for
the copying and comparison of bounded strings.@end{reason}
@end{Intro}

@begin{StaticSem}
@Leading@Keepnext@;The library package Strings.Bounded has the following declaration:
@begin{example}
@key[with] Ada.Strings.Maps;
@ChildUnit{Parent=[Ada.Strings],Child=[Bounded]}@key[package] Ada.Strings.Bounded @key[is]
   @key[pragma] Preelaborate(Bounded);


   @key[generic]
      Max   : Positive;    --@Examcom{ Maximum length of a Bounded_String}
   @key[package] @AdaPackDefn{Generic_Bounded_Length} @key[is]

      @AdaObjDefn{Max_Length} : @key[constant] Positive := Max;

      @key[type] @AdaTypeDefn{Bounded_String} @key[is] @key[private];

      @AdaObjDefn{Null_Bounded_String} : @key[constant] Bounded_String;

      @key[subtype] @AdaSubtypeDefn{Name=[Length_Range],Of=[Natural]} @key[is] Natural @key[range] 0 .. Max_Length;

      @key[function] @AdaSubDefn{Length} (Source : @key[in] Bounded_String) @key[return] Length_Range;


   --@Examcom{ Conversion, Concatenation, and Selection functions}

      @key[function] @AdaSubDefn{To_Bounded_String} (Source : @key[in] String;
                                  Drop   : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[function] @AdaSubDefn{To_String} (Source : @key[in] Bounded_String) @key[return] String;

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[      @key[procedure] @AdaSubDefn{Set_Bounded_String}
         (Target :    @key[out] Bounded_String;
          Source : @key[in]     String;
          Drop   : @key[in]     Truncation := Error);]}

      @key[function] @AdaSubDefn{Append} (Left, Right : @key[in] Bounded_String;
                       Drop        : @key[in] Truncation  := Error)
         @key[return] Bounded_String;

      @key[function] @AdaSubDefn{Append} (Left  : @key[in] Bounded_String;
                       Right : @key[in] String;
                       Drop  : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[function] @AdaSubDefn{Append} (Left  : @key[in] String;
                       Right : @key[in] Bounded_String;
                       Drop  : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[function] @AdaSubDefn{Append} (Left  : @key[in] Bounded_String;
                       Right : @key[in] Character;
                       Drop  : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[function] @AdaSubDefn{Append} (Left  : @key[in] Character;
                       Right : @key[in] Bounded_String;
                       Drop  : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[procedure] @AdaSubDefn{Append} (Source   : @key[in out] Bounded_String;
                        New_Item : @key[in] Bounded_String;
                        Drop     : @key[in] Truncation  := Error);

      @key[procedure] @AdaSubDefn{Append} (Source   : @key[in out] Bounded_String;
                        New_Item : @key[in] String;
                        Drop     : @key[in] Truncation  := Error);

      @key[procedure] @AdaSubDefn{Append} (Source   : @key[in out] Bounded_String;
                        New_Item : @key[in] Character;
                        Drop     : @key[in] Truncation  := Error);

      @key[function] "&" (Left, Right : @key[in] Bounded_String)
         @key[return] Bounded_String;

      @key[function] "&" (Left : @key[in] Bounded_String; Right : @key[in] String)
         @key[return] Bounded_String;

      @key[function] "&" (Left : @key[in] String; Right : @key[in] Bounded_String)
         @key[return] Bounded_String;

      @key[function] "&" (Left : @key[in] Bounded_String; Right : @key[in] Character)
         @key[return] Bounded_String;

      @key[function] "&" (Left : @key[in] Character; Right : @key[in] Bounded_String)
         @key[return] Bounded_String;


      @key[function] @AdaSubDefn{Element} (Source : @key[in] Bounded_String;
                        Index  : @key[in] Positive)
         @key[return] Character;

      @key[procedure] @AdaSubDefn{Replace_Element} (Source : @key[in] @key[out] Bounded_String;
                                 Index  : @key[in] Positive;
                                 By     : @key[in] Character);


      @key[function] @AdaSubDefn{Slice} (Source : @key[in] Bounded_String;
                      Low    : @key[in] Positive;
                      High   : @key[in] Natural)
         @key[return] String;

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[      @key[function] @AdaSubDefn{Bounded_Slice}
         (Source : @key[in] Bounded_String;
          Low    : @key[in] Positive;
          High   : @key[in] Natural)
             @key[return] Bounded_String;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[      @key[procedure] @AdaSubDefn{Bounded_Slice}
         (Source : @key[in]     Bounded_String;
          Target :    @key[out] Bounded_String;
          Low    : @key[in]     Positive;
          High   : @key[in]     Natural);]}

      @key[function] "="  (Left, Right : @key[in] Bounded_String) @key[return] Boolean;
      @key[function] "="  (Left : @key[in] Bounded_String; Right : @key[in] String)
        @key[return] Boolean;

      @key[function] "="  (Left : @key[in] String; Right : @key[in] Bounded_String)
        @key[return] Boolean;


      @key[function] "<"  (Left, Right : @key[in] Bounded_String) @key[return] Boolean;

      @key[function] "<"  (Left : @key[in] Bounded_String; Right : @key[in] String)
        @key[return] Boolean;

      @key[function] "<"  (Left : @key[in] String; Right : @key[in] Bounded_String)
        @key[return] Boolean;

      @key[function] "<=" (Left, Right : @key[in] Bounded_String) @key[return] Boolean;

      @key[function] "<="  (Left : @key[in] Bounded_String; Right : @key[in] String)
        @key[return] Boolean;

      @key[function] "<="  (Left : @key[in] String; Right : @key[in] Bounded_String)
        @key[return] Boolean;

      @key[function] ">"  (Left, Right : @key[in] Bounded_String) @key[return] Boolean;

      @key[function] ">"  (Left : @key[in] Bounded_String; Right : @key[in] String)
        @key[return] Boolean;

      @key[function] ">"  (Left : @key[in] String; Right : @key[in] Bounded_String)
        @key[return] Boolean;

      @key[function] ">=" (Left, Right : @key[in] Bounded_String) @key[return] Boolean;

      @key[function] ">="  (Left : @key[in] Bounded_String; Right : @key[in] String)
        @key[return] Boolean;

      @key[function] ">="  (Left : @key[in] String; Right : @key[in] Bounded_String)
        @key[return] Boolean;

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00301-01]}
   --@Examcom{ Search @Chg{Version=[2],New=[subprograms],Old=[functions]}}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[      @key[function] @AdaSubDefn{Index} (Source  : @key[in] Bounded_String;
                      Pattern : @key[in] String;
                      From    : @key[in] Positive;
                      Going   : @key[in] Direction := Forward;
                      Mapping : @key[in] Maps.Character_Mapping := Maps.Identity)
         @key[return] Natural;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[      @key[function] @AdaSubDefn{Index} (Source  : @key[in] Bounded_String;
                      Pattern : @key[in] String;
                      From    : @key[in] Positive;
                      Going   : @key[in] Direction := Forward;
                      Mapping : @key[in] Maps.Character_Mapping_Function)
         @key[return] Natural;]}

      @key[function] @AdaSubDefn{Index} (Source   : @key[in] Bounded_String;
                      Pattern  : @key[in] String;
                      Going    : @key[in] Direction := Forward;
                      Mapping  : @key[in] Maps.Character_Mapping
                                 := Maps.Identity)
         @key[return] Natural;

      @key[function] @AdaSubDefn{Index} (Source   : @key[in] Bounded_String;
                      Pattern  : @key[in] String;
                      Going    : @key[in] Direction := Forward;
                      Mapping  : @key[in] Maps.Character_Mapping_Function)
         @key[return] Natural;

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[      @key[function] @AdaSubDefn{Index} (Source  : @key[in] Bounded_String;
                      Set     : @key[in] Maps.Character_Set;
                      From    : @key[in] Positive;
                      Test    : @key[in] Membership := Inside;
                      Going   : @key[in] Direction := Forward)
         @key[return] Natural;]}

      @key[function] @AdaSubDefn{Index} (Source : @key[in] Bounded_String;
                      Set    : @key[in] Maps.Character_Set;
                      Test   : @key[in] Membership := Inside;
                      Going  : @key[in] Direction  := Forward)
         @key[return] Natural;

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[      @key[function] @AdaSubDefn{Index_Non_Blank} (Source : @key[in] Bounded_String;
                                From   : @key[in] Positive;
                                Going  : @key[in] Direction := Forward)
         @key[return] Natural;]}

      @key[function] @AdaSubDefn{Index_Non_Blank} (Source : @key[in] Bounded_String;
                                Going  : @key[in] Direction := Forward)
         @key[return] Natural;


      @key[function] @AdaSubDefn{Count} (Source   : @key[in] Bounded_String;
                      Pattern  : @key[in] String;
                      Mapping  : @key[in] Maps.Character_Mapping
                                   := Maps.Identity)
         @key[return] Natural;

      @key[function] @AdaSubDefn{Count} (Source   : @key[in] Bounded_String;
                      Pattern  : @key[in] String;
                      Mapping  : @key[in] Maps.Character_Mapping_Function)
         @key[return] Natural;

      @key[function] @AdaSubDefn{Count} (Source   : @key[in] Bounded_String;
                      Set      : @key[in] Maps.Character_Set)
         @key[return] Natural;


@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0031-1]}
@ChgAdded{Version=[3],Text=[      @key[procedure] @AdaSubDefn{Find_Token} (Source : @key[in] Bounded_String;
                            Set    : @key[in] Maps.Character_Set;
                            From   : @key[in] Positive;
                            Test   : @key[in] Membership;
                            First  : @key[out] Positive;
                            Last   : @key[out] Natural);]}

      @key[procedure] @AdaSubDefn{Find_Token} (Source : @key[in] Bounded_String;
                            Set    : @key[in] Maps.Character_Set;
                            Test   : @key[in] Membership;
                            First  : @key[out] Positive;
                            Last   : @key[out] Natural);

@Keepnext@;   --@Examcom{ String translation subprograms}

      @key[function] @AdaSubDefn{Translate} (Source  : @key[in] Bounded_String;
                          Mapping : @key[in] Maps.Character_Mapping)
         @key[return] Bounded_String;

      @key[procedure] @AdaSubDefn{Translate} (Source  : @key[in] @key[out] Bounded_String;
                           Mapping : @key[in] Maps.Character_Mapping);


      @key[function] @AdaSubDefn{Translate} (Source  : @key[in] Bounded_String;
                          Mapping : @key[in] Maps.Character_Mapping_Function)
         @key[return] Bounded_String;

      @key[procedure] @AdaSubDefn{Translate} (Source  : @key[in] @key[out] Bounded_String;
                           Mapping : @key[in] Maps.Character_Mapping_Function);

@Keepnext@;   --@Examcom{ String transformation subprograms}

      @key[function] @AdaSubDefn{Replace_Slice} (Source   : @key[in] Bounded_String;
                              Low      : @key[in] Positive;
                              High     : @key[in] Natural;
                              By       : @key[in] String;
                              Drop     : @key[in] Truncation := Error)
         @key[return] Bounded_String;


      @key[procedure] @AdaSubDefn{Replace_Slice} (Source   : @key[in] @key[out] Bounded_String;
                               Low      : @key[in] Positive;
                               High     : @key[in] Natural;
                               By       : @key[in] String;
                               Drop     : @key[in] Truncation := Error);


      @key[function] @AdaSubDefn{Insert} (Source   : @key[in] Bounded_String;
                       Before   : @key[in] Positive;
                       New_Item : @key[in] String;
                       Drop     : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[procedure] @AdaSubDefn{Insert} (Source   : @key[in] @key[out] Bounded_String;
                        Before   : @key[in] Positive;
                        New_Item : @key[in] String;
                        Drop     : @key[in] Truncation := Error);


      @key[function] @AdaSubDefn{Overwrite} (Source    : @key[in] Bounded_String;
                          Position  : @key[in] Positive;
                          New_Item  : @key[in] String;
                          Drop      : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[procedure] @AdaSubDefn{Overwrite} (Source    : @key[in] @key[out] Bounded_String;
                           Position  : @key[in] Positive;
                           New_Item  : @key[in] String;
                           Drop      : @key[in] Truncation := Error);

      @key[function] @AdaSubDefn{Delete} (Source  : @key[in] Bounded_String;
                       From    : @key[in] Positive;
                       Through : @key[in] Natural)
         @key[return] Bounded_String;

      @key[procedure] @AdaSubDefn{Delete} (Source  : @key[in] @key[out] Bounded_String;
                        From    : @key[in] Positive;
                        Through : @key[in] Natural);

   --@Examcom{String selector subprograms}

      @key[function] @AdaSubDefn{Trim} (Source : @key[in] Bounded_String;
                     Side   : @key[in] Trim_End)
         @key[return] Bounded_String;
      @key[procedure] @AdaSubDefn{Trim} (Source : @key[in] @key[out] Bounded_String;
                      Side   : @key[in] Trim_End);

      @key[function] @AdaSubDefn{Trim} (Source : @key[in] Bounded_String;
                     Left   : @key[in] Maps.Character_Set;
                     Right  : @key[in] Maps.Character_Set)
         @key[return] Bounded_String;

      @key[procedure] @AdaSubDefn{Trim} (Source : @key[in] @key[out] Bounded_String;
                      Left   : @key[in] Maps.Character_Set;
                      Right  : @key[in] Maps.Character_Set);

      @key[function] @AdaSubDefn{Head} (Source : @key[in] Bounded_String;
                     Count  : @key[in] Natural;
                     Pad    : @key[in] Character  := Space;
                     Drop   : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[procedure] @AdaSubDefn{Head} (Source : @key[in] @key[out] Bounded_String;
                      Count  : @key[in] Natural;
                      Pad    : @key[in] Character  := Space;
                      Drop   : @key[in] Truncation := Error);

      @key[function] @AdaSubDefn{Tail} (Source : @key[in] Bounded_String;
                     Count  : @key[in] Natural;
                     Pad    : @key[in] Character  := Space;
                     Drop   : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[procedure] @AdaSubDefn{Tail} (Source : @key[in] @key[out] Bounded_String;
                      Count  : @key[in] Natural;
                      Pad    : @key[in] Character  := Space;
                      Drop   : @key[in] Truncation := Error);

   --@Examcom{String constructor subprograms}

      @key[function] "*" (Left  : @key[in] Natural;
                    Right : @key[in] Character)
         @key[return] Bounded_String;

      @key[function] "*" (Left  : @key[in] Natural;
                    Right : @key[in] String)
         @key[return] Bounded_String;

      @key[function] "*" (Left  : @key[in] Natural;
                    Right : @key[in] Bounded_String)
         @key[return] Bounded_String;


      @key[function] @AdaSubDefn{Replicate} (Count : @key[in] Natural;
                          Item  : @key[in] Character;
                          Drop  : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[function] @AdaSubDefn{Replicate} (Count : @key[in] Natural;
                          Item  : @key[in] String;
                          Drop  : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[function] @AdaSubDefn{Replicate} (Count : @key[in] Natural;
                          Item  : @key[in] Bounded_String;
                          Drop  : @key[in] Truncation := Error)
         @key[return] Bounded_String;

   @key[private]
       ... -- @Examcom{not specified by the language}
   @key[end] Generic_Bounded_Length;

@key[end] Ada.Strings.Bounded;
@end{example}

@begin{ImplNote}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0097],ARef=[AI95-00115-01]}
@ChgRef{Version=[2],Kind=[DeletedAdded],ARef=[AI95-00344-01]}
@ChgDeleted{Version=[2],Text=[@Chg{Version=[1],New=[Bounded_String cannot be
implemented as a (directly) controlled type,
as Ada.Strings.Bounded.Generic_Bounded_Length can be instantiated at any
nesting depth. Bounded_String could have
a component of a controlled type, as long as that type is declared in some
other (non-generic) package (including directly in Ada.Strings.Bounded).],Old=[]}]}
@ChgNote{AI-344 allows controlled types to be declared at
any nesting depth, so this note is obsolete.}

@end{ImplNote}

Null_Bounded_String represents the null string.
If an object of type Bounded_String is not otherwise initialized, it
will be initialized to the same value as Null_Bounded_String.
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[function] Length (Source : @key[in] Bounded_String) @key[return] Length_Range;
@end{Example}
@Trailing@;The Length function returns the length of the string represented by
Source.

@begin{Example}@Keepnext
@key[function] To_Bounded_String (Source : @key[in] String;
                            Drop   : @key[in] Truncation := Error)
   @key[return] Bounded_String;
@end{Example}
@Leading@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
If Source'Length <= Max_Length@Chg{Version=[3],New=[,],Old=[]} then
this function
returns a Bounded_String that represents Source.
Otherwise@Chg{Version=[3],New=[,],Old=[]}
the effect depends on the value of Drop:
@begin{itemize}
If Drop=Left, then
the result is a Bounded_String that represents the string comprising
the rightmost Max_Length characters of Source.

 If Drop=Right, then
the result is a Bounded_String that represents the string comprising
the leftmost Max_Length characters of Source.

@Trailing@;If Drop=Error, then Strings.Length_Error is propagated.
@end{itemize}

@begin{Example}@Keepnext
@key[function] To_String (Source : @key[in] Bounded_String) @key[return] String;
@end{Example}
@Trailing@;To_String returns the String value with lower bound 1 represented by
Source. If B is a Bounded_String, then B = To_Bounded_String(To_String(B)).

@begin{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key[procedure] Set_Bounded_String
   (Target :    @key[out] Bounded_String;
    Source : @key[in]     String;
    Drop   : @key[in]     Truncation := Error);]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Target := To_Bounded_String (Source, Drop);]}

@end{DescribeCode}

Each of the Append functions returns a Bounded_String obtained by concatenating
the string or character given or represented by one of the parameters,
with the string or character given or represented by the other parameter,
and applying To_Bounded_String to the concatenation result string,
with Drop as provided to the Append function.

Each of the procedures Append(Source, New_Item, Drop) has the same
effect as the corresponding assignment Source :=
Append(Source, New_Item, Drop).

Each of the "&" functions has the same effect as the corresponding
Append function, with Error as the Drop parameter.
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[function] Element (Source : @key[in] Bounded_String;
                  Index  : @key[in] Positive)
   @key[return] Character;
@end{Example}
@Trailing@;Returns the character at position Index in the string represented
by Source; propagates Index_Error if Index > Length(Source).

@begin{Example}@Keepnext
@key[procedure] Replace_Element (Source : @key[in] @key[out] Bounded_String;
                           Index  : @key[in] Positive;
                           By     : @key[in] Character);
@end{Example}
@Trailing@;Updates Source such that the character at position Index in the
string represented by Source is By;
propagates Index_Error if Index > Length(Source).

@begin{Example}@Keepnext
@key[function] Slice (Source : @key[in] Bounded_String;
                Low    : @key[in] Positive;
                High   : @key[in] Natural)
   @key[return] String;
@end{Example}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0049],ARef=[AI95-00128-01]}
@ChgRef{Version=[1],Kind=[Revised],ARef=[AI95-00238-01]}
@Trailing@;Returns the slice at positions Low through High in the string
represented by Source; propagates Index_Error if
Low > Length(Source)+1@Chg{New=[ or High > Length(Source)],Old=[]}.@Chg{Version=[2],
New=[ The bounds of the returned string are Low and High.],Old=[]}.

@begin{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key[function] Bounded_Slice
   (Source : @key[in] Bounded_String;
    Low    : @key[in] Positive;
    High   : @key[in] Natural)
       @key[return] Bounded_String;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the slice at positions Low
through High in the string represented by Source as a bounded string;
propagates Index_Error if Low > Length(Source)+1 or High > Length(Source).]}

@begin{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key[procedure] Bounded_Slice
   (Source : @key[in]     Bounded_String;
    Target :    @key[out] Bounded_String;
    Low    : @key[in]     Positive;
    High   : @key[in]     Natural);]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Equivalent to Target := Bounded_Slice (Source, Low, High);]}

@end{DescribeCode}

Each of the functions "=", "<", ">", "<=", and ">="
returns the same result as the corresponding String
operation applied to the String values given or represented by
the two parameters.

Each of the search subprograms (Index, Index_Non_Blank,
Count, Find_Token) has the
same effect as the corresponding subprogram in Strings.Fixed applied
to the string represented by the Bounded_String parameter.

Each of the Translate subprograms, when applied to a Bounded_String, has
an analogous effect to the corresponding subprogram in Strings.Fixed.
For the Translate function,
the translation is applied to the string represented by the Bounded_String
parameter, and the result is converted (via To_Bounded_String) to a
Bounded_String.
For the Translate procedure, the string represented by the Bounded_String
parameter after the translation is given by the Translate function for
fixed-length strings applied to the string represented by the
original value of the parameter.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0049],ARef=[AI95-00128-01]}
Each of the transformation subprograms (Replace_Slice, Insert,
Overwrite, Delete), selector subprograms (Trim, Head, Tail),
and constructor functions ("*") has an effect based on its
corresponding subprogram in Strings.Fixed, and Replicate is based on
Fixed."*". @Chg{New=[In the case of a function],
Old=[For each of these subprograms]}, the corresponding
fixed-length string subprogram is applied to the string represented by
the Bounded_String parameter. To_Bounded_String is applied the result
string, with Drop (or Error in the case of Generic_Bounded_Length."*")
determining the effect when the string length exceeds Max_Length.
@Chg{New=[In the case of a procedure, the corresponding function in
Strings.@!Bounded.@!Generic_@!Bounded_@!Length is applied, with the result
assigned into the Source parameter.],Old=[]}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
The "/=" operations between Bounded_String and String, and between String
and Bounded_String, are automatically defined based on the @Chg{Version=[2],
New=[corresponding],Old=[corrsponding]}
"=" operations.
@end{Ramification}
@end{StaticSem}

@begin{ImplAdvice}
Bounded string objects should not be implemented by implicit
pointers and dynamic allocation.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Bounded string objects should not be implemented by implicit
pointers and dynamic allocation.]}]}
@begin{ImplNote}
@Leading@;The following is a possible implementation of the private part
of the package:
@begin{example}
@key[type] Bounded_String_Internals (Length : Length_Range := 0) @key[is]
   @key[record]
      Data : String(1..Length);
   @key[end] @key[record];

@key[type] Bounded_String @key[is]
   @key[record]
      Data : Bounded_String_Internals;  --@Examcom{ Unconstrained}
   @key[end] @key[record];

Null_Bounded_String : @key[constant] Bounded_String :=
   (Data => (Length => 0,
             Data   => (1..0 => ' ')));
@end{example}
@end{ImplNote}
@end{ImplAdvice}


@begin{Inconsistent95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00238-01]}
  @ChgAdded{Version=[2],Text=[@Defn{inconsistencies with Ada 95}
  @B[Amendment Correction:] The bounds of the string returned from
  Slice are now defined. This is technically an inconsistency; if a program
  depended on some other lower bound for the string returned from Slice,
  it could fail when compiled with Ada 2005. Such code is not portable even
  between Ada 95 implementations, so it should be very rare.]}
@end{Inconsistent95}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  Procedure Set_Bounded_String, two Bounded_Slice subprograms, and overloaded
  versions of Index and Index_Non_Blank are newly added to
  Strings.Bounded.Generic_Bounded_Length. If an instance of Generic_Bounded_Length is
  referenced in a @nt{use_clause}, and an entity @i<E> with the
  @nt{defining_identifier} as a new entity in Generic_Bounded_Length is defined in a
  package that is also referenced in a @nt{use_clause}, the entity @i<E> may no
  longer be use-visible, resulting in errors. This should be rare and is easily
  fixed if it does occur.]}
@end{Incompatible95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0049],ARef=[AI95-00128-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the conditions for
  which Slice raises Index_Error.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0049],ARef=[AI95-00128-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified the meaning of
  transformation, selector, and constructor subprograms by describing the
  effects of procedures and functions separately.]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0031-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
  An overloaded version of Find_Token is newly added to
  Strings.Bounded.Generic_Bounded_Length. If an instance of Generic_Bounded_Length is
  referenced in a @nt{use_clause}, and an
  entity @i<E> with a @nt{defining_identifier} of Find_Token is
  defined in a package that is also referenced in a @nt{use_clause}, the entity
  @i<E> may no longer be use-visible, resulting in errors. This should be rare
  and is easily fixed if it does occur.]}
@end{Incompatible2005}



@LabeledSubClause{Unbounded-Length String Handling}
@begin{Intro}
The language-defined package Strings.Unbounded provides a
 private type Unbounded_String and a
set of operations. An object of type Unbounded_String represents a String
whose low bound is 1 and whose length
can vary conceptually between 0 and Natural'Last.
The subprograms for fixed-length string handling
are either overloaded directly for Unbounded_String, or are modified as
needed to reflect the flexibility in length. Since the
Unbounded_String type is private, relevant constructor and selector
operations are provided.
@begin{reason}
The transformation operations for fixed- and bounded-length strings that
are not necessarily length preserving are supplied for Unbounded_String
as procedures as well as functions.
This allows an implementation to do an initial allocation for
an unbounded string and to avoid further allocations as long
as the length does not exceed the allocated length.
@end{reason}
@end{Intro}

@begin{StaticSem}
@Leading@;The library package Strings.Unbounded has the following declaration:
@begin{example}
@key[with] Ada.Strings.Maps;
@ChildUnit{Parent=[Ada.Strings],Child=[Unbounded]}@key[package] Ada.Strings.Unbounded @key[is]
   @key[pragma] Preelaborate(Unbounded);

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00161-01]}
   @key[type] @AdaTypeDefn{Unbounded_String} @key[is] @key[private];@Chg{Version=[2],New=[
   @key[pragma] Preelaborable_Initialization(Unbounded_String);],Old=[]}

   @AdaObjDefn{Null_Unbounded_String} : @key[constant] Unbounded_String;

   @key[function] @AdaSubDefn{Length} (Source : @key[in] Unbounded_String) @key[return] Natural;

   @key[type] @AdaTypeDefn{String_Access} @key[is] @key[access] @key[all] String;
   @key[procedure] @AdaSubDefn{Free} (X : @key[in] @key[out] String_Access);

--@Examcom{ Conversion, Concatenation, and Selection functions}

   @key[function] @AdaSubDefn{To_Unbounded_String} (Source : @key[in] String)
      @key[return] Unbounded_String;

   @key[function] @AdaSubDefn{To_Unbounded_String} (Length : @key[in] Natural)
      @key[return] Unbounded_String;

   @key[function] @AdaSubDefn{To_String} (Source : @key[in] Unbounded_String) @key[return] String;

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[   @key[procedure] @AdaSubDefn{Set_Unbounded_String}
     (Target :    @key[out] Unbounded_String;
      Source : @key[in]     String);]}

   @key[procedure] @AdaSubDefn{Append} (Source   : @key[in out] Unbounded_String;
                     New_Item : @key[in] Unbounded_String);

   @key[procedure] @AdaSubDefn{Append} (Source   : @key[in out] Unbounded_String;
                     New_Item : @key[in] String);

   @key[procedure] @AdaSubDefn{Append} (Source   : @key[in out] Unbounded_String;
                     New_Item : @key[in] Character);

   @key[function] "&" (Left, Right : @key[in] Unbounded_String)
      @key[return] Unbounded_String;

   @key[function] "&" (Left : @key[in] Unbounded_String; Right : @key[in] String)
      @key[return] Unbounded_String;

   @key[function] "&" (Left : @key[in] String; Right : @key[in] Unbounded_String)
      @key[return] Unbounded_String;

   @key[function] "&" (Left : @key[in] Unbounded_String; Right : @key[in] Character)
      @key[return] Unbounded_String;

   @key[function] "&" (Left : @key[in] Character; Right : @key[in] Unbounded_String)
      @key[return] Unbounded_String;


   @key[function] @AdaSubDefn{Element} (Source : @key[in] Unbounded_String;
                     Index  : @key[in] Positive)
      @key[return] Character;

   @key[procedure] @AdaSubDefn{Replace_Element} (Source : @key[in] @key[out] Unbounded_String;
                              Index  : @key[in] Positive;
                              By     : @key[in] Character);


   @key[function] @AdaSubDefn{Slice} (Source : @key[in] Unbounded_String;
                   Low    : @key[in] Positive;
                   High   : @key[in] Natural)
      @key[return] String;

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[   @key[function] @AdaSubDefn{Unbounded_Slice}
      (Source : @key[in] Unbounded_String;
       Low    : @key[in] Positive;
       High   : @key[in] Natural)
          @key[return] Unbounded_String;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[   @key[procedure] @AdaSubDefn{Unbounded_Slice}
      (Source : @key[in]     Unbounded_String;
       Target :    @key[out] Unbounded_String;
       Low    : @key[in]     Positive;
       High   : @key[in]     Natural);]}

   @key[function] "="  (Left, Right : @key[in] Unbounded_String) @key[return] Boolean;

   @key[function] "="  (Left : @key[in] Unbounded_String; Right : @key[in] String)
     @key[return] Boolean;

   @key[function] "="  (Left : @key[in] String; Right : @key[in] Unbounded_String)
     @key[return] Boolean;

   @key[function] "<"  (Left, Right : @key[in] Unbounded_String) @key[return] Boolean;

   @key[function] "<"  (Left : @key[in] Unbounded_String; Right : @key[in] String)
     @key[return] Boolean;

   @key[function] "<"  (Left : @key[in] String; Right : @key[in] Unbounded_String)
     @key[return] Boolean;

   @key[function] "<=" (Left, Right : @key[in] Unbounded_String) @key[return] Boolean;

   @key[function] "<="  (Left : @key[in] Unbounded_String; Right : @key[in] String)
     @key[return] Boolean;

   @key[function] "<="  (Left : @key[in] String; Right : @key[in] Unbounded_String)
     @key[return] Boolean;

   @key[function] ">"  (Left, Right : @key[in] Unbounded_String) @key[return] Boolean;

   @key[function] ">"  (Left : @key[in] Unbounded_String; Right : @key[in] String)
     @key[return] Boolean;

   @key[function] ">"  (Left : @key[in] String; Right : @key[in] Unbounded_String)
     @key[return] Boolean;

   @key[function] ">=" (Left, Right : @key[in] Unbounded_String) @key[return] Boolean;

   @key[function] ">="  (Left : @key[in] Unbounded_String; Right : @key[in] String)
     @key[return] Boolean;

   @key[function] ">="  (Left : @key[in] String; Right : @key[in] Unbounded_String)
     @key[return] Boolean;


--@Examcom{ Search subprograms}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[   @key[function] @AdaSubDefn{Index} (Source  : @key[in] Unbounded_String;
                   Pattern : @key[in] String;
                   From    : @key[in] Positive;
                   Going   : @key[in] Direction := Forward;
                   Mapping : @key[in] Maps.Character_Mapping := Maps.Identity)
      @key[return] Natural;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[   @key[function] @AdaSubDefn{Index} (Source  : @key[in] Unbounded_String;
                   Pattern : @key[in] String;
                   From    : @key[in] Positive;
                   Going   : @key[in] Direction := Forward;
                   Mapping : @key[in] Maps.Character_Mapping_Function)
      @key[return] Natural;]}

   @key[function] @AdaSubDefn{Index} (Source   : @key[in] Unbounded_String;
                   Pattern  : @key[in] String;
                   Going    : @key[in] Direction := Forward;
                   Mapping  : @key[in] Maps.Character_Mapping
                                := Maps.Identity)
      @key[return] Natural;

   @key[function] @AdaSubDefn{Index} (Source   : @key[in] Unbounded_String;
                   Pattern  : @key[in] String;
                   Going    : @key[in] Direction := Forward;
                   Mapping  : @key[in] Maps.Character_Mapping_Function)
      @key[return] Natural;

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[   @key[function] @AdaSubDefn{Index} (Source  : @key[in] Unbounded_String;
                   Set     : @key[in] Maps.Character_Set;
                   From    : @key[in] Positive;
                   Test    : @key[in] Membership := Inside;
                   Going    : @key[in] Direction := Forward)
      @key[return] Natural;]}

   @key[function] @AdaSubDefn{Index} (Source : @key[in] Unbounded_String;
                   Set    : @key[in] Maps.Character_Set;
                   Test   : @key[in] Membership := Inside;
                   Going  : @key[in] Direction  := Forward) @key[return] Natural;

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[   @key[function] @AdaSubDefn{Index_Non_Blank} (Source : @key[in] Unbounded_String;
                             From   : @key[in] Positive;
                             Going  : @key[in] Direction := Forward)
      @key[return] Natural;]}

   @key[function] @AdaSubDefn{Index_Non_Blank} (Source : @key[in] Unbounded_String;
                             Going  : @key[in] Direction := Forward)
      @key[return] Natural;


   @key[function] @AdaSubDefn{Count} (Source   : @key[in] Unbounded_String;
                   Pattern  : @key[in] String;
                   Mapping  : @key[in] Maps.Character_Mapping
                                := Maps.Identity)
      @key[return] Natural;

   @key[function] @AdaSubDefn{Count} (Source   : @key[in] Unbounded_String;
                   Pattern  : @key[in] String;
                   Mapping  : @key[in] Maps.Character_Mapping_Function)
      @key[return] Natural;

   @key[function] @AdaSubDefn{Count} (Source   : @key[in] Unbounded_String;
                   Set      : @key[in] Maps.Character_Set)
      @key[return] Natural;


@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0031-1]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Find_Token} (Source : @key[in] Unbounded_String;
                         Set    : @key[in] Maps.Character_Set;
                         From   : @key[in] Positive;
                         Test   : @key[in] Membership;
                         First  : @key[out] Positive;
                         Last   : @key[out] Natural);]}

   @key[procedure] @AdaSubDefn{Find_Token} (Source : @key[in] Unbounded_String;
                         Set    : @key[in] Maps.Character_Set;
                         Test   : @key[in] Membership;
                         First  : @key[out] Positive;
                         Last   : @key[out] Natural);


@Keepnext@;--@Examcom{ String translation subprograms}

   @key[function] @AdaSubDefn{Translate} (Source  : @key[in] Unbounded_String;
                       Mapping : @key[in] Maps.Character_Mapping)
      @key[return] Unbounded_String;

   @key[procedure] @AdaSubDefn{Translate} (Source  : @key[in] @key[out] Unbounded_String;
                        Mapping : @key[in] Maps.Character_Mapping);

   @key[function] @AdaSubDefn{Translate} (Source  : @key[in] Unbounded_String;
                       Mapping : @key[in] Maps.Character_Mapping_Function)
      @key[return] Unbounded_String;

   @key[procedure] @AdaSubDefn{Translate} (Source  : @key[in] @key[out] Unbounded_String;
                        Mapping : @key[in] Maps.Character_Mapping_Function);

@Keepnext@;--@Examcom{ String transformation subprograms}

   @key[function] @AdaSubDefn{Replace_Slice} (Source   : @key[in] Unbounded_String;
                           Low      : @key[in] Positive;
                           High     : @key[in] Natural;
                           By       : @key[in] String)
      @key[return] Unbounded_String;

   @key[procedure] @AdaSubDefn{Replace_Slice} (Source   : @key[in] @key[out] Unbounded_String;
                            Low      : @key[in] Positive;
                            High     : @key[in] Natural;
                            By       : @key[in] String);

   @key[function] @AdaSubDefn{Insert} (Source   : @key[in] Unbounded_String;
                    Before   : @key[in] Positive;
                    New_Item : @key[in] String)
      @key[return] Unbounded_String;

   @key[procedure] @AdaSubDefn{Insert} (Source   : @key[in] @key[out] Unbounded_String;
                     Before   : @key[in] Positive;
                     New_Item : @key[in] String);

   @key[function] @AdaSubDefn{Overwrite} (Source    : @key[in] Unbounded_String;
                       Position  : @key[in] Positive;
                       New_Item  : @key[in] String)
      @key[return] Unbounded_String;

   @key[procedure] @AdaSubDefn{Overwrite} (Source    : @key[in] @key[out] Unbounded_String;
                        Position  : @key[in] Positive;
                        New_Item  : @key[in] String);

   @key[function] @AdaSubDefn{Delete} (Source  : @key[in] Unbounded_String;
                    From    : @key[in] Positive;
                    Through : @key[in] Natural)
      @key[return] Unbounded_String;

   @key[procedure] @AdaSubDefn{Delete} (Source  : @key[in] @key[out] Unbounded_String;
                     From    : @key[in] Positive;
                     Through : @key[in] Natural);

   @key[function] @AdaSubDefn{Trim} (Source : @key[in] Unbounded_String;
                  Side   : @key[in] Trim_End)
      @key[return] Unbounded_String;

   @key[procedure] @AdaSubDefn{Trim} (Source : @key[in] @key[out] Unbounded_String;
                   Side   : @key[in] Trim_End);

   @key[function] @AdaSubDefn{Trim} (Source : @key[in] Unbounded_String;
                  Left   : @key[in] Maps.Character_Set;
                  Right  : @key[in] Maps.Character_Set)
      @key[return] Unbounded_String;

   @key[procedure] @AdaSubDefn{Trim} (Source : @key[in] @key[out] Unbounded_String;
                   Left   : @key[in] Maps.Character_Set;
                   Right  : @key[in] Maps.Character_Set);


   @key[function] @AdaSubDefn{Head} (Source : @key[in] Unbounded_String;
                  Count  : @key[in] Natural;
                  Pad    : @key[in] Character := Space)
      @key[return] Unbounded_String;

   @key[procedure] @AdaSubDefn{Head} (Source : @key[in] @key[out] Unbounded_String;
                   Count  : @key[in] Natural;
                   Pad    : @key[in] Character := Space);

   @key[function] @AdaSubDefn{Tail} (Source : @key[in] Unbounded_String;
                  Count  : @key[in] Natural;
                  Pad    : @key[in] Character := Space)
      @key[return] Unbounded_String;

   @key[procedure] @AdaSubDefn{Tail} (Source : @key[in] @key[out] Unbounded_String;
                   Count  : @key[in] Natural;
                   Pad    : @key[in] Character := Space);

   @key[function] "*" (Left  : @key[in] Natural;
                 Right : @key[in] Character)
      @key[return] Unbounded_String;

   @key[function] "*" (Left  : @key[in] Natural;
                 Right : @key[in] String)
      @key[return] Unbounded_String;

   @key[function] "*" (Left  : @key[in] Natural;
                 Right : @key[in] Unbounded_String)
      @key[return] Unbounded_String;

@key[private]
   ... -- @Examcom{not specified by the language}
@key[end] Ada.Strings.Unbounded;
@end{example}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00360-01]}
@ChgAdded{Version=[2],Text=[The type Unbounded_String
needs finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}

Null_Unbounded_String represents the null String.
If an object of type Unbounded_String is not otherwise initialized, it
will be initialized to the same value as Null_Unbounded_String.

The function Length returns the length of the String represented by Source.

The type String_Access provides a (non-private) access type for explicit
processing of unbounded-length strings. The procedure Free performs
an unchecked deallocation of an object of type String_Access.

The function To_Unbounded_String(Source : in String)
returns an Unbounded_String that represents Source.
The function To_Unbounded_String(Length : in Natural)
returns an Unbounded_String that represents an uninitialized String
whose length is Length.

@Leading@;The function To_String returns the String with lower bound 1
represented by Source. To_String and To_Unbounded_String are related as follows:
@begin{itemize}
If S is a String, then To_String(To_Unbounded_String(S)) = S.

If U is an Unbounded_String, then To_Unbounded_String(To_String(U)) = U.
@end{itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[The procedure Set_Unbounded_String sets Target to an Unbounded_String that
represents Source.]}

For each of the Append procedures,
the resulting string represented by the Source parameter is given
by the concatenation of the original value of Source and the value
of New_Item.

Each of the "&" functions returns an Unbounded_String obtained by concatenating
the string or character given or represented by one of the parameters,
with the string or character given or represented by the other parameter,
and applying To_Unbounded_String to the concatenation result string.

The Element, Replace_Element, and Slice subprograms have the same effect
as the corresponding bounded-length string subprograms.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[The function Unbounded_Slice returns the slice at
positions Low through High in the string represented by Source as an
Unbounded_String. The procedure Unbounded_Slice sets Target to the
Unbounded_String representing the slice at positions Low through High in the
string represented by Source. Both
@Chg{Version=[3],New=[subprograms],Old=[routines]} propagate Index_Error
if Low > Length(Source)+1 or High > Length(Source).]}

Each of the functions "=", "<", ">", "<=", and ">="
returns the same result as the corresponding String
operation applied to the String values given or represented by Left and Right.

Each of the search subprograms (Index, Index_Non_Blank, Count, Find_Token)
has the same effect as the corresponding subprogram in Strings.Fixed applied
to the string represented by the Unbounded_String parameter.

The Translate function has
an analogous effect to the corresponding subprogram in Strings.Fixed.
The translation is applied to the string represented by the Unbounded_String
parameter, and the result is converted (via To_Unbounded_String) to an
Unbounded_String.

Each of the transformation functions (Replace_Slice, Insert, Overwrite,
Delete), selector functions (Trim, Head, Tail), and constructor functions
("*") is likewise analogous to its corresponding
subprogram in Strings.Fixed. For each of the subprograms,
the corresponding fixed-length string subprogram is applied to the string
represented by the Unbounded_String parameter, and
To_Unbounded_String is applied the result string.

For each of the procedures Translate,
Replace_Slice, Insert, Overwrite, Delete,
Trim, Head, and Tail, the resulting string represented by the Source parameter
is given by the corresponding function for fixed-length strings applied to
the string represented by Source's original value.
@end{StaticSem}

@begin{ImplReq}
No storage associated
with an Unbounded_String object shall be
lost upon assignment or scope exit.
@begin{ImplNote}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00301-01]}
A sample implementation of the private part of
the package and several of the subprograms appears in the @Chg{Version=[2],
New=[Ada 95 ],Old=[]}Rationale.

@end{ImplNote}
@end{ImplReq}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00360-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @b[Amendment Correction:] Type Unbounded_String is defined to need
  finalization. If the restriction No_Nested_Finalization (see
  @RefSecNum{Tasking Restrictions}) applies to the partition, and
  Unbounded_String does not have a controlled part, it will not be allowed in
  local objects in Ada 2005 whereas it would be allowed in original Ada 95.
  Such code is not portable, as most Ada compilers have a controlled part in
  Unbounded_String, and thus would be illegal.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
  @ChgAdded{Version=[2],Text=[Procedure Set_Unbounded_String,
  two Unbounded_Slice subprograms, and overloaded
  versions of Index and Index_Non_Blank are newly added to Strings.Unbounded.
  If Strings.Unbounded is
  referenced in a @nt{use_clause}, and an entity @i<E> with the same
  @nt{defining_identifier} as a new entity in Strings.Unbounded is defined in a
  package that is also referenced in a @nt{use_clause}, the entity @i<E> may no
  longer be use-visible, resulting in errors. This should be rare and is easily
  fixed if it does occur.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00161-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @b[Amendment Correction:] Added a @nt{pragma} Preelaborable_Initialization to
  type Unbounded_String, so that it can be used to declare default-initialized
  objects in preelaborated units.]}
@end{Extend95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0031-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
  An overloaded version of Find_Token is newly added to
  Strings.Unbounded. If Strings.Unbounded is referenced in a @nt{use_clause},
  and an entity @i<E> with a @nt{defining_identifier} of Find_Token is
  defined in a package that is also referenced in a @nt{use_clause}, the entity
  @i<E> may no longer be use-visible, resulting in errors. This should be rare
  and is easily fixed if it does occur.]}
@end{Incompatible2005}


@LabeledSubClause{String-Handling Sets and Mappings}

@begin{Intro}

The language-defined package Strings.Maps.Constants declares
Character_Set
and Character_Mapping
constants corresponding to classification and conversion functions
in package Characters.Handling.
@begin{discussion}
The Constants package is a child of Strings.Maps since it needs
visibility of the private part of Strings.Maps in order to
initialize the constants
in a preelaborable way (i.e. via aggregates versus function calls).
@end{discussion}

@end{Intro}

@begin{StaticSem}
@Leading@;The library package Strings.Maps.Constants has the following declaration:

@begin{example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00362-01]}
@ChildUnit{Parent=[Ada.Strings.Maps],Child=[Constants]}@key[package] Ada.Strings.Maps.Constants @key[is]
   @key[pragma] @Chg{Version=[2],New=[Pure],Old=[Preelaborate]}(Constants);

   @AdaObjDefn{Control_Set}           : @key[constant] Character_Set;
   @AdaObjDefn{Graphic_Set}           : @key[constant] Character_Set;
   @AdaObjDefn{Letter_Set}            : @key[constant] Character_Set;
   @AdaObjDefn{Lower_Set}             : @key[constant] Character_Set;
   @AdaObjDefn{Upper_Set}             : @key[constant] Character_Set;
   @AdaObjDefn{Basic_Set}             : @key[constant] Character_Set;
   @AdaObjDefn{Decimal_Digit_Set}     : @key[constant] Character_Set;
   @AdaObjDefn{Hexadecimal_Digit_Set} : @key[constant] Character_Set;
   @AdaObjDefn{Alphanumeric_Set}      : @key[constant] Character_Set;
   @AdaObjDefn{Special_Set}           : @key[constant] Character_Set;
   @AdaObjDefn{ISO_646_Set}           : @key[constant] Character_Set;

   @AdaObjDefn{Lower_Case_Map}        : @key[constant] Character_Mapping;
     --@Examcom{Maps to lower case for letters, else identity}
   @AdaObjDefn{Upper_Case_Map}        : @key[constant] Character_Mapping;
     --@Examcom{Maps to upper case for letters, else identity}
   @AdaObjDefn{Basic_Map}             : @key[constant] Character_Mapping;
     --@Examcom{Maps to basic letter for letters, else identity}

@key[private]
   ... -- @Examcom{not specified by the language}
@key[end] Ada.Strings.Maps.Constants;
@end{example}


Each of these constants represents a correspondingly named
set of characters or
character mapping in Characters.Handling
(see @refsecnum(The Package Characters.Handling)).
@end{StaticSem}

@begin{Notes}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0114-1]}
@ChgAdded{Version=[3],Text=[There are certain characters which are defined to be
lower case letters by ISO 10646 and are therefore allowed in identifiers, but
are not considered lower case letters by Ada.Strings.Maps.Constants.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is to maintain runtime compatibility with the
  Ada 95 definitions of these constants; existing correct programs could break if
  the definitions were changed in a way the programs did not anticipate.]}
@end{Reason}
@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00362-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Strings.Maps.Constants is now Pure,
  so it can be used in pure units.]}
@end{Extend95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0114-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a note to clarify that
  these constants don't have any relationship to the characters allowed in
  identifiers.]}
@end{DiffWord2005}


@LabeledSubClause{Wide_String Handling}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00302-03]}
Facilities for handling strings of Wide_Character elements are
found in the packages Strings.@!Wide_Maps, Strings.@!Wide_Fixed,
Strings.@!Wide_@!Bounded, Strings.@!Wide_@!Unbounded,
and Strings.@!Wide_Maps.@!Wide_@!Constants@Chg{Version=[2],New=[, and in the
functions Strings.@!Wide_Hash, Strings.@!Wide_Fixed.@!Wide_Hash,
Strings.@!Wide_Bounded.@!Wide_Hash, and Strings.@!Wide_Unbounded.@!Wide_Hash],Old=[]}.
They provide the same string-handling operations
as the corresponding packages@Chg{Version=[2],New=[ and functions],Old=[]}
for strings of Character elements.
@ChildUnit{Parent=[Ada.Strings],Child=[Wide_@!Fixed]}
@ChildUnit{Parent=[Ada.Strings],Child=[Wide_@!Bounded]}
@ChildUnit{Parent=[Ada.Strings],Child=[Wide_@!Unbounded]}
@ChildUnit{Parent=[Ada.Strings],Child=[Wide_@!Hash]}
@SubChildUnit{Parent=[Ada.Strings.Wide_@!Fixed],Child=[Wide_@!Hash]}
@SubChildUnit{Parent=[Ada.Strings.Wide_@!Bounded],Child=[Wide_@!Hash]}
@SubChildUnit{Parent=[Ada.Strings.Wide_@!Unbounded],Child=[Wide_@!Hash]}
@ChildUnit{Parent=[Ada.Strings.Wide_@!Maps],Child=[Wide_@!Constants]}
@end{Intro}

@begin{StaticSem}
The package Strings.Wide_Maps has the following declaration.
@begin{example}
@ChildUnit{Parent=[Ada.Strings],Child=[Wide_@!Maps]}@key[package] Ada.Strings.Wide_Maps @key[is]
   @key[pragma] Preelaborate(Wide_Maps);

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00161-01]}
   --@Examcom{ Representation for a set of Wide_Character values:}
   @key[type] @AdaTypeDefn{Wide_Character_Set} @key[is] @key[private];@Chg{Version=[2],New=[
   @key[pragma] Preelaborable_Initialization(Wide_Character_Set);],Old=[]}

   @AdaObjDefn{Null_Set} : @key[constant] Wide_Character_Set;

   @key[type] @AdaTypeDefn{Wide_Character_Range} @key[is]
     @key[record]
         Low  : Wide_Character;
         High : Wide_Character;
     @key[end] @key[record];
   -- @Examcom{Represents Wide_Character range Low..High}

   @key[type] @AdaTypeDefn{Wide_Character_Ranges} @key[is] @key[array] (Positive @key[range] <>)
      @key[of] Wide_Character_Range;

   @key[function] @AdaSubDefn{To_Set}    (Ranges : @key[in] Wide_Character_Ranges)
      @key[return] Wide_Character_Set;

   @key[function] @AdaSubDefn{To_Set}    (Span   : @key[in] Wide_Character_Range)
      @key[return] Wide_Character_Set;

   @key[function] @AdaSubDefn{To_Ranges} (Set    : @key[in] Wide_Character_Set)
      @key[return] Wide_Character_Ranges;

   @key[function] "="   (Left, Right : @key[in] Wide_Character_Set) @key[return] Boolean;

   @key[function] "@key[not]" (Right : @key[in] Wide_Character_Set)
      @key[return] Wide_Character_Set;
   @key[function] "@key[and]" (Left, Right : @key[in] Wide_Character_Set)
      @key[return] Wide_Character_Set;
   @key[function] "@key[or]"  (Left, Right : @key[in] Wide_Character_Set)
      @key[return] Wide_Character_Set;
   @key[function] "@key[xor]" (Left, Right : @key[in] Wide_Character_Set)
      @key[return] Wide_Character_Set;
   @key[function] "-"   (Left, Right : @key[in] Wide_Character_Set)
      @key[return] Wide_Character_Set;

   @key[function] @AdaSubDefn{Is_In} (Element : @key[in] Wide_Character;
                   Set     : @key[in] Wide_Character_Set)
      @key[return] Boolean;

   @key[function] @AdaSubDefn{Is_Subset} (Elements : @key[in] Wide_Character_Set;
                       Set      : @key[in] Wide_Character_Set)
      @key[return] Boolean;

   @key[function] "<=" (Left  : @key[in] Wide_Character_Set;
                  Right : @key[in] Wide_Character_Set)
      @key[return] Boolean @key[renames] Is_Subset;


   --@Examcom{ Alternative representation for a set of Wide_Character values:}
   @key[subtype] @AdaSubtypeDefn{Name=[Wide_Character_Sequence],Of=[Wide_String]} @key[is] Wide_String;

   @key[function] @AdaSubDefn{To_Set} (Sequence  : @key[in] Wide_Character_Sequence)
      @key[return] Wide_Character_Set;

   @key[function] @AdaSubDefn{To_Set} (Singleton : @key[in] Wide_Character)
      @key[return] Wide_Character_Set;

   @key[function] @AdaSubDefn{To_Sequence} (Set  : @key[in] Wide_Character_Set)
      @key[return] Wide_Character_Sequence;


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00161-01]}
   --@Examcom{ Representation for a Wide_Character to Wide_Character mapping:}
   @key[type] @AdaTypeDefn{Wide_Character_Mapping} @key[is] @key[private];@Chg{Version=[2],New=[
   @key[pragma] Preelaborable_Initialization(Wide_Character_Mapping);],Old=[]}

   @key[function] @AdaSubDefn{Value} (Map     : @key[in] Wide_Character_Mapping;
                   Element : @key[in] Wide_Character)
      @key[return] Wide_Character;

   @AdaObjDefn{Identity} : @key[constant] Wide_Character_Mapping;

   @key[function] @AdaSubDefn{To_Mapping} (From, To : @key[in] Wide_Character_Sequence)
      @key[return] Wide_Character_Mapping;

   @key[function] @AdaSubDefn{To_Domain} (Map : @key[in] Wide_Character_Mapping)
      @key[return] Wide_Character_Sequence;

   @key[function] @AdaSubDefn{To_Range}  (Map : @key[in] Wide_Character_Mapping)
      @key[return] Wide_Character_Sequence;


   @key{type} @AdaTypeDefn{Wide_Character_Mapping_Function} @key{is}
      @key{access} @key{function} (From : @key{in} Wide_Character) @key{return} Wide_Character;

@key[private]
   ... -- @Examcom{not specified by the language}
@key[end] Ada.Strings.Wide_Maps;
@end{example}

The context clause for each of the packages Strings.Wide_Fixed,
Strings.Wide_Bounded, and Strings.Wide_Unbounded
identifies Strings.Wide_Maps instead of Strings.Maps.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0223-1]}
@ChgAdded{Version=[3],Text=[Types Wide_Character_Set and Wide_Character_Mapping
need finalization.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00302-03]}
@Leading@;For each of the packages Strings.Fixed, Strings.Bounded,
Strings.Unbounded, and Strings.Maps.Constants@Chg{Version=[2],New=[, and
for functions Strings.Hash, Strings.Fixed.Hash, Strings.Bounded.Hash,
and Strings.Unbounded.Hash,],Old=[]}
the corresponding wide string package has the same contents except that
@begin{itemize}
Wide_Space replaces Space

Wide_Character replaces Character

Wide_String replaces String

Wide_Character_Set replaces Character_Set

Wide_Character_Mapping replaces Character_Mapping

Wide_Character_Mapping_Function replaces Character_Mapping_Function

Wide_Maps replaces Maps

Bounded_Wide_String replaces Bounded_String

Null_Bounded_Wide_String replaces Null_Bounded_String

To_Bounded_Wide_String replaces To_Bounded_String

To_Wide_String replaces To_String

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[Set_Bounded_Wide_String replaces Set_Bounded_String]}

Unbounded_Wide_String replaces Unbounded_String

Null_Unbounded_Wide_String replaces Null_Unbounded_String

Wide_String_Access replaces String_Access

To_Unbounded_Wide_String replaces To_Unbounded_String

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[Set_Unbounded_Wide_String replaces Set_Unbounded_String]}

@end{Itemize}

@Leading@keepnext@;The following additional declaration is present in
Strings.Wide_Maps.Wide_Constants:
@begin{example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@AdaObjDefn{Character_Set} : @key[constant] Wide_Maps.Wide_Character_Set;
--@Examcom{Contains each Wide_Character value WC such that}@Chg{Version=[2],New=[
--],Old=[]}@Examcom{Characters.@Chg{Version=[2],New=[Conversions.],Old=[]}Is_Character(WC) is True}
@end{example}
@end{StaticSem}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Text=[Each Wide_Character_Set constant in the package
Strings.Wide_Maps.Wide_Constants contains no values outside the Character
portion of Wide_Character. Similarly, each Wide_Character_Mapping constant in
this package is the identity mapping when applied to any element outside the
Character portion of Wide_Character.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00362-01]}
@ChgAdded{Version=[2],Text=[@nt{Pragma} Pure is replaced by
@nt{pragma} Preelaborate in Strings.Wide_Maps.Wide_Constants.]}

@begin{Notes}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
If a null Wide_Character_Mapping_Function is passed to any of the
Wide_String handling subprograms, Constraint_Error is propagated.

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00395-01]}
@ChgDeleted{Version=[2],Text=[Each Wide_Character_Set constant in the package
Strings.Wide_Maps.Wide_Constants contains no values outside the Character
portion of Wide_Character. Similarly, each Wide_Character_Mapping
constant in this package is the identity mapping when applied to
any element outside the Character portion of Wide_Character.]}
@end{Notes}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  Various new operations are added to Strings.Wide_Fixed, Strings.Wide_Bounded,
  and Strings.Wide_Unbounded. If one of these packages is referenced in a
  @nt{use_clause}, and an entity @i<E> with the same @nt{defining_identifier}
  as a new entity is defined in a package that is also referenced in a
  @nt{use_clause}, the entity @i<E> may no longer be use-visible, resulting in
  errors. This should be rare and is easily fixed if it does occur.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00161-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @b[Amendment Correction:] Added @nt{pragma} Preelaborable_Initialization to
  types Wide_Character_Set and Wide_Character_Mapping, so that they can be
  used to declare default-initialized objects in preelaborated units.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[Corrected the description of Character_Set.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[Added wide versions of Strings.Hash and
  Strings.Unbounded.Hash.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00362-01]}
  @ChgAdded{Version=[2],Text=[Added wording so that
  Strings.Wide_Maps.Wide_Constants does not change to Pure.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
  @ChgAdded{Version=[2],Text=[The second Note is now normative text, since
  there is no way to derive it from the other rules. It's a little
  weird given the use of Unicode character classifications in Ada 2005;
  but changing it would be inconsistent with Ada 95 and a one-to-one
  mapping isn't necessarily correct anyway.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0223-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction>: Identified Wide_Character_Set
  and Wide_Character_Mapping as needing finalization. It is likely that they
  are implemented with a controlled type, so this change is unlikely to make
  any difference in practice.]}
@end{DiffWord2005}


@LabeledAddedSubClause{Version=[2],Name=[Wide_Wide_String Handling]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Text=[Facilities for handling strings of
Wide_Wide_Character elements are found in
the packages Strings.@!Wide_Wide_@!Maps, Strings.@!Wide_Wide_@!Fixed,
Strings.@!Wide_Wide_@!Bounded, Strings.@!Wide_Wide_@!Unbounded, and
Strings.@!Wide_Wide_@!Maps.@!Wide_Wide_@!Constants, and in the
functions Strings.@!Wide_Wide_@!Hash, Strings.@!Wide_Wide_@!Fixed.@!Wide_Wide_@!Hash,
Strings.@!Wide_Wide_@!Bounded.@!Wide_@!Wide_@!Hash, and
Strings.@!Wide_Wide_@!Unbounded.@!Wide_@!Wide_@!Hash.
They provide the same
string-handling operations as the corresponding packages and functions
for strings of Character elements.
@ChildUnit{Parent=[Ada.Strings],Child=[Wide_Wide_Fixed]}
@ChildUnit{Parent=[Ada.Strings],Child=[Wide_Wide_Bounded]}
@ChildUnit{Parent=[Ada.Strings],Child=[Wide_Wide_Unbounded]}
@ChildUnit{Parent=[Ada.Strings],Child=[Wide_Wide_@!Hash]}
@SubChildUnit{Parent=[Ada.Strings.Wide_Wide_@!Fixed],Child=[Wide_Wide_@!Hash]}
@SubChildUnit{Parent=[Ada.Strings.Wide_Wide_@!Bounded],Child=[Wide_Wide_@!Hash]}
@SubChildUnit{Parent=[Ada.Strings.Wide_Wide_@!Unbounded],Child=[Wide_Wide_@!Hash]}
@ChildUnit{Parent=[Ada.Strings.Wide_Wide_@!Maps],Child=[Wide_Wide_@!Constants]}]}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The library package Strings.Wide_Wide_Maps has the following declaration.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key<package> Ada.Strings.Wide_Wide_Maps @key<is>@ChildUnit{Parent=[Ada.Strings],Child=[Wide_Wide_Maps]}
   @key<pragma> Preelaborate(Wide_Wide_Maps);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @Examcom[Representation for a set of Wide_Wide_Character values:]
   @key<type> @AdaTypeDefn{Wide_Wide_Character_Set} @key<is private>;
   @key<pragma> Preelaborable_Initialization(Wide_Wide_Character_Set);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Null_Set} : @key<constant> Wide_Wide_Character_Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<type> @AdaTypeDefn{Wide_Wide_Character_Range} @key<is>
      @key<record>
         Low  : Wide_Wide_Character;
         High : Wide_Wide_Character;
      @key<end record>;
   -- @Examcom[Represents Wide_Wide_Character range Low..High]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<type> @AdaTypeDefn{Wide_Wide_Character_Ranges} @key<is array> (Positive @key<range> <>)
         @key<of> Wide_Wide_Character_Range;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{To_Set} (Ranges : @key<in> Wide_Wide_Character_Ranges)
         @key<return> Wide_Wide_Character_Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{To_Set} (Span : @key<in> Wide_Wide_Character_Range)
         @key<return> Wide_Wide_Character_Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{To_Ranges} (Set : @key<in> Wide_Wide_Character_Set)
         @key<return> Wide_Wide_Character_Ranges;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> "=" (Left, Right : @key<in> Wide_Wide_Character_Set) @key<return> Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> "@key<not>" (Right : @key<in> Wide_Wide_Character_Set)
         @key<return> Wide_Wide_Character_Set;
   @key<function> "@key<and>" (Left, Right : @key<in> Wide_Wide_Character_Set)
         @key<return> Wide_Wide_Character_Set;
   @key<function> "@key<or>" (Left, Right : @key<in> Wide_Wide_Character_Set)
         @key<return> Wide_Wide_Character_Set;
   @key<function> "@key<xor>" (Left, Right : @key<in> Wide_Wide_Character_Set)
         @key<return> Wide_Wide_Character_Set;
   @key<function> "-" (Left, Right : @key<in> Wide_Wide_Character_Set)
         @key<return> Wide_Wide_Character_Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Is_In} (Element : @key<in> Wide_Wide_Character;
                   Set     : @key<in> Wide_Wide_Character_Set)
         @key<return> Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Is_Subset} (Elements : @key<in> Wide_Wide_Character_Set;
                       Set      : @key<in> Wide_Wide_Character_Set)
         @key<return> Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> "<=" (Left  : @key<in> Wide_Wide_Character_Set;
                  Right : @key<in> Wide_Wide_Character_Set)
         @key<return> Boolean @key<renames> Is_Subset;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @Examcom[Alternative representation for a set of Wide_Wide_Character values:]
   @key<subtype> @AdaSubtypeDefn{Name=[Wide_Wide_Character_Sequence],Of=[Wide_Wide_String]} @key<is> Wide_Wide_String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{To_Set} (Sequence : @key<in> Wide_Wide_Character_Sequence)
         @key<return> Wide_Wide_Character_Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{To_Set} (Singleton : @key<in> Wide_Wide_Character)
         @key<return> Wide_Wide_Character_Set;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{To_Sequence} (Set : @key<in> Wide_Wide_Character_Set)
         @key<return> Wide_Wide_Character_Sequence;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @Examcom[Representation for a Wide_Wide_Character to Wide_Wide_Character]
   -- @Examcom[mapping:]
   @key<type> @AdaTypeDefn{Wide_Wide_Character_Mapping} @key<is private>;
   @key<pragma> Preelaborable_Initialization(Wide_Wide_Character_Mapping);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Value} (Map     : @key<in> Wide_Wide_Character_Mapping;
                   Element : @key<in> Wide_Wide_Character)
         @key<return> Wide_Wide_Character;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Identity} : @key<constant> Wide_Wide_Character_Mapping;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{To_Mapping} (From, To : @key<in> Wide_Wide_Character_Sequence)
         @key<return> Wide_Wide_Character_Mapping;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{To_Domain} (Map : @key<in> Wide_Wide_Character_Mapping)
         @key<return> Wide_Wide_Character_Sequence;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{To_Range} (Map : @key<in> Wide_Wide_Character_Mapping)
         @key<return> Wide_Wide_Character_Sequence;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<type> @AdaTypeDefn{Wide_Wide_Character_Mapping_Function} @key<is>
         @key<access function> (From : @key<in> Wide_Wide_Character)
         @key<return> Wide_Wide_Character;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key<private>
   ... -- @Examcom[not specified by the language]
@key<end> Ada.Strings.Wide_Wide_Maps;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@ChildUnit{Parent=[Ada.Strings.Wide_@!Maps],Child=[Wide_@!Constants]}
The context clause for each of the packages Strings.Wide_Wide_Fixed,
Strings.Wide_Wide_Bounded, and Strings.Wide_Wide_Unbounded identifies
Strings.Wide_Wide_Maps instead of Strings.Maps.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0223-1]}
@ChgAdded{Version=[3],Text=[Types Wide_Wide_Character_Set and
Wide_Wide_Character_Mapping need finalization.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For each of the packages
Strings.@!Fixed, Strings.@!Bounded, Strings.@!Unbounded, and Strings.@!Maps.Constants,
and for functions Strings.@!Hash, Strings.@!Fixed.Hash, Strings.@!Bounded.Hash,
and Strings.@!Unbounded.Hash,
the corresponding wide wide string package or function has the same contents
except that]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Wide_Wide_Space replaces Space]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Wide_Wide_Character replaces Character]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Wide_Wide_String replaces String]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Wide_Wide_Character_Set replaces Character_Set]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Wide_Wide_Character_Mapping replaces Character_Mapping]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Wide_Wide_Character_Mapping_Function replaces Character_Mapping_Function]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Wide_Wide_Maps replaces Maps]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Bounded_Wide_Wide_String replaces Bounded_String]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Null_Bounded_Wide_Wide_String replaces Null_Bounded_String]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[To_Bounded_Wide_Wide_String replaces To_Bounded_String]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[To_Wide_Wide_String replaces To_String]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[Set_Bounded_Wide_Wide_String replaces Set_Bounded_String]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Unbounded_Wide_Wide_String replaces Unbounded_String]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Null_Unbounded_Wide_Wide_String replaces Null_Unbounded_String]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Wide_Wide_String_Access replaces String_Access]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[To_Unbounded_Wide_Wide_String replaces To_Unbounded_String]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[Set_Unbounded_Wide_Wide_String replaces Set_Unbounded_String]}

@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[The following
additional declarations are present in
Strings.Wide_Wide_Maps.Wide_Wide_Constants:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@AdaObjDefn{Character_Set} : @key<constant> Wide_Wide_Maps.Wide_Wide_Character_Set;
-- @Examcom[Contains each Wide_Wide_Character value WWC such that]
-- @Examcom[Characters.Conversions.Is_Character(WWC) is True]
@AdaObjDefn{Wide_Character_Set} : @key<constant> Wide_Wide_Maps.Wide_Wide_Character_Set;
-- @Examcom[Contains each Wide_Wide_Character value WWC such that]
-- @Examcom[Characters.Conversions.Is_Wide_Character(WWC) is True]]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Text=[Each Wide_Wide_Character_Set constant in the package
Strings.@!Wide_Wide_@!Maps.@!Wide_Wide_@!Constants contains no values outside the Character
portion of Wide_Wide_@!Character. Similarly, each Wide_Wide_@!Character_@!Mapping constant in
this package is the identity mapping when applied to any element outside the
Character portion of Wide_Wide_Character.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Text=[@nt{Pragma} Pure is replaced by
@nt{pragma} Preelaborate in Strings.Wide_Wide_Maps.Wide_Wide_Constants.]}

@end{StaticSem}

@begin{Notes}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
If a null Wide_Wide_Character_Mapping_Function is passed to any of the
Wide_Wide_String handling subprograms, Constraint_Error is propagated.]}
@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The double-wide string-handling packages (Strings.Wide_Wide_Maps,
  Strings.Wide_Wide_Fixed, Strings.Wide_Wide_Bounded,
  Strings.Wide_Wide_Unbounded, and Strings.Wide_Wide_Maps.Wide_Wide_Constants),
  and functions Strings.Wide_Wide_Hash and
  Strings.Wide_Wide_Unbounded.Wide_Wide_Hash are new.]}
@end{Extend95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0223-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction>: Identified Wide_Wide_Character_Set
  and Wide_Wide_Character_Mapping as needing finalization. It is likely that
  they are implemented with a controlled type, so this change is unlikely to
  make any difference in practice.]}
@end{DiffWord2005}




@LabeledAddedSubClause{Version=[2],Name=[String Hashing]}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The library
function Strings.Hash has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key<with> Ada.Containers;
@key<function> Ada.Strings.Hash (Key : String) @key<return> Containers.Hash_Type;@SubChildUnit{Parent=[Ada.Strings],Child=[Hash]}
@key<pragma> Pure(Hash);]}
@end{Example}

@begin{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns an implementation-defined
value which is a function of the value of Key. If @i<A> and @i<B> are strings
such that @i<A> equals @i<B>, Hash(@i<A>) equals Hash(@i<B>).]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The values returned by Strings.Hash.]}]}

@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The library
function Strings.Fixed.Hash has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key<with> Ada.Containers, Ada.Strings.Hash;
@key<function> Ada.Strings.Fixed.Hash (Key : String) @key<return> Containers.Hash_Type
   @key<renames> Ada.Strings.Hash;
@key<pragma> Pure(Hash);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
function Strings.Bounded.Hash has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key<with> Ada.Containers;
@key<generic>@SubChildUnit{Parent=[Ada.Strings.Bounded],Child=[Hash]}
   @key<with package> Bounded @key<is>
                     @key<new> Ada.Strings.Bounded.Generic_Bounded_Length (<>);
@key<function> Ada.Strings.Bounded.Hash (Key : Bounded.Bounded_String)
   @key<return> Containers.Hash_Type;
@key<pragma> Preelaborate(Hash);]}
@end{Example}

@begin{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0001-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[3],New=[Equivalent to],
Old=[Strings.Bounded.Hash is
equivalent to the function call]} Strings.Hash (Bounded.To_String (Key));]}

@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The library
function Strings.Unbounded.Hash has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key<with> Ada.Containers;
@key<function> Ada.Strings.Unbounded.Hash (Key : Unbounded_String)@SubChildUnit{Parent=[Ada.Strings.Unbounded],Child=[Hash]}
   @key<return> Containers.Hash_Type;
@key<pragma> Preelaborate(Hash);]}
@end{Example}

@begin{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0001-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@Chg{Version=[3],New=[Equivalent to],
Old=[Strings.Unbounded.Hash is
equivalent to the function call]}
Strings.Hash (To_String (Key));]}

@end{DescribeCode}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The library function
Strings.Hash_Case_Insensitive has the following declaration:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key<with> Ada.Containers;
@key<function> Ada.Strings.Hash_Case_Insensitive (Key : String)@SubChildUnit{Parent=[Ada.Strings],Child=[Hash_Case_Insensitive]}
   @key<return> Containers.Hash_Type;
@key<pragma> Pure(Hash_Case_Insensitive);]}
@end{Example}

@begin{DescribeCode}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns an implementation-defined
value which is a function of the value of Key, converted to lower case. If
A and B are strings such that Strings.Equal_Case_Insensitive (A, B) (see
@RefSecNum{String Comparison}) is
True, then Hash_Case_Insensitive(A) equals Hash_Case_Insensitive(B).]}
@end{DescribeCode}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The library function
Strings.Fixed.Hash_Case_Insensitive has the following declaration:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key<with> Ada.Containers, Ada.Strings.Hash_Case_Insensitive;
@key[function] Ada.Strings.Fixed.Hash_Case_Insensitive (Key : String)@SubChildUnit{Parent=[Ada.Strings.Fixed],Child=[Hash_Case_Insensitive]}
   @key[return] Containers.Hash_Type @key[renames] Ada.Strings.Hash_Case_Insensitive;
@key[pragma] Pure(Hash_Case_Insensitive);]}
@end{Example}


@ChgRef{Version=[2],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
function Strings.Bounded.Hash_Case_Insensitive has the following declaration:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key<with> Ada.Containers;
@key[generic]
   @key[with package] Bounded @key[is]
          @key[new] Ada.Strings.Bounded.Generic_Bounded_Length (<>);
@key[function] Ada.Strings.Bounded.Hash_Case_Insensitive@SubChildUnit{Parent=[Ada.Strings.Bounded],Child=[Hash_Case_Insensitive]}
   (Key : Bounded.Bounded_String) @key[return] Containers.Hash_Type;
@key[pragma] Preelaborate(Hash_Case_Insensitive);]}
@end{Example}

@begin{DescribeCode}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to
Strings.Hash_Case_Insensitive (Bounded.To_String (Key));]}
@end{DescribeCode}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The library function
Strings.Unbounded.Hash_Case_Insensitive has the following declaration:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key<with> Ada.Containers;
@key[function] Ada.Strings.Unbounded.Hash_Case_Insensitive@SubChildUnit{Parent=[Ada.Strings.Unbounded],Child=[Hash_Case_Insensitive]}
   (Key : Unbounded_String) @key[return] Containers.Hash_Type;
@key[pragma] Preelaborate(Hash_Case_Insensitive);]}
@end{Example}

@begin{DescribeCode}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to
Strings.Hash_Case_Insensitive (To_String (Key));]}
@end{DescribeCode}

@end{StaticSem}


@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
@ChgAdded{Version=[2],Text=[The Hash functions should be good hash
functions, returning a wide spread of values for different string values. It
should be unlikely for similar strings to return the same value.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Strings.Hash should be good a hash
function, returning a wide spread of values for different string values,
and similar strings should rarely return the same value.]}]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The other functions are defined in terms of
  Strings.Hash, so they don't need separate advice in the Annex.]}
@end{Ramification}
@end{ImplAdvice}


@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00302-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The Strings.Hash, Strings.Fixed.Hash, Strings.Bounded.Hash, and
  Strings.Unbounded.Hash functions are new.]}
@end{Extend95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The Strings.Hash_Case_Insensitive, Strings.Fixed.Hash_Case_Insensitive,
  Strings.Bounded.Hash_Case_Insensitive, and
  Strings.Unbounded.Hash_Case_Insensitive functions are new.]}
@end{Extend2005}


@LabeledAddedSubClause{Version=[3],Name=[String Comparison]}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The library function
Strings.Equal_Case_Insensitive has the following declaration:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Ada.Strings.Equal_Case_Insensitive (Left, Right : String)@SubChildUnit{Parent=[Ada.Strings],Child=[Equal_Case_Insensitive]}
   @key[return] Boolean;
@key[pragma] Pure(Equal_Case_Insensitive);]}
@end{Example}

@begin{DescribeCode}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Compares strings Left and Right,
converted to lower case, for equality.]}
@end{DescribeCode}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The library function
Strings.Fixed.Equal_Case_Insensitive has the following declaration:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[with] Ada.Strings.Equal_Case_Insensitive;
@key[function] Ada.Strings.Fixed.Equal_Case_Insensitive@SubChildUnit{Parent=[Ada.Strings.Fixed],Child=[Equal_Case_Insensitive]}
   (Left, Right : String) @key[return] Boolean
      @key[renames] Ada.Strings.Equal_Case_Insensitive;
@key[pragma] Pure(Equal_Case_Insensitive);]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The generic library
function Strings.Bounded.Equal_Case_Insensitive has the following declaration:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[generic]
   @key[with package] Bounded @key[is]
          @key[new] Ada.Strings.Bounded.Generic_Bounded_Length (<>);
@key[function] Ada.Strings.Bounded.Equal_Case_Insensitive@SubChildUnit{Parent=[Ada.Strings.Bounded],Child=[Equal_Case_Insensitive]}
   (Left, Right : Bounded.Bounded_String) @key[return] Boolean;
@key[pragma] Preelaborate(Equal_Case_Insensitive);]}
@end{Example}

@begin{DescribeCode}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to
Strings.Equal_Case_Insensitive (Bounded.To_String (Left), Bounded.To_String (Right));]}
@end{DescribeCode}


@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The library function
Strings.Unbounded.Equal_Case_Insensitive has the following declaration:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Ada.Strings.Unbounded.Equal_Case_Insensitive@SubChildUnit{Parent=[Ada.Strings.Unbounded],Child=[Equal_Case_Insensitive]}
   (Left, Right : Unbounded_String) @key[return] Boolean;
@key[pragma] Preelaborate(Equal_Case_Insensitive);]}
@end{Example}

@begin{DescribeCode}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to
Strings.Equal_Case_Insensitive (To_String (Left), To_String (Right));]}
@end{DescribeCode}


@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The library function
Strings.Less_Case_Insensitive has the following declaration:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Ada.Strings.Less_Case_Insensitive (Left, Right : String)@SubChildUnit{Parent=[Ada.Strings],Child=[Less_Case_Insensitive]}
   @key[return] Boolean;
@key[pragma] Pure(Less_Case_Insensitive);]}
@end{Example}

@begin{DescribeCode}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Performs a lexicographic comparison
of strings Left and Right, converted to lower case.]}
@end{DescribeCode}


@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The library function
Strings.Fixed.Less_Case_Insensitive has the following declaration:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[with] Ada.Strings.Less_Case_Insensitive;
@key[function] Ada.Strings.Fixed.Less_Case_Insensitive@SubChildUnit{Parent=[Ada.Strings.Fixed],Child=[Less_Case_Insensitive]}
   (Left, Right : String) @key[return] Boolean
      @key[renames] Ada.Strings.Less_Case_Insensitive;
@key[pragma] Pure(Less_Case_Insensitive);]}
@end{Example}


@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The generic library
function Strings.Bounded.Less_Case_Insensitive has the following declaration:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[generic]
   @key[with package] Bounded @key[is]
          @key[new] Ada.Strings.Bounded.Generic_Bounded_Length (<>);
@key[function] Ada.Strings.Bounded.Less_Case_Insensitive@SubChildUnit{Parent=[Ada.Strings.Bounded],Child=[Less_Case_Insensitive]}
  (Left, Right : Bounded.Bounded_String) @key[return] Boolean;
@key[pragma] Preelaborate(Less_Case_Insensitive);]}
@end{Example}

@begin{DescribeCode}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to
Strings.Less_Case_Insensitive (Bounded.To_String (Left), Bounded.To_String (Right));]}
@end{DescribeCode}


@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The library function
Strings.Unbounded.Less_Case_Insensitive has the following declaration:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[function] Ada.Strings.Unbounded.Less_Case_Insensitive@SubChildUnit{Parent=[Ada.Strings.Unbounded],Child=[Less_Case_Insensitive]}
  (Left, Right : Unbounded_String) @key[return] Boolean;
@key[pragma] Preelaborate(Less_Case_Insensitive);]}
@end{Example}

@begin{DescribeCode}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Equivalent to
Strings.Less_Case_Insensitive (To_String (Left), To_String (Right));]}
@end{DescribeCode}

@end{StaticSem}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The Strings.Equal_Case_Insensitive, Strings.Fixed.Equal_Case_Insensitive,
  Strings.Bounded.Equal_Case_Insensitive,
  Strings.Unbounded.Equal_Case_Insensitive,
  Strings.Less_Case_Insensitive, Strings.Fixed.Less_Case_Insensitive,
  Strings.Bounded.Less_Case_Insensitive,
  Strings.Unbounded.Less_Case_Insensitive functions are new.]}
@end{Extend2005}


@LabeledAddedSubClause{Version=[3],Name=[String Encoding]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Text=[Facilities for encoding, decoding, and converting strings in various character
encoding schemes are provided by packages Strings.@!UTF_Encoding,
Strings.@!UTF_Encoding.@!Conversions, Strings.@!UTF_Encoding.@!Strings,
Strings.@!UTF_Encoding.@!Wide_Strings, and
Strings.@!UTF_Encoding.@!Wide_Wide_Strings.]}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The encoding library packages have
the following declarations:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Text=[@ChildUnit{Parent=[Ada.Strings],Child=[UTF_Encoding]}@key[package] Ada.Strings.UTF_Encoding @key[is]
   @key[pragma] Pure (UTF_Encoding);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   -- @Examcom[Declarations common to the string encoding packages]
   @key[type] @AdaTypeDefn{Encoding_Scheme} @key[is] (UTF_8, UTF_16BE, UTF_16LE);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[subtype] @AdaSubtypeDefn{Name=[UTF_String],Of=[String]} @key[is] String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[subtype] @AdaSubtypeDefn{Name=[UTF_8_String],Of=[String]} @key[is] String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[subtype] @AdaSubtypeDefn{Name=[UTF_16_Wide_String],Of=[Wide_String]} @key[is] Wide_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @AdaExcDefn{Encoding_Error} : @key[exception];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @AdaObjDefn{BOM_8}    : @key[constant] UTF_8_String :=
                Character'Val(16#EF#) &
                Character'Val(16#BB#) &
                Character'Val(16#BF#);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @AdaObjDefn{BOM_16BE} : @key[constant] UTF_String :=
                Character'Val(16#FE#) &
                Character'Val(16#FF#);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @AdaObjDefn{BOM_16LE} : @key[constant] UTF_String :=
                Character'Val(16#FF#) &
                Character'Val(16#FE#);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @AdaObjDefn{BOM_16}   : @key[constant] UTF_16_Wide_String :=
               (1 => Wide_Character'Val(16#FEFF#));]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Encoding} (Item    : UTF_String;
                      Default : Encoding_Scheme := UTF_8)
   @key[return] Encoding_Scheme;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[end] Ada.Strings.UTF_Encoding;]}


@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Text=[@ChildUnit{Parent=[Ada.Strings.UTF_Encoding],Child=[Conversions]}@key[package] Ada.Strings.UTF_Encoding.Conversions @key[is]
   @key[pragma] Pure (Conversions);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   -- @Examcom{Conversions between various encoding schemes}
   @key[function] @AdaSubDefn{Convert} (Item          : UTF_String;
                     Input_Scheme  : Encoding_Scheme;
                     Output_Scheme : Encoding_Scheme;
                     Output_BOM    : Boolean := False) @key[return] UTF_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Convert} (Item          : UTF_String;
                     Input_Scheme  : Encoding_Scheme;
                     Output_BOM    : Boolean := False)
      @key[return] UTF_16_Wide_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Convert} (Item          : UTF_8_String;
                     Output_BOM    : Boolean := False)
      @key[return] UTF_16_Wide_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Convert} (Item          : UTF_16_Wide_String;
                     Output_Scheme : Encoding_Scheme;
                     Output_BOM    : Boolean := False) @key[return] UTF_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Convert} (Item          : UTF_16_Wide_String;
                     Output_BOM    : Boolean := False) @key[return] UTF_8_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[end] Ada.Strings.UTF_Encoding.Conversions;]}


@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Text=[@ChildUnit{Parent=[Ada.Strings.UTF_Encoding],Child=[Strings]}@key[package] Ada.Strings.UTF_Encoding.Strings @key[is]
   @key[pragma] Pure (Strings);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   -- @Examcom{Encoding / decoding between String and various encoding schemes}
   @key[function] @AdaSubDefn{Encode} (Item          : String;
                    Output_Scheme : Encoding_Scheme;
                    Output_BOM    : Boolean  := False) @key[return] UTF_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Encode} (Item       : String;
                    Output_BOM : Boolean  := False) @key[return] UTF_8_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Encode} (Item       : String;
                    Output_BOM : Boolean  := False)
      @key[return] UTF_16_Wide_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Decode} (Item         : UTF_String;
                    Input_Scheme : Encoding_Scheme) @key[return] String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Decode} (Item : UTF_8_String) @key[return] String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Decode} (Item : UTF_16_Wide_String) @key[return] String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[end] Ada.Strings.UTF_Encoding.Strings;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Text=[@ChildUnit{Parent=[Ada.Strings.UTF_Encoding],Child=[Wide_Strings]}@key[package] Ada.Strings.UTF_Encoding.Wide_Strings @key[is]
   @key[pragma] Pure (Wide_Strings);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   -- @Examcom{Encoding / decoding between Wide_String and various encoding schemes}
   @key[function] @AdaSubDefn{Encode} (Item          : Wide_String;
                    Output_Scheme : Encoding_Scheme;
                    Output_BOM    : Boolean  := False) @key[return] UTF_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Encode} (Item       : Wide_String;
                    Output_BOM : Boolean  := False) @key[return] UTF_8_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Encode} (Item       : Wide_String;
                    Output_BOM : Boolean  := False)
      @key[return] UTF_16_Wide_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Decode} (Item         : UTF_String;
                    Input_Scheme : Encoding_Scheme) @key[return] Wide_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Decode} (Item : UTF_8_String) @key[return] Wide_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Decode} (Item : UTF_16_Wide_String) @key[return] Wide_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[end] Ada.Strings.UTF_Encoding.Wide_Strings;]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Text=[@ChildUnit{Parent=[Ada.Strings.UTF_Encoding],Child=[Wide_Wide_Strings]}@key[package] Ada.Strings.UTF_Encoding.Wide_Wide_Strings @key[is]
   @key[pragma] Pure (Wide_Wide_Strings);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   -- @Examcom{Encoding / decoding between Wide_Wide_String and various encoding schemes}
   @key[function] @AdaSubDefn{Encode} (Item          : Wide_Wide_String;
                    Output_Scheme : Encoding_Scheme;
                    Output_BOM    : Boolean  := False) @key[return] UTF_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Encode} (Item       : Wide_Wide_String;
                    Output_BOM : Boolean  := False) @key[return] UTF_8_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Encode} (Item       : Wide_Wide_String;
                    Output_BOM : Boolean  := False)
      @key[return] UTF_16_Wide_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Decode} (Item         : UTF_String;
                    Input_Scheme : Encoding_Scheme) @key[return] Wide_Wide_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Decode} (Item : UTF_8_String) @key[return] Wide_Wide_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Decode} (Item : UTF_16_Wide_String) @key[return] Wide_Wide_String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[end] Ada.Strings.UTF_Encoding.Wide_Wide_Strings;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[The type Encoding_Scheme defines encoding schemes.
UTF_8 corresponds to the UTF-8 encoding scheme defined by Annex D of ISO/IEC
10646. UTF_16BE corresponds to the UTF-16 encoding scheme defined by Annex C of
ISO/IEC 10646 in 8 bit, big-endian order; and UTF_16LE corresponds to the
UTF-16 encoding scheme in 8 bit, little-endian
order.@Defn{encoding scheme}@Defn{character encoding}@Defn{UTF-8}@Defn{UTF-16}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Text=[The subtype UTF_String is used to represent a String
of 8-bit values containing a sequence of values encoded in one of three ways
(UTF-8, UTF-16BE, or UTF-16LE). The subtype UTF_8_String is used to represent a
String of 8-bit values containing a sequence of values encoded in UTF-8. The
subtype UTF_16_Wide_String is used to represent a Wide_String of 16-bit values
containing a sequence of values encoded in UTF-16.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[The BOM_8, BOM_16BE, BOM_16LE, and BOM_16 constants
correspond to values used at the start of a string to indicate the encoding.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[Each of the Convert and Encode functions returns a UTF_String (respectively
UTF_8_String and UTF_16_String) value whose characters have position values
that correspond to the encoding of the Item parameter according to the
encoding scheme required by the function or specified by its Output_Scheme
parameter. For UTF_8, no overlong encoding is returned. A BOM is included
at the start of the returned string if the Output_BOM parameter is set to
True. The lower bound of the returned string is 1.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[Each of the Encode functions takes a String,
Wide_String, or Wide_Wide_String Item parameter that is assumed to be an array
of unencoded characters. Each of the Convert functions takes a UTF_String
(respectively UTF_8_String and UTF_16_String) Item parameter that is assumed to
contain characters whose position values correspond to a valid encoding sequence
according to the encoding scheme required by the function or specified by its
Input_Scheme parameter.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[Each of the Decode functions takes a UTF_String
(respectively UTF_8_String and UTF_16_String) Item parameter which is assumed to
contain characters whose position values correspond to a valid encoding sequence
according to the encoding scheme required by the function or specified by its
Input_Scheme parameter, and returns the corresponding String, Wide_String, or
value. The lower bound of the returned string is 1.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[For each of the Convert and Decode functions, an
initial BOM in the input that matches the expected encoding scheme is ignored,
and a different initial BOM causes Encoding_Error to be propagated.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The exception Encoding_Error is also propagated in the following
situations:]}
@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[By a Decode function when a UTF encoded
    string contains an invalid encoding sequence.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[By a Decode function when the expected encoding is
    UTF-16BE or UTF-16LE and the input string has an odd length.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Text=[By a Decode function yielding a String when
    the decoding of a sequence results in a code point whose value exceeds
    16#FF#.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We use "code point" here as that is what
  ISO 10646:2011 does and this text is directly referring to the contents of
  that standard; elsewhere in this Standard we have used "code position"
  to represent the same concept.]}
@end{Discussion}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[By a Decode function yielding a Wide_String when
    the decoding of a sequence results in a code point whose value exceeds
    16#FFFF#.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Text=[By an Encode function taking a Wide_String as
    input when an invalid character appears in the input. In particular, the
    characters whose position is in the range 16#D800# .. 16#DFFF# are invalid
    because they conflict with UTF-16 surrogate encodings, and the characters
    whose position is 16#FFFE# or 16#FFFF# are also invalid because they
    conflict with BOM codes.]}
@end{Itemize}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Encoding (Item    : UTF_String;
                   Default : Encoding_Scheme := UTF_8)
   @key[return] Encoding_Scheme;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Inspects a UTF_String value to
determine whether it starts with a BOM for UTF-8, UTF-16BE, or UTF_16LE. If so,
returns the scheme corresponding to the BOM; returns the value of Default
otherwise.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Convert (Item          : UTF_String;
                  Input_Scheme  : Encoding_Scheme;
                  Output_Scheme : Encoding_Scheme;
                  Output_BOM    : Boolean := False) @key[return] UTF_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the value of Item
(originally encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by
Input_Scheme) encoded in one of these three schemes as
specified by Output_Scheme.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Convert (Item          : UTF_String;
                  Input_Scheme  : Encoding_Scheme;
                  Output_BOM    : Boolean := False)
   @key[return] UTF_16_Wide_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the value of Item
(originally encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by
Input_Scheme) encoded in UTF-16.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Convert (Item          : UTF_8_String;
                  Output_BOM    : Boolean := False)
   @key[return] UTF_16_Wide_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the value of Item
(originally encoded in UTF-8) encoded in UTF-16.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Convert (Item          : UTF_16_Wide_String;
                  Output_Scheme : Encoding_Scheme;
                  Output_BOM    : Boolean := False) @key[return] UTF_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the value of Item
(originally encoded in UTF-16) encoded in UTF-8,
UTF-16LE, or UTF-16BE as specified by Output_Scheme.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Convert (Item          : UTF_16_Wide_String;
                  Output_BOM    : Boolean := False) @key[return] UTF_8_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the value of Item
(originally encoded in UTF-16) encoded in UTF-8.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Encode (Item          : String;
                 Output_Scheme : Encoding_Scheme;
                 Output_BOM    : Boolean  := False) @key[return] UTF_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the value of Item
encoded in UTF-8, UTF-16LE, or UTF-16BE as specified
by Output_Scheme.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Encode (Item       : String;
                 Output_BOM : Boolean  := False) @key[return] UTF_8_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the value of Item
encoded in UTF-8.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Encode (Item       : String;
                 Output_BOM : Boolean  := False) @key[return] UTF_16_Wide_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the value of Item
encoded in UTF_16.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Decode (Item         : UTF_String;
                 Input_Scheme : Encoding_Scheme) @key[return] String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the result of decoding Item,
which is encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by Input_Scheme.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Decode (Item : UTF_8_String) @key[return] String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the result of decoding Item,
which is encoded in UTF-8.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Decode (Item : UTF_16_Wide_String) @key[return] String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the result of decoding Item,
which is encoded in UTF-16.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Encode (Item          : Wide_String;
                 Output_Scheme : Encoding_Scheme;
                 Output_BOM    : Boolean  := False) @key[return] UTF_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the value of Item
encoded in UTF-8, UTF-16LE, or UTF-16BE as specified
by Output_Scheme.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Encode (Item       : Wide_String;
                 Output_BOM : Boolean  := False) @key[return] UTF_8_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the value of Item
encoded in UTF-8.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Encode (Item       : Wide_String;
                 Output_BOM : Boolean  := False) @key[return] UTF_16_Wide_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the value of Item
encoded in UTF_16.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Decode (Item         : UTF_String;
                 Input_Scheme : Encoding_Scheme) @key[return] Wide_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the result of decoding Item,
which is encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by Input_Scheme.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Decode (Item : UTF_8_String) @key[return] Wide_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the result of decoding Item, which is encoded in UTF-8,
and returns the corresponding Wide_String value.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Decode (Item : UTF_16_Wide_String) @key[return] Wide_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the result of decoding Item,
which is encoded in UTF-16.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Encode (Item          : Wide_Wide_String;
                 Output_Scheme : Encoding_Scheme;
                 Output_BOM    : Boolean  := False) @key[return] UTF_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the value of Item
encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by Output_Scheme.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Encode (Item       : Wide_Wide_String;
                 Output_BOM : Boolean  := False) @key[return] UTF_8_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the value of Item
encoded in UTF-8.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Encode (Item       : Wide_Wide_String;
                 Output_BOM : Boolean  := False) @key[return] UTF_16_Wide_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the value of Item
encoded in UTF_16.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Decode (Item         : UTF_String;
                 Input_Scheme : Encoding_Scheme) @key[return] Wide_Wide_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the result of decoding Item,
which is encoded in UTF-8, UTF-16LE, or UTF-16BE as specified by Input_Scheme.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Decode (Item : UTF_8_String) @key[return] Wide_Wide_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the result of decoding Item, which is encoded in UTF-8,
and returns the corresponding Wide_Wide_String value.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key[function] Decode (Item : UTF_16_Wide_String) @key[return] Wide_Wide_String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[Returns the result of decoding Item,
which is encoded in UTF-16.]}

@end{DescribeCode}

@end{StaticSem}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Text=[If an implementation supports other encoding
schemes, another similar child of Ada.Strings should be defined.]}
@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[If an implementation supports other string encoding
schemes, a child of Ada.Strings similar to UTF_Encoding should be defined.]}]}
@end{ImplAdvice}

@begin{Notes}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
@ChgAdded{Version=[3],Text=[A BOM (Byte-Order Mark, code position 16#FEFF#) can
be included in a file or other entity to indicate the encoding; it is skipped
when decoding. Typically, only the first line of a file or other entity contains
a BOM. When decoding, the Encoding function can be called on the first line to
determine the encoding; this encoding will then be used in subsequent calls to
Decode to convert all of the lines to an internal format.]}
@end{Notes}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0137-2]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The packages Strings.UTF_Encoding, Strings.UTF_Encoding.Conversions,
  Strings.UTF_Encoding.Strings, Strings.UTF_Encoding.Wide_Strings,
  and Strings.UTF_Encoding.Wide_Wide_Strings are new.]}
@end{Extend2005}


