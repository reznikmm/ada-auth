@comment{ $Source: e:\\cvsroot/ARM/Source/pre_strings.mss,v $ }
@comment{ $Revision: 1.3 $ $Date: 2000/04/15 21:58:29 $ $Author: Randy $ }
@Part(predefstrings, Root="ada.mss")
@SetPageHeadingsNoPage{$Date: 2000/04/15 21:58:29 $}

@LabeledAppendixSection{String Handling}

@begin{Intro}
This clause presents the specifications of the package Strings and
several child packages, which provide facilities for dealing with
 string data.  Fixed-length,
bounded-length, and unbounded-length strings are supported, for both
String and Wide_String.
The string-handling subprograms include searches for pattern strings
and for characters in program-specified  sets,
translation (via a character-to-character mapping), and transformation
(replacing, inserting, overwriting, and deleting of substrings).
@end{Intro}

@begin{Extend83}
This clause is new to Ada 9X.
@end{Extend83}

@LabeledAppendixSubSection{The Package Strings}
@begin{Intro}
The package Strings provides declarations
common to the string handling packages.
@end{Intro}

@begin{StaticSem}
The library package Strings has the following declaration:
@begin{example}
@ChildUnit{Parent=[Ada],Child=[Strings],Expanded=[Ada.Strings]}
@key[package] Ada.Strings @key[is]
   @key[pragma] Pure(Strings);

   Space      : @key[constant] Character      := ' ';
   Wide_Space : @key[constant] Wide_Character := ' ';

   Length_Error, Pattern_Error, Index_Error, Translation_Error : @key[exception];

@LangDefType{Package=[Ada.Strings],Type=[Alignment]}
   @key[type] Alignment  @key[is] (Left, Right, Center);
@LangDefType{Package=[Ada.Strings],Type=[Truncation]}
   @key[type] Truncation @key[is] (Left, Right, Error);
@LangDefType{Package=[Ada.Strings],Type=[Membership]}
   @key[type] Membership @key[is] (Inside, Outside);
@LangDefType{Package=[Ada.Strings],Type=[Direction]}
   @key[type] Direction  @key[is] (Forward, Backward);
@LangDefType{Package=[Ada.Strings],Type=[Trim_End]}
   @key[type] Trim_End   @key[is] (Left, Right, Both);
@key[end] Ada.Strings;
@end{example}
@end{StaticSem}

@LabeledAppendixSubSection{The Package Strings.Maps}
@begin{Intro}
The package Strings.Maps defines the types, operations, and other
entities needed for character sets and character-to-character mappings.
@end{Intro}

@begin{StaticSem}
The library package Strings.Maps has the following declaration:
@begin{example}
@ChildUnit{Parent=[Ada.Strings],Child=[Maps],Expanded=[Ada.Strings.Maps]}
@key[package] Ada.Strings.Maps @key[is]
   @key[pragma] Preelaborate(Maps);

   --@i{ Representation for a set of character values:}
@LangDefType{Package=[Ada.Strings.Maps],Type=[Character_Set]}
   @key[type] Character_Set @key[is] @key[private];

   Null_Set : @key[constant] Character_Set;

   @key[type] Character_Range @key[is]
     @Key[record]
        Low  : Character;
        High : Character;
     @key[end] @key[record];
   -- @i[Represents Character range Low..High]

   @key[type] Character_Ranges @key[is] @key[array] (Positive @key[range] <>) @key[of] Character_Range;

   @key[function] To_Set    (Ranges : @key[in] Character_Ranges) @key[return] Character_Set;

   @key[function] To_Set    (Span   : @key[in] Character_Range)  @key[return] Character_Set;

   @key[function] To_Ranges (Set    : @key[in] Character_Set)    @key[return] Character_Ranges;

   @key[function] "="   (Left, Right : @key[in] Character_Set) @key[return] Boolean;

   @key[function] "@key[not]" (Right : @key[in] Character_Set)       @key[return] Character_Set;
   @key[function] "@key[and]" (Left, Right : @key[in] Character_Set) @key[return] Character_Set;
   @key[function] "@key[or]"  (Left, Right : @key[in] Character_Set) @key[return] Character_Set;
   @key[function] "@key[xor]" (Left, Right : @key[in] Character_Set) @key[return] Character_Set;
   @key[function] "@en"   (Left, Right : @key[in] Character_Set) @key[return] Character_Set;

   @key[function] Is_In (Element : @key[in] Character;
                   Set     : @key[in] Character_Set)
      @key[return] Boolean;

   @key[function] Is_Subset (Elements : @key[in] Character_Set;
                       Set      : @key[in] Character_Set)
      @key[return] Boolean;

   @key[function] "<=" (Left  : @key[in] Character_Set;
                  Right : @key[in] Character_Set)
      @key[return] Boolean @key[renames] Is_Subset;


   --@i{ Alternative representation for a set of character values:}
   @key[subtype] Character_Sequence @key[is] String;

   @key[function] To_Set (Sequence  : @key[in] Character_Sequence) @key[return] Character_Set;

   @key[function] To_Set (Singleton : @key[in] Character)          @key[return] Character_Set;

   @key[function] To_Sequence (Set  : @key[in] Character_Set)      @key[return] Character_Sequence;


   --@i{ Representation for a character to character mapping:}
   @key[type] Character_Mapping @key[is] @key[private];

   @key[function] Value (Map     : @key[in] Character_Mapping;
                   Element : @key[in] Character)
      @key[return] Character;

   Identity : @key[constant] Character_Mapping;

   @key[function] To_Mapping (From, To : @key[in] Character_Sequence) @key[return] Character_Mapping;

   @key[function] To_Domain (Map : @key[in] Character_Mapping) @key[return] Character_Sequence;
   @key[function] To_Range  (Map : @key[in] Character_Mapping) @key[return] Character_Sequence;

   @key{type} Character_Mapping_Function @key{is}
      @key{access} @key{function} (From : @key{in} Character) @key{return} Character;

@key[private]
   ... -- @i{not specified by the language}
@key[end] Ada.Strings.Maps;
@end{example}

An object of type Character_Set represents a set of
characters.

Null_Set  represents the set containing no characters.

An object Obj of type Character_Range represents the set of characters
in the range Obj.Low .. Obj.High.

An object Obj of type Character_Ranges represents the
union of the sets corresponding to Obj(I) for I in Obj'Range.
@begin{DescribeCode}
@begin{CodeExample}
@key[function] To_Set (Ranges : @key[in] Character_Ranges) @key[return] Character_Set;
@end{CodeExample}


If Ranges'Length=0 then Null_Set is returned;
otherwise the returned value represents the set corresponding to Ranges.
@begin{CodeExample}
@key[function] To_Set (Span : @key[in] Character_Range) @key[return] Character_Set;
@end{CodeExample}

The returned value represents the set containing each character in Span.
@begin{CodeExample}
@key[function] To_Ranges (Set : @key[in] Character_Set) @key[return] Character_Ranges;
@end{CodeExample}

If Set = Null_Set then an empty Character_Ranges array is returned;
otherwise
the shortest array of contiguous ranges of Character
values in Set, in increasing order of Low, is returned.
@begin{CodeExample}
@key[function] "=" (Left, Right : @key[in] Character_Set) @key[return] Boolean;
@end{CodeExample}

The function "=" returns True if Left and Right represent identical sets,
and False otherwise.
@end{DescribeCode}

Each of the logical operators "@key[not]", "@key[and]", "@key[or]", and
"@key[xor]" returns
a Character_Set value that represents the set obtained by applying
the corresponding operation to the set(s) represented by the parameter(s)
of the operator.
"@en"(Left, Right) is equivalent to "and"(Left, "not"(Right)).
@reason{The set minus operator is provided for efficiency.}
@begin{DescribeCode}
@begin{CodeExample}
@key[function] Is_In (Element : @key[in] Character;
                Set     : @key[in] Character_Set);
   @key[return] Boolean;
@end{CodeExample}

Is_In returns True if Element is in Set, and False otherwise.
@begin{CodeExample}
@key[function] Is_Subset (Elements : @key[in] Character_Set;
                    Set      : @key[in] Character_Set)
   @key[return] Boolean;
@end{CodeExample}

Is_Subset returns True if
Elements is a subset of Set, and False otherwise.
@begin{CodeExample}
@key[subtype] Character_Sequence @key[is] String;
@end{CodeExample}

The Character_Sequence subtype is used to portray a set of character
values and also to identify the domain and range of a character
mapping.
@begin{reason}
Although a named subtype is redundant @em the predefined type String
could have been used for the parameter to To_Set and To_Mapping
below @em the use of a differently named subtype identifies the intended
purpose of the parameter.
@end{reason}
@begin{CodeExample}
@key[function] To_Set (Sequence  : @key[in] Character_Sequence) @key[return] Character_Set;

@key[function] To_Set (Singleton : @key[in] Character)          @key[return] Character_Set;
@end{CodeExample}

Sequence portrays the set of character values that it explicitly
contains (ignoring duplicates).
Singleton portrays the set comprising a single Character.
Each of the To_Set functions
returns a Character_Set value that represents
the set portrayed by Sequence or Singleton.
@begin{CodeExample}
@key[function] To_Sequence (Set : @key[in] Character_Set) @key[return] Character_Sequence;
@end{CodeExample}

The function To_Sequence returns a Character_Sequence value
containing each of the characters in the set represented by Set, in
ascending order with no duplicates.
@begin{CodeExample}
@key[type] Character_Mapping @key[is] @key[private];
@end{CodeExample}

An object  of type Character_Mapping represents a
Character-to-Character mapping.
@begin{CodeExample}
@key[function] Value (Map     : @key[in] Character_Mapping;
                Element : @key[in] Character)
   @key[return] Character;
@end{CodeExample}

The function Value returns the Character value to which Element maps
with respect to the mapping represented by Map.
@end{DescribeCode}

@Defn2{Term=match, Sec=(a character to a pattern character)}
A character C @i{matches} a pattern character P
with respect to a given Character_Mapping value Map if
Value(Map, C) = P.
@Defn2{Term=match, Sec=(a string to a pattern string)}
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
@begin{CodeExample}
Identity : @key[constant] Character_Mapping;
@end{CodeExample}

   Identity maps each Character to itself.
@begin{CodeExample}
@key[function] To_Mapping (From, To : @key[in] Character_Sequence) @key[return] Character_Mapping;
@end{CodeExample}

To_Mapping produces a Character_Mapping such that
each element of From maps to the corresponding element of To,
 and each other character maps to itself.
    If From'Length /= To'Length, or
     if some character is repeated in From, then Translation_Error
     is propagated.
@begin{CodeExample}
@key[function] To_Domain (Map : @key[in] Character_Mapping) @key[return] Character_Sequence;
@end{CodeExample}

To_Domain returns the shortest Character_Sequence value D such that
each character not in D maps to itself, and such that
the characters in D are in ascending order.
The lower bound of D is 1.
@begin{CodeExample}
@key[function] To_Range  (Map : @key[in] Character_Mapping) @key[return] Character_Sequence;
@end{CodeExample}

To_Range returns the Character_Sequence value R, with lower bound 1

and upper bound Map'Length,

such that if
D = To_Domain(Map) then
D(I) maps to R(I) for each I in D'Range.
@end{DescribeCode}

An object F of type Character_Mapping_Function maps a Character
value C to the Character value F.@key{all}(C), which is said to
@i{match} C with respect to mapping function F.
@Defn2[term=<match>,sec=<a character to a pattern character, with
respect to a character mapping function>]
@end{StaticSem}

@begin{NotesNotes}
Character_Mapping and Character_Mapping_Function
are used both for character equivalence
mappings in the search subprograms (such as for case insensitivity) and
as transformational mappings in the Translate subprograms.

To_Domain(Identity) and To_Range(Identity) each returns the null string.
@begin{Reason}
Package Strings.Maps is not pure, since it declares an
access-to-subprogram type.
@end{Reason}
@end{NotesNotes}

@begin{Examples}
To_Mapping("ABCD", "ZZAB") returns a Character_Mapping that maps 'A'
and 'B' to 'Z', 'C' to 'A', 'D' to 'B', and each other Character to
itself.
@end{Examples}

@LabeledAppendixSubSection{Fixed-Length String Handling}
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
@Discussion{Most operations that yields a String are provided both as a
function and as a procedure.  The functional form is possibly a more aesthetic
style but may introduce overhead due to extra copying or dynamic memory
usage in some implementations.  Thus a procedural form, with an @key[in]
@key[out] parameter so that all copying is done `in place', is also
supplied.}

The basic model embodied in the package is that a fixed-length string
comprises significant characters and possibly padding
(with space characters)
on either or both
ends.  When a shorter string is copied to a longer string, padding
is inserted, and when a longer string is copied to a shorter one,
padding is stripped.  The Move procedure in Strings.Fixed, which takes a
String as an @key[out] parameter, allows the programmer to control these
effects.  Similar control is provided by the string transformation
procedures.
@end{Intro}

@begin{StaticSem}
The library package Strings.Fixed has the following declaration:
@begin{example}
@key[with] Ada.Strings.Maps;
@ChildUnit{Parent=[Ada.Strings],Child=[Fixed],Expanded=[Ada.Strings.Fixed]}
@key[package] Ada.Strings.Fixed @key[is]
   @key[pragma] Preelaborate(Fixed);


--@i{ "Copy" procedure for strings of possibly different lengths}

   @key[procedure] Move (Source  : @key[in]  String;
                   Target  : @key[out] String;
                   Drop    : @key[in]  Truncation := Error;
                   Justify : @key[in]  Alignment  := Left;
                   Pad     : @key[in]  Character  := Space);


--@i{ Search subprograms}

   @key[function] Index (Source   : @key[in] String;
                   Pattern  : @key[in] String;
                   Going    : @key[in] Direction := Forward;
                   Mapping  : @key[in] Maps.Character_Mapping
                                := Maps.Identity)
      @key[return] Natural;

   @key[function] Index (Source   : @key[in] String;
                   Pattern  : @key[in] String;
                   Going    : @key[in] Direction := Forward;
                   Mapping  : @key[in] Maps.Character_Mapping_Function)
      @key[return] Natural;

   @key[function] Index (Source : @key[in] String;
                   Set    : @key[in] Maps.Character_Set;
                   Test   : @key[in] Membership := Inside;
                   Going  : @key[in] Direction  := Forward)
      @key[return] Natural;


   @key[function] Index_Non_Blank (Source : @key[in] String;
                             Going  : @key[in] Direction := Forward)
      @key[return] Natural;


   @key[function] Count (Source   : @key[in] String;
                   Pattern  : @key[in] String;
                   Mapping  : @key[in] Maps.Character_Mapping
                                 := Maps.Identity)
      @key[return] Natural;

   @key[function] Count (Source   : @key[in] String;
                   Pattern  : @key[in] String;
                   Mapping  : @key[in] Maps.Character_Mapping_Function)
      @key[return] Natural;

   @key[function] Count (Source   : @key[in] String;
                   Set      : @key[in] Maps.Character_Set)
      @key[return] Natural;


   @key[procedure] Find_Token (Source : @key[in] String;
                         Set    : @key[in] Maps.Character_Set;
                         Test   : @key[in] Membership;
                         First  : @key[out] Positive;
                         Last   : @key[out] Natural);


--@i{ String translation subprograms}

   @key[function] Translate (Source  : @key[in] String;
                       Mapping : @key[in] Maps.Character_Mapping)
      @key[return] String;

   @key[procedure] Translate (Source  : @key[in] @key[out] String;
                        Mapping : @key[in] Maps.Character_Mapping);


   @key[function] Translate (Source  : @key[in] String;
                       Mapping : @key[in] Maps.Character_Mapping_Function)
      @key[return] String;

   @key[procedure] Translate (Source  : @key[in] @key[out] String;
                        Mapping : @key[in] Maps.Character_Mapping_Function);

--@i{ String transformation subprograms}

   @key[function] Replace_Slice (Source   : @key[in] String;
                           Low      : @key[in] Positive;
                           High     : @key[in] Natural;
                           By       : @key[in] String)
      @key[return] String;

   @key[procedure] Replace_Slice (Source   : @key[in] @key[out] String;
                            Low      : @key[in] Positive;
                            High     : @key[in] Natural;
                            By       : @key[in] String;
                            Drop     : @key[in] Truncation := Error;
                            Justify  : @key[in] Alignment  := Left;
                            Pad      : @key[in] Character  := Space);


   @key[function] Insert (Source   : @key[in] String;
                    Before   : @key[in] Positive;
                    New_Item : @key[in] String)
      @key[return] String;

   @key[procedure] Insert (Source   : @key[in] @key[out] String;
                     Before   : @key[in] Positive;
                     New_Item : @key[in] String;
                     Drop     : @key[in] Truncation := Error);


   @key[function] Overwrite (Source   : @key[in] String;
                       Position : @key[in] Positive;
                       New_Item : @key[in] String)
      @key[return] String;

   @key[procedure] Overwrite (Source   : @key[in] @key[out] String;
                        Position : @key[in] Positive;
                        New_Item : @key[in] String;
                        Drop     : @key[in] Truncation := Right);


   @key[function] Delete (Source  : @key[in] String;
                    From    : @key[in] Positive;
                    Through : @key[in] Natural)
      @key[return] String;

   @key[procedure] Delete (Source  : @key[in] @key[out] String;
                     From    : @key[in] Positive;
                     Through : @key[in] Natural;
                     Justify : @key[in] Alignment := Left;
                     Pad     : @key[in] Character := Space);

 --@i{String selector subprograms}
   @key[function] Trim (Source : @key[in] String;
                  Side   : @key[in] Trim_End)
      @key[return] String;

   @key[procedure] Trim (Source  : @key[in] @key[out] String;
                   Side    : @key[in] Trim_End;
                   Justify : @key[in] Alignment := Left;
                   Pad     : @key[in] Character := Space);

   @key[function] Trim (Source : @key[in] String;
                  Left   : @key[in] Maps.Character_Set;
                  Right  : @key[in] Maps.Character_Set)
      @key[return] String;

   @key[procedure] Trim (Source  : @key[in] @key[out] String;
                   Left    : @key[in] Maps.Character_Set;
                   Right   : @key[in] Maps.Character_Set;
                   Justify : @key[in] Alignment := Strings.Left;
                   Pad     : @key[in] Character := Space);


   @key[function] Head (Source : @key[in] String;
                  Count  : @key[in] Natural;
                  Pad    : @key[in] Character := Space)
      @key[return] String;

   @key[procedure] Head (Source  : @key[in] @key[out] String;
                   Count   : @key[in] Natural;
                   Justify : @key[in] Alignment := Left;
                   Pad     : @key[in] Character := Space);

   @key[function] Tail (Source : @key[in] String;
                  Count  : @key[in] Natural;
                  Pad    : @key[in] Character := Space)
      @key[return] String;

   @key[procedure] Tail (Source  : @key[in] @key[out] String;
                   Count   : @key[in] Natural;
                   Justify : @key[in] Alignment := Left;
                   Pad     : @key[in] Character := Space);

--@i{String constructor functions}

   @key[function] "*" (Left  : @key[in] Natural;
                 Right : @key[in] Character) @key[return] String;

   @key[function] "*" (Left  : @key[in] Natural;
                 Right : @key[in] String) @key[return] String;

@key[end] Ada.Strings.Fixed;
@end{example}

The effects of the above subprograms are as follows.
@begin{DescribeCode}
@begin{CodeExample}
@key[procedure] Move (Source  : @key[in]  String;
                Target  : @key[out] String;
                Drop    : @key[in]  Truncation := Error;
                Justify : @key[in]  Alignment  := Left;
                Pad     : @key[in]  Character  := Space);
@end{CodeExample}

The Move procedure copies characters from Source to Target.
If Source has the same length as Target, then the effect is
to assign Source to Target.
If Source is shorter than Target then:
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

If Drop=Error, then the effect depends on the value of the Justify
parameter and also on whether any characters in Source other than
Pad would fail to be copied:
@begin{itemize}
If Justify=Left, and if each of the rightmost Source'Length-Target'Length
characters in Source is Pad, then the leftmost Target'Length characters
of Source are copied to Target.

If Justify=Right, and if each of the leftmost Source'Length-Target'Length
characters in Source is Pad, then the rightmost Target'Length characters
of Source are copied to Target.

Otherwise, Length_Error is propagated.
@end{itemize}
@end{itemize}
@ramification{The Move procedure will work even if Source and Target
overlap.}
@reason{The order of parameters (Source before Target) corresponds to
the order in COBOL's MOVE verb.}

@begin{CodeExample}
@key[function] Index (Source   : @key[in] String;
                Pattern  : @key[in] String;
                Going    : @key[in] Direction := Forward;
                Mapping  : @key[in] Maps.Character_Mapping
                              := Maps.Identity)
   @key[return] Natural;

@key[function] Index (Source   : @key[in] String;
                Pattern  : @key[in] String;
                Going    : @key[in] Direction := Forward;
                Mapping  : @key[in] Maps.Character_Mapping_Function)
   @key[return] Natural;
@end{CodeExample}

Each
 Index function searches for a slice of Source, with length
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
  If Pattern is the null string then Pattern_Error is propagated.
@begin{discussion}
There is no default value for the Mapping parameter that is
a Character_Mapping_Function; if there were,
a call would be ambiguous since there is also a default for
the Mapping parameter that is  a Character_Mapping.
@end{discussion}

@begin{CodeExample}
@key[function] Index (Source : @key[in] String;
                Set    : @key[in] Maps.Character_Set;
                Test   : @key[in] Membership := Inside;
                Going  : @key[in] Direction  := Forward)
   @key[return] Natural;
@end{CodeExample}

Index searches for the first or last occurrence of any of a set of
 characters (when Test=Inside),
or any of the complement of a set of characters (when Test=Outside).
It returns the smallest index I (if Going=Forward) or the largest index I
(if Going=Backward) such that
 Source(I) satisfies the Test  condition with respect to Set;
it returns 0 if there is no such Character in Source.

@begin{CodeExample}
@key[function] Index_Non_Blank (Source : @key[in] String;
                          Going  : @key[in] Direction := Forward)
   @key[return] Natural;
@end{CodeExample}

Returns Index(Source, Maps.To_Set(Space), Outside, Going)

@begin{CodeExample}
@key[function] Count (Source   : @key[in] String;
                Pattern  : @key[in] String;
                Mapping  : @key[in] Maps.Character_Mapping
                             := Maps.Identity)
   @key[return] Natural;

@key[function] Count (Source   : @key[in] String;
                Pattern  : @key[in] String;
                Mapping  : @key[in] Maps.Character_Mapping_Function)
   @key[return] Natural;
@end{CodeExample}

Returns the maximum number of nonoverlapping slices of Source that
match Pattern with respect to Mapping.
If Pattern is the null string then Pattern_Error is propagated.
@begin{reason}
We say `maximum number' because it is possible to slice a source
string in different ways yielding different numbers of matches.  For
example if Source is "ABABABA" and Pattern is "ABA", then Count yields
2, although there is a partitioning of Source that yields just 1 match,
for the middle slice.  Saying `maximum number' is equivalent to saying
that the pattern match starts either at the low index or the high index
position.
@end{reason}

@begin{CodeExample}
@key[function] Count (Source   : @key[in] String;
                Set      : @key[in] Maps.Character_Set)
   @key[return] Natural;
@end{CodeExample}

Returns the number of occurrences in Source of characters that
are in Set.

@begin{CodeExample}
@key[procedure] Find_Token (Source : @key[in] String;
                      Set    : @key[in] Maps.Character_Set;
                      Test   : @key[in] Membership;
                      First  : @key[out] Positive;
                      Last   : @key[out] Natural);
@end{CodeExample}

Find_Token returns in First and Last the indices of the beginning and
 end of the first slice of Source all of whose elements
 satisfy the Test condition, and such that the elements
 (if any) immediately before and after the slice do not
 satisfy the Test condition.
If no such slice exists, then the value returned for Last is zero, and
the value returned for First is Source'First.

@begin{CodeExample}
@key[function] Translate (Source  : @key[in] String;
                    Mapping : @key[in] Maps.Character_Mapping)
   @key[return] String;

@key[function] Translate (Source  : @key[in] String;
                    Mapping : @key[in] Maps.Character_Mapping_Function)
   @key[return] String;
@end{CodeExample}

Returns the string S whose length is Source'Length
 and such
that S(I) is the character to which Mapping maps the corresponding
element of Source, for I in 1..Source'Length.


@begin{CodeExample}
@key[procedure] Translate (Source  : @key[in] @key[out] String;
                     Mapping : @key[in] Maps.Character_Mapping);

@key[procedure] Translate (Source  : @key[in] @key[out] String;
                     Mapping : @key[in] Maps.Character_Mapping_Function);
@end{CodeExample}

Equivalent to Source := Translate(Source, Mapping).

@begin{CodeExample}
@key[function] Replace_Slice (Source   : @key[in] String;
                        Low      : @key[in] Positive;
                        High     : @key[in] Natural;
                        By       : @key[in] String)
   @key[return] String;
@end{CodeExample}

If Low > Source'Last+1, or  High < Source'First@en@;1,
 then Index_Error is propagated.
Otherwise, if High >= Low then the returned string
 comprises
 Source(Source'First..Low@en@;1) & By & Source(High+1..Source'Last),
and if High < Low then the returned string is
Insert(Source, Before=>Low, New_Item=>By).

@begin{CodeExample}
@key[procedure] Replace_Slice (Source   : @key[in] @key[out] String;
                         Low      : @key[in] Positive;
                         High     : @key[in] Natural;
                         By       : @key[in] String;
                         Drop     : @key[in] Truncation := Error;
                         Justify  : @key[in] Alignment  := Left;
                         Pad      : @key[in] Character  := Space);
@end{CodeExample}

Equivalent to Move(Replace_Slice(Source, Low, High,
By), Source, Drop, Justify, Pad).

@begin{CodeExample}
@key[function] Insert (Source   : @key[in] String;
                 Before   : @key[in] Positive;
                 New_Item : @key[in] String)
   @key[return] String;
@end{CodeExample}

Propagates Index_Error if Before is not in Source'First .. Source'Last+1;
otherwise
returns Source(Source'First..Before@en@;1) & New_Item &
Source(Before..Source'Last), but with lower bound 1.


@begin{CodeExample}
@key[procedure] Insert (Source   : @key[in] @key[out] String;
                  Before   : @key[in] Positive;
                  New_Item : @key[in] String;
                  Drop     : @key[in] Truncation := Error);
@end{CodeExample}

Equivalent to Move(Insert(Source, Before, New_Item), Source, Drop).

@begin{CodeExample}
@key[function] Overwrite (Source   : @key[in] String;
                    Position : @key[in] Positive;
                    New_Item : @key[in] String)
   @key[return] String;
@end{CodeExample}

Propagates Index_Error if Position is not in Source'First .. Source'Last+1;
otherwise
returns the string obtained from Source by consecutively replacing
characters starting at Position with corresponding characters from
New_Item.  If the end of Source is reached before the characters in
New_Item are exhausted, the remaining characters from New_Item are
appended to the string.

@begin{CodeExample}
@key[procedure] Overwrite (Source   : @key[in] @key[out] String;
                     Position : @key[in] Positive;
                     New_Item : @key[in] String;
                     Drop     : @key[in] Truncation := Right);
@end{CodeExample}

Equivalent to Move(Overwrite(Source, Position,
New_Item), Source, Drop).

@begin{CodeExample}
@key[function] Delete (Source  : @key[in] String;
                 From    : @key[in] Positive;
                 Through : @key[in] Natural)
   @key[return] String;
@end{CodeExample}

If From <= Through, the returned string is Replace_Slice(Source, From,
Through, ""), otherwise it is Source.

@begin{CodeExample}
@key[procedure] Delete (Source  : @key[in] @key[out] String;
                  From    : @key[in] Positive;
                  Through : @key[in] Natural;
                  Justify : @key[in] Alignment := Left;
                  Pad     : @key[in] Character := Space);
@end{CodeExample}

Equivalent to Move(Delete(Source, From, Through),
Source, Justify => Justify, Pad => Pad).

@begin{CodeExample}
@key[function] Trim (Source : @key[in] String;
               Side   : @key[in] Trim_End)
  @key[return] String;
@end{CodeExample}

Returns the string obtained by removing from Source all leading Space
characters (if Side = Left), all trailing Space characters
(if Side = Right), or all leading and trailing Space characters
(if Side = Both).

@begin{CodeExample}
@key[procedure] Trim (Source  : @key[in] @key[out] String;
                Side    : @key[in] Trim_End;
                Justify : @key[in] Alignment := Left;
                Pad     : @key[in] Character := Space);
@end{CodeExample}

Equivalent to Move(Trim(Source, Side), Source, Justify=>Justify, Pad=>Pad).

@begin{CodeExample}
@key[function] Trim (Source : @key[in] String;
               Left   : @key[in] Maps.Character_Set;
               Right  : @key[in] Maps.Character_Set)
   @key[return] String;
@end{CodeExample}

Returns the string obtained by removing from Source all leading characters
in Left and all
trailing characters in Right.

@begin{CodeExample}
@key[procedure] Trim (Source  : @key[in] @key[out] String;
                Left    : @key[in] Maps.Character_Set;
                Right   : @key[in] Maps.Character_Set;
                Justify : @key[in] Alignment := Strings.Left;
                Pad     : @key[in] Character := Space);
@end{CodeExample}

Equivalent to Move(Trim(Source, Left, Right), Source,
Justify => Justify, Pad=>Pad).

@begin{CodeExample}
@key[function] Head (Source : @key[in] String;
               Count  : @key[in] Natural;
               Pad    : @key[in] Character := Space)
   @key[return] String;
@end{CodeExample}

Returns a string of length Count.  If Count <= Source'Length, the string
comprises the first Count characters of Source.  Otherwise its contents
are Source concatenated with Count@en@;Source'Length Pad characters.

@begin{CodeExample}
@key[procedure] Head (Source  : @key[in] @key[out] String;
                Count   : @key[in] Natural;
                Justify : @key[in] Alignment := Left;
                Pad     : @key[in] Character := Space);
@end{CodeExample}

Equivalent to Move(Head(Source, Count, Pad), Source, Drop=>Error,
Justify=>Justify, Pad=>Pad).

@begin{CodeExample}
@key[function] Tail (Source : @key[in] String;
               Count  : @key[in] Natural;
               Pad    : @key[in] Character := Space)
   @key[return] String;
@end{CodeExample}

Returns a string of length Count.  If Count <= Source'Length, the string
comprises the last Count characters of Source.  Otherwise its contents
are Count-Source'Length Pad characters concatenated with Source.

@begin{CodeExample}
@key[procedure] Tail (Source  : @key[in] @key[out] String;
                Count   : @key[in] Natural;
                Justify : @key[in] Alignment := Left;
                Pad     : @key[in] Character := Space);
@end{CodeExample}

Equivalent to Move(Tail(Source, Count, Pad), Source, Drop=>Error,
Justify=>Justify, Pad=>Pad).

@begin{CodeExample}
@key[function] "*" (Left  : @key[in] Natural;
              Right : @key[in] Character) @key[return] String;

@key[function] "*" (Left  : @key[in] Natural;
              Right : @key[in] String) @key[return] String;
@end{CodeExample}

These functions replicate a character or string a specified number of
times.  The first function returns a string whose length is Left and each
of whose elements is Right.  The second function returns a string whose
length is Left*Right'Length and whose value is the null
string if Left = 0 and is
(Left@en@;1)*Right & Right otherwise.
@end{DescribeCode}
@end{StaticSem}

@begin{NotesNotes}
In the Index and Count functions taking Pattern and Mapping parameters,
the actual String parameter passed to Pattern should comprise characters
occurring as target characters of the mapping.  Otherwise the pattern
will not match.

In the Insert subprograms, inserting at the end of a string is obtained
by passing Source'Last+1 as the Before parameter.

@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
If a null Character_Mapping_Function is passed to any of the
string handling subprograms, Constraint_Error is propagated.
@end{NotesNotes}



@LabeledAppendixSubSection{Bounded-Length String Handling}
@begin{Intro}
The language-defined package Strings.Bounded provides a generic package
each of whose instances yields a private type Bounded_String and a
set of operations.  An object of a particular Bounded_String type
represents a String whose low bound is 1 and whose length
can vary conceptually
between 0 and a maximum size established at the
generic instantiation.
The subprograms for fixed-length string handling
are either overloaded directly  for Bounded_String, or are modified as
needed to reflect the variability in length.  Additionally, since the
Bounded_String type is private, appropriate constructor and selector
operations are provided.
@reason{Strings.Bounded declares an inner generic package, versus itself
being directly a generic child of Strings, in order to retain
compatibility with a version of the string-handling packages that
is generic with respect to the character and string types.}
@reason{The bound of a bounded-length string is specified as a parameter
to a generic, versus as the value for a discriminant, because of the
inappropriateness of assignment and equality of discriminated types for
the copying and comparison of bounded strings.}
@end{Intro}

@begin{StaticSem}
The library package Strings.Bounded has the following declaration:
@begin{example}
@key[with] Ada.Strings.Maps;
@ChildUnit{Parent=[Ada.Strings],Child=[Bounded],Expanded=[Ada.Strings.Bounded]}
@key[package] Ada.Strings.Bounded @key[is]
   @key[pragma] Preelaborate(Bounded);


   @key[generic]
      Max   : Positive;    --@i{ Maximum length of a Bounded_String}
   @key[package] Generic_Bounded_Length @key[is]

      Max_Length : @key[constant] Positive := Max;

@LangDefType{Package=[Ada.Strings.Bounded.Generic_Bounded_Length],Type=[Bounded_String]}
      @key[type] Bounded_String @key[is] @key[private];

      Null_Bounded_String : @key[constant] Bounded_String;

      @key[subtype] Length_Range @key[is] Natural @key[range] 0 .. Max_Length;

      @key[function] Length (Source : @key[in] Bounded_String) @key[return] Length_Range;


   --@i{ Conversion, Concatenation, and Selection functions}

      @key[function] To_Bounded_String (Source : @key[in] String;
                                  Drop   : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[function] To_String (Source : @key[in] Bounded_String) @key[return] String;


      @key[function] Append (Left, Right : @key[in] Bounded_String;
                       Drop        : @key[in] Truncation  := Error)
         @key[return] Bounded_String;

      @key[function] Append (Left  : @key[in] Bounded_String;
                       Right : @key[in] String;
                       Drop  : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[function] Append (Left  : @key[in] String;
                       Right : @key[in] Bounded_String;
                       Drop  : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[function] Append (Left  : @key[in] Bounded_String;
                       Right : @key[in] Character;
                       Drop  : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[function] Append (Left  : @key[in] Character;
                       Right : @key[in] Bounded_String;
                       Drop  : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[procedure] Append (Source   : @key[in out] Bounded_String;
                        New_Item : @key[in] Bounded_String;
                        Drop     : @key[in] Truncation  := Error);

      @key[procedure] Append (Source   : @key[in out] Bounded_String;
                        New_Item : @key[in] String;
                        Drop     : @key[in] Truncation  := Error);

      @key[procedure] Append (Source   : @key[in out] Bounded_String;
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


      @key[function] Element (Source : @key[in] Bounded_String;
                        Index  : @key[in] Positive)
         @key[return] Character;

      @key[procedure] Replace_Element (Source : @key[in] @key[out] Bounded_String;
                                 Index  : @key[in] Positive;
                                 By     : @key[in] Character);


      @key[function] Slice (Source : @key[in] Bounded_String;
                      Low    : @key[in] Positive;
                      High   : @key[in] Natural)
         @key[return] String;

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

   --@i{ Search functions}

      @key[function] Index (Source   : @key[in] Bounded_String;
                      Pattern  : @key[in] String;
                      Going    : @key[in] Direction := Forward;
                      Mapping  : @key[in] Maps.Character_Mapping
                                 := Maps.Identity)
         @key[return] Natural;

      @key[function] Index (Source   : @key[in] Bounded_String;
                      Pattern  : @key[in] String;
                      Going    : @key[in] Direction := Forward;
                      Mapping  : @key[in] Maps.Character_Mapping_Function)
         @key[return] Natural;

      @key[function] Index (Source : @key[in] Bounded_String;
                      Set    : @key[in] Maps.Character_Set;
                      Test   : @key[in] Membership := Inside;
                      Going  : @key[in] Direction  := Forward)
         @key[return] Natural;

      @key[function] Index_Non_Blank (Source : @key[in] Bounded_String;
                                Going  : @key[in] Direction := Forward)
         @key[return] Natural;


      @key[function] Count (Source   : @key[in] Bounded_String;
                      Pattern  : @key[in] String;
                      Mapping  : @key[in] Maps.Character_Mapping
                                   := Maps.Identity)
         @key[return] Natural;

      @key[function] Count (Source   : @key[in] Bounded_String;
                      Pattern  : @key[in] String;
                      Mapping  : @key[in] Maps.Character_Mapping_Function)
         @key[return] Natural;

      @key[function] Count (Source   : @key[in] Bounded_String;
                      Set      : @key[in] Maps.Character_Set)
         @key[return] Natural;


      @key[procedure] Find_Token (Source : @key[in] Bounded_String;
                            Set    : @key[in] Maps.Character_Set;
                            Test   : @key[in] Membership;
                            First  : @key[out] Positive;
                            Last   : @key[out] Natural);

   --@i{ String translation subprograms}

      @key[function] Translate (Source  : @key[in] Bounded_String;
                          Mapping : @key[in] Maps.Character_Mapping)
         @key[return] Bounded_String;

      @key[procedure] Translate (Source  : @key[in] @key[out] Bounded_String;
                           Mapping : @key[in] Maps.Character_Mapping);


      @key[function] Translate (Source  : @key[in] Bounded_String;
                          Mapping : @key[in] Maps.Character_Mapping_Function)
         @key[return] Bounded_String;

      @key[procedure] Translate (Source  : @key[in] @key[out] Bounded_String;
                           Mapping : @key[in] Maps.Character_Mapping_Function);

   --@i{ String transformation subprograms}

      @key[function] Replace_Slice (Source   : @key[in] Bounded_String;
                              Low      : @key[in] Positive;
                              High     : @key[in] Natural;
                              By       : @key[in] String;
                              Drop     : @key[in] Truncation := Error)
         @key[return] Bounded_String;


      @key[procedure] Replace_Slice (Source   : @key[in] @key[out] Bounded_String;
                               Low      : @key[in] Positive;
                               High     : @key[in] Natural;
                               By       : @key[in] String;
                               Drop     : @key[in] Truncation := Error);


      @key[function] Insert (Source   : @key[in] Bounded_String;
                       Before   : @key[in] Positive;
                       New_Item : @key[in] String;
                       Drop     : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[procedure] Insert (Source   : @key[in] @key[out] Bounded_String;
                        Before   : @key[in] Positive;
                        New_Item : @key[in] String;
                        Drop     : @key[in] Truncation := Error);


      @key[function] Overwrite (Source    : @key[in] Bounded_String;
                          Position  : @key[in] Positive;
                          New_Item  : @key[in] String;
                          Drop      : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[procedure] Overwrite (Source    : @key[in] @key[out] Bounded_String;
                           Position  : @key[in] Positive;
                           New_Item  : @key[in] String;
                           Drop      : @key[in] Truncation := Error);

      @key[function] Delete (Source  : @key[in] Bounded_String;
                       From    : @key[in] Positive;
                       Through : @key[in] Natural)
         @key[return] Bounded_String;

      @key[procedure] Delete (Source  : @key[in] @key[out] Bounded_String;
                        From    : @key[in] Positive;
                        Through : @key[in] Natural);

--@i{String selector subprograms}

      @key[function] Trim (Source : @key[in] Bounded_String;
                     Side   : @key[in] Trim_End)
         @key[return] Bounded_String;
      @key[procedure] Trim (Source : @key[in] @key[out] Bounded_String;
                      Side   : @key[in] Trim_End);

      @key[function] Trim (Source : @key[in] Bounded_String;
                     Left   : @key[in] Maps.Character_Set;
                     Right  : @key[in] Maps.Character_Set)
         @key[return] Bounded_String;

      @key[procedure] Trim (Source : @key[in] @key[out] Bounded_String;
                      Left   : @key[in] Maps.Character_Set;
                      Right  : @key[in] Maps.Character_Set);

      @key[function] Head (Source : @key[in] Bounded_String;
                     Count  : @key[in] Natural;
                     Pad    : @key[in] Character  := Space;
                     Drop   : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[procedure] Head (Source : @key[in] @key[out] Bounded_String;
                      Count  : @key[in] Natural;
                      Pad    : @key[in] Character  := Space;
                      Drop   : @key[in] Truncation := Error);

      @key[function] Tail (Source : @key[in] Bounded_String;
                     Count  : @key[in] Natural;
                     Pad    : @key[in] Character  := Space;
                     Drop   : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[procedure] Tail (Source : @key[in] @key[out] Bounded_String;
                      Count  : @key[in] Natural;
                      Pad    : @key[in] Character  := Space;
                      Drop   : @key[in] Truncation := Error);

--@i{String constructor subprograms}

      @key[function] "*" (Left  : @key[in] Natural;
                    Right : @key[in] Character)
         @key[return] Bounded_String;

      @key[function] "*" (Left  : @key[in] Natural;
                    Right : @key[in] String)
         @key[return] Bounded_String;

      @key[function] "*" (Left  : @key[in] Natural;
                    Right : @key[in] Bounded_String)
         @key[return] Bounded_String;


      @key[function] Replicate (Count : @key[in] Natural;
                          Item  : @key[in] Character;
                          Drop  : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[function] Replicate (Count : @key[in] Natural;
                          Item  : @key[in] String;
                          Drop  : @key[in] Truncation := Error)
         @key[return] Bounded_String;

      @key[function] Replicate (Count : @key[in] Natural;
                          Item  : @key[in] Bounded_String;
                          Drop  : @key[in] Truncation := Error)
         @key[return] Bounded_String;

   @key[private]
       ... -- @i{not specified by the language}
   @key[end] Generic_Bounded_Length;

@key[end] Ada.Strings.Bounded;
@end{example}

Null_Bounded_String represents the null string.
If an object of type Bounded_String is not otherwise initialized, it
will be initialized to the same value as Null_Bounded_String.
@begin{DescribeCode}
@begin{CodeExample}
@key[function] Length (Source : @key[in] Bounded_String) @key[return] Length_Range;
@end{CodeExample}

The Length function returns the length of the string represented by Source.

@begin{CodeExample}
@key[function] To_Bounded_String (Source : @key[in] String;
                            Drop   : @key[in] Truncation := Error)
   @key[return] Bounded_String;
@end{CodeExample}

If Source'Length <= Max_Length then this function
returns a Bounded_String that represents Source.
Otherwise the effect depends on the value of Drop:
@begin{itemize}
If Drop=Left, then
the result is a Bounded_String that represents the string comprising
the rightmost Max_Length characters of Source.

 If Drop=Right, then
the result is a Bounded_String that represents the string comprising
the leftmost Max_Length characters of Source.

If Drop=Error, then Strings.Length_Error is propagated.
@end{itemize}

@begin{CodeExample}
@key[function] To_String (Source : @key[in] Bounded_String) @key[return] String;
@end{CodeExample}

To_String returns the String value with lower bound 1 represented by
Source.  If B is a Bounded_String, then B = To_Bounded_String(To_String(B)).
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
@begin{CodeExample}
@key[function] Element (Source : @key[in] Bounded_String;
                  Index  : @key[in] Positive)
   @key[return] Character;
@end{CodeExample}

Returns the character at position Index in the string represented by Source;
propagates Index_Error if Index > Length(Source).

@begin{CodeExample}
@key[procedure] Replace_Element (Source : @key[in] @key[out] Bounded_String;
                           Index  : @key[in] Positive;
                           By     : @key[in] Character);
@end{CodeExample}

Updates Source such that the character at position Index in the string
represented by Source is By;
propagates Index_Error if Index > Length(Source).

@begin{CodeExample}
@key[function] Slice (Source : @key[in] Bounded_String;
                Low    : @key[in] Positive;
                High   : @key[in] Natural)
   @key[return] String;
@end{CodeExample}

Returns the slice at positions Low through High in the string represented
by Source; propagates Index_Error if
Low > Length(Source)+1.
@end{DescribeCode}

Each of the functions "=", "<", ">","<=", and ">="
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

Each of the transformation subprograms (Replace_Slice, Insert,
Overwrite, Delete), selector subprograms
 (Trim, Head, Tail),
and constructor functions ("*") has an effect based on its
corresponding subprogram in Strings.Fixed, and Replicate is based on
Fixed."*".  For each of these subprograms, the corresponding
fixed-length string subprogram is applied to the string represented by
the Bounded_String parameter.  To_Bounded_String is applied the result
string, with Drop (or Error in the case of Generic_Bounded_Length."*")
determining the effect when the string length exceeds Max_Length.
@begin{Ramification}
The "/=" operations between Bounded_String and String, and between String
and Bounded_String, are automatically defined based on the corrsponding
"=" operations.
@end{Ramification}
@end{StaticSem}

@begin{ImplAdvice}
Bounded string objects should not be implemented by implicit
pointers and dynamic allocation.
@begin{ImplNote}
The following is a possible implementation of the private part
of the package:
@begin{example}
@key[type] Bounded_String_Internals (Length : Length_Range := 0) @key[is]
   @key[record]
      Data : String(1..Length);
   @key[end] @key[record];

@key[type] Bounded_String @key[is]
   @key[record]
      Data : Bounded_String_Internals;  --@i{ Unconstrained}
   @key[end] @key[record];

Null_Bounded_String : @key[constant] Bounded_String :=
   (Data => (Length => 0,
             Data   => (1..0 => ' ')));
@end{example}
@end{ImplNote}
@end{ImplAdvice}

@LabeledAppendixSubSection{Unbounded-Length String Handling}
@begin{Intro}
The language-defined package Strings.Unbounded provides a
 private type Unbounded_String and a
set of operations.  An object of type Unbounded_String represents a String
whose low bound is 1 and whose length
can vary conceptually between 0 and Natural'Last.
The subprograms for fixed-length string handling
are either overloaded directly  for Unbounded_String, or are modified as
needed to reflect the flexibility in length.  Since the
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
The library package Strings.Unbounded has the following declaration:
@begin{example}
@key[with] Ada.Strings.Maps;
@ChildUnit{Parent=[Ada.Strings],Child=[Unbounded],Expanded=[Ada.Strings.Unbounded]}
@key[package] Ada.Strings.Unbounded @key[is]
   @key[pragma] Preelaborate(Unbounded);

@LangDefType{Package=[Ada.Strings.Unbounded],Type=[Unbounded_String]}
   @key[type] Unbounded_String @key[is] @key[private];

   Null_Unbounded_String : @key[constant] Unbounded_String;

   @key[function] Length (Source : @key[in] Unbounded_String) @key[return] Natural;


   @key[type] String_Access @key[is] @key[access] @key[all] String;
   @key[procedure] Free (X : @key[in] @key[out] String_Access);

--@i{ Conversion, Concatenation, and Selection functions}

   @key[function] To_Unbounded_String (Source : @key[in] String)
      @key[return] Unbounded_String;

   @key[function] To_Unbounded_String (Length : @key[in] Natural)
      @key[return] Unbounded_String;

   @key[function] To_String (Source : @key[in] Unbounded_String) @key[return] String;


   @key[procedure] Append (Source   : @key[in out] Unbounded_String;
                     New_Item : @key[in] Unbounded_String);

   @key[procedure] Append (Source   : @key[in out] Unbounded_String;
                     New_Item : @key[in] String);

   @key[procedure] Append (Source   : @key[in out] Unbounded_String;
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


   @key[function] Element (Source : @key[in] Unbounded_String;
                     Index  : @key[in] Positive)
      @key[return] Character;

   @key[procedure] Replace_Element (Source : @key[in] @key[out] Unbounded_String;
                              Index  : @key[in] Positive;
                              By     : @key[in] Character);


   @key[function] Slice (Source : @key[in] Unbounded_String;
                   Low    : @key[in] Positive;
                   High   : @key[in] Natural)
      @key[return] String;


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


--@i{ Search subprograms}

   @key[function] Index (Source   : @key[in] Unbounded_String;
                   Pattern  : @key[in] String;
                   Going    : @key[in] Direction := Forward;
                   Mapping  : @key[in] Maps.Character_Mapping
                                := Maps.Identity)
      @key[return] Natural;

   @key[function] Index (Source   : @key[in] Unbounded_String;
                   Pattern  : @key[in] String;
                   Going    : @key[in] Direction := Forward;
                   Mapping  : @key[in] Maps.Character_Mapping_Function)
      @key[return] Natural;

   @key[function] Index (Source : @key[in] Unbounded_String;
                   Set    : @key[in] Maps.Character_Set;
                   Test   : @key[in] Membership := Inside;
                   Going  : @key[in] Direction  := Forward) @key[return] Natural;


   @key[function] Index_Non_Blank (Source : @key[in] Unbounded_String;
                             Going  : @key[in] Direction := Forward)
      @key[return] Natural;


   @key[function] Count (Source   : @key[in] Unbounded_String;
                   Pattern  : @key[in] String;
                   Mapping  : @key[in] Maps.Character_Mapping
                                := Maps.Identity)
      @key[return] Natural;

   @key[function] Count (Source   : @key[in] Unbounded_String;
                   Pattern  : @key[in] String;
                   Mapping  : @key[in] Maps.Character_Mapping_Function)
      @key[return] Natural;

   @key[function] Count (Source   : @key[in] Unbounded_String;
                   Set      : @key[in] Maps.Character_Set)
      @key[return] Natural;


   @key[procedure] Find_Token (Source : @key[in] Unbounded_String;
                         Set    : @key[in] Maps.Character_Set;
                         Test   : @key[in] Membership;
                         First  : @key[out] Positive;
                         Last   : @key[out] Natural);


--@i{ String translation subprograms}

   @key[function] Translate (Source  : @key[in] Unbounded_String;
                       Mapping : @key[in] Maps.Character_Mapping)
      @key[return] Unbounded_String;

   @key[procedure] Translate (Source  : @key[in] @key[out] Unbounded_String;
                        Mapping : @key[in] Maps.Character_Mapping);

   @key[function] Translate (Source  : @key[in] Unbounded_String;
                       Mapping : @key[in] Maps.Character_Mapping_Function)
      @key[return] Unbounded_String;

   @key[procedure] Translate (Source  : @key[in] @key[out] Unbounded_String;
                        Mapping : @key[in] Maps.Character_Mapping_Function);

--@i{ String transformation subprograms}

   @key[function] Replace_Slice (Source   : @key[in] Unbounded_String;
                           Low      : @key[in] Positive;
                           High     : @key[in] Natural;
                           By       : @key[in] String)
      @key[return] Unbounded_String;

   @key[procedure] Replace_Slice (Source   : @key[in] @key[out] Unbounded_String;
                            Low      : @key[in] Positive;
                            High     : @key[in] Natural;
                            By       : @key[in] String);

   @key[function] Insert (Source   : @key[in] Unbounded_String;
                    Before   : @key[in] Positive;
                    New_Item : @key[in] String)
      @key[return] Unbounded_String;

   @key[procedure] Insert (Source   : @key[in] @key[out] Unbounded_String;
                     Before   : @key[in] Positive;
                     New_Item : @key[in] String);

   @key[function] Overwrite (Source    : @key[in] Unbounded_String;
                       Position  : @key[in] Positive;
                       New_Item  : @key[in] String)
      @key[return] Unbounded_String;

   @key[procedure] Overwrite (Source    : @key[in] @key[out] Unbounded_String;
                        Position  : @key[in] Positive;
                        New_Item  : @key[in] String);

   @key[function] Delete (Source  : @key[in] Unbounded_String;
                    From    : @key[in] Positive;
                    Through : @key[in] Natural)
      @key[return] Unbounded_String;

   @key[procedure] Delete (Source  : @key[in] @key[out] Unbounded_String;
                     From    : @key[in] Positive;
                     Through : @key[in] Natural);

   @key[function] Trim (Source : @key[in] Unbounded_String;
                  Side   : @key[in] Trim_End)
      @key[return] Unbounded_String;

   @key[procedure] Trim (Source : @key[in] @key[out] Unbounded_String;
                   Side   : @key[in] Trim_End);

   @key[function] Trim (Source : @key[in] Unbounded_String;
                  Left   : @key[in] Maps.Character_Set;
                  Right  : @key[in] Maps.Character_Set)
      @key[return] Unbounded_String;

   @key[procedure] Trim (Source : @key[in] @key[out] Unbounded_String;
                   Left   : @key[in] Maps.Character_Set;
                   Right  : @key[in] Maps.Character_Set);


   @key[function] Head (Source : @key[in] Unbounded_String;
                  Count  : @key[in] Natural;
                  Pad    : @key[in] Character := Space)
      @key[return] Unbounded_String;

   @key[procedure] Head (Source : @key[in] @key[out] Unbounded_String;
                   Count  : @key[in] Natural;
                   Pad    : @key[in] Character := Space);

   @key[function] Tail (Source : @key[in] Unbounded_String;
                  Count  : @key[in] Natural;
                  Pad    : @key[in] Character := Space)
      @key[return] Unbounded_String;

   @key[procedure] Tail (Source : @key[in] @key[out] Unbounded_String;
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
   ... -- @i{not specified by the language}
@key[end] Ada.Strings.Unbounded;
@end{example}

Null_Unbounded_String represents the null String.
If an object of type Unbounded_String is not otherwise initialized, it
will be initialized to the same value as Null_Unbounded_String.

The function Length returns the length of the String represented by Source.

The type String_Access provides a (non-private) access type for explicit
processing of unbounded-length strings.
The procedure Free performs
an unchecked deallocation of an object of type String_Access.

The function To_Unbounded_String(Source : in String)
returns an Unbounded_String that represents Source.
The function To_Unbounded_String(Length : in Natural)
returns an Unbounded_String that represents an uninitialized String
whose length is Length.

The function To_String returns the String with lower bound 1 represented by Source.
To_String and To_Unbounded_String are related as follows:
@begin{itemize}
If S is a String, then To_String(To_Unbounded_String(S)) = S.

If U is an Unbounded_String, then To_Unbounded_String(To_String(U)) = U.
@end{itemize}

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

Each of the functions "=", "<", ">","<=", and ">="
returns the same result as the corresponding String
operation applied to the String values given or represented by Left and Right.

Each of the search subprograms (Index, Index_Non_Blank, Count,
Find_Token) has the
same effect as the corresponding subprogram in Strings.Fixed applied
to the string represented by the Unbounded_String parameter.

The Translate function has
an analogous effect to the corresponding subprogram in Strings.Fixed.
The translation is applied to the string represented by the Unbounded_String
parameter, and the result is converted (via To_Unbounded_String) to an
Unbounded_String.

Each of the transformation functions (Replace_Slice, Insert, Overwrite,
Delete), selector functions (Trim, Head, Tail), and constructor functions
("*") is likewise analogous to its corresponding
subprogram in Strings.Fixed.  For each of the subprograms,
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

A sample implementation of the private part of
the package and several of the subprograms appears in the Rationale.

@end{ImplNote}
@end{ImplReq}

@LabeledAppendixSubSection{String-Handling Sets and Mappings}

@begin{Intro}

The language-defined package Strings.Maps.Constants declares
Character_Set
and Character_Mapping
constants corresponding to  classification and conversion functions
in package Characters.Handling.
@begin{discussion}
The Constants package is a child of Strings.Maps since it needs
visibility of the private part of Strings.Maps in order to
initialize the constants
in a preelaborable way (i.e. via aggregates versus function calls).
@end{discussion}

@end{Intro}

@begin{StaticSem}
The library package Strings.Maps.Constants has the following declaration:

@begin{example}
@ChildUnit{Parent=[Ada.Strings.Maps],Child=[Constants],Expanded=[Ada.Strings.Maps.Constants]}
@key[package] Ada.Strings.Maps.Constants @key[is]
   @key[pragma] Preelaborate(Constants);

   Control_Set           : @key[constant] Character_Set;
   Graphic_Set           : @key[constant] Character_Set;
   Letter_Set            : @key[constant] Character_Set;
   Lower_Set             : @key[constant] Character_Set;
   Upper_Set             : @key[constant] Character_Set;
   Basic_Set             : @key[constant] Character_Set;
   Decimal_Digit_Set     : @key[constant] Character_Set;
   Hexadecimal_Digit_Set : @key[constant] Character_Set;
   Alphanumeric_Set      : @key[constant] Character_Set;
   Special_Set           : @key[constant] Character_Set;
   ISO_646_Set           : @key[constant] Character_Set;

   Lower_Case_Map        : @key[constant] Character_Mapping;
     --@i{Maps to lower case for letters, else identity}
   Upper_Case_Map        : @key[constant] Character_Mapping;
     --@i{Maps to upper case for letters, else identity}
   Basic_Map             : @key[constant] Character_Mapping;
     --@i{Maps to basic letter for letters, else identity}

@key[private]
   ... -- @i{not specified by the language}
@key[end] Ada.Strings.Maps.Constants;
@end{example}


Each of these constants represents a correspondingly named
set of characters or
character mapping in Characters.Handling
(see @refsecnum(The Package Characters.Handling)).
@end{StaticSem}

@LabeledAppendixSubSection{Wide_String Handling}

@begin{Intro}
Facilities for handling strings of Wide_Character elements are
found in the packages Strings.Wide_Maps, Strings.Wide_Fixed,
Strings.Wide_Bounded, Strings.Wide_Unbounded,

and Strings.Wide_Maps.Wide_Constants.

They provide the same string-handling operations
as the corresponding packages for
strings of Character elements.
@Defn{Ada.Strings.Wide_Fixed}
@ChildUnit{Parent=[Ada.Strings],Child=[Wide_Fixed],Expanded=[Ada.Strings.Wide_Fixed]}
@Defn{Ada.Strings.Wide_Bounded}
@ChildUnit{Parent=[Ada.Strings],Child=[Wide_Bounded],Expanded=[Ada.Strings.Wide_Bounded]}
@Defn{Ada.Strings.Wide_Unbounded}
@ChildUnit{Parent=[Ada.Strings],Child=[Wide_Unbounded],Expanded=[Ada.Strings.Wide_Unbounded]}
@Defn{Ada.Strings.Wide_Maps.Wide_Constants}
@ChildUnit{Parent=[Ada.Strings.Wide_Maps],Child=[Wide_Constants],Expanded=[Ada.Strings.Wide_Maps.Wide_Constants]}
@end{Intro}

@begin{StaticSem}
The package Strings.Wide_Maps has the following declaration.
@begin{example}
@ChildUnit{Parent=[Ada.Strings],Child=[Wide_Maps],Expanded=[Ada.Strings.Wide_Maps]}
@key[package] Ada.Strings.Wide_Maps @key[is]
   @key[pragma] Preelaborate(Wide_Maps);

   --@i{ Representation for a set of Wide_Character values:}
@LangDefType{Package=[Ada.Strings.Wide_Maps],Type=[Wide_Character_Set]}
   @key[type] Wide_Character_Set @key[is] @key[private];

   Null_Set : @key[constant] Wide_Character_Set;

   @key[type] Wide_Character_Range @key[is]
     @key[record]
         Low  : Wide_Character;
         High : Wide_Character;
     @key[end] @key[record];
   -- @i{Represents Wide_Character range Low..High}

   @key[type] Wide_Character_Ranges @key[is] @key[array] (Positive @key[range] <>) @key[of] Wide_Character_Range;

   @key[function] To_Set    (Ranges : @key[in] Wide_Character_Ranges) @key[return] Wide_Character_Set;

   @key[function] To_Set    (Span   : @key[in] Wide_Character_Range)  @key[return] Wide_Character_Set;

   @key[function] To_Ranges (Set    : @key[in] Wide_Character_Set)    @key[return] Wide_Character_Ranges;

   @key[function] "="   (Left, Right : @key[in] Wide_Character_Set) @key[return] Boolean;

   @key[function] "@key[not]" (Right : @key[in] Wide_Character_Set)       @key[return] Wide_Character_Set;
   @key[function] "@key[and]" (Left, Right : @key[in] Wide_Character_Set) @key[return] Wide_Character_Set;
   @key[function] "@key[or]"  (Left, Right : @key[in] Wide_Character_Set) @key[return] Wide_Character_Set;
   @key[function] "@key[xor]" (Left, Right : @key[in] Wide_Character_Set) @key[return] Wide_Character_Set;
   @key[function] "@key[@en]"   (Left, Right : @key[in] Wide_Character_Set) @key[return] Wide_Character_Set;

   @key[function] Is_In (Element : @key[in] Wide_Character;
                   Set     : @key[in] Wide_Character_Set)
      @key[return] Boolean;

   @key[function] Is_Subset (Elements : @key[in] Wide_Character_Set;
                       Set      : @key[in] Wide_Character_Set)
      @key[return] Boolean;

   @key[function] "<=" (Left  : @key[in] Wide_Character_Set;
                  Right : @key[in] Wide_Character_Set)
      @key[return] Boolean @key[renames] Is_Subset;


   --@i{ Alternative representation for a set of Wide_Character values:}
   @key[subtype] Wide_Character_Sequence @key[is] Wide_String;

   @key[function] To_Set (Sequence  : @key[in] Wide_Character_Sequence) @key[return] Wide_Character_Set;

   @key[function] To_Set (Singleton : @key[in] Wide_Character) @key[return] Wide_Character_Set;

   @key[function] To_Sequence (Set  : @key[in] Wide_Character_Set) @key[return] Wide_Character_Sequence;


   --@i{ Representation for a Wide_Character to Wide_Character mapping:}
   @key[type] Wide_Character_Mapping @key[is] @key[private];

   @key[function] Value (Map     : @key[in] Wide_Character_Mapping;
                   Element : @key[in] Wide_Character)
      @key[return] Wide_Character;

   Identity : @key[constant] Wide_Character_Mapping;

   @key[function] To_Mapping (From, To : @key[in] Wide_Character_Sequence)
      @key[return] Wide_Character_Mapping;

   @key[function] To_Domain (Map : @key[in] Wide_Character_Mapping)
      @key[return] Wide_Character_Sequence;

   @key[function] To_Range  (Map : @key[in] Wide_Character_Mapping)
      @key[return] Wide_Character_Sequence;


   @key{type} Wide_Character_Mapping_Function @key{is}
      @key{access} @key{function} (From : @key{in} Wide_Character) @key{return} Wide_Character;

@key[private]
   ... -- @i{not specified by the language}
@key[end] Ada.Strings.Wide_Maps;
@end{example}

The context clause for each of the packages Strings.Wide_Fixed,

Strings.Wide_Bounded, and Strings.Wide_Unbounded

identifies Strings.Wide_Maps instead of Strings.Maps.

For each of the packages  Strings.Fixed, Strings.Bounded,
Strings.Unbounded, and

Strings.Maps.Constants

the corresponding wide string package has
the same contents except that
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

Unbounded_Wide_String replaces Unbounded_String

Null_Unbounded_Wide_String replaces Null_Unbounded_String

Wide_String_Access replaces String_Access

To_Unbounded_Wide_String replaces To_Unbounded_String
@end{Itemize}

The following additional declaration is present in
Strings.Wide_Maps.Wide_Constants:
@begin{example}
Character_Set : @key[constant] Wide_Maps.Wide_Character_Set;
--@i{Contains each Wide_Character value WC such that Characters.Is_Character(WC) is True}
@end{example}
@end{StaticSem}

@begin{NotesNotes}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
If a null Wide_Character_Mapping_Function is passed to any of the
Wide_String handling subprograms, Constraint_Error is propagated.

Each Wide_Character_Set constant in the package
Strings.Wide_Maps.Wide_Constants contains no values outside the Character
portion of Wide_Character.  Similarly, each Wide_Character_Mapping
constant in this package is the identity mapping when applied to
any element outside the Character portion of Wide_Character. 
@end{NotesNotes}

