@Part(text, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/text.mss,v $}
@comment{$Revision: 1.5 $ $Date: 2007/08/05 01:46:39 $}

@LabeledSection{package Asis.Text}

@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Text]}Asis.Text
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis],Child=[Text]}Asis.Text @key[is]}]}

This package encapsulates a set of operations to access the text of ASIS
Elements. It assumes no knowledge of the existence, location, or form of
the program text.

The text of a program consists of the texts of one or more compilations.
The text of each compilation is a sequence of separate lexical elements.
Each lexical element is either a delimiter, an identifier (which can be a
reserved word), a numeric literal, a character literal, a string literal,
blank space, or a comment.

Each ASIS Element has a text image whose value is the series of characters
contained by the text span of the Element. The text span covers all the
characters from the first character of the Element through the last
character of the Element over some range of lines.

General Usage Rules:

Line lists can be indexed to obtain individual lines. The bounds of each
list correspond to the lines with those same numbers from the compilation
text.

Any Asis.Text query may raise ASIS_Failed with a Status of Text_Error if
the program text cannot be located or retrieved for any reason such as
renaming, deletion, corruption, or moving of the text.

@LabeledClause{type Line}

An Ada text @i{line} abstraction (a private type).@Defn{Line}@Defn{Text line}

Used to represent text fragments from a compilation.
ASIS Lines are representations of the compilation text.
This shall be supported by all ASIS implementations.

@begin{DescribeCode}
@begin{Example}
@key[type] @AdaTypeDefn{Line} @key[is private];
@AdaObjDefn{Nil_Line}  : @key[constant] Line;

@key[function] "=" (Left  : @key[in] Line;
                  Right : @key[in] Line)
                  @key[return] Boolean @key[is abstract];
@end{Example}

@ChgAdded{Version=[1],Text=[Line shall be an undiscriminated private type,
or, shall be derived from an undiscriminated private type. It can be declared
as a new type or as a subtype of an existing type.]}

Nil_Line is the value of an uninitialized Line object.
@end{DescribeCode}


@LabeledClause{type Line_Number}

Line_Number is a numeric subtype that allows each ASIS implementation to place
constraints on the upper bound for Line_List elements and compilation unit size.

The upper bound of Line_Number (Maximum_Line_Number) is the only
allowed variation for these declarations.

Line_Number = 0 is reserved to act as an "invalid" Line_Number value. No
unit text line will ever have a Line_Number of zero.

@ChgDeleted{Version=[1],Text=[Line shall be an undiscriminated private type,
or, shall be derived from an undiscriminated private type. It can be declared
as a new type or as a subtype of an existing type.]}@ChgNote{Moved up}

@begin{DescribeCode}
@begin{Example}
@AdaObjDefn{Maximum_Line_Number} : @key[constant] ASIS_Natural :=
    @i{Implementation_Defined_Integer_Constant};

@key[subtype] @AdaSubtypeDefn{Name=[Line_Number],Of=[ASIS_Natural]} @key[is] ASIS_Natural @key[range] 0 .. Maximum_Line_Number;
@end{Example}
@end{DescribeCode}


@LabeledClause{type Line_Number_Positive}

@begin{DescribeCode}
@begin{Example}
@key[subtype] @AdaSubtypeDefn{Name=[Line_Number_Positive],Of=[Line_Numberl]} @key[is] Line_Number @key[range] 1 .. Maximum_Line_Number;
@end{Example}
@end{DescribeCode}


@LabeledClause{type Line_List}

@begin{DescribeCode}
@begin{Example}
@key[type] @AdaTypeDefn{Line_List} @key[is array] (Line_Number_Positive @key[range] <>) @key[of] Line;
@AdaObjDefn{Nil_Line_List} : @key[constant] Line_List;
@end{Example}
@end{DescribeCode}


@LabeledClause{type Character_Position}

Character_Position is a numeric subtype that allows each ASIS implementation to
place constraints on the upper bound for Character_Position and for compilation
unit line lengths.

The upper bound of Character_Position (Maximum_Line_Length) is the
only allowed variation for these declarations.

Character_Position = 0 is reserved to act as an "invalid"
Character_Position value. No unit text line will ever have a character in
position zero.

@begin{DescribeCode}
@begin{Example}
@AdaObjDefn{Maximum_Line_Length} : @key[constant] ASIS_Natural :=
    @i{Implementation_Defined_Integer_Constant};

@key[subtype] @AdaSubtypeDefn{Name=[Character_Position],Of=[ASIS_Natural]} @key[is] ASIS_Natural @key[range] 0 .. Maximum_Line_Length;
@end{Example}
@end{DescribeCode}


@LabeledClause{type Character_Position_Positive}

@begin{DescribeCode}
@begin{Example}
@key[subtype] @AdaSubtypeDefn{Name=[Character_Position_Positive],Of=[Character_Position]} @key[is]
    Character_Position @key[range] 1 .. Maximum_Line_Length;
@end{Example}
@end{DescribeCode}


@LabeledClause{type Span}

Span is a single text position that is identified by a line number and a column
number representing the text's position within the compilation unit.

The text of an element can span one or more lines. The textual Span of an
element identifies the lower and upper bound of a span of text positions.

Spans and positions give client tools the option of accessing compilation
unit text through the queries provided by this package, or, to access
the text directly through the original compilation unit text file. Type span
facilitates the capture of comments before or after an element.

@begin{SingleNote}
The original compilation unit text may or may not have existed in a
"file", and any such file may or may not still exist. Reference Manual 10.1
specifies that the text of a compilation unit is submitted to a compiler. It
does not specify that the text is stored in a "file", nor does it specify that
the text of a compilation unit has any particular lifetime.
@end{SingleNote}

@begin{DescribeCode}
@begin{Example}
@key[type] @AdaTypeDefn{Span} @key[is]                                       -- @examcom{Default is Nil_Span}
   @key[record]
      First_Line   : Line_Number_Positive        := 1; -- @examcom{1..0 - empty}
      First_Column : Character_Position_Positive := 1; -- @examcom{1..0 - empty}
      Last_Line    : Line_Number                 := 0;
      Last_Column  : Character_Position          := 0;
   @key[end record];

@AdaObjDefn{Nil_Span} : @key[constant Span] := (First_Line   => 1,
                             First_Column => 1,
                             Last_Line    => 0,
                             Last_Column  => 0);
@end{Example}
@end{DescribeCode}


@LabeledClause{function First_Line_Number}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{First_Line_Number} (Element : @key[in] Asis.Element)
                           @key[return] Line_Number;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to query.

Returns the first line number on which the text of the element resides.

Returns 0 if not Is_Text_Available(Element).
@end{DescribeCode}

@begin{UsageNote}
The line number recorded for a particular element may or may not match the
"true" line number of the program text for that element if the Ada environment
and the local text editors do not agree on the definition of "line". For
example, the Reference Manual states that any occurrence of an Ascii.Cr character
is to be treated as one or more end-of-line occurrences. On most Unix systems,
the editors do not treat a carriage return as being an end-of-line character.

@noprefix@;Ada treats all of the following as end-of-line characters: Ascii.Cr,
Ascii.Lf, Ascii.Ff, Ascii.Vt. It is up to the compilation system to
determine whether sequences of these characters causes one, or more,
end-of-line occurrences. Be warned, if the Ada environment and the
system editor (or any other line-counting program) do not use the same
end-of-line conventions, then the line numbers reported by ASIS may not
match those reported by those other programs.
@end{UsageNote}


@LabeledClause{function Last_Line_Number}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Last_Line_Number} (Element : @key[in] Asis.Element)
                           @key[return] Line_Number;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to query.

Returns the last line number on which the text of the element resides.

Returns 0 if not Is_Text_Available(Element).
@end{DescribeCode}


@LabeledClause{function Element_Span}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Element_Span} (Element : @key[in] Asis.Element)
                       @key[return] Span;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to
query.

Returns the span of the given element.

Returns a Nil_Span if the text of a Compilation_Unit (Compilation) cannot be
located for any reason.
@end{DescribeCode}

@begin{UsageNote}
For this query, Element is only a means to access the
Compilation_Unit (Compilation), the availability of the text of this Element
itself is irrelevant to the result of the query.
@end{UsageNote}


@LabeledClause{function Compilation_Unit_Span}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Compilation_Unit_Span} (Element : @key[in] Asis.Element)
                                @key[return] Span;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to
query.

Returns the span of the text comprising the enclosing compilation unit of
the given element.

Returns a Nil_Span if the text of a Compilation_Unit (Compilation) cannot be
located for any reason.
@end{DescribeCode}

@begin{UsageNote}
For this query, Element is only a means to access the
Compilation_Unit (Compilation), the availability of the text of this Element
itself is irrelevant to the result of the query.
@end{UsageNote}


@LabeledClause{function Compilation_Span}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Compilation_Span} (Element : @key[in] Asis.Element)
                           @key[return] Span;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to
query.

Returns the span of the text comprising the compilation to which the
element belongs. The text span may include one or more compilation units.

Returns a Nil_Span if not Is_Text_Available(Element).
@end{DescribeCode}


@LabeledClause{function Is_Nil (line)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Nil} (Right : @key[in] Line)
                 @key[return] Boolean;
@end{Example}

Right @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the line to
check.

Returns True if the argument is the Nil_Line.

A Line from a Line_List obtained from any of the Lines functions
will not be Is_Nil even if it has a length of zero.
@end{DescribeCode}


@LabeledClause{function Is_Nil (line list)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Nil} (Right : @key[in] Line_List)
                 @key[return] Boolean;
@end{Example}

Right @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the line list to
check.

Returns True if the argument has a 'Length of zero.
@end{DescribeCode}


@LabeledClause{function Is_Nil (span)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Nil} (Right : @key[in] Span)
                 @key[return] Boolean;
@end{Example}

Right @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the Span to
check.

Returns True if the argument has a Nil_Span.
@end{DescribeCode}


@LabeledClause{function Is_Equal (lines)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Equal} (Left  : @key[in] Line;
                   Right : @key[in] Line) @key[return] Boolean;
@end{Example}

Left @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the first of the two lines.
Right @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the second of the two lines.

Returns True if the two lines encompass the same text (have the same Span
and are from the same compilation).
@end{DescribeCode}


@LabeledClause{function Is_Identical (lines)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Identical} (Left  : @key[in] Line;
                       Right : @key[in] Line) @key[return] Boolean;
@end{Example}

Left @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the first of the
two lines. Right @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the
second of the two lines.

Returns True if the two lines encompass the same text (have the same Span
and are from the same compilation) and are from the same Context.
@end{DescribeCode}


@LabeledClause{function Length}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Length} (The_Line : @key[in] Line) @key[return] Character_Position;
@end{Example}

The_Line @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the line to query.

Returns the length of the line.

Raises ASIS_Inappropriate_Line if Is_Nil (The_Line).
@end{DescribeCode}


@LabeledClause{function Lines (element)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Lines} (Element : @key[in] Asis.Element) @key[return] Line_List;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to
query.

Returns a list of lines covering the span of the given program element.

Returns a Nil_Span if the text of a Compilation containing a given
Element cannot be located for any reason.

Line lists can be indexed to obtain individual lines. The bounds of each
list correspond to the lines with those same numbers in the compilation
text.

The first Line of the result contains text from the compilation starting at
the First_Line/First_Column of Element's Span. The last Line of the result
contains text from the compilation ending at the Last_Line/Last_Column of
the Element's Span. Text before or after those limits is not reflected
in the returned list.
@end{DescribeCode}

@begin{UsageNote}
For this query, Element is only a means to access the
Compilation_Unit (Compilation), the availability of the text of this Element
itself is irrelevant to the result of the query.
@end{UsageNote}



@LabeledClause{function Lines (element with span)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Lines} (Element  : @key[in] Asis.Element;
                The_Span : @key[in] Span) @key[return] Line_List;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the element to
query. The_Span @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the
textual span to return.

Returns a list of lines covering the given span from the compilation
containing the given program element.

Returns a Nil_Span if the text of a Compilation containing a given
Element cannot be located for any reason.

This operation can be used to access lines from text outside the span of an
element, but still within the compilation. For example, lines containing
preceding comments or lines between two elements.

Line lists can be indexed to obtain individual lines. The bounds of each
list correspond to the lines with those same numbers in the compilation
text.

The first Line of the result contains text from the compilation starting at
line Span.First_Line and column Span.First_Column. The last Line of the
result contains text from the compilation ending at line Span.Last_Line and
column Span.Last_Column. Text before or after those limits is not
reflected in the returned list.

Raises ASIS_Inappropriate_Line_Number if Is_Nil (The_Span). If The_Span defines
a line whose number is outside the range of text lines that can be accessed
through the Element, the implementation is encouraged, but not required to
raise ASIS_Inappropriate_Line_Number.
@end{DescribeCode}

@begin{UsageNote}
For this query, Element is only a means to access the
Compilation_Unit (Compilation), the availability of the text of this Element
itself is irrelevant to the result of the query.
@end{UsageNote}


@LabeledClause{function Lines (element with lines)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Lines} (Element    : @key[in] Asis.Element;
                First_Line : @key[in] Line_Number_Positive;
                Last_Line  : @key[in] Line_Number) @key[return] Line_List;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the element to query.
First_Line @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the first line to return.
Last_Line @Chg{Version=[1],New=[specifies],Old=[ @en Specifies]} the last line to return.

Returns a list of Lines covering the full text for each of the indicated
lines from the compilation containing the given element. This operation
can be used to access lines from text outside the span of an element, but
still within the compilation.

Returns a Nil_Span if the text of a Compilation containing a given
Element cannot be located for any reason.

Line lists can be indexed to obtain individual lines. The bounds of each
list correspond to the lines with those same numbers in the compilation text.

Raises ASIS_Inappropriate_Line_Number if the span is nil. If the span defines
a line whose number is outside the range of text lines that can be accessed
through the Element, the implementation is encouraged, but not required to
raise ASIS_Inappropriate_Line_Number.
@end{DescribeCode}

@begin{UsageNote}
For this query, Element is only a means to access the
Compilation_Unit (Compilation), the availability of the text of this Element
itself is irrelevant to the result of the query.
@end{UsageNote}


@LabeledClause{function Delimiter_Image}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Delimiter_Image} @key[return] Wide_String;
@end{Example}

Returns the string used as the delimiter separating individual lines of
text within the program text image of an element. It is also used as the
delimiter separating individual lines of strings returned by Debug_Image.
@end{DescribeCode}


@LabeledClause{function Element_Image}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Element_Image} (Element : @key[in] Asis.Element) @key[return] Program_Text;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to
query.

Returns a program text image of the element. The image of an element can
span more than one line, in which case the program text returned by the
function Delimiter_Image separates the individual lines. The bounds on
the returned program text value are 1..N, N is as large as necessary.

Returns a null string if not Is_Text_Available(Element).

If an Element's Span begins at column position P, the returned program text will
be padded at the beginning with P-1 white space characters (Ascii.' ' or Ascii.Ht).
The first character of the Element's image will thus begin at character P of the
returned program text. Due to the possible presence of Ascii.Ht characters, the
"column" position of characters within the image might not be the same as their
print-column positions when the image is displayed on a screen or printed.
@end{DescribeCode}

@begin{SingleNote}
The image of a large element can exceed the range of Program_Text. In this
case, the exception ASIS_Failed is raised with a Status of Capacity_Error.
Use the Lines function to operate on the image of large elements.
@end{SingleNote}


@LabeledClause{function Line_Image}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Line_Image} (The_Line : @key[in] Line) @key[return] Program_Text;
@end{Example}

The_Line @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the line to
query.

Returns a program text image of the line. The image of a single lexical
element can be sliced from the returned value using the first and last
column character positions from the Span of the Element. The bounds on the
returned program text are 1 .. Length(Line).

If the Line is the first line from the Lines result for an Element, it can
represent only a portion of a line from the original compilation. If the
span began at character position P, the first Line of it's Lines
result is padded at the beginning with P-1 white space characters
(Ascii.' ' or Ascii.Ht). The first character of the image will
thus begin at character P of the program text for the first Line. Due to the
possible presence of Ascii.Ht characters, the "column" position of
characters within the image may not be the same as their print-column
positions when the image is displayed or printed.

Similarly, if the Line is the last line from the Lines result for an
Element, it may represent only a portion of a line from the original
compilation. The program text image of such a Line is shorter than the
line from compilation and will contain only the initial portion of
that line.

Raises ASIS_Inappropriate_Line if Is_Nil (The_Line).
@end{DescribeCode}


@LabeledClause{function Non_Comment_Image}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Non_Comment_Image} (The_Line : @key[in] Line) @key[return] Program_Text;
@end{Example}

The_Line @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the line to query.

Returns a program text image of a Line up to, but excluding, any comment
appearing in that Line.

The value returned is the same as that returned by the Image function,
except that any hyphens ("--") that start a comment, and any characters
that follow those hyphens, are dropped.

The bounds on the returned program text are 1..N, where N is one less than the
column of any hyphens ("--") that start a comment on the line.

Raises ASIS_Inappropriate_Line if Is_Nil (The_Line).
@end{DescribeCode}


@LabeledClause{function Comment_Image}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Comment_Image} (The_Line : @key[in] Line) @key[return] Program_Text;
@end{Example}

The_Line @Chg{Version=[1],New=[specifies],Old=[   @en Specifies]} the line to
query.

Returns a program text image of any comment on that line, excluding any
lexical elements preceding the comment.

The value returned is the same as that returned by the Image function,
except that any program text prior to the two adjacent hyphens ("--") which start
a comment is replaced by an equal number of spaces. If the hyphens began in
column P of the Line, they will also begin in character position P of the
returned program text.

A null string is returned if the line has no comment.

The bounds of the program text are 1..N, where N is as large as necessary.

Raises ASIS_Inappropriate_Line if Is_Nil (The_Line).
@end{DescribeCode}


@LabeledClause{function Is_Text_Available}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Is_Text_Available} (Element : @key[in] Asis.Element) @key[return] Boolean;
@end{Example}

Element @Chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to query.

Returns True if the implementation can return a valid text image for the
given element.

Returns False for any Element that Is_Nil, Is_Part_Of_Implicit, or
Is_Part_Of_Instance.

Returns False if the text of the element cannot be located for any reason
such as renaming, deletion, or moving of text.
@end{DescribeCode}

@begin{ImplReq}
An implementation shall make text available for all explicit elements.
@end{ImplReq}

@LabeledClause{function Debug_Image (line)}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Debug_Image} (The_Line : @key[in] Line) @key[return] Wide_String;
@end{Example}

The_Line @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} the line to convert.

Returns a string value containing implementation-defined debug
information associated with the line.

The return value uses Asis.Text.Delimiter_Image to separate the lines
of multi-line results. The return value does not end with
Asis.Text.Delimiter_Image.

These values are intended for two purposes. They are suitable for
inclusion in problem reports sent to the ASIS implementor. They can
be presumed to contain information useful when debugging the
implementation itself. They are also suitable for use by the ASIS
application when printing simple application debugging messages during
application development. They are intended to be, to some worthwhile
degree, intelligible to the user.
@end{DescribeCode}

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[private]]}

@ChgDeleted{Version=[1],Text=[   @key[type] Line @key[is] @i{(Implementation_Defined)};
   Nil_Line  : @key[constant] Line := @i{Implementation_Defined};
   Nil_Line_List : @key[constant] Line_List (1 .. 0) := (@key[others] => Nil_Line);]}

@ChgDeleted{Version=[1],Text=[@key[end] Asis.Text;]}
@end{Example}



