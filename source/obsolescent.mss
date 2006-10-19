@Part(obsolescent, Root="ada.mss")

@Comment{$Date: 2006/10/19 06:40:32 $}
@LabeledNormativeAnnex{Obsolescent Features}

@Comment{$Source: e:\\cvsroot/ARM/Source/obsolescent.mss,v $}
@Comment{$Revision: 1.40 $}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00368-01]}
@Redundant[@Defn{obsolescent feature}
This Annex contains descriptions of features of the
language whose functionality is largely redundant with other features
defined by this International Standard.
Use of these features is not recommended in newly written programs.
@Chg{Version=[2],New=[Use of these features can be prevented by using pragma
Restrictions (No_Obsolescent_Features), see @RefSecNum{Language-Defined Restrictions}.],
Old=[]}]
@begin{Ramification}
These features are still part of the language,
and have to be implemented by conforming implementations.
The primary reason for putting these descriptions here
is to get redundant features out of the way of most readers.
The designers of the next version of Ada@Chg{Version=[2],New=[],
Old=[ after Ada 95]} will have to
assess whether or not it makes sense to drop these features from the
language.
@end{Ramification}
@end{Intro}

@begin{DiffWord83}
@Leading@;The following features have been removed from the language,
rather than declared to be obsolescent:
@begin{Itemize}
The package Low_Level_IO
(see @RefSecNum{Input-Output}).

The Epsilon, Mantissa, Emax, Small, Large, Safe_Emax, Safe_Small, and
Safe_Large attributes of floating point types
(see @RefSecNum{Attributes of Floating Point Types}).

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00284-02]}
@ChgDeleted{Version=[2],Text=[The pragma Interface
(see @RefSecNum{Interfacing Pragmas}).]}

The pragmas System_Name, Storage_Unit, and Memory_Size
(see @RefSecNum{The Package System}).

The pragma Shared
(see @RefSecNum{Shared Variable Control}).
@end{Itemize}

Implementations can continue to support the above features for upward
compatibility.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00368-01]}
  @ChgAdded{Version=[2],Text=[A mention of the No_Obsolescent_Features
  restriction was added.]}
@end{DiffWord95}


@LabeledClause{Renamings of Ada 83 Library Units}

@begin{StaticSem}
The following @nt{library_unit_renaming_declaration}s exist:
@begin{Example}
@key[with] Ada.Unchecked_Conversion;
@key[generic] @key[function] Unchecked_Conversion @key[renames] Ada.Unchecked_Conversion;

@key[with] Ada.Unchecked_Deallocation;
@key[generic] @key[procedure] Unchecked_Deallocation @key[renames] Ada.Unchecked_Deallocation;

@key[with] Ada.Sequential_IO;
@key[generic] @key[package] Sequential_IO @key[renames] Ada.Sequential_IO;

@key[with] Ada.Direct_IO;
@key[generic] @key[package] Direct_IO @key[renames] Ada.Direct_IO;

@key[with] Ada.Text_IO;
@key[package] Text_IO @key[renames] Ada.Text_IO;

@key[with] Ada.IO_Exceptions;
@key[package] IO_Exceptions @key[renames] Ada.IO_Exceptions;

@key[with] Ada.Calendar;
@key[package] Calendar @key[renames] Ada.Calendar;

@key[with] System.Machine_Code;
@key[package] Machine_Code @key[renames] System.Machine_Code; --@RI{ If supported.}
@end{Example}
@end{StaticSem}

@begin{ImplReq}
The implementation shall allow the user to replace these renamings.
@end{ImplReq}


@LabeledClause{Allowed Replacements of Characters}

@begin{Syntax}
@begin{SyntaxText}
@Leading@;The following replacements are allowed for the vertical line, number sign,
and quotation mark characters:
@begin{Itemize}
A vertical line character (|) can be replaced by an exclamation mark
(!) where used as a delimiter.


The number sign characters (#) of a @nt{based_literal} can be replaced
by colons (:) provided that the replacement is done for both
occurrences.
@begin{Honest}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
  The intent is that such a replacement works in the
  Value@Chg{Version=[2],New=[,],Old=[ and]}
  Wide_Value@Chg{Version=[2],New=[, and Wide_Wide_Value],Old=[]} attributes,
  and in the Get procedures of Text_IO@Chg{Version=[2],New=[ (and Wide_Text_IO
  and Wide_Wide_Text_IO as well)],Old=[]}},
  so that things like @lquotes@;16:.123:@rquotes@; is acceptable.
@end{Honest}

The quotation marks (") used as string brackets at both ends of
a string literal can be replaced by percent signs (%) provided
that the enclosed sequence of characters contains no quotation mark, and
provided that both string brackets are replaced. Any
percent sign within the sequence of characters shall then be
doubled and each such doubled percent sign is interpreted as a
single percent sign character value.
@end{Itemize}

These replacements do not change the meaning of the program.
@begin{Reason}
The original purpose of this feature was to support hardware (for
example, teletype machines) that has long been obsolete.
The feature is no longer necessary for that reason.
Another use of the feature has been to replace the vertical line
character (|) when using certain hardware that treats that character
as a (non-English) letter.
The feature is no longer necessary for that reason, either,
since Ada 95 has full support for international character sets.
Therefore, we believe this feature is no longer necessary.

Users of equipment that still uses | to represent a letter will
continue to do so.
Perhaps by next the time Ada is revised,
such equipment will no longer be in use.

@Leading@;Note that it was never legal to use this feature as a convenient
method of including double quotes in a string without doubling them
@em the string literal:
@begin{Example}
%"This is quoted."%
@end{Example}

@Leading@;is not legal in Ada 83, nor will it be in Ada 95. One has to write:
@begin{Example}
"""This is quoted."""
@end{Example}
@end{Reason}
@end{SyntaxText}
@end{Syntax}

@LabeledClause{Reduced Accuracy Subtypes}

@begin{Intro}
A @nt<digits_constraint> may be used to define
a floating point subtype with a new
value for its requested decimal precision, as reflected
by its Digits attribute. Similarly, a @nt<delta_constraint>
may be used to define an ordinary fixed point subtype with
a new value for its @i(delta), as reflected by its Delta
attribute.
@begin(Discussion)
  It might be more direct to make these attributes
  specifiable via an @nt<attribute_definition_clause>, and eliminate the
  syntax for these @ntf<_constraint>s.
@end(Discussion)
@end{Intro}

@begin{Syntax}
@Syn{lhs=<delta_constraint>,
  rhs="@key{delta} @SynI{static_}@Syn2{expression} [@Syn2{range_constraint}]"}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type], Sec=(delta_constraint expression)}
The @nt<expression> of a @nt<delta_constraint> is expected to be of
any real type.
@end{Resolution}

@begin{Legality}
The @nt<expression> of a @nt<delta_constraint> shall be static.

For a @nt<subtype_indication> with a @nt<delta_constraint>, the
@nt<subtype_mark> shall denote an ordinary fixed point subtype.

@Defn{notwithstanding}
For a @nt<subtype_indication> with a @nt<digits_constraint>,
the @nt<subtype_mark> shall denote either a decimal fixed point subtype
or a floating point subtype
(notwithstanding the rule given in @RefSecNum(Fixed Point Types)
that only allows a decimal fixed point subtype).
@begin(Discussion)
  @ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00114-01]}
  @ChgDeleted{Version=[2],Text=[We may need a better way to deal with
  obsolescent features with rules that contradict those of the non-obsolescent
  parts of the standard.]}
@end(Discussion)
@end{Legality}

@begin{StaticSem}
A @nt<subtype_indication> with a @nt<subtype_mark> that
denotes an ordinary fixed point subtype and a @nt<delta_constraint>
defines an ordinary fixed point subtype with a @i(delta)
given by the value of the @nt<expression> of the
@nt<delta_constraint>.
If the @nt<delta_constraint> includes a @nt<range_@!constraint>, then
the ordinary fixed point subtype is constrained by the @nt<range_@!constraint>.

A @nt<subtype_indication> with a @nt<subtype_mark> that
denotes a floating point subtype and a @nt<digits_constraint>
defines a floating point subtype with a requested decimal precision
(as reflected by its Digits attribute)
given by the value of the @nt<expression> of the @nt<digits_constraint>.
If the @nt<digits_constraint> includes a @nt<range_@!constraint>, then
the floating point subtype is constrained by the @nt<range_@!constraint>.
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[compatibility],
  Sec=(delta_constraint with an ordinary fixed point subtype)}
A @nt<delta_constraint> is @i(compatible) with an ordinary
fixed point subtype if the value of the @nt<expression> is no less
than the @i(delta) of the subtype, and the @nt<range_constraint>, if any,
is compatible with the subtype.

@PDefn2{Term=[compatibility],
  Sec=(digits_constraint with a floating point subtype)}
A @nt<digits_constraint> is @i(compatible) with a
floating point subtype if the value of the @nt<expression> is no greater
than the requested decimal precision of the subtype, and
the @nt<range_constraint>, if any,
is compatible with the subtype.

@PDefn2{Term=[elaboration], Sec=(delta_constraint)}
The elaboration of a @nt<delta_constraint> consists of the
elaboration of the @nt<range_constraint>, if any.
@begin{Reason}
A numeric subtype is considered @lquotes@;constrained@rquotes@; only if a range constraint
applies to it. The only effect of a @nt<digits_constraint> or a
@nt<delta_constraint> without a @nt<range_constraint> is to specify
the value of the corresponding Digits or Delta attribute in
the new subtype. The set of values of the subtype is not @lquotes@;constrained@rquotes@;
in any way by such @ntf<_constraint>s.
@end{Reason}
@end{RunTime}

@begin{DiffWord83}
In Ada 83, a @nt<delta_constraint> is called a fixed_point_constraint,
and a @nt<digits_constraint> is called a floating_point_constraint.
We have adopted other terms because @nt<digits_constraint>s apply
primarily to decimal fixed point types now (they apply to
floating point types only as an obsolescent feature).
@end{DiffWord83}

@RMNewPage@Comment{For printed Ada 2007 RM}
@LabeledClause{The Constrained Attribute}

@begin{StaticSem}
@Leading@;For every private subtype S, the following attribute is defined:
@begin{Discussion}
This includes generic formal private subtypes.
@end{Discussion}
@begin{Description}
S'@attr{Constrained}@\Yields the value False if S denotes an unconstrained
              nonformal private subtype with discriminants; also yields the
              value False if S denotes a generic formal private subtype, and
              the associated actual subtype is either an unconstrained subtype
              with discriminants or an unconstrained array subtype; yields
              the value True otherwise. The value of this attribute is of
              the predefined subtype Boolean.
@begin{Reason}
Because Ada 95 has @nt{unknown_discriminant_part}s,
the Constrained attribute of private subtypes is obsolete.
This is fortunate, since its Ada 83 definition was confusing,
as explained below. Because this attribute is obsolete,
we do not bother to extend its definition to private extensions.

The Constrained attribute of an object is @i(not) obsolete.

Note well: S'Constrained matches the Ada 95 definition of @lquotes@;constrained@rquotes@;
only for composite subtypes. For elementary subtypes,
S'Constrained is always true, whether or not S is constrained.
(The Constrained attribute of an object does not have this problem,
as it is only defined for objects of a discriminated type.)
So one should think of its designator as being 'Constrained_Or_Elementary.
@end{Reason}
@end{Description}
@end{StaticSem}

@LabeledClause{ASCII}

@begin{StaticSem}
@Leading@;The following declaration exists in the declaration of package
Standard:
@begin{example}
@key[package] ASCII @key[is]


  --@RI{  Control characters:}


@tabclear()@tabset(P47)
  NUL   : @key[constant] Character := @RI{nul}; @\SOH   : @key[constant] Character := @RI{soh};
  STX   : @key[constant] Character := @RI{stx}; @\ETX   : @key[constant] Character := @RI{etx};
  EOT   : @key[constant] Character := @RI{eot}; @\ENQ   : @key[constant] Character := @RI{enq};
  ACK   : @key[constant] Character := @RI{ack}; @\BEL   : @key[constant] Character := @RI{bel};
  BS    : @key[constant] Character := @RI{bs}; @\HT    : @key[constant] Character := @RI{ht};
  LF    : @key[constant] Character := @RI{lf}; @\VT    : @key[constant] Character := @RI{vt};
  FF    : @key[constant] Character := @RI{ff}; @\CR    : @key[constant] Character := @RI{cr};
  SO    : @key[constant] Character := @RI{so}; @\SI    : @key[constant] Character := @RI{si};
  DLE   : @key[constant] Character := @RI{dle}; @\DC1   : @key[constant] Character := @RI{dc1};
  DC2   : @key[constant] Character := @RI{dc2}; @\DC3   : @key[constant] Character := @RI{dc3};
  DC4   : @key[constant] Character := @RI{dc4}; @\NAK   : @key[constant] Character := @RI{nak};
  SYN   : @key[constant] Character := @RI{syn}; @\ETB   : @key[constant] Character := @RI{etb};
  CAN   : @key[constant] Character := @RI{can}; @\EM    : @key[constant] Character := @RI{em};
  SUB   : @key[constant] Character := @RI{sub}; @\ESC   : @key[constant] Character := @RI{esc};
  FS    : @key[constant] Character := @RI{fs}; @\GS    : @key[constant] Character := @RI{gs};
  RS    : @key[constant] Character := @RI{rs}; @\US    : @key[constant] Character := @RI{us};
  DEL   : @key[constant] Character := @RI{del};


  --@RI{ Other characters:}

  Exclam   : @key[constant] Character:= '!';@\Quotation : @key[constant] Character:= '"';
  Sharp    : @key[constant] Character:= '#';@\Dollar    : @key[constant] Character:= '$';
  Percent  : @key[constant] Character:= '%';@\Ampersand : @key[constant] Character:= '&';
  Colon    : @key[constant] Character:= ':';@\Semicolon : @key[constant] Character:= ';';
  Query    : @key[constant] Character:= '?';@\At_Sign   : @key[constant] Character:= '@@';
  L_Bracket: @key[constant] Character:= '[';@\Back_Slash: @key[constant] Character:= '\';
  R_Bracket: @key[constant] Character:= ']';@\Circumflex: @key[constant] Character:= '^';
  Underline: @key[constant] Character:= '_';@\Grave     : @key[constant] Character:= '`';
  L_Brace  : @key[constant] Character:= '{';@\Bar       : @key[constant] Character:= '|';
  R_Brace  : @key[constant] Character:= '}';@\Tilde     : @key[constant] Character:= '~';

  --@RI{ Lower case letters:}


  LC_A: @key[constant] Character:= 'a';
  ...
  LC_Z: @key[constant] Character:= 'z';


@key[end] ASCII;
@end{example}
@end{StaticSem}


@LabeledClause{Numeric_Error}

@begin{StaticSem}
@Leading@;The following declaration exists in the declaration
of package Standard:
@begin{Example}
Numeric_Error : @key[exception] @key[renames] Constraint_Error;
@end{Example}
@begin{Discussion}
This is true even though it is not shown in
@RefSecNum{The Package Standard}.
@end{Discussion}
@begin{Reason}
In Ada 83, it was unclear which situations should raise
Numeric_Error, and which should raise Constraint_Error.
The permissions of RM83-11.6 could often be used to allow
the implementation to raise Constraint_Error in a situation
where one would normally expect Numeric_Error.
To avoid this confusion, all situations that raise Numeric_Error in
Ada 83 are changed to raise Constraint_Error in Ada 95.
Numeric_Error is changed to be a renaming of Constraint_Error
to avoid most of the upward compatibilities associated with
this change.

In new code, Constraint_Error should be used instead of Numeric_Error.
@end{Reason}
@end{StaticSem}

@LabeledClause{At Clauses}

@begin{Syntax}
@Syn{lhs=<at_clause>,rhs="@key{for} @Syn2{direct_name} @key{use} @key{at} @Syn2{expression};"}
@end{Syntax}

@begin{StaticSem}
An @nt{at_clause} of the form @lquotes@;for @i{x} use at @i{y};@rquotes@; is
equivalent to an @nt{attribute_definition_clause} of the form
@lquotes@;for @i{x}'Address use @i{y};@rquotes@;.
@begin{Reason}
The preferred syntax for specifying the address of an entity is an
@nt{attribute_definition_clause} specifying the Address attribute.
Therefore, the special-purpose @nt{at_clause} syntax is now obsolete.

The above equivalence implies, for example, that only one @nt{at_clause}
is allowed for a given entity.
Similarly, it is illegal to give both
an @nt{at_clause}
and an @nt{attribute_definition_clause} specifying the Address attribute.
@end{Reason}
@end{StaticSem}

@begin{Extend83}
@Defn{extensions to Ada 83}
We now allow to define the address of an entity using an
@nt{attribute_definition_clause}.
This is because Ada 83's @nt{at_clause} is so hard to
remember: programmers often tend to write @lquotes@;for X'Address use...;@rquotes@;.
@end{Extend83}

@begin{DiffWord83}
Ada 83's @ntf{address_clause} is now called an @nt{at_clause} to avoid
confusion with the new term @lquotes@;Address clause@rquotes@; (that is, an
@nt{attribute_definition_clause} for the Address attribute).
@end{DiffWord83}

@LabeledSubClause{Interrupt Entries}

@begin{Intro}
@redundant[Implementations are permitted to allow the attachment of task entries to
interrupts via the address clause. Such an entry is referred to as an
@i{interrupt entry}.

The address of the task entry corresponds to a hardware interrupt in an
implementation-defined manner. (See Ada.Interrupts.Reference in
@RefSecNum{The Package Interrupts}.)]
@end{Intro}

@begin{StaticSem}
@Leading@;The following attribute is defined:

@Leading@;For any task entry X:
@begin{Description}
@Defn{interrupt entry}
X'@attr{Address} @\For a task entry whose address is specified
               (an @i{interrupt entry}), the value
               refers to the corresponding hardware interrupt. For such
               an entry, as for any other task entry, the meaning of this
               value is implementation defined. The value of this attribute
               is of the type of the subtype System.Address.

@NoPrefix@;@PDefn2{Term=[specifiable], Sec=(of Address for entries)}
               Address may be specified for single entries
               via an @nt{attribute_definition_clause}.
@begin{Reason}
Because of the equivalence of @nt{at_clause}s and
@nt{attribute_definition_clause}s, an interrupt entry may be
specified via either notation.
@end{Reason}
@end{Description}
@end{StaticSem}

@begin{RunTime}

@PDefn2{Term=[initialization], Sec=[of a task object]}
As part of the initialization of a task object, the
address clause for an interrupt entry is
elaborated@Redundant[, which evaluates the
@nt<expression> of the address clause].
A check is made that the address specified is
associated with some interrupt to which a task entry may be attached.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
If this check fails, Program_Error is raised.
Otherwise, the interrupt entry
is attached to the interrupt associated with the specified address.

@PDefn2{Term=[finalization], Sec=[of a task object]}
Upon finalization of the task object, the interrupt entry, if any, is
detached from the corresponding interrupt and the default treatment is
restored.

While an interrupt entry is attached to an interrupt,
the interrupt is reserved (see @RefSecNum{Interrupt Support}).

An interrupt delivered to a task entry acts as a call to the entry issued by
a hardware task whose priority is in the System.Interrupt_Priority range.
It is implementation defined whether the call is performed as
an ordinary entry call, a timed entry
call, or a conditional entry call; which kind of call is performed
can depend on the specific interrupt.

@end{RunTime}

@begin{Bounded}
@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to evaluate
E'Caller (see @RefSecNum(The Package Task_Identification))
in an @nt{accept_statement} for an interrupt
entry. The possible effects are
the same as for calling Current_Task from an entry body.
@end{Bounded}

@begin{DocReq}

The implementation shall document to which interrupts a
task entry may be attached.
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The interrupts to which a task entry may be attached.]}]}

The implementation shall document whether the invocation of an interrupt entry
has the effect of an ordinary entry call, conditional call, or a timed call,
and whether the effect varies in the presence of pending interrupts.
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The type of entry call invoked for an interrupt entry.]}]}

@end{DocReq}

@begin{ImplPerm}
The support for this subclause is optional.

Interrupts to which the implementation allows a task entry to be
attached may be designated as reserved for the entire duration
of program execution@Redundant[; that is, not just when they have an
interrupt entry attached to them].

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0077],ARef=[AI95-00111-01]}
Interrupt entry calls may be implemented by having the hardware execute
directly the appropriate @Chg{New=[@nt{accept_statement}],Old=[accept body]}.
Alternatively, the implementation is allowed to provide an internal interrupt
handler to simulate the effect of a normal task calling the entry.

The implementation is allowed to impose restrictions on the specifications
and bodies of tasks that have interrupt entries.

It is implementation defined whether direct calls (from the program) to
interrupt entries are allowed.

If a @nt{select_statement} contains both a @nt{terminate_alternative} and an
@nt{accept_alternative} for an interrupt entry, then an implementation is
allowed to impose further requirements for the selection of the
@nt{terminate_alternative} in addition to those given in
@RefSecNum{Task Dependence - Termination of Tasks}.
@end{ImplPerm}

@begin{Notes}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0077],ARef=[AI95-00111-01]}
Queued interrupts correspond to ordinary entry calls. Interrupts that are
lost if not immediately processed correspond to conditional entry calls. It
is a consequence of the priority rules that an @Chg{New=[@nt{accept_statement}],
Old=[accept body]} executed in response to an interrupt can be executed with
the active priority at which the hardware generates the interrupt, taking
precedence over lower priority tasks, without a scheduling action.

Control information that is supplied upon an interrupt can be passed to an
associated interrupt entry as one or more parameters of mode @key[in].
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of an interrupt entry:}
@begin{example}
@key[task] Interrupt_Handler @key[is]
  @key[entry] Done;
  @key[for] Done'Address @key[use] Ada.Interrupts.Reference(Ada.Interrupts.Names.Device_Done);
@key[end] Interrupt_Handler;

@end{example}
@end{Examples}

@begin{DiffWord83}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
RM83-13.5.1 did not adequately address the problems
@Chg{Version=[2],New=[associated],Old=[associate]} with
interrupts. This feature is now obsolescent and is replaced by the Ada 95
interrupt model as specified in the Systems Programming Annex.
@end{DiffWord83}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0077],ARef=[AI95-00111-01]}
@Chg{Version=[2],New=[@b<Corrigendum:> The undefined term @i{accept body}
was replaced by @nt{accept_statement}.], Old=[]}
@end{DiffWord95}

@RMNewPage@Comment{For printed RM Ada 2007}
@LabeledClause{Mod Clauses}

@begin{Syntax}
@Syn{lhs=<mod_clause>,rhs="@key{at} @key{mod} @SynI{static_}@Syn2{expression};"}
@end{Syntax}

@begin{StaticSem}
@leading@keepnext@;A @nt{record_representation_clause} of the form:
@begin{example}
@key[for] @RI{r} @key[use]
    @key[record] @key[at] @key[mod] @RI{a}
        ...
    @key[end] @key[record];
@end{example}

@leading@keepnext@;is equivalent to:
@begin{example}
@key[for] @RI{r}'Alignment @key[use] @RI{a};
@key[for] @RI{r} @key[use]
    @key[record]
        ...
    @key[end] @key[record];
@end{example}
@begin{Reason}
The preferred syntax for specifying the alignment of an entity is an
@nt{attribute_definition_clause} specifying the Alignment attribute.
Therefore, the special-purpose @nt{mod_clause} syntax is now
obsolete.

The above equivalence implies, for example, that it is illegal to give both
a @nt{mod_clause}
and an @nt{attribute_definition_clause} specifying the Alignment attribute
for the same type.
@end{Reason}
@end{StaticSem}

@begin{DiffWord83}
Ada 83's @ntf{alignment_clause} is now called a @nt{mod_clause} to avoid
confusion with the new term @lquotes@;Alignment clause@rquotes@; (that is, an
@nt{attribute_definition_clause} for the Alignment attribute).
@end{DiffWord83}

@LabeledClause{The Storage_Size Attribute}

@begin{StaticSem}
@Leading@;For any task subtype T, the following attribute is defined:
@begin{Description}
T'@attr{Storage_Size} @\Denotes an implementation-defined value
of type @i{universal_integer}
representing the number of storage
elements reserved for a task of the subtype T.
@begin{Honest}
T'Storage_Size cannot be particularly meaningful in the presence of a
@nt{pragma} Storage_Size, especially when the expression is dynamic, or
depends on a discriminant of the task,
because the Storage_Size will be different for different objects of the type.
Even without such a @nt{pragma},
the Storage_Size can be different for different objects of the type,
and in any case, the value is implementation defined.
Hence, it is always implementation defined.
@end{Honest}

@NoPrefix@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@PDefn2{Term=[specifiable], Sec=(of Storage_Size for a task
first subtype)}
@NoPrefix@;Storage_Size may be specified for a task first subtype
@Chg{Version=[2],New=[that is not an interface ],Old=[]}via
an @nt{attribute_definition_clause}.
@end{Description}
@end{StaticSem}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01]}
  @ChgAdded{Version=[2],Text=[We don't allow specifying Storage_Size on
  task interfaces. We don't need to mention class-wide task types, because
  these cannot be a first subtype.]}
@end{DiffWord95}

@LabeledAddedClause{Version=[2],Name=[Specific Suppression of Checks]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00224-01]}
@ChgAdded{Version=[2],Text=[Pragma Suppress can be used to suppress checks on
specific entities.]}
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00224-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The form of a specific
Suppress @nt{pragma} is as follows:]}
@end{SyntaxText}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],
New=`@ @ @key{pragma} @prag{Suppress}(@Syn2{identifier}, [On =>] @Syn2{name});',
Old=<>}
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00224-01]}
@ChgAdded{Version=[2],Text=[The @nt{identifier} shall be the name of a check
(see @RefSecNum{Suppressing Checks}). The @nt{name} shall
statically denote some entity.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00224-01]}
@ChgAdded{Version=[2],Text=[For a specific Suppress @nt{pragma} that is
immediately within a @nt{package_specification}, the @nt{name} shall denote an
entity (or several overloaded subprograms) declared immediately within the
@nt{package_@!specification}.]}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00224-01]}
@ChgAdded{Version=[2],Text=[A specific Suppress @nt{pragma} applies to the
named check from the place of the @nt{pragma} to the end of the innermost
enclosing declarative region, or, if the @nt{pragma} is given in a
@nt{package_specification}, to the end of the scope of the named entity. The
@nt{pragma} applies only to the named entity, or, for a subtype, on objects and
values of its type. A specific Suppress @nt{pragma} suppresses the named check
for any entities to which it applies (see @RefSecNum{Suppressing Checks}).
Which checks are associated with a specific entity is not defined by this
International Standard.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@begin{Discussion}
@ChgAdded{Version=[2],Text=[The language doesn't specify exactly which entities
control whether a check is performed. For example, in]}
@begin{Example}
@Chg{Version=[2],New=[@key{pragma} Suppress (Range_Check, On => A);
A := B;],Old=[]}
@end{Example}
@ChgAdded{Version=[2],Text=[whether or not the range check is performed is not
specified. The compiler may require that checks are suppressed on B or on the
type of A in order to omit the range check.]}
@end{Discussion}
@end{StaticSem}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00224-01]}
@ChgAdded{Version=[2],Text=[An implementation is allowed to place restrictions on
specific Suppress @nt{pragma}s.]}
@end{ImplPerm}

@begin{Notes}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00224-01]}
@ChgAdded{Version=[2],Text=[An implementation may support a similar On parameter on
@nt{pragma} Unsuppress (see @RefSecNum{Suppressing Checks}).]}
@end{Notes}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00224-01]}
  @ChgAdded{Version=[2],Text=[This clause is new. This feature was moved here
  because it is important for pragma Unsuppress that there be an unambiguous
  meaning for each checking pragma. For instance, in the example]}
@begin{Example}
@ChgAdded{Version=[2],Text=[@key{pragma} Suppress (Range_Check);
@key{pragma} Unsuppress (Range_Check, On => A);
A := B;]}
@end{Example}
  @ChgAdded{Version=[2],Text=[the user needs to be able to depend on the range check
  being made on the assignment. But a compiler survey showed that the
  interpretation of this feature varied widely; trying to define this carefully
  was likely to cause a lot of user and implementer pain. Thus the feature was
  moved here, to emphasize that its use is not portable.]}
@end{DiffWord95}


@LabeledAddedClause{Version=[2],Name=[The Class Attribute of Untagged Incomplete Types]}


@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00326-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For the first subtype S of a type
@i<T> declared by an @nt<incomplete_type_declaration> that is not tagged, the
following attribute is defined:]}
@begin{Description}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00326-01]}
@Chg{Version=[2],New=[S'@attr{Class} @\Denotes the first subtype of the
incomplete class-wide type rooted at @i<T>. The completion of @i<T> shall
declare a tagged type. Such an attribute reference shall occur in the same
library unit as the @nt<incomplete_type_declaration>.],Old=[]}
@end{Description}
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00326-01]}
    @ChgAdded{Version=[2],Text=[This must occur in the same unit to prevent
    children from imposing requirements on their ancestor library
    units for deferred incomplete types.]}
  @end{reason}
@end{StaticSem}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00326-01]}
@Chg{Version=[2],New=[This clause is new. This feature was moved here
because the tagged incomplete type provides a better way to provide this
capability (it doesn't put requirements on the completion based on uses that
could be anywhere). Pity we didn't think of it in 1994.],Old=[]}
@end{DiffWord95}


@LabeledAddedClause{Version=[2],Name=[Pragma Interface]}

@begin{Syntax}
@begin{SyntaxText}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00284-02]}
@ChgAdded{Version=[2],Text=[In addition to an identifier, the reserved word
@key{interface} is allowed as a pragma name, to provide compatibility with a prior
edition of this International Standard.]}
@end{SyntaxText}
@end{Syntax}

@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00284-02]}
@ChgAdded{Version=[2],Text=[All implementations need to at least recognize and
ignore this pragma. A syntax error is not an acceptable implementation of
this pragma.]}
@end{ImplNote}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00326-01]}
  @ChgAdded{Version=[2],Text=[This clause is new. This is necessary as @key{interface}
  is now a reserved word, which would prevent pragma Interface from being an
  implementation-defined pragma. We don't define any semantics for this
  pragma, as we expect that implementations will continue to use whatever they
  currently implement - requiring any changes would be counter-productive.]}
@end{DiffWord95}


@LabeledAddedClause{Version=[2],Name=[Dependence Restriction Identifiers]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00394-01]}
@ChgAdded{Version=[2],Text=[The following restrictions involve dependence
on specific language-defined units. The more general restriction No_Dependence
(see @RefSecNum{Language-Defined Restrictions}) should be used for this purpose.]}
@end{Intro}


@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00394-01]}
@ChgAdded{Version=[2],Type=[Leading],
Text=[The following @Syni<restriction_>@nt<identifier>s exist:]}

@begin{Description}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00394-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[Restrictions],
Sec=(No_Asynchronous_Control)}No_Asynchronous_Control @\Semantic dependence
on the predefined package Asynchronous_Task_Control is not allowed.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00394-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[Restrictions],
Sec=(No_Unchecked_Conversion)}No_Unchecked_Conversion @\Semantic
dependence on the predefined generic function Unchecked_Conversion is not
allowed.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00394-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[Restrictions],
Sec=(No_Unchecked_Deallocation)}No_Unchecked_Deallocation @\Semantic
dependence on the predefined generic procedure Unchecked_Deallocation is
not allowed.]}

@end{Description}
@end{StaticSem}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00394-01]}
  @ChgAdded{Version=[2],Text=[This clause is new. These restrictions
  are replaced by the more general No_Dependence
  (see @RefSecNum{Language-Defined Restrictions}).]}
@end{DiffWord95}

@RMNewPage
@LabeledAddedClause{Version=[2],Name=[Character and Wide_Character Conversion Functions]}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
@ChgAdded{Version=[2],Type=[Leading],
Text=[The following declarations exist in the declaration of package
Ada.Characters.Handling:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} Is_Character (Item : @key{in} Wide_Character) @key{return} Boolean
      @key{renames} Conversions.Is_Character;
   @key{function} Is_String    (Item : @key{in} Wide_String)    @key{return} Boolean
      @key{renames} Conversions.Is_String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} To_Character (Item       : @key{in} Wide_Character;
                         Substitute : @key{in} Character := ' ')
                         @key{return} Character
      @key{renames} Conversions.To_Character;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} To_String    (Item       : @key{in} Wide_String;
                          Substitute : @key{in} Character := ' ')
                          @key{return} String
      @key{renames} Conversions.To_String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} To_Wide_Character (Item : @key{in} Character) @key{return} Wide_Character
      @key{renames} Conversions.To_Wide_Character;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} To_Wide_String    (Item : @key{in} String)    @key{return} Wide_String
      @key{renames} Conversions.To_Wide_String;]}
@end{Example}
@end{StaticSem}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00394-01]}
  @ChgAdded{Version=[2],Text=[This clause is new. These subprograms were
  moved to Characters.Conversions
  (see @RefSecNum{The Package Characters.Conversions}).]}
@end{DiffWord95}

