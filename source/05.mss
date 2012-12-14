@Part(05, Root="ada.mss")

@Comment{$Date: 2012/11/28 23:53:03 $}
@LabeledSection{Statements}

@Comment{$Source: e:\\cvsroot/ARM/Source/05.mss,v $}
@Comment{$Revision: 1.59 $}

@begin{Intro}
@Redundant[A @nt{statement} defines an action to be performed upon
its execution.]

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Redundant[This @Chg{Version=[3],New=[clause],Old=[section]} describes the
general rules applicable to all @nt{statement}s.
Some @nt{statement}s are discussed in later @Chg{Version=[3],New=[clauses],Old=[sections]}:
@nt{Procedure_@!call_@!statement}s and
@Chg{Version=[2],New=[return statements],Old=[@nt{return_@!statement}s]} are
described in @RefSec{Subprograms}.
@nt{Entry_@!call_@!statement}s, @nt{requeue_@!statement}s,
@nt{delay_@!statement}s, @nt{accept_@!statement}s,
@nt{select_@!statement}s, and @nt{abort_@!statement}s are described in
@RefSec{Tasks and Synchronization}.
@nt{Raise_@!statement}s are described in @RefSec{Exceptions},
and @nt{code_@!statement}s in
@RefSecNum{Representation Issues}.
The remaining forms of @nt{statement}s are presented in this
@Chg{Version=[3],New=[clause],Old=[section]}.]
@end{Intro}

@begin{DiffWord83}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
The description of
@Chg{Version=[2],New=[return statements],Old=[@nt{return_@!statement}s]}
has been moved to
@RefSec{Return Statements}, so that it is closer to the
description of subprograms.
@end{DiffWord83}

@LabeledClause{Simple and Compound Statements - Sequences of Statements}

@begin{Intro}
@Redundant[A @nt<statement> is either simple or compound.
A @nt<simple_statement> encloses
no other @nt<statement>. A @nt<compound_statement> can enclose
@nt<simple_statement>s and other @nt<compound_statement>s.]
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0179-1]}
@Syn{lhs=<sequence_of_statements>,rhs="@Syn2{statement} {@Syn2{statement}}@Chg{Version=[3],New=[ {@Syn2{label}}],Old=[]}"}


@Syn{lhs=<statement>,rhs="
   {@Syn2{label}} @Syn2{simple_statement} | {@Syn2{label}} @Syn2{compound_statement}"}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
@Syn{tabs=[P31], lhs=<simple_statement>,rhs="@Syn2{null_statement}
   | @Syn2{assignment_statement}@\| @Syn2{exit_statement}
   | @Syn2{goto_statement}@\| @Syn2{procedure_call_statement}
   | @Chg{Version=[2],New=[@Syn2{simple_return_statement}],Old=[@Syn2{return_statement}]}@\| @Syn2{entry_call_statement}
   | @Syn2{requeue_statement}@\| @Syn2{delay_statement}
   | @Syn2{abort_statement}@\| @Syn2{raise_statement}
   | @Syn2{code_statement}"}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
@Syn{tabs=[P31], lhs=<compound_statement>,rhs="
     @Syn2{if_statement}@\| @Syn2{case_statement}
   | @Syn2{loop_statement}@\| @Syn2{block_statement}@Chg{Version=[2],New=[
   | @Syn2{extended_return_statement}],Old=[]}
   | @Syn2{accept_statement}@\| @Syn2{select_statement}"}

@Syn{lhs=<null_statement>,rhs="@key{null};"}

@Syn{lhs=<label>,rhs="<<@SynI{label_}@Syn2{statement_identifier}>>"}

@Syn{lhs=<statement_identifier>,rhs="@Syn2{direct_name}"}

@begin(SyntaxText)
The @nt<direct_name> of a @nt<statement_identifier> shall
be an @nt<identifier> (not an @nt<operator_symbol>).
@end(SyntaxText)
@end{Syntax}

@begin{Resolution}
The @nt<direct_name> of a @nt<statement_identifier> shall resolve to
denote its corresponding implicit declaration (see below).
@end{Resolution}

@begin{Legality}
Distinct @nt{identifier}s shall be used for all
@nt<statement_identifier>s that
appear in the same body, including
inner @nt{block_statement}s
but excluding inner program units.
@end{Legality}

@begin{StaticSem}
For each @nt<statement_identifier>,
there is an implicit declaration (with the specified @nt<identifier>)
at the end of the @nt{declarative_part} of the
innermost @nt{block_statement} or body that
encloses the @nt{statement_identifier}.
The implicit declarations occur in the same order as the
@nt<statement_identifier>s occur in the source text.
If a usage name denotes such an implicit declaration, the entity it
denotes is the @nt<label>, @nt<loop_statement>,
or @nt<block_statement> with the given @nt<statement_identifier>.
@begin{Reason}
  We talk in terms of individual @nt<statement_identifier>s here
  rather than in terms of the corresponding statements, since
  a given @nt{statement} may have multiple @nt<statement_identifier>s.

  A @nt{block_statement} that has no
  explicit @nt{declarative_part} has an implicit empty
  @nt{declarative_part},
  so this rule can safely
  refer to the @nt{declarative_part} of a @nt<block_statement>.

  The scope of a declaration starts at the place of the declaration
  itself (see @RefSecNum{Scope of Declarations}).
  In the case of a label, loop, or block name, it
  follows from this rule that the scope of the implicit declaration
  starts before the first explicit occurrence of the corresponding
  name, since this occurrence is either in a statement label, a
  @nt{loop_statement}, a @nt{block_statement}, or a
  @nt{goto_statement}. An implicit
  declaration in a @nt{block_statement} may hide a declaration given in an
  outer program unit or @nt{block_statement} (according to the usual rules
  of hiding explained in @RefSecNum{Visibility}).

  The syntax rule for @nt{label} uses @nt{statement_identifier} which
  is a @nt<direct_name> (not a @nt{defining_identifier}),
  because labels are implicitly declared. The same applies to loop and
  block names.
  In other words, the @nt{label} itself is not the defining occurrence;
  the implicit declaration is.

  @Leading@;We cannot consider the @nt{label} to be a defining occurrence.
  An example that can tell the difference is this:
  @begin{example}
@key[declare]
    --@RI{ Label Foo is implicitly declared here.}
@key[begin]
    @key[for] Foo @key[in] ... @key[loop]
        ...
        <<Foo>> --@RI{ Illegal.}
        ...
    @key[end] @key[loop];
@key[end];
  @end{example}

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
  The label in this example is hidden from itself by the loop parameter
  with the same name;
  the example is illegal.
  We considered creating a new syntactic category name, separate from
  @nt{direct_name} and @nt{selector_name}, for use in the case of statement
  labels.
  However, that would confuse the rules in @Chg{Version=[3],New=[Clause],Old=[Section]}
  8, so we didn't do it.
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0179-1]}
@ChgAdded{Version=[3],Text=[If one or more @nt{label}s end a
@nt{sequence_of_statements}, an implicit @nt{null_statement}
follows the @nt{label}s before any following constructs.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[The semantics of a @nt{goto_statement} is
  defined in terms of the statement having (following) that label. Thus
  we ensure that every label has a following statement, which might be
  implicit.]}
@end{Reason}
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[execution], Sec=(null_statement)}
The execution of a @nt{null_statement} has no effect.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
@Defn{transfer of control}
A @i{transfer of control} is
the run-time action of an @nt{exit_statement},
@Chg{Version=[2],New=[return statement],Old=[@nt{return_statement}]},
@nt{goto_statement},
or @nt{requeue_statement},
selection of a @nt{terminate_alternative},
raising of an exception,
or an abort,
which causes
the next action performed to be one other than what would normally be
expected from the other rules of the language.
@Redundant[As explained in
@RefSecNum{Completion and Finalization},
a transfer of control can cause the execution of constructs to be
completed and then left,
which may trigger finalization.]

@PDefn2{Term=[execution], Sec=(sequence_of_statements)}
The execution of a @nt{sequence_of_statements} consists of the execution
of the individual @nt{statement}s in succession
until the @ntf{sequence_} is completed.
@begin{Ramification}
It could be completed by reaching the end of it,
or by a transfer of control.
@end{Ramification}
@end{RunTime}

@begin{Notes}
A @nt<statement_identifier> that appears immediately within
the declarative region of a
named @nt<loop_statement> or an @nt<accept_statement> is nevertheless
implicitly declared immediately within the declarative region
of the innermost enclosing body or @nt<block_statement>;
in other words, the expanded name for a named statement is
not affected by whether the statement occurs inside or outside
a named loop or an @nt<accept_statement> @em only nesting
within @nt<block_statement>s is relevant to the form of its
expanded name.
@begin{Discussion}
@Leading@keepnext@;Each comment in the following example gives the
expanded name associated with an entity declared in the task body:
@begin{Example}
@key(task body) Compute @key(is)
   Sum : Integer := 0;                       --@RI[ Compute.Sum]
@key(begin)
 Outer:                                      --@RI[ Compute.Outer]
   @key(for) I @key(in) 1..10 @key(loop)     --@RI[ Compute.Outer.I]
    Blk:                                     --@RI[ Compute.Blk]
      @key(declare)
         Sum : Integer := 0;                 --@RI[ Compute.Blk.Sum]
      @key(begin)
         @key(accept) Ent(I : out Integer; J : in Integer) @key(do)
                                             --@RI[ Compute.Ent.I, Compute.Ent.J]
            Compute.Ent.I := Compute.Outer.I;
          Inner:                             --@RI[ Compute.Blk.Inner]
            @key(for) J @key(in) 1..10 @key(loop)
                                             --@RI[ Compute.Blk.Inner.J]
               Sum := Sum + Compute.Blk.Inner.J * Compute.Ent.J;
            @key(end loop) Inner;
         @key(end) Ent;
         Compute.Sum := Compute.Sum + Compute.Blk.Sum;
      @key(end) Blk;
   @key(end loop) Outer;
   Record_Result(Sum);
@key(end) Compute;
@end{Example}
@end{Discussion}
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Examples of labeled statements:}
@begin{Example}
<<Here>> <<Ici>> <<Aqui>> <<Hier>> @key[null];

<<After>> X := 1;
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The @nt{requeue_statement} is new.
@end{Extend83}

@begin{DiffWord83}
We define the syntactic category @nt<statement_identifier> to simplify
the description. It is used for labels, loop names, and block names.
We define the entity associated with the implicit declarations
of statement names.

Completion includes completion caused by a transfer of control,
although RM83-5.1(6) did not take this view.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00318-02]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The @nt{extended_return_statement} is new (@nt{simple_return_statement}
  is merely renamed).]}
@end{Extend95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI95-0179-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  A @nt{label} can end a @nt{sequence_of_statements},
  eliminating the requirement for having an explicit @key[null]; statement
  after an ending label (a common use).]}
@end{Extend2005}


@LabeledClause{Assignment Statements}

@begin{Intro}
@Redundant[An @nt{assignment_statement}
replaces the current value of
a variable with the result of evaluating an
@nt<expression>.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<assignment_statement>,rhs="
   @SynI{variable_}@Syn2{name} := @Syn2{expression};"}
@end{Syntax}

@begin{Intro}
The execution of an @nt<assignment_statement> includes
the evaluation of the @nt<expression> and the @i(assignment)
of the value of the @nt<expression> into the @i(target).
@RootDefn{assignment operation}
@IndexSee{Term=[assign], See=(assignment operation)}
@Redundant[An assignment operation (as opposed to
an @nt<assignment_@!statement>) is performed in other contexts
as well, including object initialization and by-copy parameter
passing.]
@Defn2{Term=[target], Sec=(of an assignment operation)}
@Defn2{Term=[target], Sec=(of an @nt{assignment_statement})}
The @i{target} of an assignment operation
is the view of the object to which a value is being assigned;
the target of an @nt{assignment_@!statement} is the variable denoted by
the @SynI{variable_}@nt{name}.
@begin{Discussion}
Don't confuse this notion of the @lquotes@;target@rquotes@; of an assignment
with the notion of the @lquotes@;target object@rquotes@; of an entry call or requeue.

Don't confuse the term @lquotes@;assignment operation@rquotes@; with the
@nt{assignment_statement}.
The assignment operation is just one part of the execution of an
@nt{assignment_statement}.
The assignment operation is also a part of the execution of various
other constructs; see @RefSec{Completion and Finalization} for a complete
list.
Note that when we say, @lquotes@;such-and-such is assigned to so-and-so@rquotes@;,
we mean that the assignment operation is being applied, and that
so-and-so is the target of the assignment operation.
@end{Discussion}
@end{Intro}

@begin{Resolution}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
@PDefn2{Term=[expected type],
  Sec=(assignment_statement variable_name)}
The @i(variable_)@nt<name> of an @nt<assignment_statement>
is expected to be of any @Chg{Version=[2],New=[],Old=[nonlimited ]}type.
@PDefn2{Term=[expected type],
  Sec=(assignment_statement expression)}
The expected type for the @nt<expression> is
the type of the target.
@begin{ImplNote}
@Leading@keepnext@;An @nt<assignment_statement> as a whole is a "complete context,"
so if the @i{variable_}@nt<name> of an @nt<assignment_statement> is
overloaded, the @nt<expression> can be used to help disambiguate it.
For example:
@begin{Example}
  @key[type] P1 @key[is access] R1;
  @key[type] P2 @key[is access] R2;

  @key[function] F return P1;
  @key[function] F return P2;

  X : R1;
@key[begin]
  F.all := X;  --@RI[ Right hand side helps resolve left hand side]
@end{Example}
@end{ImplNote}
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
The target @Redundant[denoted by the
@i(variable_)@nt<name>] shall be a variable@Chg{Version=[2],New=[ of a
nonlimited type],Old=[]}.

If the target is of a tagged class-wide type @i(T)'Class, then
the @nt<expression> shall either be dynamically tagged,
or of type @i(T) and tag-indeterminate
(see @RefSecNum{Dispatching Operations of Tagged Types}).
@begin{Reason}
  This is consistent with the general rule that a single
  dispatching operation shall not have both dynamically tagged and
  statically tagged operands. Note that for an object
  initialization (as opposed to the @nt{assignment_statement}),
  a statically tagged initialization expression is permitted,
  since there is no chance for confusion (or Tag_Check failure).
  Also, in an object initialization, tag-indeterminate expressions
  of any type covered by @i(T)'Class would be allowed, but with
  an @nt{assignment_statement}, that might not work if the tag of the target
  was for a type that didn't have one of the dispatching operations
  in the tag-indeterminate expression.
@end{Reason}
@end{Legality}

@begin{RunTime}
@PDefn2{Term=[execution], Sec=(assignment_statement)}
For the execution of an @nt{assignment_statement},
the @i(variable_)@nt<name> and the @nt<expression>
are first evaluated in an arbitrary order.@PDefn2{Term=[arbitrary order],Sec=[allowed]}
@begin{Ramification}
  Other rules of the language may require that the
  bounds of the variable be determined prior to evaluating
  the @nt<expression>, but that does not necessarily require
  evaluation of the @i(variable_)@nt<name>, as pointed out by the ACID.
@end{Ramification}

@Leading@keepnext@;When the type of the target is class-wide:
@begin(itemize)
  @PDefn2{Term=[controlling tag value], Sec=(for the @nt{expression} in an @nt{assignment_statement})}
  If the @nt<expression> is tag-indeterminate
  (see @RefSecNum{Dispatching Operations of Tagged Types}), then the controlling
  tag value for the @nt<expression> is the tag of the target;
@begin{Ramification}
    See @RefSec(Dispatching Operations of Tagged Types).
@end{Ramification}

  @IndexCheck{Tag_Check}
  @Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
  Otherwise @Redundant[(the @nt<expression> is dynamically tagged)],
  a check is made that the tag of
  the value of the @nt<expression>
  is the same as that of the target;
  if this check fails, Constraint_Error is raised.
@end(itemize)

The value of the @nt<expression> is converted to the subtype of the
target. @Redundant[The conversion might raise an exception
(see @RefSecNum{Type Conversions}).]
@PDefn2{Term=[implicit subtype conversion],Sec=(assignment_statement)}
@begin{Ramification}
  @RefSec(Type Conversions) defines what actions
  and checks are associated with subtype conversion.
  For non-array subtypes, it is just a constraint
  check presuming the types match.
  For array subtypes, it checks the lengths and slides if the
  target is constrained.
  @lquotes@;Sliding@rquotes@; means the array doesn't have to have the same bounds,
  so long as it is the same length.
@end{Ramification}

In cases involving controlled types, the target is finalized,
and an anonymous object might be used as an intermediate in the assignment,
as described in @RefSec{Completion and Finalization}.
@Defn{assignment operation}
@Defn2{Term=[assignment operation],
Sec=(during execution of an @nt{assignment_statement})}
In any case,
the converted value of the @nt<expression> is then @i(assigned) to the target,
which consists of the following two steps:
@begin{Honest}
@RefSecNum{Completion and Finalization} actually says that
finalization happens always, but unless controlled types are involved,
this finalization during an @nt{assignment_statement} does
nothing.
@end{Honest}
@begin(itemize)
  The value of the target becomes the converted value.

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
  If any part of the target is controlled, its value
  is adjusted as explained in @Chg{Version=[3],New=[subclause],Old=[clause]}
  @RefSecNum{Assignment and Finalization}.
@PDefn2{Term=[adjustment], Sec=(as part of assignment)}
@begin{Ramification}
    If any parts of the object are controlled,
    abort is deferred during the assignment operation itself,
    but not during the rest of the execution of an
    @nt<assignment_statement>.
@end{Ramification}
@end(itemize)

@end{RunTime}

@begin{Notes}
The tag of an object never changes;
in particular, an
@nt{assignment_statement}
does not change the tag of the target.

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00363-01]}
@ChgDeleted{Version=[2],Text=[The values of the discriminants of an object
designated by an access value cannot be changed (not even by assigning a
complete value to the object itself) since such objects are always constrained;
however, subcomponents of such objects may be unconstrained.]}
@begin{Ramification}
The implicit subtype conversion described above for
@nt{assignment_statement}s
is performed only for the value of the right-hand side
expression as a whole; it is not performed for subcomponents of the
value.

The determination of the type of the variable of an
@nt{assignment_statement} may require consideration of the expression
if the variable
name can be interpreted as the name of a variable designated by the
access value returned by a function call, and similarly, as a
component or slice of such a variable
(see @RefSec{The Context of Overload Resolution}).
@end{Ramification}
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Examples of assignment statements:}
@begin{Example}
Value := Max_Value - 1;
Shade := Blue;

Next_Frame(F)(M, N) := 2.5;        --@RI{  see @RefSecNum{Indexed Components}}
U := Dot_Product(V, W);            --@RI{  see @RefSecNum{Subprogram Bodies}}

Writer := (Status => Open, Unit => Printer, Line_Count => 60);  --@RI{ see @RefSecNum{Variant Parts and Discrete Choices}}
Next_Car.@key[all] := (72074, @key[null]);    --@RI{  see @RefSecNum{Incomplete Type Declarations}}
@end{Example}

@begin{WideAbove}
@Leading@keepnext@i{Examples involving scalar subtype conversions:}
@end{WideAbove}
@begin{Example}
I, J : Integer @key[range] 1 .. 10 := 5;
K    : Integer @key[range] 1 .. 20 := 15;
 ...

I := J;  --@RI{  identical ranges}
K := J;  --@RI{  compatible ranges}
J := K;  --@RI{  will raise Constraint_Error if K > 10}
@end{Example}

@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@begin{WideAbove}
@leading@keepnext@i{Examples involving array subtype conversions:}
@end{WideAbove}
@begin{Example}
A : String(1 .. 31);
B : String(3 .. 33);
 ...

A := B;  --@RI{  same number of components}

A(1 .. 9)  := "tar sauce";
A(4 .. 12) := A(1 .. 9);  --@RI{  A(1 .. 12) = "tartar sauce"}
@end{Example}
@end{Examples}

@begin{Notes}
@i{Notes on the examples:}
@nt{Assignment_statement}s are allowed even in the case of overlapping
slices of the same array,
because the @SynI{variable_}@nt{name} and @nt{expression}
are both evaluated before copying the value into the variable.
In the above example, an
implementation yielding A(1 .. 12) = "tartartartar" would be
incorrect.
@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
We now allow user-defined finalization and value adjustment actions
as part of @nt{assignment_statement}s
(see @RefSec{Assignment and Finalization}).
@end{Extend83}

@begin{DiffWord83}
The special case of array assignment is subsumed by the concept
of a subtype conversion, which is applied for all kinds of types,
not just arrays. For arrays it provides @lquotes@;sliding@rquotes@;. For numeric
types it provides conversion of a value of a universal type to
the specific type of the target. For other types,
it generally has no run-time effect, other than a constraint
check.

We now cover in a general way in @RefSecNum{Operations of Discriminated Types}
the erroneous execution possible due to
changing the value of a discriminant when
the variable in an @nt<assignment_statement> is a subcomponent
that depends on discriminants.
@end{DiffWord83}

@begin{Incompatible95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
@ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
The change of the limited check from a resolution rule to
a legality rule is not quite upward compatible. For example]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{type} AccNonLim @key{is} @key{access} NonLim;
@key{function} Foo (Arg : in Integer) @key{return} AccNonLim;
@key{type} AccLim @key{is} @key{access} Lim;
@key{function} Foo (Arg : in Integer) @key{return} AccLim;
Foo(2).@key{all} := Foo(1).@key{all};]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[where NonLim is a nonlimited type and Lim is a
limited type. The assignment is legal in Ada 95 (only the first Foo would be
considered), and is ambiguous in Ada 2005. We made the change because we want
limited types to be as similar to nonlimited types as possible. Limited
expressions are now allowed in all other contexts (with a similar
incompatibility), and it would be odd if assignments had different resolution
rules (which would eliminate ambiguities in some cases). Moreover, examples
like this one are rare, as they depend on assigning into overloaded function
calls.]}
@end{Incompatible95}


@LabeledClause{If Statements}

@begin{Intro}
@Redundant[An @nt{if_statement} selects for execution at most one of
the enclosed @ntf{sequences_of_statements}, depending on the (truth)
value of one or more corresponding @nt{condition}s.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<if_statement>,rhs="
    @key{if} @Syn2{condition} @key{then}
      @Syn2{sequence_of_statements}
   {@key{elsif} @Syn2{condition} @key{then}
      @Syn2{sequence_of_statements}}
   [@key{else}
      @Syn2{sequence_of_statements}]
    @key{end} @key{if};"}


@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0147-1]}
@DeletedSyn{Version=[3],LHS=<@Chg{Version=[3],New=[],Old=[condition]}>,
RHS="@Chg{Version=[3],New=[],Old=[@SynI{boolean_}@Syn2{expression}]}"}
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0147-1]}
@ChgDeleted{Version=[3],Text=[@PDefn2{Term=[expected type], Sec=(condition)}
A @nt{condition} is expected to be of any boolean type.]}
@end{Resolution}
@begin{NotIso}
@ChgAdded{Version=[3],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 3 and 4
were deleted.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@begin{RunTime}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@PDefn2{Term=[execution], Sec=(if_statement)}
For the execution of an @nt{if_statement}, the @nt{condition} specified
after @key{if}, and any @nt{condition}s specified after @key{elsif}, are
evaluated in succession (treating a final @key{else} as @key{elsif} True
@key{then}), until one evaluates to True or
all @nt{condition}s are evaluated and
yield False.
If a @nt{condition} evaluates to True, then the
corresponding @nt{sequence_of_statements} is executed;
otherwise@Chg{Version=[3],New=[,],Old=[]}
none of them is executed.
@begin{Ramification}
  The part about all evaluating to False can't happen if
  there is an @key{else}, since that is herein considered equivalent to
  @key{elsif} True @key{then}.
@end{Ramification}
@end{RunTime}

@begin{Examples}
@Leading@keepnext@i{Examples of if statements:}
@begin{Example}
@key[if] Month = December @key[and] Day = 31 @key[then]
   Month := January;
   Day   := 1;
   Year  := Year + 1;
@key[end] @key[if];

@key[if] Line_Too_Short @key[then]
   @key[raise] Layout_Error;
@key[elsif] Line_Full @key[then]
   New_Line;
   Put(Item);
@key[else]
   Put(Item);
@key[end] @key[if];

@key[if] My_Car.Owner.Vehicle /= My_Car @key[then]            --@RI{  see @RefSecNum{Incomplete Type Declarations}}
   Report ("Incorrect data");
@key[end] @key[if];
@end{Example}
@end{Examples}

@begin{Diffword2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1]}
  @ChgAdded{Version=[3],Text=[Moved definition of @nt{condition} to
  @RefSecNum{Conditional Expressions} in order to eliminate a forward reference.]}
@end{Diffword2005}


@LabeledClause{Case Statements}

@begin{Intro}
@Redundant[A @nt{case_statement} selects for execution one of a
number of alternative @ntf{sequences_of_statements}; the chosen
alternative is defined by the value of an expression.]
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0188-1]}
@Syn{lhs=<case_statement>,rhs="
   @key{case} @Chg{Version=[3],New=[@SynI{selecting_}],Old=[]}@Syn2{expression} @key{is}
       @Syn2{case_statement_alternative}
      {@Syn2{case_statement_alternative}}
   @key{end} @key{case};"}


@Syn{lhs=<case_statement_alternative>,rhs="
   @key{when} @Syn2{discrete_choice_list} =>
      @Syn2{sequence_of_statements}"}
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0188-1]}
@Chg{Version=[3],New=[@PDefn2{Term=[expected type], Sec=(case_statement selecting_expression)}
@PDefn2{Term=[expected type], Sec=(selecting_expression case_statement)}],Old=[@PDefn2{Term=[expected type], Sec=(case expression)}]}
The @Chg{Version=[3],New=[@SynI{selecting_}],Old=[]}@nt{expression} is
expected to be of any discrete type.
@PDefn2{Term=[expected type],
  Sec=(case_statement_alternative discrete_choice)}
The expected type for each @nt{discrete_choice} is the type of the
@Chg{Version=[3],New=[@SynI{selecting_}],Old=[]}@nt{expression}.
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0153-3]}
The @Chg{Version=[3],New=[@nt{choice_expression}s,
@nt{subtype_indication}s,],Old=[@nt{expression}s]} and
@Chg{Version=[3],New=[@nt{range}s],Old=[@nt{discrete_range}s]} given as
@nt{discrete_choice}s of a @nt{case_statement} shall be static.
@Redundant[A @nt{discrete_choice} @key(others), if present,
shall appear alone and in the last @nt{discrete_choice_list}.]

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0188-1],ARef=[AI05-0240-1]}
The possible values of the
@Chg{Version=[3],New=[@SynI{selecting_}],Old=[]}@nt{expression} shall be
covered @Chg{Version=[3],New=[(see @RefSecNum{Variant Parts and Discrete Choices}) ],
Old=[]}as follows:
  @begin{Discussion}
    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0240-1]}
    @ChgAdded{Version=[3],Text=[The meaning of "covered" here and in the
    following rules is that of the term "cover a value" that is defined in
    @RefSecNum{Variant Parts and Discrete Choices}.]}
  @end{Discussion}
@begin{itemize}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0003-1],ARef=[AI05-0153-3],ARef=[AI05-0188-1],ARef=[AI05-0262-1]}
  If the @Chg{Version=[3],New=[@SynI{selecting_}],Old=[]}@nt{expression} is a @nt{name}
  @Redundant[(including a
  @nt<type_conversion>@Chg{Version=[3],New=[, @nt{qualified_expression},],Old=[]}
  or @Chg{Version=[3],New=[],Old=[a ]}@nt<function_call>)] having
  a static and constrained nominal subtype,@Chg{Version=[3],New=[],Old=[ or
  is a @nt{qualified_expression} whose
  @nt{subtype_mark} denotes a static and constrained
  scalar subtype,]}
  then each non-@key{others} @nt{discrete_choice} shall cover only values in
  that subtype@Chg{Version=[3],New=[ that satisfy its
  predicate (see @RefSecNum{Subtype Predicates})],Old=[]},
  and each value of that subtype@Chg{Version=[3],New=[ that satisfies its
  predicate],Old=[]} shall be covered by some @nt{discrete_choice}
  @Redundant[(either explicitly or by @key(others))].
  @begin{Ramification}
    Although not official @nt<name>s of objects, a value conversion
    still has a defined nominal subtype, namely its target subtype.
    See @RefSecNum{Type Conversions}.
  @end{Ramification}

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0188-1]}
  If the type of the @Chg{Version=[3],New=[@SynI{selecting_}],Old=[]}@nt{expression} is
  @i(root_integer), @i(universal_integer), or a descendant of a
  formal scalar type,
  then the @nt{case_statement} shall have an @key{others}
  @nt{discrete_choice}.
@begin{Reason}
  This is because the base range is
  implementation defined for @i(root_integer) and @i(universal_integer),
  and not known statically in the case of a formal scalar type.
@end{Reason}

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0188-1]}
  Otherwise, each value of the base range of the type of the
  @Chg{Version=[3],New=[@SynI{selecting_}],Old=[]}@nt{expression} shall
  be covered
  @Redundant[(either explicitly or by @key(others))].
@end{itemize}

Two distinct @nt{discrete_choice}s of a
@nt{case_statement} shall not cover the same value.
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0188-1]}
The goal of these coverage rules is
that any possible value of the @Chg{Version=[3],New=[@SynI{selecting_}],Old=[]}@nt{expression} of a
@nt{case_statement} should be covered by exactly one
@nt{discrete_choice} of the
@nt{case_statement}, and that this should be checked at compile time.
The goal is achieved in most cases, but there are two minor
loopholes:
@begin{Itemize}
If the expression reads an object with an invalid representation
(e.g. an uninitialized object),
then the value can be outside the covered range.
This can happen for static constrained subtypes, as well as nonstatic or
unconstrained subtypes.
It cannot, however, happen if the @nt{case_statement} has the
@nt{discrete_choice} @key{others}, because @key{others} covers all values,
even those outside the subtype.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0188-1]}
If the compiler chooses to represent the value of an expression of an
unconstrained subtype in a way that includes values outside the bounds of the
subtype, then those values can be outside the covered range.
For example, if X: Integer := Integer'Last;, and the case @Chg{Version=[3],New=[@SynI{selecting_}],Old=[]}@nt{expression} is
X+1, then the implementation might choose to produce the correct value, which
is outside the bounds of Integer.
(It might raise Constraint_Error instead.)
This case can only happen for nongeneric subtypes that are either
unconstrained or
non@Chg{Version=[2],New=[],Old=[-]}@ChgNote{Make spelling consistent}static
(or both).
It can only happen if there is no @key{others} @nt{discrete_choice}.
@end{Itemize}

In the uninitialized variable case, the value might be anything; hence, any
alternative can be chosen, or Constraint_Error can be raised. (We intend to
prevent, however, jumping to random memory locations and the like.)
In the out-of-range case, the behavior is more sensible: if there is an
@key{others}, then the implementation may choose to raise Constraint_Error
on the evaluation of the @nt{expression} (as usual), or it may choose
to correctly evaluate the @nt{expression} and therefore choose the
@key{others} alternative.
Otherwise (no @key{others}), Constraint_Error is raised either way @em on
the @nt{expression} evaluation, or for the @nt{case_statement} itself.

For an enumeration type with a discontiguous set of internal codes
(see @RefSecNum{Enumeration Representation Clauses}),
the only way to get values in between the proper values
is via an object with an invalid representation;
there is no @lquotes@;out-of-range@rquotes@; situation that can produce them.
@end{Ramification}
@end{Legality}

@begin{RunTime}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0188-1]}
@PDefn2{Term=[execution], Sec=(case_statement)}
For the execution of a @nt{case_statement} the
@Chg{Version=[3],New=[@SynI{selecting_}],Old=[]}@nt{expression}
is first evaluated.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0188-1]}
If the value of the @Chg{Version=[3],New=[@SynI{selecting_}],Old=[]}@nt{expression}
is covered by the @nt{discrete_@!choice_@!list} of some
@nt{case_@!statement_@!alternative}, then the
@nt{sequence_of_@!statements} of the @ntf{_alternative} is executed.

@IndexCheck{Overflow_Check}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
Otherwise (the value is not covered by any
@nt{discrete_choice_list},
perhaps due to being outside the base range),
Constraint_Error is raised.
@begin{Ramification}

In this case, the value is outside the base range of its type,
or is an invalid representation.

@end{Ramification}
@end{RunTime}

@begin{Notes}
The execution of a @nt{case_statement} chooses one and only one
alternative.
Qualification of the expression of a @nt{case_statement} by a static
subtype can often be used to limit the number of choices that need be
given explicitly.
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Examples of case statements:}
@begin{Example}
@tabclear()@tabset(P22)
@key[case] Sensor @key[is]
   @key[when] Elevation@\=> Record_Elevation(Sensor_Value);
   @key[when] Azimuth@\=> Record_Azimuth  (Sensor_Value);
   @key[when] Distance@\=> Record_Distance (Sensor_Value);
   @key[when] @key[others]@\=> @key[null];
@key[end] @key[case];

@tabclear()@tabset(P22)
@key[case] Today @key[is]
   @key[when] Mon@\=> Compute_Initial_Balance;
   @key[when] Fri@\=> Compute_Closing_Balance;
   @key[when] Tue .. Thu@\=> Generate_Report(Today);
   @key[when] Sat .. Sun@\=> @key[null];
@key[end] @key[case];

@tabclear()@tabset(P16)
@key[case] Bin_Number(Count) @key[is]
   @key[when] 1@\=> Update_Bin(1);
   @key[when] 2@\=> Update_Bin(2);
   @key[when] 3 | 4@\=>
      Empty_Bin(1);
      Empty_Bin(2);
   @key[when] @key[others]@\=> @key[raise] Error;
@key[end] @key[case];
@end{Example}
@end{Examples}

@begin{Incompatible83}
@ChgRef{Version=[1],Kind=[Added]}@ChgNote{Presentation AI-00020}
@Chg{New=[@Defn{incompatibilities with Ada 83}
In Ada 95, @nt{function_call}s and @nt{type_conversion}s are @nt{name}s, whereas
in Ada 83, they were @nt{expression}s. Therefore, if the @nt{expression} of a
@nt{case_statement} is a @nt{function_call} or @nt{type_conversion}, and the
result subtype is static, it is illegal to specify a choice outside the bounds
of the subtype. For this case in Ada 83 choices only are required to be in the
base range of the type.],Old=[]}

@ChgRef{Version=[1],Kind=[Added]}@ChgNote{Presentation AI-00020}
@Chg{New=[In addition, the rule about which choices must be covered is
unchanged in Ada 95. Therefore, for a @nt{case_statement} whose @nt{expression}
is a @nt{function_call} or @nt{type_conversion}, Ada 83 required covering all
choices in the base range, while Ada 95 only requires covering choices in the
bounds of the subtype. If the @nt{case_statement} does not include an @key{others}
@nt{discrete_choice}, then a legal Ada 83 @nt{case_statement} will be illegal
in Ada 95 if the bounds of the subtype are different than the bounds of the
base type.],Old=[]}
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
In Ada 83, the @nt{expression} in a @nt{case_statement} is not allowed to
be of a generic formal type.
This restriction is removed in Ada 95; an @key{others} @nt{discrete_choice}
is required instead.

In Ada 95, a function call is the name of an object;
this was not true in Ada 83 (see @RefSec{Names}).
This change makes the following @nt{case_statement} legal:
@begin{Example}
@key[subtype] S @key[is] Integer @key[range] 1..2;
@key[function] F @key[return] S;
@key[case] F @key[is]
   @key[when] 1 => ...;
   @key[when] 2 => ...;
   --@RI{ No @key{others} needed.}
@key[end] @key[case];
@end{Example}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
Note that the result subtype given in a function
@nt{renaming_declaration} is ignored;
for a @nt{case_statement} whose expression calls a such a function, the
full coverage rules are checked using the result subtype of the original
function.
Note that predefined operators such as "+" have an unconstrained result
subtype (see @RefSecNum{Logical Operators and Short-circuit Control Forms}).
Note that generic formal functions do not have static result subtypes.
Note that the result subtype of an inherited subprogram need not
correspond to any @Chg{Version=[3],New=[nameable],Old=[namable]} subtype;
there is still a perfectly good result subtype, though.
@end{Extend83}

@begin{DiffWord83}
Ada 83 forgot to say what happens for @lquotes@;legally@rquotes@; out-of-bounds values.

We take advantage of rules and terms (e.g. @i(cover a value))
defined for @nt{discrete_choice}s and @nt{discrete_choice_list}s
in @RefSec{Variant Parts and Discrete Choices}.

In the @ResolutionName for the case expression,
we no longer need RM83-5.4(3)'s @lquotes@;which must be determinable
independently of the context in which the expression occurs, but
using the fact that the expression must be of a discrete type,@rquotes@;
because the @nt{expression} is now a complete context.
See @RefSec{The Context of Overload Resolution}.

Since @nt<type_conversion>s are now defined as @nt<name>s,
their coverage rule is now covered under the general rule
for @nt<name>s, rather than being separated out along with
@nt<qualified_expression>s.
@end{DiffWord83}


@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0003-1]}
  @ChgAdded{Version=[3],Text=[Rewording to reflect that
  a @nt{qualified_expression} is now a @nt{name}.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3]}
  @ChgAdded{Version=[3],Text=[Revised for changes to @nt{discrete_choice}s
  made to allow static predicates (see @RefSecNum{Subtype Predicates}) as
  case choices (see @RefSecNum{Variant Parts and Discrete Choices}).]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0188-1]}
  @ChgAdded{Version=[3],Text=[Added the @SynI{selecting_} prefix to
  make this wording consistent with @nt{case_expression}, and to clarify
  which @nt{expression} is being talked about in the wording.]}
@end{Diffword2005}


@LabeledClause{Loop Statements}

@begin{Intro}
@Redundant[A @nt{loop_statement} includes a
@nt{sequence_of_statements} that is to be executed repeatedly,
zero or more times.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<loop_statement>,rhs="
   [@SynI{loop_}@Syn2{statement_identifier}:]
      [@Syn2{iteration_scheme}] @key{loop}
         @Syn2{sequence_of_statements}
       @key{end} @key{loop} [@SynI{loop_}@Syn2{identifier}];"}


@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0139-2]}
@Syn{lhs=<iteration_scheme>,rhs="@key{while} @Syn2{condition}
   | @key{for} @Syn2{loop_parameter_specification}@Chg{Version=[3],New=[
   | @key{for} @Syn2{iterator_specification}],Old=[]}"}

@Syn{lhs=<loop_parameter_specification>,rhs="
   @Syn2{defining_identifier} @key{in} [@key{reverse}] @Syn2{discrete_subtype_definition}"}

@begin(SyntaxText)
If a @nt{loop_statement} has a @SynI{loop_}@nt{statement_identifier},
then the @nt<identifier> shall be repeated after the @key{end loop};
otherwise, there shall not be an @nt<identifier> after the @key{end loop}.
@end(SyntaxText)
@end{Syntax}

@begin{StaticSem}
@Defn{loop parameter}
A @nt{loop_parameter_specification} declares a @i{loop parameter},
which is an object whose
subtype is that defined by the @nt{discrete_subtype_definition}.
@IndexSeeAlso{Term=[parameter],See=[loop parameter]}
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[execution], Sec=(loop_statement)}
For the execution of a @nt{loop_statement},
the @nt{sequence_of_statements} is executed repeatedly,
zero or more times,
until the @nt{loop_statement} is complete.
The @nt{loop_statement} is complete when a transfer
of control occurs that transfers control out of the loop, or, in the
case of an @nt{iteration_scheme}, as specified below.

@PDefn2{Term=[execution],
  Sec=(loop_statement with a while iteration_scheme)}
For the execution of a @nt{loop_statement} with a @key{while}
@nt{iteration_scheme}, the condition is evaluated before each
execution of the @nt{sequence_of_@!statements}; if the value of the
@nt{condition} is True, the @nt{sequence_of_@!statements} is executed;
if False, the execution of the @nt{loop_@!statement} is complete.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0139-2],ARef=[AI05-0262-1]}
@PDefn2{Term=[execution],
  Sec=(loop_statement with a for iteration_scheme)}
@PDefn2{Term=[elaboration], Sec=(loop_parameter_specification)}
For the execution of a @nt{loop_statement} with
@Chg{Version=[3],New=[the],Old=[a @key{for}]}
@nt{iteration_scheme}@Chg{Version=[3],New=[ being @key[for]
@nt{loop_@!parameter_@!specification}],Old=[]},
the @nt{loop_@!parameter_@!specification} is first elaborated. This
elaboration creates the loop parameter and elaborates the
@nt{discrete_@!subtype_@!definition}.
If the @nt{discrete_@!subtype_@!definition} defines a subtype
with a null range,
the execution of the
@nt{loop_statement} is complete. Otherwise, the
@nt{sequence_of_@!statements} is executed once for each value of the
discrete subtype defined by the
@nt{discrete_@!subtype_@!definition}
@Chg{Version=[3],New=[that satisfies the predicate of the subtype ],Old=[]}(or
until the loop is left as a consequence of a transfer of control).
@Defn2{Term=[assignment operation], Sec=(during execution of a @key{for} loop)}
Prior to each such iteration,
the corresponding value of the discrete subtype is assigned to the
loop parameter. These values are assigned in increasing order unless
the reserved word @key{reverse} is present, in which case the values
are assigned in decreasing order.
@begin{Ramification}
The order of creating the loop parameter and evaluating the
@nt{discrete_subtype_definition} doesn't matter,
since the creation of the loop parameter has no side effects (other
than possibly raising Storage_Error, but anything can do that).

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[The predicate (if any) necessarily has to be a
static predicate as a dynamic predicate is explicitly disallowed @em
see @RefSecNum{Subtype Predicates}.]}

@end{Ramification}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[If there is a predicate, the loop
  still visits the values in the order of the underlying base type; the order of
  the values in the predicate is irrelevant. This is the case so that the
  following loops have the same sequence of calls and parameters on procedure
  Call for any subtype S:]}
@begin{Example}
@ChgAdded{Version=[3],Text=[@key[for] I @key[in] S @key[loop]
   Call (I);
@key[end loop];]}

@ChgAdded{Version=[3],Text=[@key[for] I @key[in] S'Base @key[loop]
   @key[if] I @key[in] S @key[then]
      Call (I);
   @key[end if];
@key[end loop];]}
@end{Example}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[@Redundant[For details about the execution of a
@nt{loop_statement} with the @nt{iteration_scheme} being @key[for]
@nt{iterator_specification}, see @RefSecNum{Generalized Loop Iteration}.]]}

@end{RunTime}

@begin{Notes}
A loop parameter is a constant;
it cannot be updated within the
@nt{sequence_of_statements} of the loop
(see @RefSecNum{Objects and Named Numbers}).

An @nt{object_declaration} should not be given for a loop parameter,
since the loop parameter is automatically declared by
the @nt{loop_parameter_specification}.
The scope of a loop parameter extends from the
@nt{loop_parameter_specification} to the end of the
@nt{loop_statement}, and the visibility
rules are such that a loop parameter is only visible within the
@nt{sequence_of_statements} of the loop.
@begin{ImplNote}
An implementation could give a warning if a variable is hidden by a
@nt{loop_parameter_specification}.
@end{ImplNote}

The @nt<discrete_subtype_definition> of a for loop is elaborated
just once. Use of the
reserved word @key[reverse] does not alter the discrete subtype defined,
so that the following @nt{iteration_scheme}s are not equivalent; the
first has a null range.
@begin{Example}
@key[for] J @key[in] @key[reverse] 1 .. 0
@key[for] J @key[in] 0 .. 1
@end{Example}
@begin{Ramification}
If a @nt{loop_parameter_specification} has a static discrete range,
the subtype of the loop parameter is static.
@end{Ramification}
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Example of a loop statement without an iteration scheme:}
@begin{Example}
@key[loop]
   Get(Current_Character);
   @key[exit] @key[when] Current_Character = '*';
@key[end] @key[loop];
@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Example of a loop statement with a @key[while] iteration scheme:}
@end{WideAbove}
@begin{Example}
@key[while] Bid(N).Price < Cut_Off.Price @key[loop]
   Record_Bid(Bid(N).Price);
   N := N + 1;
@key[end] @key[loop];
@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Example of a loop statement with a @key[for] iteration scheme:}
@end{WideAbove}
@begin{Example}
@key[for] J @key[in] Buffer'Range @key[loop]     --@RI{  works even with a null range}
   @key[if] Buffer(J) /= Space @key[then]
      Put(Buffer(J));
   @key[end] @key[if];
@key[end] @key[loop];
@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Example of a loop statement with a name:}
@end{WideAbove}
@begin{Example}
Summation:
   @key[while] Next /= Head @key[loop]       --@RI{ see @RefSecNum{Incomplete Type Declarations}}
      Sum  := Sum + Next.Value;
      Next := Next.Succ;
   @key[end] @key[loop] Summation;
@end{Example}
@end{Examples}

@begin{DiffWord83}
The constant-ness of loop parameters is specified in
@RefSec{Objects and Named Numbers}.
@end{DiffWord83}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0262-1],ARef=[AI05-0299-1]}
  @ChgAdded{Version=[3],Text=[Generalized @nt{iterator_specification}s are
  allowed in @key[for] loops; these are documented as an extension in the
  appropriate subclause.]}
@end{DiffWord2005}


@LabeledAddedSubClause{Version=[3],Name=[User-Defined Iterator Types]}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Type=[Leading],Keepnext=[T],Text=[The following
language-defined generic library package exists:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@ChildUnit{Parent=[Ada],Child=[Iterator_Interfaces]}@key[generic]
   @key[type] Cursor;
   @key[with function] Has_Element (Position : Cursor) @key[return] Boolean;
@key[package] Ada.Iterator_Interfaces @key[is]
   @key[pragma] Pure (Iterator_Interfaces);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Forward_Iterator} @key[is limited interface];
   @key[function] @AdaSubDefn{First} (Object : Forward_Iterator) @key[return] Cursor @key[is abstract];
   @key[function] @AdaSubDefn{Next} (Object : Forward_Iterator; Position : Cursor)
      @key[return] Cursor @key[is abstract];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Reversible_Iterator} @key[is limited interface and] Forward_Iterator;
   @key[function] @AdaSubDefn{Last} (Object : Reversible_Iterator) @key[return] Cursor @key[is abstract];
   @key[function] @AdaSubDefn{Previous} (Object : Reversible_Iterator; Position : Cursor)
      @key[return] Cursor @key[is abstract];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[end] Ada.Iterator_Interfaces;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[An @i<iterator type> is a type descended from
the Forward_Iterator interface from some instance of
Ada.Iterator_Interfaces.@Defn{iterator type}
A @i<reversible iterator type> is a type descended from the Reversible_Iterator
interface from some instance of Ada.Iterator_Interfaces.@Defn{reversible iterator type}
An @i<iterator object> is an object of an iterator type.@Defn{iterator object}
A @i<reversible iterator object> is an object
of a reversible iterator type.@Defn{reversible iterator object}
The formal subtype Cursor from the associated
instance of Ada.Iterator_Interfaces is the @i<iteration cursor subtype> for the
iterator type.@Defn{iteration cursor subtype}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The following type-related
operational aspects may be specified for an indexable container type @i<T> (see
@RefSecNum{User-Defined Indexing}):]}

@begin{Description}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Default_Iterator@\This aspect is specified
    by a @nt{name} that denotes exactly one function declared immediately within
    the same declaration list in which @i<T> is declared, whose first parameter
    is of type @i<T> or @i<T>'Class or an access parameter whose designated type
    is type @i<T> or @i<T>'Class, whose other parameters, if any, have default
    expressions, and whose result type is an iterator type. This function is the
    @i<default iterator function> for @i<T>.@Defn{default iterator function}
    Its result subtype is the @i<default iterator subtype> for
    @i<T>.@Defn{default iterator subtype} The iteration cursor subtype for
    the default iterator subtype is the @i<default cursor subtype>
    for @i<T>.@Defn{default cursor subtype}@AspectDefn{Default_Iterator}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Default_Iterator],
    Text=[@ChgAdded{Version=[3],Text=[Default iterator to be used in @key[for]
    loops.]}]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Iterator_Element@\This aspect is specified by a
    @nt{name} that denotes a subtype. This is the @i<default element subtype>
    for @i<T>.@Defn{default element subtype}@AspectDefn{Iterator_Element}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Iterator_Element],
    Text=[@ChgAdded{Version=[3],Text=[Element type to be used for user-defined
      iterators.]}]}
@end{Description}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[These aspects are inherited by descendants of
type @i<T> (including @i<T>'Class).]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Text=[An @i<iterable container type> is an indexable container type
with specified Default_Iterator and Iterator_Element aspects.@Defn{iterable container type}
A @i<reversible iterable container type> is an iterable container type with the default iterator type
being a reversible iterator type.@Defn{reversible iterable container type}
An @i<iterable container object> is an object of an iterable container type.@Defn{iterable container object}
A @i<reversible iterable container object> is an object of a reversible iterable container
type.@Defn{reversible iterable container object}]}

@ChgToGlossary{Version=[3],Kind=[Added],Term=<Iterable container type>,
Text=<@ChgAdded{Version=[3],Text=[An iterable container type is one that has
user-defined behavior for iteration, via the Default_Iterator and
Iterator_Element aspects.]}>}

@end{StaticSem}

@begin{Legality}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The Constant_Indexing aspect (if any)
of an iterable container type @i<T> shall denote exactly one function with the following
properties:]}

@begin{Itemize}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[the result type of the function is covered by the
    default element type of @i<T> or is a reference type (see
    @RefSecNum{User-Defined References}) with an access discriminant designating
    a type covered by the default element type of @i<T>;]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[the type of the second parameter of the function
    covers the default cursor type for @i<T>;]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[if there are more than two parameters, the
    additional parameters all have default expressions.]}

@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[This function (if any) is the
@i<default constant indexing function> for @i<T>.@Defn{default constant indexing function}]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This does not mean that Constant_Indexing has to
  designate only one subprogram, only that there is only one routine that meets
  all of these properties. There can be other routines designated by
  Constant_Indexing, but they cannot have the profile described above. For
  instance, map containers have a version of Constant_Indexing that takes a
  key instead of a cursor; this is allowed.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The Variable_Indexing aspect (if any)
of an iterable container type @i<T> shall denote exactly one function with the following
properties:]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[the result type of the function is a reference
    type (see @RefSecNum{User-Defined References}) with an access discriminant
    designating a type covered by the default element type of @i<T>;]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[the type of the second parameter of the function
    covers the default cursor type for @i<T>;]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[if there are more than two parameters, the
    additional parameters all have default expressions.]}

@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[This function (if any) is the
@i<default variable indexing function> for @i<T>.@Defn{default variable indexing function}]}

@end{Legality}


@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}User-defined
  iterator types are new in Ada 2012.]}
@end{Extend2005}


@LabeledAddedSubClause{Version=[3],Name=[Generalized Loop Iteration]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[Generalized forms of loop iteration are provided by
an @nt{iterator_specification}.]}
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<iterator_specification>,Old=<>}>,
rhs="@Chg{Version=[3],New=<
    @Syn2{defining_identifier} @key[in] [@key{reverse}] @SynI{iterator_}@Syn2{name}
  | @Syn2{defining_identifier} [: @Syn2{subtype_indication}] @key[of] [@key{reverse}] @SynI{iterable_}@Syn2{name}>,Old=<>}"}
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Text=[For the first form of @nt{iterator_specification},
called a @i<generalized iterator>,@Defn{generalized iterator}@Defn2{Term=[iterator],Sec=[generalized]}
the expected type for the @SynI<iterator_>@nt{name} is any iterator
type.@PDefn2{Term=[expected type],Sec=[@SynI<iterator_>@nt{name}]}
For the second form of @nt{iterator_specification},
the expected type for the @SynI<iterable_>@nt{name} is any array or iterable
container type.@PDefn2{Term=[expected type],Sec=[@SynI<iterable_>@nt{name}]}
If the @SynI<iterable_>@nt{name} denotes an array object, the
@nt{iterator_specification} is called an @i<array
component iterator>;@Defn{array component iterator}@Defn2{Term=[iterator],Sec=[array component]}
otherwise it is called a
@i<container element iterator>.@Defn{container element iterator}@Defn2{Term=[iterator],Sec=[container element]}]}
@end{Resolution}

@begin{Legality}
@ChgToGlossary{Version=[3],Kind=[Added],Term=<Iterator>,
Text=<@ChgAdded{Version=[3],Text=[An iterator is a construct that is used to
loop over the elements of an array or container. Iterators may be user defined,
and may perform arbitrary computations to access elements from a container.]}>}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[If the reserved word @key[reverse] appears,
the @nt{iterator_specification} is a @i<reverse iterator>;@Defn{reverse iterator}@Defn2{Term=[iterator],Sec=[reverse]}
otherwise it is a @i<forward iterator>.@Defn{forward iterator}@Defn2{Term=[iterator],Sec=[forward]}
In a reverse generalized iterator, the
@SynI<iterator_>@nt{name} shall be of a reversible iterator type.
In a reverse container element iterator, the default iterator type for the type
of the @SynI<iterable_>@nt{name} shall be a reversible iterator type.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[The type of the @nt{subtype_indication}, if any,
of an array component iterator shall cover the component type of the type
of the @SynI<iterable_>@nt{name}. The type of the @nt{subtype_indication},
if any, of a container element iterator shall cover the default element type
for the type of the @SynI<iterable_>@nt{name}.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[In a container element iterator whose
@SynI<iterable_>@nt{name} has type @i<T>, if the @SynI<iterable_>@nt{name}
denotes a constant or the Variable_Indexing aspect is not specified for @i<T>,
then the Constant_Indexing aspect shall be specified for @i<T>.]}

@end{Legality}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0269-1],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Text=[An @nt{iterator_specification} declares a
@i<loop parameter>.@Defn{loop parameter}
In a generalized iterator, the nominal subtype of the loop parameter is
the iteration cursor subtype. In an array component iterator or a
container element iterator, if a @nt{subtype_indication} is present, it
determines the nominal subtype of the loop parameter. In an array
component iterator, if a @nt{subtype_indication} is not present, the
nominal subtype of the loop parameter is the component subtype of the
type of the @SynI{iterable_}@nt{name}. In a container element iterator, if a
@nt{subtype_indication} is not present, the nominal subtype of the loop
parameter is the default element subtype for the type of the
@SynI{iterable_}@nt{name}.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Text=[In a generalized iterator, the loop parameter
is a constant. In an array component iterator, the loop parameter
is a constant if the @SynI<iterable_>@nt{name} denotes a constant; otherwise
it denotes a variable. In a container element iterator, the loop parameter
is a constant if the @SynI{iterable_}@nt{name} denotes a constant, or if
the Variable_Indexing aspect is not specified for the type of the
@SynI{iterable_}@nt{name}; otherwise it is a variable.]}

@end{StaticSem}

@begin{Runtime}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[For the execution of a @nt{loop_statement} with
an @nt{iterator_specification}, the @nt{iterator_specification} is
first elaborated. This elaboration elaborates the @nt{subtype_indication},
if any.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[For a generalized iterator, the loop parameter is
created, the @SynI{iterator_}@nt{name} is evaluated, and the denoted iterator
object becomes the @i<loop iterator>.@Defn{loop iterator} In a forward
generalized iterator, the operation First of the iterator type is called on the
loop iterator, to produce the initial value for the loop parameter. If the
result of calling Has_Element on the initial value is False, then the execution
of the @nt{loop_statement} is complete. Otherwise, the
@nt{sequence_of_statements} is executed and then the Next operation of the
iterator type is called with the loop iterator and the current value of the loop
parameter to produce the next value to be assigned to the loop parameter. This
repeats until the result of calling Has_Element on the loop parameter is False,
or the loop is left as a consequence of a transfer of control. For a reverse
generalized iterator, the operations Last and Previous are called rather than
First and Next.]}


@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Text=[For an array component iterator, the
@SynI<iterable_>@nt{name} is evaluated and
the denoted array object becomes the @i<array for the
loop>.@Defn{array for a loop} If the array for the loop is a null array,
then the execution of the @nt{loop_statement} is complete. Otherwise, the
@nt{sequence_of_statements} is executed with the loop parameter denoting
each component of the array for the loop, using a @i<canonical> order
of components,@Defn{canonical order of array components}
which is last dimension varying fastest (unless the
array has convention Fortran, in which case it is first dimension
varying fastest). For a
forward array component iterator, the iteration starts with the
component whose index values are each the first in their index range,
and continues in the canonical order. For a reverse array component
iterator, the iteration starts with the component whose index values
are each the last in their index range, and continues in the reverse
of the canonical order. The loop iteration proceeds until the
@nt{sequence_of_statements} has been executed for each component of the
array for the loop, or until the loop is left as a consequence of a
transfer of control.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Text=[For a container element iterator, the @SynI<iterable_>@nt{name} is evaluated
and the denoted iterable container object becomes the @i<iterable
container object for the loop>.@Defn{iterable container object for a loop}
The default iterator function for the type of
the iterable container object for the loop is called on the iterable container object
and the result is the @i<loop iterator>.@Defn2{Term=[loop iterator],Sec=[container element iterator]}
An object of the default cursor subtype is created (the @i<loop cursor>).@Defn{loop cursor}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Text=[For a forward container element iterator, the
operation First of the iterator type is called on the loop iterator, to produce
the initial value for the loop cursor. If the result of calling Has_Element on
the initial value is False, then the execution of the @nt{loop_statement} is
complete. Otherwise, the @nt{sequence_of_statements} is executed with the loop
parameter denoting an indexing (see @RefSecNum{User-Defined Indexing}) into the
iterable container object for the loop, with the only parameter to the indexing being the
current value of the loop cursor; then the Next operation of the iterator type
is called with the loop iterator and the loop cursor to produce the next value
to be assigned to the loop cursor. This repeats until the result of calling
Has_Element on the loop cursor is False, or until the loop is left as a
consequence of a transfer of control. For a reverse container element iterator,
the operations Last and Previous are called rather than First and Next. If the
loop parameter is a constant (see above), then the indexing uses the default
constant indexing function for the type of the iterable container object for the
loop; otherwise it uses the default variable indexing function.]}

@end{Runtime}

@begin{Examples}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[-- @Examcom{Array component iterator example:}
@key[for] Element @key[of] Board @key[loop]  -- @Examcom{See @RefSecNum{Index Constraints and Discrete Ranges}.}
   Element := Element * 2.0; -- @Examcom{Double each element of Board, a two-dimensional array.}
@key[end loop];]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0268-1]}
@ChgAdded{Version=[3],Text=[For examples of use of generalized iterators,
see @RefSecNum{Example of Container Use} and the corresponding container
packages in @RefSecNum{The Generic Package Containers.Vectors} and
@RefSecNum{The Generic Package Containers.Doubly_Linked_Lists}.]}
@end{Examples}


@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}Generalized forms
  of loop iteration are new.]}
@end{Extend2005}




@RMNewPageVer{Version=[0]}@Comment{For printed version of Ada 95}
@RMNewPageVer{Version=[1]}@Comment{For printed version of Ada 95 + TC1 RM}
@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
@LabeledClause{Block Statements}

@begin{Intro}
@Redundant[A @nt{block_statement} encloses a
@nt{handled_sequence_of_statements}
optionally preceded by a @nt{declarative_part}.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<block_statement>,rhs="
   [@SynI{block_}@Syn2{statement_identifier}:]
       [@key{declare}
            @Syn2{declarative_part}]
        @key{begin}
            @Syn2{handled_sequence_of_statements}
        @key{end} [@SynI{block_}@Syn2{identifier}];"}

@begin(SyntaxText)
If a @nt{block_statement} has a @SynI{block_}@nt{statement_identifier},
then the @nt<identifier> shall be repeated after the @key{end};
otherwise, there shall not be an @nt<identifier> after the @key{end}.
@end(SyntaxText)
@end{Syntax}

@begin{StaticSem}
A @nt{block_statement} that has no
explicit @nt{declarative_part} has an implicit empty
@nt{declarative_part}.
@begin{Ramification}
Thus, other rules can always
refer to the @nt{declarative_part} of a @nt<block_statement>.
@end{Ramification}
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[execution], Sec=(block_statement)}
The execution of a @nt{block_statement} consists of the elaboration
of its @nt{declarative_part} followed by the execution of
its @nt{handled_sequence_of_statements}.
@end{RunTime}

@begin{Examples}
@Leading@keepnext@i{Example of a block statement with a local variable:}
@begin{Example}
Swap:
   @key[declare]
      Temp : Integer;
   @key[begin]
      Temp := V; V := U; U := Temp;
   @key[end] Swap;
@end{Example}
@begin{Ramification}
If task objects are declared within a @nt{block_statement} whose execution
is completed, the @nt{block_statement} is not left until all its dependent
tasks are terminated
(see @RefSecNum{Assignment and Finalization}).
This rule applies to completion caused by a transfer of control.

Within a @nt{block_statement}, the block name can be used in expanded
names denoting local entities such as Swap.Temp in the above example
(see @RefSecNum{Selected Components}).
@end{Ramification}
@end{Examples}

@begin{DiffWord83}
The syntax rule for @nt{block_statement} now uses the syntactic category
@nt{handled_sequence_of_statements}.
@end{DiffWord83}

@LabeledClause{Exit Statements}

@begin{Intro}
@Redundant[An @nt{exit_statement} is used to complete the execution
of an enclosing @nt{loop_statement}; the
completion is conditional if the @nt{exit_statement} includes a
@nt{condition}.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<exit_statement>,rhs="
   @key{exit} [@SynI{loop_}@Syn2{name}] [@key{when} @Syn2{condition}];"}
@end{Syntax}

@begin{Resolution}
The @i(loop_)@nt{name}, if any, in an @nt{exit_statement} shall resolve to
denote a @nt{loop_statement}.
@end{Resolution}

@begin{Legality}
@Defn2{Term=[apply], Sec=(to a @nt{loop_statement} by an @nt{exit_statement})}
Each @nt{exit_@!statement} @i{applies to} a
@nt{loop_@!statement}; this is the @nt{loop_@!statement} being exited.
An @nt{exit_@!statement} with a @nt{name} is only allowed within the
@nt{loop_@!statement} denoted by the @nt{name},
and applies to that @nt{loop_@!statement}.
An @nt{exit_@!statement} without a @nt{name} is only allowed within a
@nt{loop_@!statement}, and applies to the innermost enclosing one.
An @nt{exit_@!statement} that applies to a given @nt{loop_@!statement}
shall not appear within a body or @nt{accept_@!statement}, if
this construct is itself enclosed by the given @nt{loop_statement}.
@end{Legality}

@begin{RunTime}
@PDefn2{Term=[execution], Sec=(exit_statement)}
For the execution of an @nt{exit_statement}, the @nt{condition}, if
present, is first evaluated.
If the value of the @nt{condition} is True, or if there is no @nt{condition},
a transfer of control is done to complete the @nt{loop_@!statement}.
If the value of the @nt{condition} is False, no transfer of control takes
place.
@end{RunTime}

@begin{Notes}
Several nested loops can be exited by an @nt{exit_statement} that names
the outer loop.
@end{Notes}

@begin{Examples}
@i{Examples of loops with exit statements:}
@begin{Example}
@key[for] N @key[in] 1 .. Max_Num_Items @key[loop]
   Get_New_Item(New_Item);
   Merge_Item(New_Item, Storage_File);
   @key[exit] @key[when] New_Item = Terminal_Item;
@key[end] @key[loop];

Main_Cycle:
   @key[loop]
      --@RI{  initial statements}
      @key[exit] Main_Cycle @key[when] Found;
      --@RI{  final statements}
   @key[end] @key[loop] Main_Cycle;
@end{Example}
@end{Examples}

@LabeledClause{Goto Statements}

@begin{Intro}
@Redundant[A @nt{goto_statement} specifies an explicit transfer of control
from this @nt{statement} to a target
statement with a given label.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<goto_statement>,rhs="@key{goto} @SynI{label_}@Syn2{name};"}
@end{Syntax}

@begin{Resolution}
@Defn2{Term=[target statement], Sec=(of a @nt{goto_statement})}
The @i(label_)@nt{name} shall resolve to denote a @nt<label>;
the @nt{statement} with that @nt{label} is the @i(target statement).
@end{Resolution}

@begin{Legality}
The innermost @nt{sequence_of_statements} that encloses the target
statement shall also enclose the @nt{goto_statement}.
Furthermore, if a @nt{goto_statement} is enclosed by an
@nt{accept_statement} or a body, then the target
statement shall not be outside this enclosing construct.
@begin{Ramification}
The @nt{goto_statement} can be a @nt{statement} of an inner
@ntf{sequence_}.

It follows from the second rule that if the target @nt{statement}
is enclosed by such a construct, then the @nt{goto_statement}
cannot be outside.
@end{Ramification}
@end{Legality}

@begin{RunTime}
@PDefn2{Term=[execution], Sec=(goto_statement)}
The execution of a @nt{goto_statement} transfers control to the
target statement, completing the execution
of any @nt<compound_statement> that encloses the @nt<goto_statement>
but does not enclose the target.
@end{RunTime}

@begin{Notes}
The above rules allow transfer of control to a @nt{statement} of an
enclosing @nt{sequence_of_statements} but not the reverse. Similarly,
they prohibit transfers of control such as between alternatives of a
@nt{case_statement}, @nt{if_statement}, or @nt{select_statement};
between @nt{exception_handler}s; or from an @nt{exception_handler} of
a @nt{handled_sequence_of_statements}
back to its @nt{sequence_of_statements}.
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Example of a loop containing a goto statement:}
@begin{Example}
<<Sort>>
@key[for] I @key[in] 1 .. N-1 @key[loop]
   @key[if] A(I) > A(I+1) @key[then]
      Exchange(A(I), A(I+1));
      @key[goto] Sort;
   @key[end] @key[if];
@key[end] @key[loop];
@end{Example}
@end{Examples}
