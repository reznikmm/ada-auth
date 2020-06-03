@Part(05, Root="ada.mss")

@Comment{$Date: 2020/06/03 00:09:00 $}
@LabeledSection{Statements}

@Comment{$Source: e:\\cvsroot/ARM/Source/05.mss,v $}
@Comment{$Revision: 1.84 $}

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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0119-1]}
@Redundant[A @nt<statement> is either simple or compound.
A @nt<simple_statement> encloses
no other @nt<statement>. A @nt<compound_statement> can enclose
@nt<simple_statement>s and other @nt<compound_statement>s.]@Chg{Version=[5],New=[
A @i<parallel construct>@Defn{parallel construct}@Defn2{Term=[construct],Sec=[parallel]}
is a construct that introduces additional
logical threads of control (see clause @RefSecNum{Tasks and Synchronization})
without creating a new task.
Parallel loops (see @RefSecNum{Loop Statements}) and
@nt{parallel_block_statement}s (see
@RefSecNum{Parallel Block Statements}) are parallel
constructs.@IndexSee{Term=[parallel processing],See=(parallel construct)}
@IndexSee{Term=[concurrent processing],See=(parallel construct)}],Old=[]}

@ChgToGlossary{Version=[5],Kind=[Added],Term=<Parallel Construct>,
Text=<@ChgAdded{Version=[5],Text=[An executable construct that defines multiple
activities of a single task that can proceed in parallel, via the execution of
multiple logical threads of control.]}>}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0119-1]}
@Syn{tabs=[P31], lhs=<compound_statement>,rhs="
     @Syn2{if_statement}@\| @Syn2{case_statement}
   | @Syn2{loop_statement}@\| @Syn2{block_statement}@Chg{Version=[2],New=[
   | @Syn2{extended_return_statement}],Old=[]}@Chg{Version=[5],New=[
   | @Syn2{parallel_block_statement}],Old=[]}
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

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0119-1]}
@ChgAdded{Version=[5],Text=[Within a parallel construct, if a transfer of
control out of the construct is initiated by one of the logical threads of
control, an attempt is made to @i<cancel>@Defn2{Term=[cancel],Sec=[a logical
thread]}@Defn2{Term=[cancel],Sec=[a parallel construct]} all other logical
threads of control initiated by the parallel construct. Once all other logical
threads of control of the construct either complete or are canceled, the
transfer of control occurs. If two or more logical threads of control of the
same construct initiate such a transfer of control concurrently, one of them is
chosen arbitrarily and the others are canceled.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0119-1]}
@ChgAdded{Version=[5],Text=[When a logical thread of control is canceled, the
cancellation causes it to complete as though it had performed a transfer of
control to the point where it would have finished its execution. Such a
cancellation is deferred while the logical thread of control is executing within
an abort-deferred operation (see
@RefSecNum{Abort of a Task - Abort of a Sequence of Statements}), and may be
deferred further, but not past
a point where the logical thread initiates a new nested parallel construct or
reaches an exception handler that is outside such an abort-deferred operation.]}
@end{RunTime}

@begin{Bounded}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0119-1]}
@ChgAdded{Version=[5],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
During the execution of a parallel construct, it is
a bounded error to invoke an operation that is potentially blocking (see
@RefSecNum{Intertask Communication}). Program_Error is raised if the error is
detected by the implementation; otherwise, the execution of the potentially
blocking operation might proceed normally, or it might result in the indefinite
blocking of some or all of the logical threads of control making up the current
task.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}
@end{Bounded}

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
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0179-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  A @nt{label} can end a @nt{sequence_of_statements},
  eliminating the requirement for having an explicit @key[null]; statement
  after an ending label (a common use).]}
@end{Extend2005}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0119-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}
  The definition of @ldquote@;parallel constructs@rdquote and the
  @nt{parallel_block_statement} are new.]}
@end{Extend2012}


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
nonlimited type],Old=[]}.@Defn2{Term=[variable],Sec=(required)}@Defn2{Term=[object],Sec=(required)}

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
  @Defn2{Term=[Constraint_Error],Sec=(raised by failure of runtime check)}
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

@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0056-1]}
Writer := (Status => Open, Unit => Printer, Line_Count => 60);  --@RI{ see @RefSecNum{Variant Parts and Discrete Choices}}
@Chg{Version=[4],New=[Next],Old=[Next_Car]}.@key[all] := (72074, @key[null]@Chg{Version=[4],New=[, Head],Old=[]});@Chg{Version=[4],New=[],Old=[ ]}   --@RI{  see @RefSecNum{Incomplete Type Declarations}}
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



@LabeledAddedSubClause{Version=[5],Name=[Target Name Symbols]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0125-3]}
@ChgAdded{Version=[5],Text=[@@, known as the @i<target name> of an assignment
statement, provides an abbreviation to avoid repetition of potentially long
names in assignment statements.@Defn{target name}]}

@begin{Syntax}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0125-3]}
@AddedSyn{Version=[5],lhs=<@Chg{Version=[5],New=<target_name>,Old=<>}>,
rhs="@Chg{Version=[5],New=<@@>,Old=<>}"}
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0125-3]}
@ChgAdded{Version=[5],Text=[@Redundant[If a @nt{target_name} occurs in an
@nt{assignment_statement} @i<A>, the @SynI<variable_>@nt{name} @i<V> of @i<A>
is a complete context. The target name is a
constant view of @i<V>, having the nominal subtype of @i<V>.]]}

@begin{TheProof}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0125-3],ARef=[AI12-0322-1]}
  @ChgAdded{Version=[5],Text=[The complete context rule is formally given in
  @RefSecNum{The Context of Overload Resolution}. The constant view rule is
  formally given in @RefSecNum{Objects and Named Numbers}; the nominal subtype
  is a property taken from the target object as described below
  in @RunTimeTitle.]}
@end{TheProof}
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0125-3],ARef=[AI12-0322-1]}
@ChgAdded{Version=[5],Text=[A @nt{target_name} shall appear only in the
@nt{expression} of an @nt{assignment_statement}.]}
@end{Legality}

@begin{RunTime}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0125-3],ARef=[AI12-0322-1]}
@ChgAdded{Version=[5],Text=[For the execution of an @nt{assignment_statement}
with one or more @nt{target_name}s appearing in its @nt{expression}, the
@SynI{variable_}@nt{name} @i{V} of the @nt{assignment_statement}
is evaluated first to determine the object denoted by @i{V}, and then the
@nt{expression} of the @nt{assignment_statement} is evaluated with the evaluation of
each @nt{target_name} yielding a constant view of the the target whose
properties are otherwise identical to those of the view provided by @i{V}. The
remainder of the execution of the @nt{assignment_statement} is as given in
subclause @RefSecNum{Assignment Statements}.]}

@begin{Honest}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[The properties here include static properties
  like whether the @nt{target_name} is aliased and the nominal subtype of
  the @nt{target_name}. It was too weird to give separate rules for static
  and dynamic properties that said almost the same thing.]}
@end{Honest}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0322-1]}
  @ChgAdded{Version=[5],Text=[Use of a @nt{target_name} can be erroneous if the
   @SynI{variable_}@nt{name} @i{V} is a discriminant-dependent component, and
   some other constituent of the @nt{expression} modifies the discriminant
   governing the component @i<V>. The assignment probably would be erroneous
   anyway, but the use of a @nt{target_name} eliminates the possibility that
   a later evaluation of @i<V> raises an exception before any erroneous
   execution occurs. See @RefSecNum{Operations of Discriminated Types}.]}
@end{Ramification}
@end{RunTime}

@begin{Examples}
@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0125-3],ARef=[AI12-0379-1]}
@ChgAdded{Version=[5],Text=[Board(1, 1) := @@ + 1.0;  -- @Examcom<An abbreviation for Board(1, 1) := Board(1, 1) + 1.0;>
                       -- @Examcom<(Board is declared in @RefSecNum{Index Constraints and Discrete Ranges}).>]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0125-3]}
@ChgAdded{Version=[5],Text=[My_Complex_Array : @key[array] (1 .. Max) @key[of] Complex; -- @Examcom<See @RefSecNum{Number Declarations}, @RefSecNum{Record Types}.>
...
-- @Examcom<Square the element in the Count (see @RefSecNum{Object Declarations}) position:>
My_Complex_Array (Count) := (Re => @@.Re**2 - @@.Im**2,
                             Im => 2.0 * @@.Re * @@.Im);
   -- @Examcom<A target_name can be used multiple times and as a prefix if needed.>]}
@end{Example}
@end{Examples}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0125-3],ARef=[AI12-0322-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}
  The target name symbol @@ is new.]}
@end{Extend2012}


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
   @key{end case};"}

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
  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0071-1]}
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
  @Chg{Version=[4],New=[predicates],Old=[predicate]} (see
  @RefSecNum{Subtype Predicates})],Old=[]},
  and each value of that subtype@Chg{Version=[3],New=[ that satisfies its
  @Chg{Version=[4],New=[predicates],Old=[predicate]}],Old=[]} shall be
  covered by some @nt{discrete_choice}
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
For the execution of a @nt{case_statement}@Chg{Version=[5],New=[,],Old=[]} the
@Chg{Version=[3],New=[@SynI{selecting_}],Old=[]}@nt{expression}
is first evaluated.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0188-1]}
If the value of the @Chg{Version=[3],New=[@SynI{selecting_}],Old=[]}@nt{expression}
is covered by the @nt{discrete_@!choice_@!list} of some
@nt{case_@!statement_@!alternative}, then the
@nt{sequence_of_@!statements} of the @ntf{_alternative} is executed.

@Defn2{Term=[Constraint_Error],Sec=(raised by case statement)}
Otherwise (the value is not covered by any
@nt{discrete_choice_list},
perhaps due to being outside the base range),
Constraint_Error is raised.
@begin{Ramification}

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0005-1]}
  In this case, the value @Chg{Version=[5],New=[fails to satisfy its
  (static) predicate (possible when the predicate is disabled),],Old=[]}
  is outside the base range of its type,
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

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0071-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Updated wording of
  case coverage to use the new term "satisfies the predicates"
  (see @RefSecNum{Subtype Predicates}).]}
@end{Diffword2012}


@LabeledClause{Loop Statements}

@begin{Intro}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0119-1]}
@Redundant[A @nt{loop_statement} includes a
@nt{sequence_of_statements} that is to be executed repeatedly,
zero or more times@Chg{Version=[5],New=[ with the iterations
running sequentially or concurrently with one another],Old=[]}.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<loop_statement>,rhs="
   [@SynI{loop_}@Syn2{statement_identifier}:]
      [@Syn2{iteration_scheme}] @key{loop}
         @Syn2{sequence_of_statements}
       @key{end} @key{loop} [@SynI{loop_}@Syn2{identifier}];"}


@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0139-2]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0119-1],ARef=[AI12-0189-1],ARef=[AI12-0251-1],ARef=[AI12-0266-1],ARef=[AI12-0326-2]}
@Syn{lhs=<iteration_scheme>,rhs="@key{while} @Syn2{condition}
   | @key{for} @Syn2{loop_parameter_specification}@Chg{Version=[3],New=[
   | @key{for} @Syn2{iterator_specification}],Old=[]}@Chg{Version=[5],New=<
   | [@key[parallel]]
     @key{for} @Syn2{procedural_iterator}
   | @key[parallel] [(@Syn2{chunk_specification})]
     @key{for} @Syn2{loop_parameter_specification}
   | @key[parallel] [(@Syn2{chunk_specification})]
     @key{for} @Syn2{iterator_specification}>,Old=[]}"}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0251-1]}
@AddedSyn{Version=[5],lhs=<@Chg{Version=[5],New=<chunk_specification>,Old=<>}>,
rhs="@Chg{Version=[5],New=<
     @SynI{integer_}@Syn2{simple_expression}
   | @Syn2{defining_identifier} @key[in] @Syn2{discrete_subtype_definition}>,Old=<>}"}

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0250-1]}
@Syn{lhs=<loop_parameter_specification>,rhs="
   @Syn2{defining_identifier} @key{in} [@key{reverse}] @Syn2{discrete_subtype_definition}@Chg{Version=[5],New=<
     [@Syn2{iterator_filter}]>,Old=<>}"}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0250-1]}
@AddedSyn{Version=[5],lhs=<@Chg{Version=[5],New=<iterator_filter>,Old=<>}>,
rhs="@Chg{Version=[5],New=<@key[when] @Syn2{condition}>,Old=<>}"}

@begin(SyntaxText)
If a @nt{loop_statement} has a @SynI{loop_}@nt{statement_identifier},
then the @nt<identifier> shall be repeated after the @key{end loop};
otherwise, there shall not be an @nt<identifier> after the @key{end loop}.

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0119-1]}
@ChgAdded{Version=[5],Text=[An @nt{iteration_scheme} that begins with the reserved word
@key[parallel] shall not have the reserved word @key[reverse] in its
@nt{loop_parameter_specification}.]}
@end(SyntaxText)
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0251-1]}
@ChgAdded{Version=[5],Text=[In a @nt{chunk_specification} that is an
@Syni{integer_}@nt{simple_expression}, the
@Syni{integer_}@nt{simple_expression} is expected to be of any integer type.]}
@end{Resolution}

@begin{StaticSem}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0061-1]}
@Defn{loop parameter}
A @nt{loop_parameter_specification} declares a @i{loop parameter},
which is an object whose subtype@Chg{Version=[5],New=[ (and nominal subtype)],Old=[]}
 is that defined by the @nt{discrete_subtype_definition}.
@IndexSeeAlso{Term=[parameter],See=[loop parameter]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0251-1]}
@ChgAdded{Version=[5],Text=[In a @nt{chunk_specification} that has a
@nt{discrete_subtype_definition}, the @nt{chunk_specification} declares a
@i{chunk parameter} object@Defn{chunk parameter} whose subtype (and nominal
subtype) is that defined by the @nt{discrete_subtype_definition}.]}
@end{StaticSem}

@begin{RunTime}
@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0250-1],ARef=[AI12-0327-1]}
@ChgAdded{Version=[5],Text=[The @i<filter> of an @i{iterator construct}
@Defn{iterator construct}@Defn2{Term=[filter],Sec={iterator construct}} (a
@nt{loop_parameter_specification}, @nt{iterator_specification}, or
@nt{procedural_iterator}) is defined to be
@i{satisfied}@Defn2{Term=[satisfied],Sec=[filter]} when there is no
@nt{iterator_filter} for the iterator construct, or when the @nt{condition}
of the @nt{iterator_filter} evaluates to True for a given iteration of the
iterator construct.@Defn{iterator filter}]}

@ChgToGlossary{Version=[5],Kind=[Added],Term=<Iterator Filter>,
Text=<@ChgAdded{Version=[5],Text=[A construct that is used to restrict the
elements produced by an iteration to those for which a boolean condition
evaluates to True.]}>}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0250-1],ARef=[AI12-0327-1]}
@ChgAdded{Version=[5],Text=[If a @nt{sequence_of_statements} of a
@nt{loop_statement} with an iterator construct is said to be
@i{conditionally executed},@Defn{conditionally executed} then the @nt{statement}s
are executed only when the filter of the iterator construct is satisfied.]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0250-1],ARef=[AI12-0327-1]}
@ChgAdded{Version=[5],Text=[The loop iterators @nt{loop_parameter_specification}
and @nt{iterator_specification} can also be used in contexts other than
@nt{loop_statement}s (for example, see @RefSecNum{Container Aggregates} and
@RefSecNum{Quantified Expressions}). In such a context,
the iterator @i{conditionally produces}@Defn2{Term=[conditionally produces],
Sec=[iteration]} values in the order specified for
the associated construct below or in @RefSecNum{Generalized Loop Iteration}.
The values produced are the values given to the loop parameter when the filter
of the iterator construct is satisfied for that value. @Redundant[No value is
produced when the @nt{condition} of an @nt{iterator_filter} evaluates to False.]]}

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0119-1]}
@PDefn2{Term=[execution], Sec=(loop_statement)}
For the execution of a @nt{loop_statement},
the @nt{sequence_of_statements} is executed@Chg{Version=[5],New=[],Old=[ repeatedly,]}
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

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0251-1],ARef=[AI12-0294-1]}
@ChgAdded{Version=[5],Text=[If the reserved word @key[parallel] is present
in the @nt{iteration_scheme} of a @nt{loop_statement} (a
@i{parallel loop}),@Defn{parallel loop}@Defn2{Term=[loop],Sec=[parallel]}
the iterations are partitioned into one or more @Defn{chunk}@i{chunks}, each
with its own separate logical thread of control (see
clause @RefSecNum{Tasks and Synchronization}). If a @nt{chunk_specification}
is present in a parallel loop, it is elaborated first, and the result of the
elaboration determines the maximum number of chunks used for the parallel loop.
@PDefn2{Term=[elaboration], Sec=(chunk_specification)}
If the @nt{chunk_specification}
is an @SynI{integer_}@nt{simple_expression}, the elaboration evaluates the
expression, and the value of the expression determines the maximum number
of chunks. If a @nt{discrete_subtype_definition} is present, the
elaboration elaborates the @nt{discrete_subtype_definition}, which
defines the subtype of the chunk parameter, and the number
of values in this subtype determines the maximum number of chunks. After
elaborating the @nt{chunk_specification}, a check is made that the determined
maximum number of chunks is greater than zero. If
this check fails, Program_Error is raised.@IndexCheck{Program_Error_Check}
@Defn2{Term=[Program_Error],Sec=(raised by failure of runtime check)}]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0139-2],ARef=[AI05-0262-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0071-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0119-1],ARef=[AI12-0250-1],ARef=[AI12-0251-1],ARef=[AI12-0294-1]}
@Chg{Version=[5],New=[@PDefn2{Term=[execution],
  Sec=(loop_statement with a for loop_parameter_specification)}],
Old=[@PDefn2{Term=[execution],
  Sec=(loop_statement with a for iteration_scheme)}]}
@PDefn2{Term=[elaboration], Sec=(loop_parameter_specification)}
For the execution of a @nt{loop_statement} @Chg{Version=[5],New=[that has an],
Old=[with @Chg{Version=[3],New=[the],Old=[a @key{for}]}]}
@nt{iteration_scheme}@Chg{Version=[3],New=[ @Chg{Version=[5],New=[including
a],Old=[being @key[for]]} @nt{loop_@!parameter_@!specification}],Old=[]},
@Chg{Version=[5],New=[after
elaborating the @nt{chunk_specification}, if any, ],Old=[]}the
@nt{loop_@!parameter_@!specification} is
@Chg{Version=[5],New=[],Old=[first ]}elaborated. This
@Chg{Version=[5],New=[],Old=[elaboration creates the loop parameter
and ]}elaborates the @nt{discrete_@!subtype_@!definition}@Chg{Version=[5],New=[,
which defines the subtype of the loop parameter],Old=[]}.
If the @nt{discrete_@!subtype_@!definition} defines a subtype
with a null range,
the execution of the
@nt{loop_statement} is complete. Otherwise, the
@nt{sequence_of_@!statements} is@Chg{Version=[5],New=[ conditionally],Old=[]}
executed once for each value of the discrete subtype defined by the
@nt{discrete_@!subtype_@!definition}
@Chg{Version=[3],New=[that satisfies the
@Chg{Version=[4],New=[predicates],Old=[predicate]} of the subtype ],Old=[]}(or
until the loop is left as a consequence of a transfer of control).
@Defn2{Term=[assignment operation], Sec=(during execution of a @key{for} loop)}
Prior to each such iteration,
the corresponding value of the discrete subtype is assigned to the
loop parameter@Chg{Version=[5],New=[ associated with the given iteration.
If the loop is a parallel loop,
each chunk has its own logical thread of control with its own copy of the loop
parameter; otherwise (a @i<sequential loop>)@Defn{sequential loop}@Defn2{Term=[loop],Sec=[sequential]},
a single logical thread of control performs the loop, and there is a single copy
of the loop parameter. Each logical thread of control handles a distinct
subrange of the values of the subtype of the loop parameter such that
all values are covered with no overlaps.
Within each logical thread of control, the],Old=[. These]}
values are assigned @Chg{Version=[5],New=[to the loop parameter ],Old=[]}in
increasing order unless the reserved word @key{reverse} is present, in which
case the values are assigned in decreasing order.
@begin{Honest}
  @ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0294-1]}
  @ChgAdded{Version=[5],Text=[This wording does not describe when the loop
     parameter
     object(s) are created. That creation has no side-effects (other than
     possibly raising Storage_Error, but anything can do that), so we
     simplified the wording by leaving it out. Each object has to be created
     before any iteration that depends on it starts, but we do not (for
     instance) require that the objects are all created at once at the start
     of the loop, nor that the objects are created after the elaboration of the
     @nt{discrete_subtype_definition}.]}
@end{Honest}
@begin{Ramification}
  @ChgRef{Version=[5],Kind=[Deleted],ARef=[AI12-0294-1]}
  @ChgDeleted{Version=[5],Text=[The order of creating the loop parameter and
  evaluating the @nt{discrete_subtype_definition} doesn't matter, since the
  creation of the loop parameter has no side effects (other than possibly
  raising Storage_Error, but anything can do that).]}

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

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0251-1],ARef=[AI12-0294-1]}
@ChgAdded{Version=[5],Text=[If a @nt{chunk_specification} with a
@nt{discrete_subtype_definition} is present, then the logical thread of control
associated with a given chunk has its own copy of the chunk parameter
initialized with a distinct value from the discrete subtype defined by the
@nt{discrete_subtype_definition}. The values of the chunk parameters are
assigned such that they increase with increasing values of the ranges covered
by the corresponding loop parameters.]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0251-1]}
@ChgAdded{Version=[5],Text=[Whether or not a @nt{chunk_specification}
is present in a parallel loop,
the total number of iterations of the loop represents an upper bound
on the number of logical threads of control devoted to the loop.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0262-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0250-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[@Redundant[For details about the execution of a
@nt{loop_statement} with the @nt{iteration_scheme}
@Chg{Version=[5],New=[including an],Old=[being @key[for]]}
@nt{iterator_specification}, see @RefSecNum{Generalized Loop Iteration}.@Chg{Version=[5],New=[
For details relating to a @nt{procedural_iterator}, see
@RefSecNum{Procedural Iterators}.],Old=[]}]]}

@end{RunTime}

@begin{Notes}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0250-1]}
A loop parameter@Chg{Version=[5],New=[ declared by a
@nt{loop_parameter_specification}],Old=[]} is a constant;
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

@begin{WideAbove}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0119-1],ARef=[AI12-0312-1]}
@ChgAdded{Version=[5],Type=[Leading],KeepNext=[T],
Text=[@i{Example of a simple parallel loop:}]}
@end{WideAbove}
@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[-- @ExamCom{see @RefSecNum{Array Types}}
@key[parallel]
@key[for] I @key[in] Grid'Range(1) @key[loop]
   Grid(I, 1) := (@key[for all] J @key[in] Grid'Range(2) => Grid(I,J) = True);
@key[end loop];]}
@end{Example}

@begin{WideAbove}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0312-1]}
@ChgAdded{Version=[5],Type=[Leading],KeepNext=[T],
Text=[@i{Example of a parallel loop with a chunk specification:}]}
@end{WideAbove}
@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key[declare]
   @key[subtype] Chunk_Number @key[is] Natural @key[range] 1 .. 8;]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   Partial_Sum,
   Partial_Max : @key[array] (Chunk_Number) @key[of] Natural := (@key[others] => 0);
   Partial_Min : @key[array] (Chunk_Number) @key[of] Natural := (@key[others] => Natural'Last);]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text={@key[begin]
   @key[parallel] (Chunk @key[in] Chunk_Number)
   @key[for] I @key[in] Grid'Range(1) @key[loop]
      @key[declare]
         True_Count : @key[constant] Natural :=
           [@key[for] J @key[in] Grid'Range(2) => (@key[if] Grid (I, J) @key[then] 1 @key[else] 0)]'Reduce("+",0);
      @key[begin]
         Partial_Sum (Chunk) := @@ + True_Count;
         Partial_Min (Chunk) := Natural'Min(@@, True_Count);
         Partial_Max (Chunk) := Natural'Max(@@, True_Count);
      @key[end];
   @key[end loop];}}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[   Put_Line("Total=" & Partial_Sum'Reduce("+", 0) &
            ", Min=" & Partial_Min'Reduce(Natural'Min, Natural'Last) &
            ", Max=" & Partial_Max'Reduce(Natural'Max, 0));
@key[end];]}

@begin{WideAbove}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0312-1]}
@ChgAdded{Version=[5],Text=[@i{For an example of an} @nt{iterator_filter}@i{,
see @RefSecNum{Quantified Expressions}.}]}
@end{WideAbove}
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

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0119-1],ARef=[AI12-0251-1],ARef=[AI12-0266-1],ARef=[AI12-0294-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}Parallel loops
  are new.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0250-1]}
  @ChgAdded{Version=[5],Text=[An @nt{iterator_filter} is now allowed on
  @nt{loop_parameter_specification}s. This is mainly for consistency with
  aggregate and reduction iterators, where it eliminates the need for
  temporary objects.]}
@end{Extend2012}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0071-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Updated wording of
  loop execution to use the new term "satisfies the predicates"
  (see @RefSecNum{Subtype Predicates}).]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0061-1]}
  @ChgAdded{Version=[5],Text=[Added text so that the nominal subtype of a
  loop parameter is clearly defined.]}
@end{Diffword2012}


@LabeledAddedSubClause{Version=[3],Name=[User-Defined Iterator Types]}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Type=[Leading],Keepnext=[T],Text=[The following
language-defined generic library package exists:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0241-1]}
@ChgAdded{Version=[3],Text=[@ChildUnit{Parent=[Ada],Child=[Iterator_Interfaces]}@key[generic]
   @key[type] Cursor;
   @key[with function] Has_Element (Position : Cursor) @key[return] Boolean;
@key[package] Ada.Iterator_Interfaces@Chg{Version=[5],New=[],Old=[ @key[is]]}
   @Chg{Version=[5],New=[@key[with] Pure, Nonblocking => False @key[is]],Old=[@key[pragma] Pure (Iterator_Interfaces);]}]}

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

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0266-1]}
@ChgAdded{Version=[5],Text=[   @key[type] @AdaTypeDefn{Parallel_Iterator} @key[is limited interface and] Forward_Iterator;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0266-1]}
@ChgAdded{Version=[5],Text=[   @key[subtype] @AdaSubtypeDefn{Name=[Chunk_Index],Of=[Positive]} @key[is] Positive;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0266-1]}
@ChgAdded{Version=[5],Text=[   @key[function] @AdaSubDefn{Is_Split} (Object : Parallel_Iterator)
      @key[return] Boolean @key[is abstract];]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0266-1]}
@ChgAdded{Version=[5],Text=[   @key[procedure] @AdaSubDefn{Split_Into_Chunks} (Object     : @key[in out] Parallel_Iterator;
                                Max_Chunks : @key[in]     Chunk_Index) @key[is abstract]
      @key[with] Pre'Class   => @key[not] Object.Is_Split @key[or else raise] Program_Error,
           Post'Class  => Object.Is_Split @key[and then]
                          Object.Chunk_Count <= Max_Chunks;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0266-1]}
@ChgAdded{Version=[5],Text=[   @key[function] @AdaSubDefn{Chunk_Count} (Object : Parallel_Iterator)
      @key[return] Chunk_Index @key[is abstract]
      @key[with] Pre'Class   => Object.Is_Split @key[or else raise] Program_Error;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0266-1]}
@ChgAdded{Version=[5],Text=[   @key[function] @AdaSubDefn{First} (Object : Parallel_Iterator;
                   Chunk  : Chunk_Index) @key[return] Cursor @key[is abstract]
      @key[with] Pre'Class   => (Object.Is_Split @key[and then]
                              Chunk <= Object.Chunk_Count)
                           @key[or else raise] Program_Error;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0266-1]}
@ChgAdded{Version=[5],Text=[   @key[function] @AdaSubDefn{Next} (Object   : Parallel_Iterator;
                  Position : Cursor;
                  Chunk    : Chunk_Index) @key[return] Cursor @key[is abstract]
      @key[with] Pre'Class   => (Object.Is_Split @key[and then]
                              Chunk <= Object.Chunk_Count)
                           @key[or else raise] Program_Error;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0266-1]}
@ChgAdded{Version=[5],Text=[   @key[type] @AdaTypeDefn{Parallel_Reversible_Iterator} @key[is limited interface]
      @b[and] Parallel_Iterator @key[and] Reversible_Iterator;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[end] Ada.Iterator_Interfaces;]}
@end{Example}

@begin{Reason}
  @ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0241-1]}
  @ChgAdded{Version=[5],Text=[This package must allow blocking
    (Nonblocking => False) for compatibility. The purpose of this package
    is to provide a template for overriding user-defined routines; and
    such routines can only allow blocking if the root type does so.
    Users can still declare their overridding routines nonblocking if they
    wish.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[An @i<iterator type> is a type descended from
the Forward_Iterator interface from some instance of
Ada.Iterator_Interfaces.@Defn{iterator type}
A @i<reversible iterator type> is a type descended from the Reversible_Iterator
interface from some instance of Ada.Iterator_Interfaces.@Defn{reversible iterator type}
@Chg{Version=[5],New=[A @i{parallel iterator type}@Defn{parallel iterator type}
is a type descended from the Parallel_Iterator interface from some instance
of Ada.Iterator_Interfaces. A type descended from the
Parallel_Reversible_Iterator interface from some
instance of Ada.Iterator_Interfaces is both a parallel iterator type and a
reversible iterator type. ],Old=[]}An @i<iterator object>
is an object of an iterator type.@Defn{iterator object}
A @i<reversible iterator object> is an object
of a reversible iterator type.@Defn{reversible iterator object}
@Chg{Version=[5],New=[A @i<parallel iterator object> is an object
of a parallel iterator type.@Defn{parallel iterator object} ],Old=[]}The
formal subtype Cursor from the associated
instance of Ada.Iterator_Interfaces is the @i<iteration cursor subtype> for the
iterator type.@Defn{iteration cursor subtype}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The following type-related
operational aspects may be specified for an indexable container type @i<T> (see
@RefSecNum{User-Defined Indexing}):]}

@begin{Description}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0111-1]}
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
    for
    @i<T>.@Defn{default cursor subtype}@AspectDefn{Default_Iterator}@Chg{Version=[5],New=[
    This aspect is inherited by descendants
    of type @i<T> (including @i<T>'Class).],Old=[]}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Default_Iterator],
    Text=[@ChgAdded{Version=[3],Text=[Default iterator to be used in @key[for]
    loops.]}]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0111-1]}
  @ChgAdded{Version=[3],Text=[Iterator_Element@\This aspect is specified by a
    @nt{name} that denotes a subtype. This is the @i<default element subtype>
    for @i<T>.@Defn{default element subtype}@AspectDefn{Iterator_Element}@Chg{Version=[5],New=[
    This aspect is inherited by descendants
    of type @i<T> (including @i<T>'Class).],Old=[]}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Iterator_Element],
    Text=[@ChgAdded{Version=[3],Text=[Element type to be used for user-defined
      iterators.]}]}

  @ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0111-1]}
  @ChgAdded{Version=[5],Type=[Leading],Text=[Iterator_View@\This aspect is
    specified by a @nt{name} that denotes a type @i<T2> with the
    following properties:@AspectDefn{Iterator_View}]}

@begin{Itemize}
    @ChgRef{Version=[5],Kind=[Added]}
    @ChgAdded{Version=[5],Text=[@i<T2> is declared in the same compilation
      unit as @i<T>;]}

    @ChgRef{Version=[5],Kind=[Added]}
    @ChgAdded{Version=[5],Text=[@i<T2> is an iterable container type;]}

    @ChgRef{Version=[5],Kind=[Added]}
    @ChgAdded{Version=[5],Text=[@i<T2> has a single discriminant which is an
      access discriminant designating @i<T>; and]}

    @ChgRef{Version=[5],Kind=[Added]}
    @ChgAdded{Version=[5],Text=[The default iterator subtypes for @i<T> and
      @i<T2> statically match.]}
@end{Itemize}

  @ChgRef{Version=[5],Kind=[Added]}
  @ChgAdded{Version=[5],NoPrefix=[T],Text=[This aspect is never
    inherited@Redundant[, even by @i<T>'Class].]}

  @begin{Reason}
    @ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Text=[Iterator_View allows specifying an alternative
    type to be automatically used by container element iterators; see
    @RefSecNum{Generalized Loop Iteration}. This allows setting state for
    an iteration only once rather than for each individual reference.]}
  @end{Reason}

  @begin{Ramification}
    @ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Text=[Since Iterator_View is not inherited, it does
    not apply to @i<T>'Class. Otherwise, the type of the iterator object would
    not be known at compile-time (since it necessarily has to be different for
    each descendant).]}
  @end{Ramification}

  @ChgAspectDesc{Version=[5],Kind=[AddedNormal],Aspect=[Iterator_View],
    Text=[@ChgAdded{Version=[5],Text=[An alternative type to used for container
      element iterators.]}]}

@end{Description}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Deleted],ARef=[AI12-0111-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[],Old=[These aspects are
inherited by descendants of type @i<T> (including @i<T>'Class).]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[An @i<iterable container type> is an indexable container type
with specified Default_Iterator and Iterator_Element aspects.@Defn{iterable container type}
A @i<reversible iterable container type> is an iterable container type with the default iterator type
being a reversible iterator type.@Defn{reversible iterable container type}
@Chg{Version=[5],New=[A @i{parallel iterable container type} is an iterable
container type with the default iterator type being a parallel iterator
type.@Defn{parallel iterable container type} ],Old=[]}An
@i<iterable container object> is an object of an iterable container type.@Defn{iterable container object}
A @i<reversible iterable container object> is an object of a reversible iterable container
type.@Defn{reversible iterable container object}@Chg{Version=[5],New=[
A @i<parallel iterable container object> is an object of a
parallel iterable container type.@Defn{parallel iterable container object}],Old=[]}]}

@ChgToGlossary{Version=[3],Kind=[Added],Term=<Iterable container type>,
Text=<@ChgAdded{Version=[3],Text=[An iterable container type is one that has
user-defined behavior for iteration, via the Default_Iterator and
Iterator_Element aspects.]}>}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0138-1]}
@ChgAdded{Version=[4],Text=[The Default_Iterator and Iterator_Element aspects
are nonoverridable (see @RefSecNum{Aspect Specifications}).]}

@begin{Reason}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[This ensures that all descendants of an
  iterable container type have aspects with the same properties. This prevents
  generic contract problems with formal derived types.]}
@end{Reason}
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

@begin{Incompatible2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0138-1]}
  @ChgAdded{Version=[4],Text=[@Defn{incompatibilities with Ada 2012}@b<Corrigendum:>
  Defined Default_Iterator and Iterator_Element to be nonoveridable, which
  makes redefinitions and hiding of these aspects illegal. It's possible that
  some program could violate one of these new restrictions, but in most cases
  this can easily be worked around by using overriding rather than
  redefinition.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0266-1]}
  @ChgAdded{Version=[5],Text=[Various new types and subprograms are newly added
  to Ada.Iterator_Interfaces. If an instance of Ada.Iterator_Interfaces is
  referenced in a @nt{use_clause}, and an entity with one of the new names
  is defined in some other package that is also referenced in a @nt{use_clause},
  the user-defined entity may no longer be use-visible,
  resulting in errors. This should be rare and is easily fixed if it does occur.]}
@end{Incompatible2012}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}Aspect Iterator_View
  is new; it allows container element iterators to set the tampering state
  once rather than for each use of the element.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0266-1]}
  @ChgAdded{Version=[5],Text=[Parallel iterator
  interfaces are new; they allow user-defined parallel loops to be defined.]}
@end{Extend2012}



@LabeledAddedSubClause{Version=[3],Name=[Generalized Loop Iteration]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[Generalized forms of loop iteration are provided by
an @nt{iterator_specification}.]}
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0156-1],ARef=[AI12-0250-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<iterator_specification>,Old=<>}>,
rhs="@Chg{Version=[3],New=<
    @Syn2{defining_identifier} @Chg{Version=[5],New=<[: @Syn2{loop_parameter_subtype_indication}] >,Old=<>}@key[in] [@key{reverse}] @SynI{iterator_}@Syn2{name}@Chg{Version=[5],New=<
      [@Syn2{iterator_filter}]>,Old=<>}
  | @Syn2{defining_identifier} [: @Chg{Version=[5],New=<@Syn2{loop_parameter_subtype_indication}>,Old=<@Syn2{subtype_indication}>}] @key[of] [@key{reverse}] @SynI{iterable_}@Syn2{name}>,Old=<>}@Chg{Version=[5],New=<
      [@Syn2{iterator_filter}]>,Old=<>}"}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0156-1]}
@AddedSyn{Version=[5],lhs=<@Chg{Version=[5],New=<loop_parameter_subtype_indication>,Old=<>}>,
rhs="@Chg{Version=[5],New=<@Syn2{subtype_indication} | @Syn2{access_definition}>,Old=<>}"}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0266-1]}
@ChgAdded{Version=[5],Text=[If an @nt{iterator_specification} is for a parallel
construct, the reserved word @key[reverse] shall not appear in the
@nt{iterator_specification}.]}
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
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[If the reserved word @key[reverse] appears,
the @nt{iterator_specification} is a
@Defn{reverse iterator}@Defn2{Term=[iterator],Sec=[reverse]}@i<reverse iterator>@Chg{Version=[5],New=[.
If the @nt{iterator_specification} is for a parallel construct, the
@nt{iterator_specification} is a
@i<parallel iterator>.@Defn{parallel iterator}@Defn2{Term=[iterator],Sec=[parallel]}
Otherwise,],Old=[; otherwise]} it is a
@i<forward iterator>.@Defn{forward iterator}@Defn2{Term=[iterator],Sec=[forward]}
@Chg{Version=[5],New=[Forward and reverse iterators are collectively called
@i<sequential>
iterators.@Defn{sequential iterator}@Defn2{Term=[iterator],Sec=[sequential]} ],Old=[]}In a
reverse generalized iterator, the
@SynI<iterator_>@nt{name} shall be of a reversible iterator type.
@Chg{Version=[5],New=[In a parallel generalized iterator, the
@SynI<iterator_>@nt{name} shall be of a parallel iterator type. ],Old=[]}In
a reverse container element iterator, the default iterator type for the type
of the @SynI<iterable_>@nt{name} shall be a reversible iterator
type.@Chg{Version=[5],New=[ In a parallel container element iterator, the
default iterator type for the type of the @SynI<iterable_>@nt{name} shall be
of a parallel iterator type.],Old=[]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0151-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0156-1],ARef=[AI12-0183-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[The subtype defined by the
@nt{loop_parameter_subtype_indication}, if any, of a generalized iterator
shall statically match the iteration cursor
subtype. ],Old=[]}The @Chg{Version=[4],New=[subtype defined by],Old=[type of]}
the @Chg{Version=[5],New=[@nt{loop_parameter_subtype_indication}],
Old=[@nt{subtype_indication}]}, if any, of an array component
iterator shall @Chg{Version=[4],New=[statically match],Old=[cover]} the
component @Chg{Version=[4],New=[subtype],Old=[type]} of the type of the
@SynI<iterable_>@nt{name}. The @Chg{Version=[4],New=[subtype defined
by],Old=[type of]} the @Chg{Version=[5],New=[@nt{loop_parameter_subtype_indication}],
Old=[@nt{subtype_indication}]}, if any, of a container element
iterator shall @Chg{Version=[4],New=[statically match],Old=[cover]} the default
element @Chg{Version=[4],New=[subtype],Old=[type]} for the type of the
@SynI<iterable_>@nt{name}.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[In a container element iterator whose
@SynI<iterable_>@nt{name} has type @i<T>, if the @SynI<iterable_>@nt{name}
denotes a constant or the Variable_Indexing aspect is not specified for @i<T>,
then the Constant_Indexing aspect shall be specified for @i<T>.]}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0047-1]}
@ChgAdded{Version=[4],Text=[The @SynI<iterator_>@nt{name} or
@SynI<iterable_>@nt{name} of an @nt{iterator_specification} shall
not denote a subcomponent that depends on discriminants of an object
whose nominal subtype is unconstrained, unless the object is known
to be constrained.]}

@begin{Reason}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[This is the same rule that applies to
  renames; it serves the same purpose of preventing the object from
  disappearing while the iterator is still using it.]}
@end{Reason}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0120-1]}
@ChgAdded{Version=[4],Text=[A container element iterator is illegal if the
call of the default iterator function that creates the loop iterator
(see below) is illegal.]}

@begin{Ramification}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[This can happen if the parameter to the default
  iterator function is @key[in out] and the @SynI<iterable_>@nt{name} is a
  constant. The wording applies to any reason that the call would be illegal,
  as it's possible that one of the default parameters would be illegal, or
  that some accessibility check would fail.]}
@end{Ramification}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0120-1]}
@ChgAdded{Version=[4],Text=[A generalized iterator is illegal if the iteration
cursor subtype of the @SynI<iterator_>@nt{name} is a limited type at the point
of the generalized iterator. A container element iterator is illegal if the
default cursor subtype of the type of the @SynI<iterable_>@nt{name} is a limited
type at the point of the container element iterator.]}

@begin{Reason}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[If the cursor type is limited, the assignment to
  the loop parameter for a generalized iterator would be illegal. The same is
  true for a container element iterator. We have to say "at the point of the
  iterator" as the limitedness of a type can change due to visibility.]}
@end{Reason}

@end{Legality}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0269-1],ARef=[AI05-0292-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0156-1]}
@ChgAdded{Version=[3],Text=[An @nt{iterator_specification} declares a
@i<loop parameter>.@Defn{loop parameter}
In a generalized iterator, @Chg{Version=[5],New=[],Old=[the nominal subtype of
the loop parameter is the iteration cursor subtype. In ]}an array component
iterator@Chg{Version=[5],New=[,],Old=[]} or a container element iterator, if
a @Chg{Version=[5],New=[@nt{loop_parameter_subtype_indication}],
Old=[@nt{subtype_indication}]} is present, it
determines the nominal subtype of the loop parameter.@Chg{Version=[5],New=[ In
a generalized iterator, if a @nt{loop_parameter_subtype_indication} is not
present, the nominal subtype of the loop parameter is the iteration cursor
subtype.],Old=[]} In an array component iterator, if a
@Chg{Version=[5],New=[@nt{loop_parameter_subtype_indication}],
Old=[@nt{subtype_indication}]} is not present, the
nominal subtype of the loop parameter is the component subtype of the
type of the @SynI{iterable_}@nt{name}. In a container element iterator, if a
@Chg{Version=[5],New=[@nt{loop_parameter_subtype_indication}],
Old=[@nt{subtype_indication}]} is not present, the nominal subtype of the loop
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

@begin{Ramification}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0093-1]}
  @ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0156-1]}
  @ChgAdded{Version=[4],Text=[The loop parameter of a generalized iterator has
  the same accessibility as the loop statement. This means that the loop
  parameter object is finalized when the loop statement is left. (It also may be
  finalized as part of assigning a new value to the loop parameter.) For array
  component iterators@Chg{Version=[5],New=[],Old=[and container element
  iterators]}, the loop parameter directly denotes an element of the array
  @Chg{Version=[5],New=[],Old=[or container ]}and has the accessibility of
  the associated array@Chg{Version=[5],New=[],Old=[ or container]}.
  @Chg{Version=[5],New=[For container element iterators, the loop parameter
  denotes the result of the indexing function call (in the case of a constant
  indexing) or a generalized reference thereof (in the case of a variable
  indexing). Roughly speaking, the loop parameter has the accessibility level
  of a single iteration of the loop. More precisely, the function result (or
  the generalized reference thereof) is considered to be renamed in the
  declarative part of a notional block statement which immediately encloses
  the loop's @nt{sequence_of_statements}; the accessibility of the loop
  parameter is that of the block statement.],Old=[]}]}
@end{Ramification}

@end{StaticSem}

@begin{Runtime}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgAdded{Version=[3],Text=[For the execution of a @nt{loop_statement} with
an @nt{iterator_specification}, the @nt{iterator_specification} is
first elaborated. This elaboration elaborates the @nt{subtype_indication},
if any.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0250-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[For a @Chg{Version=[5],New=[sequential
],Old=[]}generalized iterator, the loop parameter is created, the
@SynI{iterator_}@nt{name} is evaluated, and the denoted iterator object becomes
the @i<loop iterator>.@Defn{loop iterator} In a forward generalized iterator,
the operation First of the iterator type is called on the loop iterator, to
produce the initial value for the loop parameter. If the result of calling
Has_Element on the initial value is False, then the execution of the
@nt{loop_statement} is complete. Otherwise, the @nt{sequence_of_statements} is
@Chg{Version=[5],New=[conditionally ],Old=[]}executed and then the
Next operation of the iterator type is called with the
loop iterator and the current value of the loop parameter to produce the next
value to be assigned to the loop parameter. This repeats until the result of
calling Has_Element on the loop parameter is False, or the loop is left as a
consequence of a transfer of control. For a reverse generalized iterator, the
operations Last and Previous are called rather than First and Next.]}

@begin{Ramification}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0093-1]}
  @ChgAdded{Version=[4],Text=[The loop parameter of a generalized iterator is a
  variable of which the user only has a constant view. It follows the normal
  rules for a variable of its nominal subtype. In particular, if the nominal
  subtype is indefinite, the variable is constrained by its initial value.
  Similarly, if the nominal subtype is class-wide, the variable (like all
  variables) has the tag of the initial value. Constraint_Error may be raised by
  a subsequent iteration if Next or Previous return an object with a different
  tag or constraint.]}
@end{Ramification}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0266-1]}
@ChgAdded{Version=[5],Text=[For a parallel generalized iterator, the
@nt{chunk_specification}, if any, of the associated parallel construct, is first
elaborated, to determine the maximum number of chunks (see
@RefSecNum{Loop Statements}), and then the operation Split_Into_Chunks of the
iterator type is called, with the determined maximum passed as the Max_Chunks
parameter, specifying the upper bound for the number of loop parameter objects
(and the number of logical threads of control) to be associated with the
iterator. In the absence of a @nt{chunk_specification}, the maximum number of
chunks is determined in an implementation-defined manner.]}

@ChgImplDef{Version=[5],Kind=[AddedNormal],InitialVersion=[5],
Text=[@ChgAdded{Version=[5],Text=[The maximum number of chunks for a parallel
generalized iterator without a @nt{chunk_specification}.]}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0250-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[5],Text=[Upon return from Split_Into_Chunks, the actual
number of chunks for the loop is determined by calling the Chunk_Count operation
of the iterator, at which point one logical thread of control is initiated for
each chunk, with an associated chunk index in the range from one to the actual
number of chunks. Within each logical thread of control, a loop parameter is
created. If a @nt{chunk_specification} with a @nt{discrete_subtype_definition}
is present in the associated parallel construct, then a chunk parameter is
created, and initialized with a value from the discrete subtype defined by the
@nt{discrete_subtype_definition}, so that the order of the chosen chunk
parameter values correspond to the order of the chunk indices associated with
the logical threads of control. The operation First of the iterator type having
a Chunk parameter is called on the loop iterator, with Chunk initialized from
the corresponding chunk index, to produce the initial value for the loop
parameter. If the result of calling Has_Element on this initial value is False,
then the execution of the logical thread of control is complete. Otherwise, the
@nt{sequence_of_statements} is conditionally executed and then the Next
operation of the iterator type having a Chunk parameter is called, with the loop
iterator, the current value of the loop parameter, and the corresponding chunk
index, to produce the next value to be assigned to the loop parameter. This
repeats until the result of calling Has_Element on the loop parameter is False,
or the associated parallel construct is left as a consequence of a transfer of
control. In the absence of a transfer of control, the associated parallel
construct of a parallel generalized iterator is complete when all of its logical
threads of control are complete.]}

@begin{Discussion}
  @ChgRef{Version=[5],Kind=[Added]}
  @ChgAdded{Version=[5],Text=[The Max_Chunks parameter of the Split_Into_Chunks
    procedure is an upper bound for the number of chunks to be associated with a
    loop. A container implementation may opt for a lower value for the number of
    chunks if a more optimal split can be determined. For instance, a tree-based
    container might create the split based on the number of branches at the top
    levels of the tree.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0250-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[For an array component iterator,
@Chg{Version=[5],New=[the @nt{chunk_specification} of the associated
parallel construct, if any, is first elaborated to determine
the maximum number of chunks (see @RefSecNum{Loop Statements}), and
then ],Old=[]}the @SynI<iterable_>@nt{name} is evaluated and
the denoted array object becomes the @i<array for the
loop>.@Defn{array for a loop} If the array for the loop is a null array,
then the execution of the @nt{loop_statement} is complete. Otherwise, the
@nt{sequence_of_statements} is
@Chg{Version=[5],New=[conditionally ],Old=[]}executed with the loop parameter
denoting each component of the array for the loop, using a @i<canonical> order
of components,@Defn{canonical order of array components}
which is last dimension varying fastest (unless the
array has convention Fortran, in which case it is first dimension
varying fastest). For a
forward array component iterator, the iteration starts with the
component whose index values are each the first in their index range,
and continues in the canonical order. For a reverse array component
iterator, the iteration starts with the component whose index values
are each the last in their index range, and continues in the reverse
of the canonical order.
@Chg{Version=[5],New=[For a parallel array component iterator, the iteration is
broken up into contiguous chunks of the canonical order, such that all
components are covered with no overlaps; each chunk has its own logical thread
of control with its own loop parameter and iteration within each chunk is in the
canonical order. The number of chunks is implementation defined, but is limited
in the presence of a @nt{chunk_specification} to the determined maximum.
],Old=[]}The loop iteration proceeds until the
@nt{sequence_of_statements} has been
@Chg{Version=[5],New=[conditionally ],Old=[]}executed for each component of the
array for the loop, or until the loop is left as a consequence of a
transfer of control.]}

@ChgImplDef{Version=[5],Kind=[AddedNormal],InitialVersion=[5],
Text=[@ChgAdded{Version=[5],Text=[The number of chunks for an array component
iterator.]}]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0266-1]}
@ChgAdded{Version=[5],Text=[If a @nt{chunk_specification} with a
@nt{discrete_subtype_definition} is present
in the associated parallel construct, then the logical thread of
control associated with a given chunk has a chunk parameter
initialized with a distinct value from the discrete subtype defined by
the @nt{discrete_subtype_definition}. The values of the chunk parameters
are assigned such that they increase in the canonical order of the
starting array components for the chunks.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0111-1],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[For a container element iterator,
@Chg{Version=[5],New=[the @nt{chunk_specification} of the associated
parallel construct, if any, is first elaborated to determine
the maximum number of chunks (see @RefSecNum{Loop Statements}), and
then ],Old=[]}the @SynI<iterable_>@nt{name} is evaluated@Chg{Version=[5],New=[.
If the container type has Iterator_View specified, an object of the
Iterator_View type is created with the discriminant referencing the
iterable container object denoted by the @SynI<iterable_>@nt{name}. This is
the @i<iterable container object for the loop>. Otherwise,],Old=[and]}
the iterable container object
@Chg{Version=[5],New=[denoted by the @SynI<iterable_>@nt{name} becomes the
iterable container object for the loop],Old=[becomes the @i<iterable
container object for the loop>]}.@Defn{iterable container object for a loop}
The default iterator function for the type of
the iterable container object for the loop is called on the iterable container object
and the result is the @i<loop iterator>.@Defn2{Term=[loop iterator],Sec=[container element iterator]}
@Chg{Version=[5],New=[For a sequential container element
iterator, an],Old=[An]} object of the default cursor subtype is created
(the @i<loop cursor>).@Defn{loop cursor}@Chg{Version=[5],New=[ For a parallel
container element iterator, each chunk of iterations
will have its own loop cursor, again of the default cursor subtype.],Old=[]}]}

@begin{Reason}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1]}
  @ChgAdded{Version=[5],Text=[If Iterator_View is specified, we add an extra
  object and use that object for this iteration. This allows these iterators
  to automatically use the stable view (defined in each of the language-defined
  containers) to do the iteration. That eliminates the need to set and clear
  the tampering with elements indication each time Reference is called; that
  eliminates substantial overhead as finalization is typically used to
  implement the tampering reset.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0139-2],ARef=[AI05-0292-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[@Chg{Version=[5],New=[A],Old=[For a]} container
element iterator@Chg{Version=[5],New=[ then proceeds as described above for a
generalized iterator, except that each reference to a loop parameter
is replaced by a reference to the corresponding loop cursor. For a
container element iterator, the loop parameter for each iteration
instead denotes],Old=[, the
operation First of the iterator type is called on the loop iterator, to produce
the initial value for the loop cursor. If the result of calling Has_Element on
the initial value is False, then the execution of the @nt{loop_statement} is
complete. Otherwise, the @nt{sequence_of_statements} is executed with the loop
parameter denoting]} an indexing (see @RefSecNum{User-Defined Indexing})
into the iterable container
object for the loop, with the only parameter to the indexing being the
@Chg{Version=[5],New=[],Old=[current ]}value of the loop
cursor@Chg{Version=[5],New=[ for the given iteration],Old=[; then the Next
operation of the iterator type is called with the loop iterator and the loop
cursor to produce the next value to be assigned to the loop cursor. This repeats
until the result of calling Has_Element on the loop cursor is False, or until
the loop is left as a consequence of a transfer of control. For a reverse
container element iterator, the operations Last and Previous are called rather
than First and Next]}. If the loop parameter is a constant (see above), then the
indexing uses the default constant indexing function for the type of the
iterable container object for the loop; otherwise it uses the default variable
indexing function.]}



@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0120-1]}
@ChgAdded{Version=[4],Text=[Any exception propagated by the execution of a
generalized iterator or container element iterator is propagated by the
immediately enclosing loop statement.]}

@begin{Ramification}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[This text covers exceptions raised by called
  functions that make up the execution of the iterator as well as
  exceptions raised by the assignment to the loop parameter or cursor.]}
@end{Ramification}
@end{Runtime}

@begin{Examples}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0269-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0266-1]}
@ChgAdded{Version=[3],Text=[-- @Examcom{Array component iterator example:}
@Chg{Version=[5],New=[@key[parallel]
],Old=[]}@key[for] Element @key[of] Board @key[loop]  -- @Examcom{See @RefSecNum{Index Constraints and Discrete Ranges}.}
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

@begin{Incompatible2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0047-1]}
  @ChgAdded{Version=[4],Text=[@Defn{incompatibilities with Ada
  2012}@b<Corrigendum:> Added a rule to ensure that the object being iterated
  cannot be a component that could disappear before the loop completes. This
  could be incompatible by making a loop that was legal (and worked correctly,
  so long as the enclosing object is not modified during the loop) from the
  original Ada 2012 illegal in corrected Ada 2012. Such loops should be pretty
  rare, especially as these iterator forms are new to Ada 2012.]}

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0120-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Added rules to reject loops
  if the call to the default iterator function for a container element
  iterator is illegal, or if the cursor type of an iterator is limited.
  These are formally incompatible with original Ada 2012, but as it's unlikely
  that any Ada 2012 compiler ever allowed the illegal usages in an expansion
  of a loop (it's much more likely that they would have just caused an internal
  error in the compiler), this should have no effect in practice.]}

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0151-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Added a requirement that the
  given subtype statically match the subtype of the element or component for
  a component element iterator or array component iterator. Original Ada 2012
  text allowed any type that covered the subtype of the element or component,
  but that led to questions of what the meaning was if they are different.
  In this case, the element is essentially a renaming of the container element,
  and it doesn't make sense for the constraints to be different. Ignoring
  explicitly defined constraints in renames is a mistake that we don't want
  to continue, thus we require static matching. This means that some programs
  might be illegal, but those programs were misleading at best, and
  potentially would raise unexpected exceptions because the element values
  might have been invalid or abnormal with respect to the declared constraint.]}
@end{Incompatible2012}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0156-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}For consistency, we
  now allow a @nt{subtype_indication} on a generalized iterator, and anonymous
  access types on all forms of iterator. We introduced a new syntax
  non-terminal, @nt{loop_parameter_subtype_indication} to simplfy the wording.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0250-1]}
  @ChgAdded{Version=[5],Text=[An @nt{iterator_filter} is now allowed on
  @nt{iterator_specification}s. This is mainly for consistency with
  aggregate and reduction iterators, where it eliminates the need for
  temporary objects.]}
@end{Extend2012}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0120-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Added wording to specify that
  a loop propagates any exceptions propagated by the execution of an iterator.
  Since that's what naturally would happen from a macro-style expansion of the
  parts of an iterator, and no other interpretation makes sense given the way
  the rest of Ada works, we consider it so unlikely that any Ada 2012
  implementation ever did anything else that we don't document this as a
  possible inconsistency.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0111-1]}
  @ChgAdded{Version=[5],Text=[Added wording to include the use of the iterator
  view in a container element iterator.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0266-1]}
  @ChgAdded{Version=[5],Text=[Added wording to describe the execution of
  parallel iterators.]}
@end{DiffWord2012}


@LabeledAddedSubClause{Version=[5],Name=[Procedural Iterators]}

@begin{Intro}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1]}
@ChgAdded{Version=[5],Text=[A @nt{procedural_iterator} invokes a
user-defined procedure, passing in the body of the enclosing @nt{loop_statement}
as a parameter of an anonymous access-to-procedure type, to allow the loop body
to be executed repeatedly as part of the invocation of the user-defined
procedure.]}
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1],ARef=[AI12-0250-1]}
@AddedSyn{Version=[5],lhs=<@Chg{Version=[5],New=<procedural_iterator>,Old=<>}>,
rhs="@Chg{Version=[5],New=<
     @Syn2{iterator_parameter_specification} @key[of] @Syn2{iterator_procedure_call}
       [@Syn2{iterator_filter}]>,Old=<>}"}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1],ARef=[AI12-0308-1]}
@AddedSyn{Version=[5],lhs=<@Chg{Version=[5],New=<iterator_parameter_specification>,Old=<>}>,
rhs="@Chg{Version=[5],New=<
     @Syn2{formal_part}
   | (@Syn2{defining_identifier}{, @Syn2{defining_identifier}})>,Old=<>}"}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1]}
@AddedSyn{Version=[5],lhs=<@Chg{Version=[5],New=<iterator_procedure_call>,Old=<>}>,
rhs="@Chg{Version=[5],New=<
     @SynI{procedure_}@Syn2{name}
   | @SynI{procedure_}@Syn2{prefix} @Syn2{iterator_actual_parameter_part}>,Old=<>}"}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1]}
@AddedSyn{Version=[5],lhs=<@Chg{Version=[5],New=<iterator_actual_parameter_part>,Old=<>}>,
rhs="@Chg{Version=[5],New=<
     (@Syn2{iterator_parameter_association} {, @Syn2{iterator_parameter_association}})>,Old=<>}"}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1]}
@AddedSyn{Version=[5],lhs=<@Chg{Version=[5],New=<iterator_parameter_association>,Old=<>}>,
rhs="@Chg{Version=[5],New=<
     @Syn2{parameter_association}
   | @Syn2{parameter_association_with_box}>,Old=<>}"}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1]}
@AddedSyn{Version=[5],lhs=<@Chg{Version=[5],New=<parameter_association_with_box>,Old=<>}>,
rhs="@Chg{Version=[5],New={
   [ @SynI{formal_parameter_}@Syn2{selector_name} => ] <>},Old=<>}"}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1]}
@ChgAdded{Version=[5],Text=[At most one @nt{iterator_parameter_association} within an
@nt{iterator_actual_parameter_part} shall be a
@nt{parameter_association_with_box}.]}
@end{Syntax}

@begin{Resolution}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1],ARef=[AI12-0292-1],ARef=[AI12-0326-2]}
@ChgAdded{Version=[5],Text=[The @nt{name} or @nt{prefix} given in an
@nt{iterator_procedure_call} shall resolve to denote a callable entity @i<C>
(the @i<iterating procedure>@Defn{iterating procedure})
that is a procedure, or an entry renamed as (viewed as) a procedure.
@Redundant[When there is an @nt{iterator_actual_parameter_part}, the @nt{prefix}
can be an @nt{implicit_dereference} of an access-to-subprogram value.]]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1]}
@ChgAdded{Version=[5],Text=[An @nt{iterator_procedure_call} without a
@nt{parameter_association_with_box} is
equivalent to one with an @nt{iterator_actual_parameter_part} with an
additional @nt{parameter_association_with_box} at the end, with the
@SynI{formal_parameter_}@nt{selector_name} identifying the last formal
parameter of the callable entity denoted by the @nt{name} or @nt{prefix}.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1],ARef=[AI12-0320-1]}
@ChgAdded{Version=[5],Text=[An @nt{iterator_procedure_call} shall contain at
most one @nt{iterator_parameter_association} for each formal parameter of the
callable entity @i<C>. Each formal parameter without an
@nt{iterator_parameter_association} shall have a
@nt{default_expression} (in the profile of the view of @i<C> denoted by the
@nt{name} or @nt{prefix}).]]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1]}
@ChgAdded{Version=[5],Text=[The formal parameter of the callable entity @i<C>
associated with the @nt{parameter_association_with_box} shall be of an
anonymous access-to-procedure type @i<A>.]}
@end{Resolution}


@begin{Legality}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1],ARef=[AI12-0308-1]}
@ChgAdded{Version=[5],Text=[The anonymous access-to-procedure type @i<A> shall
have at least one formal parameter in its parameter profile. If the
@nt{iterator_parameter_specification} is a @nt{formal_part}, then this
@nt{formal_part} shall be mode conformant with that of @i<A>. If the
@nt{iterator_parameter_specification} is a list of @nt{defining_identifier}s,
the number of formal parameters of @i<A> shall be the same as the length of
this list.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1],ARef=[AI12-0292-1]}
@ChgAdded{Version=[5],Text=[@Redundant[If the @nt{name} or @nt{prefix} given in an
@nt{iterator_procedure_call} denotes an abstract subprogram, the subprogram
shall be a dispatching subprogram.]]}

@begin{TheProof}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0320-1]}
  @ChgAdded{Version=[5],Text=[This is stated normatively in
  @RefSecNum{Abstract Types and Subprograms}.]}
@end{TheProof}

@end{Legality}

@begin{StaticSem}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1],ARef=[AI12-0250-1],ARef=[AI12-0308-1],ARef=[AI12-0326-2]}
@ChgAdded{Version=[5],Text=[A @nt{loop_statement} with an @nt{iteration_scheme}
that has a @nt{procedural_iterator} is equivalent to a local declaration of a
procedure P followed by a @nt{procedure_call_statement} that is formed from the
@nt{iterator_procedure_call} by replacing the <> of the
@nt{parameter_association_with_box} with P'Access. The @nt{formal_part} of the
locally declared procedure P is formed from the @nt{formal_part} of the
anonymous access-to-procedure type @i<A>, by replacing the @nt{identifier} of
each formal parameter of this @nt{formal_part} with the @nt{identifier} of the
corresponding formal parameter or element of the list of
@nt{defining_identifier}s given in the @nt{iterator_parameter_specification}.
The body of @i<P> consists of the conditionally executed
@nt{sequence_of_statements}. The procedure P is called the
@i<loop body procedure>.@Defn{loop body procedure}]}

@begin{ImplNote}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0250-1]}
  @ChgAdded{Version=[5],Type=[Leading],Text=[For a @nt{procedural_iterator}
  with an @nt{iterator_filter}, the body of the routine would be something like:]}

@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key{procedure} @i<P> ... @key{is}
@key{begin}
   @key{if} @nt{iterator_filter} @key{then}
      @nt{sequence_of_statements}
   @key{end if};
@key{end} @i<P>;]}
@end{Example}
@end{ImplNote}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1],ARef=[AI12-0326-2]}
@ChgAdded{Version=[5],Type=[Leading],Text=[The following aspects may be specified
for a callable entity @i<S> that has at least one formal parameter of an
anonymous access-to-subprogram type:]}

@begin{Description}
   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[Allows_Exit@\The Allows_Exit aspect is of type
   Boolean. The specified value shall be static. The Allows_Exit aspect of an
   inherited primitive subprogram is True if Allows_Exit is True either for the
   corresponding subprogram of the progenitor type or for any other inherited
   subprogram that it overrides. If not specified or inherited as True, the
   Allows_Exit aspect of a callable entity is False. For an
   entry, only a confirming specification of False is permitted for the
   Allows_Exit aspect.]}

   @begin{Reason}
     @ChgRef{Version=[5],Kind=[AddedNormal]}
     @ChgAdded{Version=[5],Text=[An entry does not allow exit, because
     implementing a transfer of control out of a task or protected entry creates
     unnecessarily complex dynamic semantics.]}
   @end{Reason}

   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],NoPrefix=[T],Text=[Specifying the Allows_Exit aspect
   to be True for a subprogram indicates that the
   subprogram @i<allows exit>,@Defn2{Term=[subprogram],Sec=[allows exit]}@Defn{allows exit}
   meaning that it is prepared to be completed by arbitrary transfers of
   control from the loop body procedure@Redundant[, including propagation
   of exceptions. A subprogram for which
   Allows_Exit is True should use finalization as appropriate rather than
   exception handling to recover resources and make any necessary final
   updates to data structures].]}

   @ChgAspectDesc{Version=[5],Kind=[AddedNormal],Aspect=[Allows_Exit],
     Text=[@ChgAdded{Version=[5],Text=[An indication of whether a subprogram
     will operate correctly for arbitrary transfers of control.]}]}
   @begin{Ramification}
     @ChgRef{Version=[5],Kind=[AddedNormal]}
     @ChgAdded{Version=[5],Text=[A subprogram that does not need cleanup
     satisfies the requirements, and thus can specify Allows_Exit as True.
     If a subprogram @i<S> allows exit, it cannot expect to get control other
     than via finalization if the loop body procedure initiates a transfer of
     control as part of a @nt{procedural_iterator}. In particular, exception
     handlers in @i<S>, even @key[when others] handlers, will not be executed
     when a transfer of control occurs. The mechanism that the implementation
     uses to implement such transfers of control needs to avoid triggering
     exception handlers.]}
   @end{Ramification}

   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[Parallel_Iterator@\The Parallel_Iterator aspect
     is of type Boolean. The specified value shall be static. The
     Parallel_Iterator aspect of an inherited primitive subprogram is True if
     Parallel_Iterator is True either for the corresponding subprogram of the
     progenitor type or for any other inherited subprogram that it overrides. If
     not specified or inherited as True, the Parallel_Iterator aspect of a
     callable entity is False.]}

   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],NoPrefix=[T],Text=[Specifying the Parallel_Iterator
     aspect to be True for a callable entity indicates that the entity might
     invoke the loop body procedure from multiple distinct logical threads of
     control. The Parallel_Iterator aspect for a subprogram shall be statically
     False if the subprogram allows exit.]}

   @ChgAspectDesc{Version=[5],Kind=[AddedNormal],Aspect=[Parallel_Iterator],
     Text=[@ChgAdded{Version=[5],Text=[An indication of whether
      a subprogram may use multiple threads of control to invoke a
      loop body procedure.]}]}

   @begin{Reason}
     @ChgRef{Version=[5],Kind=[AddedNormal]}
     @ChgAdded{Version=[5],Text=[Permitting exit from a parallel procedural
       iterator introduces additional semantic and implementation complexity.]}
   @end{Reason}

@end{Description}
@end{StaticSem}

@begin{Legality}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1],ARef=[AI12-0326-2]}
@ChgAdded{Version=[5],Text=[If a callable entity overrides an inherited
dispatching subprogram that allows exit, the overriding callable entity also
shall allow exit. If a callable entity overrides an inherited dispatching
subprogram that has a True Parallel_Iterator aspect, the overriding callable
entity also shall have a True Parallel_Iterator aspect.]}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[Since an entry never allows exit, attempting
  to implement an allows exit subprogram with a task or protected entry
  is always illegal. However, the Parallel_Iterator aspect can be
  applied to an entry, so a subprogram with the Parallel_Iterator aspect
  True can be implemented by an entry.]}
@end{Ramification}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0326-2]}
@ChgAdded{Version=[5],Text=[A @nt{loop_statement} with a
@nt{procedural_iterator} as its @nt{iteration_scheme}
shall begin with the reserved word @key[parallel] if and only if the callable
entity identified in the @nt{iterator_procedure_call} has a Parallel_iterator
aspect of True.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1],ARef=[AI12-0326-2]}
@ChgAdded{Version=[5],Text=[The @nt{sequence_of_statements} of a
@nt{loop_statement} with a @nt{procedural_iterator} as its @nt{iteration_scheme}
shall contain an @nt{exit_statement}, return statement, @nt{goto_statement}, or
@nt{requeue_statement} that leaves the loop only if the callable entity
associated with the @nt{procedural_iterator} allows exit.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0294-1]}
@ChgAdded{Version=[5],Text=[The @nt{sequence_of_statements} of a
@nt{loop_statement} with a @nt{procedural_iterator} as its @nt{iteration_scheme}
shall not contain an @nt{accept_statement} whose @nt{entry_declaration} occurs
outside the @nt{loop_statement}.]}

@begin{Reason}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[An @nt{accept_statement} is not allowed in a
  procedure (see 9.5.2), it has to be directly in a @nt{task_body}. Since the
  loop body here is implemented as  a procedure, we can't allow
  @nt{accept_statement}s there, either, even if the loop itself is directly in
  a @nt{task_body}.]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[This includes cases where the
  @nt{accept_statement} is part of another construct, for instance, a
  @nt{select_statement}.]}
@end{Ramification}

@end{Legality}

@begin{Runtime}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0326-2]}
@ChgAdded{Version=[5],Text=[@Redundant[For the execution of a
  @nt{loop_statement} with an @nt{iteration_scheme} that has a
  @nt{procedural_iterator}, the procedure denoted by the @nt{name} or
  @nt{prefix} of the @nt{iterator_procedure_call} (the @i<iterating
  procedure>)@Defn{iterating procedure} is invoked, passing an access value
  designating the loop body procedure as a parameter. The iterating procedure
  then calls the loop body procedure zero or more times and returns, whereupon
  the @nt{loop_statement} is complete. If the @key[parallel] reserved word is
  present, the iterating procedure might invoke the loop body procedure from
  multiple distinct logical threads of control.]]}

@begin{TheProof}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[The stated dynamic semantics are implied by the
    static semantics given above and the bounded errors given below.]}
@end{TheProof}

@end{Runtime}

@begin{Bounded}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0326-2]}
@ChgAdded{Version=[5],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
If the callable entity identified in the @nt{iterator_procedure_call} allows
exit, then it is a bounded error for a call of the loop body procedure
to be performed from within an abort-deferred operation (see
@RefSecNum{Abort of a Task - Abort of a Sequence of Statements}),
unless the entire @nt{loop_statement} was within the same abort-deferred
operation. If detected, Program_Error is raised at the point of the
call; otherwise, a transfer of control from the @nt{sequence_of_statements}
of the @nt{loop_statement} might not terminate the @nt{loop_statement}, and the
loop body procedure might be called
again.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0326-2]}
@ChgAdded{Version=[5],Text=[If a @nt{loop_statement} with the
@nt{procedural_iterator} as its @nt{iteration_scheme}
(see @RefSecNum{Loop Statements}) does not begin with the reserved word
@key[parallel], it is a bounded error if the loop body procedure is invoked
from a different logical thread of control than the one that initiates
the @nt{loop_statement}. If detected, Program_Error is raised; otherwise,
conflicts associated with concurrent executions of the loop body
procedure can occur without being detected by the applicable conflict
check policy (see @RefSecNum{Conflict Check Policies}). Furthermore, propagating
an exception or making an attempt to exit in the presence of multiple threads of
control might not terminate the @nt{loop_statement}, deadlock might occur, or
the loop body procedure might be called
again.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}

  @begin{Discussion}
    @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0326-2]}
    @ChgAdded{Version=[5],Text=[Other Ada rules are still in effect for the
    allows exit subprogram @i<A>, of course. For instance, if a transfer of
    control causes finalization which raises an exception, Program_Error will be
    propagated by @i<A> (rather than the transfer of control). In such a case,
    the bounded error above would still apply. Another example is the case where
    an unrelated task is waiting on the normal completion of the loop body
    procedure call in @i<A>. Such a task might end up waiting forever if a
    transfer of control happens (this is a deadlock situation). This case does
    not require additional wording, as the same thing would happen if an
    exception is propagated from the loop body procedure or if @i<A> executed a
    transfer of control (such as a return statement).]}
  @end{Discussion}

@end{Bounded}

@begin{Examples}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1],ARef=[AI12-0379-1]}
@ChgAdded{Version=[5],Type=[Leading],Text=[Example of iterating over a map from
My_Key_Type to My_Element_Type (see @RefSecNum{Maps}):]}

@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key[for] (C : Cursor) @key[of] My_Map.Iterate @key[loop]
   Put_Line (My_Key_Type'Image (Key (C)) & " => " &
      My_Element_Type'Image (Element (C)));
@key[end loop];]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[--@Examcom{ The above is equivalent to:}]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key[declare]
   @key[procedure] P (C : Cursor) @key[is]
   @key[begin]
      Put_Line (My_Key_Type'Image (Key (c)) & " => " &
         My_Element_Type'Image (Element (C)));
   @key[end] P;
@key[begin]
   My_Map.Iterate (P'Access);
@key[end];]}
@end{Example}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0189-1]}
@ChgAdded{Version=[5],Type=[Leading],Text=[Example of iterating over the
environment variables (see @RefSecNum{The Package Environment_Variables}):]}

@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key[for] (Name, Val) @key[of] Ada.Environment_Variables.Iterate(<>) @key[loop]
   --@examcom{  "(<>)" is optional because it is the last parameter}
   Put_Line (Name & " => " & Val);
@key[end loop];]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[--@Examcom{ The above is equivalent to:}]}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key[declare]
   @key[procedure] P (Name : String; Val : String) @key[is]
   @key[begin]
      Put_Line (Name & " => " & Val);
   @key[end] P;
@key[begin]
   Ada.Environment_Variables.Iterate (P'Access);
@key[end];]}
@end{Example}
@end{Examples}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],
    ARef=[AI12-0189-1],ARef=[AI12-0292-1],ARef=[AI12-0294-1],ARef=[AI12-0326-2]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}Procedural
  iterators, and the Allows_Exit and Parallel_Iterator aspects are
  new in Ada 202x.]}
@end{Extend2012}


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


@LabeledAddedSubClause{Version=[5], Name=[Parallel Block Statements]}

@begin{Intro}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0119-1]}
@ChgAdded{Version=[5],Text=[@Redundant[A @nt{parallel_block_statement}
comprises two or more @nt{handled_sequence_of_statements} separated by
@key[and] where each represents an independent activity that is intended to
proceed concurrently with the others.]]}

@end{Intro}

@begin{Syntax}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0119-1]}
@AddedSyn{Version=[5],lhs=<@Chg{Version=[5],New=<parallel_block_statement>,Old=<>}>,
rhs="@Chg{Version=[5],New=<
    @key[parallel] @key[do]
       @Syn2{handled_sequence_of_statements}
    @key[and]
       @Syn2{handled_sequence_of_statements}
   {@key[and]
       @Syn2{handled_sequence_of_statements}}
    @key[end do];>,Old=<>}"}

@end{Syntax}

@begin{StaticSem}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0119-1]}
@ChgAdded{Version=[5],Text=[
Each @nt{handled_sequence_of_statements} represents a separate logical thread
of control that proceeds independently and concurrently. The
@nt{parallel_block_statement} is complete once every one of the
@nt{handled_sequence_of_statements} has completed, either by reaching the end
of its execution, or due to a transfer of control out of the construct
by one of the @nt{handled_sequence_of_statements}
(see @RefSecNum{Simple and Compound Statements - Sequences of Statements}).]}

@begin{ImplNote}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0119-1]}
  @ChgAdded{Version=[5],Text=[Although each @nt{handled_sequence_of_statements}
  of a parallel block represents a separate logical thread of control, the
  implementation may choose to combine two or more such logical threads
  of control into a single physical thread of control to reduce the cost
  of creating numerous physical threads of control.]}
@end{ImplNote}
@end{StaticSem}

@begin{Examples}
@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0119-1]}
@ChgAdded{Version=[5],Text=[@key[procedure] Traverse (T : Expr_Ptr) @key[is] --@Examcom{ see @RefSecNum{Tagged Types and Type Extensions}}
@key[begin]
   @key[if] T /= @key[null] @key[and then]
      T.@key[all] @key[in] Binary_Operation'Class --@Examcom{ see @RefSecNum{Type Extensions}}
   @key[then] --@Examcom{ recurse down the binary tree}
      @key[parallel do]
         Traverse (T.Left);
      @key[and]
         Traverse (T.Right);
      @key[and]
         Ada.Text_IO.Put_Line
            ("Processing " & Ada.Tags.Expanded_Name (T'Tag));
      @key[end do];
   @key[end if];
@key[end] Traverse;]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0119-1]}
@ChgAdded{Version=[5],Text=[@key[function] Search (S : String; Char : Character) @key[return] Boolean @key[is]
@key[begin]
   @key[if] S'Length <= 1000 @key[then]
       --@Examcom{ Sequential scan}
       @key[return] (@key[for] @key[some] C @key[of] S => C = Char);
   @key[else]
       --@Examcom{ Parallel divide and conquer}
       @key[declare]
          Mid : @key[constant] Positive := S'First + S'Length/2 - 1;
       @key[begin]
          @key[parallel do]
             @key[for] C @key[of] S(S'First .. Mid) @key[loop]
                @key[if] C = Char @key[then]
                   @key[return] True;  --@Examcom{ Terminates enclosing @key[do]}
                @key[end if];
             @key[end loop];
          @key[and]
             @key[for] C @key[of] S(Mid + 1 .. S'Last) @key[loop]
                @key[if] C = Char @key[then]
                   @key[return] True;  --@Examcom{ Terminates enclosing @key[do]}
                @key[end if];
             @key[end loop];
          @key[end do];
          --@Examcom{ Not found}
          @key[return] False;
       @key[end];
   @key[end if];
@key[end] Search;]}
@end{Example}
@end{Examples}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0119-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}
  The @nt{parallel_block_statement} is new.]}
@end{Extend2012}


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
