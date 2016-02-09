@Part(09, Root="ada.mss")

@Comment{$Date: 2015/04/03 04:12:42 $}
@LabeledSection{Tasks and Synchronization}

@Comment{$Source: e:\\cvsroot/ARM/Source/09.mss,v $}
@Comment{$Revision: 1.124 $}

@begin{Intro}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@PDefn2{Term=[execution], Sec=(Ada program)}
The execution of an Ada program consists of the execution of one
or more @i(tasks).
@Defn{task}
@Defn2{Term=[interaction], Sec=(between tasks)}
Each task represents a separate thread of
control that proceeds independently and concurrently
between the points where it @i(interacts) with other tasks.
The various forms of task interaction are
described in this @Chg{Version=[3],New=[clause],Old=[section]}, and include:
@IndexSee{Term=[parallel processing],See=(task)}
@Defn{synchronization}
@IndexSee{Term=[concurrent processing],See=(task)}
@IndexSeeAlso{Term=[intertask communication],See=(task)}
@begin(Honest)
  The execution of an Ada program consists of the execution
  of one or more partitions (see @RefSecNum(Program Execution)),
  each of which in turn consists of the execution of an environment task
  and zero or more subtasks.
@end(Honest)
@begin(itemize)
the activation and termination of a task;

@Defn{protected object}
a call on a protected subprogram of a @i(protected object),
providing exclusive read-write access, or concurrent read-only
access to shared data;

a call on an entry, either of another task,
allowing for synchronous communication with that task,
or of a protected object, allowing for asynchronous
communication with one or more other tasks using that same protected
object;

a timed operation, including a simple delay statement,
a timed entry call or accept, or a timed asynchronous
select statement (see next
item);

an asynchronous transfer of control as part of an asynchronous
select statement, where a task
stops what it is doing and begins execution at a different
point in response to the completion of an entry call or
the expiration of a delay;

an abort statement, allowing one task to cause the
termination of another task.
@end(itemize)

In addition, tasks can communicate indirectly by
reading and updating (unprotected) shared
variables, presuming the access is properly synchronized through
some other kind of task interaction.

@end{Intro}

@begin{StaticSem}
@Defn{task unit}
The properties of a task are defined by a corresponding task declaration
and @nt<task_body>, which together define a program unit
called a @i(task unit).
@end{StaticSem}

@begin{RunTime}
Over time, tasks proceed through various @i(states).
@PDefn2{Term=[task state], Sec=(inactive)}
@Defn2{Term=[inactive], Sec=(a task state)}
@PDefn2{Term=[task state], Sec=(blocked)}
@Defn2{Term=[blocked], Sec=(a task state)}
@PDefn2{Term=[task state], Sec=(ready)}
@Defn2{Term=[ready], Sec=(a task state)}
@PDefn2{Term=[task state], Sec=(terminated)}
@Defn2{Term=[terminated], Sec=(a task state)}
A task is initially @i(inactive); upon activation, and prior to its
@i{termination}
it is either @i(blocked) (as part
of some task interaction) or @i(ready) to run.
@Defn2{Term=[execution resource], Sec=(required for a task to run)}
While ready, a task competes for the available
@i(execution resources) that it requires to run.
@begin(Discussion)
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
  @Defn{task dispatching policy}
  @Defn{dispatching policy for tasks}
  The means for selecting which of the ready tasks to run,
  given the currently available execution resources, is determined by the
  @i(task dispatching policy) in effect, which is generally
  implementation defined, but may be controlled by
  @Chg{Version=[3],New=[aspects, ],Old=[]}pragmas@Chg{Version=[3],New=[, ],Old=[]}
  and operations defined in the Real-Time Annex
  (see @RefSecNum(Priority Scheduling) and @RefSecNum(Dynamic Priorities)).
@end(Discussion)
@end{RunTime}

@begin{Notes}

Concurrent task execution may be implemented on
multicomputers, multiprocessors, or with interleaved execution on a single
physical processor. On the other hand, whenever an implementation can
determine that the required semantic effects can be achieved when
parts of the execution of a
given task are performed by different physical processors acting in
parallel, it may choose to perform them in this way.

@end{Notes}

@begin{DiffWord83}
The introduction has been rewritten.

We use the term "concurrent" rather than "parallel" when talking
about logically independent execution of threads of control.
The term "parallel" is reserved for referring to the
situation where multiple physical processors run simultaneously.
@end{DiffWord83}


@LabeledClause{Task Units and Task Objects}

@begin{Intro}
@Defn{task declaration}
A task unit is declared by a @i(task declaration), which has
a corresponding @nt<task_body>. A task declaration may be
a @nt<task_type_declaration>, in which case it declares
a named task type; alternatively, it may be a @nt<single_task_declaration>,
in which case it defines an anonymous task type, as well as declaring
a named task object of that type.
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<task_type_declaration>,rhs="
   @key{task} @key{type} @Syn2{defining_identifier} [@Syn2{known_discriminant_part}]@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]} [@key{is}@Chg{Version=[2],New=<
     [@key{new} @Syn2{interface_list} @key{with}]
    >,Old=<>} @Syn2{task_definition}];"}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00399-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<single_task_declaration>,rhs="
   @key{task} @Syn2{defining_identifier} @Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]}[@key{is}@Chg{Version=[2],New=<
     [@key{new} @Syn2{interface_list} @key{with}]
    >,Old=<>} @Syn2{task_definition}];"}


@Syn{lhs=<task_definition>,rhs="
     {@Syn2{task_item}}
  [ @key{private}
     {@Syn2{task_item}}]
  @key{end} [@SynI{task_}@Syn2{identifier}]"}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@Syn{lhs=<task_item>,rhs="@Syn2{entry_declaration} | @Chg{New=[@Syn2{aspect_clause}],Old=[@Syn2{representation_clause}]}"}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0267-1]}
@Syn{lhs=<task_body>,rhs="
   @key{task} @key{body} @Syn2{defining_identifier}@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]} @key{is}
     @Syn2{declarative_part}
   @key{begin}
     @Syn2{handled_sequence_of_statements}
   @key{end} [@SynI{task_}@Syn2{identifier}];"}

@begin{SyntaxText}
If a @SynI{task_}@nt{identifier} appears at the
end of a @nt{task_definition} or @nt{task_body},
it shall repeat the @nt{defining_identifier}.
@end{SyntaxText}
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00345-01]}@ChgNote{This was just moved below}
@ChgDeleted{Version=[2],Text=[@PDefn2{Term=[requires a completion], Sec=(@nt{task_declaration})}
A task declaration requires a completion@redundant[,
which shall be a @nt{task_body},]
and every @nt{task_body} shall be the completion of some
task declaration.]}
@begin(Honest)
  @ChgRef{Version=[2],Kind=[Deleted]}
  @ChgDeleted{Version=[2],Text=[The completion can be a @nt{pragma} Import,
  if the implementation supports it.]}
@end(Honest)
@end{Legality}
@begin{NotIso}
@ChgAdded{Version=[3],Noparanum=[T],Text=[@Shrink{@i<Paragraph 8 was
deleted.>}]}@Comment{This message should be deleted if the paragraphs
are ever renumbered.}
@end{NotIso}


@begin{StaticSem}

A @nt<task_definition> defines a task type and its first subtype.
@PDefn2{Term=[visible part], Sec=(of a task unit)}
The first list of @nt{task_item}s of a @nt{task_@!definition},
together with the @nt{known_@!discriminant_@!part}, if any,
is called the visible part of the task unit.
@Redundant[@PDefn2{Term=[private part], Sec=(of a task unit)}
The optional list of @nt{task_item}s after the reserved
word @key{private} is called the private part of the task unit.]
@begin{TheProof}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
Private part is defined in @Chg{Version=[3],New=[Clause],Old=[Section]}
@RefSecNum{Visibility Rules}.
@end{theproof}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0029],ARef=[AI95-00116-01]}
@ChgAdded{Version=[1],Text=[For a task declaration without a
@nt{task_definition}, a
@nt{task_definition} without @nt{task_item}s is assumed.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01],ARef=[AI95-00397-01],ARef=[AI95-00399-01],ARef=[AI95-00419-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0042-1]}
@ChgAdded{Version=[2],Text=[For a task declaration
with an @nt{interface_list}, the task type
inherits user-defined primitive subprograms from each progenitor
type (see @RefSecNum{Interface Types}), in the same way that a derived type
inherits user-defined primitive subprograms from its progenitor types (see
@RefSecNum{Derived Types and Classes}). If the first
parameter of a primitive inherited subprogram is of the task type or an access
parameter designating the task type, and there is an @nt{entry_declaration} for
a single entry with the same identifier within the task declaration,
whose profile is type conformant with the
prefixed view profile of the inherited subprogram, the inherited subprogram is
said to be @i{implemented} by the conforming task entry@Chg{Version=[3],
New=[ using an implicitly declared nonabstract subprogram which
has the same profile as the inherited subprogram and which
overrides it@PDefn2{Term=[override],Sec=[when implemented by]}],
Old=[]}.@PDefn2{Term=[implemented],
Sec=[by a task entry]}@Defn2{Term=[type conformance],Sec=(required)}]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The inherited subprograms can only come from an
  interface given as part of the task declaration.]}
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0042-1]}
  @ChgAdded{Version=[3],Text=[The part about the implicitly declared
  subprogram is needed so that a subprogram implemented by an entry is
  considered to be overridden for the purpose of the other rules of the
  language. Without it, it would for instance be illegal for an abstract
  subprogram to be implemented by an entry, because the abstract subprogram
  would not be overridden. The @LegalityTitle below
  ensure that there is no conflict between the implicit overriding subprogram
  and a user-defined overriding subprogram.]}
@end{Reason}
@end{StaticSem}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}@ChgNote{This was just moved, not changed}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=[requires a completion], Sec=(@nt{task_declaration})}
A task declaration requires a completion@redundant[,
which shall be a @nt{task_body},]
and every @nt{task_body} shall be the completion of some
task declaration.]}
@begin(Honest)
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[If],Old=[The completion
  can be a @nt{pragma} Import, if]} the implementation supports
  it@Chg{Version=[3],New=[, the task body can be imported
  (using aspect Import, see @RefSecNum{Interfacing Aspects}),
  in which case no explicit @nt{task_body} is allowed],Old=[]}.]}
@end(Honest)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01],ARef=[AI95-00399-01]}
@ChgAdded{Version=[2],Text=[@Redundant[Each @i{interface_}@nt{subtype_mark} of an
@nt{interface_list} appearing within a task declaration shall denote
a limited interface type that is not a protected interface.]]}
@begin(TheProof)
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[@RefSecNum{Interface Types} requires that an
  @nt{interface_list} only name interface types, and limits the descendants of
  the various kinds of interface types. Only a limited, task, or
  synchronized interface can have a task type descendant. Nonlimited or
  protected interfaces are not allowed, as they offer operations that a task
  does not have.]}
@end(TheProof)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00397-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0090-1]}
@ChgAdded{Version=[2],Text=[The prefixed view profile of an explicitly
declared primitive subprogram of a tagged task type shall not be type
conformant with any entry of the task type, if the @Chg{Version=[3],New=[
subprogram has the same defining name as the entry and the ],Old=[]}first
parameter of the subprogram is of the task type or is an
access parameter designating the task type.@Defn2{Term=[type conformance],Sec=(required)}]}
@begin(Reason)
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This prevents the existence of two operations
  with the same name and profile which could be called with a prefixed view.
  If the operation was inherited, this would be illegal by the following rules;
  this rule puts inherited and noninherited routines on the same footing.
  Note that this only applies to tagged task types (that is, those with an
  interface in their declaration); we do that as there is no problem with
  prefixed view calls of primitive operations for @lquotes@;normal@rquotes
  task types, and having this rule apply to all tasks would be incompatible
  with Ada 95.]}
@end(Reason)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01],ARef=[AI95-00399-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For each primitive subprogram
inherited by the type declared by a task declaration, at most one of the
following shall apply:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[the inherited subprogram is overridden with a
primitive subprogram of the task type, in which case the overriding subprogram
shall be subtype conformant with the inherited subprogram and not abstract;
or@Defn2{Term=[subtype conformance],Sec=(required)}]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01],ARef=[AI95-00397-01]}
@ChgAdded{Version=[2],Text=[the inherited subprogram is implemented by a
single entry of the task type; in which case its prefixed view profile
shall be subtype conformant with that of the task entry.
@Defn2{Term=[subtype conformance],Sec=(required)}]}

@begin(Ramification)
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[An entry may implement two subprograms from the
  ancestors, one whose first parameter is of type @i<T> and one whose first
  parameter is of type @key{access} @i{T}. That doesn't cause implementation
  problems because @lquotes@;implemented by@rquotes (unlike
  @lquotes@;overridden@rquote) probably entails the creation of wrappers.]}
@end(Ramification)

@end{Itemize}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[If neither applies, the inherited subprogram shall be a
null procedure. @PDefn{generic contract issue}In addition to the places where
@LegalityTitle normally apply (see @RefSecNum{Generic Instantiation}),
these rules also apply in the private part of an instance of a generic unit.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Each inherited subprogram can only have a single
implementation (either from overriding a subprogram or implementing an entry),
and must have an implementation unless the subprogram is a null procedure.]}
@end{Reason}

@end{Legality}

@begin{RunTime}
@redundant[@PDefn2{Term=[elaboration], Sec=(task declaration)}
The elaboration of a task declaration elaborates the @nt<task_definition>.
@PDefn2{Term=[elaboration], Sec=(single_task_declaration)}
The elaboration of a @nt<single_@!task_@!declaration> also creates
an object of an (anonymous) task type.]
@begin(TheProof)
  This is redundant with the general rules for the elaboration
  of a @nt<full_type_declaration> and an @nt<object_declaration>.
@end(TheProof)

@PDefn2{Term=[elaboration], Sec=(task_definition)}
@Redundant[The elaboration of a @nt<task_definition>
creates the task type and its first
subtype;] it also includes the elaboration of the @nt<entry_declaration>s
in the given order.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@PDefn2{Term=[initialization], Sec=(of a task object)}
As part of the initialization of a task object, any
@Chg{New=[@nt<aspect_clause>s],Old=[@nt<representation_clause>s]} and
any per-object constraints associated with @nt<entry_@!declaration>s
of the corresponding @nt<task_@!definition> are elaborated in the given order.
@begin{Reason}
@ChgRef{Version=[1],Kind=[Revised]}
  The only @Chg{New=[@nt<aspect_clause>s],Old=[@nt<representation_clause>s]}
  defined for task entries are ones that specify the Address of an entry,
  as part of defining an interrupt entry.
  These clearly need to be elaborated per-object, not per-type.
  Normally the address will be a function of a discriminant,
  if such an Address clause is in a task type rather than a single task
  declaration, though it could rely on a parameterless function
  that allocates sequential interrupt vectors.

  We do not mention representation pragmas, since each
  pragma may have its own elaboration rules.
@end{Reason}

@PDefn2{Term=[elaboration], Sec=(task_body)}
The elaboration of a @nt{task_body} has no effect other than to establish
that tasks of the type can from then on be activated without
failing the Elaboration_Check.

@redundant[The execution of a @nt<task_body> is invoked by the activation of a
task of the corresponding type
(see @RefSecNum(Task Execution - Task Activation)).]

@leading@;The content of a task object of a given task type includes:
@begin(itemize)
  The values of the discriminants of the task object, if any;

  An entry queue for each entry of the task object;
  @begin(Ramification)
     "For each entry" implies one queue for each single entry,
      plus one for each entry of each entry family.
  @end(Ramification)

  A representation of the state of the associated task.
@end(itemize)

@end{RunTime}

@begin{Notes}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00382-01]}
@Chg{Version=[2],New=[Other than
in an @nt{access_definition}, the name of a task unit within],Old=[Within]}
the declaration or body of @Chg{Version=[2],New=[the],Old=[a]} task
unit@Chg{Version=[2],New=[],Old=[, the name of
the task unit]} denotes the current instance of the unit
(see @RefSecNum(The Context of Overload Resolution)),
rather than the first subtype of the corresponding task type (and
thus the name cannot be used as a @nt<subtype_mark>).
@begin(Discussion)
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00382-01]}
@Chg{Version=[2],New=[It can be used as a @nt{subtype_mark} in an anonymous
access type. In addition],Old=[However]}, it is possible to refer to
some other subtype of the task type within its body,
presuming such a subtype has been
declared between the @nt<task_type_declaration> and the @nt<task_body>.
@end(Discussion)

The notation of a @nt<selected_component> can be used to denote a discriminant
of a task (see @RefSecNum(Selected Components)).
Within a task unit, the name of a discriminant of the task type
denotes the corresponding discriminant of the current instance
of the unit.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
A task type is a limited type (see @RefSecNum(Limited Types)),
and hence @Chg{Version=[2],New=[precludes use of @nt{assignment_statement}s and],
Old=[has neither an assignment operation nor]} predefined equality operators.
If an application needs to store and exchange task identities, it
can do so by defining an access type designating the corresponding
task objects and by using access values for identification purposes.
Assignment is available for such an access type as for any
access type.
Alternatively, if the implementation supports the
Systems Programming Annex,
the Identity attribute
can be used for task identification
(see @Chg{Version=[2],New=[@RefSecNum(The Package Task_Identification)],
Old=[@RefSecNum(Task Information)]}).
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Examples of declarations of task types:}
@begin{Example}
@key(task) @key(type) Server @key(is)
   @key(entry) Next_Work_Item(WI : @key(in) Work_Item);
   @key(entry) Shut_Down;
@key(end) Server;

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@key(task) @key(type) Keyboard_Driver(ID : Keyboard_ID := New_ID) @key(is)@Chg{Version=[2],New=[
      @key(new) Serial_Device @key(with)  --@RI[ see @RefSecNum{Interface Types}]],Old=[]}
   @key(entry) Read (C : @key(out) Character);
   @key(entry) Write(C : @key(in)  Character);
@key(end) Keyboard_Driver;
@end{Example}

@leading@keepnext@i{Examples of declarations of single tasks:}
@begin{Example}
@key(task) Controller @key(is)
   @key(entry) Request(Level)(D : Item);  --@RI[  a family of entries]
@key(end) Controller;

@key(task) Parser @key(is)
   @key(entry) Next_Lexeme(L : @key(in)  Lexical_Element);
   @key(entry) Next_Action(A : @key(out) Parser_Action);
@key(end);

@key(task) User;  --@RI[  has no entries]
@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Examples of task objects:}
@end{WideAbove}
@begin{Example}
Agent    : Server;
Teletype : Keyboard_Driver(TTY_ID);
Pool     : @key(array)(1 .. 10) @key(of) Keyboard_Driver;
@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Example of access type designating task objects:}
@end{WideAbove}
@begin{Example}
@key(type) Keyboard @key(is) @key(access) Keyboard_Driver;
Terminal : Keyboard := @key(new) Keyboard_Driver(Term_ID);
@end{Example}

@end{Examples}

@begin{Extend83}
@ChgRef{Version=[1],Kind=[Revised]}
@Defn{extensions to Ada 83}
The syntax rules for task declarations are modified to allow a
@nt{known_discriminant_part}, and to allow a private part.
They are also modified to allow @nt{entry_declaration}s and
@Chg{New=[@nt<aspect_clause>s],Old=[@nt<representation_clause>s]} to be mixed.
@end{Extend83}

@begin{DiffWord83}
The syntax rules for tasks have been split up according to task types and
single tasks.
In particular:
The syntax rules for @ntf{task_declaration} and @ntf{task_specification} are
removed. The syntax rules for
@nt{task_type_declaration}, @nt{single_task_declaration}, @nt{task_definition}
and @nt{task_item} are new.

The syntax rule for @nt{task_body} now uses the nonterminal
@nt{handled_sequence_of_statements}.

The @nt{declarative_part} of a @nt{task_body} is now required;
that doesn't make any real difference,
because a @nt{declarative_part} can be empty.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01],ARef=[AI95-00397-01],ARef=[AI95-00399-01],ARef=[AI95-00419-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Task types and single tasks can be derived from one or more interfaces.
  Entries of the task type can implement the primitive operations of an
  interface. @nt{Overriding_indicator}s can be used to specify whether or not
  an entry implements a primitive operation.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0029],ARef=[AI95-00116-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that a task type has an
  implicit empty @nt{task_definition} if none is given.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0009],ARef=[AI95-00137-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Changed representation clauses
  to aspect clauses to reflect that they are used for more than just
  representation.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
  @ChgAdded{Version=[2],Text=[Revised the note on operations of task types to
  reflect that limited types do have an assignment operation, but not
  copying (@nt{assignment_statement}s).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00382-01]}
  @ChgAdded{Version=[2],Text=[Revised the note on use of the name of
  a task type within itself to reflect the exception for anonymous
  access types.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1],ARef=[AI05-0267-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in a
  @nt{task_type_declaration}, a @nt{single_task_declaration}, and a
  @nt{task_body}. This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0042-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified that an
  inherited procedure of a progenitor is overridden when it is
  implemented by an entry.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0090-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added the missing
  defining name in the no conflicting primitive operation rule.]}
@end{DiffWord2005}


@LabeledClause{Task Execution - Task Activation}

@begin{RunTime}

@PDefn2{Term=[execution], Sec=(task)}
The execution of a task of a given task type consists of the execution
of the corresponding @nt{task_body}.
@PDefn2{Term=[execution], Sec=(task_body)}
@Defn2{Term=[task], Sec=(execution)}
@Defn2{Term=[activation], Sec=(of a task)}
@Defn2{Term=[task], Sec=(activation)}
The initial part of this execution is called the @i(activation) of
the task; it consists of the elaboration of the @nt<declarative_part>
of the @nt<task_body>.
@Defn{activation failure}
Should an exception be propagated by the elaboration
of its @nt<declarative_part>,
the activation of the task is defined to have @i(failed),
and it becomes a completed task.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
A task object (which represents one task) can be @Chg{Version=[2],New=[a part
of a stand-alone object, of an object created by],Old=[created either as
part of the elaboration
of an @nt<object_@!declaration> occurring immediately within some
declarative region, or as part of the evaluation of]}
an @nt{allocator}@Chg{Version=[2],New=[, or of an anonymous object of a limited
type, or a coextension of one of these],Old=[]}. All
tasks@Chg{Version=[2],New=[ that are part or coextensions of any
of the stand-alone objects],Old=[]}
created by the elaboration of @nt<object_@!declaration>s@Chg{Version=[2],
New=[ (or @nt{generic_association}s of formal objects of
mode @key{in})],Old=[]}
of a single declarative region@Chg{Version=[2],
New=[],Old=[ (including subcomponents of the declared objects)]}
are activated together.
@Chg{Version=[2],New=[All tasks that are part or coextensions of a single
object that is not a stand-alone object are activated together.],Old=[Similarly,
all tasks created by the evaluation of a single @nt<allocator>
are activated together. The activation of a task is associated
with the innermost @nt<allocator> or @nt<object_@!declaration>
that is responsible for its creation.]}
@begin{Discussion}
The initialization of an @nt{object_declaration} or @nt{allocator} can
indirectly include the creation of other objects that contain tasks.
For example, the default expression for a subcomponent of an object
created by an @nt{allocator} might call a function that evaluates a
completely different @nt{allocator}. Tasks created by the two
allocators are @i{not} activated together.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
For @Chg{Version=[2],New=[the ],Old=[]}tasks@Chg{Version=[2],New=[],Old=[
created by the elaboration of @nt<object_declaration>s]}
of a given declarative region, the activations are initiated
within the context of the @nt<handled_@!sequence_of_@!statements>
(and its associated @nt<exception_@!handler>s if any @em
see @RefSecNum{Exception Handlers}), just prior to executing the
statements of the @Chg{Version=[2],New=[@nt{handled_sequence_of_statements}],
Old=[@ntf<_sequence>]}.
@Redundant[For a package without an explicit body or an explicit
@nt<handled_@!sequence_of_@!statements>,
an implicit body or an implicit @nt<null_@!statement> is assumed,
as defined in @RefSecNum(Package Bodies).]
@begin(Ramification)
  If Tasking_Error is raised, it can be handled by handlers of
  the @nt<handled_@!sequence_of_@!statements>.
@end(Ramification)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
For tasks @Chg{Version=[2],New=[that are part or coextensions of a single
object that is
not a stand-alone object, activations are initiated after completing any
initialization of the outermost object enclosing these tasks, prior
to performing any other operation on the outermost object. In
particular, for tasks that are part or coextensions of the object ],
Old=[]}created by the evaluation of an @nt<allocator>,
the activations are initiated as the last step of
evaluating the @nt<allocator>, @Chg{Version=[2],New=[],Old=[after completing
any initialization for the object created by the @nt<allocator>,
and ]}prior to returning the new access
value.@Chg{Version=[2],New=[ For tasks that are part or coextensions of an
object that is the result of a function call, the activations are
not initiated until after the function returns.],Old=[]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[The intent is that @lquotes@;temporary@rquotes@;
  objects with task parts (or coextensions) are treated similarly to an
  object created by an
  allocator. The @lquotes@;whole@rquotes@; object is initialized, and then all
  of the task parts (including the coextensions) are activated together. Each
  such @lquotes@;whole@rquotes@;
  object has its own task activation sequence, involving the activating task
  being suspended until all the new tasks complete their activation.]}
@end{Discussion}

@Defn2{Term=[activator], Sec=(of a task)}
@PDefn2{term=[blocked], Sec=(waiting for activations to complete)}
The task that created the new tasks and initiated their
activations (the @i(activator)) is
blocked until all of these activations complete (successfully
or not).
@Defn2{Term=[Tasking_Error],Sec=(raised by failure of run-time check)}
Once all of these activations are complete,
if the activation
of any of the tasks has failed
@Redundant[(due to the propagation of an exception)],
Tasking_Error is raised in the activator, at the place at which
it initiated the activations. Otherwise, the activator
proceeds with its execution normally. Any tasks that are aborted
prior to completing their activation are ignored when determining
whether to raise Tasking_Error.
@begin(Ramification)
  Note that a task created by an @nt<allocator> does not necessarily
  depend on its activator; in such a case the activator's termination
  can precede the termination of the newly created task.
@end(Ramification)
@begin(Discussion)
  Tasking_Error is raised only once, even if two or more
  of the tasks being activated fail their activation.
@end(Discussion)
@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00265-01]}
  @ChgAdded{Version=[2],Text=[The pragma Partition_Elaboration_Policy (see
  @RefSecNum{Pragma Partition_Elaboration_Policy})
  can be used to defer task activation to a later point, thus changing
  many of these rules.]}
@end{Honest}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0045-1]}
@Chg{Version=[3],New=[If the master that directly encloses the point where the
activation of a task @i<T> would be initiated, completes before the activation
of @i<T> is initiated, @i<T> becomes terminated and is never activated.
Furthermore, if a return statement is left such that the return object is not
returned to the caller, any task that was created as a part of the return
object or one of its coextensions immediately becomes],
Old=[Should the task that created
the new tasks never reach the point
where it would initiate the activations (due to an abort or
the raising of an exception),
the newly created tasks become]}
terminated @Chg{Version=[3],New=[and is],Old=[are]}
never activated.
@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0045-1]}
@ChgAdded{Version=[3],Text=[The first case can only happen if the activation
   point of T is not reached due to an exception being raised or a task or
   statement being aborted. Note that this is exclusive; if the master
   completes normally and starts finalization, we're already past the
   activation point.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0045-1]}
@ChgAdded{Version=[3],Text=[The second case can happen with an exception being
   raised in a return statement, by an exit or goto from an
   @nt{extended_return_statement}, or by a return statement being aborted. Any
   tasks created for the return object of such a return statement are never
   activated.]}
@end{Ramification}
@end{RunTime}

@begin{Notes}

An entry of a task can be called before the task has been activated.

If several tasks are activated together, the execution of any of these
tasks need not await the end of the activation of the other tasks.

A task can become completed during its activation either because of an
exception or because it is aborted
(see @RefSecNum(Abort of a Task - Abort of a Sequence of Statements)).

@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of task activation:}
@begin{Example}
@key(procedure) P @key(is)
   A, B : Server;    --@RI[  elaborate the task objects A, B]
   C    : Server;    --@RI[  elaborate the task object C]
@key(begin)
   --@RI[  the tasks A, B, C are activated together before the first statement]
   ...
@key(end);
@end{Example}

@end{Examples}

@begin{DiffWord83}

We have replaced the term @i{suspended} with @i{blocked},
since we didn't want to consider a task blocked when it was
simply competing for execution resources. "Suspended" is sometimes
used more generally to refer to tasks that are not actually running
on some processor, due to the lack of resources.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
This @Chg{Version=[3],New=[subclause],Old=[clause]} has been rewritten
in an attempt to improve presentation.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[Adjusted the wording for activating tasks to
  handle the case of anonymous function return objects. This is critical;
  we don't want to be waiting for the tasks in a return object when we exit
  the function normally.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0045-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected the wording that
  handles tasks that are never activated to ensure that no lookahead is implied
  and to make it clear that tasks created by return statements that never
  return are never activated.]}
@end{DiffWord2005}


@LabeledClause{Task Dependence - Termination of Tasks}

@begin{RunTime}

@leading@Defn2{Term=[dependence], Sec=(of a task on a master)}
@Defn2{Term=[task], Sec=(dependence)}
@Defn2{Term=[task], Sec=(completion)}
@Defn2{Term=[task], Sec=(termination)}
Each task (other than an environment task @em see @RefSecNum(Program Execution))
@i(depends) on one or more masters
(see @RefSecNum(Completion and Finalization)), as follows:
@begin(itemize)
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0070-1]}
If the task is created by the evaluation of an @nt<allocator>
for a given @Chg{Version=[4],New=[named ],Old=[]}access type,
it depends on each master that includes the
elaboration of the declaration of the ultimate ancestor of the given
access type.

If the task is created by the elaboration of an @nt<object_declaration>,
it depends on each master that includes this elaboration.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00416-01]}
@ChgAdded{Version=[2],Text=[Otherwise, the task depends on
the master of the outermost object of which it is a part (as determined by the
accessibility level of that object @em see
@RefSecNum{Operations of Access Types} and
@RefSecNum{Completion and Finalization}), as well as on any master whose
execution includes that of the master of the outermost object.]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[The master of a task created by a return
  statement changes when the accessibility of the return object changes. Note
  that its activation happens, if at all, only after the function returns and
  all accessibility level changes have occurred.]}
@end{Ramification}
@end(itemize)

@Defn2{term=[dependence], Sec=(of a task on another task)}
Furthermore, if a task depends on a given master, it is defined
to depend on the task that executes the master, and (recursively)
on any master of that task.
@begin{Discussion}
Don't confuse these kinds of dependences with the
dependences among compilation units defined in
@RefSec{Compilation Units - Library Units}.
@end{Discussion}

A task is said to be @i(completed) when the execution of its corresponding
@nt<task_body> is completed. A task is said to be @i(terminated) when
any finalization of the @nt<task_body> has been performed
(see @RefSecNum(Completion and Finalization)).
@Redundant[The first step of finalizing a master
(including a @nt<task_body>) is to
wait for the termination of any tasks dependent on the master.]
@PDefn2{Term=[blocked], Sec=(waiting for dependents to terminate)}
The task executing the master is blocked until all the dependents
have terminated. @Redundant[Any remaining finalization is then performed
and the master is left.]

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Doubled word}
@leading@;Completion of a task (and the corresponding @nt<task_body>) can
occur when the task is blocked at a @nt<select_@!statement> with an
@Chg{New=[],Old=[an ]}open @nt<terminate_alternative>
(see @RefSecNum(Selective Accept)); the open @nt<terminate_alternative>
is selected if and only if the following conditions are satisfied:
@begin{itemize}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00415-01]}
  The task depends on some completed master;@Chg{Version=[2],New=[ and],Old=[]}

  Each task that depends on the master considered is either already
  terminated or similarly blocked at a @nt<select_statement>
  with an open @nt{terminate_alternative}.
@end{itemize}

When both conditions are satisfied, the task considered becomes
completed, together with all tasks that depend on the master
considered that are not yet completed.
@begin(Ramification)
  Any required finalization is performed after the selection
  of @nt<terminate_alternative>s. The tasks are not callable
  during the finalization. In some ways it is as though they were
  aborted.
@end(Ramification)

@end{RunTime}

@begin{Notes}

The full view of a limited private type can be a task type, or
can have subcomponents of a task type. Creation of an object of
such a type creates dependences according to the full type.

An @nt<object_renaming_declaration> defines a new view of an
existing entity and hence creates no further dependence.

The rules given for the collective completion of a group
of tasks all blocked on @nt<select_statement>s with
open @nt<terminate_alternative>s ensure that the collective
completion can occur only when there are no remaining active
tasks that could call one of the tasks being collectively completed.

If two or more tasks are blocked on @nt<select_statement>s
with open @nt{terminate_alternative}s, and become
completed collectively, their finalization actions proceed concurrently.

@leading@keepnext@;The completion of a task can occur due to any of the following:
@begin{itemize}
the raising of an exception during the elaboration of the
@nt{declarative_part} of the corresponding @nt{task_body};

the completion of the
@nt{handled_sequence_of_statements} of the corresponding
@nt{task_body};

the selection of
an open @nt<terminate_alternative> of a @nt<select_statement>
in the corresponding @nt<task_body>;

the abort of the task.
@end{itemize}

@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of task dependence:}
@begin{Example}
@key(declare)
   @key(type) Global @key(is) @key(access) Server;        --@RI[  see @RefSecNum(Task Units and Task Objects)]
   A, B : Server;
   G    : Global;@Softpage
@key(begin)
   --@RI[  activation of A and B]
   @key(declare)
      @key(type) Local @key(is) @key(access) Server;
      X : Global := @key(new) Server;  --@RI[  activation of X.@key{all}]
      L : Local  := @key(new) Server;  --@RI[  activation of L.@key{all}]
      C : Server;@Softpage
   @key(begin)
      --@RI[  activation of C]
      G := X;  --@RI[  both G and X designate the same task object]
      ...
   @key(end;)  --@RI[  await termination of C and L.@key{all} (but not X.@key{all})]
   ...
@key(end;)  --@RI[  await termination of A, B, and G.@key{all}]
@end{Example}

@end{Examples}

@begin{DiffWord83}
We have revised the wording to be consistent with the definition
of master now given in @RefSec(Completion and Finalization).

Tasks that used to depend on library packages in Ada 83, now depend on the
(implicit) @nt<task_body> of the
environment task (see @RefSecNum(Program Execution)).
Therefore, the environment task has to wait for
them before performing library level finalization and terminating
the partition.
In Ada 83 the requirement to wait for tasks that depended
on library packages was not as clear.

What was "collective termination" is now "collective completion"
resulting from selecting @nt<terminate_alternative>s. This is because
finalization still occurs for such tasks, and this happens after
selecting the @nt<terminate_alternative>, but before termination.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[Added missing wording that explained the
  master of tasks that are neither @nt{object_declaration}s nor @nt{allocator}s,
  such as function returns.]}
@end{DiffWord95}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0070-1]}
  @ChgAdded{Version=[4],Text=[@B<Corrigendum:> Ensured that the master of
  tasks that are not @nt{allocator}s of named access types is correctly
  determined. (Ignoring the accessibility rules of
  @RefSecNum{Operations of Access Types} could not be intended.)]}
@end{DiffWord2012}


@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
@LabeledClause{Protected Units and Protected Objects}

@begin{Intro}
@Defn{protected object}
@Defn{protected operation}
@Defn{protected subprogram}
@Defn{protected entry}
A @i(protected object) provides coordinated access to shared data,
through calls on its visible @i(protected operations),
which can be @i{protected subprograms} or @i{protected entries}.
@Defn{protected declaration}
@Defn{protected unit}
@Defn{protected declaration}
A @i{protected unit} is declared by a @i(protected declaration), which has
a corresponding @nt<protected_body>.
A protected declaration may be a @nt<protected_type_declaration>,
in which case it declares
a named protected type; alternatively,
it may be a @nt<single_protected_declaration>,
in which case it defines an anonymous protected type, as well as declaring
a named protected object of that type.
@IndexSee{Term=[broadcast signal],See=(protected object)}
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<protected_type_declaration>,rhs="
  @key{protected} @key{type} @Syn2{defining_identifier} [@Syn2{known_discriminant_part}]@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]} @key{is}@Chg{Version=[2],New=<
     [@key{new} @Syn2{interface_list} @key{with}]
    >,Old=<>} @Syn2{protected_definition};"}


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00399-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<single_protected_declaration>,rhs="
  @key{protected} @Syn2{defining_identifier}@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]} @key{is}@Chg{Version=[2],New=<
     [@key{new} @Syn2{interface_list} @key{with}]
    >,Old=<>} @Syn2{protected_definition};"}


@Syn{lhs=<protected_definition>,rhs="
    { @Syn2{protected_operation_declaration} }
[ @key{private}
    { @Syn2{protected_element_declaration} } ]
  @key{end} [@SynI{protected_}@Syn2{identifier}]"}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@Syn{lhs=<protected_operation_declaration>,
  rhs="@Syn2{subprogram_declaration}
     | @Syn2{entry_declaration}
     | @Chg{New=[@Syn2{aspect_clause}],Old=[@Syn2{representation_clause}]}"}
@Syn{lhs=<protected_element_declaration>,
  rhs="@Syn2<protected_operation_declaration>
     | @Syn2<component_declaration>"}
@begin{Reason}
     We allow the operations and components to be mixed because that's how
     other things work (for example, package
     declarations). We have relaxed the
     ordering rules for the items inside @nt{declarative_part}s and
     @nt{task_definition}s as well.
@end{Reason}


@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0267-1]}
@Syn{lhs=<protected_body>,rhs="
  @key{protected} @key{body} @Syn2{defining_identifier}@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]} @key{is}
   { @Syn2{protected_operation_item} }
  @key{end} [@SynI{protected_}@Syn2{identifier}];"}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0147-1]}
@Syn{lhs=<protected_operation_item>,
  rhs="@Syn2{subprogram_declaration}
     | @Syn2{subprogram_body}
     | @Chg{Version=[4],New=[@Syn2{null_procedure_declaration}
     | @Syn2{expression_function_declaration}
     | ],Old=[]}@Syn2{entry_body}
     | @Chg{New=[@Syn2{aspect_clause}],Old=[@Syn2{representation_clause}]}"}

@begin{SyntaxText}
If a @SynI{protected_}@nt{identifier} appears at
the end of a @nt{protected_definition} or @nt{protected_body},
it shall repeat the @nt{defining_identifier}.
@end{SyntaxText}
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00345-01]}@ChgNote{This was just moved below}
@ChgDeleted{Version=[2],Text=[@PDefn2{Term=[requires a completion], Sec=(@nt{protected_declaration})}
A protected declaration requires a completion@redundant[,
which shall be a @nt{protected_@!body},]
and every @nt{protected_@!body} shall be the completion of some
protected declaration.]}
@begin(Honest)
  @ChgRef{Version=[2],Kind=[Deleted]}
  @ChgDeleted{Version=[2],Text=[The completion can be a @nt{pragma} Import,
  if the implementation supports it.]}
@end(Honest)
@end{Legality}
@begin{NotIso}
@ChgAdded{Version=[3],Noparanum=[T],Text=[@Shrink{@i<Paragraph 10 was
deleted.>}]}@Comment{This message should be deleted if the paragraphs
are ever renumbered.}
@end{NotIso}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01],ARef=[AI95-00401-01]}@Comment{This
is no change here, but both of these AIs reference this paragraph, adding and removing text.}
A @nt<protected_definition> defines a protected type and its first subtype.
@PDefn2{Term=[visible part], Sec=(of a protected unit)}
The list of @nt{protected_@!operation_@!declaration}s of a
@nt{protected_@!definition},
together with the @nt{known_@!discriminant_@!part}, if any,
is called the visible part of the protected unit.
@Redundant[@PDefn2{Term=[private part], Sec=(of a protected unit)}
The optional list of @nt{protected_@!element_@!declaration}s after the reserved
word @key{private} is called the private part of the protected
unit.]
@begin{TheProof}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
Private part is defined in @Chg{Version=[3],New=[Clause],Old=[Section]}
@RefSecNum{Visibility Rules}.
@end{theproof}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01],ARef=[AI95-00397-01],ARef=[AI95-00399-01],ARef=[AI95-00419-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0042-1]}
@ChgAdded{Version=[2],Text=[For a protected declaration
with an @nt{interface_list}, the protected type inherits user-defined primitive
subprograms from each progenitor type (see @RefSecNum{Interface Types}), in the
same way that a derived type inherits user-defined primitive subprograms from
its progenitor types (see @RefSecNum{Derived Types and Classes}). If the first
parameter of a primitive inherited subprogram is of the protected type or an
access parameter designating the protected type, and there is a
@nt{protected_operation_declaration} for a protected subprogram or single entry
with the same identifier within the protected declaration, whose
profile is type conformant with the prefixed view profile of the
inherited subprogram, the inherited subprogram is said to be
@i{implemented} by the conforming protected subprogram or
entry@Chg{Version=[3],New=[ using an implicitly declared nonabstract
subprogram which has the same profile as the inherited subprogram and which
overrides it@PDefn2{Term=[override],Sec=[when implemented by]}],
Old=[]}.@PDefn2{Term=[implemented],
Sec=[by a protected subprogram]}@PDefn2{Term=[implemented],
Sec=[by a protected entry]}
@Defn2{Term=[type conformance],Sec=(required)}]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The inherited subprograms can only come from an
  interface given as part of the protected declaration.]}
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[Added],Aref=[AI05-0042-1]}
  @ChgAdded{Version=[3],Text=[The part about the implicitly declared
  subprogram is needed so that a subprogram implemented by an entry or
  subprogram is considered to be overridden for the purpose of the
  other rules of the language. Without it, it would for instance be illegal
  for an abstract subprogram to be implemented by an entry, because the
  abstract subprogram would not be overridden. The @LegalityTitle below
  ensure that there is no conflict between the implicit overriding subprogram
  and a user-defined overriding subprogram.]}
@end{Reason}

@end{StaticSem}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}@ChgNote{This was just moved, not changed}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=[requires a completion], Sec=(@nt{protected_declaration})}
A protected declaration requires a completion@redundant[,
which shall be a @nt{protected_@!body},]
and every @nt{protected_@!body} shall be the completion of some
protected declaration.]}
@begin(Honest)
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[If],Old=[The completion
  can be a @nt{pragma} Import, if]} the implementation supports
  it@Chg{Version=[3],New=[, the protected body can be imported
  (using aspect Import, see @RefSecNum{Interfacing Aspects}),
  in which case no explicit @nt{protected_body} is allowed],Old=[]}.]}
@end(Honest)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01],ARef=[AI95-00399-01]}
@ChgAdded{Version=[2],Text=[@Redundant[Each @i{interface_}@nt{subtype_mark} of an
@nt{interface_list} appearing within a protected declaration shall denote a
limited interface type that is not a task interface.]]}
@begin(TheProof)
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[@RefSecNum{Interface Types} requires that an
  @nt{interface_list} only name interface types, and limits the descendants of
  the various kinds of interface types. Only a limited, protected, or
  synchronized interface can have a protected type descendant. Nonlimited or
  task interfaces are not allowed, as they offer operations that a protected
  type does not have.]}
@end(TheProof)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00397-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0042-1]}
@ChgAdded{Version=[2],Text=[The prefixed view profile of an explicitly declared
primitive subprogram of a tagged protected type shall not be type conformant
with any protected operation of the protected type, if the@Chg{Version=[3],New=[
subprogram has the same defining name as the protected operation and
the],Old=[]} first parameter of
the subprogram is of the protected type or is an access parameter designating
the protected type.@Defn2{Term=[type conformance],Sec=(required)}]}
@begin(Reason)
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This prevents the existence of two operations
  with the same name and profile which could be called with a prefixed view.
  If the operation was inherited, this would be illegal by the following rules;
  this rule puts inherited and noninherited routines on the same footing.
  Note that this only applies to tagged protected types (that is, those with an
  interface in their declaration); we do that as there is no problem with
  prefixed view calls of primitive operations for @lquotes@;normal@rquotes
  protected types, and having this rule apply to all protected types would be
  incompatible with Ada 95.]}
@end(Reason)



@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01],ARef=[AI95-00399-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For each primitive subprogram
inherited by the type declared by a protected declaration, at most one of the
following shall apply:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[the inherited subprogram is overridden with a
primitive subprogram of the protected type, in which case the overriding
subprogram shall be subtype conformant with the inherited
subprogram and not abstract; or@Defn2{Term=[subtype conformance],Sec=(required)}]}


@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01],ARef=[AI95-00397-01]}
@ChgAdded{Version=[2],Text=[the inherited subprogram is implemented by a
protected subprogram or single entry of the protected type,
in which case its prefixed view profile shall be subtype conformant with that
of the protected subprogram or entry.
@Defn2{Term=[subtype conformance],Sec=(required)}]}

@end{Itemize}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[If neither applies, the inherited subprogram shall
be a null procedure. @PDefn{generic contract issue}In addition to the places
where @LegalityTitle normally apply (see @RefSecNum{Generic Instantiation}),
these rules also apply in the private part of an instance of a generic unit.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Each inherited subprogram can only have a single
  implementation (either from overriding a subprogram, implementing a
  subprogram, or implementing an entry), and must have an implementation unless
  the subprogram is a null procedure.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0291-1]}
@ChgAdded{Version=[2],Text=[If an inherited subprogram is implemented by a
protected procedure or an entry, then the first parameter of the inherited
subprogram shall be of mode @key{out} or @key{in out}, or an
access-to-variable parameter.@Chg{Version=[3],New=[
If an inherited subprogram is implemented by a protected function, then the
first parameter of the inherited subprogram shall be of mode @key{in}, but not
an access-to-variable parameter.],Old=[]}]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgAdded{Version=[2],Text=[For a protected procedure or entry, the protected
object can be read or written (see
@RefSecNum{Protected Subprograms and Protected Actions}). A subprogram
that is implemented by a protected procedure or entry must have a profile
which reflects that in order to avoid confusion.@Chg{Version=[3],New=[
Similarly, a protected function has a parameter that is a constant,
and the inherited routine should reflect that.],Old=[]}]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00397-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[If a protected subprogram
declaration has an @nt{overriding_indicator}, then at the point of the
declaration:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[if the @nt{overriding_indicator} is
@key{overriding}, then the subprogram shall
implement an inherited subprogram;]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[if the @nt{overriding_indicator} is
@key{not overriding}, then the subprogram shall
not implement any inherited subprogram.]}

@end{Itemize}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@PDefn{generic contract issue}In addition to the
places where @LegalityTitle normally apply (see
@RefSecNum{Generic Instantiation}), these rules also apply in the private part
of an instance of a generic unit.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[These rules are subtly different than those for
  subprograms (see @RefSecNum{Overriding Indicators}) because there cannot be
  @lquotes@;late@rquotes inheritance of primitives from interfaces. Hidden
  (that is, private) interfaces are prohibited explicitly (see
  @RefSecNum{Private Types and Private Extensions}), as are hidden primitive
  operations (as private operations of public abstract types are prohibited
  @em see @RefSecNum{Abstract Types and Subprograms}).]}
@end{Discussion}

@end{Legality}

@begin{RunTime}
@redundant[@PDefn2{Term=[elaboration], Sec=(protected declaration)}The
elaboration of a protected declaration
elaborates the @nt<protected_definition>.
@PDefn2{Term=[elaboration], Sec=(single_protected_declaration)}
The elaboration of a @nt<single_@!protected_@!declaration> also creates
an object of an (anonymous) protected type.]
@begin(TheProof)
  This is redundant with the general rules for the elaboration
  of a @nt<full_type_declaration> and an @nt<object_declaration>.
@end(TheProof)

@PDefn2{Term=[elaboration], Sec=(protected_definition)}
@Redundant[The elaboration of a @nt<protected_definition>
creates the protected type and its first
subtype;] it also includes the elaboration of the
@nt<component_declaration>s and @nt<protected_operation_declaration>s
in the given order.

@redundant[@PDefn2{Term=[initialization], Sec=(of a protected object)}As
part of the initialization of a protected object,
any per-object constraints (see @RefSecNum{Record Types}) are elaborated.]
@begin{Discussion}
  We do not mention pragmas since each pragma has its
  own elaboration rules.
@end{Discussion}

@PDefn2{Term=[elaboration], Sec=(protected_body)}
The elaboration of a @nt{protected_body} has no other effect than to establish
that protected operations of the type can from then on be called without
failing the Elaboration_Check.

@leading@keepnext@;The content of an object of a given protected type includes:
@begin(itemize)
  The values of the components of the
  protected object, including (implicitly)
  an entry queue for each entry declared for the protected object;
  @begin(Ramification)
     "For each entry" implies one queue for each single entry,
      plus one for each entry of each entry family.
  @end(Ramification)

  @PDefn2{Term=[execution resource], Sec=(associated with a protected object)}
  A representation of the state of the execution resource
  @i(associated) with the protected object
  (one such resource is associated with each protected object).
@end(itemize)

@Redundant[The execution resource associated with a protected object
has to be acquired to
read or update any components of the protected object;
it can be acquired (as part of a protected action @em
see @RefSecNum(Protected Subprograms and Protected Actions))
either for concurrent read-only access, or for exclusive
read-write access.]

@PDefn2{Term=[finalization], Sec=(of a protected object)}
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
As the first step of the @i{finalization} of a protected object,
each call remaining on any entry queue of the object
is removed from its queue and
Program_Error is raised at the place of the corresponding
@nt<entry_@!call_@!statement>.
@begin(Reason)
  @leading@;This is analogous to the raising of Tasking_Error in callers
  of a task that completes before accepting the calls.
  This situation can only occur due to a
  requeue (ignoring premature unchecked_deallocation), since any task that
  has accessibility to a protected object is awaited before finalizing
  the protected object.
  For example:
@begin{Example}
@key[procedure] Main @key[is]
    @key[task] T @key[is]
        @key[entry] E;
    @key[end] T;

    @key[task] @key[body] T @key[is]
        @key[protected] PO @key[is]
            @key[entry] Ee;
        @key[end] PO;

        @key[protected] @key[body] PO @key[is]
            @key[entry] Ee @key[when] False @key[is]
            @key[begin]
                @key[null];
            @key[end] Ee;
        @key[end] PO;
    @key[begin]
        @key[accept] E @key[do]
            @key[requeue] PO.Ee;
        @key[end] E;
    @key[end] T;
@key[begin]
    T.E;
@key[end] Main;
@end{Example}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
The environment task is queued on PO.@Chg{Version=[3],New=[Ee],Old=[EE]}
when PO is finalized.

In a real example, a server task might park callers on a local protected
object for some useful purpose, so we didn't want to disallow this case.
@end(Reason)
@end{RunTime}

@begin{Bounded}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00280-01]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to call an entry or subprogram of a
protected object after that object is finalized. If the error is detected,
Program_Error is raised. Otherwise, the call proceeds normally, which may leave
a task queued forever.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This is very similar to the finalization rule. It
is a bounded error so that an implementation can avoid the overhead of the
check if it can ensure that the call still will operate properly. Such an
implementation cannot need to return resources (such as locks) to an
executive that it needs to execute calls.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[This case can happen (and has
happened in
production code) when a protected object is accessed from the Finalize routine
of a type. For example:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{with} Ada.Finalization.Controlled;
@key{package} Window_Manager @key{is}
    ...
    @key{type} Root_Window @key{is new} Ada.Finalization.Controlled @key{with private};
    @key{type} Any_Window @key{is access all} Root_Window;
    ...
@key{private}
    ...
    @key{procedure} Finalize (Object : @key{in out} Root_Window);
    ...
@key{end} Window_Manager;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package body} Window_Manager @key{is}
   @key{protected type} Lock @key{is}
       @key{entry} Get_Lock;
       @key{procedure} Free_Lock;
   ...
   @key{end} Lock;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   Window_Lock : Lock;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} Finalize (Object : @key{in out} Root_Window) @key{is}
   @key{begin}
       Window_Lock.Get_Lock;
       ...
       Window_Lock.Free_Lock;
   @key{end} Finalize;
   ...
   A_Window : Any_Window := @key{new} Root_Window;
@key{end} Window_Manager;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The environment task will call Window_Lock for
the object allocated for A_Window when the collection for Any_Window
is finalized, which
will happen after the finalization of Window_Lock (because finalization of the
package body will occur before that of the package specification).]}
@end{Reason}
@end{Bounded}

@begin{Notes}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00382-01]}
Within the declaration or body of a protected unit@Chg{Version=[2],New=[ other
than in an @nt{access_definition}],Old=[]}, the name of
the protected unit denotes the current instance of the unit
(see @RefSecNum(The Context of Overload Resolution)),
rather than the first subtype of the corresponding protected type (and
thus the name cannot be used as a @nt<subtype_mark>).
@begin(Discussion)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00382-01]}
  @Chg{Version=[2],New=[It can be used as a @nt{subtype_mark} in an anonymous
  access type. In addition],Old=[However]}, it is possible to refer to
  some other subtype of the protected type within its body,
  presuming such a subtype has been
  declared between the @nt<protected_type_declaration>
  and the @nt<protected_body>.
@end(Discussion)

A @nt<selected_component> can be used to denote a discriminant
of a protected object (see @RefSecNum(Selected Components)).
Within a protected unit, the name of a discriminant of the protected type
denotes the corresponding discriminant of the current instance
of the unit.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
A protected type is a limited type (see @RefSecNum(Limited Types)),
and hence @Chg{Version=[2],New=[precludes use of @nt{assignment_statement}s and],
Old=[has neither an assignment operation nor]} predefined equality operators.

The bodies of the protected operations given in the @nt<protected_body>
define the actions that take place upon calls to the protected operations.

The declarations in the private part are only
visible within the private part and the body of the
protected unit.
@begin{Reason}
@nt{Component_declaration}s are disallowed in a @nt{protected_body}
because, for efficiency, we wish to allow the compiler to
determine the size of protected objects (when not dynamic);
the compiler cannot necessarily see the body.
Furthermore, the semantics of initialization of such objects would be
problematic @em we do not wish to give protected objects complex
initialization semantics similar to task activation.

The same applies to @nt{entry_declaration}s,
since an entry involves an implicit component @em the entry queue.
@end{Reason}

@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of declaration of protected type and corresponding body:}
@begin{Example}
@key(protected) @key(type) Resource @key(is)
   @key(entry) Seize;
   @key(procedure) Release;
@key(private)
   Busy : Boolean := False;
@key(end) Resource;

@key(protected) @key(body) Resource @key(is)
   @key(entry) Seize @key(when not) Busy @key(is)
   @key(begin)
      Busy := True;
   @key(end) Seize;

   @key(procedure) Release @key(is)
   @key(begin)
      Busy := False;
   @key(end) Release;
@key(end) Resource;
@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Example of a single protected declaration and corresponding body:}
@end{WideAbove}
@begin{Example}
@key(protected) Shared_Array @key(is)
   --@RI[  Index, Item, and Item_Array are global types]
   @key(function)  Component    (N : @key(in) Index) return Item;
   @key(procedure) Set_Component(N : @key(in) Index; E : @key(in)  Item);
@key(private)
   Table : Item_Array(Index) := (others => Null_Item);
@key(end) Shared_Array;

@key(protected) @key(body) Shared_Array @key(is)
   @key(function) Component(N : @key(in) Index) @key(return) Item @key(is)
   @key(begin)
      @key(return) Table(N);
   @key(end) Component;

   @key(procedure) Set_Component(N : @key(in) Index; E : @key(in) Item) @key(is)
   @key(begin)
      Table(N) := E;
   @key(end) Set_Component;
@key(end) Shared_Array;
@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Examples of protected objects:}
@end{WideAbove}
@begin{Example}
Control  : Resource;
Flags    : @key(array)(1 .. 100) @key(of) Resource;
@end{Example}
@end{Examples}

@begin{Extend83}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Defn{extensions to Ada 83}
This entire @Chg{Version=[3],New=[subclause],Old=[clause]} is new;
protected units do not exist in Ada 83.
@end{Extend83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01],ARef=[AI95-00397-01],ARef=[AI95-00399-01],ARef=[AI95-00401-01],ARef=[AI95-00419-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Protected types and single protected objects can be derived from one or
  more interfaces. Operations declared in the protected type can implement
  the primitive operations of an interface. @nt{Overriding_indicator}s can
  be used to specify whether or not a protected operation implements a
  primitive operation.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0009],ARef=[AI95-00137-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Changed representation clauses
  to aspect clauses to reflect that they are used for more than just
  representation.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00280-01]}
  @ChgAdded{Version=[2],Text=[Described what happens when an operation of a
  finalized protected object is called.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
  @ChgAdded{Version=[2],Text=[Revised the note on operations of
  protected types to
  reflect that limited types do have an assignment operation, but not
  copying (@nt{assignment_statement}s).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00382-01]}
  @ChgAdded{Version=[2],Text=[Revised the note on use of the name of
  a protected type within itself to reflect the exception for anonymous
  access types.]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0291-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
  @b<Correction:> When an inherited subprogram is implemented by a protected
  function, the first parameter has to be an @key[in] parameter, but not
  an access-to-variable type. Original Ada 2005 allowed access-to-variable
  parameters in this case; the parameter will need to be changed to
  access-to-constant with the addition of the @key[constant] keyword.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1],ARef=[AI05-0267-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in a
  @nt{protected_type_declaration}, a @nt{single_protected_declaration},
  and a @nt{protected_body}. This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0042-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified that an
  inherited subprogram of a progenitor is overridden when it is
  implemented by an entry or subprogram.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0090-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added the missing
  defining name in the no conflicting primitive operation rule.]}
@end{DiffWord2005}

@begin{Extend2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0147-1]}
  @ChgAdded{Version=[4],Text=[@Defn{extensions to Ada 2012}
  @b<Corrigendum:> Null procedures and expression functions are allowed
  in protected bodies. We consider this an omission, as there is no
  reason why the convinient shorthand notations should not be allowed
  in this context.]}
@end{Extend2012}


@LabeledClause{Intertask Communication}

@begin{Intro}
@Defn{intertask communication}
@IndexSee{Term=[critical section],See=(intertask communication)}
The primary means for intertask
communication is provided by
calls on entries and protected subprograms.
Calls on protected subprograms allow coordinated access
to shared data objects.
Entry calls allow for blocking the caller until
a given condition is satisfied (namely, that the corresponding entry is open
@em see @RefSecNum(Entry Calls)),
and then communicating data or control information directly
with another task or
indirectly via a shared protected object.

@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0225-1],ARef=[AI05-0291-1]}
@Leading@;@Chg{Version=[3],New=[@Defn2{Term=[target object],
  Sec=(of the name of an entry or a protected subprogram)}],
Old=[@Defn2{Term=[target object],
  Sec=(of a call on an entry or a protected subprogram)}]}
@Chg{Version=[3],New=[When a @nt{name} or @nt{prefix} denotes],Old=[Any call on]}
an entry@Chg{Version=[3],New=[,],Old=[ or on a]}
protected subprogram@Chg{Version=[3],New=[, or a
prefixed view of a primitive subprogram of a limited interface whose
first parameter is a controlling parameter, the @nt{name} or @nt{prefix}
determines],Old=[ identifies]}
a @i(target object)@Chg{Version=[3],New=[],Old=[ for the operation,
which is either a task (for an entry call) or a protected object (for an entry
call or a protected subprogram call). The target object is considered an
implicit parameter to the operation, and is determined by the operation
@nt<name> (or @nt<prefix>) used in the call on the operation]},
as follows:
@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0291-1]}
  @ChgAdded{Version=[3],Text=[This wording uses "denotes" to mean "denotes a
  view of an entity" (when the term is used in Legality Rules), and "denotes an
  entity" (when the term is used in Dynamic Semantics rules). It does not mean
  "view of a declaration", as that would not include renames (a renames is not
  an entry or protected subprogram).]}
@end{Honest}

@begin(Itemize)
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0291-1]}
  If it is a @nt<direct_name> or expanded name
  that denotes the declaration (or body) of the operation, then
  the target object is implicitly specified to be
  the current instance of the task or protected unit
  immediately enclosing the operation;
  @Defn{internal call}
  @Chg{Version=[3],New=[],Old=[such ]}a call@Chg{Version=[3],New=[ using
  such a name],Old=[]} is defined to be an @i(internal call);

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0291-1]}
  If it is a @nt<selected_component> that is not
  an expanded name, then the target object is explicitly
  specified to be the @Chg{Version=[3],New=[],Old=[task or protected]} object
  denoted by the @nt<prefix> of the @nt<name>;
  @Defn{external call}
  @Chg{Version=[3],New=[],Old=[such ]}a call@Chg{Version=[3],New=[ using
  such a name],Old=[]} is defined to be an @i(external call);
  @begin{Discussion}
  For example:
@begin{Example}
@key[protected] @key[type] Pt @key[is]
  @key[procedure] Op1;
  @key[procedure] Op2;
@key[end] Pt;

PO : Pt;
Other_Object : Some_Other_Protected_Type;

@key[protected] @key[body] Pt @key[is]
  @key[procedure] Op1 @key[is] @key[begin] ... @key[end] Op1;

  @key[procedure] Op2 @key[is]
  @key[begin]
    Op1; --@RI{ An internal call.}
    Pt.Op1; --@RI{ Another internal call.}
    PO.Op1; --@RI{ An external call. It the current instance is PO, then}
            --@RI{ this is a bounded error (see @RefSecNum{Protected Subprograms and Protected Actions}).}
    Other_Object.Some_Op; --@RI{ An external call.}
  @key[end] Op2;
@key[end] Pt;
@end{Example}
  @end{Discussion}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0291-1]}
  If the @nt<name> or @nt<prefix> is a dereference
  (implicit or explicit) of an
  access-to-protected-subprogram value,
  then the target object is determined by the
  @nt<prefix> of the Access @nt<attribute_reference>
  that produced the access value originally@Chg{Version=[3],New=[; a],
  Old=[, and the]} call@Chg{Version=[3],New=[ using
  such a name],Old=[]} is defined to be an @i(external call);

  If the @nt<name> or @nt<prefix> denotes a
  @nt<subprogram_renaming_declaration>,
  then the target object is as determined by the @nt<name> of the renamed
  entity.

@end(Itemize)

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0291-1]}
@ChgAdded{Version=[3],Text=[A call on an entry or a protected subprogram either
uses a @nt{name} or @nt{prefix} that determines a target object implicitly, as
above, or is a call on (a non-prefixed view of) a primitive subprogram of a
limited interface whose first parameter is a controlling parameter, in which
case the target object is identified explicitly by the first parameter. This
latter case is an @i<external call>.]}

@Defn2{Term=[target object],
  Sec=(of a @nt<requeue_statement>)}
@Defn{internal requeue}
@Defn{external requeue}
A corresponding definition of target object applies
to a @nt<requeue_statement> (see @RefSecNum(Requeue Statements)),
with a corresponding distinction between an @i(internal requeue)
and an @i(external requeue).

@end{StaticSem}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0225-1],ARef=[AI05-0291-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[If a @nt{name} or
@nt{prefix} determines a target object, and the name denotes],Old=[The view of
the target protected object associated with a call of]}
a protected @Chg{Version=[3],New=[entry],Old=[procedure]} or
@Chg{Version=[3],New=[procedure, then the target object],Old=[entry]}
shall be a variable@Chg{Version=[3],New=[, unless the
@nt{prefix} is for an @nt{attribute_reference} to the Count
attribute (see @RefSecNum{Task and Entry Attributes})],Old=[]}.]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0225-1]}
  @ChgAdded{Version=[3],Text=[The point is to prevent any calls to such a
  @nt{name} whose target object is a constant view of a protected object,
  directly, or via an access value, renames, or generic formal
  subprogram. It is, however, legal to say P'Count in a protected function body,
  even though the protected object is a constant view there.]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0291-1]}
  @ChgAdded{Version=[3],Text=[This rule does not apply to calls that are not to
  a prefixed view. Specifically a "normal" call to a primitive operation of a
  limited interface is not covered by this rule. In that case, the normal
  parameter passing mode checks will prevent passing a constant protected
  object to an operation implemented by a protected entry or procedure
  as the mode is required to be @key[in out] or @key[out].]}
@end{Ramification}
@end{Legality}

@begin{RunTime}
Within the body of a protected operation, the current instance
(see @RefSecNum(The Context of Overload Resolution))
of the immediately enclosing protected unit is determined by the target object
specified (implicitly or explicitly) in the call (or requeue) on the
protected operation.
@begin{Honest}
The current instance is defined in the same way
within the body of a subprogram declared immediately within a
@nt{protected_body}.
@end{Honest}

Any call on a protected procedure or entry of a target
protected object is defined to be an update to the object,
as is a requeue on such an entry.
@begin(Reason)
  Read/write access to the components of a protected
  object is granted while inside the body
  of a protected procedure or entry.
  Also, any protected entry call can change the value of the Count
  attribute, which represents an update.
  Any protected procedure call can result in servicing the entries,
  which again might change the value of a Count attribute.
@end(Reason)
@end{RunTime}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0030-2],ARef=[AI05-0215-1]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<synchronization_kind>,Old=<>}>,
rhs="@Chg{Version=[3],New=<By_Entry | By_Protected_Procedure | Optional>,Old=<>}"}
@end{Syntax}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0215-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For the declaration of a primitive
procedure of a synchronized tagged type the following language-defined
representation aspect may be specified with an @nt{aspect_specification} (see
@RefSecNum{Aspect Specifications}):]}

@begin{Description}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Synchronization@\If specified, the aspect definition
shall be a @nt{synchronization_kind}.@AspectDefn{Synchronization}]}

@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Synchronization],
  Text=[@ChgAdded{Version=[3],Text=[Defines whether a given primitive operation
    of a synchronized interface must be implemented by an entry or protected
    procedure.]}]}

@end{Description}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0030-2],ARef=[AI05-0215-1]}
@ChgAdded{Version=[3],Text=[Inherited subprograms inherit the Synchronization
aspect, if any, from the corresponding subprogram of the parent or progenitor
type. If an overriding operation does not have a directly specified
Synchronization aspect then the Synchronization aspect of the inherited
operation is inherited by the overriding operation.]}
@end{StaticSem}

@begin{Legality}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0030-2],ARef=[AI05-0215-1]}
@ChgAdded{Version=[3],Text=[The @nt{synchronization_kind}
By_Protected_Procedure shall not be applied to a primitive procedure of a task
interface.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0030-2],ARef=[AI05-0215-1]}
@ChgAdded{Version=[3],Text=[A procedure for which the specified
@nt{synchronization_kind} is By_Entry shall be implemented by an entry. A
procedure for which the specified @nt{synchronization_kind} is
By_Protected_Procedure shall be implemented by a protected procedure. A
procedure for which the specified @nt{synchronization_kind} is Optional may be
implemented by an entry or by a procedure (including a protected procedure).]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0030-2],ARef=[AI05-0215-1]}
@ChgAdded{Version=[3],Text=[If a primitive procedure overrides an inherited
operation for which the Synchronization aspect has been specified to be By_Entry
or By_Protected_Procedure, then any specification of the aspect Synchronization
applied to the overriding operation shall have the same
@nt{synchronization_kind}.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0030-2]}
@ChgAdded{Version=[3],Text=[@PDefn{generic contract issue}In addition to the
places where @LegalityTitle normally apply (see
@RefSecNum{Generic Instantiation}), these rules also apply in the
private part of an instance of a generic unit.]}
@end{Legality}

@begin{Notes}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0030-2],ARef=[AI05-0215-1]}
@ChgAdded{Version=[3],Text=[The @nt{synchronization_kind} By_Protected_Procedure
implies that the operation will not block.]}
@end{Notes}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01]}
  @ChgAdded{Version=[2],Text=[Added a @LegalityName to make it crystal-clear
  that the protected object of an entry or procedure call must be a variable.
  This rule was implied by the @RuntimeTitle here, along with the
  @StaticSemTitle of @RefSecNum{Objects and Named Numbers}, but it is much
  better to explicitly say it. While many implementations have gotten this
  wrong, this is not an incompatibility @em allowing updates of protected
  constants has always been wrong.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0030-2],ARef=[AI05-0215-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Added the Synchronization aspect to allow
  specifying that an interface procedure is really an entry or a
  protected procedure.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0225-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified that the target object
  of any name denoted a protected procedure or entry can never be a constant
  (other than for the 'Count attribute). This closes holes involving calls to
  access-to-protected, renaming as a procedure, and generic formal subprograms.]}
@end{DiffWord2005}


@LabeledSubClause{Protected Subprograms and Protected Actions}

@begin{Intro}
@Defn{protected subprogram}
@Defn{protected procedure}
@Defn{protected function}
A @i{protected subprogram} is a subprogram declared immediately
within a @nt{protected_definition}.
Protected procedures provide exclusive read-write access
to the data of a protected object; protected functions provide
concurrent read-only access to the data.
@begin{Ramification}
A subprogram declared immediately within a @nt{protected_body} is not a
protected subprogram; it is an intrinsic subprogram.
See @RefSec{Conformance Rules}.
@end{Ramification}
@end{Intro}

@begin{StaticSem}
@Redundant[Within the body of a protected function
(or a function declared immediately within a @nt<protected_body>),
the current instance of the enclosing protected unit is defined to be a
constant
(that is, its subcomponents may be read but not updated).
Within the body of a protected procedure
(or a procedure declared immediately within a @nt<protected_body>),
and within an @nt<entry_body>,
the current instance is defined to be a variable
(updating is permitted).]
@begin(TheProof)
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0120-1]}
  @ChgAdded{Version=[3],Text=[All constant views are defined in
  @RefSec{Objects and Named Numbers}, anything not named there
  is a variable view.]}
@end(TheProof)
@begin(Ramification)
  The current instance is like an implicit parameter,
  of mode @key(in) for a protected function, and of mode @key(in out)
  for a protected procedure (or protected entry).
@end(Ramification)

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0129-1]}
@ChgAdded{Version=[4],Type=[Leading],Text=[For a type declared by a
@nt{protected_type_declaration} or for the
anonymous type of an object declared by a @nt{single_protected_declaration},
the following language-defined type-related representation aspect
may be specified:]}

@begin{Description}
  @ChgRef{Version=[4],Kind=[Added]}
  @ChgAdded{Version=[4],Text=[Exclusive_Functions@\The type of aspect
  Exclusive_Functions is Boolean. If not specified (including by inheritance),
  the aspect is False.@AspectDefn{Exclusive_Functions}]}

  @ChgRef{Version=[4],Kind=[Added]}
  @ChgAdded{Version=[4],Text=[@\A value of True for this aspect indicates that
  protected functions behave in the same way as protected procedures
  with respect to mutual exclusion and queue servicing (see below).]}

  @ChgAspectDesc{Version=[4],Kind=[Added],Aspect=[Exclusive_Functions],
    Text=[@ChgAdded{Version=[4],Text=[Specifies mutual exclusion behavior of
      protected functions in a protected type.]}]}
@end{Description}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0129-1]}
@ChgAdded{Version=[4],Text=[A protected procedure or entry is an @i<exclusive>
protected operation.@Defn2{Term=[exclusive],Sec=[protected operation]} A
protected function of a protected type @i<P> is an exclusive protected
operation if the Exclusive_Functions aspect of @i<P> is True.]}

@end{StaticSem}

@begin{RunTime}

@PDefn2{Term=[execution], Sec=(protected subprogram call)}
For the execution of a call on a protected subprogram,
the evaluation of the @nt<name> or @nt<prefix>
and of the parameter associations,
and any assigning back of @key[in out] or @key[out] parameters,
proceeds as for a normal subprogram call (see @RefSecNum{Subprogram Calls}).
If the call is an internal call (see @RefSecNum(Intertask Communication)),
the body of the subprogram
is executed as for a normal subprogram call.
If the call is an external call, then
the body of the subprogram is executed as part of a new
@i(protected action) on the target protected object;
the protected action completes after the body of the
subprogram is executed.
@Redundant[A protected action can also be started by an entry call
(see @RefSecNum{Entry Calls}).]

@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0129-1]}
@leading@Defn{protected action}
A new protected action is not started on a protected object
while another protected action on the same protected object is underway,
unless both actions are the result of a call on a
@Chg{Version=[4],New=[nonexclusive ],Old=[]}protected function.
This rule is expressible in terms of the execution resource
associated with the protected object:
@begin(Itemize)
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0129-1]}
@Defn2{Term=[protected action], Sec=(start)}
@Defn2{Term=[acquire], Sec=(execution resource associated with protected object)}
@i(Starting) a protected action on a protected object
corresponds to @i(acquiring) the execution resource associated
with the protected object, either
for @Chg{Version=[4],New=[exclusive read-write],Old=[concurrent read-only]}
access if the protected action is for a call on @Chg{Version=[4],New=[an
exclusive protected operation],Old=[a protected function]}, or
for @Chg{Version=[4],New=[concurrent read-only],Old=[exclusive read-write]}
access otherwise;

@Defn2{Term=[protected action], Sec=(complete)}
@Defn2{Term=[release], Sec=(execution resource associated with protected object)}
@i(Completing) the protected action
corresponds to @i(releasing) the associated execution resource.
@end(Itemize)

@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0129-1]}
@Redundant[After performing an @Chg{Version=[4],New=[exclusive
protected ],Old=[]}operation on a protected object@Chg{Version=[4],New=[],Old=[
other than a call on a protected function]}, but prior
to completing the associated protected action,
the entry queues (if any)
of the protected object are
serviced (see @RefSecNum(Entry Calls)).]
@end{RunTime}

@begin{Bounded}
@leading@PDefn2{Term=(bounded error),Sec=(cause)}
During a protected action, it is a bounded error to invoke an operation that
is @i(potentially blocking).
@Defn{potentially blocking operation}
@Defn{blocking, potentially}
The following are defined to be potentially blocking operations:
@begin{Reason}
  Some of these operations are not directly blocking.
  However, they are still treated as bounded errors during a protected
  action, because allowing them might impose an undesirable
  implementation burden.
@end{Reason}
@begin{itemize}
a @nt{select_statement};

an @nt{accept_statement};

an @nt{entry_call_statement};

a @nt{delay_statement};

an @nt{abort_statement};

task creation or activation;

an external call on a protected subprogram (or an external requeue)
with the same target object as that of the protected action;
@begin(Reason)
  This is really a deadlocking call, rather than a blocking call,
  but we include it in this list for simplicity.
@end(Reason)

a call on a subprogram whose body contains a
potentially blocking operation.
@begin(Reason)
  This allows an implementation to check and raise Program_Error
  as soon as a subprogram is called, rather than waiting to find out
  whether it actually reaches the potentially blocking operation.
  This in turn allows the potentially blocking operation check
  to be performed prior to run time in some environments.
@end(Reason)
@end{itemize}

@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
If the bounded error is detected, Program_Error is raised.
If not detected, the bounded error
might result in deadlock or a (nested)
protected action on the same target object.

@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00305-01]}
@ChgAdded{Version=[2],Text=[By @lquotes@;nested protected action@rquotes,
we mean that an additional protected action can be started by another task
on the same protected object. This means that mutual exclusion may be broken
in this bounded error case. A way to ensure that this does not happen is to use
pragma Detect_Blocking (see @RefSecNum{Pragma Detect_Blocking}).]}
@end{Discussion}

Certain language-defined subprograms are
potentially blocking.
In particular, the subprograms of
the language-defined input-output packages that manipulate
files (implicitly or explicitly) are potentially blocking.
Other potentially blocking subprograms are identified
where they are defined.
When not specified as potentially blocking,
a language-defined subprogram is nonblocking.
@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00178-01]}
@ChgAdded{Version=[2],Text=[Any subprogram in a language-defined input-output
package that has a file parameter or result or operates on a default file is
considered to manipulate a file. An instance of a language-defined input-output
generic package provides subprograms that are covered by this rule. The only
subprograms in language-defined input-output packages not covered by this rule
(and thus not potentially blocking) are the Get and Put routines that take
string parameters defined in the packages nested in Text_IO.]}@ChgNote{This
was the resolution of a ramification.}
@end{Discussion}
@end{Bounded}

@begin{Notes}
If two tasks both try to start a protected action
on a protected object, and at most one is calling
a protected function, then only one of the tasks can proceed.
Although the other task cannot proceed, it is not considered
blocked, and it might be consuming processing resources while it
awaits its turn. There is no language-defined ordering or queuing
presumed for tasks competing to start a protected action @em
on a multiprocessor such tasks might use busy-waiting; for
monoprocessor considerations, see @RefSec{Priority Ceiling Locking}.
@begin{Discussion}
The intended implementation on a multi-processor is in terms of
@lquotes@;spin locks@rquotes@; @em the waiting task will spin.
@end{Discussion}

The body of a protected unit may contain declarations and bodies for local
subprograms. These are not visible outside the protected unit.

The body of a protected function can contain internal calls
on other protected functions, but not protected procedures,
because the current instance is a constant.
On the other hand, the body of a protected procedure
can contain internal calls on both protected functions and procedures.

From within a protected action,
an internal call on a protected subprogram,
or an external call on a protected subprogram with a different
target object is not considered a potentially blocking operation.
@begin(Reason)
  This is because a task is not considered blocked
  while attempting to acquire the execution resource associated with
  a protected object. The acquisition of such a resource
  is rather considered part of the normal competition for execution
  resources between the various tasks that are ready.
  External calls that turn out to be on the same target
  object are considered potentially blocking, since they
  can deadlock the task indefinitely.
@end(Reason)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00305-01]}
@ChgAdded{Version=[2],Text=[The @nt{pragma} Detect_Blocking may be used to
ensure that all executions of potentially blocking operations during a
protected action raise Program_Error.
See @RefSecNum{Pragma Detect_Blocking}.]}
@end{Notes}

@begin{Examples}
@leading@i{Examples of protected subprogram calls
(see @RefSecNum(Protected Units and Protected Objects)):}
@begin{Example}
Shared_Array.Set_Component(N, E);
E := Shared_Array.Component(M);
Control.Release;
@end{Example}

@end{Examples}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00305-01]}
  @ChgAdded{Version=[2],Text=[Added a note pointing out the existence of
  @nt{pragma} Detect_Blocking. This pragma can be used to ensure portable
  (somewhat pessimistic) behavior of protected actions by converting the
  Bounded Error into a required check.]}
@end{DiffWord95}

@begin{Extend2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0129-1]}
  @ChgAdded{Version=[4],Text=[@Defn{extensions to Ada 2012}
  @b<Corrigendum:> Aspect Exclusive_Functions is new. The term
  @ldquote@;exclusive protected operations@rdquote is new.]}
@end{Extend2012}



@LabeledSubClause{Entries and Accept Statements}

@begin{Intro}
@nt<Entry_declaration>s, with the corresponding @ntf<entry_bodies>
or @nt<accept_statement>s,
are used to define potentially queued operations on
tasks and protected objects.
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00397-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<entry_declaration>,rhs="@Chg{Version=[2],New=<
   [@Syn2{overriding_indicator}]>,Old=[]}
   @key{entry} @Syn2{defining_identifier} [(@Syn2{discrete_subtype_definition})] @Syn2{parameter_profile}@Chg{Version=[3],New=<
      [@Syn2{aspect_specification}]>,Old=[]};"}


@Syn{lhs=<accept_statement>,rhs="
   @key{accept} @SynI{entry_}@Syn2{direct_name} [(@Syn2{entry_index})] @Syn2{parameter_profile} [@key{do}
     @Syn2{handled_sequence_of_statements}
   @key{end} [@SynI{entry_}@Syn2{identifier}]];"}
@begin{Reason}
  We cannot use @nt{defining_identifier} for @nt<accept_statement>s.
  Although an @nt{accept_statement} is sort of like a body, it can appear
  nested within a @nt{block_statement}, and therefore be hidden from
  its own entry by an outer homograph.
@end{Reason}

@Syn{lhs=<entry_index>,rhs="@Syn2{expression}"}


@Syn{lhs=<entry_body>,rhs="
  @key{entry} @Syn2{defining_identifier}  @Syn2{entry_body_formal_part}  @Syn2{entry_barrier} @key{is}
    @Syn2{declarative_part}
  @key{begin}
    @Syn2{handled_sequence_of_statements}
  @key{end} [@SynI{entry_}@Syn2{identifier}];"}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00397-01]}
  @ChgAdded{Version=[2],Text=[We don't allow an @nt{overriding_indicator} on
  an @nt{entry_body} because entries always implement procedures at the
  point of the type declaration; there is no late implementation. And we
  don't want to have to think about @nt{overriding_indicator}s on
  @nt{accept_statement}s.]}
@end{Discussion}

@Syn{lhs=<entry_body_formal_part>,
  rhs="[(@Syn2{entry_index_specification})] @Syn2{parameter_profile}"}


@Syn{lhs=<entry_barrier>,
  rhs="@key{when} @Syn2{condition}"}


@Syn{lhs=<entry_index_specification>,
  rhs="@key{for} @Syn2{defining_identifier} @key{in} @Syn2{discrete_subtype_definition}"}

@begin{SyntaxText}
If an @SynI{entry_}@nt{identifier} appears at the end of an
@nt{accept_statement}, it shall repeat the @SynI{entry_}@!@nt<direct_@!name>.
If an @SynI{entry_}@!@nt{identifier} appears at the end of an @nt{entry_@!body}, it shall repeat the
@nt{defining_@!identifier}.

@Redundant[An @nt{entry_declaration} is allowed only in a protected or task
declaration.]
@begin(TheProof)
  This follows from the BNF.
@end(TheProof)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00397-01]}
@ChgAdded{Version=[2],Text=[An @nt{overriding_indicator} is not allowed in an
@nt{entry_declaration} that includes a @nt{discrete_subtype_definition}.]}
@begin(Reason)
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[An entry family can never implement something,
  so allowing an indicator is felt by the majority of the ARG to be redundant.]}
@end(Reason)
@end{SyntaxText}
@end{Syntax}

@begin{Resolution}

@PDefn2{Term=[expected profile],
  Sec=(accept_statement @i{entry_}@nt<direct_name>)}
In an @nt<accept_statement>,
the expected profile for the @SynI{entry_}@nt<direct_name>
is that of the @nt<entry_@!declaration>;
@PDefn2{Term=[expected type],
  Sec=(entry_index)}
the expected type for an @nt<entry_index> is that
of the subtype defined by the @nt<discrete_@!subtype_@!definition>
of the corresponding @nt<entry_@!declaration>.

Within the @nt<handled_sequence_of_statements> of an @nt<accept_statement>,
if a @nt<selected_@!component> has a @nt<prefix> that denotes
the corresponding @nt<entry_@!declaration>, then the
entity denoted by the @nt<prefix> is the @nt<accept_@!statement>, and
the @nt<selected_@!component> is interpreted as an expanded name
(see @RefSecNum(Selected Components))@Redundant[; the @nt<selector_name>
of the @nt<selected_@!component> has to be the @nt<identifier> for
some formal parameter of the @nt<accept_@!statement>].
@begin{TheProof}
  The only declarations that occur immediately within the
  declarative region of an @nt<accept_statement> are those
  for its formal parameters.
@end{TheProof}
@end{Resolution}

@begin{Legality}

An @nt<entry_declaration> in a task declaration shall not contain
a specification for an access parameter (see @RefSecNum(Access Types)).
@begin(Reason)
  @leading@;Access parameters for task entries would require a complex
  implementation. For example:
  @begin(Example)
@key(task) T @key(is)
   @key(entry) E(Z : @key(access) Integer); --@RI{ Illegal!}
@key(end) T;

@key(task body) T @key(is)
@key(begin)
   @key(declare)
      @key(type) A @key(is access all) Integer;
      X : A;
      Int : @key(aliased) Integer;
      @key(task) Inner;
      @key(task body) Inner @key(is)
      @key(begin)
         T.E(Int'Access);
      @key(end) Inner;
   @key(begin)
      @key(accept) E(Z : @key(access) Integer) @key(do)
         X := A(Z); --@RI{ Accessibility_Check}
      @key(end) E;
   @key(end);
@key(end) T;
@end(Example)

Implementing the Accessibility_Check inside the @nt<accept_statement> for
E is difficult, since one does not know whether the entry caller
is calling from inside the immediately enclosing declare block or from
outside it. This means that the lexical nesting level associated with
the designated object is not sufficient to determine whether the
Accessibility_Check should pass or fail.

Note that such problems do not arise with protected entries, because
@ntf<entry_bodies> are always nested immediately within the
@nt<protected_body>; they cannot be further nested as can
@nt<accept_statement>s, nor can they be called from within the
@nt<protected_body> (since no entry calls are permitted inside
a @nt<protected_body>).
@end(Reason)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00397-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[If an @nt{entry_declaration} has an
@nt{overriding_indicator}, then at
the point of the declaration:]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[if the @nt{overriding_indicator} is
@key{overriding}, then the entry shall implement an inherited subprogram;]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[if the @nt{overriding_indicator} is
@key{not overriding}, then the entry shall not implement any inherited
subprogram.]}

@end{Itemize}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@PDefn{generic contract issue}In addition to the
places where @LegalityTitle normally apply (see
@RefSecNum{Generic Instantiation}), these rules also apply in the private part
of an instance of a generic unit.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[These rules are subtly different than those for
  subprograms (see @RefSecNum{Overriding Indicators}) because there cannot be
  @lquotes@;late@rquotes inheritance of primitives from interfaces. Hidden
  (that is, private) interfaces are prohibited explicitly (see
  @RefSecNum{Private Types and Private Extensions}), as are hidden primitive
  operations (as private operations of public abstract types are prohibited
  @em see @RefSecNum{Abstract Types and Subprograms}).]}
@end{Discussion}

For an @nt<accept_statement>,
the innermost enclosing body shall be a @nt<task_body>,
and the @i(entry_)@!@nt<direct_@!name> shall
denote an @nt<entry_@!declaration> in the corresponding task declaration;
the profile of the @nt{accept_@!statement} shall
conform fully to that of the corresponding @nt<entry_@!declaration>.
@Defn2{Term=[full conformance],Sec=(required)}
An @nt<accept_@!statement> shall have a parenthesized @nt<entry_@!index> if
and only if the corresponding @nt<entry_@!declaration> has a
@nt<discrete_@!subtype_@!definition>.

An @nt<accept_statement> shall not be within another
@nt{accept_statement} that corresponds to the same @nt<entry_@!declaration>,
nor within an @nt<asynchronous_@!select> inner to
the enclosing @nt<task_body>.
@begin(Reason)
@nt<Accept_statement>s are required to be immediately within
the enclosing @nt<task_body> (as opposed to being in a nested
subprogram) to ensure that a nested task does not
attempt to accept the entry of its enclosing task. We considered
relaxing this restriction, either by making the check a run-time
check, or by allowing a nested task to accept an entry of its
enclosing task. However, neither change seemed to provide sufficient
benefit to justify the additional implementation burden.

Nested @nt<accept_statement>s for the same entry (or entry family)
are prohibited to ensure that there is no ambiguity in the
resolution of an expanded name for a formal parameter of the
entry. This could be relaxed by allowing the inner
one to hide the outer one from all visibility, but again the
small added benefit didn't seem to justify making the change for Ada 95.

@nt<Accept_statement>s are not permitted within @nt<asynchronous_select>
statements to simplify the semantics and implementation:
an @nt<accept_statement> in an @nt<abortable_part> could result
in Tasking_Error being propagated from an entry call even though
the target task was still callable; implementations that use
multiple tasks implicitly to implement an @nt<asynchronous_select>
might have trouble supporting "up-level" accepts.
Furthermore, if @nt<accept_statement>s were permitted in
the @nt<abortable_part>, a task could call its own
entry and then accept it in the @nt<abortable_part>, leading
to rather unusual and possibly difficult-to-specify semantics.
@end(Reason)

@PDefn2{Term=[requires a completion], Sec=(protected @nt{entry_declaration})}
An @nt{entry_declaration} of a protected unit requires
a completion@redundant[, which shall be an @nt{entry_body},]
@PDefn2{Term=[only as a completion], Sec=(@nt<entry_body>)}
and every @nt<entry_@!body> shall be the completion
of an @nt<entry_@!declaration> of a protected unit.
@PDefn2{Term=[completion legality], Sec=(@nt<entry_body>)}
The profile of the @nt<entry_@!body> shall conform fully to that
of the corresponding declaration.
@Defn2{Term=[full conformance],Sec=(required)}
@begin{Ramification}
An @nt<entry_declaration>, unlike a @nt<subprogram_declaration>,
cannot be completed with a @nt<renaming_@!declaration>.
@end{Ramification}
@begin(Honest)
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
  @Chg{Version=[3],New=[If],Old=[The completion
  can be a @nt{pragma} Import, if]} the implementation supports
  it@Chg{Version=[3],New=[, the entry body can be imported
  (using aspect Import, see @RefSecNum{Interfacing Aspects}),
  in which case no explicit @nt{entry_body} is allowed],Old=[]}.
@end(Honest)
@begin{Discussion}
The above applies only to protected entries,
which are the only ones completed with @ntf{entry_bodies}.
Task entries have corresponding @nt{accept_statement}s
instead of having @ntf{entry_bodies}, and
we do not consider an @nt{accept_statement} to be a @lquotes@;completion,@rquotes@;
because a task @nt{entry_declaration} is allowed to have zero, one, or more
than one corresponding @nt{accept_statement}s.
@end{Discussion}

An @nt{entry_body_formal_part} shall have an @nt{entry_@!index_@!specification}
if and only if the corresponding @nt{entry_@!declaration} has
a @nt<discrete_@!subtype_@!definition>.
In this case, the @nt<discrete_@!subtype_@!definition>s of the
@nt<entry_@!declaration> and the @nt<entry_@!index_@!specification>
shall fully conform to one another (see @RefSecNum(Conformance Rules)).
@Defn2{Term=[full conformance],Sec=(required)}

A name that denotes a formal parameter of an @nt<entry_body> is not
allowed within the @nt<entry_barrier> of the @nt<entry_body>.

@end{Legality}

@begin{StaticSem}
The parameter modes defined for parameters in the @nt<parameter_profile>
of an @nt{entry_declaration}
are the same as for a @nt<subprogram_declaration> and have
the same meaning (see @RefSecNum(Formal Parameter Modes)).
@begin{Discussion}
Note that access parameters are not allowed for task entries (see above).
@end{Discussion}

@Defn2{Term=[family], Sec=(entry)}
@Defn{entry family}
@Defn{entry index subtype}
An @nt<entry_declaration> with a @nt<discrete_subtype_definition>
(see @RefSecNum(Array Types)) declares a @i(family) of distinct
entries having the same profile, with one such entry for each
value of the @i(entry index subtype) defined
by the @nt<discrete_@!subtype_@!definition>.
@Redundant[A name for an entry of a family takes the form of
an @nt<indexed_component>, where the @nt<prefix> denotes
the @nt<entry_declaration> for the family, and the index value
identifies the entry within the family.]
@Defn{single entry}
@Defn2{Term=[entry], Sec=(single)}
The term @i(single entry) is used to refer to any entry other
than an entry of an entry family.

In the @nt<entry_body> for an entry family,
the @nt<entry_index_specification> declares a named constant
whose subtype is the entry index subtype defined by the
corresponding @nt<entry_declaration>;
@Defn{named entry index}
the value of the @i(named entry index) identifies
which entry of the family was called.
@begin{Ramification}
The @nt<discrete_subtype_definition> of the @nt<entry_index_specification>
is not elaborated; the subtype of the named constant declared
is defined by the @nt<discrete_subtype_definition> of the corresponding
@nt<entry_declaration>, which is elaborated, either when the
type is declared, or when the object is created, if its constraint
is per-object.
@end{Ramification}

@end{StaticSem}

@begin{RunTime}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0002],ARef=[AI95-00171-01]}
@PDefn2{Term=[elaboration], Sec=(entry_declaration)}
@Chg{New=[The elaboration of an @nt<entry_declaration> for an entry family
consists of the elaboration of the @nt<discrete_@!subtype_@!definition>, as
described in @RefSecNum(Record Types).],
Old=[For the elaboration of an @nt<entry_@!declaration> for an
entry family, if the
@nt{discrete_@!subtype_@!definition} contains no per-object expressions
(see @RefSecNum(Record Types)), then the @nt<discrete_@!subtype_@!definition>
is elaborated. Otherwise, the elaboration of the
@nt<entry_@!declaration> consists of the evaluation of any
expression of the @nt<discrete_@!subtype_@!definition>
that is not a per-object expression (or part of one).]}
The elaboration of an @nt<entry_@!declaration> for a single entry
has no effect.
@begin{Discussion}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
The elaboration of the declaration of a protected subprogram has
no effect, as specified in @Chg{Version=[3],New=[subclause],Old=[clause]}
@RefSecNum(Subprogram Declarations).
The default initialization of an object of a task or protected
type is covered in @RefSecNum(Object Declarations).
@end{Discussion}

@Redundant[The actions to be performed when
an entry is called are specified by the
corresponding @nt{accept_@!statement}s (if any) for an entry of a task unit,
and by the corresponding @nt<entry_@!body> for an entry of a protected unit.]

@PDefn2{Term=[execution], Sec=(accept_statement)}
For the execution of an @nt{accept_statement}, the @nt<entry_index>, if
any, is first evaluated and converted to the entry index subtype;
this index value identifies which entry of the family is to be accepted.
@PDefn2{Term=[implicit subtype conversion],Sec=(entry index)}
@PDefn2{Term=[blocked], Sec=(on an @nt<accept_statement>)}
@Defn2{Term=[selection], Sec=(of an entry caller)}
Further execution of the @nt<accept_statement> is then blocked
until a caller of the corresponding entry is selected
(see @RefSecNum(Entry Calls)), whereupon
the @nt<handled_sequence_of_statements>, if any, of the @nt<accept_statement>
is executed, with the formal parameters associated with the
corresponding actual parameters of the selected entry call.
Upon completion of the @nt<handled_sequence_of_statements>,
the @nt<accept_statement> completes and is left.
When an exception is propagated from the
@nt{handled_sequence_of_statements} of an @nt{accept_statement},
the same exception is also raised by the execution of the corresponding
@nt{entry_call_statement}.
@begin{Ramification}
This is in addition to propagating it to the construct
containing the @nt{accept_statement}.
In other words, for a rendezvous, the raising splits in two,
and continues concurrently in both tasks.

The caller gets a new occurrence;
this isn't considered propagation.

Note that we say @lquotes@;propagated from the
@nt{handled_sequence_of_statements} of an @nt{accept_statement}@rquotes@;,
not @lquotes@;propagated from an @nt{accept_statement}.@rquotes@;
The latter would be wrong @em we don't want exceptions propagated by
the @nt<entry_index> to be sent to the caller (there is none yet!).
@end{Ramification}

@Defn{rendezvous}
The above interaction between a calling task and an
accepting task is called a @i(rendezvous).
@Redundant[After a rendezvous, the two tasks continue
their execution independently.]

@Redundant[An @nt<entry_body> is executed when the @nt<condition> of the
@nt<entry_barrier> evaluates to True and a caller of the corresponding
single entry, or entry of the corresponding entry family,
has been selected (see @RefSecNum(Entry Calls)).]
@PDefn2{Term=[execution], Sec=(entry_body)}
For the execution of the @nt<entry_@!body>,
the @nt<declarative_@!part> of the @nt<entry_@!body> is elaborated,
and the @nt<handled_@!sequence_of_@!statements>
of the body is executed, as for the execution
of a @nt<subprogram_body>. The value of the named entry index, if any,
is determined by the value of the entry index specified in the
@i(entry_)@nt<name> of the selected entry call (or intermediate
@nt<requeue_@!statement> @em see @RefSecNum(Requeue Statements)).
@begin(Honest)
If the entry had been renamed as a subprogram,
and the call was a @nt<procedure_call_statement> using
the name declared by the renaming, the entry index (if any) comes from
the entry @nt<name> specified in the
@nt<subprogram_renaming_declaration>.
@end(Honest)

@end{RunTime}

@begin{Notes}

A task entry has corresponding accept_statements (zero or more),
whereas a protected entry has a corresponding entry_body (exactly
one).

A consequence of the rule regarding the allowed placements of
@nt{accept_statement}s is that a task can execute @nt{accept_statement}s
only for its own entries.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
A @Chg{Version=[2],New=[return statement],Old=[@nt{return_statement}]}
(see @RefSecNum(Return Statements))
or a @nt<requeue_statement> (see @RefSecNum(Requeue Statements))
may be used to complete the execution of
an @nt<accept_statement> or an @nt<entry_body>.
@begin{Ramification}
An @nt<accept_statement> need not have a @nt<handled_sequence_of_statements>
even if the corresponding entry has parameters. Equally, it can have
a @nt<handled_sequence_of_statements> even if the corresponding entry
has no parameters.
@end{Ramification}
@begin{Ramification}
A single entry overloads a subprogram, an enumeration literal, or another
single entry if they have the same @nt{defining_identifier}. Overloading is
not allowed for entry family names.
A single entry or an entry of an entry family
can be renamed as a procedure as explained in
@RefSecNum{Subprogram Renaming Declarations}.
@end{Ramification}

The @nt<condition> in the @nt{entry_barrier} may reference
anything visible except the formal parameters of the entry.
This
includes the entry index (if any), the components (including discriminants) of
the protected object, the Count attribute of an entry of that protected object,
and data global to the protected unit.

@NoPrefix@;The restriction against referencing the formal parameters within an
@nt{entry_barrier} ensures that all calls of the same entry see
the same barrier value.
If it is necessary to look at the parameters of an entry
call before deciding whether to handle it, the @nt<entry_barrier>
can be @lquotes@;@key(when) True@rquotes@; and the caller can
be requeued (on some private entry)
when its parameters indicate that it cannot be handled immediately.
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Examples of entry declarations:}
@begin{Example}
@key(entry) Read(V : @key(out) Item);
@key(entry) Seize;
@key(entry) Request(Level)(D : Item);  --@RI[  a family of entries]
@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Examples of accept statements:}
@end{WideAbove}
@begin{Example}
@key(accept) Shut_Down;

@key(accept) Read(V : @key(out) Item) @key(do)
   V := Local_Item;
@key(end) Read;

@key(accept) Request(Low)(D : Item) @key(do)
   ...
@key(end) Request;
@end{Example}

@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The syntax rule for @nt{entry_body} is new.

@nt{Accept_statement}s can now have @nt{exception_handler}s.
@end{Extend83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0002],ARef=[AI95-00171-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified the elaboration of
  per-object constraints.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00397-01]}
  @ChgAdded{Version=[2],Text=[@nt{Overriding_indicator}s can be used on
  entries; this is only useful when a task or protected type inherits
  from an interface.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in an @nt{entry_declaration}.
  This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}


@LabeledSubClause{Entry Calls}

@begin{Intro}
@Defn{entry call}
@Redundant[An @nt<entry_call_statement> (an @i(entry call)) can appear in
various contexts.]
@Defn{simple entry call}
@Defn2{Term={entry call}, Sec=(simple)}
A @i(simple) entry call is a stand-alone statement that
represents an unconditional call on an entry of a target
task or a protected object.
@Redundant[Entry calls can also appear
as part of @nt<select_statement>s
(see @RefSecNum(Select Statements)).]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<entry_call_statement>,rhs="@SynI{entry_}@Syn2{name} [@Syn2{actual_parameter_part}];"}
@end{Syntax}

@begin{Resolution}
The @i(entry_)@nt<name> given in an @nt<entry_call_statement> shall resolve
to denote an entry. The rules for parameter
associations are the same as for subprogram calls (see @RefSecNum(Subprogram Calls)
and @RefSecNum(Parameter Associations)).
@end{Resolution}

@begin{StaticSem}
@Redundant[The @i(entry_)@nt<name> of an @nt<entry_call_statement> specifies
(explicitly or implicitly) the target object of the call,
the entry or entry family, and the entry index, if any
(see @RefSecNum(Intertask Communication)).]
@end{StaticSem}

@begin{RunTime}
@leading@Defn{open entry}
@Defn2{Term=[entry], Sec=(open)}
@Defn{closed entry}
@Defn2{Term=[entry], Sec=(closed)}
Under certain circumstances (detailed below), an entry of a task
or protected
object is checked to see whether it is @i(open) or @i(closed):
@begin(Itemize)
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@Defn2{Term=[open entry], Sec=(of a task)}
@Defn2{Term=[closed entry], Sec=(of a task)}
An entry of a task is open if the task
is blocked on an @nt<accept_statement>
that corresponds to the entry (see @RefSecNum(Entries and Accept Statements)),
or on a @nt<selective_accept>
(see @RefSecNum(Selective Accept)) with an open
@nt<accept_alternative> that corresponds to the entry;
otherwise@Chg{Version=[3],New=[,],Old=[]} it is closed.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@Defn2{Term=[open entry], Sec=(of a protected object)}
@Defn2{Term=[closed entry], Sec=(of a protected object)}
An entry of a protected object is open if
the @nt<condition> of the @nt<entry_barrier> of the
corresponding @nt<entry_body> evaluates to True;
otherwise@Chg{Version=[3],New=[,],Old=[]} it is closed.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
If the evaluation of the @nt<condition> propagates an exception, the
exception Program_Error is propagated
to all current callers of all entries of the protected object.
@begin(Reason)
  An exception during barrier evaluation is considered essentially
  a fatal error. All current entry callers are notified with a Program_Error.
  In a fault-tolerant system, a protected object might provide a Reset
  protected procedure, or equivalent, to support attempts to restore such
  a "broken" protected object to a reasonable state.
@end(Reason)
@end(Itemize)
@begin(Discussion)
  Note that the definition of when a task entry is open is
  based on the state of the (accepting) task, whereas the
  "openness" of a protected entry is defined only
  when it is explicitly checked, since the barrier expression needs to
  be evaluated. Implementation permissions are given (below) to
  allow implementations to evaluate the barrier expression more or
  less often than it is checked, but the basic semantic model presumes
  it is evaluated at the times when it is checked.
@end(Discussion)

@leading@PDefn2{Term=[execution], Sec=(entry_call_statement)}
For the execution of an @nt{entry_call_statement},
evaluation of the @nt<name>
and of the parameter associations
is as for a subprogram call (see @RefSecNum{Subprogram Calls}).
@Defn2{Term=[issue], Sec=(an entry call)}
The entry call is then @i(issued):
For a call on an entry of a protected object, a new protected
action is started on the object (see @RefSecNum(Protected Subprograms and Protected Actions)).
The named entry is checked to see if it is open;
@Defn2{Term=[select an entry call], Sec=(immediately)}
if open, the entry call is said to be @i(selected immediately),
and the execution of the call proceeds as follows:
@begin(Itemize)
  For a call on an open entry of a task, the accepting task becomes ready and
  continues the execution of the corresponding @nt<accept_statement>
  (see @RefSecNum(Entries and Accept Statements)).

  For a call on an open entry of a protected object, the corresponding
  @nt<entry_body> is executed (see @RefSecNum(Entries and Accept Statements))
  as part of the protected action.
@end(Itemize)

If the @nt<accept_statement> or @nt<entry_body> completes other than
by a requeue (see @RefSecNum(Requeue Statements)), return is made to the
caller (after servicing the entry queues @em see below);
any necessary assigning back
of formal to actual parameters occurs,
as for a subprogram call (see @RefSecNum(Parameter Associations));
such assignments take
place outside of any protected action.
@begin(Ramification)
  The return to the caller will generally not occur until
  the protected action completes, unless some other thread of
  control is given the job of completing the protected action
  and releasing the associated execution resource.
@end(Ramification)

If the named entry is closed, the entry call is added to an @i(entry queue)
(as part of the protected action, for a call on a protected entry),
and the call remains queued until it is selected or cancelled;
@Defn{entry queue}
there is a separate (logical) entry queue for each entry of a
given task or protected object
@Redundant[(including each entry of an entry family)].

@Leading@Defn2{Term=[service], Sec=(an entry queue)}
@Defn2{Term=[select an entry call], Sec=(from an entry queue)}
When a queued call is @i{selected}, it is removed from its entry queue.
Selecting a queued call from a particular entry queue is
called @i{servicing} the entry queue.
An entry with queued calls can be serviced under
the following circumstances:
@begin(Itemize)
  When the associated task reaches a corresponding @nt<accept_statement>, or
  a @nt<selective_accept> with a corresponding
  open @nt<accept_alternative>;

  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0129-1]}
  If after performing, as part of a protected action on the
  associated protected object, an
  @Chg{Version=[4],New=[exclusive protected ],Old=[]}operation
  on the object@Chg{Version=[4],New=[],Old=[ other than
  a call on a protected function]},
  the entry is checked and found to be open.
@end(Itemize)

@Defn2{Term=[select an entry call], Sec=(from an entry queue)}
If there is at least one call on a queue corresponding to
an open entry, then one such call is selected according to the
@i(entry queuing policy) in effect (see below), and the
corresponding @nt<accept_statement> or @nt<entry_body> is
executed as above for an entry call that is selected immediately.

@Defn{entry queuing policy}
The entry queuing policy controls selection among queued calls
both for task and protected entry queues.
@Defn{default entry queuing policy}
@Defn2{Term=[entry queuing policy], Sec=(default policy)}
The default entry queuing policy is to select calls on a given entry
queue in order of arrival. If calls from two or more queues are
simultaneously eligible for selection, the default entry queuing policy
does not specify which queue is serviced first.
Other entry queuing policies can be specified by @nt{pragma}s
(see @RefSecNum(Entry Queuing Policies)).

For a protected object, the above servicing of entry queues continues
until there are no open entries with queued calls, at which point
the protected action completes.
@begin(Discussion)
  While servicing the entry queues of a protected object, no new calls
  can be added to any entry queue of the object,
  except due to an internal requeue (see @RefSecNum(Requeue Statements)).
  This is because the first step of a call on a protected entry
  is to start a new protected action, which implies acquiring
  (for exclusive read-write access)
  the execution resource associated with the protected object, which cannot
  be done while another protected action is already in progress.
@end(Discussion)

@PDefn2{Term=[blocked], Sec=(during an entry call)}
For an entry call that is added to a queue,
and that is not the @nt<triggering_statement> of an
@nt<asynchronous_@!select>
(see @RefSecNum{Asynchronous Transfer of Control}),
the calling task is blocked until the call is cancelled,
or the call is selected and a corresponding @nt<accept_statement>
or @nt<entry_body> completes without requeuing.
In addition, the calling task is blocked during a rendezvous.

@begin{Ramification}

For a call on a protected entry,
the caller is not blocked if the call is selected immediately,
unless a requeue causes the call to be queued.

@end{Ramification}

@Defn2{Term=[cancellation], Sec=(of an entry call)}
An attempt can be made to cancel an entry call upon an abort
(see @RefSecNum(Abort of a Task - Abort of a Sequence of Statements))
and as part of certain forms of @nt<select_statement>
(see @RefSecNum(Timed Entry Calls),
@RefSecNum(Conditional Entry Calls), and
@RefSecNum(Asynchronous Transfer of Control)).
The cancellation does not take place until
a point (if any) when the call is on some entry queue,
and not protected from cancellation as part of a requeue
(see @RefSecNum(Requeue Statements)); at such a point, the
call is removed from the entry queue and the call completes due
to the cancellation.
The cancellation of a call on an entry of a protected object
is a protected action@Redundant[, and as such cannot take place
while any other protected action is occurring on the protected object.
Like any protected action, it includes servicing of the entry queues
(in case some entry barrier depends on a Count attribute).]
@begin(ImplNote)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
  In the case of an attempted cancellation due to abort,
  this removal might have to be performed by the calling task
  itself if the ceiling priority of the protected object
  is lower than the @Chg{Version=[2],New=[priority of the ],Old=[]}task
  initiating the abort.
@end(ImplNote)

@Defn2{Term=[Tasking_Error],Sec=(raised by failure of run-time check)}
A call on an entry of a task that has already completed its execution
raises the exception Tasking_Error at the point of the call;
similarly, this exception is raised at the point of the call if the
called task completes its execution or becomes abnormal before accepting
the call or completing the rendezvous
(see @RefSecNum(Abort of a Task - Abort of a Sequence of Statements)).
This applies equally to a simple entry call and to an entry call as part
of a @nt<select_statement>.

@end{RunTime}

@begin{ImplPerm}

An implementation may perform the sequence of steps of a protected action
using any thread of control; it need not be that of the task
that started the protected action.
If an @nt<entry_body> completes without requeuing, then the
corresponding calling task may be made ready
without waiting for the entire protected action to complete.
@begin(Reason)
  These permissions are intended to allow flexibility for implementations
  on multiprocessors. On a monoprocessor, which thread of control executes
  the protected action is essentially invisible, since the thread is
  not abortable in any case, and the "current_task" function is not
  guaranteed to work during a protected action
  (see @Chg{Version=[2],New=[@RefSecNum(The Package Task_Identification)],
  Old=[@RefSecNum(Task Information)]}).
@end(Reason)

@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0129-1]}
When the entry of a protected object is checked to see whether it
is open, the implementation need not reevaluate
the @nt<condition> of the corresponding @nt<entry_barrier>
if no variable or attribute referenced by
the @nt<condition> (directly or indirectly)
has been altered by the execution (or cancellation) of a
@Chg{Version=[4],New=[],Old=[protected procedure or entry ]}call
@Chg{Version=[4],New=[to an exclusive protected operation of],Old=[on]} the
object since the @nt<condition> was last evaluated.
@begin(Ramification)
  @ChgRef{Version=[4],Kind=[Revised]}
  Changes to variables referenced by an entry barrier that
  result from actions outside of a
  @Chg{Version=[4],New=[],Old=[protected procedure or entry ]}call
  @Chg{Version=[4],New=[to an exclusive protected operation of],Old=[on]} the
  protected object need not be "noticed." For example, if
  a global variable is referenced by an entry barrier, it should not
  be altered (except as part of a protected action on the object) any
  time after the barrier is first evaluated.
  In other words, globals can be used to "parameterize" a protected object,
  but they cannot reliably be used to control it after the first
  use of the protected object.
@end(Ramification)
@begin{ImplNote}
  Note that even if a global variable is volatile,
  the implementation need only reevaluate a barrier if the
  global is updated during a protected action on the protected object.
  This ensures that an entry-open bit-vector implementation
  approach is possible, where the bit-vector is computed at
  the end of a protected action, rather than upon each entry call.
@end{ImplNote}

An implementation may evaluate the @nt<condition>s of all @nt<entry_barrier>s
of a given protected object any time any entry of the object
is checked to see if it is open.
@begin(Ramification)
  In other words, any side effects of evaluating an entry barrier
  should be innocuous, since an entry barrier might be evaluated more
  or less often than is implied by the "official" dynamic semantics.
@end(Ramification)
@begin(ImplNote)
  It is anticipated that when the number of entries is known to be small,
  all barriers will be evaluated any time one of them needs to be,
  to produce an "entry-open bit-vector." The appropriate bit will
  be tested when the entry is called, and only if the bit is false
  will a check be made to see whether the bit-vector might need to
  be recomputed. This should allow an implementation to maximize
  the performance of a call on an open entry, which seems like the
  most important case.

  In addition to the entry-open bit-vector, an "is-valid"
  bit is needed per object, which indicates whether the current
  bit-vector setting is valid.
  A "depends-on-Count-attribute" bit is needed per type.
  The "is-valid" bit is set to false
  (as are all the bits of the bit-vector) when the protected object is first
  created, as well as any time an exception is propagated from computing
  the bit-vector. Is-valid would also be set false any time the
  Count is changed and
  "depends-on-Count-attribute" is true for the type, or a
  protected procedure or entry returns indicating it might have updated a
  variable referenced in some barrier.

  A single procedure can be compiled to evaluate all of the barriers,
  set the entry-open bit-vector accordingly, and set the is-valid bit to true.
  It could have a "when others" handler to set them all false,
  and call a routine to propagate Program_Error to all queued callers.

  For protected types where the number of entries is not known to be
  small, it makes more sense to evaluate a barrier only when the
  corresponding entry is checked to see if it is open. It isn't worth
  saving the state of the entry between checks, because of the space
  that would be required. Furthermore, the entry queues probably want
  to take up space only when there is actually a caller on them, so
  rather than an array of all entry queues, a linked list of nonempty
  entry queues make the most sense in this case, with the first caller
  on each entry queue acting as the queue header.
@end(ImplNote)

When an attempt is made to cancel an entry call, the implementation
need not make the attempt using the thread of control of the
task (or interrupt) that initiated the cancellation; in particular,
it may use the thread of control of the caller itself to attempt the
cancellation, even if this might allow the entry call to be
selected in the interim.
@begin{Reason}
  Because cancellation of a protected entry call is a protected
  action (which helps make the Count attribute of a protected
  entry meaningful), it might
  not be practical to attempt the cancellation from the thread
  of control that initiated the cancellation. For example,
  if the cancellation is due to the expiration of a delay,
  it is unlikely that the handler of the timer interrupt could
  perform the necessary protected action itself (due to being
  on the interrupt level). Similarly, if the cancellation
  is due to an abort, it is possible that the task initiating
  the abort has a priority higher than the ceiling priority of the
  protected object (for implementations that support ceiling priorities).
  Similar considerations could apply in a multiprocessor situation.
@end{Reason}

@end{ImplPerm}

@begin{Notes}

If an exception is raised during the execution of an @nt{entry_body}, it is
propagated to the corresponding caller (see @RefSecNum(Exception Handling)).

For a call on a protected entry, the entry is checked to see if
it is open prior to queuing the call, and again thereafter
if its Count attribute (see @RefSecNum{Task and Entry Attributes})
is referenced in some entry barrier.
@begin(Ramification)
  Given this, extra care is required if
  a reference to the Count attribute of an entry
  appears in the entry's own barrier.
@end(Ramification)
@begin(Reason)
  An entry is checked to see if it is open prior to queuing
  to maximize the performance of a call on an open entry.
@end(Reason)

In addition to simple entry calls,
the language permits timed, conditional, and asynchronous entry calls
(see @RefSecNum(Timed Entry Calls), @RefSecNum(Conditional Entry Calls),
and see @RefSecNum(Asynchronous Transfer of Control)).
@begin{Ramification}
A task can call its own entries, but
the task will deadlock if the call is a simple entry call.
@end{Ramification}

The @nt<condition> of an @nt<entry_barrier> is allowed to be evaluated by
an implementation more often than strictly necessary, even if the
evaluation might have side effects. On the other hand, an implementation
need not reevaluate the @nt<condition> if nothing it references was
updated by an intervening protected action on the protected object,
even if the @nt<condition> references some global variable that might
have been updated by an action performed from outside of a protected action.
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Examples of entry calls:}
@begin{Example}
Agent.Shut_Down;                      --@RI[  see @RefSecNum(Task Units and Task Objects)]
Parser.Next_Lexeme(E);                --@RI[  see @RefSecNum(Task Units and Task Objects)]
Pool(5).Read(Next_Char);              --@RI[  see @RefSecNum(Task Units and Task Objects)]
Controller.Request(Low)(Some_Item);   --@RI[  see @RefSecNum(Task Units and Task Objects)]
Flags(3).Seize;                       --@RI[  see @RefSecNum(Protected Units and Protected Objects)]
@end{Example}
@end{Examples}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0129-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Revised wording to talk
  about @ldquote@;exclusive protected operations@rdquote
  (see @RefSecNum{Protected Subprograms and Protected Actions}).]}
@end{DiffWord2012}


@LabeledSubClause{Requeue Statements}

@begin{Intro}
@redundant[A @nt<requeue_statement>
can be used to complete an @nt<accept_statement> or @nt<entry_body>,
while redirecting the corresponding entry call to a new (or the same)
entry queue.
@Defn{requeue}
Such a @i(requeue) can be performed with or without allowing
an intermediate cancellation of the call, due to an abort or
the expiration of a delay.
@IndexSee{Term=[preference control],See=(requeue)}
@IndexSee{Term=[broadcast signal],See=(requeue)}]
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0030-2]}
@Syn{lhs=<requeue_statement>,
  rhs="@key{requeue} @SynI{@Chg{Version=[3],New=[procedure_or_entry_],Old=[entry_]}}@Syn2{name} [@key{with} @key{abort}];"}
@end{Syntax}

@begin{Resolution}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0030-2],ARef=[AI05-0215-1]}
@Chg{Version=[3],New=[@Defn{requeue target}],Old=[@Defn2{Term=[target entry], Sec=(of a @nt<requeue_statement>)}]}
The @Chg{Version=[3],New=[@SynI{procedure_or_entry_}],Old=[@SynI(entry_)]}@nt{name}
of a @nt{requeue_statement} shall resolve
to denote @Chg{Version=[3],New=[a procedure or ],Old=[]}an entry (the
@Chg{Version=[3],New=[@i{requeue }],Old=[]}@i{target}@Chg{Version=[3],New=[],
Old=[@i{ entry}]})@Chg{Version=[3],New=[. The profile of
the entry, or the profile or prefixed profile of the procedure, shall],Old=[that]}
either @Chg{Version=[3],New=[have],Old=[has]} no parameters, or @Chg{Version=[3],
New=[be],Old=[that has
a profile that is]} type conformant (see @RefSecNum(Conformance Rules)) with
the profile of the innermost enclosing @nt<entry_@!body> or
@nt<accept_@!statement>.
@Defn2{Term=[type conformance],Sec=(required)}
@end{Resolution}

@begin{Legality}

A @nt{requeue_statement} shall be within a callable
construct that is either an @nt{entry_body} or an
@nt{accept_statement}, and this construct shall
be the innermost enclosing body or callable construct.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0030-2],ARef=[AI05-0215-1]}
If the
@Chg{Version=[3],New=[requeue ],Old=[]}target@Chg{Version=[3],New=[],Old=[ entry]}
has parameters, then its @Chg{Version=[3],New=[(prefixed) ],Old=[]}profile
shall be subtype conformant with
the profile of the innermost enclosing callable construct.
@Defn2{Term=[subtype conformance],Sec=(required)}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0090-1]}
@ChgAdded{Version=[4],Type=[Leading],Text=[Given a requeue_statement where the
innermost enclosing callable construct is for an entry @i<E1>, for every
@Redundant[specific or class-wide ]postcondition expression @i<P1> that
applies to @i<E1>, there shall exist a postcondition expression @i<P2> that
applies to the requeue target @i<E2> such that]}
@begin{Itemize}
  @ChgRef{Version=[4],Kind=[Added]}
  @ChgAdded{Version=[4],Text=[@i<P1> is fully conformant with
  the expression produced by replacing each reference in @i<P2> to a formal
  parameter of @i<E2> with a reference to the corresponding formal paramter
  of @i<E1>; and]}

  @ChgRef{Version=[4],Kind=[Added]}
  @ChgAdded{Version=[4],Text=[if @i<P1> is enabled, then @i<P2> is also enabled.]}
@end{Itemize}
@begin{Discussion}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[Roughly speaking, the postcondition of the requeue
  target is required to imply that of the enclosing callable construct.]}
@end{Discussion}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0090-1]}
@ChgAdded{Version=[4],Text=[The requeue target shall not have an applicable
specific or class-wide postcondition which includes an Old
attribute_reference.]}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0090-1]}
@ChgAdded{Version=[4],Text=[If the requeue target is declared immediately
within the @nt{task_definition} of a named task type or the
@nt{protected_definition} of a named protected type, and if the requeue
statement occurs within the body of that type, and if the requeue is an external
requeue, then the requeue target shall not have a specific or class-wide
postcondition which includes a name denoting either the current instance of that
type or any entity declared within the declaration of that type.]}

@begin{Discussion}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgAdded{Version=[4],Text=[The above pair of rules always apply; they
  don't depend on whether or not any of the postconditions are enabled.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0030-2],ARef=[AI05-0215-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0090-1]}@Comment{Just a number change}
@ChgAdded{Version=[3],Text=[If the target is a procedure, the name shall
denote a renaming of an entry, or shall denote a view or a prefixed view of a
primitive subprogram of a synchronized interface, where the first parameter
of the unprefixed view of the primitive subprogram shall be a
controlling parameter, and the Synchronization aspect shall be specified
with @nt{synchronization_kind} By_Entry for the primitive subprogram.]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0030-2]}
@PDefn2{Term=[accessibility rule],Sec=(requeue statement)}
In a @nt<requeue_statement> of an @nt<accept_statement> of
some task unit, either the target object shall be a part of a
formal parameter of the @nt<accept_statement>,
or the accessibility level of the target object
shall not be equal to or statically deeper than any
enclosing @nt<accept_statement> of the task unit.
In a @nt<requeue_@!statement> of an @nt<entry_@!body>
of some protected unit, either the target object shall be
a part of a formal parameter of the @nt<entry_@!body>,
or the accessibility level of the target object
shall not be statically deeper than that
of the @nt<entry_declaration>@Chg{Version=[3],New=[ for the @nt{entry_body}],Old=[]}.

@begin{Ramification}
  In the @nt{entry_body} case, the intent is that the target object can
  be global,
  or can be a component of the protected unit,
  but cannot be a local variable of the @nt{entry_body}.
@end{Ramification}
@begin(Reason)
  These restrictions ensure that the target object of the requeue outlives the
  completion and finalization of the enclosing callable construct.
  They also prevent requeuing from a nested
  @nt<accept_statement> on a parameter of an outer @nt<accept_statement>,
  which could create some strange "long-distance" connections between
  an entry caller and its server.

  Note that in the strange case where a @nt<task_body> is nested inside
  an @nt<accept_statement>, it is permissible to requeue from an
  @nt<accept_statement> of the inner @nt<task_body> on parameters of
  the outer @nt<accept_statement>. This is not
  a problem because all calls on the inner task have to complete before
  returning from the outer @nt<accept_statement>, meaning no "dangling
  calls" will be created.
@end(Reason)
@begin(ImplNote)
  By disallowing certain requeues,
  we ensure that the normal @nt<terminate_alternative> rules remain
  sensible, and that explicit clearing of the entry queues of a protected
  object during finalization is rarely necessary. In particular, such
  clearing of the entry queues is necessary only (ignoring premature
  Unchecked_Deallocation) for protected objects declared in a
  @nt<task_body> (or created by an allocator for an access type declared
  in such a body) containing one or more @nt<requeue_statement>s.
  Protected objects declared in subprograms, or at the library level,
  will never need to have their entry queues explicitly cleared during
  finalization.
@end(ImplNote)
@end{Legality}

@begin{RunTime}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0030-2]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0090-1]}
@PDefn2{Term=[execution], Sec=(requeue_statement)}
The execution of a @nt{requeue_statement} proceeds by first evaluating the
@Chg{Version=[3],New=[@SynI{procedure_or_entry_}],Old=[@SynI(entry_)]}@nt<name>@Redundant[,
including the @nt<prefix> identifying the target task
or protected object and the @nt<expression> identifying the entry within an
entry family, if any]. @Chg{Version=[4],New=[Precondition checks are then
performed as for a call to the requeue target entry or subprogram. ],Old=[]}The
@nt{entry_body} or @nt{accept_statement} enclosing the @nt{requeue_statement} is
then completed@Redundant[, finalized, and
left (see @RefSecNum(Completion and Finalization))].

@PDefn2{Term=[execution], Sec=(requeue task entry)}
For the execution of a requeue on an entry of a target task,
after leaving the enclosing callable construct, the named entry
is checked to see if it is open and the requeued call is either
selected immediately or queued, as for a normal entry call
(see @RefSecNum(Entry Calls)).

@leading@PDefn2{Term=[execution], Sec=(requeue protected entry)}
For the execution of a requeue on an entry of a target protected
object, after leaving the enclosing callable construct:
@begin(Itemize)
  if the requeue is an internal requeue
  (that is, the requeue is back on an entry of the same protected object @em
  see @RefSecNum(Intertask Communication)),
  the call is added to the queue of the named entry and
  the ongoing protected action continues (see @RefSecNum(Protected Subprograms and Protected Actions));
  @begin(Ramification)
    Note that for an internal requeue, the call
    is queued without checking whether the target entry is open.
    This is because the entry queues will be serviced before the
    current protected action completes anyway, and considering the
    requeued call immediately might allow it to "jump" ahead of
    existing callers on the same queue.
  @end(Ramification)

  if the requeue is an external requeue (that is, the target protected
  object is not implicitly the same as the current object @em
  see @RefSecNum(Intertask Communication)),
  a protected action is started on the target object and proceeds
  as for a normal entry call (see @RefSecNum(Entry Calls)).
@end(Itemize)

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0030-2]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI05-0090-1]}
If the @Chg{Version=[3],New=[requeue target],Old=[new entry]}
named in the @nt<requeue_statement>
has formal parameters, then during the execution of the
@nt<accept_statement> or @nt<entry_body> corresponding to the new
entry@Chg{Version=[4],New=[ and during the checking of
any preconditions of the new entry],Old=[]},
the formal parameters denote the same objects as
did the corresponding formal parameters
of the callable construct completed by the requeue.
@Redundant[In any case, no parameters are specified in a
@nt<requeue_statement>; any parameter passing is implicit.]

@leading@Defn{requeue-with-abort}
If the @nt<requeue_statement> includes the reserved words @key(with abort)
(it is a @i(requeue-with-abort)), then:
@begin(Itemize)
  if the original entry call has been aborted
  (see @RefSecNum(Abort of a Task - Abort of a Sequence of Statements)), then
  the requeue acts as an abort completion point for the call,
  and the call is cancelled and no requeue is
  performed;

  if the original entry call was timed (or conditional),
  then the original expiration time is the expiration
  time for the requeued call.
@end(Itemize)

If the reserved words @key(with abort) do not appear, then the
call remains protected against cancellation while queued as the result
of the @nt<requeue_statement>.
@begin(Ramification)
  This protection against cancellation lasts only until the
  call completes or a subsequent requeue-with-abort is performed
  on the call.
@end(Ramification)
@begin(Reason)
  We chose to protect a requeue, by default, against abort or cancellation.
  This seemed safer, since it is likely that extra steps need to be taken
  to allow for possible cancellation once the servicing of an entry
  call has begun. This also means that in the absence of @key(with abort)
  the usual Ada 83 behavior is preserved, namely that once an
  entry call is accepted, it cannot be cancelled until it completes.
@end(Reason)
@end{RunTime}

@begin{Notes}

A requeue is permitted from a single entry to an entry of
an entry family, or vice-versa. The entry index, if any,
plays no part in the subtype conformance check between the
profiles of the two entries; an entry index
is part of the @i(entry_)@nt<name> for an entry of a family.
@PDefn{subtype conformance}

@end{Notes}

@begin{Examples}
@leading@keepnext@i{Examples of requeue statements:}
@begin{Example}
@key[requeue] Request(Medium) @key[with abort];
                    --@RI[ requeue on a member of an entry family of the current task, see @RefSecNum{Task Units and Task Objects}]

@key[requeue] Flags(I).Seize;
                    --@RI[ requeue on an entry of an array component, see @RefSecNum{Protected Units and Protected Objects}]
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The @nt<requeue_statement> is new.
@end{Extend83}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0030-2],ARef=[AI05-0215-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Added the ability
  to requeue on operations of synchronized interfaces that are
  declared to be an entry.]}
@end{Extend2005}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI05-0090-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  We now define that any preconditions of the requeue target are evaluated
  as part of a @nt<requeue_statement>. Original Ada 2012 did not specify this,
  so a program that requeues when the preconditions fail will raise an
  exception when none would happen in original Ada 2012. We don't expect this
  to be a problem, as in that case, the entry body would be called with some
  of its preconditions evaluating as False; the body is likely to assume that
  they are true and probably will have failed in some other way anyway.]}
@end{Inconsistent2012}

@begin{Incompatible2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI05-0090-1]}
  @ChgAdded{Version=[4],Text=[@Defn{incompatibilities with Ada 2012}@b<Corrigendum:>
  If a requeue target has a different postcondition than the original
  entry, the requeue is now illegal. In such a case, the original postcondition
  would never have been evaluated, and assumptions that the caller relied upon
  might not be true. A requeue should be invisible to the caller with respect
  to any postconditions; thus we only allow it when the original entry has no
  postconditions or the requeue target has (at least) the same postconditions.]}
@end{Incompatible2012}


@LabeledClause{Delay Statements, Duration, and Time}

@begin{Intro}
@redundant[@PDefn{expiration time}
A @nt<delay_statement> is used to block further execution until
a specified @i(expiration time) is reached.
The expiration time
can be specified either as a particular point in time (in a
@nt<delay_@!until_@!statement>), or in seconds from the current time
(in a @nt<delay_@!relative_@!statement>).
The language-defined
package Calendar provides definitions for a type Time and associated
operations, including a function Clock that returns the current time.
@IndexSee{Term=[timing],See=(delay_statement)}]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<delay_statement>,
  rhs="@Syn2{delay_until_statement} | @Syn2{delay_relative_statement}"}


@Syn{lhs=<delay_until_statement>,
  rhs="@key{delay until} @SynI(delay_)@Syn2{expression};"}

@Syn{lhs=<delay_relative_statement>,
  rhs="@key{delay} @SynI(delay_)@Syn2{expression};"}
@end{Syntax}

@begin{Resolution}

@PDefn2{Term=[expected type],
  Sec=(delay_relative_statement expression)}
The expected type for the @i(delay_)@nt{expression} in a
@nt{delay_relative_statement} is the predefined type Duration.
@PDefn2{Term=[expected type],
  Sec=(delay_until_statement expression)}
The @i(delay_)@nt<expression> in a @nt<delay_until_statement>
is expected to be of any nonlimited type.

@end{Resolution}

@begin{Legality}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
@Defn{time type}
@Defn{time base}
@Defn{clock}
There can be multiple time bases, each with a corresponding
clock, and a corresponding @i{time type}.
The type of the @i(delay_)@nt<expression>
in a @nt{delay_until_statement} shall be a time type @em either the
type Time defined in the language-defined package Calendar (see
below),@Chg{Version=[3],New=[ the type Time in the package Real_Time
(see @RefSecNum(Monotonic Time)),],Old=[]}
or some other implementation-defined time type@Chg{Version=[3],New=[],Old=[
(see @RefSecNum(Monotonic Time))]}.
@ImplDef{Any implementation-defined time types.}
@end{Legality}

@begin{StaticSem}
@Redundant[There is a predefined fixed point type
named Duration, declared in the visible part of package Standard;]
a value of type Duration is used to represent the length
of an interval of time, expressed in seconds.
@Redundant[The type Duration is not specific to a particular time base,
but can be used with any time base.]

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
A value of the type Time in package Calendar, or of some other
@Chg{Version=[3],New=[],Old=[implementation-defined ]}time type,
represents a time as reported by a corresponding clock.

@leading@keepnext@;The following language-defined library package exists:
@begin{Example}
@ChildUnit{Parent=[Ada],Child=[Calendar]}
@key(package) Ada.Calendar @key(is)
  @key(type) @AdaTypeDefn{Time} @key(is) @key(private);

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00351-01]}
  @key(subtype) @AdaSubtypeDefn{Name=[Year_Number],Of=[Integer]}  @key(is) Integer @key(range) 1901 .. @Chg{Version=[2],New=[2399],Old=[2099]};
  @key(subtype) @AdaSubtypeDefn{Name=[Month_Number],Of=[Integer]} @key(is) Integer @key(range) 1 .. 12;
  @key(subtype) @AdaSubtypeDefn{Name=[Day_Number],Of=[Integer]}   @key(is) Integer @key(range) 1 .. 31;
  @key(subtype) @AdaSubtypeDefn{Name=[Day_Duration],Of=[Duration]} @key(is) Duration @key(range) 0.0 .. 86_400.0;

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
  @ChgAdded{Version=[2],Text=[A range of 500 years was chosen, as that only
  requires one extra bit for the year as compared to Ada 95. This was done
  to minimize disruptions with existing implementations. (One implementor
  reports that their time values represent nanoseconds, and this year range
  requires 63.77 bits to represent.)]}
@end{Reason}

  @key(function) @AdaSubDefn{Clock} @key(return) Time;

  @key(function) @AdaSubDefn{Year}   (Date : Time) @key(return) Year_Number;
  @key(function) @AdaSubDefn{Month}  (Date : Time) @key(return) Month_Number;
  @key(function) @AdaSubDefn{Day}    (Date : Time) @key(return) Day_Number;
  @key(function) @AdaSubDefn{Seconds}(Date : Time) @key(return) Day_Duration;

  @key(procedure) @AdaSubDefn{Split} (Date  : @key(in) Time;
                   Year    : @key(out) Year_Number;
                   Month   : @key(out) Month_Number;
                   Day     : @key(out) Day_Number;
                   Seconds : @key(out) Day_Duration);

  @key(function) @AdaSubDefn{Time_Of}(Year  : Year_Number;
                   Month   : Month_Number;
                   Day     : Day_Number;
                   Seconds : Day_Duration := 0.0)
   @key(return) Time;

  @key(function) "+" (Left : Time;   Right : Duration) @key(return) Time;
  @key(function) "+" (Left : Duration; Right : Time) @key(return) Time;
  @key(function) "-" (Left : Time;   Right : Duration) @key(return) Time;
  @key(function) "-" (Left : Time;   Right : Time) @key(return) Duration;

  @key(function) "<" (Left, Right : Time) @key(return) Boolean;
  @key(function) "<="(Left, Right : Time) @key(return) Boolean;
  @key(function) ">" (Left, Right : Time) @key(return) Boolean;
  @key(function) ">="(Left, Right : Time) @key(return) Boolean;

  @AdaExcDefn{Time_Error} : @key(exception;)

@key(private)
   ... -- @RI{not specified by the language}
@key(end) Ada.Calendar;

@end{Example}

@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[execution], Sec=(delay_statement)}
For the execution of a @nt<delay_statement>, the @i(delay_)@nt<expression>
is first evaluated.
@Defn2{Term=[expiration time], Sec=(for a @nt<delay_until_statement>)}
For a @nt<delay_until_statement>, the expiration time for the
delay is the value of the @i(delay_)@nt<expression>, in the time
base associated with the type of the @nt<expression>.
@Defn2{Term=[expiration time], Sec=(for a @nt<delay_relative_statement>)}
For a @nt<delay_relative_statement>, the expiration time is
defined as the current time, in the time base associated
with relative delays, plus
the value of the @i(delay_)@nt<expression>
converted to the type Duration, and then rounded up
to the next clock tick.
@PDefn2{Term=[implicit subtype conversion],Sec=(delay expression)}
The time base associated with relative delays
is as defined in @RefSec{Delay Accuracy} or is
implementation defined.
@ImplDef{The time base associated with relative delays.}
@begin{Ramification}
  Rounding up to the next clock tick means that the reading of the
  delay-relative clock when the delay expires should be no less than
  the current reading of the delay-relative
  clock plus the specified duration.
@end{Ramification}

@PDefn2{Term=[blocked], Sec=(on a @nt<delay_statement>)}
The task executing a @nt<delay_statement> is blocked
until the expiration time is reached, at which point it
becomes ready again. If the expiration time
has already passed, the task is not blocked.
@begin(Discussion)
  For a @nt<delay_relative_statement>, this case corresponds to
  when the value of the @i(delay_)@nt<expression> is zero
  or negative.

  Even though the task is not blocked,
  it might be put back on the end of its ready queue.
  See @RefSec(Priority Scheduling).
@end(Discussion)

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
@Defn2{Term=[cancellation], Sec=(of a @nt<delay_statement>)}
If an attempt is made to @i(cancel) the @nt<delay_statement>
@Redundant[(as part of an @nt<asynchronous_@!select> or abort @em
see @RefSecNum{Asynchronous Transfer of Control} and
@RefSecNum{Abort of a Task - Abort of a Sequence of Statements})],
the @Chg{Version=[3],New=[statement],Old=[@ntf<_statement>]}
is cancelled if the expiration time has not yet passed,
thereby completing the @nt<delay_statement>.
@begin(Reason)
  This is worded this way so that in an @nt<asynchronous_select>
  where the @nt<triggering_statement> is a @nt<delay_statement>,
  an attempt to cancel the delay when the @nt<abortable_part> completes
  is ignored if the expiration time has already passed, in which case the
  optional statements of the @nt<triggering_alternative> are executed.
@end(Reason)

The time base associated with the type Time of package Calendar
is implementation defined. The function Clock of package Calendar
returns a value representing the current time for this time base.
@Redundant[The implementation-defined value of the
named number System.Tick (see @RefSecNum(The Package System))
is an approximation of the length of the real-time
interval during which the value of Calendar.Clock remains constant.]
@ImplDef{The time base of the type Calendar.Time.}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00351-01]}
The functions Year,
Month, Day, and Seconds return the corresponding values for
a given value of the type Time,
as appropriate to an implementation-defined
@Chg{Version=[2],New=[time zone],Old=[timezone]}; the procedure Split
returns all four
corresponding values. Conversely, the function Time_Of combines a
year number, a month number, a day number, and a duration, into
a value of type Time. The operators "+" and "@en@;" for addition
and subtraction of times and durations, and the relational operators
for times, have the conventional meaning.
@ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],
Text=[The @Chg{Version=[2],New=[time zone],Old=[timezone]} used for
package Calendar operations.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0119-1]}
  @ChgAdded{Version=[3],Text=[The behavior of these values and subprograms if
  the time zone changes is also implementation-defined. In particular, the
  changes associated with summer time adjustments (like Daylight Savings Time in
  the United States) should be treated as a change in the implementation-defined
  time zone. The language does not specify whether the time zone information is
  stored in values of type Time; therefore the results of binary operators are
  unspecified when the operands are the two values with different
  effective time zones. In particular, the results of "-" may differ from the
  "real" result by the difference in the time zone adjustment. Similarly, the
  result of UTC_Time_Offset (see 9.6.1) may or may not reflect a time zone
  adjustment.]}
@end{Ramification}

If Time_Of is called with a seconds value of 86_400.0, the value
returned is equal to the value of Time_Of for the next day
with a seconds value of 0.0.
The value returned by the function
Seconds or through the Seconds parameter of the procedure
Split is always less than 86_400.0.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0030],ARef=[AI95-00113-01]}
The exception Time_Error is raised by the function Time_Of if the
actual parameters do not form a proper date. This exception
is also raised by the operators "+" and "@en@;" if the
result is not representable in the type Time or Duration, as appropriate.
This exception is also raised by the
function@Chg{New=[s],Old=[]} Year@Chg{New=[, Month, Day, and Seconds and],
Old=[or]} the procedure Split if the year number of the given date is
outside of the range of the subtype Year_Number.
@begin(Honest)
  @ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0106],ARef=[AI95-00160-01]}
  By "proper date" above we mean that the given year has
  a month with the given day. For example, February 29th is
  a proper date only for a leap year. @Chg{New=[We do not mean to include
  the Seconds in this notion; in particular, we do not mean to require
  implementations to check for the @lquotes@;missing hour@rquotes that occurs
  when Daylight Savings Time starts in the spring.],Old=[]}
@end(Honest)
@begin(Reason)
  @ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0030],ARef=[AI95-00113-01]}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00351-01]}
  We allow Year and Split to raise Time_Error because the arithmetic operators
  are allowed (but not required) to produce times that are outside the range
  of years from 1901 to @Chg{Version=[2],New=[2399],Old=[2099]}. This is
  similar to the way integer operators may
  return values outside the base range of their type so long as the value is
  mathematically correct.
  @Chg{New=[We allow the functions Month, Day and Seconds to raise Time_Error
  so that they can be implemented in terms of Split.],Old=[]}
@end(Reason)
@end{RunTime}

@begin{ImplReq}
The implementation of the type Duration shall allow representation of
time intervals (both positive and negative) up to at least 86400 seconds (one
day); Duration'Small shall not be greater than twenty milliseconds.
The implementation of the type Time shall allow representation of
all dates with year numbers in the range of Year_Number@Redundant[; it
may allow representation of other dates as well (both earlier and later).]
@end{ImplReq}

@begin{ImplPerm}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
An implementation may define additional time
types@Chg{Version=[3],New=[],Old=[ (see @RefSecNum{Monotonic Time})]}.

An implementation may raise Time_Error if the
value of a @i{delay_}@nt<expression> in a @nt<delay_until_statement>
of a @nt<select_statement> represents a time more than 90 days past the
current time. The actual limit, if any, is implementation-defined.
@ImplDef{Any limit on @nt<delay_until_statement>s of @nt<select_statement>s.}
@begin{ImplNote}
  This allows an implementation to implement @nt<select_statement>
  timeouts using
  a representation that does not support the full range of a time type.
  In particular 90 days of seconds can be represented in 23 bits,
  allowing a signed 24-bit representation for the seconds part of
  a timeout.
  There is no similar restriction allowed for stand-alone
  @nt<delay_until_statement>s, as these can be implemented
  internally using a loop if necessary to accommodate a long delay.
@end{ImplNote}

@end{ImplPerm}

@begin{ImplAdvice}

Whenever possible in an implementation, the value of
Duration'Small should be no greater than 100 microseconds.
@begin(ImplNote)
  This can be satisfied using a 32-bit 2's complement representation
  with a @i(small) of 2.0**(@en@;14) @em that is, 61 microseconds @em and a
  range of @PorM 2.0**17 @em that is, 131_072.0.
@end(ImplNote)
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The value of Duration'Small should be no greater than 100 microseconds.]}]}

The time base for @nt{delay_relative_statement}s should be monotonic;
it need not be the same time base as used for Calendar.Clock.
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The time base for @nt{delay_relative_statement}s should be monotonic.]}]}

@end{ImplAdvice}

@begin{Notes}

A @nt{delay_relative_statement} with a negative value of the
@i(delay_)@nt<expression> is equivalent to one with a zero value.

A @nt{delay_statement} may be executed by the environment task;
consequently @nt{delay_statement}s may be executed as part of
the elaboration of a @nt{library_item} or the execution of the main subprogram.
Such statements delay the environment task (see @RefSecNum(Program Execution)).

@PDefn2{Term=[potentially blocking operation],Sec=(delay_statement)}
@PDefn2{Term=[blocking, potentially],Sec=(delay_statement)}
A @nt{delay_statement} is an abort completion point and
a potentially blocking operation,
even if the task is not actually blocked.

There is no necessary relationship between System.Tick (the
resolution of the clock of package Calendar)
and Duration'Small (the @i(small) of type Duration).
@begin{Ramification}
The inaccuracy of the @nt{delay_statement} has no relation to System.Tick.
In particular, it is possible that the clock used for the
@nt{delay_statement} is less accurate than Calendar.Clock.

We considered making Tick a run-time-determined quantity,
to allow for easier configurability.
However, this would not be upward compatible,
and the desired configurability can be achieved using
functionality defined in @RefSec{Real-Time Systems}.
@end{Ramification}

Additional requirements associated with @nt<delay_statement>s
are given in @RefSec(Delay Accuracy).

@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of a relative delay statement:}
@begin{example}
@key(delay) 3.0;  --@RI[ delay 3.0 seconds]
@end{example}

@begin{WideAbove}
@leading@keepnext@Defn2{Term=[periodic task],Sec=(example)}
@IndexSee{Term=[periodic task],See=(delay_until_statement)}
@i{Example of a periodic task:}
@end{WideAbove}
@begin{example}
@key(declare)
   @key(use) Ada.Calendar;
   Next_Time : Time := Clock + Period;
                      --@RI[ Period is a global constant of type Duration]
@key(begin)
   @key(loop)               --@RI[ repeated every Period seconds]
      @key(delay) @key(until) Next_Time;
      ... --@RI[ perform some actions]
      Next_Time := Next_Time + Period;
   @key(end) @key(loop;)
@key(end;)
@end{example}
@end{Examples}

@begin{Inconsistent83}
@Defn{inconsistencies with Ada 83}
For programs that raise Time_Error on "+" or "@en@;" in Ada 83,the exception
might be deferred until a call on Split or Year_Number, or might
not be raised at all (if the offending time is never Split after being
calculated). This should not affect typical programs,
since they deal only with times corresponding to the relatively
recent past or near future.
@end{Inconsistent83}

@begin{Extend83}
@Defn{extensions to Ada 83}
The syntax rule for @nt{delay_statement} is modified to allow
@nt{delay_until_statement}s.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00351-01]}
The type Time may represent dates with year numbers outside of Year_Number.
Therefore, the operations "+" and
"@en@;" need only raise Time_Error if the result is not representable
in Time (or Duration); also, Split or Year will now raise Time_Error
if the year number is outside of Year_Number.
This change is intended to simplify the implementation
of "+" and "@en@;" (allowing them to depend on overflow for
detecting when to raise Time_Error) and to allow local
@Chg{Version=[2],New=[time zone],Old=[timezone]} information to be
considered at the time of Split rather than Clock (depending on
the implementation approach). For example, in a POSIX environment,
it is natural for the type Time to be based on GMT, and
the results of procedure Split (and the functions
Year, Month, Day, and Seconds) to depend on local time zone information.
In other environments, it is more natural for the type Time to
be based on the local time zone, with the results of
Year, Month, Day, and Seconds being pure functions of their input.

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00351-01]}
@ChgDeleted{Version=[2],Text=[We anticipate that implementations will provide
child packages of Calendar to provide more explicit control over time zones
and other environment-dependent time-related issues.
These would be appropriate for standardization in a given
environment (such as POSIX).]}
@end{Extend83}

@begin{Inconsistent95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
  @ChgAdded{Version=[2],Text=[@Defn{inconsistencies with Ada 95}The upper bound
  of Year_Number has been changed to avoid a year 2100 problem. A program
  which expects years past 2099 to raise Constraint_Error will fail in Ada 2005.
  We don't expect there to be many programs which are depending on an exception
  to be raised. A program that uses Year_Number'Last as a magic number may also
  fail if values of Time are stored outside of the program.
  Note that the lower bound of Year_Number wasn't changed, because
  it is not unusual to use that value in a constant to represent an unknown
  time.]}
@end{Inconsistent95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0002],ARef=[AI95-00171-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that Month, Day, and
  Seconds can raise Time_Error.]}
@end{DiffWord95}


@LabeledAddedSubclause{Version=[2],Name=[Formatting, Time Zones, and other operations for Time]}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01],ARef=[AI95-00427-01]}
@ChgAdded{Version=[2],Text=[The following language-defined library packages exist:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@ChildUnit{Parent=[Ada.Calendar],Child=[Time_Zones]}@key(package) Ada.Calendar.Time_Zones @key(is)]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[Time zone manipulation:]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<type> @AdaTypeDefn{Time_Offset} @key<is range> -28*60 .. 28*60;]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0005-1]}
  @ChgAdded{Version=[2],Text=[We want to be able to specify the difference
  between any two arbitrary time zones. You might think that 1440 (24 hours)
  would be enough, but there are places (like Tonga, which is UTC+13hr) which
  are more than 12 hours @Chg{Version=[4],New=[different ],Old=[]}than UTC.
  Combined with summer time (known as daylight saving time in some parts of the
  world) @en which switches opposite in the northern and
  @Chg{Version=[4],New=[southern],Old=[souther]} hemispheres @en and even greater
  differences are possible. We know of cases of a 26 hours difference, so we err
  on the safe side by selecting 28 hours as the limit.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaExcDefn{Unknown_Zone_Error} : @key<exception>;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{UTC_Time_Offset} (Date : Time := Clock) @key<return> Time_Offset;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key<end> Ada.Calendar.Time_Zones;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@ChildUnit{Parent=[Ada.Calendar],Child=[Arithmetic]}
@key(package) Ada.Calendar.Arithmetic @key(is)]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[Arithmetic on days:]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<type> @AdaTypeDefn{Day_Count} @key<is range>
     -366*(1+Year_Number'Last - Year_Number'First)
     ..
     366*(1+Year_Number'Last - Year_Number'First);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<subtype> @AdaSubtypeDefn{Name=[Leap_Seconds_Count],Of=[Integer]} @key<is> Integer @key<range> -2047 .. 2047;]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The maximum number of leap seconds is likely
  to be much less than this, but we don't want to reach the limit too soon
  if the earth's behavior suddenly changes. We believe that the maximum number
  is 1612, based on the current rules, but that number is too weird to use here.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<procedure> @AdaSubDefn{Difference} (Left, Right : @key<in> Time;
                         Days : @key<out> Day_Count;
                         Seconds : @key<out> Duration;
                         Leap_Seconds : @key<out> Leap_Seconds_Count);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> "+" (Left : Time; Right : Day_Count) @key<return> Time;
   @key<function> "+" (Left : Day_Count; Right : Time) @key<return> Time;
   @key<function> "-" (Left : Time; Right : Day_Count) @key<return> Time;
   @key<function> "-" (Left, Right : Time) @key<return> Day_Count;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key<end> Ada.Calendar.Arithmetic;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@ChildUnit{Parent=[Ada.Calendar],Child=[Formatting]}
@key<with> Ada.Calendar.Time_Zones;
@key(package) Ada.Calendar.Formatting @key(is)]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[Day of the week:]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<type> @AdaTypeDefn{Day_Name} @key<is> (@AdaObjDefn{Monday}, @AdaObjDefn{Tuesday}, @AdaObjDefn{Wednesday}, @AdaObjDefn{Thursday},
       @AdaObjDefn{Friday}, @AdaObjDefn{Saturday}, @AdaObjDefn{Sunday});]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Day_of_Week} (Date : Time) @key<return> Day_Name;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[Hours:Minutes:Seconds access:]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<subtype> @AdaSubtypeDefn{Name=[Hour_Number],Of=[Natural]}         @key<is> Natural @key<range> 0 .. 23;
   @key<subtype> @AdaSubtypeDefn{Name=[Minute_Number],Of=[Natural]}       @key<is> Natural @key<range> 0 .. 59;
   @key<subtype> @AdaSubtypeDefn{Name=[Second_Number],Of=[Natural]}       @key<is> Natural @key<range> 0 .. 59;
   @key<subtype> @AdaSubtypeDefn{Name=[Second_Duration],Of=[Day_Duration]}     @key<is> Day_Duration @key<range> 0.0 .. 1.0;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Year}       (Date : Time;
                        Time_Zone  : Time_Zones.Time_Offset := 0)
                           @key<return> Year_Number;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Month}      (Date : Time;
                        Time_Zone  : Time_Zones.Time_Offset := 0)
                           @key<return> Month_Number;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Day}        (Date : Time;
                        Time_Zone  : Time_Zones.Time_Offset := 0)
                           @key<return> Day_Number;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Hour}       (Date : Time;
                        Time_Zone  : Time_Zones.Time_Offset := 0)
                           @key<return> Hour_Number;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Minute}     (Date : Time;
                        Time_Zone  : Time_Zones.Time_Offset := 0)
                           @key<return> Minute_Number;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Second}     (Date : Time)
                           @key<return> Second_Number;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Sub_Second} (Date : Time)
                           @key<return> Second_Duration;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Seconds_Of} (Hour   :  Hour_Number;
                        Minute : Minute_Number;
                        Second : Second_Number := 0;
                        Sub_Second : Second_Duration := 0.0)
       @key<return> Day_Duration;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<procedure> @AdaSubDefn{Split} (Seconds    : @key<in> Day_Duration;
                    Hour       : @key<out> Hour_Number;
                    Minute     : @key<out> Minute_Number;
                    Second     : @key<out> Second_Number;
                    Sub_Second : @key<out> Second_Duration);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Time_Of} (Year       : Year_Number;
                     Month      : Month_Number;
                     Day        : Day_Number;
                     Hour       : Hour_Number;
                     Minute     : Minute_Number;
                     Second     : Second_Number;
                     Sub_Second : Second_Duration := 0.0;
                     Leap_Second: Boolean := False;
                     Time_Zone  : Time_Zones.Time_Offset := 0)
                             @key<return> Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Time_Of} (Year       : Year_Number;
                     Month      : Month_Number;
                     Day        : Day_Number;
                     Seconds    : Day_Duration := 0.0;
                     Leap_Second: Boolean := False;
                     Time_Zone  : Time_Zones.Time_Offset := 0)
                             @key<return> Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<procedure> @AdaSubDefn{Split} (Date       : @key<in> Time;
                    Year       : @key<out> Year_Number;
                    Month      : @key<out> Month_Number;
                    Day        : @key<out> Day_Number;
                    Hour       : @key<out> Hour_Number;
                    Minute     : @key<out> Minute_Number;
                    Second     : @key<out> Second_Number;
                    Sub_Second : @key<out> Second_Duration;
                    Time_Zone  : @key<in> Time_Zones.Time_Offset := 0);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<procedure> @AdaSubDefn{Split} (Date       : @key<in> Time;
                    Year       : @key<out> Year_Number;
                    Month      : @key<out> Month_Number;
                    Day        : @key<out> Day_Number;
                    Hour       : @key<out> Hour_Number;
                    Minute     : @key<out> Minute_Number;
                    Second     : @key<out> Second_Number;
                    Sub_Second : @key<out> Second_Duration;
                    Leap_Second: @key<out> Boolean;
                    Time_Zone  : @key<in> Time_Zones.Time_Offset := 0);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<procedure> @AdaSubDefn{Split} (Date       : @key<in> Time;
                    Year       : @key<out> Year_Number;
                    Month      : @key<out> Month_Number;
                    Day        : @key<out> Day_Number;
                    Seconds    : @key<out> Day_Duration;
                    Leap_Second: @key<out> Boolean;
                    Time_Zone  : @key<in> Time_Zones.Time_Offset := 0);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[Simple image and value:]
   @key<function> @AdaSubDefn{Image} (Date : Time;
                   Include_Time_Fraction : Boolean := False;
                   Time_Zone  : Time_Zones.Time_Offset := 0) @key<return> String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Value} (Date : String;
                   Time_Zone  : Time_Zones.Time_Offset := 0) @key<return> Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Image} (Elapsed_Time : Duration;
                   Include_Time_Fraction : Boolean := False) @key<return> String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<function> @AdaSubDefn{Value} (Elapsed_Time : String) @key<return> Duration;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key<end> Ada.Calendar.Formatting;]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgAdded{Version=[2],Text=[Type Time_Offset represents the number of minutes
difference between the implementation-defined time zone used by Calendar
and another time zone.]}

@begin{DescribeCode}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> UTC_Time_Offset (Date : Time := Clock) @key<return> Time_Offset;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0119-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns, as a number of minutes, the
@Chg{Version=[3],New=[result of subtracting],Old=[difference between]}
the implementation-defined time zone of Calendar@Chg{Version=[3],New=[ from],Old=[, and]}
UTC time, at the time Date. If the time zone of the Calendar implementation is
unknown, then Unknown_Zone_Error is raised.]}
@begin{Ramification}
    @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0119-1]}
    @ChgAdded{Version=[3],Text=[In North America, the result will be negative;
    in Europe, the result will be zero or positive.]}
@end{Ramification}
@begin{Discussion}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[The Date parameter is needed to take
    into account time differences caused by daylight-savings time and other
    time changes. This parameter is measured in the time zone of Calendar,
    if any, not necessarily the UTC time zone.]}

    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[Other time zones can be supported with a
    child package. We don't define one because of the lack of agreement
    on the definition of a time zone.]}

    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[The accuracy of this routine is not specified;
    the intent is that the facilities of the underlying target operating system
    are used to implement it.]}
@end{Discussion}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<procedure> Difference (Left, Right : @key<in> Time;
                      Days : @key<out> Day_Count;
                      Seconds : @key<out> Duration;
                      Leap_Seconds : @key<out> Leap_Seconds_Count);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01],ARef=[AI95-00427-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the difference between
Left and Right. Days is the number of days of difference, Seconds is the
remainder seconds of difference excluding leap seconds, and Leap_Seconds is
the number of leap seconds. If Left < Right, then Seconds <= 0.0, Days <= 0,
and Leap_Seconds <= 0. Otherwise, all values are nonnegative.
The absolute value of Seconds is always less than 86_400.0.
For the returned values, if Days =
0, then Seconds + Duration(Leap_Seconds) = Calendar."@en" (Left, Right).]}
@begin{Discussion}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[Leap_Seconds, if any, are not included in
    Seconds. However, Leap_Seconds should be included in calculations
    using the operators defined in Calendar, as is specified for "@en" above.]}
@end{Discussion}


@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> "+" (Left : Time; Right : Day_Count) @key<return> Time;
@key<function> "+" (Left : Day_Count; Right : Time) @key<return> Time;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Adds a number of days to a time value.
Time_Error is raised if the result is not representable as a value of type
Time.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> "-" (Left : Time; Right : Day_Count) @key<return> Time;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Subtracts a number of days from a time value.
Time_Error is raised if the result is not representable as a value of type
Time.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> "-" (Left, Right : Time) @key<return> Day_Count;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Subtracts two time values, and returns the
number of days between them. This is the same value that Difference would
return in Days.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Day_of_Week (Date : Time) @key<return> Day_Name;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the day of the week for Time. This is
based on the Year, Month, and Day values of Time.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Year       (Date : Time;
                     Time_Zone  : Time_Zones.Time_Offset := 0)
                        @key<return> Year_Number;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00427-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the year for Date, as
appropriate for the specified time zone offset.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Month      (Date : Time;
                     Time_Zone  : Time_Zones.Time_Offset := 0)
                        @key<return> Month_Number;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00427-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the month for Date, as
appropriate for the specified time zone offset.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Day        (Date : Time;
                     Time_Zone  : Time_Zones.Time_Offset := 0)
                        @key<return> Day_Number;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00427-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the day number for Date, as
appropriate for the specified time zone offset.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Hour       (Date : Time;
                     Time_Zone  : Time_Zones.Time_Offset := 0)
                        @key<return> Hour_Number;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the hour for Date, as appropriate for
the specified time zone offset.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Minute     (Date : Time;
                     Time_Zone  : Time_Zones.Time_Offset := 0)
                        @key<return> Minute_Number;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the minute within the hour for Date,
as appropriate for the specified time zone offset.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Second     (Date : Time)
                        @key<return> Second_Number;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01],ARef=[AI95-00427-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the second within the hour and minute
for Date.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Sub_Second (Date : Time)
                        @key<return> Second_Duration;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01],ARef=[AI95-00427-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the fraction of second for
Date (this has the same accuracy as Day_Duration). The value returned is always
less than 1.0.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Seconds_Of (Hour   : Hour_Number;
                     Minute : Minute_Number;
                     Second : Second_Number := 0;
                     Sub_Second : Second_Duration := 0.0)
    @key<return> Day_Duration;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01],ARef=[AI95-00427-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a Day_Duration value for
the combination of the given Hour, Minute, Second, and Sub_Second.
This value can be used in Calendar.Time_Of as
well as the argument to Calendar."+" and Calendar."@en". If Seconds_Of is
called with a Sub_Second value of 1.0, the value returned is equal to the value
of Seconds_Of for the next second with a Sub_Second value of 0.0.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<procedure> Split (Seconds    : @key<in> Day_Duration;
                 Hour       : @key<out> Hour_Number;
                 Minute     : @key<out> Minute_Number;
                 Second     : @key<out> Second_Number;
                 Sub_Second : @key<out> Second_Duration);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01],ARef=[AI95-00427-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0238-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Splits Seconds into Hour, Minute,
Second and Sub_Second in such a way that the resulting values all belong to
their respective subtypes. The value returned in the Sub_Second
parameter is always less than 1.0.@Chg{Version=[3],New=[ If Seconds = 86400.0,
Split propagates Time_Error.],Old=[]}]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[There is only one way to do the split which
  meets all of the requirements.]}
@end{Ramification}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0238-1]}
  @ChgAdded{Version=[3],Text=[If Seconds = 86400.0, one of the returned values
  would have to be out of its defined range (either Sub_Second = 1.0 or Hour =
  24 with the other value being 0). This doesn't seem worth breaking the
  invariants.]}
@end{Reason}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Time_Of (Year       : Year_Number;
                  Month      : Month_Number;
                  Day        : Day_Number;
                  Hour       : Hour_Number;
                  Minute     : Minute_Number;
                  Second     : Second_Number;
                  Sub_Second : Second_Duration := 0.0;
                  Leap_Second: Boolean := False;
                  Time_Zone  : Time_Zones.Time_Offset := 0)
                          @key<return> Time;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01],ARef=[AI95-00427-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Leap_Second is False,
returns a Time built from the date and time
values, relative to the specified time zone offset. If Leap_Second is True,
returns the Time that represents the time within the leap second that is one
second later than the time specified by the other parameters.
Time_Error is raised if the parameters do not form a proper date or time.
If Time_Of is called with a Sub_Second value of 1.0, the value
returned is equal to the value of Time_Of for the next second with
a Sub_Second value of 0.0.]}
@begin{Discussion}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[Time_Error should be raised if Leap_Second
    is True, and the date and time values do not represent the second before
    a leap second. A leap second always occurs at midnight UTC,
    and is 23:59:60 UTC in ISO notation. So, if the time zone is UTC and
    Leap_Second is True, if any of Hour /= 23, Minute /= 59, or Second /= 59,
    then Time_Error should be raised.
    However, we do not say that, because other time zones will have different
    values where a leap second is allowed.]}
@end{Discussion}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Time_Of (Year       : Year_Number;
                  Month      : Month_Number;
                  Day        : Day_Number;
                  Seconds    : Day_Duration := 0.0;
                  Leap_Second: Boolean := False;
                  Time_Zone  : Time_Zones.Time_Offset := 0)
                          @key<return> Time;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01],ARef=[AI95-00427-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Leap_Second is False, returns
a Time built from the date and time
values, relative to the specified time zone offset. If Leap_Second is True,
returns the Time that represents the time within the leap second that is one
second later than the time specified by the other parameters.
Time_Error is raised if the parameters do not form a proper date or time.
If Time_Of is called with a Seconds value of 86_400.0, the value
returned is equal to the value of Time_Of for the next day with
a Seconds value of 0.0.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<procedure> Split (Date       : @key<in> Time;
                 Year       : @key<out> Year_Number;
                 Month      : @key<out> Month_Number;
                 Day        : @key<out> Day_Number;
                 Hour       : @key<out> Hour_Number;
                 Minute     : @key<out> Minute_Number;
                 Second     : @key<out> Second_Number;
                 Sub_Second : @key<out> Second_Duration;
                 Leap_Second: @key<out> Boolean;
                 Time_Zone  : @key<in> Time_Zones.Time_Offset := 0);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01],ARef=[AI95-00427-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Date does not represent a time
within a leap second, splits Date into its constituent parts (Year, Month, Day,
Hour, Minute, Second, Sub_Second), relative to the specified time zone offset,
and sets Leap_Second to False. If Date represents a time within a leap second,
set the constituent parts to values corresponding to a time one second earlier
than that given by Date, relative to the specified time zone offset, and sets
Leap_Seconds to True. The value returned in the Sub_Second parameter is always
less than 1.0.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<procedure> Split (Date       : @key<in> Time;
                 Year       : @key<out> Year_Number;
                 Month      : @key<out> Month_Number;
                 Day        : @key<out> Day_Number;
                 Hour       : @key<out> Hour_Number;
                 Minute     : @key<out> Minute_Number;
                 Second     : @key<out> Second_Number;
                 Sub_Second : @key<out> Second_Duration;
                 Time_Zone  : @key<in> Time_Zones.Time_Offset := 0);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01],ARef=[AI95-00427-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Splits Date into its constituent parts (Year,
Month, Day, Hour, Minute, Second, Sub_Second), relative to the specified time
zone offset. The value returned in the Sub_Second parameter is always less
than 1.0.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<procedure> Split (Date       : @key<in> Time;
                 Year       : @key<out> Year_Number;
                 Month      : @key<out> Month_Number;
                 Day        : @key<out> Day_Number;
                 Seconds    : @key<out> Day_Duration;
                 Leap_Second: @key<out> Boolean;
                 Time_Zone  : @key<in> Time_Zones.Time_Offset := 0);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01],ARef=[AI95-00427-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If Date does not represent a time
within a leap second, splits Date into its constituent parts (Year, Month, Day,
Seconds), relative to the specified time zone offset, and sets Leap_Second to
False. If Date represents a time within a leap second, set the constituent
parts to values corresponding to a time one second earlier than that given by
Date, relative to the specified time zone offset, and sets Leap_Seconds to
True. The value returned in the Seconds parameter is always less than 86_400.0.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Image (Date : Time;
                Include_Time_Fraction : Boolean := False;
                Time_Zone  : Time_Zones.Time_Offset := 0) @key<return> String;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a string form of the Date relative to
the given Time_Zone.
The format is "Year-Month-Day Hour:Minute:Second", where the Year is a
4-digit value, and all others are 2-digit values, of the functions
defined in Calendar and Calendar.Formatting, including a leading zero,
if needed. The separators between the values are
a minus, another minus, a colon, and a single space between the Day and Hour.
If Include_Time_Fraction is True, the integer part of Sub_Seconds*100 is
suffixed to the string as a point followed by a 2-digit value.]}
@begin{Discussion}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[The Image provides a string in ISO 8601 format, the
    international standard time format. Alternative representations allowed
    in ISO 8601 are not supported here.]}

    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[ISO 8601 allows 24:00:00 for midnight; and a seconds
    value of 60 for leap seconds. These are not allowed here (the routines
    mentioned above cannot produce those results).]}
@end{Discussion}

@begin{Ramification}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[The fractional part is truncated, not rounded.
    It would be quite hard to define the result with proper rounding, as it can
    change all of the values of the image. Values can be rounded up by adding
    an appropriate constant (0.5 if Include_Time_Fraction is False,
    0.005 otherwise) to the time before taking the image.]}
@end{Ramification}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Value (Date : String;
                Time_Zone  : Time_Zones.Time_Offset := 0) @key<return> Time;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a Time value for the image given as
Date, relative to the given time zone. Constraint_Error is raised if the string
is not formatted as described for Image, or the function cannot interpret the
given string as a Time value.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[The intent is that the implementation
  enforce the same range rules on the string as the appropriate function
  Time_Of, except for the hour, so
  @lquotes@;cannot interpret the given string as a Time value@rquotes
  happens when one of the values is out of the required range.
  For example, "2005-08-31 @Chg{Version=[3],New=[24:00:00],Old=[24:0:0]}"
  should raise Constraint_Error (the hour is out of range).]}
@end{Discussion}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Image (Elapsed_Time : Duration;
                Include_Time_Fraction : Boolean := False) @key<return> String;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a string form of the Elapsed_Time.
The format is "Hour:Minute:Second", where all values are
2-digit values, including a leading zero, if needed.
The separators between the values are colons.
If Include_Time_Fraction is True, the integer part of Sub_Seconds*100 is
suffixed to the string as a point followed by a 2-digit value.
If Elapsed_Time < 0.0, the result is Image (@key<abs> Elapsed_Time,
Include_Time_Fraction) prefixed with a minus sign. If @key<abs> Elapsed_Time
represents 100 hours or more, the result is implementation-defined.]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The result of Calendar.Formating.Image if its argument represents more
than 100 hours.]}]}
@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This cannot be implemented (directly) by calling
    Calendar.Formatting.Split, since it may be out of the range of
    Day_Duration, and thus the number of hours may be out of the range of
    Hour_Number.]}

@ChgAdded{Version=[2],Text=[If a Duration value can represent more then 100 hours,
    the implementation will need to define a format for the return of Image.]}
@end{ImplNote}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key<function> Value (Elapsed_Time : String) @key<return> Duration;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a Duration value for the image given
as Elapsed_Time. Constraint_Error is raised if the string is not formatted as
described for Image, or the function cannot interpret the given string as a
Duration value.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The intent is that the implementation
  enforce the same range rules on the string as the appropriate function
  Time_Of, except for the hour, so
  @lquotes@;cannot interpret the given string as a Time value@rquotes
  happens when one of the values is out of the required range.
  For example, "10:23:60" should raise Constraint_Error (the seconds value
  is out of range).]}
@end{Discussion}


@end{DescribeCode}

@end{StaticSem}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgAdded{Version=[2],Text=[An implementation should support leap seconds if the
target system supports them. If leap seconds are not supported, Difference
should return zero for Leap_Seconds, Split should return False for Leap_Second,
and Time_Of should raise Time_Error if Leap_Second is True.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Leap seconds should be supported if the target system supports them.
Otherwise, operations in Calendar.Formatting should return results
consistent with no leap seconds.]}]}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[An implementation can always support leap seconds
when the target system does not; indeed, this isn't particularly
hard (all that is required is a table of when leap seconds were inserted). As
such, leap second support isn't @lquotes@;impossible or impractical@rquotes
in the sense of @RefSecNum{Conformity of an Implementation with the Standard}.
However, for some purposes, it may be important to follow the target system's
lack of leap second support (if the target is a GPS satellite, which does not
use leap seconds, leap second support would be a handicap to work around).
Thus, this @ImplAdviceTitle should be read as giving permission to not support
leap seconds on target systems that don't support leap seconds. Implementers
should use the needs of their customers to determine whether or not support
leap seconds on such targets.]}
@end{Discussion}
@end{ImplAdvice}

@begin{Notes}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgAdded{Version=[2],Text=[The implementation-defined time zone of package Calendar
may, but need not, be the local time zone. UTC_Time_Offset always returns the
difference relative to the implementation-defined time zone of package
Calendar. If UTC_Time_Offset does not raise Unknown_Zone_Error, UTC time
can be safely calculated (within the accuracy of the underlying time-base).]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
  @ChgAdded{Version=[2],Text=[The time in the time zone known as Greenwich
  Mean Time (GMT) is generally very close to UTC time; for most purposes they
  can be treated the same. GMT is the time based on the rotation of the Earth;
  UTC is the time based on atomic clocks, with leap seconds periodically
  inserted to realign with GMT (because most human activities depend on the
  rotation of the Earth). At any point in time, there will be a sub-second
  difference between GMT and UTC.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01]}
@ChgAdded{Version=[2],Text=[Calling Split on the results of subtracting
Duration(UTC_Time_Offset*60) from Clock provides the components (hours,
minutes, and so on) of the UTC time. In the United States, for example,
UTC_Time_Offset will generally be negative.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is an illustration to help specify the value of
  UTC_Time_Offset. A user should pass UTC_Time_Offset as the Time_Zone
  parameter of Split, rather than trying to make the above calculation.]}
@end{Discussion}
@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00351-01],ARef=[AI95-00428-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Packages Calendar.Time_Zones, Calendar.Arithmetic, and Calendar.Formatting
  are new.]}
@end{Extend95}

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0238-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}@b<Correction:>
  Defined that Split for Seconds raises Time_Error for a value of exactly
  86400.0, rather than breaking some invariant or raising some other exception.
  Ada 2005 left this unspecified; a program that depended on what some
  implementation does might break, but such a program is not portable anyway.]}
@end{Inconsistent2005}

@begin{Diffword2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0119-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified the sign of
  UTC_Time_Offset.]}
@end{Diffword2005}


@LabeledClause{Select Statements}

@begin{Intro}
@redundant[There are four forms of the @nt{select_statement}. One form provides a
selective wait for one or more @nt{select_alternative}s. Two provide
timed and conditional entry calls. The fourth provides asynchronous
transfer of control.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<select_statement>,rhs="
   @Syn2{selective_accept}
  | @Syn2{timed_entry_call}
  | @Syn2{conditional_entry_call}
  | @Syn2{asynchronous_select}"}
@end{Syntax}

@begin{Examples}
@leading@keepnext@i{Example of a select statement:}
@begin{Example}
@key(select)
   @key(accept) Driver_Awake_Signal;
@key(or)
   @key(delay) 30.0*Seconds;
   Stop_The_Train;
@key(end) @key(select);
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
@nt{Asynchronous_select} is new.
@end{Extend83}

@LabeledSubClause{Selective Accept}

@begin{Intro}
@redundant[This form of the @nt{select_statement} allows a combination of waiting for,
and selecting from, one or more alternatives. The
selection may depend on conditions associated with each alternative of the
@nt{selective_accept}.
@IndexSee{Term=[time-out],See=(selective_accept)}]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<selective_accept>,rhs="
  @key{select}
   [@Syn2{guard}]
     @Syn2{select_alternative}
{ @key{or}
   [@Syn2{guard}]
     @Syn2{select_alternative} }
[ @key{else}
   @Syn2{sequence_of_statements} ]
  @key{end select};"}

@Syn{lhs=<guard>,rhs="@key{when} @Syn2{condition} =>"}


@Syn{lhs=<select_alternative>,rhs="
   @Syn2{accept_alternative}
  | @Syn2{delay_alternative}
  | @Syn2{terminate_alternative}"}


@Syn{lhs=<accept_alternative>,rhs="
  @Syn2{accept_statement} [@Syn2{sequence_of_statements}]"}

@Syn{lhs=<delay_alternative>,rhs="
  @Syn2{delay_statement} [@Syn2{sequence_of_statements}]"}

@Syn{lhs=<terminate_alternative>,rhs="@key{terminate};"}

@begin(SyntaxText)
@leading@;A @nt{selective_accept} shall contain at least one @nt{accept_alternative}.
In addition, it can contain:
@begin{itemize}
a @nt{terminate_alternative} (only one); or

one or more @nt{delay_alternative}s; or

@Defn2{Term=[else part], Sec=(of a @nt<selective_accept>)}
an @i(else part) (the reserved word @key(else) followed
by a @nt<sequence_of_statements>).
@end{itemize}

These three possibilities are mutually exclusive.
@end(SyntaxText)
@end{Syntax}

@begin{Legality}

If a @nt{selective_accept} contains more than one @nt{delay_alternative},
then all shall be @nt<delay_@!relative_@!statement>s,
or all shall be @nt<delay_@!until_@!statement>s for the same time type.
@begin{Reason}
  This simplifies the implementation and the description of the semantics.
@end{Reason}

@end{Legality}

@begin{RunTime}

@Defn{open alternative}
A @nt<select_alternative> is said to be @i(open) if
it is not immediately preceded by a @nt<guard>, or if
the @nt<condition> of its @nt<guard> evaluates to True. It
is said to be @i(closed) otherwise.

@PDefn2{Term=[execution], Sec=(selective_accept)}
For the execution of a @nt{selective_accept}, any @nt{guard}
@nt{condition}s are evaluated; open alternatives are
thus determined. For an open @nt{delay_alternative}, the
@i(delay_)@nt<expression> is also evaluated. Similarly, for an open
@nt{accept_alternative} for
an entry of a family, the @nt{entry_index} is also evaluated.
These evaluations are performed in an arbitrary order, except that
a @i(delay_)@nt<expression> or @nt<entry_index> is not evaluated until
after evaluating the corresponding @nt<condition>, if any.
Selection and execution of one open alternative, or of the else part, then
completes the execution of the @nt{selective_accept}; the rules for
this selection are described below.@PDefn2{Term=[arbitrary order],Sec=[allowed]}

Open @nt{accept_alternative}s are first considered. Selection of one such
alternative takes place immediately if the corresponding
entry already has queued calls. If several alternatives
can thus be selected, one of them is selected according to the
entry queuing policy in effect (see @RefSecNum(Entry Calls) and
@RefSecNum(Entry Queuing Policies)).
When such an
alternative is selected, the selected call is
removed from its entry queue and the @nt<handled_sequence_of_@!statements>
(if any) of the corresponding @nt{accept_statement} is executed; after the
rendezvous completes any subsequent @nt<sequence_of_@!statements>
of the alternative is executed.
@PDefn2{Term=[blocked], Sec=(execution of a @nt<selective_accept>)}
If no selection is immediately possible (in the above sense)
and there is no else part, the task
blocks until an open alternative can be selected.

@leading@;Selection of the other forms of alternative or of an else part is performed
as follows:
@begin{itemize}

An open @nt{delay_alternative} is selected when
its expiration time is reached if no @nt{accept_@!alternative}
or other @nt<delay_@!alternative> can be selected prior to the
expiration time. If several
@nt{delay_@!alternative}s have this same expiration time,
one of them is selected according to the queuing policy in
effect (see @RefSecNum{Entry Queuing Policies}); the default queuing
policy chooses arbitrarily among the @nt<delay_@!alternative>s
whose expiration time has passed.

The else part is selected and its @nt<sequence_of_@!statements> is executed
if no @nt{accept_alternative} can immediately be selected;
in particular, if all alternatives are closed.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
An open @nt{terminate_alternative} is selected if the conditions stated at the
end of @Chg{Version=[3],New=[subclause],Old=[clause]}
@RefSecNum{Task Dependence - Termination of Tasks}
are satisfied.
@begin(Ramification)
  In the absence of a @nt<requeue_statement>, the conditions stated
  are such that a @nt<terminate_alternative> cannot be selected while
  there is a queued entry call for any entry of the task.
  In the presence of requeues from a task to one of its subtasks,
  it is possible that when a @nt<terminate_alternative> of the
  subtask is selected, requeued calls (for closed entries only) might still
  be queued on some entry of the subtask. Tasking_Error will
  be propagated to such callers, as is usual when a task completes
  while queued callers remain.
@end(Ramification)

@end{itemize}

@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
The exception Program_Error is raised if all alternatives are closed and
there is no else part.

@end{RunTime}

@begin{Notes}

A @nt{selective_accept} is allowed to have several open
@nt{delay_alternative}s. A @nt{selective_accept} is allowed
to have several open
@nt{accept_alternative}s for the same entry.

@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of a task body with a selective accept:}
@begin{Example}
@key(task) @key(body) Server @key(is)
   Current_Work_Item : Work_Item;
@key(begin)
   @key(loop)
      @key(select)
         @key(accept) Next_Work_Item(WI : @key(in) Work_Item) @key(do)
            Current_Work_Item := WI;
         @key(end);
         Process_Work_Item(Current_Work_Item);
      @key(or)
         @key(accept) Shut_Down;
         @key(exit);       --@RI[ Premature shut down requested]
      @key(or)
         @key(terminate);  --@RI[ Normal shutdown at end of scope]
      @key(end) @key(select);
   @key(end) @key(loop);
@key(end) Server;
@end{Example}
@end{Examples}

@begin{DiffWord83}
The name of @ntf{selective_wait} was changed to @nt{selective_accept} to
better describe what is being waited for.
We kept @nt{select_alternative} as is, because
@ntf<selective_accept_alternative> was too easily confused
with @nt<accept_alternative>.
@end{DiffWord83}


@LabeledSubClause{Timed Entry Calls}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@redundant[A @nt{timed_entry_call} issues an entry call that is
cancelled if the call (or a requeue-with-abort of the call)
is not selected before the expiration time is
reached.@Chg{Version=[2],New=[ A procedure call may appear rather than
an entry call for cases where the procedure might be implemented by
an entry.],Old=[]}
@IndexSee{Term=[time-out],See=(timed_entry_call)}]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<timed_entry_call>,rhs="
  @key{select}
   @Syn2{entry_call_alternative}
  @key{or}
   @Syn2{delay_alternative}
  @key{end select};"}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@Syn{lhs=<entry_call_alternative>,rhs="
  @Chg{Version=[2],New=[@Syn2{procedure_or_entry_call}],Old=[@Syn2{entry_call_statement}]} [@Syn2{sequence_of_statements}]"}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}
@AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=[procedure_or_entry_call],Old=[]}>,rhs="
  @Chg{Version=[2],New=[@Syn2{procedure_call_statement} | @Syn2{entry_call_statement}],Old=[]}"}
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[If a @nt{procedure_call_statement} is used for a
@nt{procedure_or_entry_call}, the @SynI{procedure_}@nt{name} or
@SynI{procedure_}@nt{prefix} of the @nt{procedure_call_statement} shall
statically denote an entry renamed as a procedure or (a view of) a
primitive subprogram of a limited interface whose first parameter is a
controlling parameter (see @RefSecNum{Dispatching Operations of Tagged Types}).]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This would be a confusing way to call a procedure,
so we only allow it when it is possible that the procedure is actually an
entry. We could have allowed formal subprograms here, but we didn't because
we'd have to allow all formal subprograms, and it would increase the
difficulty of generic code sharing.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[We say @lquotes@;statically denotes@rquotes
because an access-to-subprogram cannot be primitive, and we don't have
anything like access-to-entry. So only names of entries or procedures are
possible.]}
@end{Reason}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg],ARef=[AI05-0291-1]}@Comment{
We don't need a message here, as this is the last inserted paragraph}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[If a
@nt{procedure_call_statement} is used for a @nt{procedure_or_entry_call}, and
the procedure is implemented by an entry, then the @SynI{procedure_}@nt{name},
or @SynI{procedure_}@nt{prefix} and possibly the first parameter of the
@nt{procedure_call_statement}, determine the target object of the call and the
entry to be called.]}]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[The above says @lquotes@;possibly the first
    parameter@rquotes@;, because Ada allows entries to be renamed and passed as
    formal subprograms. In those cases, the task or protected object is implicit
    in the name of the routine; otherwise the object is an explicit parameter to
    the call.]}]}
@end{Discussion}
@end{StaticSem}

@begin{RunTime}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@PDefn2{Term=[execution], Sec=(timed_entry_call)}
For the execution of a @nt{timed_entry_call}, the @SynI(entry_)@nt<name>@Chg{Version=[2],
New=[, @Syni{procedure_}@nt{name}, or @Syni{procedure_}@nt{prefix},],Old=[]}
and any actual parameters are evaluated,
as for a simple entry call (see @RefSecNum(Entry Calls))@Chg{Version=[2],New=[
or procedure call (see @RefSecNum{Subprogram Calls})],Old=[]}.
The expiration time
(see @RefSecNum(Delay Statements, Duration, and Time))
for the call is determined by evaluating
the @i(delay_)@nt<expression> of the
@nt<delay_alternative>@Chg{Version=[2],New=[. If the call is an entry call or
a call on a procedure implemented by an entry,],Old=[;]}
the entry call is then issued.@Chg{Version=[2],New=[ Otherwise, the call
proceeds as described in @RefSecNum{Subprogram Calls} for a procedure call,
followed by the @nt{sequence_of_@!statements} of the @nt{entry_call_@!alternative};
the @nt{sequence_of_@!statements} of the @nt{delay_@!alternative} is ignored.],Old=[]}


If the call is queued (including due to a requeue-with-abort),
and not selected before the expiration
time is reached, an attempt to cancel the call is made.
If the call completes due to the cancellation, the optional
@nt<sequence_of_@!statements> of the @nt<delay_@!alternative> is
executed; if the entry call completes normally, the optional
@nt<sequence_of_@!statements> of the @nt<entry_call_@!alternative> is
executed.
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00345-01]}
@ChgDeleted{Version=[2],Text=[The fact that the syntax calls for
an @nt{entry_call_statement} means
that this fact is used in overload resolution.
For example,
if there is a procedure X and an entry X (both with no parameters),
then "select X; ..." is legal,
because overload resolution knows that the entry is the one that was
meant.]}
@end{Ramification}

@end{RunTime}

@begin{Examples}
@leading@keepnext@i{Example of a timed entry call:}
@begin{Example}
@key(select)
   Controller.Request(Medium)(Some_Item);
@key(or)
   @key(delay) 45.0;
   --@RI[  controller too busy, try something else]
@key(end) @key(select);
@end{Example}
@end{Examples}

@begin{DiffWord83}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
This @Chg{Version=[3],New=[subclause],Old=[clause]} comes before the one
for Conditional Entry Calls,
so we can define conditional entry calls in terms of timed entry calls.
@end{DiffWord83}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  A procedure @Chg{Version=[3],New=[call ],Old=[]}can be used as the
  @Chg{Version=[3],New=[@nt{entry_call_alternative} ],Old=[]}in a timed or
  conditional entry call, if the procedure
  might actually be an entry. Since the fact that something is an entry
  could be used in resolving these calls in Ada 95, it is possible for
  timed or conditional entry calls that resolved in Ada 95 to be ambiguous
  in Ada 2005. That could happen if both an entry and procedure with the
  same name and profile exist, which should be rare.]}
@end{Incompatible95}


@LabeledSubClause{Conditional Entry Calls}

@begin{Intro}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@Redundant[A @nt{conditional_entry_call} issues an entry call that is
then cancelled if it is not selected immediately (or if a requeue-with-abort
of the call is not selected immediately).@Chg{Version=[2],New=[ A procedure
call may appear rather than
an entry call for cases where the procedure might be implemented by
an entry.],Old=[]}]
@begin(Honest)
  In the case of an entry call on a protected object, it is OK if the entry
  is closed at the start of the corresponding protected action, so long as
  it opens and the call is selected before the end of that protected
  action (due to changes in the Count attribute).
@end(Honest)

@end{Intro}

@begin{Syntax}
@Syn{lhs=<conditional_entry_call>,rhs="
  @key{select}
   @Syn2{entry_call_alternative}
  @key{else}
   @Syn2{sequence_of_statements}
  @key{end select};"}
@end{Syntax}

@begin{RunTime}

@PDefn2{Term=[execution], Sec=(conditional_entry_call)}
The execution of a @nt<conditional_entry_call> is defined to be equivalent
to the execution of a @nt<timed_@!entry_@!call> with a @nt<delay_@!alternative>
specifying an immediate expiration time and the
same @nt<sequence_of_@!statements> as given after the reserved word @key(else).
@end{RunTime}

@begin{Notes}

A @nt{conditional_entry_call} may briefly increase the Count attribute of
the entry, even if the conditional call is not selected.

@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of a conditional entry call:}
@begin{Example}
@key(procedure) Spin(R : @key[in] Resource) @key(is)
@key(begin)
   @key(loop)
      @key(select)
         R.Seize;
         @key(return);
      @key(else)
         @key(null);  --@RI[  busy waiting]
      @key(end) @key(select);
   @key(end) @key(loop);
@key(end);
@end{Example}
@end{Examples}

@begin{DiffWord83}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
This @Chg{Version=[3],New=[subclause],Old=[clause]} comes after the
one for Timed Entry Calls, so we can define conditional entry calls in terms of
timed entry calls.
We do that so that an "expiration time" is defined for both,
thereby simplifying the definition of what happens on
a requeue-with-abort.
@end{DiffWord83}


@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
@LabeledSubClause{Asynchronous Transfer of Control}

@begin{Intro}
@redundant[An asynchronous @nt{select_statement} provides
asynchronous transfer of control
upon completion of an entry call or the expiration of a delay.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<asynchronous_select>,rhs="
  @key{select}
   @Syn2{triggering_alternative}
  @key{then abort}
   @Syn2{abortable_part}
  @key{end select};"}

@Syn{lhs=<triggering_alternative>,rhs="@Syn2{triggering_statement} [@Syn2{sequence_of_statements}]"}


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@Syn{lhs=<triggering_statement>,rhs="@Chg{Version=[2],New=[@Syn2{procedure_or_entry_call}],Old=[@Syn2{entry_call_statement}]} | @Syn2{delay_statement}"}

@Syn{lhs=<abortable_part>,rhs="@Syn2{sequence_of_statements}"}
@end{Syntax}

@begin{RunTime}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@PDefn2{Term=[execution],
  Sec=(asynchronous_select with an entry call trigger)}
@Chg{Version=[2],New=[@PDefn2{Term=[execution],
  Sec=(asynchronous_select with a procedure call trigger)}],Old=[]}
For the execution of an @nt{asynchronous_select}
whose @nt<triggering_@!statement> is @Chg{Version=[2],
New=[a @nt<procedure_or_entry_call>],Old=[an @nt<entry_call_statement>]},
the @Syni(entry_)@nt<name>@Chg{Version=[2],New=[, @Syni{procedure_}@nt{name},
or @Syni{procedure_}@nt{prefix},],Old=[]} and actual parameters are evaluated
as for a simple entry call (see @RefSecNum(Entry Calls))@Chg{Version=[2],New=[
or procedure call (see @RefSecNum{Subprogram Calls}).
If the call is an entry call or a call on a procedure implemented by an
entry,],Old=[, and]} the entry call is issued.
If the entry call is queued (or requeued-with-abort),
then the @nt<abortable_part> is executed.
@Redundant[If the entry call is selected immediately,
and never requeued-with-abort,
then the @nt<abortable_part> is never started.]@Chg{Version=[2],New=[ If the
call is on a procedure that is not implemented by an entry, the call proceeds
as described in @RefSecNum{Subprogram Calls}, followed by the
@nt{sequence_of_@!statements} of the @nt{triggering_@!alternative}@Redundant[;
the @nt{abortable_part} is never started].],Old=[]}


@PDefn2{Term=[execution],
  Sec=(asynchronous_select with a delay_statement trigger)}
For the execution of an @nt<asynchronous_select> whose
@nt<triggering_@!statement> is a @nt<delay_statement>,
the @i(delay_)@nt<expression> is evaluated
and the expiration time is determined,
as for a normal @nt<delay_statement>.
If the expiration time has not already passed, the @nt<abortable_part>
is executed.

If the @nt<abortable_part> completes and is left prior to completion of the
@nt<triggering_@!statement>,
an attempt to cancel the @nt<triggering_@!statement> is made.
If the attempt to cancel succeeds (see @RefSecNum(Entry Calls) and
@RefSecNum(Delay Statements, Duration, and Time)), the
@nt<asynchronous_select> is complete.

If the @nt<triggering_@!statement> completes other than
due to cancellation,
the @nt<abortable_part>
is aborted (if started but not yet completed @em
see @RefSecNum(Abort of a Task - Abort of a Sequence of Statements)).
If the @nt<triggering_@!statement> completes normally, the optional
@nt<sequence_of_@!statements> of the @nt<triggering_@!alternative> is
executed after the @nt<abortable_part> is left.
@begin(Discussion)
  We currently don't specify when the by-copy [@key(in)] @key(out)
  parameters are assigned back into the actuals. We considered
  requiring that to happen after the @nt<abortable_part> is
  left. However, that doesn't seem useful enough
  to justify possibly overspecifying the implementation approach,
  since some of the parameters are passed by reference anyway.

  In an earlier description, we required that the @nt<sequence_of_@!statements>
  of the @nt<triggering_@!alternative> execute after aborting
  the @nt<abortable_part>, but before waiting for it to complete
  and finalize, to provide more rapid response to the triggering event
  in case the finalization was unbounded. However, various reviewers felt
  that this created unnecessary complexity in the description,
  and a potential for undesirable concurrency (and nondeterminism)
  within a single task. We have now reverted to simpler, more
  deterministic semantics,
  but anticipate that further discussion of this issue might be
  appropriate during subsequent reviews.
  One possibility is to leave this area implementation defined,
  so as to encourage experimentation. The user would then have
  to assume the worst about what kinds of actions are appropriate
  for the @nt<sequence_of_@!statements> of the @nt<triggering_@!alternative>
  to achieve portability.
@end(Discussion)

@end{RunTime}

@begin{Examples}
@leading@keepnext@Defn2{Term=[signal handling], Sec=(example)}
@Defn2{Term=[interrupt],Sec=(example using @nt<asynchronous_select>)}
@Defn2{Term=[terminal interrupt], Sec=(example)}
@i(Example of a main command loop for a command interpreter:)
@begin(Example)
@key(loop)
   @key(select)
      Terminal.Wait_For_Interrupt;
      Put_Line("Interrupted");
   @key(then abort)
      -- @RI(This will be abandoned upon terminal interrupt)
      Put_Line("-> ");
      Get_Line(Command, Last);
      Process_Command(Command(1..Last));
   @key(end) @key(select);
@key(end) @key(loop);
@end(Example)

@begin{WideAbove}
@leading@keepnext@i(Example of a time-limited calculation:)
@IndexSee{Term=[time-out],See=(asynchronous_select)}
@Defn2{Term=[time-out],Sec=(example)}
@Defn2{Term=[time limit],Sec=(example)}
@Defn2{Term=[interrupt],Sec=(example using @nt<asynchronous_select>)}
@Defn2{Term=[timer interrupt],Sec=(example)}
@end{WideAbove}
@begin(Example)
@key(select)
   @key(delay) 5.0;
   Put_Line("Calculation does not converge");
@key(then abort)
   -- @RI(This calculation should finish in 5.0 seconds;)
   -- @RI( if not, it is assumed to diverge.)
   Horribly_Complicated_Recursive_Function(X, Y);
@key(end) @key(select);
@end(Example)

@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0098-1]}
@ChgAdded{Version=[4],Text=[Note that these examples presume that there are
abort completion points within the execution of the @nt{abortable_part}.]}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
@nt<Asynchronous_select> is new.
@end{Extend83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  A procedure can be used as the
  @nt{triggering_@!statement} of an @nt<asynchronous_select>, if the procedure
  might actually be an entry.]}
@end{Extend95}


@LabeledClause{Abort of a Task - Abort of a Sequence of Statements}

@begin{Intro}
@redundant[An @nt{abort_statement} causes one or more tasks to become abnormal, thus
preventing any further interaction with such tasks. The completion
of the @nt<triggering_@!statement> of an @nt<asynchronous_select>
causes a @nt{sequence_of_@!statements} to be aborted.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<abort_statement>,
  rhs="@key{abort} @SynI{task_}@Syn2{name} {, @SynI{task_}@Syn2{name}};"}
@end{Syntax}

@begin{Resolution}

@PDefn2{Term=[expected type], Sec=(abort_statement task_name)}
Each @SynI{task_}@nt{name} is expected to be of any task
type@Redundant[; they need not all be of the same task type.]

@end{Resolution}

@begin{RunTime}

@PDefn2{Term=[execution], Sec=(abort_statement)}
For the execution of an @nt<abort_statement>, the given @i(task_)@nt<name>s
are evaluated in an arbitrary order.@PDefn2{Term=[arbitrary order],Sec=[allowed]}
@Defn2{Term=[abort], Sec=(of a task)}
@Defn{abnormal task}
@PDefn2{Term=[task state], Sec=(abnormal)}
Each named task is
then @i(aborted), which consists of making the task @i(abnormal)
and aborting the execution of the corresponding @nt<task_body>,
unless it is already completed.
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
Note that aborting those tasks is not defined to be an
abort-deferred operation.
Therefore, if one of the named tasks is the task executing the
@nt{abort_statement}, or if the task executing the
@nt{abort_statement} depends on one of the named tasks,
then it is possible for the execution of the @nt{abort_statement} to be
aborted, thus leaving some of the tasks unaborted.
This allows the implementation to use either a sequence of calls to an
@lquotes@;abort task@rquotes@; @Chg{Version=[2],New=[run-time
system],Old=[RTS]} primitive, or a single call to an @lquotes@;abort list of
tasks@rquotes@; @Chg{Version=[2],New=[run-time system],Old=[RTS]} primitive.
@end{Ramification}

@leading@PDefn2{Term=[execution], Sec=(aborting the execution of a construct)}
@Defn2{Term=[abort], Sec=(of the execution of a construct)}
When the execution of a construct
is @i(aborted) (including that of a @nt<task_@!body> or of a
@nt<sequence_of_@!statements>), the execution of every construct
included within the aborted execution is also aborted,
except for executions included within the execution of an @i(abort-deferred)
operation; the execution of an abort-deferred operation
continues to completion without being affected by the abort;
@Defn{abort-deferred operation}
the following are the abort-deferred operations:
@begin(Itemize)
  a protected action;

  waiting for an entry call to complete (after having
  initiated the attempt to cancel it @em see below);

  waiting for the termination of dependent tasks;

  the execution of an Initialize procedure as the last step
  of the default initialization of a controlled object;

  the execution of a Finalize procedure as part of the
  finalization of a controlled object;

  an assignment operation to an object with a controlled part.
@end(Itemize)

@Redundant[The last three of these are discussed further in
@RefSecNum(Assignment and Finalization).]
@begin{Reason}
  Deferring abort during Initialize and finalization allows,
  for example, the result of an allocator performed in
  an Initialize operation to be assigned into an access object without
  being interrupted in the middle, which would cause storage leaks.
  For an object with several controlled parts,
  each individual Initialize is abort-deferred.
  Note that there is generally no semantic difference between
  making each Finalize
  abort-deferred, versus making a group of them abort-deferred,
  because if the task gets aborted, the first thing it will do is
  complete any remaining finalizations.
  Individual objects are finalized prior to an assignment operation
  (if nonlimited controlled) and as part of Unchecked_Deallocation.
@end{Reason}
@begin(Ramification)
Abort is deferred during the entire assignment operation
to an object with a controlled part,
even if only some subcomponents are controlled.
Note that this says "assignment operation,"
not "@nt{assignment_statement}."
Explicit calls to Initialize, Finalize, or Adjust are
not abort-deferred.
@end(Ramification)


When a master is aborted, all tasks
that depend on that master are aborted.

@PDefn{unspecified}
The order in which tasks become abnormal as the result
of an @nt<abort_statement> or the abort of a @nt<sequence_of_@!statements>
is not specified by the language.

@leading@;If the execution of an entry call is aborted,
an immediate attempt is made to cancel the entry call
(see @RefSecNum(Entry Calls)).
If the execution of a construct
is aborted at a time when the execution is blocked,
other than for an entry call, at a point that is outside
the execution of an abort-deferred operation,
then the execution of the construct completes immediately.
For an abort due to an @nt<abort_statement>,
these immediate effects occur before the execution of
the @nt<abort_statement> completes.
Other than for these immediate cases, the execution
of a construct that is aborted does not necessarily
complete before the @nt<abort_statement> completes.
However, the execution of the aborted construct
completes no later than its next @i(abort completion point) (if any)
that occurs outside of an abort-deferred operation;
@Defn{abort completion point}
the following are abort completion points for an execution:
@begin(Itemize)
  the point where the execution initiates the activation of another task;

  the end of the activation of a task;

  the start or end of the execution of an entry call,
  @nt<accept_statement>, @nt<delay_statement>, or @nt<abort_statement>;
  @begin(Ramification)
    Although the abort completion point doesn't occur until the end
    of the entry call or @nt<delay_statement>, these operations might
    be cut short because an abort attempts to cancel them.
  @end(Ramification)

  the start of the execution of a @nt<select_statement>,
  or of the @nt<sequence_of_@!statements> of an @nt<exception_handler>.
  @begin(Reason)
    The start of an @nt<exception_handler> is considered an abort completion
    point simply because it is easy for an implementation to check
    at such points.
  @end(Reason)
  @begin(ImplNote)
    Implementations may of course check for abort more often than at
    each abort completion point; ideally, a fully preemptive
    implementation of abort will be provided.
    If preemptive abort is not supported in a given environment,
    then supporting the checking for abort
    as part of subprogram calls and loop iterations might be a useful option.
  @end(ImplNote)
@end(Itemize)

@end{RunTime}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@PDefn2{Term=(bounded error),Sec=(cause)}
An attempt to execute an @nt<asynchronous_select> as
part of the execution of an abort-deferred operation is a bounded error.
Similarly, an attempt to create a task that depends on a master
that is included entirely within the execution of
an abort-deferred operation is a bounded error.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
In both cases, Program_Error is raised if the error is detected
by the implementation; otherwise@Chg{Version=[3],New=[,],Old=[]} the
operations proceed as they would outside an abort-deferred operation, except
that an abort of the @nt<abortable_part>
or the created task might or might not have an effect.
@begin(Reason)
  An @nt<asynchronous_select> relies on an abort of the
  @nt<abortable_part> to effect the
  asynchronous transfer of control. For an @nt<asynchronous_select>
  within an abort-deferred operation, the abort might
  have no effect.

  Creating a task dependent on a master included within an abort-deferred
  operation is considered an error, because such tasks could be aborted while
  the abort-deferred operation was still progressing, undermining the
  purpose of abort-deferral. Alternatively, we could say that such
  tasks are abort-deferred for their entire execution, but that seems
  too easy to abuse. Note that task creation is already a bounded error
  in protected actions, so this additional rule only applies to local task
  creation as part of Initialize, Finalize, or Adjust.
@end(Reason)
@end{Bounded}

@begin{Erron}
@PDefn{normal state of an object}
@PDefn{abnormal state of an object}
@Defn{disruption of an assignment}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
If an assignment operation completes prematurely due to an abort,
the assignment is said to be @i{disrupted};
the target of the assignment or its parts can become abnormal,
and certain subsequent uses of the object can be erroneous,
as explained in @RefSecNum{Data Validity}.
@end{Erron}

@begin{Notes}

An @nt{abort_statement} should be used only in situations
requiring unconditional termination.

A task is allowed to abort any task it can name, including itself.

Additional requirements associated with abort
are given in @RefSec(Preemptive Abort).
@end{Notes}

@begin{DiffWord83}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
This @Chg{Version=[3],New=[subclause],Old=[clause]} has been rewritten to
accommodate the concept
of aborting the execution of a construct, rather than just of a task.
@end{DiffWord83}


@LabeledClause{Task and Entry Attributes}

@begin{RunTime}

@leading@;
For @PrefixType{a @nt<prefix> T that
is of a task type @Redundant[(after
any implicit dereference)]},
the following attributes are defined:
@begin{Description}
@Comment{@ChgAttribute{Version=[2], Kind=[Revised], ChginAnnex=[F], Leading=[F],
  Prefix=<T>, AttrName=<Callable>, ARef=[AI95-00345],
  Text=<Yields the value True when the task denoted by T
                is @i(callable), and False otherwise;>}
                @PDefn2{Term=[task state], Sec=(callable)}
                @Defn{callable}
                a task is callable unless it is completed or abnormal.
                The value of this attribute is of the predefined
                type Boolean.}
@Attribute{Prefix=<T>, AttrName=<Callable>,
  Text=<Yields the value True when the task denoted by T
                is @i(callable), and False otherwise;>}
                @PDefn2{Term=[task state], Sec=(callable)}
                @Defn{callable}
                a task is callable unless it is completed or abnormal.
                The value of this attribute is of the predefined
                type Boolean.

@Attribute{Prefix=<T>, AttrName=<Terminated>,
  Text=<Yields the value True if the task denoted by T is
                terminated, and False otherwise. The value of this
                attribute is of the predefined type Boolean.>}
@end{Description}
@EndPrefixType{}

For @PrefixType{a @nt<prefix> E that denotes an entry
of a task or protected unit},
the following attribute is defined.
This attribute is only allowed within the body of the task or protected
unit, but excluding, in the case of an entry of a task unit, within any
program unit that is, itself, inner to the body of the task unit.
@begin{Description}

@Attribute{Prefix=<E>, AttrName=<Count>,
  Text=<Yields the number of calls presently queued on the
                entry E of the current instance of the unit.
                The value of this attribute is of the type
                @i{universal_integer}.>}

@end{Description}
@EndPrefixType{}
@end{RunTime}

@begin{Notes}

For the Count attribute, the entry can be either a single entry or an
entry of a family. The name of the entry or entry
family can be either a @nt<direct_name> or an expanded name.

Within task units, algorithms interrogating the attribute E'Count should
take precautions to allow for the increase of the value of this attribute
for incoming entry calls, and its decrease, for example with
@nt{timed_entry_call}s. Also, a @nt{conditional_entry_call} may briefly
increase this value, even if the conditional call is not accepted.

Within protected units, algorithms interrogating the attribute E'Count
in the @nt<entry_barrier> for the entry E should take precautions to
allow for the evaluation of the @nt<condition> of the barrier both before
and after queuing a given caller.
@end{Notes}



@LabeledClause{Shared Variables}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0009-1],ARef=[AI05-0201-1],ARef=[AI05-0229-1],ARef=[AI05-0295-1]}
@Defn2{Term=[shared variable], Sec=(protection of)}
@Defn{independently addressable}
If two different objects, including nonoverlapping
parts of the same object, are @i{independently addressable},
they can be manipulated concurrently by two different tasks
without synchronization.
@Chg{Version=[3],New=[Any two nonoverlapping objects are independently
addressable if either object is specified as independently addressable (see
@RefSecNum{Shared Variable Control}). Otherwise,
two nonoverlapping objects are independently addressable
except when they are both parts of a composite object for which
a nonconfirming value is specified for any of the following representation
aspects: (record) Layout, Component_Size, Pack, Atomic, or Convention;
in this case it is unspecified whether the parts are independently
addressable.@PDefn{unspecified}],
Old=[Normally, any two
nonoverlapping objects are independently addressable.
However, if packing, record layout, or Component_Size
is specified for a given composite object,
then it is implementation defined whether or not
two nonoverlapping parts of that composite object
are independently addressable.]}
@ChgImplDef{Version=[3],Kind=[Deleted],InitialVersion=[0],
Text=[@Chg{Version=[3],New=[],Old=[Whether or not two
nonoverlapping parts of a composite
object are independently addressable,
in the case where packing, record layout, or Component_Size
is specified for the object.]}]}
@begin{ImplNote}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
Independent addressability is the only high level semantic effect of
@Chg{Version=[3],New=[aspect],Old=[a @nt{pragma}]} Pack.
If two objects are independently addressable,
the implementation should allocate them in such a way
that each can be written by the hardware without writing the other.
For example, unless the user asks for it,
it is generally not feasible to choose a bit-packed
representation on a machine without an atomic bit field
insertion instruction,
because there might be tasks that update neighboring subcomponents
concurrently,
and locking operations on all subcomponents is generally not a good
idea.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
Even if @Chg{Version=[3],New=[Pack],Old=[packing]} or one of the other
above-mentioned aspects is specified, subcomponents should still be updated
independently if the hardware efficiently supports it.
@end{ImplNote}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0009-1],ARef=[AI05-0201-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0001-1]}
@ChgAdded{Version=[3],Text=[An atomic object (including atomic
components) is always independently addressable from any other nonoverlapping
object. @Chg{Version=[4],New=[@nt{Aspect_specification}s and representation
items cannot change that fact],Old=[Any @nt{aspect_specification} or
representation item which would prevent this from being true should be rejected,
notwithstanding what this Standard says elsewhere]}. Note, however, that the
components of an atomic object are not necessarily atomic.]}
@end{Ramification}
@end{StaticSem}

@begin{RunTime}
@leading@redundant[Separate tasks normally proceed independently and concurrently
with one another. However, task interactions can be used
to synchronize the actions of two or more tasks to allow,
for example, meaningful communication by the direct updating and
reading of variables shared between the tasks.]
The actions of two different tasks are synchronized in this
sense when an
action of one task @i(signals) an action of the other task;
@Defn2{Term=[signal], Sec=(as defined between actions)}
an action A1 is defined to signal an action A2 under the following
circumstances:
@begin(Itemize)
  If A1 and A2 are part of the execution of the same task,
  and the language rules require A1 to be performed before A2;

  If A1 is the action of an activator that initiates the
  activation of a task, and
  A2 is part of the execution of the task that is activated;

  If A1 is part of the activation of a task, and A2
  is the action of
  waiting for completion of the activation;

  If A1 is part of the execution of a task, and A2 is
  the action of waiting for the termination of the task;

  @ChgRef{Version=[1],Kind=[Added],Ref=[8652/0031],ARef=[AI95-00118-01]}
  @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0072-1]}
  @ChgAdded{Version=[1],Text=[If A1 is the termination of a task T, and A2 is
  either @Chg{Version=[3],New=[an],Old=[the]} evaluation of the expression
  T'Terminated@Chg{Version=[3],New=[ that results in True,],Old=[]} or
  a call to Ada.Task_Identification.Is_Terminated with an actual parameter that
  identifies T @Chg{Version=[3],New=[and a result of True ],Old=[]}(see
  @RefSecNum(The Package Task_Identification));]}

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
  If A1 is the action of issuing an entry call, and A2 is
  part of the corresponding execution of the appropriate
  @nt<entry_body> or @nt<accept_statement>@Chg{Version=[3],New=[;],Old=[.]}
  @begin(Ramification)
    Evaluating the @nt<entry_index> of an @nt<accept_statement>
    is not synchronized with a corresponding entry call,
    nor is evaluating the entry barrier of an @nt<entry_body>.
  @end(Ramification)

  If A1 is part of the execution of an @nt<accept_statement> or
  @nt<entry_body>, and A2 is the action of returning
  from the corresponding entry call;

  If A1 is part of the execution of a protected procedure body
  or @nt<entry_body> for a given protected object, and A2 is part of
  a later execution of an @nt<entry_body> for the same
  protected object;
  @begin(Reason)
    The underlying principle here is that
    for one action to @lquotes@;signal@rquotes@; a second, the second action has to follow
    a potentially blocking operation, whose blocking is dependent on
    the first action in some way.
    Protected procedures are not potentially blocking, so they can
    only be "signalers," they cannot be signaled.
  @end(Reason)
  @begin(Ramification)
    Protected subprogram calls are not defined to signal one another,
    which means that such calls alone cannot be used to synchronize
    access to shared data outside of a protected object.
  @end(Ramification)
  @begin(Reason)
    The point of this distinction is so that on multiprocessors with
    inconsistent caches, the caches only need to be refreshed at
    the beginning of an entry body, and forced out at the end of an
    entry body or protected procedure that leaves an entry open.
    Protected function calls, and protected subprogram calls for
    entryless protected objects do not require full cache consistency.
    Entryless protected objects are intended to be treated roughly like
    atomic objects @em each operation is indivisible with respect to
    other operations (unless both are reads), but such operations cannot
    be used to synchronize access to other nonvolatile
    shared variables.
  @end(Reason)

  @Leading@Comment{This "Leading" is to help fit the next example on one page.}
  If A1 signals some action that in turn signals A2.
@end(Itemize)

@end{RunTime}

@begin{Erron}
@Leading@;@PDefn2{Term=(erroneous execution),Sec=(cause)}
Given an action of assigning to an object,
and an action of reading or updating a part of the same object
(or of a neighboring object if the two are not
independently addressable), then the execution of the actions is erroneous
unless the actions are @i(sequential).
@Defn2{Term=[sequential], Sec=(actions)}
Two actions are sequential if one of the following is true:
@begin(Itemize)
  One action signals the other;

  Both actions occur as part of the execution of the same task;
  @begin{Reason}
    Any two actions of the same task are sequential, even
    if one does not signal the other because they can be
    executed in an @lquotes@;arbitrary@rquotes@;
    (but necessarily equivalent to some @lquotes@;sequential@rquotes@;) order.
  @end{Reason}

  Both actions occur as part
  of protected actions on the same protected object, and
  at most one of the actions is part of a call on a protected function
  of the protected object.
  @begin(Reason)
    Because actions within protected actions do not always imply
    signaling, we have to mention them here explicitly to make sure
    that actions occurring within different protected actions of the
    same protected object are sequential with respect to one another
    (unless both are part of calls on protected functions).
  @end(Reason)
  @begin(Ramification)
    It doesn't matter whether or not the variable being assigned is
    actually a subcomponent of the protected object; globals can be
    safely updated from within the bodies of protected procedures or entries.
  @end(Ramification)
@end(Itemize)

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[Aspect],Old=[A @nt{pragma}]}
Atomic or @Chg{Version=[3],New=[aspect ],Old=[]}Atomic_Components may also
be @Chg{Version=[3],New=[specified],Old=[used]} to
ensure that certain reads and updates are sequential @em
see @RefSecNum(Shared Variable Control).
@begin(Ramification)
  If two actions are @lquotes@;sequential@rquotes@; it is known that their executions
  don't overlap in time, but it is not necessarily specified which occurs first.
  For example, all actions of a single task are sequential, even though
  the exact order of execution is not fully specified for all constructs.
@end(Ramification)
@begin(Discussion)
  Note that if two assignments to the same variable are sequential,
  but neither signals the other, then the program is not erroneous,
  but it is not specified which assignment ultimately prevails.
  Such a situation usually corresponds to a programming mistake, but
  in some (rare) cases, the order makes no difference, and for this
  reason this situation is not considered erroneous nor even a bounded error.
  In Ada 83, this was considered an @lquotes@;incorrect order dependence@rquotes@; if
  the @lquotes@;effect@rquotes@; of the program was affected, but @lquotes@;effect@rquotes@; was never
  fully defined. In Ada 95, this situation represents a potential
  nonportability, and a friendly compiler might want to warn the
  programmer about the situation, but it is not considered an error.
  An example where this would come up would be in gathering statistics
  as part of referencing some information, where the assignments
  associated with
  statistics gathering don't need to be ordered since they are
  just accumulating aggregate counts, sums, products, etc.
@end(Discussion)
@end{Erron}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0031],ARef=[AI95-00118-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that a task T2 can rely on
  values of variables that are updated by another task T1, if task T2 first
  verifies that T1'Terminated is True.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0009-1],ARef=[AI05-0201-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Revised the definition of
  independent addressability to exclude conforming representation clauses
  and to require that atomic and independent objects always have
  independent addressability. This should not change behavior that the
  user sees for any Ada program, so it is not an inconsistency.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0072-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected the wording of
  AI95-00118-01 to actually say what was intended (as described above).]}
@end{DiffWord2005}


@LabeledClause{Example of Tasking and Synchronization}

@begin{Examples}

@Leading@;The following example defines a buffer protected object
to smooth variations between
the speed of output of a producing task and the speed of input of some
consuming task. For instance, the producing task might have the
following structure:

@begin(Example)
@key(task) Producer;

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@key(task body) Producer @key(is)
   @Chg{Version=[2],New=[Person : Person_Name; --@RI[ see @RefSecNum{Incomplete Type Declarations}]],Old=[Char : Character;]}
@key(begin)
   @key(loop)
      ... --@RI[  @Chg{Version=[2],New=[simulate arrival of the next customer],Old=[produce the next character Char]}]
      Buffer.@Chg{Version=[2],New=[Append_Wait(Person)],Old=[Write(Char)]};
      @key(exit) @key(when) @Chg{Version=[2],New=[Person = @key(null)],Old=[Char = ASCII.EOT]};
   @key(end) @key(loop);
@key(end) Producer;
@end(Example)

@leading@keepnext@;and the consuming task might have the following structure:

@begin(Example)
@key(task) Consumer;

@Trailing@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}@key(task body) Consumer @key(is)
   @Chg{Version=[2],New=[Person : Person_Name;],Old=[Char : Character;]}
@key(begin)
   @key(loop)
      Buffer.@Chg{Version=[2],New=[Remove_First_Wait(Person)],Old=[Read(Char)]};
      @key(exit) @key(when) @Chg{Version=[2],New=[Person = @key(null)],Old=[Char = ASCII.EOT]};
      ... --@RI[  @Chg{Version=[2],New=[simulate serving a customer],Old=[consume the character Char]}]
   @key(end) @key(loop);
@key(end) Consumer;
@end(Example)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
The buffer object contains an internal @Chg{Version=[2],New=[array],Old=[pool]}
of @Chg{Version=[2],New=[person names],Old=[characters]} managed in a
round-robin fashion. The @Chg{Version=[2],New=[array],Old=[pool]} has two
indices, an In_Index denoting the @Chg{Version=[2],New=[index],Old=[space]}
for the next input @Chg{Version=[2],New=[person name],Old=[character]} and an
Out_Index denoting the @Chg{Version=[2],New=[index],Old=[space]} for the next
output @Chg{Version=[2],New=[person name],Old=[character]}.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Text=[The Buffer is defined as an extension of the
Synchronized_Queue interface (see @RefSecNum{Interface Types}), and as such
promises to implement the abstraction defined by that interface. By doing so,
the Buffer can be passed to the Transfer class-wide operation defined for
objects of a type covered by Queue'Class.]}

@begin(Example)
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@key(protected) Buffer @key(is)@Chg{Version=[2],New=[ @key(new) Synchronized_Queue @key(with)  --@RI[ see @RefSecNum{Interface Types}]],Old=[]}
   @key(entry) @Chg{Version=[2],New=[Append_Wait(Person : @key(in) Person_Name);],Old=[Read (C : @key(out) Character);]}
   @key(entry) @Chg{Version=[2],New=[Remove_First_Wait(Person : @key(out) Person_Name);
   @key(function) Cur_Count @key(return) Natural;
   @key(function) Max_Count @key(return) Natural;
   @key(procedure) Append(Person : @key(in) Person_Name);
   @key(procedure) Remove_First(Person : @key(out) Person_Name);],Old=[Write(C : @key(in)  Character);]}
@key(private)
   Pool      : @Chg{Version=[2],New=[Person_Name_Array],Old=[String]}(1 .. 100);
   Count     : Natural := 0;
   In_Index, Out_Index : Positive := 1;
@key(end) Buffer;

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@key(protected body) Buffer @key(is)
   @key(entry) @Chg{Version=[2],New=[Append_Wait(Person : @key(in) Person_Name)],Old=[Write(C : @key(in) Character)]}
      @key(when) Count < Pool'Length @key(is)
   @key(begin)
      @Chg{Version=[2],New=[Append(Person);],Old=[Pool(In_Index) := C;
      In_Index := (In_Index @key(mod) Pool'Length) + 1;
      Count    := Count + 1;]}
   @key(end) @Chg{Version=[2],New=[Append_Wait],Old=[Write]};

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Text=[   @key(procedure) Append(Person : @key(in) Person_Name) @key(is)
   @key(begin)
      @key(if) Count = Pool'Length @key(then)
         @key(raise) Queue_Error @key(with) "Buffer Full";  --@RI[ see @RefSecNum{Raise Statements and Raise Expressions}]
      @key(end if);
      Pool(In_Index) := Person;
      In_Index       := (In_Index @key(mod) Pool'Length) + 1;
      Count          := Count + 1;
   @key(end) Append;]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
   @key(entry) @Chg{Version=[2],New=[Remove_First_Wait(Person : @key(out) Person_Name)],Old=[Read(C : @key(out) Character)]}
      @key(when) Count > 0 @key(is)
   @key(begin)
      @Chg{Version=[2],New=[Remove_First(Person);],Old=[C := Pool(Out_Index);
      Out_Index := (Out_Index @key(mod) Pool'Length) + 1;
      Count     := Count - 1;]}
   @key(end) @Chg{Version=[2],New=[Remove_First_Wait],Old=[Read;
@key(end) Buffer]};

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Text=[   @key(procedure) Remove_First(Person : @key(out) Person_Name) @key(is)
   @key(begin)
      @key(if) Count = 0 @key(then)
         @key(raise) Queue_Error @key(with) "Buffer Empty"; --@RI[ see @RefSecNum{Raise Statements and Raise Expressions}]
      @key(end if);
      Person    := Pool(Out_Index);
      Out_Index := (Out_Index @key(mod) Pool'Length) + 1;
      Count     := Count - 1;
   @key(end) Remove_First;]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Text=[   @key(function) Cur_Count @key(return) Natural @key(is)
   @key(begin)
       @key(return) Buffer.Count;
   @key(end) Cur_Count;]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Text=[   @key(function) Max_Count @key(return) Natural @key(is)
   @key(begin)
       @key(return) Pool'Length;
   @key(end) Max_Count;
@key(end) Buffer;]}
@end(Example)

@end{Examples}

