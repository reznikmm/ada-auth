@Part(09, Root="ada.mss")

@SetPageHeadings{$Date: 2000/04/15 00:44:01 $}
@LabeledChapter{Tasks and Synchronization}

@Comment{$Source: e:\\cvsroot/ARM/Source/09.mss,v $}
@Comment{$Revision: 1.2 $}

@begin{Intro}

@PDefn2{Term=execution, Sec=(Ada program)}
The execution of an Ada program consists of the execution of one
or more @i(tasks).
@Defn{task}
@Defn2{Term=interaction, Sec=(between tasks)}
Each task represents a separate thread of
control that proceeds independently and concurrently
between the points where it @i(interacts) with other tasks.
The various forms of task interaction are
described in this section, and include:
@IndexSee{Term=[parallel processing],See=(task)}
@Defn{synchronization}
@IndexSee{Term=[concurrent processing],See=(task)}
@IndexSeeAlso{Term=[intertask communication],See=(task)}
@begin(Honest)
  The execution of an Ada program consists of the execution
  of one or more partitions (@lSeeSecNum(Program Execution)),
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
@Defn2{Term=blocked, Sec=(a task state)}
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
  @Defn{task dispatching policy}
  @Defn{dispatching policy for tasks}
  The means for selecting which of the ready tasks to run,
  given the currently available execution resources, is determined by the
  @i(task dispatching policy) in effect, which is generally
  implementation defined, but may be controlled by pragmas
  and operations defined in the Real-Time Annex
  (@lSeeSecNum(Priority Scheduling) and @RefSecNum(Dynamic Priorities)).
@end(Discussion)
@end{RunTime}

@begin{NotesNotes}

Concurrent task execution may be implemented on
multicomputers, multiprocessors, or with interleaved execution on a single
physical processor.  On the other hand, whenever an implementation can
determine that the required semantic effects can be achieved when
parts of the execution of a
given task are performed by different physical processors acting in
parallel, it may choose to perform them in this way.

@end{NotesNotes}

@begin{DiffWord83}
The introduction has been rewritten.

We use the term "concurrent" rather than "parallel" when talking
about logically independent execution of threads of control.
The term "parallel" is reserved for referring to the
situation where multiple physical processors run simultaneously.
@end{DiffWord83}

@LabeledSection{Task Units and Task Objects}

@begin{Intro}
@Defn{task declaration}
A task unit is declared by a @i(task declaration), which has
a corresponding @nt<task_body>.  A task declaration may be
a @nt<task_type_declaration>, in which case it declares
a named task type; alternatively, it may be a @nt<single_task_declaration>,
in which case it defines an anonymous task type, as well as declaring
a named task object of that type.
@end{Intro}

@begin{Syntax}
@Syn{lhs=<task_type_declaration>,rhs="
   @key{task} @key{type} @Syn2{defining_identifier} [@Syn2{known_discriminant_part}] [@key{is} @Syn2{task_definition}];"}

@Syn{lhs=<single_task_declaration>,rhs="
   @key{task} @Syn2{defining_identifier} [@key{is} @Syn2{task_definition}];"}

@Hinge{}
@Syn{lhs=<task_definition>,rhs="
     {@Syn2{task_item}}
  [ @key{private}
     {@Syn2{task_item}}]
  @key{end} [@SynI{task_}@Syn2{identifier}]"}

@Syn{lhs=<task_item>,rhs="@Syn2{entry_declaration} | @Syn2{representation_clause}"}

@Hinge{}
@Syn{lhs=<task_body>,rhs="
   @key{task} @key{body} @Syn2{defining_identifier} @key{is}
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
@PDefn2{Term=[requires a completion], Sec=(@nt{@nt{task_declaration}})}
A task declaration requires a completion@begin{Redundant},
which shall be a @nt{task_body},
@end{Redundant}
and every @nt{task_body} shall be the completion of some
task declaration.
@begin(Honest)
  The completion can be a @nt{pragma} Import,
  if the implementation supports it.
@end(Honest)
@end{Legality}

@begin{StaticSem}

A @nt<task_definition> defines a task type and its first subtype.
@PDefn2{Term=[visible part], Sec=(of a task unit)}
The first list of @nt{task_item}s of a @nt{task_definition},
@oBigChg{}
together with the @nt{known_discriminant_part}, if any,
@oEndBigChg{}
is called the visible part of the task unit.
@oChgRef{94-4426.a}
@oChgRef{94-4873.a}
@oChgRef{94-4881.a}
@Redundant[@PDefn2{Term=[private part], Sec=(of a task unit)}
The optional list of @nt{task_item}s after the reserved
word @key{private} is called the private part of the task unit.]
@TheProof{Private part is defined in Section 8.}

@end{StaticSem}

@begin{RunTime}

@begin(Redundant)
@PDefn2{Term=[elaboration], Sec=(task declaration)}
The elaboration of a task declaration elaborates the @nt<task_definition>.
@PDefn2{Term=[elaboration], Sec=(single_task_declaration)}
The elaboration of a @nt<single_task_declaration> also creates
an object of an (anonymous) task type.
@end(Redundant)
@begin(TheProof)
  This is redundant with the general rules for the elaboration
  of a @nt<full_type_declaration> and an @nt<object_declaration>.
@end(TheProof)

@PDefn2{Term=[elaboration], Sec=(task_definition)}
@Redundant[The elaboration of a @nt<task_definition>
creates the task type and its first
subtype;] it also includes the elaboration of the @nt<entry_declaration>s
in the given order.

@PDefn2{Term=[initialization], Sec=(of a task object)}
As part of the initialization of a task object, any
@nt<representation_clause>s and any per-object
constraints associated with @nt<entry_declaration>s
of the corresponding @nt<task_definition>
are elaborated in the given order.
@begin{Reason}
  The only @nt<representation_clause>s defined for task entries
  are ones that specify the Address of an entry,
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

@begin(Redundant)
The execution of a @nt<task_body> is invoked by the activation of a
task of the corresponding type
(@lSeeSecNum(Task Execution - Task Activation)).
@end(Redundant)

The content of a task object of a given task type includes:
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

@begin{NotesNotes}

Within the declaration or body of a task unit, the name of
the task unit denotes the current instance of the unit
(@lSeeSecNum(The Context of Overload Resolution)),
rather than the first subtype of the corresponding task type (and
thus the name cannot be used as a @nt<subtype_mark>).
@begin(Discussion)
However, it is possible to refer to
some other subtype of the task type within its body,
presuming such a subtype has been
declared between the @nt<task_type_declaration> and the @nt<task_body>.
@end(Discussion)

The notation of a @nt<selected_component> can be used to denote a discriminant
of a task (@lSeeSecNum(Selected Components)).
Within a task unit, the name of a discriminant of the task type
denotes the corresponding discriminant of the current instance
of the unit.

A task type is a limited type (@lSeeSecNum(Limited Types)),
and hence has neither
an assignment operation nor predefined equality operators.
If an application needs to store and exchange task identities, it
can do so by defining an access type designating the corresponding
task objects and by using access values for identification purposes.
Assignment is available for such an access type as for any
access type.
Alternatively, if the implementation supports the
Systems Programming Annex,
the Identity attribute
can be used for task identification
(@lSeeSecNum(Task Identification and Attributes)).
@end{NotesNotes}

@begin{Examples}
@i{Examples of declarations of task types:}
@begin{Example}
@key(task) @key(type) Server @key(is)
   @key(entry) Next_Work_Item(WI : @key(in) Work_Item);
   @key(entry) Shut_Down;
@key(end) Server;

@key(task) @key(type) Keyboard_Driver(ID : Keyboard_ID := New_ID) @key(is)
   @key(entry) Read (C : @key(out) Character);
   @key(entry) Write(C : @key(in)  Character);
@key(end) Keyboard_Driver;
@end{Example}

@i{Examples of declarations of single tasks:}
@begin{Example}
@key(task) Controller @key(is)
   @key(entry) Request(Level)(D : Item);  @i[--  a family of entries]
@key(end) Controller;

@key(task) Parser @key(is)
   @key(entry) Next_Lexeme(L : @key(in)  Lexical_Element);
   @key(entry) Next_Action(A : @key(out) Parser_Action);
@key(end);

@key(task) User;  @i[--  has no entries]
@end{Example}

@i{Examples of task objects:}
@begin{Example}
Agent    : Server;
Teletype : Keyboard_Driver(TTY_ID);
Pool     : @key(array)(1 .. 10) @key(of) Keyboard_Driver;
@end{Example}

@i{Example of access type designating task objects:}
@begin{Example}
@key(type) Keyboard @key(is) @key(access) Keyboard_Driver;
Terminal : Keyboard := @key(new) Keyboard_Driver(Term_ID);
@end{Example}

@end{Examples}

@begin{Extend83}
The syntax rules for task declarations are modified to allow a
@nt{known_discriminant_part}, and to allow a private part.
They are also modified to allow @nt{entry_declaration}s and
@nt{representation_clause}s to be mixed.
@end{Extend83}

@begin{DiffWord83}
The syntax rules for tasks have been split up according to task types and
single tasks.
In particular:
The syntax rules for @nt{task_declaration} and @nt{task_specification} are
removed.  The syntax rules for
@nt{task_type_declaration}, @nt{single_task_declaration}, @nt{task_definition}
and @nt{task_item} are new.

The syntax rule for @nt{task_body} now uses the nonterminal
@nt{handled_sequence_of_statements}.

The @nt{declarative_part} of a @nt{task_body} is now required;
that doesn't make any real difference,
because a @nt{declarative_part} can be empty.
@end{DiffWord83}

@LabeledSection{Task Execution - Task Activation}

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

A task object (which represents one task)
can be created either as part of the elaboration
of an @nt<object_declaration> occurring immediately within some
declarative region,
or as part of the evaluation of an @nt<allocator>.
All tasks created by the elaboration of @nt<object_declaration>s
of a single declarative region (including subcomponents
of the declared objects) are activated together.  Similarly,
all tasks created by the evaluation of a single @nt<allocator>
are activated together.  The activation of a task is associated
with the innermost @nt<allocator> or @nt<object_declaration>
that is responsible for its creation.
@begin{Discussion}
The initialization of an @nt{object_declaration} or @nt{allocator} can
indirectly include the creation of other objects that contain tasks.
For example, the default expression for a subcomponent of an object
created by an @nt{allocator} might call a function that evaluates a
completely different @nt{allocator}.  Tasks created by the two
allocators are @i{not} activated together.
@end{Discussion}

For tasks created by the elaboration of @nt<object_declaration>s
of a given declarative region, the activations are initiated
within the context of the @nt<handled_sequence_of_statements>
(and its associated @nt<exception_handler>s if any @em
@lSeeSecNum{Exception Handlers}), just prior to executing the
statements of the @nt<_sequence>.
@Redundant[For a package without an explicit body or an explicit
@nt<handled_sequence_of_statements>,
an implicit body or an implicit @nt<null_statement> is assumed,
as defined in @RefSecNum(Package Bodies).]
@begin(Ramification)
  If Tasking_Error is raised, it can be handled by handlers of
  the @nt<handled_sequence_of_statements>.
@end(Ramification)

For tasks created by the evaluation of an @nt<allocator>,
the activations are initiated as the last step of
evaluating the @nt<allocator>, after completing
any initialization for the object created by the @nt<allocator>,
and prior to returning the new access
value.

@Defn2{Term=activator, Sec=(of a task)}
@PDefn2{term=blocked, Sec=(waiting for activations to complete)}
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
it initiated the activations.  Otherwise, the activator
proceeds with its execution normally.  Any tasks that are aborted
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

Should the task that created
the new tasks never reach the point
where it would initiate the activations (due to an abort or the
raising of an exception),
the newly created tasks become terminated and are never activated.
@end{RunTime}

@begin{NotesNotes}

An entry of a task can be called before the task has been activated.

If several tasks are activated together, the execution of any of these
tasks need not await the end of the activation of the other tasks.

A task can become completed during its activation either because of an
exception or because it is aborted
(@lSeeSecNum(Abort of a Task - Abort of a Sequence of Statements)).

@end{NotesNotes}

@begin{Examples}
@i{Example of task activation:}
@begin{Example}
@key(procedure) P @key(is)
   A, B : Server;    @i[--  elaborate the task objects A, B]
   C    : Server;    @i[--  elaborate the task object C]
@key(begin)
   @i[--  the tasks A, B, C are activated together before the first statement]
   ...
@key(end);
@end{Example}

@end{Examples}

@begin{DiffWord83}

We have replaced the term @i{suspended} with @i{blocked},
since we didn't want to consider a task blocked when it was
simply competing for execution resources.  "Suspended" is sometimes
used more generally to refer to tasks that are not actually running
on some processor, due to the lack of resources.

This clause has been rewritten in an attempt to improve presentation.
@end{DiffWord83}

@LabeledSection{Task Dependence - Termination of Tasks}

@begin{RunTime}

@Defn2{Term=dependence, Sec=(of a task on a master)}
@Defn2{Term=[task], Sec=(dependence)}
@Defn2{Term=[task], Sec=(completion)}
@Defn2{Term=[task], Sec=(termination)}
Each task (other than an environment task  @em @lSeeSecNum(Program Execution))
@i(depends) on one or more masters
(@lSeeSecNum(Completion and Finalization)), as follows:
@begin(itemize)
If the task is created by the evaluation of an @nt<allocator>
for a given access type,
it depends on each master @oBigChg{}that@oEndBigChg{} includes the
elaboration of the declaration of the ultimate ancestor of the given
access type.

If the task is created by the elaboration of an @nt<object_declaration>,
it depends on each master @oBigChg{}that@oEndBigChg{} includes this elaboration.
@oChgRef{94-4715.a}
@end(itemize)

@Defn2{term=dependence, Sec=(of a task on another task)}
Furthermore, if a task depends on a given master, it is defined
to @oChg{}depend@oEndChg{} on the task that executes the master, and (recursively)
on any master of that task.
@begin{Discussion}
Don't confuse these kinds of dependences with the
dependences among compilation units defined in
@RefSec{Compilation Units - Library Units}.
@end{Discussion}

A task is said to be @i(completed) when the execution of its corresponding
@nt<task_body> is completed.  A task is said to be @i(terminated) when
any finalization of the @nt<task_body> has been performed
(@lSeeSecNum(Completion and Finalization)).
@Redundant[The first step of finalizing a master
(including a @nt<task_body>) is to
wait for the termination of any tasks dependent on the master.]
@PDefn2{Term=blocked, Sec=(waiting for dependents to terminate)}
The task executing the master is blocked until all the dependents
have terminated.  @Redundant[Any remaining finalization is then performed
and the master is left.]

Completion of a task (and the corresponding @nt<task_body>) can occur when
the task is blocked at a @nt<select_statement> with an
an open @nt<terminate_alternative>
(@lSeeSecNum(Selective Accept)); the open @nt<terminate_alternative>
is selected if and only if the following conditions are satisfied:
@begin{itemize}
  The task depends on some @oBigChg{}completed master;@oEndBigChg{}
@oChgRef{94-4715.a}

  Each task that depends on the master considered is either already
  terminated or similarly blocked at a @nt<select_statement>
  with an open @nt{terminate_alternative}.
@end{itemize}

When both conditions are satisfied, the task considered becomes
completed, together with all tasks that depend on the master
considered that are not yet completed.
@begin(Ramification)
  Any required finalization is performed after the selection
  of @nt<terminate_alternative>s.  The tasks are not callable
  during the finalization.  In some ways it is as though they were
  aborted.
@end(Ramification)

@end{RunTime}

@begin{NotesNotes}

The full view of a limited private type can be a task type, or
can have subcomponents of a task type.  Creation of an object of
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

The completion of a task can occur due to any of the following:
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

@end{NotesNotes}

@begin{Examples}
@i{Example of task dependence:}
@begin{Example}
@key(declare)
   @key(type) Global @key(is) @key(access) Server;        @i[--  @lSeeSecNum(Task Units and Task Objects)]
   A, B : Server;
   G    : Global;
@hinge{}
@key(begin)
   @i[--  activation of A and B]
   @key(declare)
      @key(type) Local @key(is) @key(access) Server;
      X : Global := @key(new) Server;  @i[--  activation of X.@key{all}]
      L : Local  := @key(new) Server;  @i[--  activation of L.@key{all}]
      C : Server;
@hinge{}
   @key(begin)
      @i[--  activation of C]
      G := X;  @i[--  both G and X designate the same task object]
      ...
   @key(end;)  @i[--  await termination of C and L.@key{all} (but not X.@key{all})]
   ...
@key(end;)  @i[--  await termination of A, B, and G.@key{all}]
@end{Example}

@end{Examples}

@begin{DiffWord83}
We have revised the wording to be consistent with the definition
of master now given in @RefSec(Completion and Finalization).

Tasks that used to depend on library packages in Ada 83, now depend on the
(implicit) @nt<task_body> of the
environment task (@lSeeSecNum(Program Execution)).
Therefore, the environment task has to wait for
them before performing library level finalization and terminating
the partition.
In Ada 83 the requirement to wait for tasks that depended
on library packages was not as clear.

What was "collective termination" is now "collective completion"
resulting from selecting @nt<terminate_alternative>s.  This is because
finalization still occurs for such tasks, and this happens after
selecting the @nt<terminate_alternative>, but before termination.
@end{DiffWord83}

@LabeledSection{Protected Units and Protected Objects}

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
@Syn{lhs=<protected_type_declaration>,rhs="
  @key{protected} @key{type} @Syn2{defining_identifier} [@Syn2{known_discriminant_part}] @key{is} @Syn2{protected_definition};"}
@Hinge{}

@Syn{lhs=<single_protected_declaration>,rhs="
  @key{protected} @Syn2{defining_identifier} @key{is} @Syn2{protected_definition};"}

@hinge()
@Syn{lhs=<protected_definition>,rhs="
    { @Syn2{protected_operation_declaration} }
[ @key{private}
    { @Syn2{protected_element_declaration} } ]
  @key{end} [@SynI{protected_}@Syn2{identifier}]"}

@Syn{lhs=<protected_operation_declaration>,
  rhs="@Syn2{subprogram_declaration}
     | @Syn2{entry_declaration}
     | @Syn2{representation_clause}"}
@Syn{lhs=<protected_element_declaration>,
  rhs="@Syn2<protected_operation_declaration>
     | @Syn2<component_declaration>"}
@begin{Reason}
     We allow the operations and components to be mixed because that's how
     other things work (for example, package
     declarations).  We have relaxed the
     ordering rules for the items inside @nt{declarative_part}s and
     @nt{task_definition}s as well.
@end{Reason}
@hinge()

@Syn{lhs=<protected_body>,rhs="
  @key{protected} @key{body} @Syn2{defining_identifier} @key{is}
   { @Syn2{protected_operation_item} }
  @key{end} [@SynI{protected_}@Syn2{identifier}];"}

@Syn{lhs=<protected_operation_item>,
  rhs="@Syn2{subprogram_declaration}
     | @Syn2{subprogram_body}
     | @Syn2{entry_body}
     | @Syn2{representation_clause}"}

@begin{SyntaxText}
If a @SynI{protected_}@nt{identifier} appears at
the end of a @nt{protected_definition} or @nt{protected_body},
it shall repeat the @nt{defining_identifier}.
@end{SyntaxText}
@end{Syntax}

@begin{Legality}
@PDefn2{Term=[requires a completion], Sec=(@nt{@nt{protected_declaration}})}
A protected declaration requires a completion@begin{Redundant},
which shall be a @nt{protected_body},
@end{Redundant}
and every @nt{protected_body} shall be the completion of some
protected declaration.
@begin(Honest)
  The completion can be a @nt{pragma} Import,
  if the implementation supports it.
@end(Honest)
@end{Legality}

@begin{StaticSem}

A @nt<protected_definition> defines a protected type and its first subtype.
@PDefn2{Term=[visible part], Sec=(of a protected unit)}
The list of @nt{protected_operation_declaration}s of a
@nt{protected_definition},
@oBigChg{}
together with the @nt{known_discriminant_part}, if any,
@oEndBigChg{}
is called the visible part of the protected unit.
@oChgRef{94-4426.a}
@oChgRef{94-4873.a}
@oChgRef{94-4881.a}
@Redundant[@PDefn2{Term=[private part], Sec=(of a protected unit)}
The optional list of @nt{protected_element_declaration}s after the reserved
word @key{private} is called the private part of the protected unit.]
@TheProof{Private part is defined in Section 8.}

@end{StaticSem}

@begin{RunTime}

@begin(Redundant)
@PDefn2{Term=[elaboration], Sec=(protected declaration)}
The elaboration of a protected declaration
elaborates the @nt<protected_definition>.
@PDefn2{Term=[elaboration], Sec=(single_protected_declaration)}
The elaboration of a @nt<single_protected_declaration> also creates
an object of an (anonymous) protected type.
@end(Redundant)
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

@begin(Redundant)
@PDefn2{Term=[initialization], Sec=(of a protected object)}
As part of the initialization of a protected object,
any per-object constraints (@lSeeSecNum{Record Types}) are elaborated.
@end(Redundant)
@begin{Discussion}
  We do not mention pragmas since each pragma has its
  own elaboration rules.
@end{Discussion}

@PDefn2{Term=[elaboration], Sec=(protected_body)}
The elaboration of a @nt{protected_body} has no other effect than to establish
that protected operations of the type can from then on be called without
failing the Elaboration_Check.

The content of an object of a given protected type includes:
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
@lSeeSecNum(Protected Subprograms and Protected Actions))
either for concurrent read-only access, or for exclusive
read-write access.]

@PDefn2{Term=[finalization], Sec=(of a protected object)}
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
As the first step of the @i{finalization} of a protected object,
each call remaining on any entry queue of the object
is removed from its queue and
Program_Error is raised at the place of the corresponding
@nt<entry_call_statement>.
@begin(Reason)
  This is analogous to the raising of Tasking_Error in callers
  of a task that completes before accepting the calls.
  This situation can only occur due to a requeue (ignoring premature
  unchecked_deallocation), since any task that has accessibility to
  a protected object is awaited before finalizing the protected
  object.
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

The environment task is queued on PO.EE when PO is finalized.

In a real example, a server task might park callers on a local protected
object for some useful purpose, so we didn't want to disallow this case.
@end(Reason)
@end{RunTime}

@begin{NotesNotes}

Within the declaration or body of a protected unit, the name of
the protected unit denotes the current instance of the unit
(@lSeeSecNum(The Context of Overload Resolution)),
rather than the first subtype of the corresponding protected type (and
thus the name cannot be used as a @nt<subtype_mark>).
@begin(Discussion)
  However, it is possible to refer to
  some other subtype of the protected type within its body,
  presuming such a subtype has been
  declared between the @nt<protected_type_declaration>
  and the @nt<protected_body>.
@End(Discussion)

A @nt<selected_component> can be used to denote a discriminant
of a protected object (@lSeeSecNum(Selected Components)).
Within a protected unit, the name of a discriminant of the protected type
denotes the corresponding discriminant of the current instance
of the unit.

A protected type is a limited type (@lSeeSecNum(Limited Types)),
and hence has neither
an assignment operation nor predefined equality operators.

The bodies of the protected operations given in the @nt<protected_body>
define the actions that take place upon calls to the protected operations.

The declarations in the private part are only
visible within the private part and the body of the
protected unit.
@begin{Reason}
@nt{Component_declarations} are disallowed in a @nt{protected_body}
because, for efficiency, we wish to allow the compiler to
determine the size of protected objects (when not dynamic);
the compiler cannot necessarily see the body.
Furthermore, the semantics of initialization of such objects would be
problematic @em we do not wish to give protected objects complex
initialization semantics similar to task activation.

The same applies to @nt{entry_declaration}s,
since an entry involves an implicit component @em the entry queue.
@end{Reason}

@end{NotesNotes}

@begin{Examples}
@i{Example of declaration of protected type and corresponding body:}
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

@i{Example of a single protected declaration and corresponding body:}
@begin{Example}
@key(protected) Shared_Array @key(is)
   @i[--  Index, Item, and Item_Array are global types]
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

@i{Examples of protected objects:}
@begin{Example}
Control  : Resource;
Flags    : @key(array)(1 .. 100) @key(of) Resource;
@end{Example}
@end{Examples}

@begin{Extend83}
This entire clause is new;
protected units do not exist in Ada 83.
@end{Extend83}

@LabeledSection{Intertask Communication}

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
@em @lSeeSecNum(Entry Calls)),
and then communicating data or control information directly
with another task or
indirectly via a shared protected object.

@end{Intro}

@begin{StaticSem}
@Defn2{Term=[target object],
  Sec=(of a call on an entry or a protected subprogram)}
Any call on an entry or on a protected subprogram
identifies a @i(target object) for the operation,
which is either a task (for an entry call) or a protected
object (for an entry call or a protected subprogram call).
The target object is considered an implicit parameter to the operation,
and is determined by the operation
@nt<name> (or @nt<prefix>) used in the call on the operation, as follows:
@begin(Itemize)
  If it is a @nt<direct_name> or expanded name
  that denotes the declaration (or body) of the operation, then
  the target object is implicitly specified to be
  the current instance of the task or protected unit
  immediately enclosing the operation;
  @Defn{internal call}
  such a call is defined to be an @i(internal call);

  If it is a @nt<selected_component> that is not
  an expanded name, then the target object is explicitly
  specified to be the task or protected object
  denoted by the @nt<prefix> of the @nt<name>;
  @Defn{external call}
  such a call is defined to be an @i(external call);
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
    Op1; --@i{ An internal call.}
    Pt.Op1; --@i{ Another internal call.}
    PO.Op1; --@i{ An external call.  It the current instance is PO, then}
            --@i{ this is a bounded error (@lSeeSecNum{Protected Subprograms and Protected Actions}).}
    Other_Object.Some_Op; --@i{ An external call.}
  @key[end] Op2;
@key[end] Pt;
@end{Example}
  @end{Discussion}

  If the @nt<name> or @nt<prefix> is a dereference
  (implicit or explicit) of an
  access-to-protected-subprogram value,
  then the target object is determined by the
  @nt<prefix> of the Access @nt<attribute_reference>
  that produced the access value originally, and the
  call is defined to be an @i(external call);

  If the @nt<name> or @nt<prefix> denotes a
  @nt<subprogram_renaming_declaration>,
  then the target object is as determined by the @nt<name> of the renamed
  entity.

@end(Itemize)

@Defn2{Term=[target object],
  Sec=(of a @nt<requeue_statement>)}
@Defn{internal requeue}
@Defn{external requeue}
A corresponding definition of target object applies
to a @nt<requeue_statement> (@lSeeSecNum(Requeue Statements)),
with a corresponding distinction between an @i(internal requeue)
and an @i(external requeue).
@end{StaticSem}

@begin{RunTime}
Within the body of a protected operation, the current instance
(@lSeeSecNum(The Context of Overload Resolution))
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

@LabeledSubSection{Protected Subprograms and Protected Actions}

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
@SeeSec{Conformance Rules}.
@end{Ramification}
@end{Intro}

@begin{StaticSem}
Within the body of a protected function
(or a function declared immediately within a @nt<protected_body>),
the current instance of the enclosing protected unit is defined to be a
constant
@Redundant[(that is, its subcomponents may be read but not updated)].
Within the body of a protected procedure
(or a procedure declared immediately within a @nt<protected_body>),
and within an @nt<entry_body>,
the current instance is defined to be a variable
@Redundant[(updating is permitted)].
@begin(Ramification)
  The current instance is like an implicit parameter,
  of mode @key(in) for a protected function, and of mode @key(in out)
  for a protected procedure (or protected entry).
@end(Ramification)

@end{StaticSem}

@begin{RunTime}

@PDefn2{Term=[execution], Sec=(protected subprogram call)}
For the execution of a call on a protected subprogram,
the evaluation of the @nt<name> or @nt<prefix>
and of the parameter associations,
and any assigning back of @key[in out] or @key[out] parameters,
proceeds as for a normal subprogram call (@lSeeSecNum{Subprogram Calls}).
If the call is an internal call (@lSeeSecNum(Intertask Communication)),
the body of the subprogram
is executed as for a normal subprogram call.
If the call is an external call, then
the body of the subprogram is executed as part of a new
@i(protected action) on the target protected object;
the protected action completes after the body of the
subprogram is executed.
@Redundant[A protected action can also be started by an entry call
(@lSeeSecNum{Entry Calls}).]

@Defn{protected action}
A new protected action is not started on a protected object
while another protected action on the same protected object is underway,
unless both actions are the result of a call on a protected function.
This rule is expressible in terms of the execution resource
associated with the protected object:
@begin(Itemize)
@Defn2{Term=[protected action], Sec=(start)}
@Defn2{Term=acquire, Sec=(execution resource associated with protected object)}
@i(Starting) a protected action on a protected object
corresponds to @i(acquiring) the execution resource associated
with the protected object, either for concurrent read-only access
if the protected action is for a call on a protected function,
or for exclusive read-write access otherwise;

@Defn2{Term=[protected action], Sec=(complete)}
@Defn2{Term=release, Sec=(execution resource associated with protected object)}
@i(Completing) the protected action
corresponds to @i(releasing) the associated execution resource.
@end(Itemize)

@Redundant[After performing an operation on a protected object other than
a call on a protected function, but prior
to completing the associated protected action,
the entry queues (if any)
of the protected object are
serviced (@lSeeSecNum(Entry Calls)).]
@end{RunTime}

@begin{Bounded}
During a protected action,
it is a bounded error to invoke an operation that
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

Certain language-defined subprograms are
potentially blocking.
In particular, the subprograms of
the language-defined input-output packages that manipulate
files (implicitly or explicitly) are potentially blocking.
Other potentially blocking subprograms are identified
where they are defined.
When not specified as potentially blocking,
a language-defined subprogram is nonblocking.
@end{Bounded}

@begin{NotesNotes}
If two tasks both try to start a protected action
on a protected object, and at most one is calling
a protected function, then only one of the tasks can proceed.
Although the other task cannot proceed, it is not considered
blocked, and it might be consuming processing resources while it
awaits its turn.  There is no language-defined ordering or queuing
presumed for tasks competing to start a protected action @em
on a multiprocessor such tasks might use busy-waiting; for
monoprocessor considerations, @lSeeSec{Priority Ceiling Locking}.
@begin{Discussion}
The intended implementation on a multi-processor is in terms of
``spin locks'' @em the waiting task will spin.
@end{Discussion}

The body of a protected unit may contain declarations and bodies for local
subprograms.  These are not visible outside the protected unit.

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
  a protected object.  The acquisition of such a resource
  is rather considered part of the normal competition for execution
  resources between the various tasks that are ready.
  External calls that turn out to be on the same target
  object are considered potentially blocking, since they
  can deadlock the task indefinitely.
@end(Reason)
@end{NotesNotes}

@begin{Examples}
@i{Examples of protected subprogram calls
(@lSeeSecNum(Protected Units and Protected Objects)):}
@begin{Example}
Shared_Array.Set_Component(N, E);
E := Shared_Array.Component(M);
Control.Release;
@end{Example}

@end{Examples}

@LabeledSubSection{Entries and Accept Statements}

@begin{Intro}
@nt<Entry_declaration>s, with the corresponding @nt<entry_bodies>
or @nt<accept_statement>s,
are used to define potentially queued operations on
tasks and protected objects.
@end{Intro}

@begin{Syntax}
@Syn{lhs=<entry_declaration>,rhs="
   @key{entry} @Syn2{defining_identifier} [(@Syn2{discrete_subtype_definition})] @Syn2{parameter_profile};"}

@Hinge{}
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

@Hinge{}
@Syn{lhs=<entry_body>,rhs="
  @key{entry} @Syn2{defining_identifier}  @Syn2{entry_body_formal_part}  @Syn2{entry_barrier} @key{is}
    @Syn2{declarative_part}
  @key{begin}
    @Syn2{handled_sequence_of_statements}
  @key{end} [@SynI{entry_}@Syn2{identifier}];"}

@Hinge{}
@Syn{lhs=<entry_body_formal_part>,
  rhs="[(@Syn2{entry_index_specification})] @Syn2{parameter_profile}"}

@Hinge{}
@Syn{lhs=<entry_barrier>,
  rhs="@key{when} @Syn2{condition}"}

@Hinge{}
@Syn{lhs=<entry_index_specification>,
  rhs="@key{for} @Syn2{defining_identifier} @key{in} @Syn2{discrete_subtype_definition}"}

@begin{SyntaxText}
If an @SynI{entry_}@nt{identifier} appears at the end of an
@nt{accept_statement}, it shall repeat the @SynI{entry_}@nt<direct_name>.
If an @SynI{entry_}@nt{identifier} appears at the end of an @nt{entry_body}, it shall repeat the
@nt{defining_identifier}.

@Redundant[An @nt{entry_declaration} is allowed only in a protected or task
declaration.]
@begin(TheProof)
  This follows from the BNF.
@end(TheProof)
@end{SyntaxText}
@end{Syntax}

@begin{Resolution}

@PDefn2{Term=[expected profile],
  Sec=(accept_statement @i{entry_}@nt<direct_name>)}
In an @nt<accept_statement>,
the expected profile for the @SynI{entry_}@nt<direct_name>
is that of the @nt<entry_declaration>;
@PDefn2{Term=[expected type],
  Sec=(entry_index)}
the expected type for an @nt<entry_index> is that
of the subtype defined by the @nt<discrete_subtype_definition>
of the corresponding @nt<entry_declaration>.

Within the @nt<handled_sequence_of_statements> of an @nt<accept_statement>,
if a @nt<selected_component> has a @nt<prefix> that denotes
the corresponding @nt<entry_declaration>, then the
entity denoted by the @nt<prefix> is the @nt<accept_statement>, and
the @nt<selected_component> is interpreted as an expanded name
(@lSeeSecNum(Selected Components))@Redundant[; the @nt<selector_name>
of the @nt<selected_component> has to be the @nt<identifier> for
some formal parameter of the @nt<accept_statement>].
@begin{TheProof}
  The only declarations that occur immediately within the
  declarative region of an @nt<accept_statement> are those
  for its formal parameters.
@end{TheProof}
@end{Resolution}

@begin{Legality}

An @nt<entry_declaration> in a task declaration shall not contain
a specification for an access parameter (@lSeeSecNum(Access Types)).
@begin(Reason)
  Access parameters for task entries would require a complex implementation.
  For example:
  @begin(Example)
@key(task) T @key(is)
   @key(entry) E(Z : @key(access) Integer); --@i{ Illegal!}
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
         X := A(Z); -- Accessibility_Check
      @key(end) E;
   @key(end);
@key(end) T;
@end(Example)

Implementing the Accessibility_Check inside the @nt<accept_statement> for
E is difficult, since one does not know whether the entry caller
is calling from inside the immediately enclosing declare block or from
outside it.  This means that the lexical nesting level associated with
the designated object is not sufficient to determine whether the
Accessibility_Check should pass or fail.

Note that such problems do not arise with protected entries, because
@nt<entry_bodies> are always nested immediately within the
@nt<protected_body>; they cannot be further nested as can
@nt<accept_statement>s, nor can they be called from within the
@nt<protected_body> (since no entry calls are permitted inside
a @nt<protected_body>).
@end(Reason)

For an @nt<accept_statement>,
the innermost enclosing body shall be a @nt<task_body>,
and the @i(entry_)@nt<direct_name> shall
denote an @nt<entry_declaration> in the corresponding task declaration;
the profile of the @nt{accept_statement} shall
conform fully to that of the corresponding @nt<entry_declaration>.
@Defn2{Term=[full conformance],Sec=(required)}
An @nt<accept_statement> shall have a parenthesized @nt<entry_index> if
and only if the corresponding @nt<entry_declaration> has a
@nt<discrete_subtype_definition>.

An @nt<accept_statement> shall not be within another
@nt{accept_statement} that corresponds to the same @nt<entry_declaration>,
nor within an @nt<asynchronous_select> inner to
the enclosing @nt<task_body>.
@begin(Reason)
@nt<Accept_statement>s are required to be immediately within
the enclosing @nt<task_body> (as opposed to being in a nested
subprogram) to ensure that a nested task does not
attempt to accept the entry of its enclosing task.  We considered
relaxing this restriction, either by making the check a run-time
check, or by allowing a nested task to accept an entry of its
enclosing task.  However, neither change seemed to provide sufficient
benefit to justify the additional implementation burden.

Nested @nt<accept_statement>s for the same entry (or entry family)
are prohibited to ensure that there is no ambiguity in the
resolution of an expanded name for a formal parameter of the
entry.  This could be relaxed by allowing the inner
one to hide the outer one from all visibility, but again the
small added benefit didn't seem to justify making the change for Ada 9X.

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
a completion@begin{Redundant}, which shall be an @nt{entry_body},
@end{Redundant}
@PDefn2{Term=[only as a completion], Sec=(@nt<entry_body>)}
and every @nt<entry_body> shall be the completion
of an @nt<entry_declaration> of a protected unit.
@PDefn2{Term=[completion legality], Sec=(@nt<entry_body>)}
The profile of the @nt<entry_body> shall conform fully to that
of the corresponding declaration.
@Defn2{Term=[full conformance],Sec=(required)}
@begin{Ramification}
An @nt<entry_declaration>, unlike a @nt<subprogram_declaration>,
cannot be completed with a @nt<renaming_declaration>.
@end{Ramification}
@begin(Honest)
  The completion can be a @nt{pragma} Import,
  if the implementation supports it.
@end(Honest)
@begin{Discussion}
The above applies only to protected entries,
which are the only ones completed with @nt{entry_bodies}.
Task entries have corresponding @nt{accept_statements}
instead of having @nt{entry_bodies}, and
we do not consider an @nt{accept_statement} to be a ``completion,''
because a task @nt{entry_declaration} is allowed to have zero, one, or more
than one corresponding @nt{accept_statement}s.
@end{Discussion}

An @nt{entry_body_formal_part} shall have an @nt{entry_index_specification}
if and only if the corresponding @nt{entry_declaration} has
a @nt<discrete_subtype_definition>.
In this case, the @nt<discrete_subtype_definition>s of the
@nt<entry_declaration> and the @nt<entry_index_specification>
shall fully conform to one another (@lSeeSecNum(Conformance Rules)).
@Defn2{Term=[full conformance],Sec=(required)}

A name that denotes a formal parameter of an @nt<entry_body> is not
allowed within the @nt<entry_barrier> of the @nt<entry_body>.

@end{Legality}

@begin{StaticSem}
The parameter modes defined for parameters in the @nt<parameter_profile>
of an @nt{entry_declaration}
are the same as for a @nt<subprogram_declaration> and have
the same meaning (@lSeeSecNum(Formal Parameter Modes)).
@begin{Discussion}
Note that access parameters are not allowed for task entries (see above).
@end{Discussion}

@Defn2{Term=family, Sec=(entry)}
@Defn{entry family}
@Defn{entry index subtype}
An @nt<entry_declaration> with a @nt<discrete_subtype_definition>
(@lSeeSecNum(Array Types)) declares a @i(family) of distinct
entries having the same profile, with one such entry for each
value of the @i(entry index subtype) defined
by the @nt<discrete_subtype_definition>.
@Redundant[A name for an entry of a family takes the form of
an @nt<indexed_component>, where the @nt<prefix> denotes
the @nt<entry_declaration> for the family, and the index value
identifies the entry within the family.]
@Defn{single entry}
@Defn2{Term=entry, Sec=(single)}
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

@PDefn2{Term=[elaboration], Sec=(entry_declaration)}
For the elaboration of an @nt<entry_declaration> for an
entry family, if the
@nt{discrete_subtype_definition} contains no per-object expressions
(@lSeeSecNum(Record Types)), then the @nt<discrete_subtype_definition>
is elaborated.  Otherwise, the elaboration of the
@nt<entry_declaration> consists of the evaluation of any
expression of the @nt<discrete_subtype_definition>
that is not a per-object expression (or part of one).
The elaboration of an @nt<entry_declaration> for a single entry
has no effect.
@begin{Discussion}
The elaboration of the declaration of a protected subprogram has
no effect, as specified in clause @RefSecNum(Subprogram Declarations).
The default initialization of an object of a task or protected
type is covered in @RefSecNum(Object Declarations).
@end{Discussion}

@Redundant[The actions to be performed when
an entry is called are specified by the
corresponding @nt{accept_statement}s (if any) for an entry of a task unit,
and by the corresponding @nt<entry_body> for an entry of a protected unit.]

@PDefn2{Term=[execution], Sec=(accept_statement)}
For the execution of an @nt{accept_statement}, the @nt<entry_index>, if
any, is first evaluated and converted to the entry index subtype;
this index value identifies which entry of the family is to be accepted.
@PDefn2{Term=[implicit subtype conversion],Sec=(entry index)}
@PDefn2{Term=blocked, Sec=(on an @nt<accept_statement>)}
@Defn2{Term=selection, Sec=(of an entry caller)}
Further execution of the @nt<accept_statement> is then blocked
until a caller of the corresponding entry is selected
(@lSeeSecNum(Entry Calls)), whereupon
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

Note that we say ``propagated from the
@nt{handled_sequence_of_statements} of an @nt{accept_statement}'',
not ``propagated from an @nt{accept_statement}.''
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
has been selected (@lSeeSecNum(Entry Calls)).]
@PDefn2{Term=[execution], Sec=(entry_body)}
For the execution of the @nt<entry_body>,
the @nt<declarative_part> of the @nt<entry_body> is elaborated,
and the @nt<handled_sequence_of_statements>
of the body is executed, as for the execution
of a @nt<subprogram_body>.  The value of the named entry index, if any,
is determined by the value of the entry index specified in the
@i(entry_)@nt<name> of the selected entry call (or intermediate
@nt<requeue_statement> @em @lSeeSecNum(Requeue Statements)).
@begin(Honest)
If the entry had been renamed as a subprogram,
and the call was a @nt<procedure_call_statement> using
the name declared by the renaming, the entry index (if any) comes from
the entry @nt<name> specified in the
@nt<subprogram_renaming_declaration>.
@end(Honest)

@end{RunTime}

@begin{NotesNotes}

A task entry has corresponding accept_statements (zero or more),
whereas a protected entry has a corresponding entry_body (exactly
one).

A consequence of the rule regarding the allowed placements of
@nt{accept_statement}s is that a task can execute @nt{accept_statement}s
only for its own entries.

A @nt<return_statement> (@lSeeSecNum(Return Statements))
or a @nt<requeue_statement> (@lSeeSecNum(Requeue Statements))
may be used to complete the execution of
an @nt<accept_statement> or an @nt<entry_body>.
@begin{Ramification}
An @nt<accept_statement> need not have a @nt<handled_sequence_of_statements>
even if the corresponding entry has parameters.  Equally, it can have
a @nt<handled_sequence_of_statements> even if the corresponding entry
has no parameters.
@end{Ramification}
@begin{Ramification}
A single entry overloads a subprogram, an enumeration literal, or another
single entry if they have the same @nt{defining_identifier}.  Overloading is
not allowed for entry family names.
A single entry or an entry of an entry family
can be renamed as a procedure as explained in
@RefSecNum{Subprogram Renaming Declarations}.
@end{Ramification}

@begin{Multiple}
The @nt<condition> in the @nt{entry_barrier} may reference
anything visible except the formal parameters of the entry.
This
includes the entry index (if any), the components (including discriminants) of
the protected object, the Count attribute of an entry of that protected object,
and data global to the protected unit.

The restriction against referencing the formal parameters within an
@nt{entry_barrier} ensures that all calls of the same entry see
the same barrier value.
If it is necessary to look at the parameters of an entry
call before deciding whether to handle it, the @nt<entry_barrier>
can be ``@key(when) True'' and the caller can
be requeued (on some private entry)
when its parameters indicate that it cannot be handled immediately.
@end{Multiple}

@end{NotesNotes}

@begin{Examples}
@i{Examples of entry declarations:}
@begin{Example}
@key(entry) Read(V : @key(out) Item);
@key(entry) Seize;
@key(entry) Request(Level)(D : Item);  @i[--  a family of entries]
@end{Example}

@i{Examples of accept statements:}
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

The syntax rule for @nt{entry_body} is new.

@nt{Accept_statement}s can now have @nt{exception_handler}s.

@end{Extend83}

@LabeledSubSection{Entry Calls}

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
(@lSeeSecNum(Select Statements)).]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<entry_call_statement>,rhs="@SynI{entry_}@Syn2{name} [@Syn2{actual_parameter_part}];"}
@end{Syntax}

@begin{Resolution}
The @i(entry_)@nt<name> given in an @nt<entry_call_statement> shall resolve
to denote an entry.  The rules for parameter
associations are the same as for subprogram calls (@lSeeSecNum(Subprogram Calls)
and @RefSecNum(Parameter Associations)).
@end{Resolution}

@begin{StaticSem}
@Redundant[The @i(entry_)@nt<name> of an @nt<entry_call_statement> specifies
(explicitly or implicitly) the target object of the call,
the entry or entry family, and the entry index, if any
(@lSeeSecNum(Intertask Communication)).]
@end{StaticSem}

@begin{RunTime}
@Defn{open entry}
@Defn2{Term=entry, Sec=(open)}
@Defn{closed entry}
@Defn2{Term=entry, Sec=(closed)}
Under certain circumstances (detailed below), an entry of a task
or protected
object is checked to see whether it is @i(open) or @i(closed):
@begin(Itemize)
@Defn2{Term=[open entry], Sec=(of a task)}
@Defn2{Term=[closed entry], Sec=(of a task)}
An entry of a task is open if the task
is blocked on an @nt<accept_statement>
that corresponds to the entry (@lSeeSecNum(Entries and Accept Statements)),
or on a @nt<selective_accept>
(@lSeeSecNum(Selective Accept)) with an open
@nt<accept_alternative> that corresponds to the entry; otherwise
it is closed.

@Defn2{Term=[open entry], Sec=(of a protected object)}
@Defn2{Term=[closed entry], Sec=(of a protected object)}
An entry of a protected object is open if
the @nt<condition> of the @nt<entry_barrier> of the
corresponding @nt<entry_body> evaluates to True; otherwise it is closed.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
If the evaluation of the @nt<condition> propagates an exception, the
exception Program_Error is propagated
to all current callers of all entries of the protected object.
@begin(Reason)
  An exception during barrier evaluation is considered essentially
  a fatal error.  All current entry callers are notified with a Program_Error.
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
  be evaluated.  Implementation permissions are given (below) to
  allow implementations to evaluate the barrier expression more or
  less often than it is checked, but the basic semantic model presumes
  it is evaluated at the times when it is checked.
@end(Discussion)

@PDefn2{Term=[execution], Sec=(entry_call_statement)}
For the execution of an @nt{entry_call_statement},
evaluation of the @nt<name>
and of the parameter associations
is as for a subprogram call (@lSeeSecNum{Subprogram Calls}).
@Defn2{Term=issue, Sec=(an entry call)}
The entry call is then @i(issued):
For a call on an entry of a protected object, a new protected
action is started on the object (@lSeeSecNum(Protected Subprograms and Protected Actions)).
The named entry is checked to see if it is open;
@Defn2{Term=[select an entry call], Sec=(immediately)}
if open, the entry call is said to be @i(selected immediately),
and the execution of the call proceeds as follows:
@begin(Itemize)
  For a call on an open entry of a task, the accepting task becomes ready and
  continues the execution of the corresponding @nt<accept_statement>
  (@lSeeSecNum(Entries and Accept Statements)).

  For a call on an open entry of a protected object, the corresponding
  @nt<entry_body> is executed (@lSeeSecNum(Entries and Accept Statements))
  as part of the protected action.
@end(Itemize)

If the @nt<accept_statement> or @nt<entry_body> completes other than
by a requeue (@lSeeSecNum(Requeue Statements)), return is made to the
caller (after servicing the entry queues @em see below);
any necessary assigning back
of formal to actual parameters occurs,
as for a subprogram call (@lSeeSecNum(Parameter Associations));
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

@Defn2{Term=service, Sec=(an entry queue)}
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

  If after performing, as part of a protected action on the
  associated protected object, an operation on the object other than
  a call on a protected function,
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
queue in order of arrival.  If calls from two or more queues are
simultaneously eligible for selection, the default entry queuing policy
does not specify which queue is serviced first.
Other entry queuing policies can be specified by @nt{pragma}s
(@lSeeSecNum(Entry Queuing Policies)).

For a protected object, the above servicing of entry queues continues
until there are no open entries with queued calls, at which point
the protected action completes.
@begin(Discussion)
  While servicing the entry queues of a protected object, no new calls
  can be added to any entry queue of the object,
  except due to an internal requeue (@lSeeSecNum(Requeue Statements)).
  This is because the first step of a call on a protected entry
  is to start a new protected action, which implies acquiring
  (for exclusive read-write access)
  the execution resource associated with the protected object, which cannot
  be done while another protected action is already in progress.
@end(Discussion)

@PDefn2{Term=blocked, Sec=(during an entry call)}
@oBigChg{}
For an entry call that is added to a queue,
and that is not the @nt<triggering_statement> of an
@nt<asynchronous_select>
(@lSeeSecNum{Asynchronous Transfer of Control}),
the calling task is blocked until the call is cancelled,
or the call is selected and a corresponding @nt<accept_statement>
or @nt<entry_body> completes without requeuing.
In addition, the calling task is blocked during a rendezvous.
@oEndBigChg{}
@oChgRef{94-4776.d}
@begin{Ramification}
@oBigChg{}
For a call on a protected entry,
the caller is not blocked if the call is selected immediately,
unless a requeue causes the call to be queued.
@oEndBigChg{}
@end{Ramification}

@Defn2{Term=[cancellation], Sec=(of an entry call)}
An attempt can be made to cancel an entry call upon an abort
(@lSeeSecNum(Abort of a Task - Abort of a Sequence of Statements))
and as part of certain forms of @nt<select_statement>
(@lSeeSecNum(Timed Entry Calls),
@RefSecNum(Conditional Entry Calls), and
@RefSecNum(Asynchronous Transfer of Control)).
The cancellation does not take place until
a point (if any) when the call is on some entry queue,
and not protected from cancellation as part of a requeue
(@lSeeSecNum(Requeue Statements)); at such a point, the
call is removed from the entry queue and the call completes due
to the cancellation.
The cancellation of a call on an entry of a protected object
is a protected action@Redundant[, and as such cannot take place
while any other protected action is occurring on the protected object.
Like any protected action, it includes servicing of the entry queues
(in case some entry barrier depends on a Count attribute).]
@begin(ImplNote)
  In the case of an attempted cancellation due to abort,
  this removal might have to be performed by the calling task
  itself if the ceiling priority of the protected object
  is lower than the task initiating the abort.
@end(ImplNote)

@Defn2{Term=[Tasking_Error],Sec=(raised by failure of run-time check)}
A call on an entry of a task that has already completed its execution
raises the exception Tasking_Error at the point of the call;
similarly, this exception is raised at the point of the call if the
called task completes its execution or becomes abnormal before accepting
the call or completing the rendezvous
(@lSeeSecNum(Abort of a Task - Abort of a Sequence of Statements)).
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
  on multiprocessors.  On a monoprocessor, which thread of control executes
  the protected action is essentially invisible, since the thread is
  not abortable in any case, and the "current_task" function is not
  guaranteed to work during a protected action
  (@lSeeSecNum(Task Identification and Attributes)).
@end(Reason)

When the entry of a protected object is checked to see whether it
is open, the implementation need not reevaluate
the @nt<condition> of the corresponding @nt<entry_barrier>
if no variable or attribute referenced by
the @nt<condition> (directly or indirectly)
has been altered
by the execution (or cancellation) of a protected procedure or entry call
on the object since the @nt<condition> was last evaluated.
@begin(Ramification)
  Changes to variables referenced by an entry barrier that
  result from actions outside of a protected procedure or entry call on the
  protected object need not be "noticed."  For example, if
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
  In other words, any side-effects of evaluating an entry barrier
  should be innocuous, since an entry barrier might be evaluated more
  or less often than is implied by the "official" dynamic semantics.
@end(Ramification)
@begin(ImplNote)
  It is anticipated that when the number of entries is known to be small,
  all barriers will be evaluated any time one of them needs to be,
  to produce an "entry-open bit-vector."  The appropriate bit will
  be tested when the entry is called, and only if the bit is false
  will a check be made to see whether the bit-vector might need to
  be recomputed.  This should allow an implementation to maximize
  the performance of a call on an open entry, which seems like the
  most important case.

  In addition to the entry-open bit-vector, an "is-valid"
  bit is needed per object, which indicates whether the current
  bit-vector setting is valid.
  A "depends-on-Count-attribute" bit is needed per type.
  The "is-valid" bit is set to false
  (as are all the bits of the bit-vector) when the protected object is first
  created, as well as any time an exception is propagated from computing
  the bit-vector.  Is-valid would also be set false any time the
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
  corresponding entry is checked to see if it is open.  It isn't worth
  saving the state of the entry between checks, because of the space
  that would be required.  Furthermore, the entry queues probably want
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
  of control that initiated the cancellation.  For example,
  if the cancellation is due to the expiration of a delay,
  it is unlikely that the handler of the timer interrupt could
  perform the necessary protected action itself (due to being
  on the interrupt level).  Similarly, if the cancellation
  is due to an abort, it is possible that the task initiating
  the abort has a priority higher than the ceiling priority of the
  protected object (for implementations that support ceiling priorities).
  Similar considerations could apply in a multiprocessor situation.
@end{Reason}

@end{ImplPerm}

@begin{NotesNotes}

If an exception is raised during the execution of an @nt{entry_body}, it is
propagated to the corresponding caller (@lSeeSecNum(Exception Handling)).

For a call on a protected entry, the entry is checked to see if
it is open prior to queuing the call, and again thereafter
if its Count attribute (@lSeeSecNum{Task and Entry Attributes})
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
(@lSeeSecNum(Timed Entry Calls), @RefSecNum(Conditional Entry Calls),
and @lSeeSecNum(Asynchronous Transfer of Control)).
@begin{Ramification}
A task can call its own entries, but
the task will deadlock if the call is a simple entry call.
@end{Ramification}

The @nt<condition> of an @nt<entry_barrier> is allowed to be evaluated by
an implementation more often than strictly necessary, even if the
evaluation might have side effects.  On the other hand, an implementation
need not reevaluate the @nt<condition> if nothing it references was
updated by an intervening protected action on the protected object,
even if the @nt<condition> references some global variable that might
have been updated by an action performed from outside of a protected action.
@end{NotesNotes}

@begin{Examples}
@i{Examples of entry calls:}
@begin{Example}
Agent.Shut_Down;                      @i[--  @lSeeSecNum(Task Units and Task Objects)]
Parser.Next_Lexeme(E);                @i[--  @lSeeSecNum(Task Units and Task Objects)]
Pool(5).Read(Next_Char);              @i[--  @lSeeSecNum(Task Units and Task Objects)]
Controller.Request(Low)(Some_Item);   @i[--  @lSeeSecNum(Task Units and Task Objects)]
Flags(3).Seize;                       @i[--  @lSeeSecNum(Protected Units and Protected Objects)]
@end{Example}
@end{Examples}

@LabeledSubSection{Requeue Statements}

@begin{Intro}
@begin{Redundant}
A @nt<requeue_statement>
can be used to complete an @nt<accept_statement> or @nt<entry_body>,
while redirecting the corresponding entry call to a new (or the same)
entry queue.
@Defn{requeue}
Such a @i(requeue) can be performed with or without allowing
an intermediate cancellation of the call, due to an abort or
the expiration of a delay.
@IndexSee{Term=[preference control],See=(requeue)}
@IndexSee{Term=[broadcast signal],See=(requeue)}
@end{Redundant}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<requeue_statement>,
  rhs="@key{requeue} @SynI{entry_}@Syn2{name} [@key{with} @key{abort}];"}
@end{Syntax}

@begin{Resolution}

@Defn2{Term=[target entry], Sec=(of a @nt<requeue_statement>)}
The @i(entry_)@nt{name} of a @nt{requeue_statement} shall resolve
to denote an entry (the @i(target entry))
that either has no parameters, or that has
a profile that is type conformant (@lSeeSecNum(Conformance Rules)) with
the profile of the innermost enclosing @nt<entry_body> or
@nt<accept_statement>.
@Defn2{Term=[type conformance],Sec=(required)}

@end{Resolution}

@begin{Legality}

A @nt{requeue_statement} shall be within a callable
construct that is either an @nt{entry_body} or an
@nt{accept_statement}, and this construct shall
be the innermost enclosing body or callable construct.

If the target entry has parameters,
then its profile shall be subtype conformant with
the profile of the innermost enclosing callable construct.
@Defn2{Term=[subtype conformance],Sec=(required)}

@PDefn2{Term=[accessibility rule],Sec=(requeue statement)}
@oBigChg{}
In a @nt<requeue_statement> of an @nt<accept_statement> of
some task unit, either the target object shall be a part of a
formal parameter of the @nt<accept_statement>,
or the accessibility level of the target object
shall not be equal to or statically deeper than any
enclosing @nt<accept_statement> of the task unit.
In a @nt<requeue_statement> of an @nt<entry_body>
of some protected unit, either the target object shall be
a part of a formal parameter of the @nt<entry_body>,
or the accessibility level of the target object
shall not be statically deeper than that
of the @nt<entry_declaration>.
@oEndBigChg{}
@oChgRef{94-4715.a}
@oChgRef{94-4828.c}
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
  the outer @nt<accept_statement>.  This is not
  a problem because all calls on the inner task have to complete before
  returning from the outer @nt<accept_statement>, meaning no "dangling
  calls" will be created.
@end(Reason)
@begin(ImplNote)
  By disallowing certain requeues,
  we ensure that the normal @nt<terminate_alternative> rules remain
  sensible, and that explicit clearing of the entry queues of a protected
  object during finalization is rarely necessary.  In particular, such
  clearing of the entry queues is @oChg{}necessary only@oEndChg{} (ignoring premature
  Unchecked_Deallocation) for protected objects declared in a
  @nt<task_body> (or created by an allocator for an access type declared
  in such a body) containing one or more @nt<requeue_statement>s.
  Protected objects declared in subprograms, or at the library level,
  will never need to have their entry queues explicitly cleared during
  finalization.
@end(ImplNote)
@end{Legality}

@begin{RunTime}

@PDefn2{Term=[execution], Sec=(requeue_statement)}
The execution of a @nt{requeue_statement} proceeds by first evaluating the
@i(entry_)@nt<name>@Redundant[, including the @nt<prefix>
identifying the target task
or protected object and the @nt<expression>
identifying the entry
within an entry family, if any].
The @nt{entry_body} or @nt{accept_statement}
enclosing the @nt{requeue_statement} is then
completed@Redundant[, finalized, and left
(@lSeeSecNum(Completion and Finalization))].

@PDefn2{Term=[execution], Sec=(requeue task entry)}
For the execution of a requeue on an entry of a target task,
after leaving the enclosing callable construct, the named entry
is checked to see if it is open and the requeued call is either
selected immediately or queued, as for a normal entry call
(@lSeeSecNum(Entry Calls)).

@PDefn2{Term=[execution], Sec=(requeue protected entry)}
For the execution of a requeue on an entry of a target protected
object, after leaving the enclosing callable construct:
@begin(Itemize)
  if the requeue is an internal requeue
  (that is, the requeue is back on an entry of the same protected object @em
  @lSeeSecNum(Intertask Communication)),
  the call is added to the queue of the named entry and
  the ongoing protected action continues (@lSeeSecNum(Protected Subprograms and Protected Actions));
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
  @lSeeSecNum(Intertask Communication)),
  a protected action is started on the target object and proceeds
  as for a normal entry call (@lSeeSecNum(Entry Calls)).
@end(Itemize)

If the new entry named in the @nt<requeue_statement>
has formal parameters, then during the execution of the
@nt<accept_statement> or @nt<entry_body> corresponding to the new entry,
the formal parameters denote the same objects as
did the corresponding formal parameters
of the callable construct completed by the requeue.
@Redundant[In any case, no parameters are specified in a
@nt<requeue_statement>; any parameter passing is implicit.]

@Defn{requeue-with-abort}
If the @nt<requeue_statement> includes the reserved words @key(with abort)
(it is a @i(requeue-with-abort)), then:
@begin(Itemize)
  if the original entry call has been aborted
  (@lSeeSecNum(Abort of a Task - Abort of a Sequence of Statements)), then
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
  call has begun.  This also means that in the absence of @key(with abort)
  the usual Ada 83 behavior is preserved, namely that once an
  entry call is accepted, it cannot be cancelled until it completes.
@end(Reason)
@end{RunTime}

@begin{NotesNotes}

A requeue is permitted from a single entry to an entry of
an entry family, or vice-versa.  The entry index, if any,
plays no part in the subtype conformance check between the
profiles of the two entries; an entry index
is part of the @i(entry_)@nt<name> for an entry of a family.
@PDefn{subtype conformance}

@end{NotesNotes}

@begin{Examples}
@i{Examples of requeue statements:}
@begin{Example}
@key[requeue] Request(Medium) @key[with abort];
                    @i[-- requeue on a member of an entry family of the current task, @lSeeSecNum{Task Units and Task Objects}]

@key[requeue] Flags(I).Seize;
                    @i[-- requeue on an entry of an array component, @lSeeSecNum{Protected Units and Protected Objects}]
@end{Example}
@end{Examples}

@begin{Extend83}
The @nt<requeue_statement> is new.
@end{Extend83}

@LabeledSection{Delay Statements, Duration, and Time}

@begin{Intro}
@begin(Redundant)
@PDefn{expiration time}
A @nt<delay_statement> is used to block further execution until
a specified @i(expiration time) is reached.
The expiration time
can be specified either as a particular point in time (in a
@nt<delay_until_statement>), or in seconds from the current time
(in a @nt<delay_relative_statement>).
The language-defined
package Calendar provides definitions for a type Time and associated
operations, including a function Clock that returns the current time.
@IndexSee{Term=[timing],See=(delay_statement)}
@end(Redundant)
@end{Intro}

@begin{Syntax}
@Syn{lhs=<delay_statement>,
  rhs="@Syn2{delay_until_statement} | @Syn2{delay_relative_statement}"}
@Hinge{}

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

@Defn{time type}
@Defn{time base}
@Defn{clock}
There can be multiple time bases, each with a corresponding
clock, and a corresponding @i{time type}.
The type of the @i(delay_)@nt<expression>
in a @nt{delay_until_statement} shall be a time type @em either the
type Time defined in the language-defined package Calendar (see below),
or some other implementation-defined time type
(@lSeeSecNum(Monotonic Time)).
@ImplDef{Any implementation-defined time types.}
@end{Legality}

@begin{StaticSem}
@Redundant[There is a predefined fixed point type
named Duration, declared in the visible part of package Standard;]
a value of type Duration is used to represent the length
of an interval of time, expressed in seconds.
@Redundant[The type Duration is not specific to a particular time base,
but can be used with any time base.]

A value of the type Time in package Calendar, or of some other
implementation-defined time type, represents a time as reported
by a corresponding clock.

The following language-defined library package exists:
@begin{Example}
@ChildUnit{Parent=[Ada],Child=[Calendar],Expanded=[Ada.Calendar]}
@key(package) Ada.Calendar @key(is)
@LangDefType{Package=[Ada.Calendar],Type=[Time]}
  @key(type) Time @key(is) @key(private;)

  @key(subtype) Year_Number  @key(is) Integer @key(range) 1901 ..  2099;
  @key(subtype) Month_Number @key(is) Integer @key(range) 1 ..  12;
  @key(subtype) Day_Number   @key(is) Integer @key(range) 1 ..  31;
  @key(subtype) Day_Duration @key(is) Duration @key(range) 0.0 ..  86_400.0;
@Hinge()

  @key(function) Clock @key(return) Time;

  @key(function) Year   (Date : Time) @key(return) Year_Number;
  @key(function) Month  (Date : Time) @key(return) Month_Number;
  @key(function) Day    (Date : Time) @key(return) Day_Number;
  @key(function) Seconds(Date : Time) @key(return) Day_Duration;
@Hinge()

  @key(procedure) Split (Date  : @key(in) Time;
                   Year    : @key(out) Year_Number;
                   Month   : @key(out) Month_Number;
                   Day     : @key(out) Day_Number;
                   Seconds : @key(out) Day_Duration);
@Hinge()

  @key(function) Time_Of(Year  : Year_Number;
                   Month   : Month_Number;
                   Day     : Day_Number;
                   Seconds : Day_Duration := 0.0)
   @key(return) Time;
@Hinge()

  @key(function) "+" (Left : Time;   Right : Duration) @key(return) Time;
  @key(function) "+" (Left : Duration; Right : Time) @key(return) Time;
  @key(function) "-" (Left : Time;   Right : Duration) @key(return) Time;
  @key(function) "-" (Left : Time;   Right : Time) @key(return) Duration;
@Hinge()

  @key(function) "<" (Left, Right : Time) @key(return) Boolean;
  @key(function) "<="(Left, Right : Time) @key(return) Boolean;
  @key(function) ">" (Left, Right : Time) @key(return) Boolean;
  @key(function) ">="(Left, Right : Time) @key(return) Boolean;

  Time_Error : @key(exception;)

@key(private)
   ... -- @i{not specified by the language}
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

@PDefn2{Term=blocked, Sec=(on a @nt<delay_statement>)}
The task executing a @nt<delay_statement> is blocked
until the expiration time is reached, at which point it
becomes ready again.  If the expiration time
has already passed, the task is not blocked.
@begin(Discussion)
  For a @nt<delay_relative_statement>, this case corresponds to
  when the value of the @i(delay_)@nt<expression> is zero
  or negative.

  Even though the task is not blocked,
  it might be put back on the end of its ready queue.
  @SeeSec(Priority Scheduling).
@end(Discussion)

@Defn2{Term=[cancellation], Sec=(of a @nt<delay_statement>)}
If an attempt is made to @i(cancel) the @nt<delay_statement>
@Redundant[(as part of an @nt<asynchronous_select> or abort @em
@lSeeSecNum{Asynchronous Transfer of Control} and
@RefSecNum{Abort of a Task - Abort of a Sequence of Statements})],
the @nt<_statement> is cancelled if
the expiration time has not yet passed,
thereby completing the @nt<delay_statement>.
@begin(Reason)
  This is worded this way so that in an @nt<asynchronous_select>
  where the @nt<triggering_statement> is a @nt<delay_statement>,
  an attempt to cancel the delay when the @nt<abortable_part> completes
  is ignored if the expiration time has already passed, in which case the
  optional statements of the @nt<triggering_alternative> are executed.
@end(Reason)

The time base associated with the type Time of package Calendar
is implementation defined.
@ImplDef{The time base of the type Calendar.Time.}
The function Clock of package Calendar
returns a value representing the current time for this
time base.
@Redundant[The implementation-defined value of the
named number System.Tick (@lSeeSecNum(The Package System))
is an approximation of the length of the real-time
interval during which the value of Calendar.Clock remains constant.]

The functions Year,
Month, Day, and Seconds return the corresponding values for
a given value of the type Time,
as appropriate to an implementation-defined timezone;
the procedure Split returns all four
corresponding values.  Conversely, the function Time_Of combines a
year number, a month number, a day number, and a duration, into
a value of type Time.  The operators "+" and "@en@;" for addition
and subtraction of times and durations, and the relational operators
for times, have the conventional meaning.
@ImplDef{The timezone used for package Calendar operations.}

If Time_Of is called with a seconds value of 86_400.0, the value
returned is equal to the value of Time_Of for the next day
with a seconds value of 0.0.
The value returned by the function
Seconds or through the Seconds parameter of the procedure
Split is always less than 86_400.0.

The exception Time_Error is raised by the function Time_Of if the
actual parameters do not form a proper date.  This exception
is also raised by the operators "+" and "@en@;" if the
result is not representable in the type Time or
Duration, as appropriate.  This exception is
also raised by the function Year or the procedure
Split if the year number of the given date is outside of
the range of the subtype Year_Number.
@begin(Honest)
  By "proper date" above we mean that the given year has
  a month with the given day.  For example, February 29th is
  a proper date only for a leap year.
@end(Honest)
@begin(Reason)
  We allow Year and Split to raise Time_Error because the
  arithmetic operators are allowed (but not required) to produce times
  that are outside the range of years from 1901 to 2099.
  This is similar to the way integer operators may return
  values outside the base range of their type
  so long as the value is mathematically correct.
@end(Reason)

@end{RunTime}

@begin{ImplReq}

The implementation of the
type Duration shall allow representation of
time intervals (both positive and negative) up to at least 86400 seconds (one
day); Duration'Small shall not be greater than twenty milliseconds.
The implementation of the type Time shall allow representation of
all dates with year numbers in the range of Year_Number@Redundant[; it
may allow representation of other dates as well (both earlier and later).]

@end{ImplReq}

@begin{ImplPerm}

An implementation may define additional time
types (@lSeeSecNum{Monotonic Time}).

An implementation may raise Time_Error if the
value of a @i{delay_}@nt<expression> in a @nt<delay_until_statement>
of a @nt<select_statement> represents a time more than 90 days past the
current time.  The actual limit, if any, is implementation-defined.
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
  range of @Math{@PorM} 2.0**17 @em that is, 131_072.0.
@end(ImplNote)

The time base for @nt{delay_relative_statement}s should be
monotonic;
it need not be the same time base as used for Calendar.Clock.

@end{ImplAdvice}

@begin{NotesNotes}

A @nt{delay_relative_statement} with a negative value of the
@i(delay_)@nt<expression> is equivalent to one with a zero value.

A @nt{delay_statement} may be executed by the environment task;
consequently @nt{delay_statement}s may be executed as part of
the elaboration of a @nt{library_item} or the execution of the main subprogram.
Such statements delay the environment task (@lSeeSecNum(Program Execution)).

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

@end{NotesNotes}

@begin{Examples}
@i{Example of a relative delay statement:}
@begin{example}
@key(delay) 3.0;  @i[-- delay 3.0 seconds]
@end{example}

@Defn2{Term=[periodic task],Sec=(example)}
@IndexSee{Term=[periodic task],See=(delay_until_statement)}
@i{Example of a periodic task:}
@begin{example}
@key(declare)
   @key(use) Ada.Calendar;
   Next_Time : Time := Clock + Period;
                      @i[-- Period is a global constant of type Duration]
@key(begin)
   @key(loop)               @i[-- repeated every Period seconds]
      @key(delay) @key(until) Next_Time;
      ... @i[-- perform some actions]
      Next_Time := Next_Time + Period;
   @key(end) @key(loop;)
@key(end;)
@end{example}
@end{Examples}

@begin{Inconsistent83}
For programs that raise Time_Error on "+" or "@en@;" in Ada 83,the exception
might be deferred until a call on Split or Year_Number, or might
not be raised at all (if the offending time is never Split after being
calculated).  This should not affect typical programs,
since they deal only with times corresponding to the relatively
recent past or near future.
@end{Inconsistent83}

@begin{Extend83}

The syntax rule for @nt{delay_statement} is modified to allow
@nt{delay_until_statement}s.

The type Time may represent dates with year numbers outside of Year_Number.
Therefore, the operations "+" and
"@en@;" need only raise Time_Error if the result is not representable
in Time (or Duration); also, Split or Year will now raise Time_Error
if the year number is outside of Year_Number.
This change is intended to simplify the implementation
of "+" and "@en@;" (allowing them to depend on overflow for
detecting when to raise Time_Error) and to allow local
timezone information to be
considered at the time of Split rather than Clock (depending on
the implementation approach).  For example, in a POSIX environment,
it is natural for the type Time to be based on GMT, and
the results of procedure Split (and the functions
Year, Month, Day, and Seconds) to depend on local time zone information.
In other environments, it is more natural for the type Time to
be based on the local time zone, with the results of
Year, Month, Day, and Seconds being pure functions of their input.

We anticipate that implementations will provide child packages
of Calendar to provide more explicit control over time zones
and other environment-dependent time-related issues.
These would be appropriate for standardization in a given
environment (such as POSIX).
@end{Extend83}

@LabeledSection{Select Statements}

@begin{Intro}
@begin{Redundant}
There are four forms of the @nt{select_statement}.  One form provides a
selective wait for one or more @nt{select_alternative}s.  Two provide
timed and conditional entry calls.  The fourth provides asynchronous
transfer of control.
@end{Redundant}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<select_statement>,rhs="
   @Syn2{selective_accept}
  | @Syn2{timed_entry_call}
  | @Syn2{conditional_entry_call}
  | @Syn2{asynchronous_select}"}
@Hinge{}
@end{Syntax}

@begin{Examples}
@i{Example of a select statement:}
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
@nt{Asynchronous_select} is new.
@end{Extend83}

@LabeledSubSection{Selective Accept}

@begin{Intro}
@begin{Redundant}
This form of the @nt{select_statement} allows a combination of waiting for,
and selecting from, one or more alternatives.  The
selection may depend on conditions associated with each alternative of the
@nt{selective_accept}.
@IndexSee{Term=[time-out],See=(selective_accept)}
@end{Redundant}
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
@Hinge{}

@Syn{lhs=<select_alternative>,rhs="
   @Syn2{accept_alternative}
  | @Syn2{delay_alternative}
  | @Syn2{terminate_alternative}"}

@hinge()
@Syn{lhs=<accept_alternative>,rhs="
  @Syn2{accept_statement} [@Syn2{sequence_of_statements}]"}

@Syn{lhs=<delay_alternative>,rhs="
  @Syn2{delay_statement} [@Syn2{sequence_of_statements}]"}

@Syn{lhs=<terminate_alternative>,rhs="@key{terminate};"}

@begin(SyntaxText)
A @nt{selective_accept} shall contain at least one @nt{accept_alternative}.
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
then all shall be @nt<delay_relative_statement>s,
or all shall be @nt<delay_until_statement>s for the same time type.
@begin{Reason}
  This simplifies the implementation and the description of the semantics.
@end{Reason}

@end{Legality}

@begin{RunTime}

@Defn{open alternative}
A @nt<select_alternative> is said to be @i(open) if
it is not immediately preceded by a @nt<guard>, or if
the @nt<condition> of its @nt<guard> evaluates to True.  It
is said to be @i(closed) otherwise.

@PDefn2{Term=[execution], Sec=(selective_accept)}
For the execution of a @nt{selective_accept}, any @nt{guard}
@nt{condition}s are evaluated; open alternatives are
thus determined.  For an open @nt{delay_alternative}, the
@i(delay_)@nt<expression> is also evaluated.  Similarly, for an open
@nt{accept_alternative} for
an entry of a family, the @nt{entry_index} is also evaluated.
These evaluations are performed in an arbitrary order, except that
a @i(delay_)@nt<expression> or @nt<entry_index> is not evaluated until
after evaluating the corresponding @nt<condition>, if any.
Selection and execution of one open alternative, or of the else part, then
completes the execution of the @nt{selective_accept}; the rules for
this selection are described below.

Open @nt{accept_alternative}s are first considered.  Selection of one such
alternative takes place immediately if the corresponding
entry already has queued calls.  If several alternatives
can thus be selected, one of them is selected according to the
entry queuing policy in effect (@lSeeSecNum(Entry Calls) and
@RefSecNum(Entry Queuing Policies)).
When such an
alternative is selected, the selected call is
removed from its entry queue and the @nt<handled_sequence_of_statements>
(if any) of the corresponding @nt{accept_statement} is executed; after the
rendezvous completes any subsequent @nt<sequence_of_statements>
of the alternative is executed.
@PDefn2{Term=[blocked], Sec=(execution of a @nt<selective_accept>)}
If no selection is immediately possible (in the above sense)
and there is no else part, the task
blocks until an open alternative can be selected.

Selection of the other forms of alternative or of an else part is performed
as follows:
@begin{itemize}

An open @nt{delay_alternative} is selected when
its expiration time is reached if no @nt{accept_alternative}
or other @nt<delay_alternative> can be selected prior to the
expiration time.  If several
@nt{delay_alternative}s have this same expiration time,
one of them is selected according to the queuing policy in
effect (@lSeeSecNum{Entry Queuing Policies}); the default queuing
policy chooses arbitrarily among the @nt<delay_alternative>s
whose expiration time has passed.

The else part is selected and its @nt<sequence_of_statements> is executed
if no @nt{accept_alternative} can immediately be selected;
in particular, if all alternatives are closed.

An open @nt{terminate_alternative} is selected if the conditions stated at the
end of clause @RefSecNum{Task Dependence - Termination of Tasks}
are satisfied.
@begin(Ramification)
  In the absence of a @nt<requeue_statement>, the conditions stated
  are such that a @nt<terminate_alternative> cannot be selected while
  there is a queued entry call for any entry of the task.
  In the presence of requeues from a task to one of its subtasks,
  it is possible that when a @nt<terminate_alternative> of the
  subtask is selected, requeued calls (for closed entries only) might still
  be queued on some entry of the subtask.  Tasking_Error will
  be propagated to such callers, as is usual when a task completes
  while queued callers remain.
@end(Ramification)

@end{itemize}

@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
The exception Program_Error is raised if all alternatives are closed and
there is no else part.

@end{RunTime}

@begin{NotesNotes}

A @nt{selective_accept} is allowed to have several open
@nt{delay_alternative}s.  A @nt{selective_accept} is allowed
to have several open
@nt{accept_alternative}s for the same entry.

@end{NotesNotes}

@begin{Examples}
@i{Example of a task body with a selective accept:}
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
         @key(exit);       @i[-- Premature shut down requested]
      @key(or)
         @key(terminate);  @i[-- Normal shutdown at end of scope]
      @key(end) @key(select);
   @key(end) @key(loop);
@key(end) Server;
@end{Example}
@end{Examples}

@begin{DiffWord83}
The name of @nt{selective_wait} was changed to @nt{selective_accept} to
better describe what is being waited for.
We kept @nt{select_alternative} as is, because
@nt<selective_accept_alternative> was too easily confused
with @nt<accept_alternative>.
@end{DiffWord83}

@LabeledSubSection{Timed Entry Calls}

@begin{Intro}
@begin{Redundant}
A @nt{timed_entry_call} issues an entry call that is
cancelled if the call (or a requeue-with-abort of the call)
is not selected before the expiration time is reached.
@IndexSee{Term=[time-out],See=(timed_entry_call)}
@end{Redundant}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<timed_entry_call>,rhs="
  @key{select}
   @Syn2{entry_call_alternative}
  @key{or}
   @Syn2{delay_alternative}
  @key{end select};"}

@Syn{lhs=<entry_call_alternative>,rhs="
  @Syn2{entry_call_statement} [@Syn2{sequence_of_statements}]"}
@end{Syntax}

@begin{RunTime}

@PDefn2{Term=[execution], Sec=(timed_entry_call)}
For the execution of a @nt{timed_entry_call}, the @i(entry_)@nt<name>
and the actual parameters are evaluated, as for a simple entry call
(@lSeeSecNum(Entry Calls)).
The expiration time
(@lSeeSecNum(Delay Statements))
for the call is determined by evaluating
the @i(delay_)@nt<expression> of the
@nt<delay_alternative>;
the entry call is then issued.

If the call is queued (including due to
a requeue-with-abort),
and not selected before the expiration
time is reached, an attempt to cancel the call is made.
If the call completes due to the cancellation, the optional
@nt<sequence_of_statements> of the @nt<delay_alternative> is
executed; if the entry call completes normally, the optional
@nt<sequence_of_statements> of the @nt<entry_call_alternative> is
executed.
@begin{Ramification}
The fact that the syntax calls for an @nt{entry_call_statement} means
that this fact is used in overload resolution.
For example,
if there is a procedure X and an entry X (both with no parameters),
then "select X; ..." is legal,
because overload resolution knows that the entry is the one that was
meant.
@end{Ramification}

@end{RunTime}

@begin{Examples}
@i{Example of a timed entry call:}
@begin{Example}
@key(select)
   Controller.Request(Medium)(Some_Item);
@key(or)
   @key(delay) 45.0;
   @i[--  controller too busy, try something else]
@key(end) @key(select);
@end{Example}
@end{Examples}

@begin{DiffWord83}
This clause comes before the one for Conditional Entry Calls,
so we can define conditional entry calls in terms of timed entry calls.
@end{DiffWord83}

@LabeledSubSection{Conditional Entry Calls}

@begin{Intro}

@Redundant[A @nt{conditional_entry_call} issues an entry call that is
then cancelled if it is not selected immediately (or if a requeue-with-abort
of the call is not selected immediately).]
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

@PDefn2{Term=execution, Sec=(conditional_entry_call)}
The execution of a @nt<conditional_entry_call> is defined to be equivalent
to the execution of a @nt<timed_entry_call> with a @nt<delay_alternative>
specifying an immediate expiration time and the
same @nt<sequence_of_statements> as given after the reserved word @key(else).
@end{RunTime}

@begin{NotesNotes}

A @nt{conditional_entry_call} may briefly increase the Count attribute of
the entry, even if the conditional call is not selected.

@end{NotesNotes}

@begin{Examples}
@i{Example of a conditional entry call:}
@begin{Example}
@key(procedure) Spin(R : @key[in] Resource) @key(is)
@key(begin)
   @key(loop)
      @key(select)
         R.Seize;
         @key(return);
      @key(else)
         @key(null);  @i[--  busy waiting]
      @key(end) @key(select);
   @key(end) @key(loop);
@key(end);
@end{Example}
@end{Examples}

@begin{DiffWord83}
This clause comes after the one for Timed Entry Calls,
so we can define conditional entry calls in terms of timed
entry calls.
We do that so that an "expiration time" is defined for both,
thereby simplifying the definition of what happens on
a requeue-with-abort.
@end{DiffWord83}

@LabeledSubSection{Asynchronous Transfer of Control}

@begin{Intro}
@begin{Redundant}
An asynchronous @nt{select_statement} provides
asynchronous transfer of control
upon completion of an entry call or the expiration of a delay.
@end{Redundant}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<asynchronous_select>,rhs="
  @key{select}
   @Syn2{triggering_alternative}
  @key{then abort}
   @Syn2{abortable_part}
  @key{end select};"}

@Syn{lhs=<triggering_alternative>,rhs="@Syn2{triggering_statement} [@Syn2{sequence_of_statements}]"}
@Hinge{}

@Syn{lhs=<triggering_statement>,rhs="@Syn2{entry_call_statement} | @Syn2{delay_statement}"}

@Syn{lhs=<abortable_part>,rhs="@Syn2{sequence_of_statements}"}
@end{Syntax}

@begin{RunTime}

@PDefn2{Term=[execution],
  Sec=(asynchronous_select with an entry call trigger)}
For the execution of an @nt{asynchronous_select}
whose @nt<triggering_statement> is an @nt<entry_call_statement>,
the @i(entry_)@nt<name> and actual parameters are evaluated as
for a simple entry call (@lSeeSecNum(Entry Calls)), and the
entry call is issued.
If the entry call is queued (or requeued-with-abort),
then the @nt<abortable_part> is executed.
@Redundant[If the entry call is selected immediately,
and never requeued-with-abort,
then the @nt<abortable_part> is never started.]

@PDefn2{Term=[execution],
  Sec=(asynchronous_select with a delay_statement trigger)}
For the execution of an @nt<asynchronous_select> whose
@nt<triggering_statement> is a @nt<delay_statement>,
the @i(delay_)@nt<expression> is evaluated
and the expiration time is determined,
as for a normal @nt<delay_statement>.
If the expiration time has not already passed, the @nt<abortable_part>
is executed.

If the @nt<abortable_part> completes and is left prior to completion of the
@nt<triggering_statement>,
an attempt to cancel the @nt<triggering_statement> is made.
If the attempt to cancel succeeds (@lSeeSecNum(Entry Calls) and
@RefSecNum(Delay Statements)), the
@nt<asynchronous_select> is complete.

If the @nt<triggering_statement> completes other than
due to cancellation,
the @nt<abortable_part>
is aborted (if started but not yet completed @em
@lSeeSecNum(Abort of a Task - Abort of a Sequence of Statements)).
If the @nt<triggering_statement> completes normally, the optional
@nt<sequence_of_statements> of the @nt<triggering_alternative> is
executed after the @nt<abortable_part> is left.
@begin(Discussion)
  We currently don't specify when the by-copy [@key(in)] @key(out)
  parameters are assigned back into the actuals.  We considered
  requiring that to happen after the @nt<abortable_part> is
  left.  However, that doesn't seem useful enough
  to justify possibly overspecifying the implementation approach,
  since some of the parameters are passed by reference anyway.

  In an earlier description, we required that the @nt<sequence_of_statements>
  of the @nt<triggering_alternative> execute after aborting
  the @nt<abortable_part>, but before waiting for it to complete
  and finalize, to provide more rapid response to the triggering event
  in case the finalization was unbounded.  However, various reviewers felt
  that this created unnecessary complexity in the description,
  and a potential for undesirable concurrency (and nondeterminism)
  within a single task.  We have now reverted to simpler, more
  deterministic semantics,
  but anticipate that further discussion of this issue might be
  appropriate during subsequent reviews.
  One possibility is to leave this area implementation defined,
  so as to encourage experimentation.  The user would then have
  to assume the worst about what kinds of actions are appropriate
  for the @nt<sequence_of_statements> of the @nt<triggering_alternative>
  to achieve portability.
@end(Discussion)

@end{RunTime}

@begin{Examples}
@Defn2{Term=[signal handling], Sec=(example)}
@Defn2{Term=[interrupt],Sec=(example using @nt<asynchronous_select>)}
@Defn2{Term=[terminal interrupt], Sec=(example)}
@i(Example of a main command loop for a command interpreter:)
@begin(Example)
@key(loop)
    @key(select)
        Terminal.Wait_For_Interrupt;
        Put_Line("Interrupted");
    @key(then abort)
        -- @i(This will be abandoned upon terminal interrupt)
        Put_Line("-> ");
        Get_Line(Command, Last);
        Process_Command(Command(1..Last));
    @key(end) @key(select);
@key(end) @key(loop);
@end(Example)

@i(Example of a time-limited calculation:)
@IndexSee{Term=[time-out],See=(asynchronous_select)}
@Defn2{Term=[time-out],Sec=(example)}
@Defn2{Term=[time limit],Sec=(example)}
@Defn2{Term=[interrupt],Sec=(example using @nt<asynchronous_select>)}
@Defn2{Term=[timer interrupt],Sec=(example)}
@begin(Example)
@key(select)
   @key(delay) 5.0;
   Put_Line("Calculation does not converge");
@key(then abort)
   -- @i(This calculation should finish in 5.0 seconds;)
   -- @i( if not, it is assumed to diverge.)
   Horribly_Complicated_Recursive_Function(X, Y);
@key(end) @key(select);
@end(Example)

@end{Examples}

@begin{Extend83}
@nt<Asynchronous_select> is new.
@end{Extend83}

@LabeledSection{Abort of a Task - Abort of a Sequence of Statements}

@begin{Intro}
@begin{Redundant}
An @nt{abort_statement} causes one or more tasks to become abnormal, thus
preventing any further interaction with such tasks.  The completion
of the @nt<triggering_statement> of an @nt<asynchronous_select>
causes a @nt{sequence_of_statements} to be aborted.
@end{Redundant}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<abort_statement>,
  rhs="@key{abort} @SynI{task_}@Syn2{name} {, @SynI{task_}@Syn2{name}};"}
@end{Syntax}

@begin{Resolution}

@PDefn2{Term=[expected type], Sec=(abort_statement task_name)}
Each @SynI{task_}@nt{name} is expected to be of any task type@Redundant[;
they need not all be of the same task type.]

@end{Resolution}

@begin{RunTime}

@PDefn2{Term=[execution], Sec=(abort_statement)}
For the execution of an @nt<abort_statement>, the given @i(task_)@nt<name>s
are evaluated in an arbitrary order.
@Defn2{Term=abort, Sec=(of a task)}
@Defn{abnormal task}
@PDefn2{Term=[task state], Sec=(abnormal)}
Each named task is
then @i(aborted), which consists of making the task @i(abnormal)
and aborting the execution of the corresponding @nt<task_body>,
unless it is already completed.
@begin{Ramification}
Note that aborting those tasks is not defined to be an
abort-deferred operation.
Therefore, if one of the named tasks is the task executing the
@nt{abort_statement}, or if the task executing the
@nt{abort_statement} depends on one of the named tasks,
then it is possible for the execution of the @nt{abort_statement} to be
aborted, thus leaving some of the tasks unaborted.
This allows the implementation to use either a sequence of calls to an
``abort task'' RTS primitive, or
a single call to an ``abort list of tasks'' RTS primitive.
@end{Ramification}

@PDefn2{Term=[execution], Sec=(aborting the execution of a construct)}
@Defn2{Term=[abort], Sec=(of the execution of a construct)}
When the execution of a construct
is @i(aborted) (including that of a @nt<task_body> or of a
@nt<sequence_of_statements>), the execution of every construct
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
@RefSecNum(User-Defined Assignment and Finalization).]
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


When @oBigChg{}a@oEndBigChg{} master is aborted, all tasks
that depend on that master are aborted.
@oChgRef{94-4715.a}

@PDefn{unspecified}
The order in which tasks become abnormal as the result
of an @nt<abort_statement> or the abort of a @nt<sequence_of_statements>
is not specified by the language.

If the execution of an entry call is aborted,
an immediate attempt is made to cancel the entry call
(@lSeeSecNum(Entry Calls)).
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
  or of the @nt<sequence_of_statements> of an @nt<exception_handler>.
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
An attempt to execute an @nt<asynchronous_select> as
part of the execution of an abort-deferred operation is a bounded error.
Similarly, an attempt to create a task that depends on a master
@oBigChg{}that@oEndBigChg{} is included entirely within the execution of
an abort-deferred operation is a bounded error.
@oChgRef{94-4715.a}
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
In both cases, Program_Error is raised if the error is detected
by the implementation; otherwise the operations proceed
as they would outside an abort-deferred operation, except
that an abort of the @nt<abortable_part>
or the created task might or might not have an effect.
@begin(Reason)
  An @nt<asynchronous_select> relies on an abort of the
  @nt<abortable_part> to effect the
  asynchronous transfer of control.  For an @nt<asynchronous_select>
  within an abort-deferred operation, the abort might
  have no effect.

  Creating a task dependent on a master included within an abort-deferred
  operation is considered an error, because such tasks could be aborted while
  the abort-deferred operation was still progressing, undermining the
  purpose of abort-deferral.  Alternatively, we could say that such
  tasks are abort-deferred for their entire execution, but that seems
  too easy to abuse.  Note that task creation is already a bounded error
  in protected actions, so this additional rule only applies to local task
  creation as part of Initialize, Finalize, or Adjust.
@end(Reason)
@end{Bounded}

@begin{Erron}
@PDefn{normal state of an object}
@PDefn{abnormal state of an object}
@Defn{disruption of an assignment}
If an assignment operation completes prematurely due to an abort,
the assignment is said to be @i{disrupted};
the target of the assignment or its parts can become abnormal,
and certain subsequent uses of the object can be erroneous,
as explained in @RefSecNum{Data Validity}.
@end{Erron}

@begin{NotesNotes}

An @nt{abort_statement} should be used only in situations
requiring unconditional termination.

A task is allowed to abort any task it can name, including itself.

Additional requirements associated with abort
are given in @RefSec(Preemptive Abort).
@end{NotesNotes}

@begin{DiffWord83}
This clause has been rewritten to accommodate the concept
of aborting the execution of a construct, rather than just of a task.
@end{DiffWord83}

@LabeledSection{Task and Entry Attributes}

@begin{RunTime}

For @PrefixType{a @nt<prefix> T that is of a task type
@Redundant[(after any implicit dereference)]},
the following attributes are defined:
@begin{Description}
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
                terminated, and False otherwise.  The value of this
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

@begin{NotesNotes}

For the Count attribute, the entry can be either a single entry or an
entry of a family.  The name of the entry or entry
family can be either a @nt<direct_name> or an expanded name.

Within task units, algorithms interrogating the attribute E'Count should
take precautions to allow for the increase of the value of this attribute
for incoming entry calls, and its decrease, for example with
@nt{timed_entry_calls}.  Also, a @nt{conditional_entry_call} may briefly
increase this value, even if the conditional call is not accepted.

Within protected units, algorithms interrogating the attribute E'Count
in the @nt<entry_barrier> for the entry E should take precautions to
allow for the evaluation of the @nt<condition> of the barrier both before
and after queuing a given caller.
@end{NotesNotes}

@LabeledSection{Shared Variables}

@begin{StaticSem}
@Defn2{Term=[shared variable], Sec=(protection of)}
@Defn{independently addressable}
If two different objects, including nonoverlapping
parts of the same object, are @i{independently addressable},
they can be manipulated concurrently by two different tasks
without synchronization.
Normally, any two nonoverlapping objects are independently addressable.
However, if packing, record layout, or Component_Size
is specified for a given composite object,
then it is implementation defined whether or not
two nonoverlapping parts of that composite object
are independently addressable.
@ImplDef{Whether or not two nonoverlapping parts of a composite
object are independently addressable,
in the case where packing, record layout, or Component_Size
is specified for the object.}
@begin{ImplNote}
Independent addressability is the only high level semantic effect of
a @nt{pragma} Pack.
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

Even if packing or one of the other above-mentioned aspects is specified,
subcomponents should still be updated independently if the
hardware efficiently supports it.
@end{ImplNote}
@end{StaticSem}

@begin{RunTime}
@begin{Redundant}
Separate tasks normally proceed independently and concurrently
with one another.  However, task interactions can be used
to synchronize the actions of two or more tasks to allow,
for example, meaningful communication by the direct updating and
reading of variables shared between the tasks.
@end{Redundant}
The actions of two different tasks are synchronized in this
sense when an
action of one task @i(signals) an action of the other task;
@Defn2{Term=signal, Sec=(as defined between actions)}
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

  If A1 is the action of issuing an entry call, and A2 is
  part of the corresponding execution of the appropriate
  @nt<entry_body> or @nt<accept_statement>.
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
    for one action to ``signal'' a second, the second action has to follow
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

  If A1 signals some action that in turn signals A2.
@end(Itemize)

@end{RunTime}

@begin{Erron}
Given an action of assigning to an object,
and an action of reading or updating a part of the same object
(or of a neighboring object if the two are not
independently addressable), then the execution of the actions is erroneous
unless the actions are @i(sequential).
@Defn2{Term=sequential, Sec=(actions)}
Two actions are sequential if one of the following is true:
@begin(Itemize)
  One action signals the other;

  Both actions occur as part of the execution of the same task;
  @begin{Reason}
    Any two actions of the same task are sequential, even
    if one does not signal the other because they can be
    executed in an ``arbitrary''
    (but necessarily equivalent to some ``sequential'') order.
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

A @nt{pragma} Atomic or Atomic_Components may also be used to
ensure that certain reads and updates are sequential @em
@lSeeSecNum(Shared Variable Control).
@begin(Ramification)
  If two actions are ``sequential'' it is known that their executions
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
  In Ada 83, this was considered an ``incorrect order dependence'' if
  the ``effect'' of the program was affected, but ``effect'' was never
  fully defined.  In Ada 9X, this situation represents a potential
  nonportability, and a friendly compiler might want to warn the
  programmer about the situation, but it is not considered an error.
  An example where this would come up would be in gathering statistics
  as part of referencing some information, where the assignments
  associated with
  statistics gathering don't need to be ordered since they are
  just accumulating aggregate counts, sums, products, etc.
@end(Discussion)
@end{Erron}

@LabeledSection{Example of Tasking and Synchronization}

@begin{Examples}

The following example defines a buffer protected object
to smooth variations between
the  speed  of  output  of  a producing task and the speed of input of some
consuming  task.   For  instance,  the  producing  task  might have the
following structure:

@begin(Example)
@key(task) Producer;

@key(task body) Producer @key(is)
   Char : Character;
@key(begin)
   @key(loop)
      ... @i[--  produce the next character Char]
      Buffer.Write(Char);
      @key(exit) @key(when) Char = ASCII.EOT;
   @key(end) @key(loop);
@key(end) Producer;
@end(Example)

and the consuming task might have the following structure:

@begin(Example)
@key(task) Consumer;

@key(task body) Consumer @key(is)
   Char : Character;
@key(begin)
   @key(loop)
      Buffer.Read(Char);
      @key(exit) @key(when) Char = ASCII.EOT;
      ... @i[--  consume the character Char]
   @key(end) @key(loop);
@key(end) Consumer;
@end(Example)

The  buffer object contains an internal pool of characters managed in a
round-robin fashion.  The pool has two indices, an  In_Index  denoting  the
space  for the next input character and an Out_Index denoting the space for
the next output character.

@begin(Example)
@key(protected) Buffer @key(is)
   @key(entry) Read (C : @key(out) Character);
   @key(entry) Write(C : @key(in)  Character);
@key(private)
   Pool      : String(1 .. 100);
   Count     : Natural := 0;
   In_Index, Out_Index : Positive := 1;
@key(end) Buffer;

@key(protected body) Buffer @key(is)
   @key(entry) Write(C : @key(in) Character)
      @key(when) Count < Pool'Length @key(is)
   @key(begin)
      Pool(In_Index) := C;
      In_Index := (In_Index @key(mod) Pool'Length) + 1;
      Count    := Count + 1;
   @key(end) Write;

   @key(entry) Read(C : @key(out) Character)
      @key(when) Count > 0 @key(is)
   @key(begin)
      C := Pool(Out_Index);
      Out_Index := (Out_Index @key(mod) Pool'Length) + 1;
      Count     := Count - 1;
   @key(end) Read;
@key(end) Buffer;
@end(Example)

@end{Examples}
