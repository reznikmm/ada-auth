@Comment{ $Source: e:\\cvsroot/ARM/Source/rt.mss,v $ }
@comment{ $Revision: 1.113 $ $Date: 2012/04/03 20:37:03 $ $Author: randy $ }
@Part(realtime, Root="ada.mss")
@Comment{$Date: 2012/04/03 20:37:03 $}

@LabeledNormativeAnnex{Real-Time Systems}

@begin{Intro}
@Defn{real-time systems}
@Defn{embedded systems}
This Annex specifies additional characteristics of Ada implementations
intended for real-time systems software. To conform to this Annex, an
implementation shall also conform to the Systems Programming Annex.
@end{Intro}

@begin{Metrics}

The metrics are documentation requirements; an implementation shall
document the values of the language-defined metrics for at least one
configuration @Redundant[of hardware or an underlying system] supported by
the implementation, and shall document the details of that configuration.
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Values of all @MetricsTitle.]}]}@ChgNote{We're going to document the
individual metrics sections.}
@ChgDocReq{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[The details of the configuration used to generate the values of all
metrics.]}]}
@begin{Reason}
The actual values of the metrics are likely to depend on hardware
configuration details that are variable and generally outside the control
of a compiler vendor.
@end{Reason}

The metrics do not necessarily yield a simple number.
@Redundant[For some, a range is more suitable, for others a formula
dependent on some parameter is appropriate, and for
others,
it may be more suitable to break the metric into several cases.]
Unless specified otherwise, the metrics in this annex are expressed in
processor clock cycles.
For metrics that require documentation of an upper bound,
if there is no upper bound,
the implementation shall report that the metric is unbounded.
@begin{Discussion}
There are several good reasons to specify metrics in seconds; there are however
equally good reasons to specify them in processor clock cycles. In
defining the metrics, we have tried to strike a balance on a case-by-case
basis.

It has been suggested that all metrics should be given names,
so that @lquotes@;data-sheets@rquotes@; could be formulated and published
by vendors.
However the paragraph number can serve that purpose.
@end{Discussion}

@end{Metrics}

@begin{Notes}

The specification of the metrics makes a distinction between upper bounds
and simple execution times. Where something is just specified as @lquotes@;the
execution time of@rquotes@; a piece of code, this leaves one
the freedom
to choose a nonpathological case. This kind of metric is of the form
@lquotes@;there exists a program such that the value of the metric is V@rquotes@;.
Conversely, the meaning of upper bounds is @lquotes@;there is no program such
that the value of the metric is greater than V@rquotes@;.
This kind of metric can only be partially tested, by finding the value
of V for one or more test programs.

The metrics do not cover the whole language; they are limited
to features that are specified in @RefSec{Systems Programming}
and in this Annex. The metrics are intended
to provide guidance to potential users as to whether a particular
implementation of such a feature is going to be adequate for a
particular real-time application. As such, the metrics are aimed
at known implementation choices that can result in significant
performance differences.

The purpose of the metrics is not necessarily to provide fine-grained
quantitative results or to serve as a comparison between different
implementations on the same or different platforms. Instead, their
goal is rather qualitative; to define a standard set of approximate values
that can be measured and used to estimate the general suitability of an
implementation, or to evaluate the comparative utility of certain features
of an implementation for a particular real-time application.

@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
This Annex is new to Ada 95.
@end{Extend83}


@LabeledClause{Task Priorities}
@begin{Intro}
@Redundant[This clause specifies the priority model for real-time systems.
In addition, the methods for specifying priorities are defined.]
@end{Intro}

@begin{NotIso}
@ChgAdded{Version=[3],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 2
through 6 were moved to @RefSec{Obsolescent Features}.>}]}@Comment{This message
should be deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@begin{Syntax}
@begin{SyntaxText}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Type=[Leading],KeepNext=[T],Text=[The form of a
@nt{pragma} Priority is as follows:]}
@end{SyntaxText}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@DeletedPragmaSyn<Version=[3],InitialVersion=[0],@ChgDeleted{Version=[3],
Text=[@key{pragma} @prag(Priority)(@Syn2{expression});]}>

@begin{SyntaxText}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Type=[Leading],KeepNext=[T],Text=[The form of a
@nt{pragma} Interrupt_Priority is as follows:]}
@end{SyntaxText}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@DeletedPragmaSyn<Version=[3],InitialVersion=[0],@ChgDeleted{Version=[3],
Text=[@key{pragma} @prag(Interrupt_Priority)[(@Syn2{expression})];]}>
@end{Syntax}

@begin{Resolution}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[@PDefn2{Term=[expected type],
  Sec=(Priority pragma argument)}
@PDefn2{Term=[expected type],
  Sec=(Interrupt_Priority pragma argument)}
The expected type for the @nt{expression} in a Priority
or Interrupt_Priority pragma is Integer.]}
@end{Resolution}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For a task type (including the
anonymous type of a @nt{single_task_declaration}), protected type (including the
anonymous type of a @nt{single_protected_declaration}), or subprogram,
the following language-defined representation aspects may be specified:]}

@begin{Description}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Priority@\The aspect Priority is
an @nt{expression}, which shall be of type Integer.@AspectDefn{Priority}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Priority],
    Text=[@ChgAdded{Version=[3],Text=[Priority of a task object or type, or
      priority of a protected object or type; the priority is not in the
      interrupt range.]}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Interrupt_Priority@\The aspect
Interrupt_Priority is an @nt{expression}, which shall be of type
Integer.@AspectDefn{Interrupt_Priority}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Interrupt_Priority],
    Text=[@ChgAdded{Version=[3],Text=[Priority of a task object or type, or
      priority of a protected object or type; the priority is in the
      interrupt range.]}]}

@end{Description}
@end{StaticSem}


@begin{Legality}

@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[A Priority pragma is allowed only immediately
within a @nt{task_definition}, a @nt{protected_definition}, or the
@nt{declarative_part} of a @nt{subprogram_body}. An Interrupt_Priority pragma is
allowed only immediately within a @nt{task_definition} or a
@nt{protected_definition}. At most one such pragma shall appear within a given
construct.]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[If the],Old=[For a]} Priority @Chg{Version=[3],New=[aspect
is specified for a subprogram],Old=[pragma that appears in the
@nt{declarative_part} of a @nt{subprogram_body}]}, the @nt{expression} shall be
static, and its value shall be in the range of System.Priority.
@begin{Reason}
This value is needed before it gets elaborated, when the environment task
starts executing.
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[At most one of the Priority and Interrupt_Priority
aspects may be specified for a given entity.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This includes specifying via pragmas
  (see @RefSecNum{Pragmas Priority and Interrupt_Priority}). Note that
  @RefSecNum{Operational and Representation Aspects} prevents multiple
  specifications of a single representation aspect by any means.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[Neither of the Priority or Interrupt_Priority
aspects shall be specified for a synchronized interface type.]}

@end{Legality}

@begin{StaticSem}

@leading@keepnext@;The following declarations exist in package System:
@begin{example}
@key{subtype} Any_Priority @key{is} Integer @key{range} @RI{implementation-defined};
@key{subtype} Priority @key{is} Any_Priority
   @key{range} Any_Priority'First .. @RI{implementation-defined};
@key{subtype} Interrupt_Priority @key{is} Any_Priority
   @key{range} Priority'Last+1 .. Any_Priority'Last;

Default_Priority : @key{constant} Priority := (Priority'First + Priority'Last)/2;
@end{example}
@ImplDef{The declarations of Any_Priority and Priority.}

The full range of priority values supported by an implementation is specified
by the subtype Any_Priority. The subrange of priority values that are high
enough to require the blocking of one or more interrupts is specified by the
subtype Interrupt_@!Priority. @Redundant[The subrange of priority values below
System.@!Interrupt_@!Priority'First is specified by the subtype System.@!Priority.]

@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[The priority specified by a Priority or
Interrupt_Priority pragma is the value of the @nt{expression} in the pragma, if
any. If there is no @nt{expression} in an Interrupt_Priority pragma, the
priority value is Interrupt_Priority'Last.]}

@end{StaticSem}

@begin{RunTime}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[The],Old=[A]} Priority
@Chg{Version=[3],New=[aspect],Old=[pragma]} has no effect
if it @Chg{Version=[3],New=[is specified for],Old=[it occurs
in the @nt{declarative_part} of the @nt{subprogram_body} of]} a
subprogram other than the main subprogram@Chg{Version=[3],New=[; the Priority
value is not associated with any task],Old=[]}.

@Defn{task priority}
@Defn{priority}
@Defn{priority inheritance}
@Defn{base priority}
@Defn{active priority}
A @i{task priority} is an integer value that indicates a degree of urgency
and is the basis for resolving competing demands of tasks for
resources. Unless otherwise specified, whenever tasks compete
for processors or other implementation-defined resources, the
resources are allocated to the task with the highest priority
value.
The @i{base priority} of a task is the priority with which it was
created, or to which it was later set by Dynamic_Priorities.Set_Priority
(see @RefSecNum{Dynamic Priorities}). At all times, a task also has
an @i{active priority}, which generally reflects its base priority
as well as any priority it inherits from other sources.
@i{Priority inheritance} is the process by which the priority of a
task or other entity (e.g. a protected object;
see @RefSecNum{Priority Ceiling Locking}) is used in the evaluation of another
task's active priority.
@ImplDef{Implementation-defined execution resources.}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
The effect of specifying @Chg{Version=[3],New=[a Priority or
Interrupt_Priority aspect for a protected type or
@nt{single_protected_declaration}],Old=[such a pragma
in a @nt{protected_definition}]}
is discussed in @RefSecNum{Priority Ceiling Locking}.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Defn2{Term=[creation], Sec=(of a task object)}
The @nt{expression} @Chg{Version=[3],New=[specified for the],Old=[in a]}
Priority or Interrupt_Priority @Chg{Version=[3],New=[aspect of a task],
Old=[pragma that
appears in a @nt{task_definition}]} is evaluated for each task object
(see @RefSecNum{Task Units and Task Objects}).
For @Chg{Version=[3],New=[the],Old=[a]} Priority @Chg{Version=[3],New=[aspect],Old=[pragma]},
the value of the @nt{expression} is converted to the subtype Priority;
for @Chg{Version=[3],New=[the],Old=[an]}
Interrupt_Priority @Chg{Version=[3],New=[aspect],Old=[pragma]}, this value
is converted to the subtype Any_Priority.
The priority value is then associated with the task object whose
@Chg{Version=[3],New=[task declaration specifies the
aspect],Old=[@nt{task_definition} contains the pragma]}.
@Chg{Version=[3],New=[@PDefn2{Term=[implicit subtype conversion],Sec=(Priority aspect)}
@PDefn2{Term=[implicit subtype conversion],Sec=(Interrupt_Priority aspect)}],
Old=[@PDefn2{Term=[implicit subtype conversion],Sec=(pragma Priority)}
@PDefn2{Term=[implicit subtype conversion],Sec=(pragma Interrupt_Priority)}]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
Likewise, the priority value is associated with the environment task if the
@Chg{Version=[3],New=[aspect is specified for],Old=[pragma appears in the
@nt{declarative_part} of]} the main subprogram.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
The initial value of a task's base priority is specified by default or
by means of a Priority or Interrupt_Priority
@Chg{Version=[3],New=[aspect],Old=[pragma]}.
@Redundant[After a task is created,
its base priority can be changed only by a call to
Dynamic_Priorities.Set_Priority (see @RefSecNum{Dynamic Priorities}).]
The initial base priority of a task in the absence of
@Chg{Version=[3],New=[an aspect],Old=[a pragma]} is the
base priority of the task that creates it at the time of creation
(see @RefSecNum{Task Units and Task Objects}).
If @Chg{Version=[3],New=[the aspect],Old=[a pragma]} Priority
@Chg{Version=[3],New=[is not specified for],Old=[does not apply to]} the
main subprogram, the initial base priority of the environment task is
System.Default_Priority.
@Redundant[The task's active priority is used when the task competes for
processors.
Similarly, the task's active priority is used
to determine the task's position in any queue when Priority_Queuing is
specified (see @RefSecNum{Entry Queuing Policies}).]

@Leading@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00357-01]}
At any time, the active priority of a task is the maximum of all the
priorities the task is inheriting at that instant. For a task that is not
held (see @RefSecNum{Asynchronous Task Control}), its base priority is
@Chg{Version=[2],New=[],Old=[always ]}a source of priority inheritance
@Chg{Version=[2],New=[unless otherwise
specified for a particular task dispatching policy],Old=[]}.
Other sources of priority inheritance are specified under the following
conditions:
@begin{Discussion}
Other parts of the annex, e.g.
@RefSecNum{Asynchronous Task Control}, define
other sources of priority inheritance.
@end{Discussion}
@begin{itemize}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0072],ARef=[AI95-00092-01]}
During activation, a task being activated inherits the active priority
@Chg{New=[that],Old=[of the]} its activator (see
@RefSecNum{Task Execution - Task Activation})@Chg{New=[ had at the time
the activation was initiated],Old=[]}.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0072],ARef=[AI95-00092-01]}
During rendezvous, the task accepting the entry call inherits the
@Chg{New=[],Old=[active ]}priority of the @Chg{New=[entry call],Old=[caller]}
(see @RefSecNum{Entry Calls}@Chg{New=[ and @RefSecNum{Entry Queuing Policies}],Old=[]}).

During a protected action on a protected object, a task inherits the ceiling
priority of the protected object (see @RefSecNum{Intertask Communication} and
@RefSecNum{Priority Ceiling Locking}).

@end{itemize}

In all of these cases, the priority ceases to be
inherited as soon as the condition calling for the inheritance no longer
exists.

@end{RunTime}

@begin{ImplReq}

The range of System.Interrupt_Priority shall include at least one value.

The range of System.Priority shall include at least 30 values.

@end{ImplReq}

@begin{Notes}

The priority expression can include references to
discriminants of the enclosing type.

It is a consequence of the active priority rules that at the point when
a task stops inheriting a priority from another source, its active priority
is re-evaluated. This is in addition to other instances described in this
Annex for such re-evaluation.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0248-1]}
An implementation may provide a
@Chg{Version=[3],New=[nonstandard],Old=[non-standard]} mode in which tasks
inherit priorities under conditions other than those specified above.
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
The use of a Priority or Interrupt_Priority
@Chg{Version=[3],New=[aspect],Old=[pragma]} does not
require the package System to be named in a @nt{with_clause} for the
enclosing @nt{compilation_unit}.
@end{Ramification}

@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
The priority of a task is per-object and not per-type.

Priorities need not be static anymore (except for the main subprogram).

@end{Extend83}

@begin{DiffWord83}

The description of the Priority pragma has been moved to this annex.

@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0072],ARef=[AI95-00092-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that dynamic
  priority changes are not transitive - that is, they don't apply to tasks
  that are being activated by or in rendezvous with the task that had its
  priority changed.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
  @ChgAdded{Version=[2],Text=[Generalized the definition of priority
  inheritance to take into account the differences between the existing and
  new dispatching policies.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Aspects Priority and Interrupt_Priority are new; @nt{pragma}s
  Priority and Interrupt_Priority are now obsolescent.]}
@end{Extend2005}


@LabeledClause{Priority Scheduling}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00321-01]}
@Redundant[This clause describes the rules that determine which task is
selected for execution when more than one task is ready
(see @Chg{Version=[2],New=[@RefSecNum{Tasks and Synchronization}],
Old=[@RefSecNum{Task Execution - Task Activation}]}).@Chg{Version=[2],
New=[],Old=[ The rules have two parts: the task dispatching model
(see @RefSecNum{The Task Dispatching Model}),
and a specific task dispatching policy
(see @RefSecNum{Task Dispatching Pragmas}).]}]
@end{Intro}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00321-01]}
  @ChgAdded{Version=[2],Text=[This introduction is simplified in order to
  reflect the rearrangement and expansion of this clause.]}
@end{DiffWord95}


@LabeledSubClause{The Task Dispatching Model}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00321-01]}
@Redundant[The task dispatching model specifies @Chg{Version=[2],
New=[task],Old=[preemptive]} scheduling, based on conceptual
priority-ordered ready queues.]
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The following
language-defined library package exists:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0166-1]}
@ChgAdded{Version=[2],Text=[@key<package> Ada.Dispatching @key<is>@ChildUnit{Parent=[Ada],Child=[Dispatching]}
  @key<pragma> @Chg{Version=[3],New=[Preelaborate],Old=[Pure]}(Dispatching);@Chg{Version=[3],New=[],Old=[
  @AdaExcDefn{Dispatching_Policy_Error} : @key<exception>;
@key<end> Ada.Dispatching;]}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0166-1]}
@ChgAdded{Version=[3],Text=[  @key<procedure> @AdaSubDefn{Yield};]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0166-1]}
@ChgAdded{Version=[3],Text=[  @AdaExcDefn{Dispatching_Policy_Error} : @key<exception>;
@key<end> Ada.Dispatching;]}
@end{Example}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[Dispatching serves as the parent of other
language-defined library units concerned with task dispatching.]}

@end{StaticSem}

@begin{RunTime}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00321-01]}
A task @Chg{Version=[2],New=[can become],Old=[runs (that is, it becomes]} a
@i{running task}@Chg{Version=[2],New=[],Old=[)]} only @Chg{Version=[2],
New=[if],Old=[when]} it is ready (see @Chg{Version=[2],New=[@RefSecNum{Tasks and Synchronization}],
Old=[@RefSecNum{Task Execution - Task Activation}]}) and
the execution resources required by that task are available.
Processors are allocated to tasks based on each task's active priority.

It is implementation defined whether, on a multiprocessor, a task that
is waiting for access to a protected object keeps its processor busy.
@ImplDef{Whether, on a multiprocessor, a task that
is waiting for access to a protected object keeps its processor busy.}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00321-01]}
@Defn{task dispatching}
@Defn{dispatching, task}
@RootDefn{task dispatching point}
@RootDefn{dispatching point}
@i{Task dispatching} is the process by which one ready task is selected
for execution on a processor. This selection is done at certain points
during the execution of a task called @i{task dispatching points}.
A task reaches a task dispatching point whenever it becomes blocked,
and @Chg{Version=[2],New=[when it terminates],Old=[whenever it becomes ready.
In addition, the completion of an @nt{accept_statement}
(see @RefSecNum{Entries and Accept Statements}), and task termination are
task dispatching points for the executing task]}.
@Redundant[Other task dispatching points are defined
throughout this Annex@Chg{Version=[2],New=[ for specific policies],Old=[]}.]
@begin{Ramification}
On multiprocessor systems, more than one task can be chosen, at the
same time, for execution on more than one processor, as explained below.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00321-01]}
@Defn{ready queue}
@Defn{head (of a queue)}
@Defn{tail (of a queue)}
@Defn{ready task}
@PDefn{task dispatching policy}
@PDefn{dispatching policy for tasks}
@i{Task dispatching policies} are specified in terms of conceptual
@i{ready queues}@Chg{Version=[2],New=[ and],Old=[,]} task states@Chg{Version=[2],
New=[],Old=[, and task preemption]}.
A ready queue is an ordered list of ready tasks.
The first position in a queue is called the
@i{head of the queue}, and the last position is called the
@i{tail of the queue}.
A task is @i{ready} if it is in a ready queue,
or if it is running.
Each processor has one ready queue for each priority value. At any instant,
each ready queue of a processor contains exactly the set of tasks of that
priority that are ready for execution on that
processor, but are not running on any processor; that is, those tasks
that are ready, are not running on any processor, and can be
executed using that processor and other available resources.
A task can be on the ready queues of more than one processor.
@begin{Discussion}
The core language defines a ready task as one that is not
blocked. Here we refine this definition and
talk about ready queues.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00321-01]}
@Defn{running task}
Each processor also has one @i{running task},
which is the task currently being executed by that processor.
Whenever a task running on a processor reaches a task dispatching
point@Chg{Version=[2],New=[ it goes back to one or more ready queues; a],
Old=[, one]} task @Chg{Version=[2],New=[(possibly the same task) ],Old=[]}is
@Chg{Version=[2],New=[then ],Old=[]}selected to run on that processor.
The task selected is the one at the head of the highest priority
nonempty ready queue;
this task is then removed from all ready queues to which it
belongs.
@begin{Discussion}
There is always at least one task to run,
if we count the idle task.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00321-01]}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0166-1]}
@ChgDeleted{Version=[2],Text=[@Defn{preemptible resource}
A preemptible resource is a resource that while allocated
to one task can be allocated (temporarily) to another
instead.
Processors are preemptible resources. Access to a protected object
(see @RefSecNum{Protected Subprograms and Protected Actions})
is a nonpreemptible resource.
@Defn{preempted task}
When a higher-priority task is dispatched to the processor, and the previously
running task is placed on the appropriate ready queue, the latter task
is said to be @i{preempted}.]}
@ChgAdded{Version=[3],Text=[A call of Yield is a task dispatching point. Yield
is a potentially blocking operation (see @RefSecNum{Protected Subprograms and Protected Actions}).]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[Deleted]}
  @ChgDeleted{Version=[2],Text=[A processor that is executing a task is available
  to execute tasks of higher priority, within the set of tasks that that
  processor is able to execute. Write access to a protected object, on the other
  hand, cannot be granted to a new task before the old task has released it.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00321-01]}
@ChgDeleted{Version=[2],Text=[@PDefn{task dispatching point}
@PDefn{dispatching point}
A new running task is also selected whenever there is a nonempty ready queue
with a higher priority than the priority of the running
task, or when the task dispatching policy requires a
running task to go back to a ready queue.
@Redundant[These are also task dispatching points.]]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Deleted]}
  @ChgDeleted{Version=[2],Text=[Thus, when a task becomes ready, this is a task
  dispatching point for all running tasks of lower priority.]}
@end{Ramification}

@end{RunTime}

@begin{ImplPerm}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00321-01]}
An implementation is allowed to define additional resources as execution
resources, and to define the corresponding allocation policies for them.
Such resources may have an implementation@Chg{Version=[2],New=[-],Old=[ ]}defined
effect on task dispatching@Chg{Version=[2],New=[],
Old=[ (see @RefSecNum{Task Dispatching Pragmas})]}.
@ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],
Text=[The @Chg{Version=[2],New=[effect],Old=[affect]} of
implementation@Chg{Version=[2],New=[-],Old=[ ]}defined
execution resources on task dispatching.]}

An implementation may place implementation-defined restrictions on
tasks whose active priority is in the Interrupt_Priority range.
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
For example, on some operating systems,
it might be necessary to disallow them altogether.
This permission applies to tasks whose priority is set to interrupt
level for any reason: via @Chg{Version=[3],New=[an aspect],Old=[a pragma]},
via a call to Dynamic_Priorities.Set_Priority,
or via priority inheritance.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00321-01]}
@ChgNote{This was moved up from the previous section.}
@ChgAdded{Version=[2],Text=[@Redundant[For optimization purposes,]
an implementation may alter the points at which task dispatching occurs, in an
implementation-defined manner. However, a @nt{delay_statement} always
corresponds to at least one task dispatching point.]}

@end{ImplPerm}

@begin{Notes}

Section 9 specifies under which circumstances a task
becomes ready.
The ready state is affected by the rules for
task activation and termination, delay statements, and entry calls.
@PDefn{blocked}
When a task is not ready, it is said to be blocked.

An example of a possible implementation-defined execution
resource is a page of physical memory, which needs to be loaded
with a particular page of virtual memory before a task can
continue execution.

The ready queues are purely conceptual; there is no requirement that such
lists physically exist in an implementation.

While a task is running, it is not on any ready queue. Any time
the task that is running on a processor is added to a ready queue,
a new running task is selected for that processor.

In a multiprocessor system, a task can be on the ready queues of more than
one processor. At the extreme, if several processors share the same set of
ready tasks, the contents of their ready queues is identical, and so
they can be viewed as sharing one ready queue, and can be implemented that
way.
@Redundant[Thus, the dispatching model covers
multiprocessors where dispatching is implemented using a single
ready queue, as well as those with separate dispatching domains.]

The priority of a task is determined by rules specified in this subclause, and
under @RefSec{Task Priorities}, @RefSec{Priority Ceiling Locking}, and
@RefSec{Dynamic Priorities}.

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00321-01]}
@ChgNote{This note is moved up from the next subclause.}
@ChgAdded{Version=[2],Text=[The setting of a task's base priority as a result
of a call to Set_Priority does not always take effect immediately when
Set_Priority is called. The effect of setting the task's base priority is
deferred while the affected task performs a protected action.]}

@end{Notes}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00321-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[This description is simplified to describe only
  the parts of the dispatching model common to all policies. In particular,
  rules about preemption are moved elsewhere. This makes
  it easier to add other policies (which @Chg{Version=[3],New=[might],Old=[may]}
  not involve preemption).]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0166-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
  Procedure Yield is added to Dispatching.
  If Dispatching is referenced in a @nt{use_clause}, and an
  entity @i<E> with a @nt{defining_identifier} of Yield is
  defined in a package that is also referenced in a @nt{use_clause}, the entity
  @i<E> may no longer be use-visible, resulting in errors. This should be rare
  and is easily fixed if it does occur.]}
@end{Incompatible2005}


@LabeledRevisedSubClause{Version=[2],
New=[Task Dispatching Pragmas],
Old=[The Standard Task Dispatching Policy]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[@Redundant[This clause allows a single task
dispatching policy to be defined for all priorities, or the range of priorities
to be split into subranges that are assigned individual dispatching
policies.]]}
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
@Leading@Keepnext@;The form of a @nt{pragma} Task_Dispatching_Policy is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Task_Dispatching_Policy)(@SynI{policy_}@Syn2{identifier});'

@begin{SyntaxText}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[The form of a
@nt{pragma} Priority_Specific_Dispatching is as follows:]}
@end{SyntaxText}

@ChgRef{Version=[2],Kind=[Added]}
@AddedPragmaSyn<Version=[2],@ChgAdded{Version=[2],Text=`@key{pragma} @prag<Priority_Specific_Dispatching> (@*
@ @ @ @ @ @SynI{policy_}@Syn2{identifier}, @SynI{first_priority_}@Syn2{expression}, @SynI{last_priority_}@Syn2{expression});'}>

@end{Syntax}

@begin{Resolution}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[The expected type for @SynI{first_priority_}@nt{expression}
and @SynI{last_priority_}@nt{expression} is Integer.]}
@end{Resolution}

@begin{Legality}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00321-01],ARef=[AI95-00355-01]}
The @SynI{policy_}@nt{identifier} @Chg{Version=[2],New=[used in a @nt{pragma}
Task_Dispatching_Policy shall be the name of a task dispatching policy],
Old=[shall either be FIFO_Within_Priorities or
an implementation-defined @Syn2{identifier}]}.
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Implementation-defined @SynI{policy_}@Syn2{identifier}s allowed
in a @nt{pragma} Task_Dispatching_Policy.]}]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[The @SynI{policy_}@nt{identifier}
used in a @nt{pragma}
Priority_Specific_Dispatching shall be the name of a task dispatching policy.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[Both @Syni{first_priority_}@!@nt{expression} and
@Syni{last_priority_}@!@nt{expression} shall be static expressions in the range
of System.Any_Priority; @SynI{last_priority_}@!@nt{expression} shall have a
value greater than or equal to @SynI{first_priority_}@!@nt{expression}.]}

@end{Legality}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[@nt{Pragma} Task_Dispatching_Policy specifies the
single task dispatching policy.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[@nt{Pragma} Priority_Specific_Dispatching specifies
the task dispatching policy for the specified range of priorities. Tasks with
base priorities within the range of priorities specified in a
Priority_Specific_Dispatching pragma have their active priorities determined
according to the specified dispatching policy. Tasks with active priorities
within the range of priorities specified in a Priority_Specific_Dispatching
pragma are dispatched according to the specified dispatching policy.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
  @ChgAdded{Version=[2],Text=[Each ready queue is managed by exactly one
  policy. Anything else would be chaos. The ready queue is determined by
  the active priority. However, how the active priority is calculated is
  determined by the policy; in order to break out of this circle, we have
  to say that the active priority is calculated by the method determined
  by the policy of the base priority.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00355-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[If a partition contains one or more
Priority_Specific_Dispatching pragmas@Chg{Version=[3],New=[,],Old=[]}
the dispatching policy for priorities not
covered by any Priority_Specific_Dispatching pragmas is
FIFO_Within_Priorities.]}

@end{StaticSem}

@begin{LinkTime}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00355-01]}
@PDefn2{Term=[configuration pragma], Sec=(Task_Dispatching_Policy)}
@PDefn2{Term=[pragma, configuration], Sec=(Task_Dispatching_Policy)}
A Task_Dispatching_Policy pragma is a configuration pragma.@Chg{Version=[2],
New=[ A Priority_Specific_Dispatching pragma is a configuration pragma.
@PDefn2{Term=[configuration pragma], Sec=(Priority_Specific_Dispatching)}
@PDefn2{Term=[pragma, configuration], Sec=(Priority_Specific_Dispatching)}],Old=[]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[The priority ranges specified in more than one
Priority_Specific_Dispatching pragma within the same partition shall not be
overlapping.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[If a partition contains one or more
Priority_Specific_Dispatching pragmas it shall not contain a
Task_Dispatching_Policy pragma.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00333-01]}
@ChgDeleted{Version=[2],Text=[If the FIFO_Within_Priorities policy is specified
for a partition, then the Ceiling_Locking policy
(see @RefSecNum{Priority Ceiling Locking}) shall also be specified for
the partition.]}

@end{LinkTime}

@begin{RunTime}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00355-01]}
@Defn{task dispatching policy}
@Redundant[A @i{task dispatching policy} specifies the details of task
dispatching that are not covered by the basic task dispatching model.
These rules govern when tasks are inserted into and
deleted from the ready queues@Chg{Version=[2],New=[],Old=[,
and whether a task is inserted at the head or the tail of the
queue for its active priority]}.]
@Chg{Version=[2],New=[A single],Old=[The]} task dispatching policy is
specified by a Task_Dispatching_Policy @Chg{Version=[2],New=[],Old=[configuration ]}pragma.
@Chg{Version=[2],New=[Pragma Priority_Specific_Dispatching assigns distinct
dispatching policies to subranges of System.Any_Priority.],
Old=[@PDefn{unspecified}If no such pragma appears in any of the program
units comprising a partition, the task dispatching policy for
that partition is unspecified.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[@PDefn{unspecified}If neither @nt{pragma} applies
to any of the program units comprising a partition, the task dispatching policy
for that partition is unspecified.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00355-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[If a partition contains one or more
Priority_Specific_Dispatching pragmas@Chg{Version=[3],New=[,],Old=[]}
a task dispatching point occurs for the
currently running task of a processor whenever there is a nonempty ready queue
for that processor with a higher priority than the priority of the running
task.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[If we have priority specific dispatching then we
  want preemption across the entire range of priorities. That prevents higher
  priority tasks from being blocked by lower priority tasks that have a
  different policy. On the other hand, if we have a single policy for the
  entire partition, we want the characteristics of that policy to apply for
  preemption; specifically, we @Chg{Version=[3],New=[might],Old=[may]}
  not require any preemption. Note that policy
  Non_Preemptive_FIFO_Within_Priorities is not allowed in a priority specific
  dispatching pragma.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[A task that has its base priority changed may move
from one dispatching policy to another. It is immediately subject
to the new dispatching policy.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Once subject to the new dispatching policy, it
  may be immediately preempted or dispatched, according the rules of the new
  policy.]}
@end{Ramification}

@ChgNote{The following stuff is moved to the next subclause}
@begin{NotIso}
@ChgAdded{Version=[2],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 7
through 13 were moved to D.2.3.>}]}@Comment{This message should be deleted if the
paragraphs are ever renumbered.}
@end{NotIso}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00321-01]}
@ChgDeleted{Version=[2],Type=[Leading],Text=[The language defines only one task
dispatching policy, FIFO_Within_Priorities; when this policy is in effect,
modifications to the ready queues occur only as follows:]}

@begin{itemize}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00321-01]}
@ChgDeleted{Version=[2],Text=[When a blocked task becomes ready,
it is added at the tail of the ready queue for its active priority.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00321-01]}
@ChgDeleted{Version=[2],Text=[When the active priority of a ready task that is
not running changes, or the setting of its base priority takes effect, the task
is removed from the ready queue for its old active priority and is added at the
tail of the ready queue for its new active priority, except in the case where
the active priority is lowered due to the loss of inherited priority, in which
case the task is added at the head of the ready queue for its new active
priority.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00321-01]}
@ChgDeleted{Version=[2],Text=[When the setting of the base priority of a
running task takes effect, the task is added to the tail of the ready queue for
its active priority.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00321-01]}
@ChgDeleted{Version=[2],Text=[When a task executes a @nt{delay_statement} that
does not result in blocking, it is added to the tail of the ready queue for its
active priority.]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[2],Text=[If the delay does result in blocking,
  the task moves to the @lquotes@;delay queue@rquotes@;,
  not to the ready queue.]}
@end{Ramification}

@end{itemize}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00321-01]}
@ChgDeleted{Version=[2],Text=[@PDefn{task dispatching point}
@PDefn{dispatching point}
Each of the events specified above is a task dispatching point
(see @RefSecNum{The Task Dispatching Model}).]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00321-01]}
@ChgDeleted{Version=[2],Text=[In addition, when a task is preempted, it is
added at the head of the ready queue for its active priority.]}

@end{RunTime}

@begin{ImplReq}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00333-01],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[An implementation shall allow, for a single
partition, both the locking policy (see @RefSecNum{Priority Ceiling Locking})
to be specified as Ceiling_Locking
and also one or more Priority_Specific_Dispatching pragmas to be given.]}
@end{ImplReq}

@begin{DocReq}

@begin{NotIso}
@ChgAdded{Version=[2],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 14
through 16 were moved to D.2.3.>}]}@Comment{This message should be deleted if the
paragraphs are ever renumbered.}
@end{NotIso}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00321-01]}
@ChgDeleted{Version=[2],Type=[Leading],Text=[@Defn{priority inversion}
@i{Priority inversion} is the duration for which a task remains at the
head of the highest priority ready queue while the processor executes
a lower priority task. The implementation shall document:]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00321-01]}
@ChgDeleted{Version=[2],Text=[The maximum priority inversion a user task can experience due to activity
of the implementation (on behalf of lower priority tasks), and]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00321-01]}
@ChgDeleted{Version=[2],Text=[whether execution of a task can be preempted by
the implementation processing of delay
expirations for lower priority tasks, and if so, for how long.]}
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Implementation-defined aspects of priority inversion.]}]}
@end{Itemize}

@end{DocReq}

@begin{ImplPerm}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00256-01]}
Implementations are allowed to define other task dispatching policies, but
need not support more than one @Chg{Version=[2],New=[task dispatching],
Old=[such]} policy per partition.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00355-01]}
@Chg{Version=[2],New=[An implementation need not support @nt{pragma}
Priority_Specific_Dispatching if it is infeasible to support it in the
target environment.],
Old=[@Redundant[For optimization purposes,]
an implementation may alter the points at which task dispatching occurs,
in an implementation defined manner.
However, a @nt{delay_statement} always corresponds to at least one task
dispatching point.]}

@ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],
Text=[Implementation defined task
dispatching@Chg{Version=[2],New=[ policies],Old=[]}.]}

@end{ImplPerm}

@begin{Notes}

@begin{NotIso}
@ChgAdded{Version=[2],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 19
through 21 were deleted.>}]}@Comment{This message should be deleted if the
paragraphs are ever renumbered.}
@end{NotIso}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00321-01]}
@ChgDeleted{Version=[2],Text=[If the active priority of a running task is
lowered due to loss of inherited priority (as it is on completion of a
protected operation) and there is a ready task of the same active priority that
is not running, the running task continues to run (provided that there is no
higher priority task).]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00321-01]}
@ChgDeleted{Version=[2],Text=[The setting of a task's base priority as a result
of a call to Set_Priority does not always take effect immediately when
Set_Priority is called. The effect of setting the task's base priority is
deferred while the affected task performs a protected action.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00321-01]}
@ChgDeleted{Version=[2],Text=[Setting the base priority of a ready task causes
the task to move to the end of the queue for its active priority,
regardless of whether the active priority of the task actually changes.]}

@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00333-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @B[Amendment Correction:] It is no longer required to specify Ceiling_Locking
  with the language-defined task dispatching policies; we only require that
  implementations @i<allow> them to be used together.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[@key{Pragma} Priority_Specific_Dispatching is
  new; it allows @Chg{Version=[3],New=[the specification of],Old=[specifying]}
  different policies for different priorities.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00256-01]}
  @ChgAdded{Version=[2],Text=[Clarified that an implementation need support
  only one task dispatching policy (of any kind, language-defined or otherwise)
  per partition.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00321-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[This description is simplified to describe only
  the rules for the Task_Dispatching_Policy pragma that are common to
  all policies. In particular, rules about preemption are moved elsewhere. This
  makes it easier to add other policies (which
  @Chg{Version=[3],New=[might],Old=[may]} not involve preemption).]}
@end{DiffWord95}


@LabeledAddedSubClause{Version=[2],Name=[Preemptive Dispatching]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00321-01]}
@ChgAdded{Version=[2],Text=[@Redundant[This clause defines a preemptive task
dispatching policy.]]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[The @SynI{policy_}@nt{identifier}
FIFO_Within_Priorities is a task dispatching
policy.@Chg{Version=[3],New=[@Defn2{Term=[task dispatching policy],
Sec=(FIFO_Within_Priorities)}@Defn{FIFO_Within_Priorities task dispatching policy}],
Old=[]}]}

@end{StaticSem}

@begin{RunTime}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00321-01]}
@ChgAdded{Version=[2],Text=[When FIFO_Within_Priorities is in effect,
modifications to the ready queues occur only as follows:]}

@begin{itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00321-01]}
@ChgAdded{Version=[2],Text=[When a blocked task becomes ready, it is added at
the tail of the ready queue for its active priority.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[When the active priority of a ready task that is
not running changes, or the setting of its base
priority takes effect, the task is removed from the ready queue for
its old active priority and is added at the tail of the ready queue for its new
active priority, except in the case where the active priority is lowered due to
the loss of inherited priority, in which case the task is added at the
head of the ready queue for its new active priority.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[When the setting of the base priority of a running task takes effect, the
task is added to the tail of the ready queue for its active priority.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[When a task executes a @nt{delay_statement} that
does not result in blocking, it is added to the tail of the ready queue for
its active priority.]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If the delay does result in blocking,
  the task moves to the @lquotes@;delay queue@rquotes@;,
  not to the ready queue.]}
@end{Ramification}

@end{itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00321-01]}
@ChgAdded{Version=[2],Text=[@PDefn{task dispatching point}
@PDefn{dispatching point}
Each of the events specified above is a task dispatching point
(see @RefSecNum{The Task Dispatching Model}).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00321-01]}
@ChgAdded{Version=[2],Text=[A task dispatching point occurs for the currently
running task of a processor whenever there is a nonempty ready queue for that
processor with a higher priority than the priority of the running task. The
currently running task is said to be @i<preempted> and it is added at the head
of the ready queue for its active priority.@Defn2{Term=[preempt],Sec=[a running task]}]}

@end{RunTime}

@begin{ImplReq}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00333-01]}
@ChgAdded{Version=[2],Text=[An implementation shall allow, for a single
partition, both the task dispatching policy to be specified as
FIFO_Within_Priorities and also the locking policy (see
@RefSecNum{Priority Ceiling Locking}) to be specified as Ceiling_Locking.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is the preferred combination of the
  FIFO_Within_Priorities policy with a locking policy, and we want that
  combination to be portable.]}
@end{Reason}
@end{ImplReq}

@begin{DocReq}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00321-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Defn{priority inversion}
@i{Priority inversion} is the duration for which a task remains at the
head of the highest priority nonempty ready queue while the processor executes
a lower priority task. The implementation shall document:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The maximum priority inversion a user task
can experience due to activity
of the implementation (on behalf of lower priority tasks), and]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The maximum priority inversion a user task can experience from
the implementation.]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[whether execution of a task can be preempted
by the implementation processing of delay
expirations for lower priority tasks, and if so, for how long.]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The amount of time that a task can be preempted for processing on
behalf of lower-priority tasks.]}]}
@end{Itemize}

@end{DocReq}

@begin{Notes}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00321-01]}
@ChgAdded{Version=[2],Text=[If the active priority of a running task is
lowered due to loss of
inherited priority (as it is on completion of a protected
operation) and there is a ready task of the same active priority
that is not running,
the running task continues to run (provided that there is no higher
priority task).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00321-01]}
@ChgAdded{Version=[2],Text=[Setting the base priority of a ready task causes
the task to move to the tail of the queue for its active priority,
regardless of whether the active priority of the task actually changes.]}

@end{Notes}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00321-01]}
  @ChgAdded{Version=[2],Text=[This subclause is new; it mainly consists of
  text that was found in @RefSecNum{The Task Dispatching Model} and
  @RefSecNum{Task Dispatching Pragmas} in Ada 95. This was
  separated out so the definition of additional policies was easier.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00333-01]}
  @ChgAdded{Version=[2],Text=[We require that implementations allow
  this policy and Ceiling_Locking together.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
  @ChgAdded{Version=[2],Text=[We explicitly defined FIFO_Within_Priorities
  to be a task dispatching policy.]}
@end{DiffWord95}


@LabeledAddedSubClause{Version=[2],Name=[Non-Preemptive Dispatching]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00298-01]}
@ChgAdded{Version=[2],Text=[@Redundant[This clause defines a non-preemptive task
dispatching policy.]]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00298-01],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[The @SynI{policy_}@nt{identifier}
Non_Preemptive_FIFO_Within_Priorities is a task dispatching
policy.@Chg{Version=[3],New=[@Defn2{Term=[task dispatching policy],
Sec=(Non_Preemptive_@!FIFO_@!Within_@!Priorities)}@Defn{Non_Preemptive_FIFO_@!Within_@!Priorities task disp. policy}],
Old=[]}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0166-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The following
language-defined library package exists:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key<package> Ada.Dispatching.Non_Preemptive @key<is>@ChildUnit{Parent=[Ada.Dispatching],Child=[Non_Preemptive]}
  @key<pragma> Preelaborate(Non_Preemptive);
  @key<procedure> @AdaSubDefn{Yield_To_Higher};
  @key<procedure> @AdaSubDefn{Yield_To_Same_Or_Higher} @key<renames> Yield;
@key<end> Ada.Dispatching.Non_Preemptive;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0166-1],ARef=[AI05-0264-1]}
@ChgAdded{Version=[3],Text=[A call of Yield_To_Higher is a task dispatching
point for this policy. If the task at the head of the highest priority ready
queue has a higher active priority than the calling task, then the calling task
is preempted.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[For language-defined policies other than
  Non_Preemptive_FIFO_Within_Priorities, a higher priority task should never be
  on a ready queue while a lower priority task is executed. Thus, for such
  policies, Yield_To_Higher does nothing.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Yield_To_Higher is @i<not> a potentially blocking
  operation; it can be used during a protected operation. That is allowed,
  as under the predefined Ceiling_Locking policy any
  task with a higher priority than the protected operation cannot call the
  operation (that would violate the locking policy). An
  implementation-defined locking policy may need to define the semantics of
  Yield_To_Higher differently.]}
@end{Ramification}
@end{StaticSem}

@begin{Legality}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[Non_Preemptive_FIFO_Within_Priorities shall not be
specified as the @SynI{policy_}@nt{identifier} of @nt{pragma}
Priority_Specific_Dispatching (see
@RefSecNum{Task Dispatching Pragmas}).]}

@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The non-preemptive nature of this policy could
cause the policies of higher priority tasks to malfunction, missing deadlines
and having unlimited priority inversion. That would render the use of such
policies impotent and misleading. As such, this policy only makes sense
for a complete system.]}
@end{Reason}
@end{Legality}

@begin{RunTime}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00298-01]}
@ChgAdded{Version=[2],Text=[When Non_Preemptive_FIFO_Within_Priorities is in
effect, modifications to the ready queues occur only as follows:]}

@begin{itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00298-01]}
@ChgAdded{Version=[2],Text=[When a blocked task becomes ready, it is added at
the tail of the ready queue for its active priority.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[When the active priority of a ready task that is
not running changes, or the setting of its base
priority takes effect, the task is removed from the ready queue for
its old active priority and is added at the tail of the ready queue for its new
active priority.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[When the setting of the base priority of a running task takes effect, the
task is added to the tail of the ready queue for its active priority.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[When a task executes a @nt{delay_statement} that
does not result in blocking, it is added to the tail of the ready queue for
its active priority.]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If the delay does result in blocking,
  the task moves to the @lquotes@;delay queue@rquotes@;,
  not to the ready queue.]}
@end{Ramification}

@end{itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0166-1]}
@ChgAdded{Version=[2],Text=[For this policy, @Chg{Version=[3],New=[blocking
or termination of a task, ],Old=[]}a@Chg{Version=[3],New=[],Old=[ non-blocking]}
@nt{delay_statement}@Chg{Version=[3],New=[, a call to Yield_To_Higher, and
a call to Yield_To_Same_Or_Higher or Yield are],Old=[ is]}
the only@Chg{Version=[3],New=[],Old=[ non-blocking event that is a]}
task dispatching @Chg{Version=[3],New=[points],Old=[point]} (see
@RefSecNum{The Task Dispatching Model}).@PDefn{task dispatching point}
@PDefn{dispatching point}]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0166-1]}
  @ChgAdded{Version=[3],Text=[A @nt{delay_statement} is always a task
  dispatching point even if it is not blocking. Similarly, a call to
  Yield_To_Higher is never blocking, but it is a task dispatching point
  In each of these cases, they can cause the current task to stop running (it is
  still ready). Otherwise, the running task continues to run until it is blocked.]}
@end{Ramification}
@end{RunTime}

@begin{ImplReq}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00333-01]}
@ChgAdded{Version=[2],Text=[An implementation shall allow, for a single
partition, both the task dispatching policy to be specified as
Non_Preemptive_FIFO_Within_Priorities and also the locking policy (see
@RefSecNum{Priority Ceiling Locking}) to be specified as Ceiling_Locking.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is the preferred combination of the
  Non_Preemptive_FIFO_Within_Priorities policy with a locking policy, and we
  want that combination to be portable.]}
@end{Reason}
@end{ImplReq}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00298-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[2],Text=[Since implementations are allowed to round all
ceiling priorities in subrange System.Priority to System.Priority'Last (see
@RefSecNum{Priority Ceiling Locking}), an implementation may allow a
task@Chg{Version=[3],New=[ of a partition using the
Non_Premptive_FIFO_Within_Priorities policy],Old=[]} to
execute within a protected object without raising its active priority provided
the associated protected unit does not contain @Chg{Version=[3],New=[any
subprograms with aspects Interrupt_Handler or Attach_Handler specified, nor
does the unit have aspect],Old=[pragma]} Interrupt_Priority
@Chg{Version=[3],New=[ specified. When the locking policy
(see @RefSecNum{Priority Ceiling Locking}) is
Ceiling_Locking, an implementation taking advantage of this permission shall
ensure that a call to Yield_to_Higher that occurs within a protected action uses
the ceiling priority of the protected object (rather than the active priority of
the task) when determining whether to preempt the task],Old=[,
Interrupt_Handler, or Attach_Handler]}.]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],Text=[We explicitly require that the ceiling priority be
  used in calls to Yield_to_Higher in order to prevent a risk of priority
  inversion and consequent loss of mutual exclusion when Yield_to_Higher is used
  in a protected object. This requirement might lessen the value of the
  permission (as the current Ceiling_Priority will have to be maintained in the
  TCB), but loss of mutual exclusion cannot be tolerated. The primary benefit of
  the permission (eliminating the need for preemption at the end of a protected
  action) is still available. As noted above, an implementation-defined locking
  policy will need to specify the semantics of Yield_to_Higher, including this
  case.]}
@end{Reason}
@end{ImplPerm}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00298-01],ARef=[AI95-00355-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Policy Non_Preemptive_FIFO_Within_Priorities is new.]}
@end{Extend95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0166-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}Package
  Dispatching.Non_Preemptive is new.]}
@end{Extend2005}


@RMNewPageVer{Version=[3]}@Comment{For printed RM Ada 2012}
@LabeledAddedSubClause{Version=[2],Name=[Round Robin Dispatching]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[@Redundant[This clause defines the task dispatching
policy Round_Robin_Within_Priorities and the package Round_Robin.]]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[The @SynI{policy}_@nt{identifier}
Round_Robin_Within_Priorities is a task dispatching
policy.@Chg{Version=[3],New=[@Defn2{Term=[task dispatching policy],
Sec=(Round_Robin_Within_Priorities)}@Defn{Round_Robin_Within_Priorities task dispatching policy}],
Old=[]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The following
language-defined library package exists:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{with} System;
@key{with} Ada.Real_Time;
@key{package} Ada.Dispatching.Round_Robin @key{is}@ChildUnit{Parent=[Ada.Dispatching],Child=[Round_Robin]}
  @AdaObjDefn{Default_Quantum} : @key{constant} Ada.Real_Time.Time_Span :=
             @RI[implementation-defined];
  @key{procedure} @AdaSubDefn{Set_Quantum} (Pri     : @key{in} System.Priority;
                         Quantum : @key{in} Ada.Real_Time.Time_Span);
  @key{procedure} @AdaSubDefn{Set_Quantum} (Low, High : @key{in} System.Priority;
                         Quantum   : @key{in} Ada.Real_Time.Time_Span);
  @key{function} @AdaSubDefn{Actual_Quantum} (Pri : System.Priority)
             @key{return} Ada.Real_Time.Time_Span;
  @key{function} @AdaSubDefn{Is_Round_Robin} (Pri : System.Priority) @key{return} Boolean;
@key{end} Ada.Dispatching.Round_Robin;]}
@end{Example}
@ChgImplDef{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[The value of Default_Quantum in Dispatching.Round_Robin.]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[When task dispatching policy Round_Robin_Within_Priorities is the single
policy in effect for a partition, each task with priority in the range of
System.Interrupt_Priority is dispatched according to policy
FIFO_Within_Priorities.]}

@end{StaticSem}

@begin{Runtime}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[The procedures Set_Quantum set the required Quantum
value for a single priority level Pri or a range of priority levels Low .. High.
If no quantum is set for a Round Robin priority level, Default_Quantum is used.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[The function Actual_Quantum returns the actual
quantum used by the implementation for the priority level Pri.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The function Is_Round_Robin returns True if
priority Pri is covered by task dispatching policy
Round_Robin_Within_Priorities; otherwise@Chg{Version=[3],New=[,],Old=[]}
it returns False.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[A call of Actual_Quantum or Set_Quantum raises
exception Dispatching.Dispatching_Policy_Error if a predefined policy other
than Round_Robin_Within_Priorities applies to the specified priority
or any of the priorities in the specified range.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For Round_Robin_Within_Priorities,
the dispatching rules for FIFO_Within_Priorities apply with the following
additional rules:]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[When a task is added or moved to the tail of the
ready queue for its base priority, it has an execution time budget equal to the
quantum for that priority level. This will also occur when a blocked task
becomes executable again.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[When a task is preempted (by a higher priority
task) and is added to the head of the ready queue for its priority level, it
retains its remaining budget.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[While a task is executing, its budget is decreased
by the amount of execution time it uses. The accuracy of this accounting is the
same as that for execution time clocks (see @RefSecNum{Execution Time}).]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Note that this happens even when the task
  is executing at a higher, inherited priority, and even if that higher
  priority is dispatched by a different policy than round robin.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[When a task has exhausted its budget and is without
an inherited priority (and is not executing within a protected operation), it
is moved to the tail of the ready queue for its priority level. This is a task
dispatching point.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[In this case, it will be given
  a budget as described in the first bullet.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The rules for FIFO_Within_Priority (to which
  these bullets are added) say that a task that has its base priority set to a
  Round Robin priority is moved to the tail of the ready queue for its new
  priority level, and then will be given a budget as described in the first
  bullet. That happens whether or not the task's original base priority was
  a Round Robin priority.]}
@end{Ramification}

@end{Itemize}

@end{Runtime}

@begin{ImplReq}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00333-01],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[An implementation shall allow, for a single
partition, both the task dispatching policy to be specified as
Round_Robin_Within_Priorities and also the locking policy (see
@RefSecNum{Priority Ceiling Locking}) to be specified as Ceiling_Locking.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is the preferred combination of the
  Round_Robin_Within_Priorities policy with a locking policy, and we
  want that combination to be portable.]}
@end{Reason}
@end{ImplReq}

@begin{DocReq}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[An implementation shall document the quantum values
supported.]}
@ChgDocReq{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[The quantum values supported for round robin dispatching.]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[An implementation shall document the accuracy with
which it detects the exhaustion of the budget of a task.]}
@ChgDocReq{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[The accuracy of the detection of the exhaustion of the budget of a task
for round robin dispatching.]}]}

@end{DocReq}

@begin{Notes}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[Due to implementation constraints, the quantum
value returned by Actual_Quantum might not be identical to that set with
Set_Quantum.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
@ChgAdded{Version=[2],Text=[A task that executes continuously with an inherited
priority will not be subject to round robin dispatching.]}

@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Policy Round_Robin_Within_Priorities and package
  Dispatching.Round_Robin are new.]}
@end{Extend95}


@RMNewPageVer{Version=[2]}@Comment{For printed RM Ada 2005}
@LabeledAddedSubClause{Version=[2],Name=[Earliest Deadline First Dispatching]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[The deadline of a task is an indication of the
urgency of the task; it represents a point on an ideal physical time line.
The deadline might affect how resources are allocated to the task.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@ChgAdded{Version=[2],Text=[This clause defines a package for representing the
deadline of a task and a dispatching policy that defines Earliest Deadline
First (EDF) dispatching. @Chg{Version=[3],New=[An aspect],Old=[A pragma]}
is defined to assign an initial deadline to a task.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[2],Text=[This @Chg{Version=[3],New=[aspect],Old=[pragma]}
  is the only way of assigning an
  initial deadline to a task so that its activation can be controlled by EDF
  scheduling. This is similar to the way
  @Chg{Version=[3],New=[aspect],Old=[pragma]} Priority is used to give an
  initial priority to a task.]}
@end{Discussion}

@end{Intro}

@begin{MetaRules}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[To predict the behavior of a multi-tasking program
it is necessary to control access to the processor which is preemptive, and
shared objects which are usually non-preemptive and embodied in protected
objects. Two common dispatching policies for the processor are fixed priority
and EDF. The most effective control over shared objects is via preemption
levels. With a pure priority scheme a single notion of priority is used for
processor dispatching and preemption levels. With EDF and similar schemes
priority is used for preemption levels (only), with another measure used for
dispatching. T.P. Baker showed (@i<Real-Time Systems>, March 1991, vol. 3, num.
1, @i<Stack-Based Scheduling of Realtime Processes>) that for EDF a newly
released task should only preempt the currently running task if it has an
earlier deadline and a higher preemption level than any currently
@lquotes@;locked@rquotes protected object. The rules of this clause implement
this scheme including the case where the newly released task should execute
before some existing tasks but not preempt the currently executing task.]}
@end{MetaRules}

@begin{NotIso}
@ChgAdded{Version=[3],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 3
through 6 were moved to @RefSec{Obsolescent Features}.>}]}@Comment{This message
should be deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@begin{Syntax}

@begin{SyntaxText}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Type=[Leading],Keepnext=[T],Text=[@Chg{Version=[2],New=[The
form of a @nt{pragma} Relative_Deadline is as follows:],Old=[]}]}
@end{SyntaxText}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@DeletedPragmaSyn<Version=[3],InitialVersion=[2],@ChgDeleted{Version=[3],
Text=`@Chg{Version=[2],New=[@key{pragma} @prag<Relative_Deadline> (@SynI{relative_deadline_}@Syn2{expression});],Old=[]}'}>

@end{Syntax}

@begin{Resolution}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[@Chg{Version=[2],New=[The expected type for
@SynI{relative_deadline_}@nt{expression} is Real_Time.Time_Span.],Old=[]}]}

@end{Resolution}

@begin{Legality}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[@Chg{Version=[2],New=[A Relative_Deadline pragma
is allowed only immediately within a @nt{task_definition} or
the @nt{declarative_part} of a @nt{subprogram_body}. At most one such pragma
shall appear within a given construct.],Old=[]}]}

@end{Legality}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[The @SynI{policy_}@nt{identifier}
EDF_Across_Priorities is a task dispatching
policy.@Chg{Version=[3],New=[@Defn2{Term=[task dispatching policy],
Sec=(EDF_Across_Priorities)}@Defn{EDF_Across_Priorities task dispatching policy}],
Old=[]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The following
language-defined library package exists:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{with} Ada.Real_Time;
@key{with} Ada.Task_Identification;
@key{package} Ada.Dispatching.EDF @key{is}@ChildUnit{Parent=[Ada.Dispatching],Child=[EDF]}
  @key{subtype} @AdaSubtypeDefn{Name=[Deadline],Of=[Time]} @key{is} Ada.Real_Time.Time;
  @AdaObjDefn{Default_Deadline} : @key{constant} Deadline :=
              Ada.Real_Time.Time_Last;
  @key{procedure} @AdaSubDefn{Set_Deadline} (D : @key{in} Deadline;
              T : @key{in} Ada.Task_Identification.Task_Id :=
              Ada.Task_Identification.Current_Task);
  @key{procedure} @AdaSubDefn{Delay_Until_And_Set_Deadline} (
              Delay_Until_Time : @key{in} Ada.Real_Time.Time;
              Deadline_Offset : @key{in} Ada.Real_Time.Time_Span);
  @key{function} @AdaSubDefn{Get_Deadline} (T : Ada.Task_Identification.Task_Id :=
              Ada.Task_Identification.Current_Task) @key{return} Deadline;
@key{end} Ada.Dispatching.EDF;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For a task type (including the
anonymous type of a @nt{single_task_declaration}) or subprogram, the following
language-defined representation aspect may be specified:]}

@begin{Description}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Relative_Deadline@\The aspect
Relative_Deadline is an @nt{expression}, which shall be of
type Real_Time.Time_Span.@AspectDefn{Relative_Deadline}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Relative_Deadline],
    Text=[@ChgAdded{Version=[3],Text=[Task parameter used in Earliest Deadline
      First Dispatching.]}]}

@end{Description}

@end{StaticSem}

@begin{Legality}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[The Relative_Deadline aspect shall not be specified
on a task interface type.]}
@end{Legality}

@begin{LinkTime}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[If the EDF_Across_Priorities policy is specified
for a partition, then the Ceiling_Locking policy (see
@RefSecNum{Priority Ceiling Locking}) shall also be
specified for the partition.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[If the EDF_Across_Priorities policy appears in a
Priority_Specific_Dispatching pragma
(see @RefSecNum{Task Dispatching Pragmas})
in a partition, then the
Ceiling_Locking policy (see @RefSecNum{Priority Ceiling Locking}) shall also
be specified for the partition.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Unlike the other language-defined dispatching
  policies, the semantic description of EDF_Across_Priorities assumes
  Ceiling_Locking (and a ceiling priority) in order to make the mapping between
  deadlines and priorities work. Thus, we require both policies to be specified
  if EDF is used in the partition.]}
@end{Reason}

@end{LinkTime}

@begin{RunTime}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[The],Old=[A]}
Relative_Deadline @Chg{Version=[3],New=[aspect],Old=[pragma]} has no effect
if it @Chg{Version=[3],New=[is specified for],
Old=[occurs in the @nt{declarative_part} of the @nt{subprogram_body} of]} a
subprogram other than the main subprogram.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@ChgAdded{Version=[2],Text=[The initial absolute deadline of a task
@Chg{Version=[3],New=[for which aspect],Old=[containing
pragma]} Relative_Deadline@Chg{Version=[3],New=[ is specified],Old=[]} is
the value of Real_Time.Clock +
@Chg{Version=[3],New=[the @nt{expression} that is the value of the
aspect],Old=[@SynI{relative_deadline_}@nt{expression}]}, where
@Chg{Version=[3],New=[this entire expression, including ],Old=[]}the
call of Real_Time.Clock@Chg{Version=[3],New=[, is evaluated],Old=[ is made]}
between task creation and the start of its activation. If
@Chg{Version=[3],New=[the aspect],Old=[there is no]}
Relative_Deadline @Chg{Version=[3],New=[is not specified,],Old=[pragma]}
then the initial absolute deadline of a task is the
value of Default_Deadline. The environment task is also given
an initial deadline by this rule@Chg{Version=[3],New=[, using the value of
the Relative_Deadline aspect of the main subprogram (if any)],Old=[]}.]}

@begin{TheProof}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The environment task is a normal task by
  @RefSecNum{Program Execution}, so of course this rule applies to it.]}
@end{TheProof}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[The procedure Set_Deadline changes the absolute
deadline of the task to D. The function Get_Deadline returns the absolute
deadline of the task.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[The procedure Delay_Until_And_Set_Deadline delays
the calling task until time Delay_Until_Time. When the task becomes runnable
again it will have deadline Delay_Until_Time + Deadline_Offset.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[On a system with a single processor, the setting of
the deadline of a task to the new value occurs immediately at the first point
that is outside the execution of a protected action. If the task is currently
on a ready queue it is removed and re-entered on to the ready queue determined
by the rules defined below.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[When EDF_Across_Priorities is specified for
priority range @i<Low>..@i<High> all ready queues in this range are ordered by
deadline. The task at the head of a queue is the one with the earliest
deadline.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[A task dispatching point occurs
for the currently running task @i<T> to
which policy EDF_Across_Priorities applies:]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[when a change to the deadline of @i<T> occurs;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[there is a task on the ready queue for the
active priority of @i<T> with a deadline earlier than the deadline of @i<T>; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[there is a nonempty ready queue for that processor
with a higher priority than the active priority of the running task.]}

@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[In these cases, the currently running task is said
to be preempted and is returned to
the ready queue for its active priority.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For a task @i<T> to which policy
EDF_Across_Priorities applies, the base priority is not a source of
priority inheritance; the active priority when first activated or
while it is blocked is defined as the maximum of the following:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[the lowest priority in the range specified as
EDF_Across_Priorities that includes the base priority of @i<T>;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[the priorities, if any, currently inherited by
@i<T>;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised], ARef=[AI05-0055-1]}
@ChgAdded{Version=[2],Text=[the highest priority @i<P>, if any, less than the
base priority of @i<T> such that one or more tasks are executing within a
protected object with ceiling priority @i<P> and task @i<T>
has an earlier deadline than all such tasks@Chg{Version=[3],New=[;
and furthermore @i<T> has an earlier deadline than all other tasks on
ready queues with priorities in the given EDF_Across_Priorities range that
are strictly less than @i<P>],Old=[]}.]}
@end{Itemize}

@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The active priority of @i<T> might be lower than
its base priority.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[When a task @i<T> is first activated or becomes
unblocked, it is added to the ready queue corresponding to this active
priority. Until it becomes blocked again, the active priority of @i<T>
remains no less than this value; it will exceed this value only while it is
inheriting a higher priority.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[These rules ensure that a task executing in
  a protected object is preempted only by a task with a shorter deadline and a
  higher base priority. This matches the traditional preemption level
  description without the need to define a new kind of protected object
  locking.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[When the setting of the base priority of a ready
task takes effect and the new priority is in a range specified as
EDF_Across_Priorities, the task is added to the ready queue
corresponding to its new active priority, as determined above.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[For all the operations defined in Dispatching.EDF,
Tasking_Error is raised if the task identified by T has terminated.
Program_Error is raised if the value of T is Null_Task_Id.]}

@end{RunTime}

@begin{Bounded}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
If EDF_Across_Priorities is specified for priority range @i<Low>..@i<High>, it
is a bounded error to declare a protected object with ceiling priority
@i<Low> or to assign the value @i<Low> to attribute 'Priority. In either case
either Program_Error is raised or the ceiling of the protected
object is assigned the value @i<Low>+1.]}

@end{Bounded}

@begin{Erron}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=(erroneous execution),Sec=(cause)}
If a value of Task_Id is passed as a parameter to any of the subprograms
of this package and the corresponding task object no longer exists,
the execution of the program is erroneous.]}

@end{Erron}

@begin{DocReq}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[On a multiprocessor, the implementation shall
document any conditions that cause the completion of the setting of the deadline
of a task to be delayed later than what is specified for a single processor.]}
@ChgDocReq{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Any conditions that cause the completion of the setting of the deadline
of a task to be delayed for a multiprocessor.]}]}
@end{DocReq}

@begin{Notes}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[If two adjacent priority ranges, @i<A>..@i<B> and
@i<B>+1..@i<C> are specified to have policy
EDF_Across_Priorities@Chg{Version=[3],New=[,],Old=[]} then
this is not equivalent to this policy being
specified for the single range, @i<A>..@i<C>.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[The above rules implement the preemption-level
protocol (also called Stack Resource Policy protocol) for resource sharing
under EDF dispatching. The preemption-level for a task is denoted by its base
priority. The definition of a ceiling preemption-level for a protected object
follows the existing rules for ceiling locking.]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
  @ChgAdded{Version=[2],Text=[An implementation may support additional
  dispatching policies by replacing absolute deadline with an alternative
  measure of urgency.]}
@end{ImplNote}

@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Policy EDF_Across_Priorities and package Dispatching.EDF are new.]}
@end{Extend95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Aspect Relative_Deadline is new; @nt{pragma} Relative_Deadline
  is now obsolescent.]}
@end{Extend2005}

@begin{Diffword2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0055-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected definition
  of active priority to avoid deadline inversion in an unusual case.]}
@end{Diffword2005}


@LabeledClause{Priority Ceiling Locking}

@begin{Intro}
@Redundant[This clause specifies the interactions between priority task
scheduling and protected object ceilings. This interaction is based on
the concept of the @i{ceiling priority} of a protected object.]
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
@Leading@Keepnext@;The form of a @nt{pragma} Locking_Policy is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Locking_Policy)(@SynI{policy_}@Syn2{identifier});'
@end{Syntax}

@begin{Legality}

The @SynI{policy_}@Syn2{identifier} shall either be Ceiling_Locking
or an implementation-defined @Syn2{identifier}.
@ImplDef{Implementation-defined @SynI{policy_}@Syn2{identifier}s allowed
in a @nt{pragma} Locking_Policy.}

@end{Legality}

@begin{LinkTime}

@PDefn2{Term=[configuration pragma], Sec=(Locking_Policy)}
@PDefn2{Term=[pragma, configuration], Sec=(Locking_Policy)}
A Locking_Policy pragma is a configuration pragma.

@end{LinkTime}

@begin{RunTime}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0073],ARef=[AI95-00091-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00327-01]}
@Defn{locking policy}
@Redundant[A locking policy specifies the details of protected object
locking. @Chg{Version=[2],New=[All protected objects have a priority.
The locking policy specifies the meaning of
the priority of a],Old=[These rules specify whether or not]} protected @Chg{Version=[2],
New=[object],Old=[objects have
priorities]}, and the relationships between these priorities and
task priorities. In addition, the policy specifies the state of a task
when it executes a protected action, and how its active priority is
affected by the locking.]
The @i{locking policy} is specified by a Locking_Policy pragma. For
implementation-defined locking policies, the @Chg{Version=[2],New=[meaning of
the priority of],Old=[effect of a Priority or
Interrupt_Priority pragma on]} a protected object is
implementation defined.
If no Locking_Policy pragma @Chg{New=[applies to],Old=[appears in]} any
of the program units comprising a partition, the locking policy for that
partition, as well as the @Chg{Version=[2],New=[meaning of
the priority of],Old=[effect of specifying either a Priority or
Interrupt_Priority pragma for]} a protected object, are implementation defined.
@Chg{Version=[2],New=[@Defn2{Term=[Priority],Sec=[of a protected object]}],Old=[]}

@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The locking policy if no Locking_Policy pragma applies to any unit of
a partition.]}]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00327-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0229-1]}
@ChgAdded{Version=[2],Text=[The @nt{expression} @Chg{Version=[3],New=[specified
for the],Old=[of a]} Priority or Interrupt_Priority
@Chg{Version=[3],New=[aspect],Old=[pragma]}
(see @RefSecNum{Task Priorities}) is evaluated as part of the creation
of the corresponding
protected object and converted to the subtype System.Any_Priority or
System.Interrupt_Priority, respectively. The value of the expression is the
initial priority of the corresponding protected object. If no Priority or
Interrupt_Priority @Chg{Version=[3],New=[aspect is specified for],Old=[pragma
applies to]} a protected object, the initial priority
is specified by the locking policy.
@Chg{Version=[3],New=[@PDefn2{Term=[implicit subtype conversion],Sec=(Priority aspect)}
@PDefn2{Term=[implicit subtype conversion],Sec=(Interrupt_Priority aspect)}],
Old=[@PDefn2{Term=[implicit subtype conversion],Sec=(pragma Priority)}
@PDefn2{Term=[implicit subtype conversion],Sec=(pragma Interrupt_Priority)}]}]}

@Leading@;There is one predefined locking policy, Ceiling_Locking; this policy is
defined as follows:@Chg{Version=[3],New=[@Defn2{Term=[locking policy],
Sec=(Ceiling_Locking)}@Defn{Ceiling_Locking locking policy}],
Old=[]}
@begin{itemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00327-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Defn2{Term=[ceiling priority], Sec=(of a protected object)}
Every protected object has a @i{ceiling priority}, which
is determined by either a Priority or Interrupt_Priority
@Chg{Version=[3],New=[aspect],Old=[pragma]} as
defined in @RefSecNum{Task Priorities}@Chg{Version=[2],New=[, or by
assignment to the Priority attribute as described
in @RefSecNum{Dynamic Priorities for Protected Objects}],Old=[]}.
The ceiling priority of a protected object (or ceiling, for short) is an
upper bound on the active priority a task can have when
it calls protected operations of that protected object.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00327-01]}
The @Chg{Version=[2],New=[initial ceiling priority of a],Old=[@nt{expression}
of a Priority or Interrupt_Priority pragma is evaluated as part of the creation of the corresponding]}
protected object
@Chg{Version=[2],New=[is
equal to the initial priority for that object.],Old=[and converted
to the subtype System.Any_Priority or System.Interrupt_Priority, respectively.
The value of the @nt{expression} is the ceiling priority of
the corresponding protected object.
@PDefn2{Term=[implicit subtype conversion],Sec=(pragma Priority)}
@PDefn2{Term=[implicit subtype conversion],Sec=(pragma Interrupt_Priority)}]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00327-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
If an Interrupt_Handler or Attach_Handler @Chg{Version=[3],New=[aspect],
Old=[pragma]} (see @RefSecNum{Protected Procedure Handlers})
@Chg{Version=[3],New=[is specified for a protected subprogram of a
protected type that does not have the ],Old=[appears in a
@nt{protected_definition} without an]} Interrupt_Priority
@Chg{Version=[3],New=[aspect specified],Old=[pragma]}, the
@Chg{Version=[2],New=[initial],Old=[ceiling]} priority of protected objects
of that type is implementation defined,
but in the range of the subtype System.Interrupt_Priority.
@ImplDef{Default ceiling priorities.}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00327-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
If @Chg{Version=[3],New=[neither aspect],Old=[no @nt{pragma}]}
Priority@Chg{Version=[3],New=[ nor],Old=[,]}
Interrupt_Priority@Chg{Version=[3],New=[],Old=[, Interrupt_Handler,
or Attach_Handler]} is specified
@Chg{Version=[3],New=[for a protected type, and no protected subprogram
of the type has aspect Interrupt_Handler or
Attach_Handler specified],Old=[in the
@nt{protected_definition}]}, then the
@Chg{Version=[2],New=[initial],Old=[ceiling]} priority of the corresponding
protected object is System.Priority'Last.

While a task executes a protected action, it inherits the ceiling
priority of the corresponding protected object.

@IndexCheck{Ceiling_Check}
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
When a task calls a protected operation, a check is made that its active
priority is not higher than the ceiling of the corresponding protected object;
Program_Error is raised if this check fails.

@end{Itemize}

@end{RunTime}

@begin{Bounded}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00327-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[Following any change of priority,
it is a bounded error for the active priority of any task with a call queued on
an entry of a protected object to be higher than the ceiling priority of the
protected object.
@PDefn2{Term=(bounded error),Sec=(cause)}
In this case one of the following applies:]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[at any time prior to executing the entry body
Program_Error is raised in the calling task;
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[when the entry is open the entry body is executed
at the ceiling priority of the protected object;]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[when the entry is open the entry body is executed
at the ceiling priority of the protected object and then Program_Error is
raised in the calling task; or
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[when the entry is open the entry body
is executed at the ceiling priority of the protected object that was in effect
when the entry call was queued.]}
@end{Itemize}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Added]}@Comment{This note was moved along with the above rules}
@ChgAdded{Version=[2],Text=[Note that the error is @lquotes@;blamed@rquotes@;
on the task that did the entry call, not the task that changed the
priority of the task or protected object.
This seems to make sense for the case of changing the priority of a task
blocked on a call, since if the Set_Priority had happened a
little bit sooner, before the task queued a call,
the entry-calling task would get the error.
Similarly, there is no reason not to raise the priority of a
task that is executing in an @nt{abortable_part}, so long as its
priority is lowered before it gets to the end and needs to cancel the
call.
The priority might need to be lowered to allow it to remove the call
from the entry queue,
in order to avoid violating the ceiling.
This seems relatively harmless, since there is an error,
and the task is about to start raising an exception anyway.]}
@end{Ramification}
@end{Bounded}

@begin{ImplPerm}

The implementation is allowed to round all ceilings in a certain
subrange of System.Priority or System.Interrupt_Priority up to
the top of that subrange, uniformly.
@begin{Discussion}
For example, an implementation might use Priority'Last for all ceilings
in Priority, and Interrupt_Priority'Last for all ceilings in
Interrupt_Priority.
This would be equivalent to having two ceiling priorities for protected objects,
@lquotes@;nonpreemptible@rquotes@; and @lquotes@;noninterruptible@rquotes@;, and is an allowed behavior.

Note that the implementation cannot choose a subrange that crosses the
boundary between normal and interrupt priorities.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00256-01]}
Implementations are allowed to define other locking policies,
but need not support more than one @Chg{Version=[2],New=[locking],Old=[such]}
policy per partition.

@Redundant[Since implementations are allowed to place restrictions
on code that runs at an interrupt-level active priority
(see @RefSecNum{Protected Procedure Handlers}
and @RefSecNum{The Task Dispatching Model}),
the implementation may implement a language feature in terms
of a protected object with an implementation-defined ceiling,
but the ceiling shall be no less than Priority'Last.]
@ImplDef{The ceiling of any protected object used internally by the
implementation.}
@begin{TheProof}
This permission follows from the fact that
the implementation can place restrictions on interrupt
handlers and on any other code that runs at an interrupt-level
active priority.

The implementation might protect a storage pool with a
protected object whose ceiling is Priority'Last, which would cause
@nt{allocator}s to fail when evaluated at interrupt priority.
Note that the ceiling of such an object has to be at least
Priority'Last,
since there is no permission for @nt{allocator}s to fail when evaluated at
a noninterrupt priority.
@end{TheProof}

@end{ImplPerm}

@begin{ImplAdvice}

The implementation should use names that end with
@lquotes@;_Locking@rquotes@; for implementation-defined locking policies.

@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Names that end with @lquotes@;_Locking@rquotes@; should be used for
implementation-defined locking policies.]}]}

@end{ImplAdvice}

@begin{Notes}

While a task executes in a protected action, it can be preempted
only by tasks whose active priorities are higher than the
ceiling priority of the protected object.

If a protected object has a ceiling priority in the range
of Interrupt_Priority, certain interrupts are blocked while
protected actions of that object execute. In the extreme, if
the ceiling is Interrupt_Priority'Last, all blockable interrupts
are blocked during that time.

The ceiling priority of a protected object has to be in the
Interrupt_Priority range if one of its procedures is to be used as
an interrupt handler (see @RefSecNum{Interrupt Support}).

When specifying the ceiling of a protected object, one should
choose a value that is at least as high as the highest active priority
at which tasks can be executing when they call
protected operations of that object. In determining this
value the following factors, which can affect active priority,
should be considered: the effect of Set_Priority, nested
protected operations, entry calls, task activation, and other
implementation-defined factors.

Attaching a protected procedure whose ceiling is below the
interrupt hardware priority to an interrupt causes the execution of the
program to be erroneous
(see @RefSecNum{Protected Procedure Handlers}).

On a single processor implementation, the ceiling priority
rules guarantee that there is no possibility of deadlock involving
only protected subprograms (excluding the case where a protected operation
calls another protected operation on the same protected object).

@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00327-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  All protected objects now have a priority, which
  is the value of the Priority attribute of
  @RefSecNum{Dynamic Priorities for Protected Objects}. How this value
  is interpreted depends on the locking policy; for instance, the ceiling
  priority is derived from this value when the locking policy is
  Ceiling_Locking.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0073],ARef=[AI95-00091-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the wording to
  reflect that pragma Locking_Policy cannot be inside of a program unit.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00256-01]}
  @ChgAdded{Version=[2],Text=[Clarified that an implementation need support
  only one locking policy (of any kind, language-defined or otherwise)
  per partition.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00327-01]}
  @ChgAdded{Version=[2],Text=[The bounded error for the priority of a task
  being higher than the ceiling of an object it is currently in was moved here
  from @RefSecNum{Dynamic Priorities}, so that it applies no matter how the
  situation arises.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[Revised to use aspects Priority and
  Interrupt_Priority as @nt{pragma}s
  Priority and Interrupt_Priority are now obsolescent.]}
@end{DiffWord2005}


@RMNewPageVer{Version=[2]}@Comment{For printed RM Ada 2005}
@LabeledClause{Entry Queuing Policies}

@begin{Intro}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0074],ARef=[AI95-00068-01]}
@Redundant[@Defn{queuing policy}
This clause specifies a mechanism for a user to choose an entry
@i{queuing policy}. It also defines @Chg{New=[two],Old=[one]}
such polic@Chg{New=[ies],Old=[y]}. Other policies are implementation defined.]
@ImplDef{Implementation-defined queuing policies.}
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
@Leading@Keepnext@;The form of a @nt{pragma} Queuing_Policy is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Queuing_Policy)(@SynI{policy_}@Syn2{identifier});'
@end{Syntax}

@begin{Legality}

The @SynI{policy_}@Syn2{identifier} shall be either FIFO_Queuing,
Priority_Queuing or an implementation-defined @Syn2{identifier}.

@end{Legality}

@begin{LinkTime}

@PDefn2{Term=[configuration pragma], Sec=(Queuing_Policy)}
@PDefn2{Term=[pragma, configuration], Sec=(Queuing_Policy)}
A Queuing_Policy pragma is a configuration pragma.

@end{LinkTime}

@begin{RunTime}

@Defn{queuing policy}
@Redundant[A @i{queuing policy} governs the order in which tasks are queued
for entry service, and the order in which different entry queues are
considered for service.]
The queuing policy is specified by a Queuing_Policy pragma.
@begin{Ramification}
The queuing policy includes entry queuing order,
the choice among open alternatives of a @nt{selective_accept},
and the choice among queued entry calls of
a protected object when more than one @nt{entry_barrier} @nt{condition} is True.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00355-01]}
Two queuing policies, FIFO_Queuing and Priority_Queuing,
are language defined. If no Queuing_Policy pragma
@Chg{Version=[2],New=[applies to],Old=[appears in]} any of the program units
comprising the partition, the queuing policy for that partition
is FIFO_Queuing.@Chg{Version=[3],New=[@Defn2{Term=[queuing policy],
Sec=(FIFO_Queuing)}@Defn{FIFO_Queuing queuing policy}],
Old=[]} The rules for this policy are specified in
@RefSecNum{Entry Calls} and @RefSecNum{Selective Accept}.

@Leading@Keepnext@;The Priority_Queuing policy is defined as
follows:@Chg{Version=[3],New=[@Defn2{Term=[queuing policy],
Sec=(Priority_Queuing)}@Defn{Priority_Queuing queuing policy}],
Old=[]}@begin{itemize}

@Defn{priority of an entry call}
The calls to an entry @Redundant[(including a member of an entry family)]
are queued in an order consistent with the priorities of the calls. The
@i{priority of an entry call} is initialized from the active
priority of the calling task at the time
the call is made, but can change later. Within the same priority,
the order is consistent with the calling (or requeuing,
or priority setting) time (that is, a FIFO order).

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0075],ARef=[AI95-00205-01]}
After a call is first queued, changes to the active priority of a task do
not affect the priority of the call, unless the base priority of the task is
set@Chg{New=[ while the task is blocked on an entry call],Old=[]}.

When the base priority of a task is set (see @RefSecNum{Dynamic Priorities}),
if the task is blocked on an entry call, and the call is queued,
the priority of the call is updated to the new active priority of the
calling task. This causes the call to be removed from and then reinserted in
the queue at the new active priority.
@begin{Reason}
A task is blocked on an entry call if the entry call is simple,
conditional, or timed.
If the call came from the @nt{triggering_statement} of an
@nt{asynchronous_select}, or a requeue thereof,
then the task is not blocked on that call;
such calls do not have their priority updated.
Thus, there can exist many queued calls from a given task
(caused by many nested ATC's),
but a task can be blocked on only one call at a time.

A previous version of Ada 9X required queue reordering in the
@nt{asynchronous_select} case as well.
If the call corresponds to a @lquotes@;synchronous@rquotes@; entry call, then the task
is blocked while queued, and it makes good sense to move it up in the
queue if its priority is raised.

However, if the entry call is @lquotes@;asynchronous,@rquotes@; that is, it is
due to an @nt{asynchronous_select} whose @nt{triggering_statement}
is an entry call, then the task is not waiting for this
entry call, so the placement of the entry call on the
queue is irrelevant to the rate at which the task proceeds.

Furthermore, when an entry is used for @nt{asynchronous_select}s,
it is almost certain to be a @lquotes@;broadcast@rquotes@; entry or have
only one caller at a time. For example, if the entry is
used to notify tasks of a mode switch, then all tasks on the
entry queue would be signaled when the mode changes. Similarly,
if it is indicating some interrupting event such as a control-C,
all tasks sensitive to the interrupt will want to be informed
that the event occurred. Hence, the order on such a queue is
essentially irrelevant.

Given the above, it seems an unnecessary semantic and implementation
complexity to specify that asynchronous queued calls are moved in
response to dynamic priority changes. Furthermore, it is somewhat
inconsistent, since the call was originally queued based on the active
priority of the task, but dynamic priority changes are changing the base
priority of the task, and only indirectly the active priority. We say
explicitly that asynchronous queued calls are not affected by normal
changes in active priority during the execution of an
@nt{abortable_part}. Saying that, if a change in the base priority
affects the active priority, then we do want the calls reordered, would
be inconsistent.
It would also require the implementation to maintain a readily
accessible list of all queued calls which would not otherwise be
necessary.

Several rules were removed or simplified when we changed the rules so
that calls due to @nt{asynchronous_select}s are never moved due to
intervening changes in active priority, be they due to protected
actions, some other priority inheritance, or changes in the base
priority.
@end{Reason}

When more than one @nt{condition} of an @nt{entry_barrier} of a protected
object becomes True, and more than one of the respective queues is nonempty,
the call with the highest priority is selected. If more than one such
call has the same priority, the call that is queued on the entry whose
declaration is first in textual order in the @nt{protected_definition} is
selected. For members of the same entry family,
the one with the lower family index is selected.

If the expiration time of two or more open
@nt{delay_alternative}s is the same and no other
@nt{accept_alternative}s are open, the
@nt{sequence_of_statements} of the @nt{delay_alternative} that is
first in textual order in the @nt{selective_accept} is executed.

When more than one alternative of a @nt{selective_accept} is
open and has queued calls, an alternative whose queue has the highest-priority
call at its head is selected.
If two or more open alternatives have equal-priority queued calls,
then a call on the entry in the @nt{accept_alternative} that is
first in textual order in the @nt{selective_accept}
is selected.

@end{itemize}

@end{RunTime}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00256-01]}
Implementations are allowed to define other queuing policies, but
need not support more than one @Chg{Version=[2],New=[queuing],Old=[such]}
policy per partition.
@begin{Discussion}
  @ChgRef{Version=[1],Kind=[Added],Ref=[8652/0116],ARef=[AI95-00069-01]}
  @ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00256-01]}
  @ChgAdded{Version=[1],Text=[This rule is really redundant, as
  @RefSecNum(Pragmas and Program Units) allows an implementation to limit the
  use of configuration pragmas to an empty environment. In that case, there
  would be no way to have multiple policies in a partition.@Chg{Version=[2],New=[],
  Old=[ In any case, the
  wording here really ought to be "...more than one queuing policy per
  partition.", since this part of the rule applies to all queuing policies, not
  just implementation-defined ones.]}]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00188-02]}
@ChgAdded{Version=[2],Text=[Implementations are allowed to defer the reordering
of entry queues following a change of base priority of a task blocked on the
entry call if it is not practical to reorder the queue immediately.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[Priority change is immediate, but the effect of the
change on entry queues can be deferred. That is necessary in order to implement
priority changes on top of a non-Ada kernel.]}
@end{Reason}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[The reordering should occur as soon as the blocked
task can itself perform the reinsertion into the entry queue.]}
@end{Discussion}
@end{ImplPerm}

@begin{ImplAdvice}

The implementation should use names that end with
@lquotes@;_Queuing@rquotes@; for implementation-defined queuing policies.

@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Names that end with @lquotes@;_Queuing@rquotes@; should be used for
implementation-defined queuing policies.]}]}

@end{ImplAdvice}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0074],ARef=[AI95-00068-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the number of
  queuing policies defined.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0075],ARef=[AI95-00205-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected so that a call of
  Set_Priority in an abortable part does not change the priority of the
  triggering entry call.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00188-02]}
  @ChgAdded{Version=[2],Text=[Added a permission to defer queue reordering
  when the base priority of a task is changed. This is a counterpart to
  stronger requirements on the implementation of priority change.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00256-01]}
  @ChgAdded{Version=[2],Text=[Clarified that an implementation need support
  only one queuing policy (of any kind, language-defined or otherwise)
  per partition.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
  @ChgAdded{Version=[2],Text=[Fixed wording to make clear that @nt{pragma}
  never appears inside of a unit; rather it @lquotes@;applies to@rquotes the
  unit.]}
@end{DiffWord95}


@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledClause{Dynamic Priorities}


@begin{Intro}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00327-01]}
  @ChgAdded{Version=[2],Text=[@Redundant[This clause describes how the
  priority of an entity can be modified or queried at run time.]]}
@end{Intro}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00327-01]}
  @ChgAdded{Version=[2],Text=[This clause is turned into two subclauses.
  This clause introduction is new.]}
@end{DiffWord95}


@LabeledAddedSubClause{Version=[2],Name=[Dynamic Priorities for Tasks]}

@begin{Intro}
@Redundant[This clause describes how the base priority of a task can be
modified or queried at run time.]
@end{Intro}

@begin{StaticSem}
@Leading@Keepnext@;The following language-defined library package exists:
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00362-01]}
@key[with] System;
@key[with] Ada.Task_Identification; @RI{-- See @RefSecNum[The Package Task_Identification]}
@key[package] Ada.Dynamic_Priorities @key[is]@ChildUnit{Parent=[Ada],Child=[Dynamic_Priorities]}@Chg{Version=[2],New=[
    @key[pragma] Preelaborate(Dynamic_Priorities);],Old=[]}

    @key[procedure] @AdaSubDefn{Set_Priority}(Priority : @key[in] System.Any_Priority;
                           T : @key[in] Ada.Task_Identification.Task_Id :=
                           Ada.Task_Identification.Current_Task);

    @key[function] @AdaSubDefn{Get_Priority} (T : Ada.Task_Identification.Task_Id :=
                           Ada.Task_Identification.Current_Task)
                           @key[return] System.Any_Priority;

@key[end] Ada.Dynamic_Priorities;
@end{example}
@end{StaticSem}

@begin{RunTime}
The procedure Set_Priority sets the base priority of the specified task
to the specified Priority value.
Set_Priority has no effect if the task is terminated.

The function Get_Priority returns T's current base priority.
@Defn2{Term=[Tasking_Error],Sec=(raised by failure of run-time check)}
Tasking_Error is raised if the task is terminated.
@begin{Reason}
There is no harm in setting the priority of a terminated task.
A previous version of Ada 9X made this a run-time error.
However, there is little difference between setting the priority of a
terminated task, and setting the priority of a task that is about to
terminate very soon;
neither case should be an error.
Furthermore, the run-time check is not necessarily feasible to implement
on all systems, since priority changes might be deferred due to
inter-processor communication overhead,
so the error might not be detected until after Set_Priority has
returned.

However, we wish to allow implementations to avoid storing @lquotes@;extra@rquotes@;
information about terminated tasks.
Therefore, we make Get_Priority of a terminated task raise an exception;
the implementation need not continue to store the priority of a task
that has terminated.
@end{Reason}

@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
Program_Error is raised by Set_Priority and Get_Priority if T is equal
to Null_Task_Id.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00188-02]}
@Chg{Version=[2],New=[On a system with a single processor, the setting of],
Old=[Setting]} the @Chg{Version=[2],New=[],Old=[task's ]}base
priority@Chg{Version=[2],New=[ of a task @i{T}],Old=[]}
to the new value @Chg{Version=[2],
New=[occurs immediately at the first point when @i{T} is
outside the execution of],Old=[takes place as soon
as is practical but not while the task is performing]} a
protected action@Chg{Version=[2],New=[],Old=[.
This setting occurs no later then the next abort completion point of
the task T
(see @RefSecNum{Abort of a Task - Abort of a Sequence of Statements})]}.

@begin{ImplNote}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00188-02]}
@Chg{Version=[2],New=[The priority change is immediate if the target task
is on a delay queue or a ready queue outside of a protected action.
However, consider when],Old=[When]}
Set_Priority is called by a task T1 to set the priority of T2,
if T2 is blocked, waiting on an entry call queued on a protected object,
the entry queue needs to be reordered.
Since T1 might have a priority that is higher than the ceiling of the
protected object, T1 cannot, in general, do the reordering.
One way to implement this is to wake T2 up and have T2 do the work.
This is similar to the disentangling of queues that needs to happen when
a high-priority task aborts a lower-priority task,
which might have a call queued on a protected object with a low
ceiling.@Chg{Version=[2],New=[ We have an @ImplPermName in
@RefSecNum{Entry Queuing Policies} to allow this implementation. We could
have required an immediate priority change if on a ready queue during a
protected action, but that would have required extra checks for ceiling
violations to meet @BoundedName requirements of
@RefSecNum{Priority Ceiling Locking} and potentially could cause a protected
action to be abandoned in the middle (by raising Program_Error). That seems
bad.],Old=[]}
@end{ImplNote}
@begin{Reason}
@Leading@;A previous version of Ada 9X made it a run-time error
for a high-priority task to set the priority of a lower-priority
task that has a queued call on a protected object with a low ceiling.
This was changed because:
@begin{Itemize}
The check was not feasible to implement on all systems,
since priority changes might be deferred due to
inter-processor communication overhead.
The calling task would continue to execute without finding out whether
the operation succeeded or not.

The run-time check would tend to cause intermittent system failures @em
how is the caller supposed to know whether the other task happens to
have a queued call at any given time? Consider for example an
interrupt that needs to trigger a priority change in some task.
The interrupt handler could not safely call Set_Priority without knowing
exactly what the other task is doing,
or without severely restricting the ceilings used in the system.
If the interrupt handler wants to hand the job off to a third task whose
job is to call Set_Priority, this won't help, because one would normally
want the third task to have high priority.
@end{Itemize}
@end{Reason}
@end{RunTime}

@begin{Bounded}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00327-01]}
@ChgDeleted{Version=[2],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
If a task is blocked on a protected entry call, and the call is queued,
it is a bounded error to raise its base priority
above the ceiling priority of the corresponding
protected object.
When an entry call is cancelled, it is a bounded error
if the priority of the calling task is higher than
the ceiling priority of the corresponding
protected object.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
In either of these cases,
either Program_Error is raised in the task that called the entry,
or its priority is temporarily lowered,
or both, or neither.]}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[Note that the error is @lquotes@;blamed@rquotes@; on the task that did the entry call,
not the task that called Set_Priority.
This seems to make sense for the case of a task blocked on a call,
since if the Set_Priority had happened a
little bit sooner, before the task queued a call,
the entry-calling task would get the error.
In the other case, there is no reason not to raise the priority of a
task that is executing in an @nt{abortable_part}, so long as its
priority is lowered before it gets to the end and needs to cancel the
call.
The priority might need to be lowered to allow it to remove the call
from the entry queue,
in order to avoid violating the ceiling.
This seems relatively harmless, since there is an error,
and the task is about to start raising an exception anyway.]}
@end{Ramification}
@end{Bounded}
@begin{NotIso}
@ChgAdded{Version=[3],Noparanum=[T],Text=[@Shrink{@i<Paragraph 11 was
deleted.>}]}@Comment{This message should be deleted if the paragraphs
are ever renumbered.}
@end{NotIso}

@begin{Erron}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
If any subprogram in this package is called with a parameter T that
specifies a task object that no longer exists, the execution of the
program is erroneous.
@begin{Ramification}
Note that this rule overrides the above rule saying that
Program_Error is raised on Get_Priority of a terminated task.
If the task object still exists, and the task is terminated,
Get_Priority raises Program_Error.
However, if the task object no longer exists,
calling Get_Priority causes erroneous execution.
@end{Ramification}
@end{Erron}

@begin{DocReq}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00188-02]}
@ChgAdded{Version=[2],Text=[On a multiprocessor, the implementation shall
document any conditions that cause the completion of the setting of the
priority of a task to be delayed later than what is specified for a
single processor.]}
@ChgDocReq{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Any conditions that cause the completion of the setting of the priority
of a task to be delayed for a multiprocessor.]}]}
@end{DocReq}

@begin{Metrics}
@Leading@;The implementation shall document the following metric:
@begin{Itemize}
The execution time of a call to Set_Priority, for the nonpreempting case,
in processor clock cycles. This is measured for a call that modifies the
priority of a ready task that is not running (which
cannot be the calling one), where the new
base priority of the affected task is lower than the active priority of the
calling task, and the affected task is not on any entry queue and is not
executing a protected operation.
@end{Itemize}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The metrics for Set_Priority.]}]}
@end{Metrics}

@begin{Notes}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00321-01]}
Setting a task's base priority affects task dispatching. First, it can
change the task's active priority. Second, under the @Chg{Version=[2],
New=[FIFO_Within_Priorities],Old=[standard
task dispatching]} policy it always causes the task to move to
the tail of the ready queue corresponding to its active priority,
even if the new base priority is unchanged.

Under the priority queuing policy, setting a task's base
priority has an effect on a queued entry call
if the task is blocked waiting for the call. That is, setting the
base priority of a task causes the priority of a queued entry
call from that task to be updated and the call to be removed and
then reinserted in the entry queue at the new priority
(see @RefSecNum{Entry Queuing Policies}),
unless the call originated from the @nt{triggering_statement} of an
@nt{asynchronous_select}.

The effect of two or more Set_Priority calls executed in parallel on
the same task is defined as executing these calls in some serial order.

@begin{TheProof}
This follows from the general reentrancy requirements stated near the
beginning of @RefSec{Predefined Language Environment}.
@end{TheProof}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
The rule for when Tasking_Error is raised for Set_Priority or Get_Priority is
different from the rule for when Tasking_Error is raised on an
entry call (see @RefSecNum{Entry Calls}). In particular,
@Chg{Version=[3],New=[],Old=[setting or ]}querying the priority of
a completed or an abnormal
task is allowed, so long as the task is not yet
terminated@Chg{Version=[3],New=[, and setting the priority of a task
is allowed for any task state (including for terminated tasks)],Old=[]}.

Changing the priorities of a set of tasks can be performed by a
series of calls to Set_Priority for each task separately. For
this to work reliably, it should be done within a protected
operation that has high enough ceiling priority to guarantee that
the operation completes without being preempted by any of the
affected tasks.

@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00188-02]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @b[Amendment Correction:] Priority changes are
  now required to be done immediately so long as the target task is not on an
  entry queue.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00362-01]}
  @ChgAdded{Version=[2],Text=[Dynamic_Priorities is now Preelaborated,
  so it can be used in preelaborated units.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00327-01]}
  @ChgAdded{Version=[2],Text=[This Ada 95 clause was turned into a subclause.
  The paragraph numbers are the same as those for
  @RefSecNum{Dynamic Priorities} in Ada 95.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00321-01]}
  @ChgAdded{Version=[2],Text=[There is no @lquotes@;standard@rquotes policy
  anymore, so that phrase was replaced by the name of a specific policy in
  the notes.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00327-01]}
  @ChgAdded{Version=[2],Text=[The bounded error for the priority of a task
  being higher than the ceiling of an object it is currently in was moved
  to @RefSecNum{Priority Ceiling Locking}, so that it applies no matter how
  the situation arises.]}
@end{DiffWord95}


@RMNewPageVer{Version=[2]}@Comment{For printed RM Ada 2005}
@LabeledAddedSubClause{Version=[2],Name=[Dynamic Priorities for Protected Objects]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00327-01]}
@ChgAdded{Version=[2],Text=[This clause specifies how the priority of a
protected object can be modified or queried at run time.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00327-01]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[The following attribute
is defined for @PrefixType{a @nt{prefix} P that denotes a protected object}:]}

@begin(Description)
@ChgAttribute{Version=[2],Kind=[AddedNormal],ChginAnnex=[T],
  Leading=<F>, Prefix=<P>, AttrName=<Priority>, ARef=[AI95-00327-01],
  Text=[@Chg{Version=[2],New=[Denotes a non-aliased component of the
  protected object P. This component is of type System.Any_Priority and its
  value is the priority of P. P'Priority denotes a variable if and only if P
  denotes a variable. A reference to this attribute shall appear only
  within the body of P.],Old=[]}]}
@EndPrefixType{}
@end{Description}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00327-01]}
@ChgAdded{Version=[2],Text=[The initial value of this attribute is
the initial value of the priority of the protected object@Redundant[, and can
be changed by an assignment].]}
@end{StaticSem}

@begin{Runtime}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00327-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[If the locking policy Ceiling_Locking (see
@RefSecNum{Priority Ceiling Locking}) is in effect@Chg{Version=[3],New=[,],Old=[]}
then the ceiling priority of a protected object @i<P> is set to the value of
@i<P>'Priority at the end of each protected action of @i<P>.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00445-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@ChgAdded{Version=[2],Text=[If the locking policy Ceiling_Locking is in effect,
then for a protected object @i<P> with either an Attach_Handler or
Interrupt_Handler @Chg{Version=[3],New=[aspect specified
for],Old=[pragma applying to]} one of its procedures, a check is made
that the value to be assigned to @i<P>'Priority is in the range
System.Interrupt_Priority. If the check fails, Program_Error is
raised.@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}]}
@end{Runtime}

@begin{Metrics}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00327-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The implementation shall document
the following metric:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The difference in execution time of
calls to the following procedures in protected object P:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key<protected> P @key<is>
   @key<procedure> Do_Not_Set_Ceiling (Pr : System.Any_Priority);
   @key<procedure> Set_Ceiling (Pr : System.Any_Priority);
@key<end> P;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key<protected body> P @key<is>
   @key<procedure> Do_Not_Set_Ceiling (Pr : System.Any_Priority) @key<is>
   @key<begin>
      @key<null>;
   @key<end>;
   @key<procedure> Set_Ceiling (Pr : System.Any_Priority) @key<is>
   @key<begin>
      P'Priority := Pr;
   @key<end>;
@key<end> P;]}
@end{Example}
@end{Itemize}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The metrics for setting the priority of a protected object.]}]}
@end{Metrics}

@begin{Notes}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00327-01]}
@ChgAdded{Version=[2],Text=[Since P'Priority is a normal variable, the value
following an assignment to the attribute immediately reflects the new value
even though its impact on the ceiling priority of P is postponed until
completion of the protected action in which it is executed.]}
@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00327-01],ARef=[AI95-00445-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The ability to dynamically change and query the priority of a protected
  object is new.]}
@end{Extend95}


@RMNewPageVer{Version=[2]}@Comment{For printed RM Ada 2005}
@LabeledClause{Preemptive Abort}

@begin{Intro}
@Redundant[This clause specifies requirements on the immediacy with
which an aborted construct is completed.]
@end{Intro}

@begin{RunTime}

On a system with a single processor, an aborted construct is completed
immediately at the first point that is outside the execution of an
abort-deferred operation.

@end{RunTime}

@begin{DocReq}
On a multiprocessor, the implementation shall document any conditions that
cause the completion of an aborted construct to be delayed later than
what is specified for a single processor.
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[On a multiprocessor, any conditions that
cause the completion of an aborted construct to be delayed later than
what is specified for a single processor.]}]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[On a multiprocessor, any conditions that
cause the completion of an aborted construct to be delayed later than
what is specified for a single processor.]}]}
@end{DocReq}

@begin{Metrics}
@Leading@;The implementation shall document the following metrics:
@begin{Itemize}
The execution time, in processor clock cycles, that it takes for an
@nt{abort_statement} to cause the completion of the aborted task.
This is measured in a situation where a task T2 preempts task T1
and aborts T1. T1 does not have any finalization code. T2 shall
verify that T1 has terminated, by means of the Terminated attribute.

On a multiprocessor, an upper bound in seconds,
on the time that the completion of an aborted task can be delayed beyond
the point that it is required for a single processor.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
An upper bound on the execution time of an
@nt{asynchronous_select}, in processor clock cycles. This is measured
between a point immediately before a task
T1 executes a protected operation Pr.Set that makes the @nt{condition}
of an @nt{entry_barrier} Pr.Wait @Chg{Version=[2],
New=[True],Old=[true]}, and the point where task T2 resumes
execution immediately after an entry call to Pr.Wait in an
@nt{asynchronous_select}. T1 preempts T2 while
T2 is executing the abortable part, and then blocks itself so that
T2 can execute. The execution time of T1 is measured separately,
and subtracted.

An upper bound on the execution time of an
@nt{asynchronous_select},
in the case that no asynchronous transfer of control takes
place. This is measured between a point immediately before a task
executes the @nt{asynchronous_select} with a nonnull abortable
part, and the point where the task continues execution immediately after
it. The execution time of the abortable part is subtracted.
@end{Itemize}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The metrics for aborts.]}]}
@end{Metrics}

@begin{ImplAdvice}

Even though the @nt{abort_statement} is included in the list of
potentially blocking operations
(see @RefSecNum{Protected Subprograms and Protected Actions}),
it is recommended that this statement be implemented in a way that
never requires the task executing the @nt{abort_statement} to
block.

@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The @nt{abort_statement} should not require the task executing the
statement to block.]}]}

On a multi-processor,
the delay associated with aborting a task on another processor
should be bounded;
the implementation should use periodic polling,
if necessary, to achieve this.

@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[On a multi-processor,
the delay associated with aborting a task on another processor
should be bounded.]}]}

@end{ImplAdvice}

@begin{Notes}

Abortion does not change the active or base priority of the aborted task.

Abortion cannot be more immediate than is allowed by the rules for
deferral of abortion during finalization and in protected actions.

@end{Notes}


@LabeledClause{Tasking Restrictions}

@begin{Intro}
@Redundant[This clause defines restrictions that can be used with a
pragma Restrictions (see @RefSecNum{Pragma Restrictions and Pragma Profile}) to facilitate the
construction of highly efficient tasking run-time systems.]
@end{Intro}

@begin{StaticSem}
@Leading@;The following @SynI{restriction_}@nt{identifier}s are language defined:
@begin{Description}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0013-1],ARef=[AI05-0216-1]}
@Defn2{Term=[restrictions],Sec=(No_Task_Hierarchy)}@Chg{Version=[3],New=[@Defn{No_Task_Hierarchy restriction}],
   Old=[]}No_Task_Hierarchy @\@Chg{Version=[3],
  New=[No task depends on a master other than the library-level master],Old=[All
  (nonenvironment) tasks depend directly on
  the environment task of the partition]}.

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0216-1]}
@ChgAdded{Version=[3],Text=[This is equivalent to saying @ldquote@;no task
   depends on a master other than the
   master that is the execution of the body of the environment task of the
   partition@rdquote, but it is much easier to understand. This is a
   post-compilation check, which can be checked at compile-time.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0013-1]}
@ChgAdded{Version=[3],Text=[This disallows any function returning an
    object with a task part or coextension, even if called at the library level,
    as such a task would temporarily depend on a nested master (the master
    of the return statement), which is disallowed by this restriction.]}
@end{Ramification}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0042],ARef=[AI95-00130-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00360-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0013-1]}
@Defn2{Term=[restrictions],Sec=(No_Nested_Finalization)}@Chg{Version=[3],New=[@Defn{No_Nested_Finalization restriction}],
   Old=[]}No_Nested_Finalization @\Objects
  @Chg{Version=[2],New=[of a type that needs finalization (see
  @RefSecNum{Assignment and Finalization})],Old=[with
  controlled@Chg{New=[, protected, or task],Old=[]} parts]}
  @Chg{Version=[3],New=[are],Old=[and
  access types that designate @Chg{Version=[2],New=[a type that needs
  finalization],Old=[such objects@Chg{New=[,],Old=[]}]} shall be]}
  declared only at library level.@Chg{Version=[3],New=[ If an access type
  does not have library-level accessibility, then there are
  no @nt{allocator}s of the type where the type determined by the
  @nt{subtype_mark} of the @nt{subtype_indication} or
  @nt{qualified_expression} needs finalization.],Old=[]}
    @begin{Ramification}
@ChgRef{Version=[1],Kind=[Deleted],Ref=[8652/0042],ARef=[AI95-00130-01]}
    @ChgNote{This is no longer true.}
    @ChgDeleted{Version=[1],Text=[Note that protected types with entries and
    interrupt-handling protected types have nontrivial finalization actions.
    However, this restriction does not restrict those things.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0013-1]}
    @ChgAdded{Version=[3],Text=[The second sentence prevents the declaration
    of objects of access types which would require nested finalization. It
    also prevents the declarations of coextensions that need
    finalization in a nested scope. The latter cannot be done by preventing
    the declaration of the objects, as it is not necessarily known if the
    coextension type needs finalization (it could be a limited view).]}
    @end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0211-1]}
@Defn2{Term=[restrictions],Sec=(No_Abort_Statements)}@Chg{Version=[3],New=[@Defn{No_Abort_Statements restriction}],
   Old=[]}No_Abort_Statements @\There are no @nt{abort_statement}s, and there
@Chg{Version=[3],New=[is no use of a @nt{name} denoting],Old=[are no calls on]}
Task_Identification.Abort_Task.

@Defn2{Term=[restrictions],Sec=(No_Terminate_Alternatives)}@Chg{Version=[3],New=[@Defn{No_Terminate_Alternatives restriction}],
   Old=[]}No_Terminate_Alternatives @\There are no @nt{selective_accept}s with
                        @nt{terminate_alternative}s.

@Defn2{Term=[restrictions],Sec=(No_Task_Allocators)}@Chg{Version=[3],New=[@Defn{No_Task_Allocators restriction}],
   Old=[]}No_Task_Allocators @\There are no @nt{allocator}s for task types or types
                        containing task subcomponents.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0224-1]}
@ChgAdded{Version=[3],NoPrefix=[T],Text=[In the case of an initialized
   @nt{allocator} of an access type whose designated type is class-wide and
   limited, a check is made that the specific type of the allocated object has
   no task subcomponents. Program_Error is raised if this check
   fails.@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}]}
   @Comment{We don't index this "check" as it doesn't have a name.}

@Defn2{Term=[restrictions],Sec=(No_Implicit_Heap_Allocations)}@Chg{Version=[3],New=[@Defn{No_Implicit_Heap_Allocations restriction}],
   Old=[]}No_Implicit_Heap_Allocations @\There are no operations that implicitly require
                        heap storage allocation to be performed by the
                        implementation. The operations that implicitly
                        require heap storage allocation are
                        implementation defined.
@ImplDef{Any operations that implicitly
require heap storage allocation.}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00327-01]}
No_Dynamic_Priorities @\There are no semantic dependences on the package
                Dynamic_Priorities@Chg{Version=[2],New=[, and no occurrences
                of the attribute Priority],Old=[]}.
@Defn2{Term=[restrictions],Sec=(No_Dynamic_Priorities)}@Chg{Version=[3],New=[@Defn{No_Dynamic_Priorities restriction}],
Old=[]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00305-01],ARef=[AI95-00394-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0013-1],ARef=[AI05-0211-1]}
@Chg{Version=[2],New=[@Defn2{Term=[restrictions],
   Sec=(No_Dynamic_Attachment)}@Chg{Version=[3],New=[@Defn{No_Dynamic_Attachment restriction}],
   Old=[]}No_Dynamic_Attachment],
Old=[@Defn2{Term=[restrictions],Sec=(No_Asynchronous_Control)}No_Asynchronous_Control]}
    @\There
    @Chg{Version=[2],New=[is no @Chg{Version=[3],New=[use of a @nt{name}
    denoting],Old=[call to]} any of the operations defined
    in package Interrupts (Is_Reserved, Is_Attached, Current_Handler,
    Attach_Handler, Exchange_Handler, Detach_Handler, and Reference).],
    Old=[are no semantic dependences on the package Asynchronous_Task_Control.]}
@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0013-1]}
@ChgAdded{Version=[3],Text=[This includes 'Access and 'Address of any
    of these operations, as well as inherited versions of these operations.]}
@end{Ramification}


@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00305-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0013-1]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[restrictions],Sec=(No_Local_Protected_Objects)}@Chg{Version=[3],New=[@Defn{No_Local_Protected_Objects restriction}],
   Old=[]}No_Local_Protected_Objects @\Protected
   objects @Chg{Version=[2],New=[are],Old=[shall be]} declared only at
   library level.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00297-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0013-1]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[restrictions],Sec=(No_Local_Timing_Events)}@Chg{Version=[3],New=[@Defn{No_Local_Timing_Events restriction}],
   Old=[]}No_Local_Timing_Events @\Timing_Events
   @Chg{Version=[2],New=[are],Old=[shall be]} declared only at library level.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00305-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[restrictions],Sec=(No_Protected_Type_Allocators)}@Chg{Version=[3],New=[@Defn{No_Protected_Type_Allocators restriction}],
   Old=[]}No_Protected_Type_Allocators @\There
   are no @nt{allocator}s for protected types or types
   containing protected type subcomponents.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0224-1]}
@ChgAdded{Version=[3],NoPrefix=[T],Text=[In the case of an initialized
   @nt{allocator} of an access type whose designated type is class-wide and
   limited, a check is made that the specific type of the allocated object has
   no protected subcomponents. Program_Error is raised if this check
   fails.@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}]}
   @Comment{We don't index this "check" as it doesn't have a name.}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00305-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0211-1]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[restrictions],Sec=(No_Relative_Delay)}@Chg{Version=[3],New=[@Defn{No_Relative_Delay restriction}],
   Old=[]}No_Relative_Delay @\There
   are no @nt{delay_relative_statement}s@Chg{Version=[3],New=[, and there is no
   use of a @nt{name} that denotes the Timing_Events.Set_Handler
   subprogram that has a Time_Span parameter],Old=[]}.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00305-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}@Comment{Number changed due to insertion above}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[restrictions],Sec=(No_Requeue_Statements)}@Chg{Version=[3],New=[@Defn{No_Requeue_Statements restriction}],
   Old=[]}No_Requeue_Statements @\There
   are no @nt{requeue_statement}s.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00305-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}@Comment{Number changed due to insertion above}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[restrictions],Sec=(No_Select_Statements)}@Chg{Version=[3],New=[@Defn{No_Select_Statements restriction}],
   Old=[]}No_Select_Statements @\There
   are no @nt{select_statement}s.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00394-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0211-1]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[restrictions],Sec=(No_Specific_Termination_Handlers)}@Chg{Version=[3],New=[@Defn{No_Specific_Termination_Handlers restriction}],
   Old=[]}No_Specific_Termination_Handlers @\There
  @Chg{Version=[3],New=[is no use of a @nt{name} denoting],Old=[are no calls to]}
  the Set_Specific_Handler and Specific_Handler subprograms
  in Task_Termination.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00305-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0013-1]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[restrictions],Sec=(Simple_Barriers)}@Chg{Version=[3],New=[@Defn{Simple_Barriers restriction}],
   Old=[]}Simple_Barriers @\The
   Boolean expression in @Chg{Version=[3],New=[each],Old=[an]} entry barrier
   @Chg{Version=[3],New=[is],Old=[shall be]} either a static
   @Chg{Version=[3],New=[],Old=[Boolean ]}expression or a
   @Chg{Version=[3],New=[name that statically denotes a],Old=[Boolean]}
   component of the enclosing protected object.]}

@end{Description}

@Leading@;The following @SynI{restriction_parameter_}@nt{identifier}s are
language defined:
@begin{Description}
@Defn2{Term=[restrictions],Sec=(Max_Select_Alternatives)}@Chg{Version=[3],New=[@Defn{Max_Select_Alternatives restriction}],
   Old=[]}Max_Select_Alternatives @\Specifies the maximum number of alternatives
                in a @nt{selective_accept}.

@Defn2{Term=[restrictions],Sec=(Max_Task_Entries)}@Chg{Version=[3],New=[@Defn{Max_Task_Entries restriction}],
   Old=[]}Max_Task_Entries @\Specifies the maximum number of entries per task.
    The bounds of every entry family
    of a task unit shall be static,
    or shall be defined by a discriminant of a subtype whose
    corresponding bound is static.
    @Redundant[A value of zero indicates that no rendezvous
    are possible.]

Max_Protected_Entries @\Specifies the maximum number of entries per
    protected type.
    The bounds of every entry family
    of a protected unit shall be static,
    or shall be defined by a discriminant of a subtype whose
    corresponding bound is static.
@Defn2{Term=[restrictions],Sec=(Max_Protected_Entries)}@Chg{Version=[3],New=[@Defn{Max_Protected_Entries restriction}],
Old=[]}
@end{Description}
@end{StaticSem}

@begin{RunTime}

@ChgRef{Version=[1],Kind=[Deleted],Ref=[8652/0076],ARef=[AI95-00067-01]}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00305-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The following
@SynI{restriction_}@nt{identifier} is language defined:]}@Comment{Use ChgAdded so
we get conditional Leading.}@Chg{Version=[1],New=[],Old=[If the following restrictions
are violated, the behavior is implementation defined.
@IndexCheck{Storage_Check}
@Defn2{Term=[Storage_Error],Sec=(raised by failure of run-time check)}
If an implementation chooses to detect such a violation,
Storage_Error should be raised.]}

@begin{Description}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00305-01],ARef=[AI95-00394-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[restrictions],Sec=(No_Task_Termination)}@Chg{Version=[3],New=[@Defn{No_Task_Termination restriction}],
   Old=[]}No_Task_Termination @\All
   tasks are nonterminating. It is implementation-defined what happens if
   a task attempts to terminate. If there is a fall-back handler (see C.7.3)
   set for the partition it should be called when the first task attempts to
   terminate.]}
@ChgImplDef{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[When restriction No_Task_Termination applies to a partition, what
  happens when a task terminates.]}]}
@end{Description}

@Leading@;The following @SynI{restriction_parameter_}@nt{identifier}s are
language defined:
@begin{Description}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0076],ARef=[AI95-00067-01]}
@Defn2{Term=[restrictions],Sec=(Max_Storage_At_Blocking)}@Chg{Version=[3],New=[@Defn{Max_Storage_At_Blocking restriction}],
   Old=[]}Max_Storage_At_Blocking @\Specifies
  the maximum portion @redundant[(in storage elements)]
  of a task's Storage_Size that can be retained by a blocked task@Chg{New=[.
  If an implementation chooses to detect a violation of this
  restriction, Storage_Error should be raised;
  @IndexCheck{Storage_Check}
  @Defn2{Term=[Storage_Error],Sec=(raised by failure of run-time check)}
  otherwise, the behavior is implementation defined],Old=[]}.
@ChgImplDef{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[The behavior when restriction Max_Storage_At_Blocking is violated.]}]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0076],ARef=[AI95-00067-01]}
@Defn2{Term=[restrictions],Sec=(Max_Asynchronous_Select_Nesting)}@Chg{Version=[3],New=[@Defn{Max_Asynchronous_Select_Nesting restriction}],
   Old=[]}Max_Asynchronous_Select_Nesting @\Specifies
  the maximum dynamic nesting level of @nt{asynchronous_select}s.
  A value of zero prevents the use of any @nt{asynchronous_@!select}@Chg{New=[ and,
  if a program contains an @nt{asynchronous_@!select}, it is illegal.
  @ChgNote{Part of the previous rule is redundant, but it is a different part
  [all of it for Old; from "prevents" to "and," for New] for each. So we omit it.}
  If an implementation chooses to detect a violation of this
  restriction for values other than zero, Storage_Error should be raised;
  @IndexCheck{Storage_Check}
  @Defn2{Term=[Storage_Error],Sec=(raised by failure of run-time check)}
  otherwise, the behavior is implementation defined],Old=[]}.
@ChgImplDef{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[The behavior when restriction Max_Asynchronous_Select_Nesting is violated.]}]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0076],ARef=[AI95-00067-01]}
@Defn2{Term=[restrictions],Sec=(Max_Tasks)}@Chg{Version=[3],New=[@Defn{Max_Tasks restriction}],
   Old=[]}Max_Tasks @\Specifies the maximum
  number of task creations that may be executed over the lifetime of a
  partition, not counting the creation of the environment task@Chg{New=[.
  A value of zero prevents any task creation and, if a program contains a
  task creation, it is illegal. If an implementation chooses to detect a
  violation of this restriction, Storage_Error should be raised;
  @IndexCheck{Storage_Check}
  @Defn2{Term=[Storage_Error],Sec=(raised by failure of run-time check)}
  otherwise, the behavior is implementation defined],Old=[]}.
  @begin{Ramification}
     Note that this is not a limit on the
     number of tasks active at a given time;
     it is a limit on the total number of task creations that occur.
  @end{Ramification}
  @begin{ImplNote}
     We envision an implementation approach that places TCBs or pointers
     to them in a fixed-size table, and never reuses table elements.
  @end{ImplNote}
@ChgImplDef{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[The behavior when restriction Max_Tasks is violated.]}]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00305-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[restrictions],
Sec=(Max_Entry_Queue_Length)}@Chg{Version=[3],New=[@Defn{Max_Entry_Queue_Length restriction}],
Old=[]}Max_Entry_Queue_Length @\Max_Entry_Queue_Length
  defines the maximum number of calls
  that are queued on an entry. Violation of this restriction
  results in the raising of Program_Error at the point of the call or
  requeue.@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0189-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[restrictions],
Sec=(No_Standard_Allocators_After_Elaboration)}@Defn{No_Standard_Allocators_After_Elaboration restriction}
No_Standard_Allocators_After_Elaboration @\Specifies that an @nt{allocator} using
a standard storage pool (see @RefSecNum{Storage Management}) shall not occur
within a parameterless library subprogram, nor within the
@nt{handled_sequence_of_statements} of a task body. For the purposes of this rule, an
@nt{allocator} of a type derived from a formal access type does not use a standard
storage pool.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0189-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],NoPrefix=[T],Text=[At run time, Storage_Error is raised if
an @nt{allocator} using a standard storage pool is evaluated after the elaboration of
the @nt{library_item}s of the partition has completed.@Defn2{Term=[Storage_Error],
Sec=(raised by failure of run-time check)}]}
@end{Description}

It is implementation defined whether the use of pragma Restrictions
results in a reduction in executable program size, storage requirements,
or execution time. If possible, the implementation should provide
quantitative descriptions of such effects for each restriction.
@ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],
Text=[@Chg{Version=[2],
New=[Whether the use of],Old=[Implementation-defined aspects of]}
pragma Restrictions@Chg{Version=[2],New=[ results in a reduction in
program code or data size or execution time],Old=[]}.]}
@end{RunTime}

@begin{ImplAdvice}
When feasible, the implementation should take advantage of the specified
restrictions to produce a more efficient implementation.

@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[When feasible, specified restrictions should be used to produce a more
efficient implementation.]}]}
@end{ImplAdvice}

@begin{Notes}
The above Storage_Checks can be suppressed with pragma Suppress.
@end{Notes}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00360-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @b[Amendment Correction:] The No_Nested_Finalization is now defined in terms
  of types that need finalization. These types include a variety of
  language-defined types that @i<might> be implemented with a controlled type.
  If the restriction No_Nested_Finalization (see
  @RefSecNum{Tasking Restrictions}) applies to the partition, and one of these
  language-defined types does not have a controlled part, it will not be
  allowed in local objects in Ada 2005 whereas it would be allowed in original
  Ada 95. Such code is not portable, as other Ada compilers may have had a
  controlled part, and thus would be illegal under the restriction.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01],ARef=[AI95-00305-01],ARef=[AI95-00394-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Restrictions No_Dynamic_Attachment, No_Local_Protected_Objects,
  No_Protected_Type_Allocators, No_Local_Timing_Events, No_Relative_Delay,
  No_Requeue_Statement, No_Select_Statements, No_Specific_Termination_Handlers,
  No_Task_Termination, Max_Entry_Queue_Length, and Simple_Barriers are newly
  added to Ada.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0042],ARef=[AI95-00130-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that
  No_Nested_Finalization covered task and protected parts as well.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0076],ARef=[AI95-00067-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Changed the description of
  Max_Tasks and Max_Asynchronous_Select_Nested to eliminate conflicts with the
  High Integrity Annex (see @RefSecNum{High Integrity Restrictions}).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00327-01]}
  @ChgAdded{Version=[2],Text=[Added using of the new Priority attribute to
  the restriction No_Dynamic_Priorities.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00394-01]}
  @ChgAdded{Version=[2],Text=[Restriction No_Asynchronous_Control is now
  obsolescent.]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0013-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Changed so that coextensions of types that require nested finalization are
  also prohibited; this is done by prohibiting @nt{allocator}s rather than
  objects of specific access types. It seems unlikely that any program depending
  on this restriction would violate it in this blatant manner, so it is expected
  that very few programs will be affected by this change.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0211-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:>
  The restriction No_Relative_Delay was changed to include the Timing_Events
  routine that uses a relative delay. This means that a program that uses
  that routine and this restriction will now be rejected. However, such a
  program violates the spirit and intent of the restriction and as such the
  program should never have been allowed. Moreover, it is unlikely that
  any program depending on this restriction would violate it in such an obvious
  manner, so it is expected that very few programs will be affected by this
  change.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0211-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> A number of restrictions were
  changed from "no calls" on some subprogram to "no use of a @nt{name} that
  denotes" that subprogram. This closes a hole where renames, uses as the prefix
  of 'Access, and the like, would not be rejected by the restriction, possibly
  allowing backdoor access to the prohibited subprogram. A program that
  uses one of these restrictions and using such backdoor access will now be
  rejected; however, it is extremely unlikely that any program that relies
  on these restrictions would also use an end-run around the restriction, so
  it is expected that very few programs will be affected by this change.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0189-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Restriction No_Standard_Allocators_After_Elaboration is newly
  added to Ada.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0013-1],ARef=[AI05-0216-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Improved the wording
  of various restrictions to make it clearer that they prohibit
  things that would otherwise be legal, and to word them as
  definitions, not @LegalityTitle;.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0192-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added wording to explain
  how No_Task_Allocators and No_Protected_Type_Allocators are checked
  for class-wide types. This might be an extension if the compiler
  assumed the worst in the past (it is now a runtime check).]}
@end{DiffWord2005}



@LabeledClause{Monotonic Time}
@begin{Intro}
@Redundant[This clause specifies a high-resolution,
monotonic clock package.]
@end{Intro}

@begin{StaticSem}
@Leading@;The following language-defined library package exists:
@begin{example}
@key[package] Ada.Real_Time @key[is]@ChildUnit{Parent=[Ada],Child=[Real_Time]}

  @key[type] @AdaTypeDefn{Time} @key[is] @key[private];
  @AdaObjDefn{Time_First} : @key[constant] Time;
  @AdaObjDefn{Time_Last} : @key[constant] Time;
  @AdaObjDefn{Time_Unit} : @key[constant] := @RI{implementation-defined-real-number};



  @key[type] @AdaTypeDefn{Time_Span} @key[is] @key[private];
  @AdaObjDefn{Time_Span_First} : @key[constant] Time_Span;
  @AdaObjDefn{Time_Span_Last} : @key[constant] Time_Span;
  @AdaObjDefn{Time_Span_Zero} : @key[constant] Time_Span;
  @AdaObjDefn{Time_Span_Unit} : @key[constant] Time_Span;


  @AdaObjDefn{Tick} : @key[constant] Time_Span;
  @key[function] @AdaSubDefn{Clock} @key[return] Time;


  @key[function] "+" (Left : Time; Right : Time_Span) @key[return] Time;
  @key[function] "+" (Left : Time_Span; Right : Time) @key[return] Time;
  @key[function] "-" (Left : Time; Right : Time_Span) @key[return] Time;
  @key[function] "-" (Left : Time; Right : Time) @key[return] Time_Span;


  @key[function] "<" (Left, Right : Time) @key[return] Boolean;
  @key[function] "<="(Left, Right : Time) @key[return] Boolean;
  @key[function] ">" (Left, Right : Time) @key[return] Boolean;
  @key[function] ">="(Left, Right : Time) @key[return] Boolean;


  @key[function] "+" (Left, Right : Time_Span) @key[return] Time_Span;
  @key[function] "-" (Left, Right : Time_Span) @key[return] Time_Span;
  @key[function] "-" (Right : Time_Span) @key[return] Time_Span;
  @key[function] "*" (Left : Time_Span; Right : Integer) @key{return} Time_Span;
  @key[function] "*" (Left : Integer; Right : Time_Span) @key{return} Time_Span;
  @key[function] "/" (Left, Right : Time_Span) @key[return] Integer;
  @key[function] "/" (Left : Time_Span; Right : Integer) @key[return] Time_Span;

  @key[function] "@key[abs]"(Right : Time_Span) @key[return] Time_Span;

@ChgRef{Version=[1], Kind=[Deleted]}
@Chg[New=<>,Old=<@ @;@comment{Empty paragraph to hang junk paragraph number from original RM}>]

  @key[function] "<" (Left, Right : Time_Span) @key[return] Boolean;
  @key[function] "<="(Left, Right : Time_Span) @key[return] Boolean;
  @key[function] ">" (Left, Right : Time_Span) @key[return] Boolean;
  @key[function] ">="(Left, Right : Time_Span) @key[return] Boolean;


  @key[function] @AdaSubDefn{To_Duration} (TS : Time_Span) @key[return] Duration;
  @key[function] @AdaSubDefn{To_Time_Span} (D : Duration) @key[return] Time_Span;


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00386-01]}
  @key[function] @AdaSubDefn{Nanoseconds}  (NS : Integer) @key{return} Time_Span;
  @key[function] @AdaSubDefn{Microseconds} (US : Integer) @key{return} Time_Span;
  @key[function] @AdaSubDefn{Milliseconds} (MS : Integer) @key{return} Time_Span;@Chg{Version=[2],New=[
  @key[function] @AdaSubDefn{Seconds}      (S  : Integer) @key{return} Time_Span;
  @key[function] @AdaSubDefn{Minutes}      (M  : Integer) @key{return} Time_Span;],Old=[]}


  @key[type] @AdaTypeDefn{Seconds_Count} @key[is] @key[range] @RI{implementation-defined};

  @key{procedure} @AdaSubDefn{Split}(T : @key{in} Time; SC : @key{out} Seconds_Count; TS : @key{out} Time_Span);
  @key{function} @AdaSubDefn{Time_Of}(SC : Seconds_Count; TS : Time_Span) @key{return} Time;

@key[private]
   ... -- @RI{not specified by the language}
@key[end] Ada.Real_Time;
@end{example}
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Implementation-defined aspects of package Real_Time.]}]}

@Defn{real time}
In this Annex, @i{real time} is defined to be the physical time as observed
in the external environment.
The type Time is a @i{time type} as defined by
@RefSecNum{Delay Statements, Duration, and Time};
@Redundant[values of this type may be used in a
@nt{delay_until_statement}.]
Values of this type
represent segments of an ideal time line. The set of values of
the type Time corresponds one-to-one with an
implementation-defined range of mathematical integers.
@begin{Discussion}
Informally, real time is defined to be the International Atomic Time (TAI)
which is monotonic and nondecreasing. We use it here for the purpose of
discussing rate of change and monotonic behavior only. It does not imply
anything about the absolute value of Real_Time.Clock, or about Real_Time.Time
being synchronized with TAI. It is also used for real time in the metrics,
for comparison purposes.
@end{Discussion}
@begin{ImplNote}
The specification of TAI as @lquotes@;real time@rquotes@; does not preclude the
use of a simulated TAI clock for simulated execution environments.
@end{ImplNote}

@Defn{epoch}
@PDefn{unspecified}
The Time value I represents the half-open real time
interval that starts with E+I*Time_Unit and is limited by E+(I+1)*Time_Unit,
where Time_Unit is an implementation-defined real number and E is an
unspecified origin point, the @i{epoch}, that is the same
for all values of the type Time.
It is not specified by the language whether the time values are
synchronized with any standard time reference.
@Redundant[For example, E can correspond to the time of system
initialization or it can correspond to the epoch of some time standard.]
@begin{Discussion}
E itself does not have to be a proper time value.

This half-open interval I consists of all
real numbers R such that E+I*Time_Unit <= R < E+(I+1)*Time_Unit.
@end{Discussion}

Values of the type Time_Span represent length of real time
duration.
The set of values of this type corresponds one-to-one
with an implementation-defined range of mathematical integers.
The Time_Span value corresponding to the integer I
represents the real-time duration I*Time_Unit.
@begin{Reason}
The purpose of this type is similar to Standard.Duration; the idea is to
have a type with a higher resolution.
@end{Reason}
@begin{Discussion}
We looked at many possible names for this type: Real_Time.Duration,
Fine_Duration, Interval, Time_Interval_Length, Time_Measure, and more.
Each of these names had some problems, and we've finally settled for Time_Span.
@end{Discussion}

Time_First and Time_Last are the smallest and largest values of the
Time type, respectively.
Similarly, Time_Span_First and Time_Span_Last are the smallest and
largest values of the Time_Span type, respectively.

A value of type Seconds_Count represents an elapsed time,
measured in seconds,
since the epoch.

@end{StaticSem}

@begin{RunTime}

Time_Unit is the smallest amount of real time representable by the Time type;
it is expressed in seconds. Time_Span_Unit is the difference between
two successive values of the Time type. It is also the smallest positive
value of type Time_Span. Time_Unit and Time_Span_Unit represent
the same real time duration.
@Defn{clock tick}
A @i{clock tick} is a real time interval during
which the clock value (as observed by calling the Clock function) remains
constant. Tick is the average length of such intervals.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00432-01]}
The function To_Duration converts the value TS to a value of type
Duration. Similarly, the function To_Time_Span converts the value D
to a value of type Time_Span. For @Chg{Version=[2],New=[To_Duration],
Old=[both operations]}, the result is
rounded to the nearest @Chg{Version=[2],New=[value of type Duration],
Old=[exactly representable value]} (away from zero if exactly
halfway between two @Chg{Version=[2],New=[],
Old=[exactly representable ]}values).@Chg{Version=[2],New=[ If the result
is outside the range of Duration, Constraint_Error is raised. For To_Time_Span,
the value of D is first rounded to the nearest integral multiple of Time_Unit,
away from zero if exactly halfway between two multiples. If the
rounded value is outside the range of Time_Span, Constraint_Error is
raised. Otherwise, the value is converted to the type Time_Span.],Old=[]}

To_Duration(Time_Span_Zero) returns 0.0,
and To_Time_Span(0.0) returns Time_Span_Zero.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00386-01],ARef=[AI95-00432-01]}
The functions Nanoseconds, Microseconds, @Chg{Version=[2],New=[],
Old=[and ]}Milliseconds@Chg{Version=[2],New=[, Seconds, and Minutes],Old=[]}
convert the input
parameter to a value of the type Time_Span. NS, US,@Chg{Version=[2],New=[],Old=[ and]}
MS@Chg{Version=[2],New=[, S, and M],Old=[]} are interpreted as a number of
nanoseconds, microseconds,@Chg{Version=[2],New=[],Old=[ and]}
milliseconds@Chg{Version=[2],New=[, seconds, and minutes],Old=[]}
respectively.@Chg{Version=[2],New=[ The input parameter is first converted to
seconds and rounded to the nearest integral multiple of Time_Unit, ],
Old=[The result is rounded to the nearest exactly
representable value (]}away from zero if exactly halfway between two
@Chg{Version=[2],New=[multiples. If the rounded value
is outside the range of Time_Span, Constraint_Error is raised.
Otherwise, the rounded value is converted to the type Time_Span],
Old=[exactly representable values)]}.
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00432-01]}
  @ChgDeleted{Version=[2],Text=[The above does not imply that the Time_Span
  type will have to accommodate Integer'Last of milliseconds; Constraint_Error
  is allowed to be raised.]}
@end{Discussion}

The effects of the operators on Time and Time_Span are as for the
operators defined for integer types.
@begin{ImplNote}
  Though time values are modeled by integers, the types Time and
  Time_Span need not be implemented as integers.
@end{ImplNote}

The function Clock returns
the amount of time since the epoch.

The effects of the Split and Time_Of operations are defined as follows,
treating values of type
Time, Time_Span, and Seconds_Count as mathematical integers.
The effect of Split(T,SC,TS) is to set SC and TS to values
such that T*Time_Unit = SC*1.0 + TS*Time_Unit, and 0.0 <= TS*Time_Unit < 1.0.
The value returned by Time_Of(SC,TS) is the value T such that T*Time_Unit =
SC*1.0 + TS*Time_Unit.
@end{RunTime}

@begin{ImplReq}

The range of Time values shall be sufficient to uniquely
represent the range of real times from program start-up to 50 years later.
Tick shall be no greater than 1 millisecond.
Time_Unit shall be less than or equal to 20 microseconds.
@begin{ImplNote}
The required range and accuracy of Time are such that
32-bits worth of seconds and 32-bits worth of ticks in a second could be
used as the representation.
@end{ImplNote}

Time_Span_First shall be no greater than @en@;3600 seconds, and
Time_Span_Last shall be no less than 3600 seconds.
@begin{Reason}
This is equivalent to @PorM one hour and there is still room for
a two-microsecond resolution.
@end{Reason}

@Defn{clock jump}
A @i{clock jump} is the difference between two successive distinct values of
the clock (as observed by calling the Clock function). There shall be no
backward clock jumps.

@end{ImplReq}

@begin{DocReq}

The implementation shall document the values of Time_First, Time_Last,
Time_Span_@!First, Time_Span_@!Last, Time_Span_@!Unit, and Tick.
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The values of Time_First, Time_Last,
Time_Span_@!First, Time_Span_@!Last, Time_Span_@!Unit, and Tick
for package Real_Time.]}]}

The implementation shall document the properties of the underlying
time base used for the clock and for type Time,
such as the range of values supported
and any relevant aspects of the underlying hardware
or operating system facilities used.
@ChgDocReq{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[The properties of the underlying
time base used in package Real_Time.]}]}
@begin{Discussion}
If there is an underlying operating system,
this might include information about which system call is used
to implement the clock.
Otherwise, it might include information about which
hardware clock is used.
@end{Discussion}

The implementation shall document whether or not there is any synchronization
with external time references, and if such synchronization exists, the sources
of synchronization information, the frequency of synchronization, and the
synchronization method applied.
@ChgDocReq{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Any synchronization of package Real_Time with external time references.]}]}

@ChgRef{Version=[1],Kind=[Revised]}
The implementation shall document any aspects of the @Chg{New=[], Old=[the]}
@chgnote{Correct typo as noted at Potsdam ARG meeting}
external environment that could interfere with the clock behavior as defined
in this clause.
@ChgDocReq{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Any aspects of the external environment that could interfere with
package Real_Time.]}]}
@begin{Discussion}
For example, the implementation is allowed to rely on the time services of
an underlying operating system, and this operating system clock can
implement time zones or allow the clock to be reset by an operator.
This dependence has to be documented.
@end{Discussion}
@end{DocReq}

@begin{Metrics}
For the purpose of the metrics defined in this clause, real time is
defined to be the International Atomic Time (TAI).

@Leading@;The implementation shall document the following metrics:
@begin{Itemize}
An upper bound on the real-time duration of a clock tick. This is a value
D such that if t1 and t2 are any real times such that t1 < t2 and
Clock@-{t1} = Clock@-{t2} then t2 @en@; t1 <= D.

An upper bound on the size of a clock jump.

@Defn{drift rate}
An upper bound on the @i{drift rate} of Clock with respect to real time.
This is a real number D such that
@begin{display}
E*(1@en@;D) <= (Clock@-{t+E} @en@; Clock@-{t}) <= E*(1+D)
        provided that: Clock@-{t} + E*(1+D) <= Time_Last.
@end{display}

where Clock@-{t} is the value of Clock at time t, and E is a real
time duration not less than 24 hours. The value of E used for
this metric shall be reported.
@begin{Reason}
This metric is intended to provide a measurement
of the long term (cumulative) deviation; therefore, 24
hours is the
lower bound on the measurement period. On some implementations,
this is also the maximum period, since the language does not
require that the range of the type Duration be more than 24 hours.
On those implementations that support longer-range Duration, longer
measurements should be performed.
@end{Reason}

An upper bound on the execution time of a call to the Clock
function, in processor clock cycles.

Upper bounds on the execution times of the operators of the types Time
and Time_Span, in processor clock cycles.
@begin{ImplNote}
A fast implementation of the Clock function involves repeated
reading until you get the same value twice.
It is highly improbable that more than three reads will be necessary.
Arithmetic on time values should not be significantly slower
than 64-bit arithmetic in the underlying machine instruction set.
@end{ImplNote}
@end{Itemize}
@ChgDocReq{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[The metrics for package Real_Time.]}]}
@end{Metrics}

@begin{ImplPerm}
Implementations targeted to machines with word size smaller than 32
bits need not support the full range and granularity of the
Time and Time_Span types.
@begin{Discussion}
These requirements are based on machines with a word size of 32 bits.

Since the range and granularity are implementation defined, the supported
values need to be documented.
@end{Discussion}
@end{ImplPerm}

@begin{ImplAdvice}

When appropriate, implementations should provide configuration mechanisms to
change the value of Tick.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[When appropriate, mechanisms to change the value of Tick should be
provided.]}]}
@begin{Reason}
This is often needed when the compilation system was originally targeted to a
particular processor with a particular interval timer, but the customer
uses the same processor with a different interval timer.
@end{Reason}
@begin{Discussion}
Tick is a deferred constant and not a named number
specifically for this purpose.
@end{Discussion}
@begin{ImplNote}
This can be achieved either by pre-run-time configuration
tools, or by having Tick be initialized
(in the package private part)
by a function call residing in a board specific module.
@end{ImplNote}

It is recommended that Calendar.Clock and Real_Time.Clock be implemented
as transformations of the same time base.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Calendar.Clock and Real_Time.Clock should be transformations of the
same time base.]}]}

It is recommended that the @lquotes@;best@rquotes@; time base which exists in the
underlying system be available to the application through
Clock. @lquotes@;Best@rquotes@; may mean highest accuracy or largest range.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[The @lquotes@;best@rquotes@; time base which exists in the
underlying system should be available to the application through
Real_Time.Clock.]}]}

@end{ImplAdvice}

@begin{Notes}

The rules in this clause do not imply that the implementation can protect
the user from operator or installation errors which could result in the
clock being set incorrectly.

Time_Unit is the granularity of the Time type. In contrast,
Tick represents the granularity of Real_Time.Clock.
There is no requirement that these be the same.

@end{Notes}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00386-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  Functions Seconds and Minutes are @Chg{Version=[3],New=[],Old=[newly ]}added
  to Real_Time. If
  Real_Time is referenced in a @nt{use_clause}, and an entity @i<E> with a
  @nt{defining_identifier} of Seconds or Minutes is defined in a package that
  is also referenced in a @nt{use_clause}, the entity @i<E> may no longer be
  use-visible, resulting in errors. This should be rare and is easily fixed if
  it does occur.]}
@end{Incompatible95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00432-01]}
  @ChgAdded{Version=[2],Text=[Added wording explaining how and when many of
  these functions can raise Constraint_Error. While there always was an
  intent to raise Constraint_Error if the values did not fit, there never
  was any wording to that effect, and since Time_Span was a private type,
  the normal numeric type rules do not apply to it.]}
@end{DiffWord95}


@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledClause{Delay Accuracy}

@begin{Intro}
@Redundant[This clause specifies performance requirements for the
@nt{delay_statement}.
The rules apply both to @nt{delay_@!relative_@!statement} and to
@nt{delay_@!until_@!statement}. Similarly, they apply equally to a
simple @nt{delay_@!statement} and to one which appears in a
@nt{delay_@!alternative}.]
@end{Intro}

@begin{RunTime}
@Leading@;The effect of the @nt{delay_statement} for Real_Time.Time is
defined in terms of Real_Time.Clock:
@begin{itemize}

If C@-{1} is a value of Clock read before a task executes a
@nt{delay_relative_statement} with duration D, and C@-{2} is a value of
Clock read after the task resumes execution following that
@nt{delay_statement}, then C@-{2} @en@; C@-{1} >= D.

If C is a value of Clock read after a task resumes execution following a
@nt{delay_until_statement} with Real_Time.Time value T, then C >= T.
@end{itemize}

@PDefn2{Term=[potentially blocking operation],Sec=(delay_statement)}
@PDefn2{Term=[blocking, potentially],Sec=(delay_statement)}
A simple @nt{delay_statement} with a negative or zero value for the
expiration time does not cause the calling task to be blocked; it is
nevertheless a potentially blocking operation
(see @RefSecNum{Protected Subprograms and Protected Actions}).

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0004-1]}
When a @nt{delay_statement} appears in a @nt{delay_alternative} of a
@nt{timed_entry_call} the selection of the entry call is attempted,
regardless of the specified expiration time.
When a @nt{delay_statement} appears in a
@Chg{Version=[3],New=[@nt{select_alternative}],Old=[@ntf{selective_accept_alternative}]},
and a call is queued on one of the open entries, the selection of that
entry call proceeds, regardless of the value of the delay expression.
@begin{Ramification}
The effect of these requirements is that one has to always attempt a rendezvous,
regardless of the value of the delay expression. This can be tested by
issuing a @nt{timed_entry_call} with an expiration time
of zero, to an open entry.
@end{Ramification}

@end{RunTime}

@begin{DocReq}

The implementation shall document the minimum value of the delay expression
of a @nt{delay_relative_statement} that causes the task to actually be blocked.
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The minimum value of the delay expression of a
@nt{delay_relative_statement} that causes a task to actually be blocked.]}]}

The implementation shall document the minimum difference between the value of
the delay expression of a @nt{delay_until_statement} and the value of
Real_Time.Clock, that causes the task to actually be blocked.
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Implementation-defined aspects of @nt{delay_statement}s.]}]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The minimum difference between the value of the delay expression of a
@nt{delay_until_statement} and the value of Real_Time.Clock, that causes the
task to actually be blocked.]}]}

@end{DocReq}

@begin{Metrics}
@Leading@;The implementation shall document the following metrics:
@begin{Itemize}
An upper bound on the execution time, in processor clock cycles, of a
@nt{delay_relative_statement} whose requested value of the delay expression
is less than or equal to zero.

An upper bound on the execution time, in processor clock cycles, of a
@nt{delay_until_statement} whose requested value of the delay expression is
less than or equal to the value of Real_Time.Clock at the
time of executing the statement. Similarly, for Calendar.Clock.

@Defn{lateness}
@Defn{actual duration}
An upper bound on the @i{lateness} of a @nt{delay_relative_statement},
for a positive value of the delay expression, in a situation
where the task has sufficient priority to preempt the processor as
soon as it becomes ready, and does not need to
wait for any other execution resources. The upper bound is
expressed as a function of the value of the delay expression.
The lateness is obtained by subtracting the value of the delay expression
from the @i{actual duration}. The actual duration is measured from a point
immediately before a task executes the @nt{delay_statement} to a point
immediately after the task resumes execution following this statement.

An upper bound on the lateness of a @nt{delay_until_statement}, in a
situation where the value of the requested expiration time is after the time
the task begins executing the statement, the task has sufficient priority
to preempt the processor as soon as it becomes ready, and
it does not need to wait for any other execution resources. The upper
bound is expressed as a function of the difference between the requested
expiration time and the clock value at the time the statement begins
execution. The lateness of a @nt{delay_until_statement} is obtained by
subtracting the requested expiration time from the real time that the task
resumes execution following this statement.
@end{Itemize}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The metrics for delay statements.]}]}
@end{Metrics}

@begin{Notes}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00355-01]}
@ChgDeleted{Version=[2],Text=[The execution time of a @nt{delay_statement} that
does not cause the task to be blocked (e.g. @lquotes@;@key[delay]
0.0;@rquotes@; ) is of interest in situations where delays are used to achieve
voluntary round-robin task dispatching among equal-priority tasks.]}

@end{Notes}

@begin{DiffWord83}

The rules regarding a @nt{timed_entry_call} with a very small positive
Duration value, have been tightened to always require the check whether
the rendezvous is immediately possible.

@end{DiffWord83}

@begin{DiffWord95}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00355-01]}
  @ChgAdded{Version=[2],Text=[The note about @lquotes@;voluntary
  round-robin@rquote, while still true, has been deleted as potentially
  confusing as it is describing a different kind of round-robin than is defined
  by the round-robin dispatching policy.]}

@end{DiffWord95}


@LabeledClause{Synchronous Task Control}

@begin{Intro}
@Redundant[This clause describes a language-defined private semaphore
(suspension object), which can be used for @i{two-stage suspend}
operations and as a simple building block for implementing higher-level
queues.]
@end{Intro}

@begin{StaticSem}

@Leading@;The following language-defined package exists:
@begin{example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00362-01]}
@key{package} Ada.Synchronous_Task_Control @key{is}@ChildUnit{Parent=[Ada],Child=[Synchronous_Task_Control]}@Chg{Version=[2],New=[
  @key[pragma] Preelaborate(Synchronous_Task_Control);],Old=[]}

  @key{type} @AdaTypeDefn{Suspension_Object} @key{is} @key{limited} @key{private};
  @key{procedure} @AdaSubDefn{Set_True}(S : @key{in} @key{out} Suspension_Object);
  @key{procedure} @AdaSubDefn{Set_False}(S : @key{in} @key{out} Suspension_Object);
  @key{function} @AdaSubDefn{Current_State}(S : Suspension_Object) @key{return} Boolean;
  @key{procedure} @AdaSubDefn{Suspend_Until_True}(S : @key{in} @key{out} Suspension_Object);
@key{private}
     ... -- @RI{not specified by the language}
@key{end} Ada.Synchronous_Task_Control;
@end{example}

The type Suspension_Object is a by-reference type.@begin{ImplNote}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}@ChgNote{This is a real term now, let's get it right}
The implementation can ensure this by, for example, making the full view
@Chg{Version=[2],New=[an explicitly],Old=[a]}
limited record type.@end{implnote}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0168-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The following language-defined package
exists:]}
@begin{example}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0168-1]}
@ChgAdded{Version=[3],Text=[@key{package} Ada.Synchronous_Task_Control.EDF @key{is}@ChildUnit{Parent=[Ada.Synchronous_Task_Control],Child=[EDF]}
   @key{procedure} @AdaSubDefn{Suspend_Until_True_And_Set_Deadline}
      (S  : @key{in out} Suspension_Object;
       TS : @key{in}     Ada.Real_Time.Time_Span);
@key{end} Ada.Synchronous_Task_Control.EDF;]}
@end{example}

@end{StaticSem}

@begin{RunTime}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
An object of the type Suspension_Object has two visible states:
@Chg{Version=[2],New=[True],Old=[true]} and
@Chg{Version=[2],New=[False],Old=[false]}. Upon initialization,
its value is set to @Chg{Version=[2],New=[False],Old=[false]}.
@begin{Discussion}
This object is assumed to be private to the declaring task, i.e. only that
task will call Suspend_Until_True on this object, and the count of callers is
at most one. Other tasks can, of course, change and query the state of this
object.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
The operations Set_True and Set_False are atomic with respect to each other
and with respect to Suspend_Until_True; they set the state to
@Chg{Version=[2],New=[True],Old=[true]} and
@Chg{Version=[2],New=[False],Old=[false]} respectively.

Current_State returns the current state of the object.
@begin{Discussion}
This state can change immediately after the operation returns.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
The procedure Suspend_Until_True blocks the calling task until the
state of the object S is @Chg{Version=[2],New=[True],Old=[true]}; at that
point the task becomes ready
and the state of the object becomes @Chg{Version=[2],New=[False],Old=[false]}.

@PDefn2{Term=[potentially blocking operation],Sec=(Suspend_Until_True)}
@PDefn2{Term=[blocking, potentially],Sec=(Suspend_Until_True)}
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
Program_Error is raised upon calling Suspend_Until_True if another
task is already waiting on that suspension object.
Suspend_Until_True is a potentially blocking operation
(see @RefSecNum{Protected Subprograms and Protected Actions}).

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0168-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[The procedure Suspend_Until_True_And_Set_Deadline
blocks the calling task until the state of the object S is True; at that point
the task becomes ready with a deadline of Ada.Real_Time.Clock + TS, and the
state of the object becomes False. Program_Error is raised upon calling
Suspend_Until_True_And_Set_Deadline if another task is already waiting on that
suspension object. Suspend_Until_True_And_Set_Deadline is a potentially blocking
operation.]}
@end{RunTime}

@begin{ImplReq}

The implementation is required to allow the calling of Set_False and
Set_True during any protected action, even one that has its ceiling priority
in the Interrupt_Priority range.

@end{ImplReq}

@begin{Notes}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0168-1]}
@ChgAdded{Version=[3],Text=[More complex schemes, such as setting the deadline
relative to when Set_True is called, can be programmed using a protected
object.]}
@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00362-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Synchronous_Task_Control is now Preelaborated,
  so it can be used in preelaborated units.]}
@end{Extend95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0168-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}Child
  package Ada.Synchronous_Task_Control.EDF is new.]}
@end{Extend2005}


@LabeledAddedSubClause{Version=[3],Name=[Synchronous Barriers]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0174-1]}
@ChgAdded{Version=[3],Text=[This clause introduces a language-defined package to
synchronously release a group of tasks after the number of blocked tasks reaches
a specified count value.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0174-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The following
language-defined library package exists:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[package] Ada.Synchronous_Barriers @key{is}@ChildUnit{Parent=[Ada],Child=[Synchronous_Barriers]}
   @key[pragma] Preelaborate(Synchronous_Barriers);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[subtype] @AdaSubtypeDefn{Name=[Barrier_Limit],Of=[Positive]} @key[is] Positive @key[range] 1 .. @RI<implementation-defined>;]}
@ChgImplDef{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The value of Barrier_Limit'Last in Synchronous_Barriers.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Synchronous_Barrier} (Release_Threshold : Barrier_Limit) @key[is limited private];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Wait_For_Release} (The_Barrier : @key[in out] Synchronous_Barrier;
                               Notified    :    @key[out] Boolean);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[private]
   -- @RI{not specified by the language}
@key[end] Ada.Synchronous_Barriers;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0174-1]}
@ChgAdded{Version=[3],Text=[Type Synchronous_Barrier needs finalization (see
@RefSecNum{Assignment and Finalization}).]}
@end{StaticSem}

@begin{Runtime}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0174-1]}
@ChgAdded{Version=[3],Text=[Each call to Wait_For_Release blocks the calling
task until the number of blocked tasks associated with the Synchronous_Barrier
object is equal to Release_Threshold, at which time all blocked tasks are released.
Notified is set to True for one of the released tasks, and set to False for all
other released tasks.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0174-1]}
@ChgAdded{Version=[3],Text=[The mechanism for determining which task sets
Notified to True is implementation defined.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0174-1]}
@ChgAdded{Version=[3],Text=[Once all tasks have been released, a
Synchronous_Barrier object may be reused to block another Release_Threshold number
of tasks.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0174-1]}
@ChgAdded{Version=[3],Text=[As the first step of the finalization of a
Synchronous_Barrier, each blocked task is unblocked and Program_Error is raised
at the place of the call to Wait_For_Release.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0174-1]}
@ChgAdded{Version=[3],Text=[It is implementation defined whether an abnormal
task which is waiting on a Synchronous_Barrier object is aborted immediately or
aborted when the tasks waiting on the object are released.]}
@ChgImplDef{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[When an aborted task that is waiting on a Synchronous_Barrier is aborted.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0174-1]}
@ChgAdded{Version=[3],Text=[Wait_For_Release is a potentially blocking operation
(see @RefSecNum{Protected Subprograms and Protected Actions}).]}
@end{Runtime}

@begin{Bounded}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0174-1]}
@ChgAdded{Version=[3],Text=[It is a bounded error to call Wait_For_Release on a
Synchronous_Barrier object after that object is finalized. If the error is
detected, Program_Error is raised. Otherwise, the call proceeds normally, which
may leave a task blocked forever.]}
@end{Bounded}


@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0174-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The package Ada.Synchronous_Barriers is new.]}
@end{Extend2005}



@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledClause{Asynchronous Task Control}

@begin{Intro}
@Redundant[This clause introduces a language-defined package to do
asynchronous suspend/resume on tasks.
It uses a conceptual @i{held priority} value to represent the task's
@i{held} state.]
@end{Intro}

@begin{StaticSem}

@Leading@;The following language-defined library package exists:
@begin{example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00362-01]}
@key{with} Ada.Task_Identification;
@key{package} Ada.Asynchronous_Task_Control @key{is}@ChildUnit{parent=[Ada],Child=[Asynchronous_Task_Control]}@Chg{Version=[2],New=[
  @key[pragma] Preelaborate(Asynchronous_Task_Control);],Old=[]}
  @key{procedure} @AdaSubDefn{Hold}(T : @key[in] Ada.Task_Identification.Task_Id);
  @key{procedure} @AdaSubDefn{Continue}(T : @key[in] Ada.Task_Identification.Task_Id);
  @key{function} @AdaSubDefn{Is_Held}(T : Ada.Task_Identification.Task_Id)
   @key{return} Boolean;
@key{end} Ada.Asynchronous_Task_Control;
@end{example}

@end{StaticSem}

@begin{RunTime}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00357-01]}
@PDefn2{Term=[task state], Sec=(held)}
@Defn{held priority}
@Defn{idle task}
After the Hold operation has been applied to a task, the task becomes
@i{held}. For each processor there is a conceptual @i{idle task},
which is always ready. The base priority of the idle task is below
System.@!Any_@!Priority'First. The @i{held priority} is a
constant of the type @Chg{Version=[2],New=[Integer],Old=[integer]}
whose value is below the base priority of the idle task.
@begin{Discussion}
The held state should not be confused with the blocked state as defined
in @RefSecNum{Task Execution - Task Activation}; the task is still ready.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00357-01]}
@ChgAdded{Version=[2],Text=[For any priority below System.Any_Priority'First,
the task dispatching policy is FIFO_Within_Priorities.]}
@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This applies even if a Task_Dispatching_Policy
  specifies the policy for all of the priorities of the partition.]}
@end{Honest}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[A task at the held priority never runs, so it is
  not necessary to implement FIFO_Within_Priorities for systems that have only
  one policy (such as EDF_Across_Priorities).]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00357-01]}
The Hold operation sets the state of T to held. For a held
task@Chg{Version=[2],New=[, the active
priority is reevaluated as if the base priority of the task were the held
priority],Old=[: the task's own base priority does not constitute an
inheritance source
(see @RefSecNum{Task Priorities}), and the value of the held priority
is defined to be such a source instead]}.
@begin{Ramification}
For example, if T is currently inheriting priorities from other sources (e.g.
it is executing in a protected action), its active priority does not change,
and it continues to execute until it leaves the protected action.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00357-01]}
The Continue operation resets the state of T to not-held;
@Chg{Version=[2],New=[its],Old=[T's]} active priority
is then reevaluated as @Chg{Version=[2],New=[determined by the
task dispatching policy associated with its base priority.],Old=[described in
@RefSecNum{Task Priorities}.
@Redundant[This time, T's base priority is taken into account.]]}

The Is_Held function returns True if and only if T is in the held state.
@begin{Discussion}
Note that the state of T can be changed immediately after Is_Held returns.
@end{Discussion}

As part of these operations, a check is made that the task
identified by
T is not terminated.
@Defn2{Term=[Tasking_Error],Sec=(raised by failure of run-time check)}
Tasking_Error is raised if the check fails.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
Program_Error is raised if the value of T is Null_Task_Id.

@end{RunTime}

@begin{Erron}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
If any operation in this package is called with a parameter T that
specifies a task object that no longer exists, the execution of the
program is erroneous.
@end{Erron}

@begin{ImplPerm}

An implementation need not support Asynchronous_Task_Control if it is
infeasible to support it in the target environment.
@begin{Reason}
A direct implementation of the Asynchronous_Task_Control semantics using
priorities is not necessarily efficient enough.
Thus, we envision implementations that use some other mechanism to set
the @lquotes@;held@rquotes@; state.
If there is no other such mechanism,
support for Asynchronous_Task_Control might be infeasible,
because an implementation in terms of priority would require one idle
task per processor.
On some systems, programs are not supposed to know how many processors
are available,
so creating enough idle tasks would be problematic.
@end{Reason}

@end{ImplPerm}

@begin{Notes}

It is a consequence of the priority rules that held tasks cannot be dispatched
on any processor in a partition (unless they are inheriting
priorities) since their priorities are defined to be
below the priority of any idle task.

The effect of calling Get_Priority and Set_Priority on a Held task is the
same as on any other task.

Calling Hold on a held task or Continue on a non-held task has no effect.

@Leading@;The rules affecting queuing are derived from the above rules, in
addition to the normal priority rules:
@begin{itemize}

When a held task is on the ready queue, its priority is so low as to never
reach the top of the queue as long as there are other tasks on that queue.

If a task is executing in a protected action, inside a rendezvous, or is
inheriting priorities from other sources (e.g. when activated), it
continues to execute until it is no longer executing the corresponding
construct.

If a task becomes held while waiting (as a caller) for a rendezvous to
complete, the active priority of the accepting task is not affected.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0077],ARef=[AI95-00111-01]}
If a task becomes held while waiting in a @nt{selective_accept},
and a@Chg{New=[n],Old=[]} entry call is issued to one of the open entries,
the corresponding @Chg{New=[@nt{accept_@!alternative}],Old=[accept body]}
executes. When the rendezvous completes, the active
priority of the accepting task is lowered to the held priority
(unless it is still inheriting from other sources), and the task does
not execute until another Continue.

The same holds if the held task is the only task on a protected entry queue
whose barrier becomes open. The corresponding entry body executes.

@end{itemize}

@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00362-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Asynchronous_Task_Control is now Preelaborated,
  so it can be used in preelaborated units.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0077],ARef=[AI95-00111-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected to eliminate the
  use of the undefined term @lquotes@;accept body@rquotes@;.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00357-01]}
  @ChgAdded{Version=[2],Text=[The description of held tasks was changed to
  reflect that the calculation of active priorities depends on the
  dispatching policy of the base priority. Thus, the policy of the held
  priority was specified in order to avoid surprises (especially when using
  the EDF policy).]}
@end{DiffWord95}


@LabeledClause{Other Optimizations and Determinism Rules}

@begin{Intro}
@Redundant[This clause describes various requirements for
improving the response and determinism in a real-time system.]
@end{Intro}

@begin{ImplReq}

If the implementation blocks interrupts (see @RefSecNum{Interrupt Support}) not
as a result of direct user
action (e.g. an execution of a protected action) there shall be an upper
bound on the duration of this blocking.
@begin{Ramification}
The implementation shall not allow itself to be interrupted when it is in a
state where it is unable to support all the language-defined operations
permitted in the execution of interrupt handlers.
(see @RefSecNum{Protected Subprograms and Protected Actions}).
@end{Ramification}

The implementation shall recognize entry-less protected types.
The overhead of acquiring the execution resource of an object of such a type
(see @RefSecNum{Protected Subprograms and Protected Actions}) shall be
minimized. In particular, there should not be any overhead due to evaluating
@nt{entry_barrier} @nt{condition}s.
@begin{ImplNote}
Ideally the overhead should just be a spin-lock.
@end{ImplNote}

Unchecked_Deallocation shall be supported for terminated tasks that are
designated by access types, and shall have the effect of releasing all
the storage associated with the task. This includes any run-time system
or heap storage that has been implicitly allocated for the task by the
implementation.

@end{ImplReq}

@begin{DocReq}

The implementation shall document the
upper bound on the duration of interrupt blocking caused by the
implementation. If this is different for different interrupts or
interrupt priority levels, it should be documented for each case.
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[The upper bound on the duration of interrupt blocking caused by
the implementation.]}]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The upper bound on the duration of interrupt blocking caused by
the implementation.]}]}

@end{DocReq}

@begin{Metrics}
@Leading@;The implementation shall document the following metric:
@begin{Itemize}
The overhead associated with obtaining
a mutual-exclusive access to an entry-less protected object. This shall be
measured in the following way:

@NoPrefix@Leading@keepnext@;For a protected object of the form:
@begin{example}
@key{protected} Lock @key{is}
   @key{procedure} Set;
   @Key{function} Read @Key{return} Boolean;
@key{private}
   Flag : Boolean := False;
@key{end} Lock;

@key{protected body} Lock @key{is}
   @key{procedure} Set @key{is}
   @key{begin}
      Flag := True;
   @key{end} Set;
   @Key{function} Read @Key{return} Boolean
   @key{Begin}
      @key{return} Flag;
   @key{end} Read;
@key{end} Lock;
@end{example}

@NoPrefix@;The execution time, in processor clock cycles, of a call to
Set. This shall be measured between the point just before
issuing the call, and the point just after the call
completes.
The function Read shall be called later to verify that Set was indeed
called (and not optimized away). The
calling task shall have
sufficiently high priority as to not be preempted during the measurement
period. The protected object shall have sufficiently high ceiling priority
to allow the task to call Set.

@NoPrefix@;For a multiprocessor, if supported, the metric shall be reported for the
case where no contention (on the execution resource) exists
@Redundant[from tasks executing on other processors].
@end{Itemize}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The metrics for entry-less protected objects.]}]}
@end{Metrics}


@LabeledRevisedClause{Version=[3],InitialVersion=[2],New=[The Ravenscar Profile],Old=[Run-time Profiles]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00249-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0246-1]}
@ChgAdded{Version=[2],Text=[@Redundant[This clause
@Chg{Version=[3],New=[defines the Ravenscar profile.@Defn{Ravenscar}],
Old=[specifies a mechanism for defining run-time profiles.]}]]}
@end{Intro}

@begin{NotIso}
@ChgAdded{Version=[3],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 2
and 3 were moved to @RefSec{Pragma Restrictions and Pragma Profile}.>}]}@Comment{This message
should be deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@begin{Syntax}
@begin{SyntaxText}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00249-01]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0246-1]}
@ChgDeleted{Version=[3],Type=[Leading],Keepnext=[T],Text=[@Chg{Version=[2],New=[The
form of a @nt{pragma} Profile is as follows:],Old=[]}]}
@end{SyntaxText}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@DeletedPragmaSyn<Version=[3],InitialVersion=[2],@ChgDeleted{Version=[3],
Text=`@Chg[Version=[2],New=[@key{pragma} @prag<Profile> (@SynI{profile_}@Syn2{identifier} {, @SynI{profile_}@Syn2{pragma_argument_association}});],Old=[]]'}>

@end{Syntax}

@begin{Legality}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00249-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0246-1]}
@ChgAdded{Version=[2],Text=[The @SynI{profile_}@nt{identifier}
@Chg{Version=[3],New=[Ravenscar is a usage profile (see @RefSecNum{Pragma Restrictions and Pragma Profile}).
For usage profile Ravenscar, there shall be no],Old=[shall be the name
of a run-time profile. The semantics of any]}
@SynI{profile_}@nt{pragma_@!argument_@!association}s@Chg{Version=[3],New=[],Old=[
are defined by
the run-time profile specified by the @SynI{profile_}@nt{identifier}]}.]}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00249-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0246-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Chg{Version=[3],New=[The
usage profile Ravenscar is equivalent to the following set of
pragmas:],Old=[A profile is equivalent to the set of configuration
pragmas that is defined for each run-time profile.]}]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI95-00249-01],ARef=[AI95-00297-01],ARef=[AI95-00394-01],ARef=[AI05-0171-1],ARef=[AI05-0246-1]}
@ChgAdded{Version=[3],Text=[
@key{pragma} Task_Dispatching_Policy (FIFO_Within_Priorities);
@key{pragma} Locking_Policy (Ceiling_Locking);
@key{pragma} Detect_Blocking;
@key{pragma} Restrictions (
              No_Abort_Statements,
              No_Dynamic_Attachment,
              No_Dynamic_Priorities,
              No_Implicit_Heap_Allocations,
              No_Local_Protected_Objects,
              No_Local_Timing_Events,
              No_Protected_Type_Allocators,
              No_Relative_Delay,
              No_Requeue_Statements,
              No_Select_Statements,
              No_Specific_Termination_Handlers,
              No_Task_Allocators,
              No_Task_Hierarchy,
              No_Task_Termination,
              Simple_Barriers,
              Max_Entry_Queue_Length => 1,
              Max_Protected_Entries => 1,
              Max_Task_Entries => 0,
              No_Dependence => Ada.Asynchronous_Task_Control,
              No_Dependence => Ada.Calendar,
              No_Dependence => Ada.Execution_Time.Group_Budgets,
              No_Dependence => Ada.Execution_Time.Timers,
              No_Dependence => Ada.Task_Attributes@Chg{Version=[3],New=[,
              No_Dependence => System.Multiprocessors.Dispatching_Domains],Old=[]});]}
@end{Example}

@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[The Ravenscar profile is named for the location
of the meeting that defined its initial version. The name is now in widespread
use, so we stick with existing practice, rather than using a more descriptive
name.@Comment{ This is another example of Ada's lousy marketing sense; casual
readers, especially those outside of Ada, have no conception of what
@lquotes@;Ravenscar@rquotes@; is, and thus are much less likely to investigate
it to find out how it can help them.}]}
@end{Discussion}
@end{StaticSem}

@begin{Linktime}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00249-01]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0246-1]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[@PDefn2{Term=[configuration pragma], Sec=(Profile)}
@PDefn2{Term=[pragma, configuration], Sec=(Profile)}
A @nt{pragma} Profile is a configuration pragma.
There may be more than one @nt{pragma} Profile for a partition.]}]}
@end{Linktime}
@begin{NotIso}
@ChgAdded{Version=[3],Noparanum=[T],Text=[@Shrink{@i<Paragraph 7 was
deleted.>}]}@Comment{This message should be deleted if the paragraphs
are ever renumbered.}
@end{NotIso}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[A task shall only be on the ready queues of one
processor, and the
processor to which a task belongs shall be defined statically.
Whenever a task running on a processor reaches a task dispatching point,
it goes back to the ready queues of the same processor. A task with
a CPU value of Not_A_Specific_CPU will execute on an implementation
defined processor. @Redundant[A task without a CPU aspect will activate and
execute on the same processor as its activating task.]]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The processor of a task without a CPU aspect is
  defined in @RefSecNum{Multiprocessor Implementation}.]}
@end{TheProof}
@ChgImplDef{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The processor on which a task with a CPU value of a Not_A_Specific_CPU
will execute when the Ravenscar profile is in effect.]}]}
@end{ImplReq}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1]}
@ChgAdded{Version=[3],Text=[On a multiprocessor system, an implementation should
support a fully partitioned approach. Each processor should have separate and
disjoint ready queues.]}

@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[On a multiprocessor system, each processor should have a separate
and disjoint ready queue.]}]}
@end{ImplAdvice}

@begin{Notes}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI95-00249-01],ARef=[AI05-0246-1]}
@ChgAdded{Version=[3],Text=[The effect of the Max_Entry_Queue_Length => 1
restriction applies only to protected entry queues due to the accompanying
restriction of Max_Task_Entries => 0.]}
@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00249-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0246-1]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @Chg{Version=[3],New=[The Ravenscar profile is new; it was moved here by Ada
  2012],Old=[@nt{Pragma} Profile is new]}.]}
@end{Extend95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1]}
  @ChgAdded{Version=[3],Text=[How Ravenscar behaves on a multiprocessor
  system is now defined.]}
@end{DiffWord2005}

@Comment{Moved the following to the previous clause...
@RMNewPageVer{Version=[2]}@Comment{For printed RM Ada 2005}
@LabeledAddedSubClause{Version=[2],Name=[The Ravenscar Profile]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00249-01]}
@ChgAdded{Version=[2],Text=[@Redundant[This clause
defines the Ravenscar profile.]@Defn{Ravenscar}]}
@end{Intro}

@begin{Legality}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00249-01]}
@ChgAdded{Version=[2],Text=[The @SynI{profile_}@nt{identifier}
Ravenscar is a run-time profile.
For run-time profile Ravenscar, there shall be no
@Syni{profile_}@nt{pragma_argument_association}s.]}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00249-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The run-time profile
Ravenscar is equivalent to the following set of pragmas:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00249-01],ARef=[AI95-00297-01],ARef=[AI95-00394-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0171-1]}
@ChgAdded{Version=[2],Text=[
@key{pragma} Task_Dispatching_Policy (FIFO_Within_Priorities);
@key{pragma} Locking_Policy (Ceiling_Locking);
@key{pragma} Detect_Blocking;
@key{pragma} Restrictions (
              No_Abort_Statements,
              No_Dynamic_Attachment,
              No_Dynamic_Priorities,
              No_Implicit_Heap_Allocations,
              No_Local_Protected_Objects,
              No_Local_Timing_Events,
              No_Protected_Type_Allocators,
              No_Relative_Delay,
              No_Requeue_Statements,
              No_Select_Statements,
              No_Specific_Termination_Handlers,
              No_Task_Allocators,
              No_Task_Hierarchy,
              No_Task_Termination,
              Simple_Barriers,
              Max_Entry_Queue_Length => 1,
              Max_Protected_Entries => 1,
              Max_Task_Entries => 0,
              No_Dependence => Ada.Asynchronous_Task_Control,
              No_Dependence => Ada.Calendar,
              No_Dependence => Ada.Execution_Time.Group_Budgets,
              No_Dependence => Ada.Execution_Time.Timers,
              No_Dependence => Ada.Task_Attributes@Chg{Version=[3],New=[,
              No_Dependence => System.Multiprocessors.Dispatching_Domains],Old=[]});]}
@end{Example}

@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The Ravenscar profile is named for the location
of the meeting that defined its initial version. The name is now in widespread
use, so we stick with existing practice, rather than using a more descriptive
name.@Comment{ This is another example of Ada's lousy marketing sense; casual
readers, especially those outside of Ada, have no conception of what
@lquotes@;Ravenscar@rquotes@; is, and thus are much less likely to investigate
it to find out how it can help them.}]}
@end{Discussion}

@end{StaticSem}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1]}
@ChgAdded{Version=[3],Text=[A task shall only be on the ready queues of one
processor, and the
processor to which a task belongs shall be defined statically.
Whenever a task running on a processor reaches a task dispatching point,
it goes back to the ready queues of the same processor. A task with
a CPU value of Not_A_Specific_CPU will execute on an implementation
defined processor. @Redundant[A task without a CPU aspect will activate and
execute on the same processor as its activating task.]]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The processor of a task without an aspect CPU is
  defined in @RefSecNum{Multiprocessor Implementation}.]}
@end{TheProof}
@ChgImplDef{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The processor on which a task with a CPU value of a Not_A_Specific_CPU
will execute when the Ravenscar profile is in effect.]}]}
@end{ImplReq}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1]}
@ChgAdded{Version=[3],Text=[On a multiprocessor system, an implementation should
support a fully partitioned approach. Each processor should have separate and
disjoint ready queues.]}

@ChgImplAdvice{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[On a multiprocessor system, each processor should have a separate
and disjoint ready queue.]}]}
@end{ImplAdvice}

@begin{Notes}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00249-01]}
@ChgAdded{Version=[2],Text=[
The effect of the Max_Entry_Queue_Length => 1 restriction applies
only to protected entry queues due to the accompanying restriction of
Max_Task_Entries => 0.]}
@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00249-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The Ravenscar profile is new.]}
@end{Extend95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1]}
  @ChgAdded{Version=[3],Text=[How Ravenscar behaves on a multiprocessor
  system is now defined.]}
@end{DiffWord2005}
end commented out text...}@Comment{End of original Ravenscar}


@RMNewPageVer{Version=[2]}@Comment{For printed RM Ada 2005}
@LabeledAddedClause{Version=[2],Name=[Execution Time]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[This clause describes a language-defined package to
measure execution time.]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The following
language-defined library package exists:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{with} Ada.Task_Identification;
@key{with} Ada.Real_Time; @key{use} Ada.Real_Time;
@key{package} Ada.Execution_Time @key{is}@ChildUnit{Parent=[Ada],Child=[Execution_Time]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{CPU_Time} @key{is private};
   @AdaObjDefn{CPU_Time_First} : @key{constant} CPU_Time;
   @AdaObjDefn{CPU_Time_Last}  : @key{constant} CPU_Time;
   @AdaObjDefn{CPU_Time_Unit}  : @key{constant} := @RI{implementation-defined-real-number};
   @AdaObjDefn{CPU_Tick} : @key{constant} Time_Span;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Clock}
     (T : Ada.Task_Identification.Task_Id
          := Ada.Task_Identification.Current_Task)
     @key{return} CPU_Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "+"  (Left : CPU_Time; Right : Time_Span) @key{return} CPU_Time;
   @key{function} "+"  (Left : Time_Span; Right : CPU_Time) @key{return} CPU_Time;
   @key{function} "-"  (Left : CPU_Time; Right : Time_Span) @key{return} CPU_Time;
   @key{function} "-"  (Left : CPU_Time; Right : CPU_Time)  @key{return} Time_Span;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} "<"  (Left, Right : CPU_Time) @key{return} Boolean;
   @key{function} "<=" (Left, Right : CPU_Time) @key{return} Boolean;
   @key{function} ">"  (Left, Right : CPU_Time) @key{return} Boolean;
   @key{function} ">=" (Left, Right : CPU_Time) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Split}
     (T : @key{in} CPU_Time; SC : @key{out} Seconds_Count; TS : @key{out} Time_Span);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Time_Of} (SC : Seconds_Count;
                     TS : Time_Span := Time_Span_Zero) @key{return} CPU_Time;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0170-1]}
@ChgAdded{Version=[3],Text=[   @AdaObjDefn{Interrupt_Clocks_Supported} : @key[constant] Boolean := @RI<implementation-defined>;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0170-1]}
@ChgAdded{Version=[3],Text=[   @AdaObjDefn{Separate_Interrupt_Clocks_Supported} : @key[constant] Boolean :=
     @RI<implementation-defined>;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0170-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Clock_For_Interrupts} @key[return] CPU_Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{private}
   ... -- @RI[not specified by the language]
@key{end} Ada.Execution_Time;]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0170-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[execution time],Sec=[of a task]}
@Defn2{Term=[CPU time],Sec=[of a task]}
The @i<execution time> or CPU time of a given task is defined as the time spent by
the system executing that task, including the time spent executing run-time or
system services on its behalf. The mechanism used to measure execution time is
implementation defined. @Chg{Version=[3],New=[The Boolean constant Interrupt_Clocks_Supported is
set to True if the implementation separately accounts for the execution time
of interrupt handlers. If it is set to False it],Old=[It]} is implementation
defined which task, if any, is charged the execution time that is consumed by
interrupt handlers@Chg{Version=[3],New=[. The Boolean constant
Separate_Interrupt_Clocks_Supported is set to True if the implementation
separately accounts for the execution time of individual interrupt
handlers (see @RefSecNum{Execution Time of Interrupt Handlers})],Old=[ and
run-time services on behalf of the system]}.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The implementation-defined properties above
  and of the values declared in the package are repeated in @DocReqTitle,
  so we don't mark them as implementation-defined.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[The type CPU_Time represents the execution time of
a task. The set of values of this type corresponds one-to-one with an
implementation-defined range of mathematical integers.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[The CPU_Time value I represents the half-open
execution-time interval that starts with I*CPU_Time_Unit and is limited by
(I+1)*CPU_Time_Unit, where CPU_Time_Unit is an implementation-defined
real number. For each task, the execution time value is set to zero at
the creation of the task.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Since it is implementation-defined which task
  is charged execution time for system services, the execution time value
  may become nonzero even before the start of the activation of the task.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[CPU_Time_First and CPU_Time_Last are the smallest
and largest values of the CPU_Time type, respectively.]}
@end{StaticSem}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0170-1]}
@ChgAdded{Version=[3],Text=[The execution time value for the
function Clock_For_Interrupts is initialized to zero.]}

@begin{RunTime}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[@Defn{CPU clock tick}
CPU_Time_Unit is the smallest amount of execution time representable
by the CPU_Time type; it is expressed in seconds. A @i<CPU clock tick> is an
execution time interval during which the clock value (as observed by
calling the Clock function) remains constant. CPU_Tick is the average
length of such intervals.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[The effects of the operators on CPU_Time and
Time_Span are as for the operators defined for integer types.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[The function Clock returns the current execution
time of the task identified by T; Tasking_Error is raised if that task has
terminated; Program_Error is raised if the value of T is
Task_Identification.Null_Task_Id.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[The effects of the Split and Time_Of operations are defined as
follows, treating values of type CPU_Time, Time_Span, and
Seconds_Count as mathematical integers. The effect of Split (T, SC,
TS) is to set SC and TS to values such that T*CPU_Time_Unit = SC*1.0 +
TS*CPU_Time_Unit, and 0.0 <= TS*CPU_Time_Unit < 1.0. The value
returned by Time_Of(SC,TS) is the execution-time value T such that
T*CPU_Time_Unit=SC*1.0 + TS*CPU_Time_Unit.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0170-1]}
@ChgAdded{Version=[3],Text=[The function Clock_For_Interrupts returns the total
cumulative time spent executing within all interrupt handlers. This time is not
allocated to any task execution time clock. If Interrupt_Clocks_Supported is set
to False the function raises Program_Error.]}

@end{RunTime}

@begin{Erron}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=(erroneous execution),Sec=(cause)}
For a call of Clock, if the task identified by T no longer exists, the
execution of the program is erroneous.]}
@end{Erron}

@begin{ImplReq}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[The range of CPU_Time values shall be sufficient
to uniquely represent the range of execution times from the task start-up to 50
years of execution time later. CPU_Tick shall be no greater than 1
millisecond.]}
@end{ImplReq}

@begin{DocReq}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[The implementation shall document the values of
CPU_Time_First, CPU_Time_Last, CPU_Time_Unit, and CPU_Tick.]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The values of CPU_Time_First, CPU_Time_Last, CPU_Time_Unit, and CPU_Tick
of package Execution_Time.]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[The implementation shall document the properties of
the underlying mechanism used to measure execution times, such as the range of
values supported and any relevant aspects of the underlying hardware or
operating system facilities used.]}
@ChgDocReq{Version=[3],Kind=[Revised],InitialVersion=[2],
Text=[@ChgAdded{Version=[2],
Text=[The properties of the mechanism used to implement
package Execution_Time@Chg{Version=[3],New=[,
including the values of the constants defined in the package],Old=[]}.]}]}

@end{DocReq}

@begin{Metrics}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[The implementation
shall document the following metrics:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text={An upper bound on the execution-time duration of a
clock tick. This is a value D such that if t1 and t2 are any execution times of
a given task such that t1 < t2 and Clock@-{t1} = Clock@-{t2} then
t2 @en@; t1 <= D.}}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[An upper bound on the size of a clock jump. A clock
jump is the difference between two successive distinct values of an
execution-time clock (as observed by calling the Clock function with the same
Task_Id).]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[An upper bound on the execution time of a call to
the Clock function, in processor clock cycles.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Upper bounds on the execution times of the
operators of the type CPU_Time, in processor clock cycles.]}
@end{Itemize}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The metrics for execution time.]}]}

@end{Metrics}

@begin{ImplPerm}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[Implementations targeted to machines with word size
smaller than 32 bits need not support the full range and granularity of the
CPU_Time type.]}

@end{ImplPerm}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[When appropriate, implementations should provide
configuration mechanisms to change the value of CPU_Tick.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[When appropriate, implementations should provide
configuration mechanisms to change the value of Execution_Time.CPU_Tick.]}]}

@end{ImplAdvice}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The package Execution_Time is new.]}
@end{Extend95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0170-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
  Function Clock_For_Interrupts, and constants Interrupt_Clocks_Supported and
  Separate_Interrupt_Clocks_Supported are added to Execution_Time.
  If Execution_Time is referenced in a @nt{use_clause}, and an
  entity @i<E> with a @nt{defining_identifier} of one of the added entities
  is defined in a package that is also referenced in a @nt{use_clause}, the entity
  @i<E> may no longer be use-visible, resulting in errors. This should be rare
  and is easily fixed if it does occur.]}
@end{Incompatible2005}

@begin{Diffword2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0170-1]}
  @ChgAdded{Version=[3],Text=[If Interrupt_Clocks_Supported is True, it is
  now possible to determine the execution time of interrupt handlers. This
  is not an inconsistency, as not charging any task for such time was a
  legitimate implementation for Ada 2005.]}
@end{Diffword2005}


@LabeledAddedSubclause{Version=[2],Name=[Execution Time Timers]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[This clause describes a language-defined package
that provides a facility for calling a handler when a task has used a defined
amount of CPU time.]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The following
language-defined library package exists:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{with} System;
@key{package} Ada.Execution_Time.Timers @key{is}@ChildUnit{Parent=[Ada.Execution_Time],Child=[Timers]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Timer} (T : @key{not null access constant}
                       Ada.Task_Identification.Task_Id) @key{is}
      @key{tagged limited private};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Timer_Handler} @key{is}
      @key{access protected procedure} (TM : @key{in out} Timer);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaObjDefn{Min_Handler_Ceiling} : @key{constant} System.Any_Priority :=
   @RI[implementation-defined];]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Set_Handler} (TM      : @key{in out} Timer;
                          In_Time : @key{in} Time_Span;
                          Handler : @key{in} Timer_Handler);
   @key{procedure} @AdaSubDefn{Set_Handler} (TM      : @key{in out} Timer;
                          At_Time : @key{in} CPU_Time;
                          Handler : @key{in} Timer_Handler);
   @key{function} @AdaSubDefn{Current_Handler} (TM : Timer) @key{return} Timer_Handler;
   @key{procedure} @AdaSubDefn{Cancel_Handler} (TM        : @key{in out} Timer;
                             Cancelled :    @key{out} Boolean);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Time_Remaining} (TM : Timer) @key{return} Time_Span;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaExcDefn{Timer_Resource_Error} : @key{exception};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{private}
   ... -- @RI{not specified by the language}
@key{end} Ada.Execution_Time.Timers;]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[The type Timer represents an execution-time event
for a single task and is capable of detecting execution-time overruns. The
access discriminant T identifies the task concerned. The type Timer needs
finalization (see @RefSecNum{Assignment and Finalization}).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[An object of type Timer is said to be @i<set> if it
is associated with a nonnull value of type Timer_Handler and @i<cleared>
otherwise. All Timer objects are initially cleared.
@PDefn2{Term=[set],Sec=[execution timer object]}
@PDefn2{Term=[clear],Sec=[execution timer object]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[The type Timer_Handler identifies a protected
procedure to be executed by the implementation when the timer expires. Such a
protected procedure is called a @i<handler>.
@PDefn2{Term=[handler],Sec=[execution timer]}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Type Timer is tagged. This makes it possible to
  share a handler between several events. In simple cases, 'Access can be used
  to compare the parameter with a specific timer object (this works because a
  tagged type is a by-reference type). In more complex cases, a type extension
  of type Timer can be declared; a double type conversion can be used to access
  the extension data. An example of how this can be done can be found for the
  similar type Timing_Event, see @RefSecNum{Timing Events}.]}
@end{Discussion}

@end{StaticSem}

@begin{Runtime}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[When a Timer object is created, or upon the first
call of a Set_Handler procedure with the timer as parameter, the resources
required to operate an execution-time timer based on the associated
execution-time clock are allocated and initialized. If this operation would
exceed the available resources, Timer_Resource_Error is raised.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The procedures Set_Handler associate the handler
Handler with the timer TM@Chg{Version=[3],New=[:],Old=[;]}
if Handler is @key[null], the timer is cleared@Chg{Version=[3],New=[;],Old=[,]}
otherwise@Chg{Version=[3],New=[,],Old=[]} it is set. The first procedure
Set_Handler loads the timer TM with an interval specified by the Time_Span
parameter. In this mode, the timer TM @i<expires> when the execution time of the
task identified by TM.T.@key[all] has increased by In_Time; if In_Time is less
than or equal to zero, the timer expires immediately. The second procedure
Set_Handler loads the timer TM with the absolute value specified by At_Time. In
this mode, the timer TM expires when the execution time of the task identified
by TM.T.@key[all] reaches At_Time; if the value of At_Time has already been
reached when Set_Handler is called, the timer expires
immediately.@Defn2{Term=[expires], Sec=[execution timer]}]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Since an access-to-constant can designate a
  variable, the Task_Id value designated by the discriminant of a Timer
  object can be changed after the object is created. Thus, an implementation
  cannot use the value of the Task_Id other than where this Standard specifies.
  For instance, the Task_Id should be read when the timer is set, but it
  should not be used when the timer expires (as it may designate a different
  task at that point).]}
@end{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[A call of a procedure Set_Handler for a timer that
is already set replaces the handler and the (absolute or relative) execution
time; if Handler is not @b<null>, the timer remains set.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[When a timer expires, the associated handler is
executed, passing the timer as parameter. The initial action of the execution
of the handler is to clear the event.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The function Current_Handler returns the handler
associated with the timer TM if that timer is set;
otherwise@Chg{Version=[3],New=[,],Old=[]} it returns @b<null>.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The procedure Cancel_Handler clears the timer if it
is set. Cancelled is assigned True if the timer was set prior to it being
cleared; otherwise@Chg{Version=[3],New=[,],Old=[]}
it is assigned False.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The function Time_Remaining returns the execution
time interval that remains until the timer TM would expire, if that timer is
set; otherwise@Chg{Version=[3],New=[,],Old=[]} it returns Time_Span_Zero.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[The constant Min_Handler_Ceiling is the
minimum ceiling priority required for a protected object with a handler to
ensure that no ceiling violation will occur when that handler is invoked.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[As part of the finalization of an object of type
Timer, the timer is cleared.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[For all the subprograms defined in this package,
Tasking_Error is raised if the task identified by TM.T.@key[all] has terminated, and
Program_Error is raised if the value of TM.T.@key[all] is
Task_Identification.Null_Task_Id.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[An exception propagated from a handler invoked as
part of the expiration of a timer has no effect.]}

@end{Runtime}

@begin{Erron}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=(erroneous execution),Sec=(cause)}
For a call of any of the subprograms defined in this package, if the task
identified by TM.T.@key[all] no longer exists, the execution of the program is
erroneous.]}

@end{Erron}

@begin{ImplReq}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[For a given Timer object, the implementation shall
perform the operations declared in this package atomically with respect to any
of these operations on the same Timer object. The replacement of a handler by a
call of Set_Handler shall be performed atomically with respect to the execution
of the handler.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This prevents various race conditions. In
  particular it ensures that if an event occurs when Set_Handler is changing
  the handler then either the new or old handler is executed in response to the
  appropriate event. It is never possible for a new handler to be executed in
  response to an old event]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[When an object of type Timer is finalized, the
system resources used by the timer shall be deallocated.]}

@end{ImplReq}

@begin{ImplPerm}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[Implementations may limit the number of timers that
can be defined for each task. If this limit is exceeded@Chg{Version=[3],New=[,],Old=[]}
then Timer_Resource_Error is raised.]}

@end{ImplPerm}

@begin{Notes}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
@ChgAdded{Version=[2],Text=[A Timer_Handler can be associated with several
Timer objects.]}

@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00307-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The package Execution_Time.Timers is new.]}
@end{Extend95}


@RMNewPageVer{Version=[2]}@Comment{For printed RM Ada 2005}
@ISOOnlyRMNewPageVer{Version=[3]}@Comment{For ISO version of Ada 2012 Standard}
@LabeledAddedSubclause{Version=[2],Name=[Group Execution Time Budgets]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[This clause describes a language-defined package to
assign execution time budgets to groups of tasks.]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The following
language-defined library package exists:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0169-1]}
@ChgAdded{Version=[2],Text=[@key{with} System;@Chg{Version=[3],New=[
@key{with} System.Multiprocessors;],Old=[]}
@key{package} Ada.Execution_Time.Group_Budgets @key{is}@ChildUnit{Parent=[Ada.Execution_Time],Child=[Group_Budgets]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1],ARef=[AI05-0169-1]}
@ChgAdded{Version=[2],Text=[  @key{type} @AdaTypeDefn{Group_Budget}@Chg{Version=[3],New=[(CPU : System.Multiprocessors.CPU :=
                             System.Multiprocessors.CPU'First)
   ],Old=[]} @key{is tagged limited private};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[  @key{type} @AdaTypeDefn{Group_Budget_Handler} @key{is access}
       @key{protected procedure} (GB : @key{in out} Group_Budget);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[  @key{type} @AdaTypeDefn{Task_Array} @key{is array} (Positive @key{range} <>) @key{of}
                                  Ada.Task_Identification.Task_Id;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[  @AdaObjDefn{Min_Handler_Ceiling} : @key{constant} System.Any_Priority :=
    @RI[implementation-defined];]}
@ChgImplDef{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The value of Min_Handler_Ceiling in Execution_Time.Group_Budgets.]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[  @key{procedure} @AdaSubDefn{Add_Task} (GB : @key{in out} Group_Budget;
                      T  : @key{in} Ada.Task_Identification.Task_Id);
  @key{procedure} @AdaSubDefn{Remove_Task} (GB: @key{in out} Group_Budget;
                         T  : @key{in} Ada.Task_Identification.Task_Id);
  @key{function} @AdaSubDefn{Is_Member} (GB : Group_Budget;
                      T : Ada.Task_Identification.Task_Id) @key{return} Boolean;
  @key{function} @AdaSubDefn{Is_A_Group_Member}
     (T : Ada.Task_Identification.Task_Id) @key{return} Boolean;
  @key{function} @AdaSubDefn{Members} (GB : Group_Budget) @key{return} Task_Array;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[  @key{procedure} @AdaSubDefn{Replenish} (GB : @key{in out} Group_Budget; To : @key{in} Time_Span);
  @key{procedure} @AdaSubDefn{Add} (GB : @key{in out} Group_Budget; Interval : @key{in} Time_Span);
  @key{function} @AdaSubDefn{Budget_Has_Expired} (GB : Group_Budget) @key{return} Boolean;
  @key{function} @AdaSubDefn{Budget_Remaining} (GB : Group_Budget) @key{return} Time_Span;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[  @key{procedure} @AdaSubDefn{Set_Handler} (GB      : @key{in out} Group_Budget;
                         Handler : @key{in} Group_Budget_Handler);
  @key{function} @AdaSubDefn{Current_Handler} (GB : Group_Budget)
     @key{return} Group_Budget_Handler;
  @key{procedure} @AdaSubDefn{Cancel_Handler} (GB        : @key{in out} Group_Budget;
                            Cancelled : @key{out} Boolean);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[  @AdaExcDefn{Group_Budget_Error} : @key{exception};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{private}
    --  @RI{not specified by the language}
@key{end} Ada.Execution_Time.Group_Budgets;]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[The type Group_Budget represents an execution time
budget to be used by a group of tasks. The type Group_Budget
needs finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}). A task can belong
to at most one group. Tasks of any priority can be added to a group.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[An object of type Group_Budget has an associated
nonnegative value of type Time_Span known as its @i<budget>, which is
initially Time_Span_Zero. The type Group_Budget_Handler identifies a protected
procedure to be executed by the implementation when the budget is
@i<exhausted>, that is, reaches zero. Such a protected procedure is called a
@i<handler>.@Defn{budget}@Defn2{Term=[exhaust],Sec=[a budget]}
@PDefn2{Term=[handler],Sec=[group budget]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[An object of type Group_Budget also includes a
handler, which is a value of type Group_Budget_Handler. The handler of the
object is said to be @i<set> if it is not null and @i<cleared> otherwise. The
handler of all Group_Budget objects is initially cleared.
@PDefn2{Term=[set],Sec=[group budget object]}
@PDefn2{Term=[clear],Sec=[group budget object]}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]} @ChgAdded{Version=[2],Text=[Type
  Group_Budget is tagged. This makes it possible to share a handler between
  several events. In simple cases, 'Access can be used to compare the parameter
  with a specific group budget object (this works because a tagged type is a
  by-reference type). In more complex cases, a type extension of type
  Group_Budget can be declared; a double type conversion can be used to access
  the extension data. An example of how this can be done can be found for the
  similar type Timing_Event, see @RefSecNum{Timing Events}.]}
@end{Discussion}

@end{StaticSem}

@begin{RunTime}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[The procedure Add_Task adds the task identified by
T to the group GB; if that task is already a member of some other group,
Group_Budget_Error is raised.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[The procedure Remove_Task removes the task
identified by T from the group GB; if that task is not a member of the group
GB, Group_Budget_Error is raised. After successful execution of this procedure,
the task is no longer a member of any group.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The function Is_Member returns True if the task
identified by T is a member of the group GB;
otherwise@Chg{Version=[3],New=[,],Old=[]} it
@Chg{Version=[3],New=[returns],Old=[return]} False.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The function Is_A_Group_Member returns True if the
task identified by T is a member of some group;
otherwise@Chg{Version=[3],New=[,],Old=[]} it returns False.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[The function Members returns an array of values of
type Task_Identification.Task_Id identifying the members of the group GB. The
order of the components of the array is unspecified.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1],ARef=[AI05-0169-1]}
@ChgAdded{Version=[2],Text=[The procedure Replenish loads the group budget GB
with To as the Time_Span value. The exception Group_Budget_Error is raised if
the Time_Span value To is nonpositive. Any execution @Chg{Version=[3],New=[on
CPU ],Old=[]}of any member of the
group of tasks results in the budget counting down, unless exhausted. When the
budget becomes exhausted (reaches Time_Span_Zero), the associated handler is
executed if the handler of group budget GB is set. Nevertheless, the tasks
continue to execute.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[The procedure Add modifies the budget of the group
GB. A positive value for Interval increases the budget. A negative value for
Interval reduces the budget, but never below Time_Span_Zero. A zero value for
Interval has no effect. A call of procedure Add that results in the value of
the budget going to Time_Span_Zero causes the associated handler to be executed
if the handler of the group budget GB is set.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The function Budget_Has_Expired returns True if the
budget of group GB is exhausted (equal to Time_Span_Zero);
otherwise@Chg{Version=[3],New=[,],Old=[]} it returns False.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[The function Budget_Remaining returns the remaining
budget for the group GB. If the budget is exhausted it returns Time_Span_Zero.
This is the minimum value for a budget.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The procedure Set_Handler associates the handler
Handler with the Group_Budget GB@Chg{Version=[3],New=[:],Old=[;]}
if Handler is @b<null>, the handler of
Group_Budget is cleared@Chg{Version=[3],New=[;],Old=[,]}
otherwise@Chg{Version=[3],New=[,],Old=[]} it is set.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[A call of Set_Handler for a Group_Budget that
already has a handler set replaces the handler; if Handler is not @b<null>, the
handler for Group_Budget remains set.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The function Current_Handler returns the handler
associated with the group budget GB if the handler for that group budget is
set; otherwise@Chg{Version=[3],New=[,],Old=[]} it returns @b<null>.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The procedure Cancel_Handler clears the handler for
the group budget if it is set. Cancelled is assigned True if the handler for
the group budget was set prior to it being cleared;
otherwise@Chg{Version=[3],New=[,],Old=[]} it is assigned False.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[The constant Min_Handler_Ceiling is the
minimum ceiling priority required for a protected object with a handler to
ensure that no ceiling violation will occur when that handler is invoked.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[The precision of the accounting of task execution
time to a Group_Budget is the same as that defined for execution-time clocks
from the parent package.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[As part of the finalization of an object of type
Group_Budget all member tasks are removed from the group identified by that
object.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[If a task is a member of a Group_Budget when it
terminates@Chg{Version=[3],New=[,],Old=[]} then
as part of the finalization of the task it is removed from the group.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[For all the operations defined in this package,
Tasking_Error is raised if the task identified by T has terminated, and
Program_Error is raised if the value of T is
Task_Identification.Null_Task_Id.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[An exception propagated from a handler invoked when
the budget of a group of tasks becomes exhausted has no effect.]}

@end{RunTime}

@begin{Erron}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=(erroneous execution),Sec=(cause)}
For a call of any of the subprograms defined in this package, if the task
identified by T no longer exists, the execution of the program is erroneous.]}
@end{Erron}

@begin{ImplReq}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[For a given Group_Budget object, the implementation
shall perform the operations declared in this package atomically with respect
to any of these operations on the same Group_Budget object. The replacement of
a handler, by a call of Set_Handler, shall be performed atomically with respect
to the execution of the handler.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This prevents various race conditions. In
  particular it ensures that if the budget is exhausted when Set_Handler
  is changing the handler then either the new or old handler is executed
  and the exhausting event is not lost.]}
@end{Reason}
@end{ImplReq}

@begin{Notes}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[Clearing or setting of the handler of a group
budget does not change the current value of the budget. Exhaustion or loading
of a budget does not change whether the handler of the group budget is set or
cleared.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
@ChgAdded{Version=[2],Text=[A Group_Budget_Handler can be associated with
several Group_Budget objects.]}
@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00354-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The package Execution_Time.Group_Budgets is new.]}
@end{Extend95}

@begin{Inconsistent2005}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0169-1]}
@ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}
A Group_Budget is now defined to work on a single processor.
If an implementation managed to make this package work for
programs running on a multiprocessor system, and a program
depends on that fact, it could fail when ported to Ada 2012.
We believe it is unlikely that such an implementation exists
because of the difficulty of signalling other processors when
the time reaches zero; in any case, depending on such an
implementation is not portable.]}
@end{Inconsistent2005}


@LabeledAddedSubClause{Version=[3],Name=[Execution Time of Interrupt Handlers]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0170-1]}
@ChgAdded{Version=[3],Text=[This clause describes a language-defined package to
measure the execution time of interrupt handlers.]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0170-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The following
language-defined library package exists:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{with} Ada.Interrupts;
@key{package} Ada.Execution_Time.Interrupts @key{is}@ChildUnit{Parent=[Ada.Execution_Time],Child=[Interrupts]}
   @key{function} @AdaSubDefn{Clock} (Interrupt : Ada.Interrupts.Interrupt_Id)
        @key{return} CPU_Time;
   @key{function} @AdaSubDefn{Supported} (Interrupt : Ada.Interrupts.Interrupt_Id)
        @key{return} Boolean;
@key{end} Ada.Execution_Time.Interrupts;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0170-1]}
@ChgAdded{Version=[3],Text=[The execution time or CPU time of a given interrupt
Interrupt is defined as the time spent by the system executing interrupt
handlers identified by Interrupt, including the time spent executing run-time or
system services on its behalf. The mechanism used to measure execution time is
implementation defined. Time spent executing interrupt handlers is distinct from
time spent executing any task.]}
@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[The implementation-defined mechanism here is the
same as that covered by the @DocReqTitle of @RefSecNum{Execution Time}, so we
don't repeat that requirement here.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0170-1]}
@ChgAdded{Version=[3],Text=[For each interrupt, the execution time value
is initially set to zero.]}
@end{StaticSem}

@begin{RunTime}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0170-1]}
@ChgAdded{Version=[3],Text=[The function Clock returns the current cumulative
execution time of the interrupt identified by Interrupt. If
Separate_Interrupt_Clocks_Supported is set to False the function raises
Program_Error.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0170-1],ARef=[AI05-0264-1]}
@ChgAdded{Version=[3],Text=[The function Supported returns True if the
implementation is monitoring the execution time of the interrupt identified by
Interrupt; otherwise, it returns False. For any Interrupt_Id Interrupt for which
Supported(Interrupt) returns False, the function Clock(Interrupt) will return a
value equal to Ada.Execution_Time.Time_Of(0).]}

@end{RunTime}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0170-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The package Execution_Time.Interrupts is new.]}
@end{Extend2005}


@LabeledAddedClause{Version=[2],Name=[Timing Events]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Text=[This clause describes a language-defined package to
allow user-defined protected procedures to be executed at a specified time
without the need for a task or a delay statement.]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The following
language-defined library package exists:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} Ada.Real_Time.Timing_Events @key{is}@ChildUnit{Parent=[Ada.Real_Time],Child=[Timing_Events]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[  @key{type} @AdaTypeDefn{Timing_Event} @key{is tagged limited private};
  @key{type} @AdaTypeDefn{Timing_Event_Handler}
       @key{is access protected procedure} (Event : @key{in out} Timing_Event);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[  @key{procedure} @AdaSubDefn{Set_Handler} (Event   : @key{in out} Timing_Event;
                         At_Time : @key{in} Time;
                         Handler : @key{in} Timing_Event_Handler);
  @key{procedure} @AdaSubDefn{Set_Handler} (Event   : @key{in out} Timing_Event;
                         In_Time : @key{in} Time_Span;
                         Handler : @key{in} Timing_Event_Handler);
  @key{function} @AdaSubDefn{Current_Handler} (Event : Timing_Event)
       @key{return} Timing_Event_Handler;
  @key{procedure} @AdaSubDefn{Cancel_Handler} (Event     : @key{in out} Timing_Event;
                            Cancelled : @key{out} Boolean);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[  @key{function} @AdaSubDefn{Time_Of_Event} (Event : Timing_Event) @key{return} Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{private}
  ... -- @RI[not specified by the language]
@key{end} Ada.Real_Time.Timing_Events;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Text=[The type Timing_Event represents a time in the future
when an event is to occur. The type Timing_Event
needs finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Text=[An object of type Timing_Event is said to be
@i<set> if it is associated with a nonnull value of type Timing_Event_Handler
and @i<cleared> otherwise. All Timing_Event objects are initially cleared.
@PDefn2{Term=[set],Sec=[timing event object]}
@PDefn2{Term=[clear],Sec=[timing event object]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Text=[The type Timing_Event_Handler identifies a
protected procedure to be executed by the implementation when the timing event
occurs. Such a protected procedure is called a @i{handler}.
@PDefn2{Term=[handler],Sec=[timing event]}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[Type Timing_Event is tagged. This
  makes it possible to share a handler between several events. In simple cases,
  'Access can be used to compare the parameter with a specific timing event
  object (this works because a tagged type is a by-reference type). In more
  complex cases, a type extension of type Timing_Event can be declared; a
  double type conversion can be used to access the extension data. For
  example:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{type} Toaster_Timing_Event @key{is new} Timing_Event @key{with record}
   Slot : Natural;
@key{end record};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[...]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{protected body} Toaster @key{is}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} Timer (Event : @key{in out} Timing_Event) @key{is}
   @key{begin}
      Pop_Up_Toast (Toaster_Timing_Event(Timing_Event'Class(Event)).Slot);
   @key{end} Timer;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   ...
@key{end} Toaster;]}
@end{Example}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The extra conversion to the class-wide type
  is necessary to make the conversions legal. While this usage is clearly
  ugly, we think that the need for this sort of usage will be rare, so
  we can live with it. It's certainly better than having no way to associate
  data with an event.]}
@end{Discussion}

@end{StaticSem}

@begin{Runtime}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The procedures Set_Handler associate the handler
Handler with the event Event@Chg{Version=[3],New=[:],Old=[;]} if Handler
is @key{null}, the event is cleared@Chg{Version=[3],New=[;],Old=[,]}
otherwise@Chg{Version=[3],New=[,],Old=[]} it is set. The first procedure
Set_Handler sets the execution time for the event to be At_Time. The second
procedure Set_Handler sets the execution time for the event to be
Real_Time.Clock + In_Time.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Text=[A call of a procedure Set_Handler for an event that
is already set replaces the handler and the time of execution; if Handler is
not @key{null}, the event remains set.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Text=[As soon as possible after the time set for the
event, the handler is executed, passing the event as parameter. The handler is
only executed if the timing event is in the set state at the time of execution.
The initial action of the execution of the handler is to clear the event.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The second sentence of this paragraph is because
  of a potential race condition. The time might expire and yet before the
  handler is executed, some task could call Cancel_Handler (or equivalently
  call Set_Handler with a @key{null} parameter) and thus clear the handler.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Text=[If the Ceiling_Locking policy (see
@RefSecNum{Priority Ceiling Locking}) is in effect when a procedure
Set_Handler is called, a check is made that the ceiling priority of
Handler.@key{all} is Interrupt_Priority'Last. If the check fails, Program_Error
is raised.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0094-1],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[If a procedure Set_Handler is called with zero or
negative In_Time or with At_Time indicating a time in the
past@Chg{Version=[3],New=[,],Old=[]} then the handler
is executed @Chg{Version=[3],New=[as soon as possible after the completion of],
Old=[immediately by the task executing]} the call of Set_Handler.@Chg{Version=[3],
New=[],Old=[ The timing event Event is cleared.]}]}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0094-1]}
@ChgAdded{Version=[3],Text=[The handler will still be executed. Under no
circumstances is a scheduled call of a handler lost.]}
@end{Ramification}

@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0094-1]}
@ChgAdded{Version=[3],Text=[We say @ldquote@;as soon as possible@rdquote
so that we do not deadlock if we are executing the handler when Set_Handler is
called. In that case, the current invocation of the handler must complete
before the new handler can start executing.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The function Current_Handler returns the handler
associated with the event Event if that event is set;
otherwise@Chg{Version=[3],New=[,],Old=[]} it returns @key{null}.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The procedure Cancel_Handler clears the event if it
is set. Cancelled is assigned True if the event was set prior to it being
cleared; otherwise@Chg{Version=[3],New=[,],Old=[]} it is assigned False.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The function Time_Of_Event returns the time of the
event if the event is set;
otherwise@Chg{Version=[3],New=[,],Old=[]} it returns Real_Time.Time_First.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Text=[As part of the finalization of an object
of type Timing_Event, the Timing_Event is cleared.]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is the only finalization defined by the
  language that has a visible effect; but an implementation may have other
  finalization that it needs to perform. Implementations need to ensure that
  the event is cleared before anything else is finalized that would prevent
  a set event from being triggered.]}
@end{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Text=[If several timing events are set for the same time,
they are executed in FIFO order of being set.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Text=[An exception propagated from a handler invoked by a
timing event has no effect.]}

@end{Runtime}

@begin{ImplReq}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Text=[For a given Timing_Event object, the implementation
shall perform the operations declared in this package atomically with respect
to any of these operations on the same Timing_Event object. The replacement of
a handler by a call of Set_Handler shall be performed atomically with respect
to the execution of the handler.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This prevents various race conditions. In
  particular it ensures that if an event occurs when Set_Handler is changing
  the handler then either the new or old handler is executed in response to the
  appropriate event. It is never possible for a new handler to be executed in
  response to an old event.]}
@end{Reason}
@end{ImplReq}

@begin{Metrics}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The implementation shall document
the following metric:]}
@begin{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0210-1]}
@ChgAdded{Version=[2],Text=[An upper bound on the lateness of the execution of
a handler. That is, the maximum time between @Chg{Version=[3],New=[the time specified
for the event and ],Old=[]}when a handler is actually
@Chg{Version=[3],New=[invoked assuming no other handler or task is executing
during this interval],Old=[executed and the time specified when the event was
set]}.]}

@end{Itemize}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The metrics for timing events.]}]}
@end{Metrics}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Text=[The protected handler procedure should be executed
directly by the real-time clock interrupt mechanism.]}

@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[For a timing event, the handler should be executed directly by the
real-time clock interrupt mechanism.]}]}

@end{ImplAdvice}

@begin{Notes}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Text=[Since a call of Set_Handler is not a potentially
blocking operation, it can be called from within a handler.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
@ChgAdded{Version=[2],Text=[A Timing_Event_Handler can be associated with several
Timing_Event objects.]}

@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00297-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The package Real_Time.Timing_Events is new.]}
@end{Extend95}

@begin{Diffword2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0094-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Reworded to eliminate a
  deadlock condition if the event time is in the past and a handler is currently
  executing. This is technically an inconsistency, but only if a program is
  depending on deadlocking; since it is impossible to imagine how that could
  be useful, we have not documented this as an inconsistency.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0210-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified the metric for lateness
  of a timing event to exclude interference from other handlers and tasks.
  This change might change the documentation of an implementation, but not
  the implementation itself, so there is no inconsistency.]}
@end{Diffword2005}


@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledAddedClause{Version=[3],Name=[Multiprocessor Implementation]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1]}
@ChgAdded{Version=[3],Text=[This clause allows implementations on multiprocessor
platforms to be configured.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The following
language-defined library package exists:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[package] System.Multiprocessors @key{is}@ChildUnit{Parent=[System],Child=[Multiprocessors]}
   @key[pragma] Preelaborate(Multiprocessors);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{CPU_Range} @key[is range] 0 .. @RI<implementation-defined>;
   @AdaObjDefn{Not_A_Specific_CPU} : @key[constant] CPU_Range := 0;
   @key[subtype] @AdaSubtypeDefn{Name=[CPU],Of=[CPU_Range]} @key[is] CPU_Range @key[range] 1 .. CPU_Range'Last;]}
@ChgImplDef{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The value of CPU_Range'Last in System.Multiprocessors.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Number_Of_CPUs} @key[return] CPU;
@key[end] System.Multiprocessors;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1]}
@ChgAdded{Version=[3],Text=[A call of Number_Of_CPUs returns the number of
processors available to the program. Within a given partition, each call on
Number_Of_CPUs will return the same value.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For a task type (including the
anonymous type of a @nt{single_task_declaration}) or subprogram, the following
language-defined representation aspect may be specified:]}

@begin{Description}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[CPU@\The aspect CPU is an @nt{expression},
which shall be of type System.Multiprocessors.CPU_Range.@AspectDefn{CPU}]}

@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[CPU],
  Text=[@ChgAdded{Version=[3],Text=[Processor on which a given task should
    run.]}]}

@end{Description}
@end{StaticSem}

@begin{Legality}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[If the CPU aspect is specified for a subprogram,
the @nt{expression} shall be static.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[The CPU aspect shall not be specified on a task
interface type.]}

@end{Legality}

@begin{Runtime}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[The @nt{expression} specified for the CPU aspect
of a task is evaluated for each task object (see
@RefSecNum{Task Units and Task Objects}). The CPU value is then associated with
the task object whose task declaration specifies the aspect.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[The CPU aspect has no effect if it is specified for
a subprogram other than the main subprogram;
the CPU value is not associated with any task.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[The CPU value is associated with the environment
task if the CPU aspect is specified for the main subprogram.
If the CPU aspect is not specified for the main subprogram it is implementation
defined on which processor the environment task executes.]}
@ChgImplDef{Version=[3],Kind=[Added],Text=[@ChgAdded{Version=[3],
Text=[The processor on which the environment task executes in the absence of a
value for the aspect CPU.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1],ARef=[AI05-0264-1]}
@ChgAdded{Version=[3],Text=[The CPU value determines the processor on which the
task will activate and execute; the task is said to be assigned to that
processor. If the CPU value is Not_A_Specific_CPU, then the task is not assigned
to a processor. A task without a CPU aspect specified will activate and execute on the
same processor as its activating task if the activating task is assigned a
processor. If the CPU value is not in the range of
System.Multiprocessors.CPU_Range or is greater than Number_Of_CPUs the task is
defined to have failed, and it becomes a completed task (see
@RefSecNum{Task Execution - Task Activation}).]}

@end{Runtime}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0171-1],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The package System.Multiprocessors and the CPU aspect are new.]}
@end{Extend2005}

@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledAddedSubClause{Version=[3],Name=[Multiprocessor Dispatching Domains]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[This clause allows implementations on multiprocessor
platforms to be partitioned into distinct dispatching domains during program
startup.]}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],KeepNext=[T],Type=[Leading],Text=[The following
language-defined library package exists:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[with] Ada.Real_Time;
@key[with] Ada.Task_Identification;
@key[package] System.Multiprocessors.Dispatching_Domains @key{is}@ChildUnit{Parent=[System.Multiprocessors],Child=[Dispatching_Domains]}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @AdaExcDefn{Dispatching_Domain_Error} : @key[exception];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] @AdaTypeDefn{Dispatching_Domain} (<>) @key[is limited private];]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @AdaObjDefn{System_Dispatching_Domain} : @key[constant] Dispatching_Domain;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Create} (First, Last : CPU) @key[return] Dispatching_Domain;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Get_First_CPU} (Domain : Dispatching_Domain) @key[return] CPU;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Get_Last_CPU}  (Domain : Dispatching_Domain) @key[return] CPU;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Get_Dispatching_Domain}
      (T   : Ada.Task_Identification.Task_Id :=
                 Ada.Task_Identification.Current_Task)
           @key[return] Dispatching_Domain;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Assign_Task}
      (Domain : @key[in out] Dispatching_Domain;
       CPU    : @key[in]     CPU_Range := Not_A_Specific_CPU;
       T      : @key[in]     Ada.Task_Identification.Task_Id :=
                 Ada.Task_Identification.Current_Task);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Set_CPU}
      (CPU : @key[in] CPU_Range;
       T   : @key[in] Ada.Task_Identification.Task_Id :=
                 Ada.Task_Identification.Current_Task);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Get_CPU}
      (T   : Ada.Task_Identification.Task_Id :=
                 Ada.Task_Identification.Current_Task)
           @key[return] CPU_Range;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[procedure] @AdaSubDefn{Delay_Until_And_Set_CPU}
      (Delay_Until_Time : @key[in] Ada.Real_Time.Time; CPU : @key[in] CPU_Range);]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key[private]
   ... -- @RI[not specified by the language]
@key[end] System.Multiprocessors.Dispatching_Domains;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[The type Dispatching_Domain represents a series of
processors on which a task may execute. Each processor is contained within
exactly one Dispatching_Domain. System_Dispatching_Domain contains the processor
or processors on which the environment task executes. At program start-up all
processors are contained within System_Dispatching_Domain.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For a task type (including the
anonymous type of a @nt{single_task_declaration}), the following language-defined
representation aspect may be specified:]}

@begin{Description}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Dispatching_Domain@\The value of
aspect Dispatching_Domain is an @nt{expression}, which shall be of
type Dispatching_Domains.Dispatching_Domain. This aspect is the domain to
which the task (or all objects of the task type) are
assigned.@AspectDefn{Dispatching_Domain}]}

@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Dispatching_Domain],
  Text=[@ChgAdded{Version=[3],Text=[Domain (group of processors) on which a
    given task should run.]}]}

@end{Description}
@end{StaticSem}

@begin{Legality}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[The Dispatching_Domain aspect shall not be specified
for a task interface.]}

@end{Legality}

@begin{Runtime}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[The expression specified for the Dispatching_Domain
aspect of a task is evaluated for each task object (see
@RefSecNum{Task Units and Task Objects}). The Dispatching_Domain value is then
associated with the task object whose task declaration specifies the aspect.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[If a task is not explicitly assigned to any domain,
it is assigned to that of the activating task. A task always executes on some
CPU in its domain.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[If both Dispatching_Domain and CPU are specified for
a task, and the CPU value is not contained within the range of processors for
the domain (and is not Not_A_Specific_CPU), the activation of the task is
defined to have failed, and it becomes a completed task (see
@RefSecNum{Task Execution - Task Activation}).]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[The function Create creates and returns a
Dispatching_Domain containing all the processors in the range First .. Last.
These processors are removed from System_Dispatching_Domain. A call of Create
will raise Dispatching_Domain_Error if any designated processor is not currently
in System_Dispatching_Domain, or if the system cannot support a distinct domain
over the processors identified, or if a processor has a task assigned to it, or
if the allocation would leave System_Dispatching_Domain empty. A call of Create
will raise Dispatching_Domain_Error if the calling task is not the environment
task, or if Create is called after the call to the main subprogram.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[The function Get_First_CPU returns the first CPU in
Domain; Get_Last_CPU returns the last one.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[The function Get_Dispatching_Domain returns the
Dispatching_Domain on which the task is assigned.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1],ARef=[AI05-0278-1]}
@ChgAdded{Version=[3],Text=[A call of the procedure Assign_Task assigns task T
to the CPU within Dispatching_Domain Domain. Task T can now execute only on CPU
unless CPU designates Not_A_Specific_CPU, in which case it can execute on any
processor within Domain. The exception Dispatching_Domain_Error is propagated if
T is already assigned to a Dispatching_Domain other than
System_Dispatching_Domain, or if CPU is not one of the processors of Domain (and
is not Not_A_Specific_CPU). A call of Assign_Task is a task dispatching point
for task T unless T is inside of a protected action, in which case the effect on
task T is delayed until its next task dispatching point. If T is the
Current_Task the effect is immediate if T is not inside a protected action,
otherwise the effect is as soon as practical. Assigning a task to
System_Dispatching_Domain that is already assigned to that domain has no
effect.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1],ARef=[AI05-0278-1]}
@ChgAdded{Version=[3],Text=[A call of procedure Set_CPU assigns task T to the
CPU. Task T can now execute only on CPU, unless CPU designates
Not_A_Specific_CPU, in which case it can execute on any processor within its
Dispatching_Domain. The exception Dispatching_Domain_Error is propagated if CPU
is not one of the processors of the Dispatching_Domain on which T is assigned
(and is not Not_A_Specific_CPU). A call of Set_CPU is a task dispatching point
for task T unless T is inside of a protected action, in which case the effect on
task T is delayed until its next task dispatching point. If T is the
Current_Task the effect is immediate if T is not inside a protected action,
otherwise the effect is as soon as practical.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[The function Get_CPU returns the processor assigned
to task T, or Not_A_Specific_CPU if the task is not assigned to a processor.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[A call of Delay_Until_And_Set_CPU delays the calling
task for the designated time and then assigns the task to the specified
processor when the delay expires. The exception Dispatching_Domain_Error is
propagated if P is not one of the processors of the calling task's
Dispatching_Domain (and is not Not_A_Specific_CPU).]}

@end{Runtime}

@begin{ImplReq}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[The implementation shall perform the operations
Assign_Task, Set_CPU, Get_CPU and Delay_Until_And_Set_CPU atomically with
respect to any of these operations on the same dispatching_domain, processor or
task.]}

@end{ImplReq}

@begin{ImplAdvice}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[Each dispatching domain should have separate and
disjoint ready queues.]}

@ChgImplAdvice{Version=[3],Kind=[AddedNormal],Text=[@ChgAdded{Version=[3],
Text=[Each dispatching domain should have separate and
disjoint ready queues.]}]}

@end{ImplAdvice}

@begin{DocReq}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[The implementation shall document the processor(s)
on which the clock interrupt is handled and hence where delay queue and ready
queue manipulations occur. For any Interrupt_Id whose handler can execute on
more than one processor the implementation shall also document this set of
processors.]}

@ChgDocReq{Version=[3],Kind=[AddedNormal],Text=[@ChgAdded{Version=[3],
Text=[The processor(s) on which the clock interrupt is handled; the processors
on which each Interrupt_Id can be handled.]}]}

@end{DocReq}

@begin{ImplPerm}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[An implementation may limit the number of
dispatching domains that can be created and raise Dispatching_Domain_Error if an
attempt is made to exceed this number.]}

@end{ImplPerm}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1],ARef=[AI05-0278-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The package System.Multiprocessors.Dispatching_Domains and the aspect
  Dispatching_Domains are new.]}
@end{Extend2005}

