@Part(xxx, Root="rat.msm")

@comment($Source: e:\\cvsroot/ARM/Rationale/tasking.mss,v $)
@comment($Revision: 1.6 $ $Date: 2006/12/23 06:01:58 $)

@LabeledSection{Tasking and Real-Time}

@i{This chapter
describes various improvements in the tasking and real-time areas
for Ada 2005.}

@i{There are only a few changes to the core tasking model itself. One
major extension, however, is the ability to combine the interface
feature described in an earlier chapter with the tasking model; this
draws together the object-oriented and tasking models of Ada which
previously were disjoint aspects of the language.}

@i{There are also many additional predefined packages in the Real-Time
Systems annex concerning matters such as scheduling and timing; these
form the major topic of this chapter.}


@LabeledClause{Ada Issues: Tasking and Real-Time}


The WG9 guidance document @LocalLink{Target=[R1],Sec=[References],Text={[1]}}
identifies real-time systems as an important area. It says

"The main purpose of the Amendment is to address identified problems
in Ada that are interfering with Ada's usage or adoption, especially
in its major application areas (such as high-reliability, long-lived
real-time and/or embedded applications and very large complex systems).
The resulting changes may range from relatively minor, to more substantial."

@leading@;It then identifies the inclusion of the Ravenscar profile
@LocalLink{Target=[R4],Sec=[References],Text={[4]}} (for predictable real-time)
as a worthwhile addition and then asks the ARG to pay particular attention to

@begin[SyntaxText]
Improvements that will maintain or improve Ada's advantages, especially
in those user domains where safety and criticality are prime concerns.
Within this area it cites as high priority, improvements in the real-time
features and improvements in the high integrity features.
@end[SyntaxText]

@leading@;Ada 2005 does indeed make many improvements in the real-time area
and includes the Ravenscar profile as specifically mentioned. The
following Ada Issues cover the relevant changes and are described
in detail in this chapter:

@begin[Description]
@begin[Description]@Comment{Second one to indent this}
@AILink{AI=[AI95-00249-01],Text=[249]}@\Ravenscar profile for high-integrity
systems

@AILink{AI=[AI95-00265-01],Text=[265]}@\Partition elaboration policy for
high-integrity systems

@AILink{AI=[AI95-00266-02],Text=[266]}@\Task termination procedure

@AILink{AI=[AI95-00297-01],Text=[297]}@\Timing events

@AILink{AI=[AI95-00298-01],Text=[298]}@\Non-preemptive dispatching

@AILink{AI=[AI95-00305-01],Text=[305]}@\New pragma and restrictions for
real-time systems

@AILink{AI=[AI95-00307-01],Text=[307]}@\Execution-time clocks

@AILink{AI=[AI95-00321-01],Text=[321]}@\Definition of dispatching policies

@AILink{AI=[AI95-00327-01],Text=[327]}@\Dynamic ceiling priorities

@AILink{AI=[AI95-00345-01],Text=[345]}@\Protected and task interfaces

@AILink{AI=[AI95-00347-01],Text=[347]}@\Title of Annex H

@AILink{AI=[AI95-00354-01],Text=[354]}@\Group execution-time budgets

@AILink{AI=[AI95-00355-01],Text=[355]}@\Priority dispatching including Round
Robin

@AILink{AI=[AI95-00357-01],Text=[357]}@\Earliest Deadline First scheduling

@AILink{AI=[AI95-00386-01],Text=[386]}@\Further functions returning time-span
values

@AILink{AI=[AI95-00394-01],Text=[394]}@\Redundant Restrictions identifiers and
Ravenscar

@AILink{AI=[AI95-00397-01],Text=[397]}@\Conformance and overriding for
procedures and entries

@AILink{AI=[AI95-00399-01],Text=[399]}@\Single tasks and protected objects with
interfaces

@AILink{AI=[AI95-00421-01],Text=[421]}@\Sequential activation and attachment

@AILink{AI=[AI95-00443-01],Text=[443]}@\Synchronized private extensions

@AILink{AI=[AI95-00445-01],Text=[445]}@\Dynamic ceilings and interrupt handlers

@end[Description]
@end[Description]

These changes can be grouped as follows.

First there is the introduction of a mechanism for monitoring task
termination (@AILink{AI=[AI95-00266-02],Text=[266]}).

A major innovation in the core language is the introduction of synchronized
interfaces which provide a high degree of unification between the
object-oriented and real-time aspects of Ada
(@AILink{AI=[AI95-00345-01],Text=[345]},
@AILink{AI=[AI95-00397-01],Text=[397]},
@AILink{AI=[AI95-00399-01],Text=[399]},
@AILink{AI=[AI95-00443-01],Text=[443]}).

There is of course the introduction of the Ravenscar profile
(@AILink{AI=[AI95-00249-01],Text=[249]}) plus associated restrictions
(@AILink{AI=[AI95-00305-01],Text=[305]},
@AILink{AI=[AI95-00394-01],Text=[394]}) in the
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-D.html],Text=[Real-Time Systems annex (D)]}.

There are major improvement to the scheduling and task dispatching mechanisms
with the addition of further standard policies
(@AILink{AI=[AI95-00298-01],Text=[298]},
@AILink{AI=[AI95-00321-01],Text=[321]},
@AILink{AI=[AI95-00327-01],Text=[327]},
@AILink{AI=[AI95-00355-01],Text=[355]},
@AILink{AI=[AI95-00357-01],Text=[357]},
@AILink{AI=[AI95-00445-01],Text=[445]}). These are also in Annex D.

A number of timing mechanisms are now provided. These concern stand-alone
timers, timers for monitoring the CPU time of a single task, and timers
for controlling the budgeting of time for groups of tasks
(@AILink{AI=[AI95-00297-01],Text=[297]},
@AILink{AI=[AI95-00307-01],Text=[307]},
@AILink{AI=[AI95-00354-01],Text=[354]},
@AILink{AI=[AI95-00386-01],Text=[386]}). Again these are in Annex D.

Finally, more control is provided over partition elaboration which is very
relevant to real-time high-integrity systems
(@AILink{AI=[AI95-00265-01],Text=[265]},
@AILink{AI=[AI95-00421-01],Text=[421]}). This is in
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-H.html],Text=[Annex
H]} which is now entitled High Integrity Systems
(@AILink{AI=[AI95-00347-01],Text=[347]}).

Note that further operations for the manipulation of time in child packages of
@exam[Calendar] (@AILink{AI=[AI95-00351-01],Text=[351]}) will be discussed with
the predefined library in Section @RefSecNum{Times and dates}.


@LabeledClause{Task termination}

@leading@;In the Introduction (in Section
@RefSecNum{Overview: Tasking and real-time facilities})
we mentioned the problem of how tasks can have
a silent death in Ada 95. This happens if a task raises an exception
which is not handled by the task itself. Tasks may also terminate
because of going abnormal as well as terminating normally. The detection
of task termination and its causes can be monitored in Ada 2005 by
the package @exam[Ada.Task_Termination] whose specification is
essentially@Defn{task termination handler}

@begin[Example]
@key[with] Ada.Task_Identification; @key[use] Ada.Task_Identification;
@key[with] Ada.Exceptions; @key[use] Ada.Exceptions;
@key[package] Ada.Task_Termination @key[is]
   @key[pragma] Preelaborable(Task_Termination);

   @key[type] Cause_Of_Termination @key[is] (Normal, Abnormal, Unhandled_Exception);

   @key[type] Termination_Handler @key[is access protected]
         @key[procedure](Cause: @key[in] Cause_Of_Termination;
                         T: @key[in] Task_Id; X: @key[in] Exception_Occurrence);

   @key[procedure] Set_Dependents_Fallback_Handler (Handler: @key[in] Termination_Handler);
   @key[function] Current_Task_Fallback_Handler @key[return] Termination_Handler;

   @key[procedure] Set_Specific_Handler(T: @key[in] Task_Id; Handler: @key[in] Termination_Handler);
   @key[function] Specific_Handler(T: @key[in] Task_Id) @key[return] Termination_Handler;

@key[end] Ada.Task_Termination;
@end[Example]

(The above includes use clauses in order to simplify the presentation;
the actual package does not have use clauses. The other predefined packages
described in this chapter are treated similarly.)

The general idea is that we can associate a protected procedure with
a task. The protected procedure is then invoked when the task terminates
with an indication of the reason passed via its parameters. The protected
procedure is identified by using the type @exam[Termination_Handler]
which is an access type referring to a protected procedure.

@leading@;The association can be done in two ways. Thus (as in the Introduction)
we might declare a protected object @exam[Grim_Reaper]
@begin[Example]
@key[protected] Grim_Reaper @key[is]
   @key[procedure] Last_Gasp(C: Cause_Of_Termination; T: Task_Id; X: Exception_Occurrence);
@key[end] Grim_Reaper;
@end[Example]

which contains the protected procedure @exam[Last_Gasp]. Note that
the parameters of @exam[Last_Gasp] match those of the access type
@exam[Termination_Handler].

@leading@;We can then nominate @exam[Last_Gasp] as the protected procedure to
be called when the specific task @exam[T] dies by
@begin[Example]
Set_Specific_Handler(T'Identity, Grim_Reaper.Last_Gasp'Access);
@end[Example]

@leading@;Alternatively we can nominate @exam[Last_Gasp] as the protected
procedure to be called when any of the tasks dependent on the current task
becomes terminated by writing
@begin[Example]
Set_Dependents_Fallback_Handler(Grim_Reaper.Last_Gasp'Access);
@end[Example]

Note that a task is not dependent upon itself and so this does not
set a handler for the current task.

Thus a task can have two handlers. A fallback handler and a specific
handler and either or both of these can be null. When a task terminates
(that is after any finalization but just before it vanishes), the
specific handler is invoked if it is not null. If the specific handler
is null, then the fallback handler is invoked unless it too is null.
If both are null then no handler is invoked.

@leading@;The body of protected procedure @exam[Last_Gasp] might then output
various diagnostic messages to a log for later analysis, thus
@begin[Example]
@key[procedure] Last_Gasp(C: Cause_Of_Termination; T: Task_Id; X: Exception_Occurrence) @key[is]
@key[begin]
   @key[case] C @key[is]
      @key[when] Normal => @key[null];
      @key[when] Abnormal =>
         Put_Log("Something nasty happened to task ");
         Put_Log(Image(T));
      @key[when] Unhandled_Exception =>
         Put_Log("Unhandled exception occurred in task ");
         Put_Log(Image(T));
         Put_Log(Exception_Information(X));
   @key[end case];
@key[end] Last_Gasp;
@end[Example]

There are three possible reasons for termination, it could be normal,
abnormal (caused by abort), or because of propagation of an unhandled
exception. In the last case the parameter @exam[X] gives details of
the exception occurrence whereas in the other cases @exam[X] has the
value @exam[Null_Occurrence].

Initially both specific and fallback handlers are null for all tasks.
However, note that if a fallback handler has been set for all dependent
tasks of @exam[T] then the handler will also apply to any task subsequently
created by @exam[T] or one of its descendants. Thus a task can be
born with a fallback handler already in place.

If a new handler is set then it replaces any existing handler of the
appropriate kind. Calling either setting procedure with null for the
handler naturally sets the appropriate handler to null.
The current handlers can be found by calling the functions
@exam[Current_Task_Fallback_Handler] or @exam[Specific_Handler]; they return
null if the handler is null.

It is important to realise that the fallback handlers for the tasks
dependent on @exam[T] need not all be the same since one of the dependent
tasks of @exam[T] might set a different handler for its own dependent
tasks. Thus the fallback handlers for a tree of tasks can be different
in various subtrees. This structure is reflected by the fact that
the determination of the current fallback handler of a task is in
fact done by searching recursively the tasks on which it depends.

Note that we cannot directly interrogate the fallback handler of a
specific task but only that of the current task. Also, if a task
sets a fallback handler for its dependents and then enquires of its
own fallback handler it will not in general get the same answer since
it is not one of its own dependents.

Remember the situation regarding the environment
task. This unnamed task is the task that elaborates the library units
and then calls the main subprogram. Library tasks (that
is tasks declared at library level) are activated by the environment
task before it calls the main subprogram.

@leading@;Suppose the main subprogram calls the setting procedures as follows
@begin[Example]
@key[procedure] Main @key[is]

   @key[protected] RIP @key[is]
      @key[procedure] One( ... );
      @key[procedure] Two( ... );
   @key[end];
   ...
@key[begin]
   Set_Dependents_Fallback_Handler(RIP.One'Access);
   Set_Specific_Handler(Current_Task, RIP.Two'Access);
   ...
@key[end] Main;
@end[Example]

The specific handler for the environment task is then set to @exam[Two]
(because @exam[Current_Task] is the environment task at this point)
but the fallback handler for the environment task is null. On the
other hand the fallback handler for all other tasks in the program
including any library tasks is set to @exam[One]. Note that it is
not possible to set the fallback handler for the environment task.

@leading@;The astute reader will note that there is actually a race condition
here since a library task might have terminated before the handler
gets set. We could overcome this by setting the handler as part of
the elaboration code thus
@begin[Example]
@tabset[P42]
@key[package] Start_Up @key[is]
   @key[pragma] Elaborate_Body;
@key[end];

@key[with] Ada.Task_Termination; @key[use] Ada.Task_Termination;
@key[package body] Start_Up @key[is]
@key[begin]
   Set_Dependents_Fallback_Handler(RIP.One'Access);
@key[end] Start_Up;

@key[with] Start_Up;
@key[pragma] Elaborate(Start_Up);
@key[package] Library_Tasks @key[is]
   ...@\-- @examcom[declare library tasks here]
@key[end];
@end[Example]

Note how the use of pragmas @exam[Elaborate_Body] and @exam[Elaborate]
ensures that things get done in the correct order.

Some minor points are that if we try to set the specific handler for
a task that has already terminated then @exam[Tasking_Error] is raised.
And if we try to set the specific handler for the null task, that
is call @exam[Set_Specific_Handler] with parameter @exam[T] equal
to @exam[Null_Task_Id], then @exam[Program_Error] is raised. These
exceptions are also raised by calls of the function @exam[Specific_Handler]
in similar circumstances.


@LabeledClause{Synchronized interfaces}


We now turn to the most important improvement to the core tasking
features introduced by Ada 2005. This concerns the coupling of object
oriented and real-time features through inheritance.

@leading@;Recall from the chapter on the object oriented model (see
Section @RefSecNum{Interfaces}) that we can declare
an interface thus
@begin[Example]
@key[type] Int @key[is interface];
@end[Example]

@leading@;An interface is essentially an abstract tagged type that cannot have
any components but can have abstract operations and null procedures.
We can then derive other interfaces and tagged types by inheritance
such as
@begin[Example]
@key[type] Another_Int @key[is interface and] Int1 @key[and] Int2;

@key[type] T @key[is new] Int1 @key[and] Int2;

@key[type] TT @key[is new] T @key[and] Int3 @key[and] Int4;
@end[Example]

Remember that a tagged type can be derived from at most one other
normal tagged type but can also be derived from several interfaces.
In the list, the first is called the parent (it can be a normal tagged
type or an interface) and any others (which can only be interfaces)
are called progenitors.

Ada 2005 also introduces further categories of interfaces, namely
synchronized, protected, and task interfaces. A synchronized interface
can be implemented by either a task or protected type; a protected
interface can only be implemented by a protected type and a task interface
can only be implemented by a task type.@Defn{synchronized interface}@Defn{task interface}@Defn{protected interface}@Defn2{Term=[interface],Sec=[synchronized]}@Defn2{Term=[interface],Sec=[task]}@Defn2{Term=[interface],Sec=[protected]}

A nonlimited interface can only be implemented by a nonlimited type.
However, an explicitly marked limited interface can be implemented
by any tagged type (limited or not) or by a protected or task type.
Remember that task and protected types are inherently limited. Note
that we use the term limited interface to refer collectively to interfaces
marked limited, synchronized, task or protected and we use explicitly
limited to refer to those actually marked as limited.

@leading@keepnext@;So we can write
@begin[Example]
@tabset[P49]
@key[type] LI @key[is limited interface];@\-- @examcom[similarly a type LI2]

@key[type] SI@key[ is synchronized interface];

@key[type] TI @key[is task interface];

@key[type] PI @key[is protected interface];
@end[Example]

and we can of course provide operations which must be abstract or
null. (Remember that @key[synchronized] is a new reserved word.)@Defn2{Term=[synchronized],Sec=[keyword]}

@leading@;We can compose these interfaces provided that no conflict arises.
The following are all permitted:
@begin[Example]
@key[type] TI2 @key[is task interface and] LI @key[and] TI;

@key[type] LI3 @key[is limited interface and] LI @key[and] LI2;

@key[type] TI3 @key[is task interface and] LI @key[and] LI2;

@key[type] SI2 @key[is synchronized interface and] LI @key[and] SI;
@end[Example]

The rule is simply that we can compose two or more interfaces provided
that we do not mix task and protected interfaces and the resulting
interface must be not earlier in the hierarchy: limited, synchronized,
task/protected than any of the ancestor interfaces.

@leading@;We can derive a real task type or protected type from one or more
of the appropriate interfaces
@begin[Example]
@tabset[P42]
@key[task type] TT @key[is new] TI @key[with]
   ...@\-- @examcom[and here we give entries as usual]
@key[end] TT;
@end[Example]

@leading@;or

@begin[Example]
@key[protected type] PT @key[is new] LI @key[and] SI @key[with]
   ...
@key[end] PT;
@end[Example]

Unlike tagged record types we cannot derive a task or protected type
from another task or protected type as well. So the derivation hierarchy
can only be one level deep once we declare an actual task or protected
type.

The operations of these various interfaces are declared in the usual
way and an interface composed of several interfaces has the operations
of all of them with the same rules regarding duplication and overriding
of an abstract operation by a null one and so on as for normal tagged
types.

When we declare an actual task or protected type then we must implement
all of the operations of the interfaces concerned. This can be done
in two ways, either by declaring an entry or protected operation in
the specification of the task or protected object or by declaring
a distinct subprogram in the same list of declarations (but not both).
Of course, if an operation is null then it can be inherited or overridden
as usual.

@leading@;Thus the interface
@begin[Example]
@key[package] Pkg @key[is]
   @key[type] TI @key[is task interface];
   @key[procedure] P(X: @key[in] TI) @key[is abstract];
   @key[procedure] Q(X: @key[in] TI; I: @key[in] Integer) @key[is null];
@key[end] Pkg;
@end[Example]

@leading@;could be implemented by
@begin[Example]
@tabset[P42]
@key[package] PT1 @key[is]
   @key[task type] TT1 @key[is new] TI @key[with]
      @key[entry] P; @\-- @examcom[P and Q implemented by entries]
      @key[entry] Q(I: @key[in] Integer);
   @key[end] TT1;
@key[end] PT1;
@end[Example]

@leading@;or by

@begin[Example]
@tabset[P42]
@key[package] PT2 @key[is]
   @key[task type] TT2@key[ is new ]TI @key[with]
      @key[entry] P;@\-- @examcom[P implemented by an entry]
   @key[end] TT2;
   ...@\-- @examcom[Q implemented by a procedure]
   @key[procedure] Q(X: @key[in] TT2; I: @key[in] Integer);
@key[end] PT2;

@end[Example]

@leading@;or even by
@begin[Example]
@tabset[P42]
@key[package] PT3 @key[is]
   @key[task type] TT3 @key[is new] TI @key[with end];
@\-- @examcom[P implemented by a procedure ]
@\-- @examcom[Q inherited as a null procedure]
   @key[procedure] P(X: @key[in] TT3);
@key[end] PT3;
@end[Example]

In this last case there are no entries and so we have the juxtaposition
@key[with end] which is somewhat similar to the juxtaposition @key[is
end] that occurs with generic packages used as signatures.

@leading@;Observe how the first parameter which denotes the task is omitted
if it is implemented by an entry. This echoes the new prefixed notation
for calling operations of tagged types in general. Remember that rather
than writing
@begin[Example]
Op(X, Y, Z, ...);
@end[Example]

@leading@keepnext@;we can write
@begin[Example]
X.Op(Y, Z, ...);
@end[Example]

provided certain conditions hold such as that @exam[X] is of a tagged
type and that @exam[Op] is a primitive operation of that type.

In order for the implementation of an interface operation by an entry
of a task type or a protected operation of a protected type to be
possible some fairly obvious conditions must be satisfied.

In all cases the first parameter of the interface operation must be
of the task type or protected type (it may be an access parameter).

In addition, in the case of a protected type, the first parameter
of an operation implemented by a protected procedure or entry must
have mode @key[out] or @key[in out] (and in the case of an access
parameter it must be an access to variable parameter).

If the operation does not fit these rules then it has to be implemented
as a subprogram. An important example is that a function has to be
implemented as a function in the case of a task type because there
is no such thing as a function entry. However, a function can often
be directly implemented as a protected function in the case of a protected
type.

Entries and protected operations which implement inherited operations
may be in the visible part or private part of the task or protected
type in the same way as for tagged record types.

It may seem rather odd that an operation can be implemented by a subprogram
that is not part of the task or protected type itself @en it seems
as if it might not be task safe in some way. But a common paradigm
is where an operation as an abstraction has to be implemented by two
or more entry calls. An example occurs in some implementations of
the classic readers and writers problem as we shall see in a moment.

Of course a task or protected type which implements an interface can
have additional entries and operations as well just as a derived tagged
type can have more operations than its parent.

@leading@;The overriding indicators @key[overriding] and @key[not overriding]
can be applied to entries as well as to procedures. Thus the package
@exam[PT2] above could be written as@Defn{overriding indicator}
@begin[Example]
@tabset[P42]
@key[package] PT2 @key[is]
   @key[task type] TT2@key[ is new ]TI @key[with]
      @key[overriding]@\-- @examcom[P implemented by an entry]
      @key[entry] P;
   @key[end] TT2;

   @key[overriding]@\-- @examcom[Q implemented by procedure]
   @key[procedure] Q(X: @key[in] TT2; I: @key[in] Integer);
@key[end] PT2;
@end[Example]

@leading:We will now explore a simple readers and writers example in order
to illustrate various points. We start with the following interface
@begin[Example]
@key[package] RWP @key[is]
   @key[type] RW @key[is limited interface];
   @key[procedure] Write(Obj: @key[out] RW; X: @key[in] Item) @key[is abstract];
   @key[procedure] Read(Obj: @key[in] RW; X: @key[out] Item) @key[is abstract];
@key[end] RWP;

@end[Example]
The intention here is that the interface describes the abstraction
of providing an encapsulation of a hidden location and a means of
writing a value (of some type @exam[Item]) to it and reading a value
from it @en very trivial.

@leading@;We could implement this in a nonsynchronized manner thus
@begin[Example]
@key[type] Simple_RW @key[is new] RW @key[with]
   @key[record]
      V: Item;
   @key[end record];

@key[overriding]
@key[procedure] Write(Obj: @key[out] Simple_RW; X: @key[in] Item);

@key[overriding]
@key[procedure] Read(Obj: @key[in] Simple_RW; X: @key[out] Item);

...

@key[procedure] Write(Obj: @key[out] Simple_RW; X: @key[in] Item)
@key[is]
@key[begin]
   Obj.V := X;
@key[end] Write;

@key[procedure] Read(Obj: @key[in] Simple_RW; X: @key[out] Item) @key[is]
@key[begin]
   X := Obj.V;
@key[end] Read;
@end[Example]

This implementation is of course not task safe (task safe is sometimes
referred to as thread-safe). If a task calls @exam[Write] and the
type @exam[Item] is a composite type and the writing task is interrupted
part of the way through writing, then a task which calls @exam[Read]
might get a curious result consisting of part of the new value and
part of the old value.

@leading@;For illustration we could derive a synchronized interface
@begin[Example]
@key[type] Sync_RW @key[is synchronized interface and] RW;
@end[Example]

@leading@;This interface can only be implemented by a task or protected type.
For a protected type we might have
@begin[Example]
@key[protected type] Prot_RW @key[is new ]Sync_RW @key[with]
   @key[overriding]
   @key[procedure] Write(X: @key[in] Item);
   @key[overriding]
   @key[procedure] Read(X: @key[out] Item);
@key[private]
   V: Item;
@key[end];

@key[protected body] Prot_RW @key[is]
   @key[procedure] Write(X: @key[in] Item) @key[is]
   @key[begin]
      V := X;
   @key[end] Write;

   @key[procedure] Read(X: @key[out] Item) is
   @key[begin]
      X := V;
   @key[end] Read;
@key[end] Prot_RW;
@end[Example]

Again observe how the first parameter of the interface operations
is omitted when they are implemented by protected operations.

This implementation is perfectly task safe. However, one of the characteristics
of the readers and writers example is that it is quite safe to allow
multiple readers since they cannot interfere with each other. But
the type @exam[Prot_RW] does not allow multiple readers because protected
procedures can only be executed by one task at a time.

@leading@keepnext@;Now consider
@begin[Example]
@key[protected type] Multi_Prot_RW @key[is new] Sync_RW @key[with]
   @key[overriding]
   @key[procedure] Write(X: @key[in] Item);
   @key[not overriding]
   @key[function] Read @key[return] Item;
@key[private]
   V: Item;
@key[end];

@key[overriding]
@key[procedure] Read(Obj: @key[in] Multi_Prot_RW; X: @key[out] Item);

...

@key[protected body] Multi_Prot_RW @key[is]
   @key[procedure] Write(X: @key[in] Item) @key[is]
   @key[begin]
      V := X;
   @key[end] Write;

   @key[function] Read @key[return] Item is
   @key[begin]
      @key[return] V;
   @key[end] Read;
@key[end] Multi_Prot_RW;

@key[procedure] Read(Obj: @key[in] Multi_Prot_RW; X: @key[out] Item)
@key[is]
@key[begin]
   X := Obj.Read;
@key[end] Read;
@end[Example]

In this implementation the procedure @exam[Read] is implemented by
a procedure outside the protected type and this procedure then calls
the function @exam[Read] within the protected type. This allows multiple
readers because one of the characteristics of protected functions
is that multiple execution is permitted (but of course calls of the
protected procedure @exam[Write] are locked out while any calls of
the protected function are in progress). The structure is emphasized
by the use of overriding indicators.

@leading@;A simple tasking implementation might be as follows
@begin[Example]
@key[task type] Task_RW@key[ is new] Sync_RW @key[with]
   @key[overriding]
   @key[entry] Write(X: @key[in] Item);
   @key[overriding]
   @key[entry] Read(X: @key[out] Item);
@key[end];

@key[task body] Task_RW @key[is
]   V: Item;
@key[begin]
   @key[loop]
      @key[select]
         @key[accept] Write(X: @key[in] Item) @key[do]
            V := X;
         @key[end] Write;
      @key[or]
         @key[accept] Read(X: @key[out] Item) @key[do]
            X := V;
         @key[end] Read;
      @key[or]
         @key[terminate];
      @key[end select];
   @key[end loop];
@key[end] Task_RW;
@end[Example]

Finally, here is a tasking implementation which allows multiple readers
and ensures that an initial value is set by only allowing a call of
@exam[Write] first. It is based on an example in @i{Programming in Ada 95}
by the author @LocalLink{Target=[R6],Sec=[References],Text={[6]}}.
@begin[Example]
@key[task type] Multi_Task_RW(V: @key[access] Item) @key[is] @key[new] Sync_RW @key[with]
   @key[overriding]
   @key[entry] Write(X: @key[in] Item);
   @key[not] @key[overriding]
   @key[entry] Start;
   @key[not overriding]
   @key[entry] Stop;
@key[end];

@key[overriding]
@key[procedure] Read(Obj: @key[in] Multi_Task_RW; X: @key[out] Item);

...

@key[task body] Multi_Task_RW @key[is]
   Readers: Integer := 0;
@key[begin]
   @key[accept] Write(X: @key[in] Item) do
      V.@key[all] := X;
   @key[end] Write;
   @key[loop]
      @key[select]
         @key[when] Write'Count = 0 =>
         @key[accept] Start;
         Readers := Readers + 1;
      @key[or]
         @key[accept] Stop;
         Readers := Readers @en 1;

      @key[or]
         @key[when] Readers = 0 =>
         @key[accept] Write(X: @key[in] Item) @key[do]
            V.@key[all] := X;
         @key[end] Write;
      @key[or]
         @key[terminate];
      @key[end select];
   @key[end loop];
@key[end] Multi_Task_RW;

@key[overriding
procedure] Read(Obj: @key[in] Multi_Task_RW; X: @key[out] Item) @key[is]
@key[begin]
   Obj.Start;
   X := Obj.V.@key[all];
   Obj.Stop;
@key[end] Read;
@end[Example]

In this case the data being protected is accessed via the access discriminant
of the task. It is structured this way so that the procedure @exam[Read]
can read the data directly. Note also that the procedure @exam[Read]
(which is the implementation of the procedure @exam[Read] of the interface)
calls two entries of the task.

It should be observed that this last example is by way of illustration
only. As is well known, the @exam[Count] attribute used in tasks (as
opposed to protected objects) can be misleading if tasks are aborted
or if entry calls are timed out. Moreover, it would be gruesomely
slow.

So we have seen that a limited interface such as @exam[RW] might be
implemented by a normal tagged type (plus its various operations)
and by a protected type and also by a task type. We could then dispatch
to the operations of any of these according to the tag of the type
concerned. Observe that task and protected types are now other forms
of tagged types and so we have to be careful to say tagged record
type (or informally, normal tagged type) where appropriate.

In the above example, the types @exam[Simple_RW], @exam[Prot_RW],
@exam[Multi_Prot_RW], @exam[Task_RW] and @exam[Multi_Task_RW] all
implement the interface @exam[RW].

@leading@keepnext@;So we might have
@begin[Example]
@tabset[P42]
RW_Ptr: @key[access] RW'Class := ...

...
RW_Ptr.Write(An_Item);@\-- @examcom[dispatches]
@end[Example]

and according to the value in @exam[RW_Ptr] this might call the appropriate
entry or procedure of an object of any of the types implementing the
interface @exam[RW].

@leading@keepnext@;However if we have
@begin[Example]
Sync_RW_Ptr: @key[access] Sync_RW'Class := ...
@end[Example]

@leading@;then we know that any implementation of the synchronized interface
@exam[Sync_RW] will be task safe because it can only be implemented
by a task or protected type. So the dispatching call
@begin[Example]
@tabset[P42]
Sync_RW_Ptr.Write(An_Item);@\-- @examcom[task safe dispatching]
@end[Example]

will be task safe.

An interesting point is that because a dispatching call might be to
an entry or to a procedure we now permit what appear to be procedure
calls in timed entry calls if they might dispatch to an entry.

@leading@keepnext@;So we could have
@begin[Example]
@tabset[P42]
@key[select]
   RW_Ptr.Read(An_Item);@\-- @examcom[dispatches]
@key[or]
   @key[delay] Seconds(10);
@key[end select];
@end[Example]

Of course it might dispatch to the procedure @exam[Read] if the type
concerned turns out to be @exam[Simple_RW] in which case a time out
could not occur. But if it dispatched to the entry @exam[Read] of
the type @exam[Task_RW] then it could time out.

@leading@;On the other hand we are not allowed to use a timed call if it is
statically known to be a procedure. So
@begin[Example]
@tabset[P42]
A_Simple_Object: Simple_RW;
...
@key[select]
   A_Simple_Object.Read(An_Item);@\-- @examcom[illegal]
@key[or]
   @key[delay] Seconds(10);
@key[end select];
@end[Example]

is not permitted.

A note of caution is in order. Remember that the time out is to when
the call gets accepted. If it dispatches to @exam[Multi_Task_RW.Read]
then time out never happens because the @exam[Read] itself is a procedure
and gets called at once. However, behind the scenes it calls two entries
and so could take a long time. But if we called the two entries directly
with timed calls then we would get a time out if there were a lethargic
writer in progress. So the wrapper distorts the abstraction. In a
sense this is not much worse than the problem we have anyway that
a time out is to when a call is accepted and not to when it returns
@en it could hardly be otherwise.

The same rules apply to conditional entry calls and also to asynchronous
select statements where the triggering statement can be a dispatching
call.

In a similar way we also permit timed calls on entries renamed as
procedures. But note that we do not allow timed calls on generic formal
subprograms even though they might be implemented as entries.

Another important point to note is that we can as usual assume the
common properties of the class concerned. Thus in the case of a task
interface we know that it must be implemented by a task and so the
operations such as @key[abort] and the attributes @exam[Identity],
@exam[Callable] and so on can be applied. If we know that an interface
is synchronized then we do know that it has to be implemented by a
task or a protected type and so is task safe.

@leading@;Typically an interface is implemented by a task or protected type
but it can also be implemented by a singleton task or protected object
despite the fact that singletons have no type name. Thus we might
have
@begin[Example]
@key[protected] An_RW @key[is new] Sync_RW @key[with]
   @key[procedure] Write(X: @key[in] Item);
   @key[procedure] Read(X: @key[out] Item);
@key[end];
@end[Example]

with the obvious body. However we could not declare a single protected
object similar to the type @exam[Multi_Prot_RW] above. This is because
we need a type name in order to declare the overriding procedure @exam[Read]
outside the protected object. So singleton implementations are possible
provided that the interface can be implemented directly by the task
or protected object without external subprograms.

@leading@keepnext@;Here is another example
@begin[Example]
@key[type] Map @key[is protected interface];
@key[procedure] Put(M: Map; K: Key; V: Value)@key[ is abstract];
@end[Example]

@leading@keepnext@;can be implemented by

@begin[Example]
@key[protected] A_Map@key[ is new] Map @key[with]
   @key[procedure] Put(K: Key; V: Value);
   ...
@key[end] A_Map;
@end[Example]

@leading@;There is an important rule about tagged private types and synchronized
interfaces. Both partial and full view must be synchronized or not.
Thus if we wrote
@begin[Example]
@tabset[P49]
@key[type] SI @key[is synchronized interface];
@key[type] T @key[is synchronized new] SI @key[with private];@\-- @examcom[Says synchronized]
@end[Example]

then the full type @exam[T] has to be a task type or protected type
or possibly a synchronized, protected or task interface.

@leading@;It is vital that the synchronized property cannot be hidden
since this would violate privacy. This is
largely because type extensions of synchronized interfaces and tagged
concurrent types are not allowed. We musn't
need to look into the private part to see whether a type extension is allowed.
Note that the word @key[synchronized] is always given. We could also write

@begin[Example]
@tabset[P49]
@key[type] LI @key[is limited interface];
@key[type] T @key[is synchronized new] LI @key[with private];
@end[Example]

in which case the ancestor is not synchronized. But the fact that @exam[T]
is synchronized is clearly visible.

@leading@;It might be remembered that if a private view is untagged then the
full view might be tagged. In this case type extension is not allowed with the
private view anyway and so the full type might be synchronized. So we can have
(in Ada 95 as well)

@begin[Example]
@tabset[P49]
   @key[type] T @key[is limited private];@\-- @examcom[untagged]
@key[private]
   @key[task type] T @key[is] ...@\-- @examcom[synchronized property is hidden]
@end[Example]

@leading@keepnext@;but we cannot have

@begin[Example]
@tabset[P49]
   @key[type] T @key[is abstract tagged limited private];@\-- @examcom[tagged]
@key[private]
   @key[type] T @key[is synchronized interface];@\-- @examcom[illegal]
@end[Example]

@leading@;We conclude this discussion on interfaces by saying a few words about
the use of the word @key[limited]. (Much of this has already been explained
in the chapter on the object oriented model (see @RefSecNum{Interfaces}) but it
is worth repeating
in the context of concurrent types.) We always explicitly insert @key[limited],
@key[synchronized], @key[task], or @key[protected] in the case of a
limited interface
in order to avoid confusion. So to derive a new explicitly limited
interface from an existing limited interface @exam[LI] we write
@begin[Example]
@key[type] LI2 @key[is limited interface and] LI;
@end[Example]

whereas in the case of normal types we can write
@begin[Example]
@tabset[P42]
@key[type] LT @key[is limited] ...

@key[type] LT2 @key[is new] LT @key[and] LI @key[with] ...@\-- @examcom[LT2 is limited]
@end[Example]

@leading@;then @exam[LT2] is limited by the normal derivation rules. Types take
their limitedness from their parent (the first one in the list, provided
it is not an interface) and it does not have to be given explicitly
on type derivation @en although it can be in Ada 2005 thus
@begin[Example]
@key[type] LT2 @key[is limited new] LT @key[and] LI @key[with] ...
@end[Example]

Remember the important rule that all descendants of a nonlimited interface
have to be nonlimited because otherwise limited types could end up
with an assignment operation.

@leading@keepnext@;This means that we cannot write
@begin[Example]
@tabset[P42]
@key[type] NLI @key[is interface];@\-- @examcom[nonlimited]

@key[type] LI @key[is limited interface];@\-- @examcom[limited]

@key[task type] TT@key[ is new] NLI @key[and] LI @key[with] ...@\--@examcom[illegal]
@end[Example]

This is illegal because the interface @exam[NLI] in the declaration
of the task type @exam[TT] is not limited.


@LabeledClause{The Ravenscar profile}

The purpose of the Ravenscar profile is to restrict the use of many
tasking facilities so that the effect of the program is predictable.
The profile was defined by the International Real-Time Ada Workshops
which met twice at the remote village of Ravenscar on the coast of
Yorkshire in North-East England. A general description of the principles
and use of the profile in high integrity systems will be found in
an ISO/IEC Technical Report [2] and so we shall not cover that material
here.@Defn{Ravenscar profile}

Here is a historical interlude. It is reputed that the hotel in which
the workshops were held was originally built as a retreat for King
George III to keep a mistress. Another odd rumour is that he ordered
all the natural trees to be removed and replaced by metallic ones
whose metal leaves clattered in the wind. It also seems that Henry
Bolingbroke landed at Ravenscar in July 1399 on his way to take the
throne as Henry IV. Ravenscar is mentioned several times by Shakespeare
in Act II of King Richard II; it is spelt Ravenspurg which is slightly
confusing @en maybe we need the ability to rename profile identifiers.

@leading@Defn{Profile pragma}@Defn2{Term=[pragma],Sec=[Profile]}@;A
profile is a mode of operation and is specified by the pragma @exam[Profile]
which defines the particular profile to be used. The syntax is
@begin[Example]
@key[pragma] Profile(@examcom[profile]_identifier [ , profile_argument_associations]);


@end[Example]

where @exam[profile_argument_associations] is simply a list of pragma
argument associations separated by commas.

@leading@;Thus to ensure that a program conforms to the Ravenscar profile we
write
@begin[Example]
@key[pragma] Profile(Ravenscar);
@end[Example]

The general idea is that a profile is equivalent to a set of configuration
pragmas.

In the case of Ravenscar the pragma is equivalent to the joint effect
of the following pragmas
@begin[Example]
@key[pragma] Task_Dispatching_Policy(FIFO_Within_Priorities);
@key[pragma] Locking_Policy(Ceiling_Locking);
@key[pragma] Detect_Blocking;

@key[pragma] Restrictions(
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
     No_Dependence => Ada.Execution_Time.Group_Budget,
     No_Dependence => Ada.Execution_Time.Timers,
     No_Dependence => Ada.Task_Attributes);
@end[Example]

The pragma @exam[Detect_Blocking] plus many of the Restrictions identifiers
are new to Ada 2005. These will now be described.

The pragma @exam[Detect_Blocking], as its name implies, ensures that
the implementation will detect a potentially blocking operation in
a protected operation and raise @exam[Program_Error]. Without this
pragma the implementation is not required to detect blocking and so
tasks might be locked out for an unbounded time and the program might
even deadlock.@Defn{Detect_Blocking pragma}@Defn2{Term=[pragma],Sec=[Detect_Blocking]}

The identifier @exam[No_Dynamic_Attachment] means that there are no
calls of the operations in the package @exam[Ada.Interrupts].@Defn2{Term=[restrictions identifier],Sec=[No_Dynamic_Attachment]}

The identifier @exam[No_Dynamic_Priorities] means that there is no
dependence on the package @exam[Ada.Priorities] as well as no uses
of the attribute @exam[Priority] (this is a new attribute for protected
objects as explained at the end of this
section).@Defn2{Term=[restrictions identifier],Sec=[No_Dynamic_Priorities]}

Note that the rules are that you cannot read as well as not write
the priorities @en this applies to both the procedure for reading
task priorities and reading the attribute for protected objects.

The identifier @exam[No_Local_Protected_Objects] means that protected objects
can only be declared at library level and the identifier
@exam[No_Protected_Type_Allocators] means that there are no allocators for
protected objects or objects containing components of protected
types.@Defn2{Term=[restrictions identifier],Sec=[No_Local_Protected_Objects]}@Defn2{Term=[restrictions identifier],Sec=[No_Protected_Type_Allocators]}

The identifier @exam[No_Local_Timing_Events] means that objects of
the type @exam[Timing_Event] in the package @exam[Ada.Real_Time.Timing_Events]
can only be declared at library level. This package is described in
Section 6 below.@Defn2{Term=[restrictions identifier],Sec=[No_Local_Timing_Events]}

The identifiers @exam[No_Relative_Delay], @exam[No_Requeue_Statements],
and @exam[No_Select_Statements] mean that there are no relative delay,
requeue or select statements
respectively.@Defn2{Term=[restrictions identifier],Sec=[No_Relative_Delay]}@Defn2{Term=[restrictions identifier],Sec=[No_Requeue_Statements]}@Defn2{Term=[restrictions identifier],Sec=[No_Select_Statements]}

The identifier @exam[No_Specific_Termination_Handlers] means that
there are no calls of the procedure @exam[Set_Specific_Handler] or
the function @exam[Specific_Handler] in the package @exam[Task_Termination]
and the identifier @exam[No_Task_Termination] means that all tasks
should run for ever. Note that we are permitted to set a fallback
handler so that if any task does attempt to terminate then it will
be detected.@Defn2{Term=[restrictions identifier],Sec=[No_Specific_Termination_Handlers]}

The identifier @exam[Simple_Barriers] means that the Boolean expression
in a barrier of an entry of a protected object shall be either a static
expression (such as @exam[True]) or a Boolean component of the protected
object itself.@Defn2{Term=[restrictions identifier],Sec=[Simple_Barriers]}

The Restrictions identifier @exam[Max_Entry_Queue_Length] sets a limit
on the number of calls permitted on an entry queue. It is an important
property of the Ravenscar profile that only one call is permitted
at a time on an entry queue of a protected
object.@Defn2{Term=[restrictions identifier],Sec=[Max_Entry_Queue_Length]}

The identifier @exam[No_Dependence] is not specific to the Real-Time
Systems annex and is properly described in Section
@RefSecNum{Pragmas and Restrictions}.
In essence it indicates that the program does not depend upon the given
language defined package. In this case it means that a program conforming to
the Ravenscar profile cannot use any of the packages @exam[Asynchronous_Task_Control],
@exam[Calendar], @exam[Execution_Time.Group_Budget],
@exam[Execution_Time.Timers] and @exam[Task_Attributes].
Some of these packages are new and are described later in this chapter (in
Section @RefSecNum{CPU clocks and timers}).@Defn2{Term=[restrictions identifier],Sec=[No_Dependence]}

Note that @exam[No_Dependence] cannot be used for @exam[No_Dynamic_Attachment]
because that would prevent use of the child package @exam[Ada.Interrupts.Names].

All the other restrictions identifiers used by the Ravenscar profile were
already defined in Ada 95. Note also that the identifier
@exam[No_Asynchronous_Control] has been moved to
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-J-13.html],Text=[Annex J]}
because it can now be replaced by the use of @exam[No_Dependence].


@LabeledClause{Scheduling and dispatching}

Another area of increased flexibility in Ada 2005 is that of task dispatching
policies. In Ada 95, the only predefined policy is
@exam[FIFO_Within_Priorities] although other policies are permitted. Ada 2005
provides further pragmas, policies and packages which facilitate many different
mechanisms such as non-preemption within priorities, the familiar Round Robin
using timeslicing, and the more recently acclaimed Earliest Deadline First
(EDF) policy. Moreover it is possible to mix different policies according to
priority level within a partition.@Defn{task dispatching policy}@Defn2{Term=[policy],Sec=[task dispatching]}

@leading@;In order to accommodate these many changes, Section D.2 (Priority
Scheduling) of the Reference Manual has been reorganized as follows
@begin[Example]
D.2.1  The Task Dispatching Model
D.2.2  Task Dispatching Pragmas
D.2.3  Preemptive Dispatching
D.2.4  Non-Preemptive Dispatching
D.2.5  Round Robin Dispatching
D.2.6  Earliest Deadline First Dispatching
@end[Example]

@leading@keepnext@;Overall control is provided by two pragmas. They are
@begin[Example]
@tabset[P21]
@key[pragma] Task_Dispatching_Policy(@examcom[policy_]identifier);

@key[pragma] Priority_Specific_Dispatching(
@\@examcom[policy_]identifer,
@\@examcom[first_priority_]expression,
@\@examcom[last_priority_]expression);
@end[Example]

The pragma @exam[Task_Dispatching_Policy], which already exists in
Ada 95, applies the same policy throughout a whole
partition.@Defn{Task_Dispatching_Policy pragma}@Defn2{Term=[pragma],Sec=[Task_Dispatching_Policy]}
The pragma @exam[Priority_Specific_Dispatching], which is new in Ada 2005,
can be used to set different policies for different ranges of priority
levels.@Defn{Priority_Specific_Dispatching pragma}@Defn2{Term=[pragma],Sec=[Priority_Specific_Dispatching]}

@leading@keepnext@;The full set of predefined policies in Ada 2005 is

@begin[Description]
@exam[FIFO_Within_Priorities] @en@\This already
exists in Ada 95. Within each priority level to which it applies tasks
are dealt with on a first-in-first-out basis. Moreover, a task may
preempt a task of a lower priority.@Defn2{Term=[policy],Sec=[FIFO_Within_Priorities]}

@exam[Non_Preemptive_FIFO_Within_Priorities] @en@\This
is new in Ada 2005. Within each priority level to which it applies
tasks run to completion or until they are blocked or execute a delay
statement. A task cannot be preempted by one of higher priority. This
sort of policy is widely used in high integrity
applications.@Defn2{Term=[policy],Sec=[Non_Preemptive_FIFO_Within_Priorities]}

@exam[Round_Robin_Within_Priorities] @en@\This
is new in Ada 2005. Within each priority level to which it applies
tasks are timesliced with an interval that can be specified. This
is a very traditional policy widely used since the earliest days of
concurrent
programming.@Defn2{Term=[policy],Sec=[Round_Robin_Within_Priorities]}

@exam[EDF_Across_Priorities] @en@\This is new in
Ada 2005. This provides Earliest Deadline First dispatching. The general
idea is that within a range of priority levels, each task has a deadline
and that with the earliest deadline is processed. This is a fashionable
new policy and has mathematically provable advantages with respect
to efficiency.@Defn2{Term=[policy],Sec=[EDF_Across_Priorities]}
@end[Description]

For further details of these policies consult
@i{Concurrent and Real-Time Programming in Ada 2005}
by Alan Burns and Andy Wellings @LocalLink{Target=[R8],Sec=[References],Text={[8]}}.

@leading@;These various policies are controlled by the package
@exam[Ada.Dispatching] plus two child packages. The root package has
specification@Defn2{Term=[package],Sec=[Ada.Dispatching]}@Defn{Ada.Dispatching package}@Defn{Dispatching package}
@begin[Example]
@key[package] Ada.Dispatching @key[is]
   @key[pragma] Pure(Dispatching);
   Dispatching_Policy_Error: @key[exception];
@key[end] Ada.Dispatching;
@end[Example]

As can be seen this root package simply declares the exception
@exam[Dispatching_Policy_Error] which is used by the child packages.

@leading@;The child package @exam[Round_Robin] enables the setting of the time
quanta for time slicing within one or more priority levels. Its specification
is@Defn2{Term=[package],Sec=[Ada.Dispatching.Round_Robin]}@Defn{Ada.Dispatching.Round_Robin package}@Defn{Dispatching.Round_Robin package}
@begin[Example]
@key[with] System;  @key[use] System;
@key[with] Ada.Real_Time;  @key[use] Ada.Real_Time;
@key[package] Ada.Dispatching.Round_Robin @key[is]
   Default_Quantum: @key[constant] Time_Span := @examcom[implementation-defined];
   @key[procedure] Set_Quantum(Pri: @key[in] Priority, Quantum: @key[in] Time_Span);
   @key[procedure] Set_Quantum(Low, High: @key[in] Priority; Quantum: @key[in] Time_Span);
   @key[function] Actual_Quantum(Pri: Priority) @key[return] Time_Span;
   @key[function] Is_Round_Robin(Pri: Priority) @key[return] Boolean;
@key[end] Ada.Dispatching.Round_Robin;
@end[Example]

The procedures @exam[Set_Quantum] enable the time quantum to be used
for time slicing to be set for one or a range of priority levels.
The default value is of course the constant @exam[Default_Quantum].
The function @exam[Actual_Quantum] enables us to find out the current
value of the quantum being used for a particular priority level. Its
identifier reflects the fact that the implementation may not be able
to apply the exact actual value given in a call of @exam[Set_Quantum].
The function @exam[Is_Round_Robin] enables us to check whether the
round robin policy has been applied to the given priority level. If
we attempt to do something stupid such as set the quantum for a priority
level to which the round robin policy does not apply then the exception
@exam[Dispatching_Policy_Error] is raised.

@leading@;The other new policy concerns deadlines and is controlled by a new
pragma @exam[Relative_Deadline] and the child package @exam[Dispatching.EDF].
The syntax of the pragma is@Defn2{Term=[pragma],Sec=[Relative_Deadline]}@Defn{Relative_Deadline pragma}
@begin[Example]
@key[pragma] Relative_Deadline(@examcom[relative_deadline_]expression);
@end[Example]

@leading@;The deadline of a task is a property similar to priority and both
are used for scheduling. Every task has a priority of type @exam[Integer]
and every task has a deadline of type @exam[Ada.Real_Time.Time]. Priorities
can be set when a task is created by pragma @exam[Priority]
@begin[Example]
@key[task] T @key[is]
   @key[pragma] Priority(P);
@end[Example]

@leading@;and deadlines can similarly be set by the pragma
@exam[Relative_Deadline] thus
@begin[Example]
@key[task] T @key[is]
   @key[pragma] Relative_Deadline(RD);
@end[Example]

@leading@;The expression @exam[RD] has type @exam[Ada.Real_Time.Time_Span].
Note carefully that the pragma sets the relative and not the absolute
deadline. The initial absolute deadline of the task is
@begin[Example]
Ada.Real_Time.Clock + RD
@end[Example]

where the call of @exam[Clock] is made between task creation and the
start of its activation.

@leading@;Both pragmas @exam[Priority] and @exam[Relative_Deadline] can appear
in the main subprogram and they then apply to the environment task.
If they appear in any other subprogram then they are ignored. Both
properties can also be set via a discriminant. In the case of priorities
we can write
@begin[Example]
@key[task type] TT(P: Priority) @key[is]
   @key[pragma] Priority(P);
   ...
@key[end];

High_Task: TT(13);
Low_Task: TT(7);
@end[Example]

@leading@;We cannot do the direct equivalent for deadlines because
@exam[Time_Span] is private and so not discrete. We have to use an access
discriminant thus
@begin[Example]
@key[task type] TT(RD: @key[access] Timespan) @key[is]
   @key[pragma] Relative_Deadline(RD.@key[all]);
   ...
@key[end];

One_Sec: @key[aliased] @key[constant] Time_Span := Seconds(1);
Ten_Mins: @key[aliased] @key[constant] Time_Span := Minutes(10);

Hot_Task: TT(One_Sec'Access);
Cool_Task: TT(Ten_Mins'Access);
@end[Example]

Note incidentally that functions @exam[Seconds] and @exam[Minutes]
have been added to the package @exam[Ada.Real_Time]. Existing functions
@exam[Nanoseconds], @exam[Microseconds] and @exam[Milliseconds] in
Ada 95 enable the convenient specification of short real time intervals
(values of type @exam[Time_Span]). However, the specification of longer
intervals such as four minutes meant writing something like
@exam[Milliseconds(240_000)] or perhaps @exam[4*60*Milliseconds(1000)]. In view
of the fact that EDF scheduling and timers (see @RefSecNum{CPU clocks and timers})
would be likely to require longer times the functions @exam[Seconds] and
@exam[Minutes] are added in Ada 2005. There is no function @exam[Hours] because
the range of time spans is only guaranteed to be 3600 seconds anyway.
(The numerate will recall that 3600 seconds is one hour.)

If a task is created and it does not have a pragma @exam[Priority] then its
initial priority is that of the task that created it. If a task does not have a
pragma @exam[Relative_Deadline] then its initial absolute deadline is the
constant @exam[Default_Deadline] in the package @exam[Ada.Dispatching.EDF];
this constant has the value @exam[Ada.Real_Time.Time_Last] (effectively the end
of the universe).

Priorities can be dynamically manipulated by the subprograms in the
package @exam[Ada.Dynamic_Priorities] and deadlines can similarly
be manipulated by the subprograms in the package @exam[Ada.Dispatching.EDF]
whose specification
is@Defn2{Term=[package],Sec=[Ada.Dispatching.EDF]}@Defn{Ada.Dispatching.EDF package}@Defn{Dispatching.EDF package}
@begin[Example]
@tabset[P42]
@key[with] Ada.Real_Time;  @key[use] Ada.Real_Time;
@key[with] Ada.Task_Identification; @key[use] Ada.Task_Identification;
@key[package] Ada.Dispatching.EDF @key[is]
   @key[subtype] Deadline @key[is] Ada.Real_Time.Time;
   Default_Deadline: @key[constant] Deadline := Time_Last;
   @key[procedure] Set_Deadline(D: @key[in] Deadline; T: @key[in] Task_Id := Current_Task);
   @key[procedure] Delay_Until_And_Set_Deadline (
@\Delay_Until_Time: @key[in] Time;
@\Deadline_Offset: @key[in] Time_Span);
   @key[function] Get_Deadline(T: Task_Id := Current_Task) @key[return]
Deadline;
@key[end] Ada.Dispatching.EDF;
@end[Example]

The subtype @exam[Deadline] is just declared as a handy abbreviation.
The constant @exam[Default_Deadline] is set to the end of the universe
as already mentioned. The procedure @exam[Set_Deadline] sets the deadline
of the task concerned to the value of the parameter @exam[D]. The
long-winded @exam[Delay_Until_And_Set_Deadline] delays the task concerned
until the value of @exam[Delay_Until_Time] and sets its deadline to
be the interval @exam[Deadline_Offset] from that time @en this is
useful for periodic tasks. The function @exam[Get_Deadline] enables
us to find the current deadline of a task.

It is important to note that this package can be used to set and retrieve
deadlines for tasks whether or not they are subject to EDF dispatching.
We could for example use an ATC on a deadline overrun (ATC = Asynchronous
Transfer of Control using a select statement). Hence there is no function
@exam[Is_EDF] corresponding to @exam[Is_Round_Robin] and calls of
the subprograms in this package can never raise the exception
@exam[Dispatching_Policy_Error].@Defn{ATC}

If we attempt to apply one of the subprograms in this package to a
task that has already terminated then @exam[Tasking_Error] is raised.
If the task parameter is @exam[Null_Task_Id] then @exam[Program_Error]
is raised.

@leading@;As mentioned earlier, a policy can be selected for a whole partition
by for example
@begin[Example]
@key[pragma] Task_Dispatching_Policy(Round_Robin_Within_Priorities);
@end[Example]

@leading@;whereas in order to mix different policies across different priority
levels we can write
@begin[Example]
@key[pragma] Priority_Specific_Dispatching(Round_Robin_Within_Priority, 1, 1);
@key[pragma] Priority_Specific_Dispatching(EDF_Across_Priorities, 2, 10);
@key[pragma] Priority_Specific_Dispatching(FIFO_Within_Priority, 11, 24);
@end[Example]

This sets Round Robin at priority level 1, EDF at levels 2 to 10,
and FIFO at levels 11 to 24. This means for example that none of the
EDF tasks can run if any of the FIFO ones can. In other words if any
tasks in the highest group can run then they will do so and none in
the other groups can run. The scheduling within a range takes over
only if tasks in that range can go and none in the higher ranges can.

@leading@keepnext@;Note that if we write
@begin[Example]
@key[pragma] Priority_Specific_Dispatching(EDF_Across_Priorities, 2, 5);
@key[pragma] Priority_Specific_Dispatching(EDF_Across_Priorities, 6, 10);
@end[Example]

@leading@keepnext@;then this is not the same us

@begin[Example]
@key[pragma] Priority_Specific_Dispatching(EDF_Across_Priorities, 2, 10);
@end[Example]

despite the fact that the two ranges in the first case are contiguous.
This is because in the first case any task in the 6 to 10 range will
take precedence over any task in the 2 to 5 range whatever the deadlines.
If there is just one range then only the deadlines count in deciding
which tasks are scheduled.
This is emphasized by the fact that the policy name uses @exam[Across] rather
than @exam[Within]. For other policies such as
@exam[Round_Robin_Within_Priority] two contiguous ranges would be the same as a
single range.

We conclude this section with a few words about ceiling priorities.

@leading@;In Ada 95, the priority of a task can be changed during
execution but the ceiling priority
of a protected object cannot be so changed. It is permanently set when
the object is created using the pragma @exam[Priority]. This is often
done using a discriminant so that at least different objects of a
given protected type can have different priorities. Thus we might
have
@begin[Example]
@tabset[P42]
@key[protected type] PT(P: Priority) @key[is]
   @key[pragma] Priority(P);
   ...
@key[end] PT;

PO: PT(7);@\-- @examcom[ceiling priority is 7]
@end[Example]

The fact that the ceiling priority of a protected object is static
can be a nuisance in many applications especially when the priority
of tasks can be dynamic. A common workaround is to give a protected
object a higher ceiling than needed in all circumstances (often called
"the ceiling of ceilings"). This results in tasks having a higher
active priority than necessary when accessing the protected object
and this can interfere with the processing of other tasks in the system
and thus upset overall schedulability. Moreover, it means that a task
of high priority can access an object when it should not (if a task
with a priority higher than the ceiling priority of a protected object
attempts to access the object then @exam[Program_Error] is raised
@en if the object has an inflated priority then this check will pass
when it should not).

This difficulty is overcome in Ada 2005 by allowing protected objects
to change their priority. This is done through the introduction of
an attribute @exam[Priority] which applies just to protected objects.
It can only be accessed within the body of the protected object concerned.

@leading@;As an example a protected object might have a procedure to change
its ceiling priority by a given amount. This could be written as follows
@begin[Example]
@tabset[P42]
@key[protected type] PT @key[is]
   @key[procedure] Change_Priority(Change: @key[in] Integer);
   ...
@key[end];

@key[protected body] PT @key[is]
   @key[procedure] Change_Priority(Change: @key[in] Integer) @key[is]
   @key[begin]
      ...   @\-- @examcom[PT'Priority has old value here]
      PT'Priority := PT'Priority + Change;
      ...   @\-- @examcom[PT'Priority has new value here]
      ...
   @key[end] Change_Priority;
   ...
@key[end] PT;
@end[Example]

Changing the ceiling priority is thus done while mutual exclusion
is in force. Although the value of the attribute itself is changed
immediately when the assignment is made, the actual ceiling priority of
the protected object is only changed when the protected operation
(in this case the call of @exam[Change_Priority]) is finished.
Note that if any of the procedures of the protected object is an interrupt
handler (through pragma Attach_Handler or Interrupt_Handler) then a check is
made that the value is in the range of System.Interrupt_Priority; Program_Error
is raised if the check fails.

Note the unusual syntax. Here we permit an attribute as the destination
of an assignment statement. This happens nowhere else in the language.
Other forms of syntax were considered but this seemed the most expressive.


@LabeledClause{CPU clocks and timers}

Ada 2005 introduces three different kinds of timers. Two are concerned
with monitoring the CPU time of tasks @en one applies to a single
task and the other to groups of tasks. The third timer measures real
time rather than execution time and can be used to trigger events
at specific real times. We will look first at the CPU timers because
that introduces more new concepts.@Defn{timer}@Defn{CPU timer}

The execution time of one or more tasks can be monitored and controlled
by the new package @exam[Ada.Execution_Time] plus two child packages.
@begin[Description]
@exam[Ada.Execution_Time] @en@\This is the root
package and enables the monitoring of execution time of individual
tasks.

@exam[Ada.Execution_Time.Timers] @en@\This provides
facilities for defining and enabling timers and for establishing a
handler which is called by the run time system when the execution
time of the task reaches a given value.

@exam[Ada.Execution_Time.Group_Budgets] @en@\
This enables several tasks to share a budget
and provides means whereby action can be taken when the budget expires.
@end[Description]

@leading@;The execution time of a task, or CPU time as it is commonly called,
is the time spent by the system executing the task and services on
its behalf. CPU times are represented by the private type @exam[CPU_Time].
This type and various subprograms are declared in the root package
@exam[Ada.Execution_Time] whose specification is as follows (as before
we have added some use clauses in order to ease the
presentation)@Defn2{Term=[package],Sec=[Ada.Execution_Time]}@Defn{Ada.Execution_Time package}@Defn{Execution_Time package}
@begin[Example]
@key[with] Ada.Task_Identification;  @key[use] Ada.Task_Identification;
@key[with] Ada.Real_Time;  @key[use] Ada.Real_Time;
@key[package] Ada.Execution_Time @key[is]

   @key[type] CPU_Time @key[is private];
   CPU_Time_First: @key[constant] CPU_Time;
   CPU_Time_Last: @key[constant] CPU_Time;
   CPU_Time_Unit: @key[constant] := @examcom[implementation-defined-real-number];
   CPU_Tick: @key[constant] Time_Span;

   @key[function] Clock(T: Task_Id := Current_Task) @key[return] CPU_Time;

   @key[function] "+" (Left: CPU_Time; Right: Time_Span) @key[return] CPU_Time;
   @key[function] "+" (Left: Time_Span; Right: CPU_Time) @key[return] CPU_Time;
   @key[function] "@en" (Left: CPU_Time; Right: Time_Span) @key[return] CPU_Time;
   @key[function] "@en" (Left: CPU_Time; Right: CPU_Time) @key[return] Time_Span;

   @key[function] "<" (Left, Right: CPU_Time) @key[return] Boolean;
   @key[function] "<=" (Left, Right: CPU_Time) @key[return] Boolean;
   @key[function] ">" (Left, Right: CPU_Time) @key[return] Boolean;
   @key[function] ">=" (Left, Right: CPU_Time) @key[return] Boolean;

   @key[procedure] Split(T: @key[in] CPU_Time; SC: @key[out] Seconds_Count; TS: @key[out] Time_Span);
   @key[function] Time_Of(SC: Seconds_Count; TS: Time_Span := Time_Span_Zero)
                        @key[return] CPU_Time;

@key[private]
   ... -- @examcom[not specified by the language]
@key[end] Ada.Execution_Time;
@end[Example]

The CPU time of a particular task is obtained by calling the function
@exam[Clock] with the task as parameter. It is set to zero at task
creation.

The constants @exam[CPU_Time_First] and @exam[CPU_Time_Last] give the range of
values of @exam[CPU_Time]. @exam[CPU_Tick] gives the average interval during
which successive calls of @exam[Clock] give the same value and thus is a
measure of the accuracy whereas @exam[CPU_Time_Unit] gives the unit of time
measured in seconds. We are assured that @exam[CPU_Tick] is no greater than one
millisecond and that the range of values of @exam[CPU_Time] is at least 50
years (provided always of course that the implementation can cope).

The various subprograms perform obvious operations on the type @exam[CPU_Time]
and the type @exam[Time_Span] of the package @exam[Ada.Real_Time].

@leading@;A value of type @exam[CPU_Time] can be converted to a
@exam[Seconds_Count] plus residual @exam[Time_Span] by the function
@exam[Split] which is similar to that in the package @exam[Ada.Real_Time]. The
function @exam[Time_Of] similarly works in the opposite direction. Note the
default value of @exam[Time_Span_Zero] for the second parameter @en this
enables times of exact numbers of seconds to be given more conveniently thus
@begin[Example]
Four_Secs: CPU_Time := Time_Of(4);
@end[Example]

@leading@;In order to find out when a task reaches a particular CPU time we
can use the facilities of the child package @exam[Ada.Execution_Time.Timers]
whose specification@Defn2{Term=[timer],Sec=[CPU time]}
is@Defn2{Term=[package],Sec=[Ada.Execution_Time.Timers]}@Defn{Ada.Execution_Time.Timers package}@Defn{Execution_Time.Timers package}
@begin[Example]
@key[with] System;  @key[use] System;
@key[package] Ada.Execution_Time.Timers @key[is]

   @key[type] Timer(T: @key[not null] @key[access] @key[constant] Task_Id) @key[is tagged limited private];
   @key[type] Timer_Handler @key[is] @key[access protected procedure] (TM:@key[ in out] Timer);

   Min_Handler_Ceiling: @key[constant] Any_Priority := @examcom[implementation-defined];

   @key[procedure] Set_Handler(TM: @key[in out] Timer; In_Time: Time_Span; Handler: Timer_Handler);
   @key[procedure] Set_Handler(TM:@key[ in out] Timer; At_Time: CPU_Time; Handler: Timer_Handler);

   @key[function] Current_Handler(TM: Timer) @key[return] Timer_Handler;
   @key[procedure] Cancel_Handler(TM: @key[in out] Timer; Cancelled: @key[out] Boolean);
   @key[function] Time_Remaining(TM: Timer) @key[return] Time_Span;

   Timer_Resource_Error: @key[exception];

@key[private]
   ...  -- @examcom[not specified by the language]
@key[end] Ada.Execution_Time.Timers;
@end[Example]

@leading@;The general idea is that we declare an object of type @exam[Timer]
whose discriminant identifies the task to be monitored @en note the
use of @key[not null] and @key[constant] in the discriminant. We also
declare a protected procedure which takes the timer as its parameter
and which performs the actions required when the @exam[CPU_Time] of
the task reaches some value. Thus to take some action (perhaps abort
for example although that would be ruthless) when the @exam[CPU_Time]
of the task @exam[My_Task] reaches 2.5 seconds we might first declare
@begin[Example]
My_Timer: Timer(My_Task'Identity'Access);
Time_Max: CPU_Time := Time_Of(2, Milliseconds(500));
@end[Example]

@leading@keepnext@;and then
@begin[Example]
@key[protected] Control @key[is]
   @key[procedure] Alarm(TM: @key[in out] Timer);
@key[end];

@key[protected body] Control @key[is]
   @key[procedure] Alarm(TM: @key[in out] Timer) @key[is]
   @key[begin]
      -- @examcom[abort the task]
      Abort_Task(TM.T.@key[all]);
   @key[end] Alarm;
@key[end] Control;
@end[Example]

@leading@;Finally we set the timer in motion by calling the procedure
@exam[Set_Handler] which takes the timer, the time value and (an access to) the
protected procedure thus
@begin[Example]
Set_Handler(My_Timer, Time_Max, Control.Alarm'Access);
@end[Example]

and then when the CPU time of the task reaches @exam[Time_Max], the
protected procedure @exam[Control.Alarm] is executed. Note how the
timer object incorporates the information regarding the task concerned
using an access discriminant @exam[T] and that this is passed to the
handler via its parameter @exam[TM].

@leading@;Aborting the task is perhaps a little violent. Another possibility
is simply to reduce its priority so that it is no longer troublesome,
thus
@begin[Example]
      -- @examcom[cool that task]
      Set_Priority(Priority'First, TM.T.@key[all]);
@end[Example]

Another version of @exam[Set_Handler] enables the timer to be set
for a given interval (of type @exam[Time_Span]).

The handler associated with a timer can be found by calling the function
@exam[Current_Handler]. This returns null if the timer is not set
in which case we say that the timer is clear.

@leading@;When the timer expires, and just before calling the protected procedure,
the timer is set to the clear state. One possible action of the handler,
having perhaps made a note of the expiration of the timer, it to set
the handler again or perhaps another handler. So we might have
@begin[Example]
@tabset[P42]
@key[protected body] Control @key[is]
   @key[procedure] Alarm(TM: @key[in out] Timer) @key[is]
   @key[begin]
      Log_Overflow(TM);@\ -- @examcom[note that timer had expired]
      -- @examcom[and then reset it for another 500 milliseconds]

      Set_Handler(TM, Milliseconds(500), Kill'Access);
   @key[end] Alarm;

   @key[procedure] Kill(TM:@key[ in out] Timer) @key[is]
   @key[begin]
      -- @examcom[expired again so kill it]
      Abort_Task(TM.T.@key[all]);
   @key[end] Kill;
@key[end] Control;
@end[Example]

In this scenario we make a note of the fact that the task has overrun
and then give it another 500 milliseconds but with the handler
@exam[Control.Kill] so that the second time is the last chance.

@leading@;Setting the value of 500 milliseconds directly in the call is a bit
crude. It might be better to parameterize the protected type thus
@begin[Example]
@key[protected type] Control(MS: Integer) @key[is] ...
...
My_Control: Control(500);
@end[Example]

@leading@;and then the call of @exam[Set_Handler] in the protected procedure
@exam[Alarm] would be
@begin[Example]
Set_Handler(TM, Milliseconds(MS), Kill'Access);
@end[Example]

Observe that overload resolution neatly distinguishes whether we are
calling @exam[Set_Handler] with an absolute time or a relative time.

The procedure @exam[Cancel_Handler] can be used to clear a timer.
The out parameter @exam[Cancelled] is set to @exam[True] if the timer
was in fact set and @exam[False] if it was clear. The function
@exam[Time_Remaining] returns @exam[Time_Span_Zero] if the timer is not set
and otherwise the time remaining.

Note also the constant @exam[Min_Handler_Ceiling]. This is the minimum
ceiling priority that the protected procedure should have to ensure
that ceiling violation cannot occur.

This timer facility might be implemented on top of a POSIX system.
There might be a limit on the number of timers that can be supported
and an attempt to exceed this limit will raise @exam[Timer_Resource_Error].

We conclude by summarizing the general principles. A timer can be
set or clear. If it is set then it has an associated (non-null) handler
which will be called after the appropriate time. The key subprograms
are @exam[Set_Handler], @exam[Cancel_Handler] and @exam[Current_Handler].
The protected procedure has a parameter which identifies the event
for which it has been called. The same protected procedure can be
the handler for many events. The same general structure applies to
other kinds of timers which will now be described.

In order to program various so-called aperiodic servers it is necessary
for tasks to share a CPU budget.@Defn{CPU budget}@Defn{group budget}

@leading@;This can be done using the child package
@exam[Ada.Execution_Time.Group_Budgets] whose specification
is@Defn2{Term=[package],Sec=[Ada.Execution_Time.Group_Budgets]}@Defn{Ada.Execution_Time.Group_Budgets package}@Defn{Execution_Time.Group_Budgets package}
@begin[Example]
@key[with] System;  @key[use] System;
@key[package] Ada.Execution_Time.Group_Budgets @key[is]

   @key[type] Group_Budget @key[is tagged limited private];
   @key[type] Group_Budget_Handler @key[is access] @key[protected procedure ](GB: @key[in out] Group_Budget);

   @key[type] Task_Array @key[is array] (Positive @key[range] <>) @key[of] Task_Id;

   Min_Handler_Ceiling: @key[constant] Any_Priority := @examcom[implementation-defined];

   @key[procedure] Add_Task(GB: @key[in out] Group_Budget; T: @key[in] Task_Id);
   @key[procedure] Remove_Task(GB: @key[in out] Group_Budget; T: @key[in] Task_Id);
   @key[function] Is_Member(GB: Group_Budget; T: Task_Id) @key[return] Boolean;
   @key[function] Is_A_Group_Member(T: Task_Id) @key[return] Boolean;
   @key[function] Members(GB: Group_Budget) @key[return] Task_Array;

   @key[procedure] Replenish(GB: @key[in out] Group_Budget; To: @key[in] Time_Span);
   @key[procedure] Add(GB: @key[in out] Group_Budget; Interval: @key[in] Time_Span);

   @key[function] Budget_Has_Expired(GB: Group_Budget) @key[return] Boolean;
   @key[function] Budget_Remaining(GB: Group_Budget) @key[return] Time_Span;

   @key[procedure] Set_Handler(GB: @key[in out] Group_Budget; Handler: @key[in] Group_Budget_Handler);
   @key[function] Current_Handler(GB: Group_Budget) @key[return] Group_Budget_Handler;
   @key[procedure] Cancel_Handler(GB: @key[in out ]Group_Budget; Cancelled: @key[out] Boolean);

   Group_Budget_Error: @key[exception];

@key[private]
   ...  -- @examcom[not specified by the language]
@key[end] Ada.Execution_Time.Group_Budgets;
@end[Example]

This has much in common with its sibling package @exam[Timers] but
there are a number of important differences.

The first difference is that we are here considering a CPU budget
shared among several tasks. The type @exam[Group_Budget] both identifies
the group of tasks it covers and the size of the budget.

Various subprograms enable tasks in a group to be manipulated. The
procedures @exam[Add_Task] and @exam[Remove_Task] add or remove a
task. The function @exam[Is_Member] identifies whether a task belongs
to a specific group whereas @exam[Is_A_Group_Member] identifies whether
a task belongs to any group. A task cannot be a member of more than
one group. An attempt to add a task to more than one group or remove
it from the wrong group and so on raises @exam[Group_Budget_Error].
Finally the function @exam[Members] returns all the members of a group
as an array.

The value of the budget (initially @exam[Time_Span_Zero]) can be loaded
by the procedure @exam[Replenish] and increased by the procedure @exam[Add].
Whenever a budget is non-zero it is counted down as the tasks in the
group execute and so consume CPU time. Whenever a budget goes to @exam[Time_Span_Zero]
it is said to have become exhausted and is not reduced further. Note
that @exam[Add] with a negative argument can reduce a budget @en it
can even cause it to become exhausted but not make it negative.

The function @exam[Budget_Remaining] simply returns the amount left
and @exam[Budget_Has_Expired] returns @exam[True] if the budget is
exhausted and so has value @exam[Time_Span_Zero].

Whenever a budget @i[becomes] exhausted (that is when the value transitions
to zero) a hander is called if one has been set. A handler is a protected
procedure as before and  procedures @exam[Set_Handler], @exam[Cancel_Handler],
and function @exam[Current_Handler] are much as expected. But a major
difference is that @exam[Set_Handler] does not set the time value
of the budget since that is done by @exam[Replenish] and @exam[Add].
The setting of the budget and the setting of the handler are decoupled
in this package. Indeed a handler can be set even though the budget
is exhausted and the budget can be counting down even though no handler
is set. The reason for the different approach simply reflects the
usage paradigm for the feature.

@leading@;So we could set up a mechanism to monitor the CPU time usage of a
group of three tasks @exam[TA], @exam[TB], and @exam[TC] by first
declaring an object of type @exam[Group_Budget], adding the three
tasks to the group and then setting an appropriate handler. Finally
we call @exam[Replenish] which sets the counting mechanism going.
So we might write
@begin[Example]
ABC: Group_Budget;
...
Add_Task(ABC, TA'Identity);
Add_Task(ABC, TB'Identity);
Add_Task(ABC, TC'Identity);

Set_Handler(ABC, Control.Monitor'Access);
Replenish(ABC, Seconds(10));
@end[Example]

Remember that functions @exam[Seconds] and @exam[Minutes] have been
added to the package @exam[Ada.Real_Time].

@leading@keepnext@;The protected procedure might be
@begin[Example]
@tabset[P42]
@key[protected body] Control @key[is]
   @key[procedure] Monitor(GB: @key[in out] Group_Budget) @key[is]
   @key[begin]
      Log_Budget;
      Add(GB, Seconds(10));@\-- @examcom[add more time]
   @key[end] Monitor;
@key[end] Control;
@end[Example]

The procedure @exam[Monitor] logs the fact that the budget was exhausted
and then adds a further 10 seconds to it. Remember that the handler
remains set all the time in the case of group budgets whereas in the
case of the single task timers it automatically becomes cleared and
has to be set again if required.

If a task terminates then it is removed from the group as part of
the finalization process.

Note that again there is the constant @exam[Min_Handler_Ceiling].

The final kind of timer concerns real time rather than CPU time and
so is provided by a child package of @exam[Ada.Real_Time] whereas
the timers we have seen so far were provided by child packages of
@exam[Ada.Execution_Time]. The specification of the package @exam[Ada.Real_Time.Timing_Events]
is@Defn2{Term=[timer],Sec=[real time]}@Defn2{Term=[package],Sec=[Ada.Real_Time.Timing_Events]}@Defn{Ada.Real_Time.Timing_Events package}@Defn{Real_Time.Timing_Events package}
@begin[Example]
@key[package] Ada.Real_Time.Timing_Events @key[is]

   @key[type] Timing_Event @key[is tagged limited private];
   @key[type] Timing_Event_Handler @key[is access] @key[protected procedure] (Event: @key[in out] Timing_Event);

   @key[procedure] Set_Handler(Event: @key[in out] Timing_Event; At_Time: Time;
                               Handler: Timing_Event_Handler);
   @key[procedure] Set_Handler(Event: @key[in out] Timing_Event; In_Time: Time_Span;
                               Handler: Timing_Event_Handler);

   @key[function] Current_Handler(Event: Timing_Event) @key[return] Timing_Event_Handler;
   @key[procedure] Cancel_Handler(Event: @key[in out] Timing_Event; Cancelled: @key[out] Boolean);

   @key[function] Time_Of_Event(Event: Timing_Event) @key[return] Time;

@key[private]
   ...  -- @examcom[not specified by the language]
@key[end] Ada.Real_Time.Timing_Events;
@end[Example]

This package provides a very low
level facility and does not involve Ada tasks at all. It has a very
similar pattern to the package @exam[Execution_Time.Timers].
A handler can be set by @exam[Set_Handler]
and again there are two versions one for a relative time and one
for absolute time. There are also subprograms @exam[Current_Handler]
and @exam[Cancel_Handler]. If no
handler is set then @exam[Current_Handler] returns null.

@exam[Set_Handler] also specifies
the protected procedure to be called when the time is reached. Times
are of course specified using the type @exam[Real_Time]
rather than @exam[CPU_Time].

A minor difference is that this package has a function @exam[Time_Of_Event]
rather than @exam[Time_Remaining].

@leading@;A simple example was given in the introductory chapter. We repeat it
here for convenience. The idea is that we wish to ring a pinger when
our egg is boiled after four minutes. The protected procedure might
be
@begin[Example]
@key[protected] @key[body] Egg @key[is]
   @key[procedure] Is_Done(Event: @key[in out] Timing_Event) @key[is]
   @key[begin]
      Ring_The_Pinger;
   @key[end] Is_Done;
@key[end] Egg;
@end[Example]

@leading@keepnext@;and then
@begin[Example]
Egg_Done: Timing_Event;
Four_Min: Time_Span := Minutes(4);
...
Put_Egg_In_Water;
Set_Handler(Event => Egg_Done, In_Time => Four_Min, Handler => Egg.Is_Done'Access);
-- @examcom[now read newspaper whilst waiting for egg]
@end[Example]

@leading@;This is unreliable because if we are interrupted between the calls
of @exam[Put_Egg_In_Water] and @exam[Set_Handler] then the egg will
be boiled for too long. We can overcome this by adding a further protected
procedure @exam[Boil] to the protected object and placing @exam[Is_Done]
in the private part so that it becomes
@begin[Example]
@key[protected] Egg @key[is]
   @key[procedure] Boil(For_Time: @key[in] Time_Span);
@key[private]
   @key[procedure] Is_Done(Event: @key[in out] Timing_Event);
   Egg_Done: Timing_Event;
@key[end] Egg;

@key[protected] @key[body] Egg @key[is]

   @key[procedure] Boil(For_Time: @key[in] Time_Span) @key[is]
   @key[begin]
      Put_Egg_In_Water;
      Set_Handler(Egg_Done, For_Time, Is_Done'Access);
   @key[end] Boil;

   @key[procedure] Is_Done(Event: @key[in out] Timing_Event) @key[is]
   @key[begin]
      Ring_The_Pinger;
   @key[end] Is_Done;
@key[end] Egg;
@end[Example]

@leading@;This is much better. The timing mechanism is now completely
encapsulated in the protected object and the procedure @exam[Is_Done] is no
longer visible outside. So all we have to do is
@begin[Example]
Egg.Boil(Minutes(4));
-- @examcom[now read newspaper whilst waiting for egg]
@end[Example]

Of course if the telephone rings as the pinger goes off and before we have a
chance to eat the egg then it still gets overdone. One solution is to eat the
egg within the protected procedure @exam[Is_Done] as well. A gentleman would
never let a telephone call disturb his breakfast.@Defn{breakfast}

One protected procedure could be used to respond to several events.
In the case of the CPU timer the discriminant of the parameter identifies
the task; in the case of the group and real-time timers, the parameter
identifies the event.

If we want to use the same timer for several events then various techniques
are possible. Note that the timers are limited so we cannot test for
them directly. However, they are tagged and so can be extended. Moreover,
we know that they are passed by reference and that the parameters
are considered aliased.

@leading@;Suppose we are boiling six eggs in one of those French breakfast
things with a different coloured holder for each egg. We can write
@begin[Example]
@key[type] Colour @key[is] (Black, Blue, Red, Green, Yellow, Purple);

Eggs_Done: @key[array] (Colour) @key[of aliased] Timing_Event;
@end[Example]

@leading@;We can then set the handler for the egg in the red holder by
something like
@begin[Example]
Set_Handler(Eggs_Done(Red), For_Time, Is_Done'Access);
@end[Example]

@leading@keepnext@;and then the protected procedure might be
@begin[Example]
@key[procedure] Is_Done(E: @key[in out ]Timing_Event) @key[is]
@key[begin]
   @key[for] C @key[in] Colour @key[loop]
      @key[if] E'Access = Eggs_Done(C)'Access @key[then]
               -- @examcom[egg in holder colour C is ready]
         ...
         @key[return];
      @key[end if];
   @key[end loop];
               -- @examcom[falls out of loop @en unknown event!]
   @key[raise] Not_An_Egg ;
@key[end] Is_Done;
@end[Example]

Although this does work it is more than a little distasteful to compare
access values in this way and moreover requires a loop to see which
event occurred.

@leading@;A much better approach is to use type extension and view conversions.
First we extend the type @exam[Timing_Event] to include additional
information about the event (in this case the colour) so that we can
identify the particular event from within the handler
@begin[Example]
@key[type] Egg_Event @key[is new] Timing_Event @key[with]
   @key[record]
      Event_Colour: Colour;
   @key[end record];
@end[Example]

@leading@;We then declare an array of these extended events (they need not be
aliased)
@begin[Example]
Eggs_Done: @key[array] (Colour) @key[of] Egg_Event;
@end[Example]

@leading@;We can now call @exam[Set_Handler] for the egg in the red holder
@begin[Example]
Set_Handler(Eggs_Done(Red), For_Time, Is_Done'Access);
@end[Example]

This is actually a call on the @exam[Set_Handler] for the type @exam[Egg_Event]
inherited from @exam[Timing_Event]. But it is the same code anyway.

Remember that values of tagged types are always passed by reference.
This means that from within the procedure @exam[Is_Done] we can recover
the underlying type and so discover the information in the extension.
This is done by using view conversions.

@leading@;In fact we have to use two view conversions, first we convert to the
class wide type @exam[Timing_Event'Class] and then to the specific
type @exam[Egg_Event]. And then we can select the component @exam[Event_Colour].
In fact we can do these operations in one statement thus
@begin[Example]
@key[procedure] Is_Done(E: @key[in out ]Timing_Event) @key[is]
   C: @key[constant] Colour := Egg_Event(Timing_Event'Class(E)).Event_Colour;
@key[begin]
               -- @examcom[egg in holder colour C is ready]
   ...
@key[end] Is_Done;
@end[Example]

@leading@;Note that there is a check on the conversion from the class wide type
@exam[Timing_Event'Class] to the specific type @exam[Egg_Event] to
ensure that the object passed as parameter is indeed of the type @exam[Egg_Event]
(or a further extension of it). If this fails then @exam[Tag_Error]
is raised. In order to avoid this possibility we can use a membership
test. For example
@begin[Example]
@key[procedure] Is_Done(E: @key[in out ]Timing_Event) @key[is]
   C: Colour;
@key[begin]
   @key[if] Timing_Event'Class(E) @key[in] Egg_Event @key[then]
      C := Egg_Event(Timing_Event'Class(E)).Event_Colour;
               -- @examcom[egg in holder colour C is ready]
      ...
   @key[else]
               -- @examcom[unknown event @en not an egg event!]
      @key[raise] Not_An_Egg;
   @key[end if];
@key[end] Is_Done;
@end[Example]

The membership test ensures that the event is of the specific type
@exam[Egg_Event]. We could avoid the double conversion to the class
wide type by introducing an intermediate variable.

It is important to appreciate that no dispatching is involved in these
operations at all @en everything is static apart from the membership
test.

Of course, it would have been a little more flexible if the various subprograms
took a parameter of type @exam[Timing_Event'Class] but this would have
conflicted with the @exam[Restrictions] identifier @exam[No_Dispatch]. Note
that Ravenscar itself does not impose @exam[No_Dispatch] but the restriction is
in the High-Integrity annex and thus might be imposed on some high-integrity
applications which might nevertheless wish to use timers in a simple manner.

A few minor points of difference between the timers are worth summarizing.

The two CPU timers have a constant @exam[Min_Handler_Ceiling]. This
prevents ceiling violation. It is not necessary for the real-time
timer because the call of the protected procedure is treated like
an interrupt and thus is at interrupt ceiling level.

The group budget timer and the real-time timer do not have an exception
corresponding to @exam[Timer_Resource_Error] for the single task CPU
timer. As mentioned above, it is anticipated that the single timer
might be implemented on top of a POSIX system in which case there
might be a limit to the number of timers especially since each task
could be using several timers. In the group case, a task can only
be in one group so the number of group timers is necessarily less
than the number of tasks and no limit is likely to be exceeded. In
the real-time case the events are simply placed on the delay queue
and no other resources are required anyway.

It should also be noted that the group timer could be used to monitor
the execution time of a single task. However, a task can only be in
one group and so only one timer could be applied to a task that way
whereas, as just mentioned, the single CPU timer is quite different
since a given task could have several timers set for it to expire
at different times. Thus both kinds of timers have their own distinct
usage patterns.


@LabeledClause{High Integrity Systems annex}

There are a few changes to this annex. The most noticeable is that
its title has been changed from Safety and Security to
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-H.html],
Text=[High Integrity Systems]}. This reflects common practice in that
high-integrity is now
the accepted general term for systems such as safety-critical systems
and security-critical systems.

There are some small changes to reflect the introduction of the Ravenscar
profile. It is clarified that tasking is permitted in a high-integrity
system provided that it is well controlled through, for example, the
use of the Ravenscar profile.

A new pragma @exam[Partition_Elaboration_Policy] is introduced. Its
syntax is@Defn{Partition elaboration policy}@Defn2{Term=[policy],Sec=[partition elaboration]}
@begin[Example]
@key[pragma] Partition_Elaboration_Policy(@examcom[policy]_identifier);
@end[Example]

Two policy identifiers are predefined, namely, @exam[Concurrent] and
@exam[Sequential]. The pragma is a configuration pragma and so applies
throughout a partition. The default policy is @exam[Concurrent].

The normal behaviour in Ada when a program starts is that a task declared
at library level is activated by the environment task and can begin
to execute before all library level elaboration is completed and before
the main subprogram is called by the environment task. Race conditions
can arise especially when several library tasks are involved. Problems
also arise with the attachment of interrupt handlers.

@leading@;If the policy @exam[Sequential] is specified then the rules are
changed. The following things happen in sequence
@begin[Itemize]
The elaboration of all library units takes place (this
is done by the environment task) but library tasks are not activated
(we say their activation is deferred). Similarly the attachment of
interrupt handlers is deferred.

The environment task then attaches the interrupts.

The library tasks are then activated. While this is
happening the environment task is suspended.

Finally, the environment task then executes the main
subprogram in parallel with the executing tasks.
@end[Itemize]

Note that from the library tasks' point of view they go seamlessly
from activation to execution. Moreover, they are assured that all
library units will have been elaborated and all handlers attached
before they execute.

@leading@keepnext@;If @exam[Sequential] is specified then
@begin[Example]
@key[pragma] Restrictions(No_Task_Hierarchy);
@end[Example]

must also be specified. This ensures that all tasks are at library level.

A final small point is that the Restrictions identifiers
@exam[No_Unchecked_Conversion] and @exam[No_Unchecked_Deallocation] are now
banished to @URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-J-13.html],Text=[Annex J]}
because @exam[No_Dependence] can be used instead.


