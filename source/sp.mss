@comment{ $Source: e:\\cvsroot/ARM/Source/sp.mss,v $ }
@comment{ $Revision: 1.80 $ $Date: 2015/04/03 04:12:43 $ $Author: randy $ }
@Part(sysprog, Root="ada.mss")
@Comment{$Date: 2015/04/03 04:12:43 $}

@LabeledNormativeAnnex{Systems Programming}

@begin{Intro}
@Redundant[@Defn{systems programming}
@Defn{low-level programming}
@Defn{real-time systems}
@Defn{embedded systems}
@Defn{distributed systems}
@Defn{information systems}
The Systems Programming Annex specifies additional capabilities
provided for low-level programming. These capabilities are also
required in many real-time, embedded, distributed, and information systems.]
@end{Intro}

@begin{Extend83}
@Defn{extensions to Ada 83}
This Annex is new to Ada 95.
@end{Extend83}

@LabeledClause{Access to Machine Operations}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Redundant[This @Chg{Version=[3],New=[subclause],Old=[clause]} specifies rules regarding access to machine instructions
from within an Ada program.]
@ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],
Text=[@Chg{Version=[2],
New=[Implementation-defined intrinsic subprograms],
Old=[Support for access to machine instructions]}.]}
@end{Intro}

@begin{ImplReq}
@Defn{machine code insertion}
The implementation shall support machine code insertions
(see @RefSecNum{Machine Code Insertions}) or intrinsic subprograms
(see @RefSecNum{Conformance Rules}) (or both).
Implementation-defined attributes shall be provided to
allow the use of Ada entities as operands.
@end{ImplReq}

@begin{ImplAdvice}
The machine code or intrinsics support should allow access to all
operations normally available to assembly language programmers for the
target environment,
including privileged instructions, if any.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[The machine code or intrinsics support should allow access to all
operations normally available to assembly language programmers for the
target environment.]}]}
@begin{Ramification}
Of course, on a machine with protection, an attempt to execute a
privileged instruction in user mode will probably trap.
Nonetheless, we want implementations to provide access to them so that
Ada can be used to write systems programs that run in privileged mode.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Defn{interface to assembly language}
@Defn2{Term=[language], Sec=(interface to assembly)}
@Defn{mixed-language programs}
@Defn{assembly language}
The @Chg{Version=[3],New=[support for ],Old=[]}interfacing @Chg{Version=[3],New=[aspects],Old=[pragmas]}
(see @RefSecNum{Interface to Other Languages})
should @Chg{Version=[3],New=[include],Old=[support]} interface to assembler;
the default assembler should be associated with the convention
identifier Assembler.
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Interface to assembler should be supported; the default assembler
should be associated with the convention identifier Assembler.]}]}

If an entity is exported to assembly language, then the implementation
should allocate it at an addressable location,
and should ensure that it is retained by the linking process,
even if not otherwise referenced from the Ada code.
The implementation should assume that any call to a machine code or
assembler subprogram is allowed to read or update every object that is
specified as exported.
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[If an entity is exported to assembly language, then the implementation
should allocate it at an addressable location even if not otherwise referenced
from the Ada code. A call to a machine code or assembler subprogram should
be treated as if it could read or update every object that is
specified as exported.]}]}
@end{ImplAdvice}

@begin{DocReq}

The implementation shall document the overhead associated with calling
machine-code or intrinsic subprograms, as compared to a fully-inlined
call, and to a regular out-of-line call.
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The overhead of calling machine-code or intrinsic subprograms.]}]}

The implementation shall document the types of
the package System.Machine_Code usable for machine code
insertions, and the attributes to be used in
machine code insertions for references to Ada entities.
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The types and attributes used in machine code insertions.]}]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
The implementation shall document the subprogram calling conventions
associated with the convention identifiers available for use
with the @Chg{Version=[3],New=[Convention aspect],Old=[interfacing pragmas]}
(Ada and Assembler, at a minimum),
including register saving,
exception propagation, parameter passing, and function value returning.
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The subprogram calling conventions for all supported convention
identifiers.]}]}

For exported and imported subprograms,
the implementation shall document the mapping
between the Link_Name string, if specified, or
the Ada designator, if not, and the external link name used
for such a subprogram.
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Implementation-defined aspects of access to machine operations.]}]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The mapping between the Link_Name or Ada designator and the external
link name.]}]}

@end{DocReq}

@begin{ImplAdvice}

The implementation should ensure that little or no overhead is associated
with calling intrinsic and machine-code subprograms.
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Little or no overhead should be associated
with calling intrinsic and machine-code subprograms.]}]}

@Leading@;It is recommended that intrinsic subprograms be provided for convenient
access to any machine operations that provide special capabilities
or efficiency and that are not otherwise available through the language
constructs. Examples of such instructions
include:
@begin{itemize}

Atomic read-modify-write operations @em e.g., test and set, compare and swap,
decrement and test, enqueue/dequeue.

Standard numeric functions @em e.g., @i{sin}, @i{log}.

String manipulation operations @em e.g., translate and test.

Vector operations @em e.g., compare vector against thresholds.

Direct operations on I/O ports.

@end{itemize}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Intrinsic subprograms should be provided to access any machine operations
that provide special capabilities or efficiency not normally available.]}]}

@end{ImplAdvice}

@LabeledClause{Required Representation Support}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
This @Chg{Version=[3],New=[subclause],Old=[clause]} specifies minimal requirements on the
@Chg{Version=[2],New=[],Old=[implementation's ]}support for representation
items and related features.
@end{Intro}

@begin{ImplReq}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@PDefn2{Term=[recommended level of support], Sec=(required in Systems
Programming Annex)}
The implementation shall support at least the functionality
defined by the recommended levels of support in @Chg{Version=[3],New=[Clause],
Old=[Section]} @RefSecNum{Representation Issues}.
@end{ImplReq}

@LabeledClause{Interrupt Support}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Redundant[This @Chg{Version=[3],New=[subclause],Old=[clause]} specifies
the language-defined model for hardware interrupts
in addition to mechanisms for handling interrupts.]
@IndexSee{Term=[signal],See=(interrupt)}
@end{Intro}

@begin{RunTime}

@Defn{interrupt}
@Redundant[An @i{interrupt} represents a class of events that are detected by
the hardware or the system software.]
@Defn2{Term=[occurrence], Sec=(of an interrupt)}
Interrupts are said to occur. An @i{occurrence} of an interrupt is
separable into generation and delivery.
@Defn2{Term=[generation], Sec=(of an interrupt)}
@i{Generation} of an interrupt is the event in the underlying hardware
or system that makes the interrupt available to the program.
@Defn2{Term=[delivery], Sec=(of an interrupt)}
@i{Delivery} is the action that invokes part of the program as
response to the interrupt occurrence.
@Defn{pending interrupt occurrence}
Between generation and delivery, the interrupt occurrence @Redundant[(or
interrupt)] is @i{pending}.
@Defn{blocked interrupt}
Some or all interrupts may be @i{blocked}. When an interrupt is blocked, all
occurrences of that interrupt are prevented from being delivered.
@Defn2{Term=[attaching], Sec=(to an interrupt)}
@Defn{reserved interrupt}
Certain interrupts are @i{reserved}. The set of reserved interrupts is
implementation defined. A reserved interrupt is either an interrupt for
which user-defined handlers are not supported, or one which
already has an attached handler by some other implementation-defined means.
@Defn{interrupt handler}@PDefn2{Term=[handler],Sec=[interrupt]}
Program units can be connected to nonreserved interrupts. While
connected, the program unit is said to be @i{attached} to that interrupt.
The execution of that program unit, the @i{interrupt handler}, is invoked upon
delivery of the interrupt occurrence.
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Implementation-defined aspects of interrupts.]}]}
@begin{Honest}
  As an obsolescent feature,
  interrupts may be attached to task entries by an address clause.
  See @RefSecNum{Interrupt Entries}.
@end{Honest}

While a handler is attached to an interrupt, it is called once for each
delivered occurrence of that interrupt. While the handler executes, the
corresponding interrupt is blocked.

While an interrupt is blocked, all occurrences of that interrupt are prevented
from being delivered. Whether such occurrences remain pending or are lost is
implementation defined.

@Defn{default treatment}
Each interrupt has a @i{default treatment} which determines the system's
response to an occurrence of that interrupt when no user-defined
handler is attached. The set of possible default treatments is
implementation defined, as is the method (if one exists) for configuring
the default treatments for interrupts.

An interrupt is delivered to the handler (or default treatment) that is in
effect for that interrupt at the time of delivery.

An exception propagated from a handler that is invoked by an
interrupt has no effect.

@Redundant[If the Ceiling_Locking policy (see @RefSecNum{Priority Ceiling Locking}) is
in effect, the interrupt handler executes with the active priority that is the
ceiling priority of the corresponding protected object.]

@end{RunTime}

@begin{ImplReq}

The implementation shall provide a mechanism to determine the minimum
stack space that is needed
for each interrupt handler and to reserve that space for
the execution of the handler. @Redundant{This space should accommodate
nested invocations of the handler where the system permits this.}

If the hardware or the underlying system holds pending interrupt
occurrences, the implementation shall provide for later delivery
of these occurrences to the program.

If the Ceiling_Locking policy is not in effect, the implementation
shall provide means for the application to specify whether interrupts
are to be blocked during protected actions.

@end{ImplReq}

@begin{DocReq}
@Leading@;The implementation shall document the following items:
@begin{Discussion}
This information may be different for different forms of interrupt handlers.
@end{Discussion}
@begin{Enumerate}
For each interrupt, which interrupts are blocked from delivery when a handler
attached to that interrupt executes (either as a result of an interrupt
delivery or of an ordinary call on a procedure of the corresponding
protected object).

Any interrupts that cannot be blocked, and the effect of attaching
handlers to such interrupts, if this is permitted.

Which run-time stack an interrupt handler uses when it executes as a
result of an interrupt delivery; if this is configurable, what is the
mechanism to do so; how to specify how much space to reserve on that stack.

Any implementation- or hardware-specific activity that happens
before a user-defined interrupt handler gets control (e.g.,
reading device registers, acknowledging devices).

Any timing or other limitations imposed on the execution of interrupt handlers.

The state (blocked/unblocked) of the nonreserved interrupts
when the program starts; if some interrupts are unblocked, what
is the mechanism a program can use to protect itself before it
can attach the corresponding handlers.

Whether the interrupted task is allowed to resume execution before the
interrupt handler returns.

The treatment of interrupt occurrences that are generated while
the interrupt is blocked; i.e., whether one or more occurrences
are held for later delivery, or all are lost.

Whether predefined or implementation-defined exceptions are raised as a
result of the occurrence of any interrupt, and the mapping between the
machine interrupts (or traps) and the predefined
exceptions.

On a multi-processor, the rules governing the delivery of an interrupt
to a particular processor.
@end{Enumerate}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The treatment of interrupts.]}]}
@ChgNote{A Bob Duff explanation, but this is just too much junk.}
@end{DocReq}

@begin{ImplPerm}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
If the underlying system or hardware does not allow interrupts to be
blocked, then no blocking is required @Redundant[as part of the execution of
subprograms of a protected object @Chg{Version=[2],New=[for which],Old=[whose]}
one of its subprograms is an interrupt handler].

In a multi-processor with more than one interrupt subsystem, it is
implementation defined whether (and how) interrupt sources from
separate subsystems share the same Interrupt_Id type
(see @RefSecNum{The Package Interrupts}).
In particular, the meaning of a blocked or pending interrupt may then be
applicable to one processor only.
@begin{discussion}
This issue is tightly related to the issue of scheduling on a
multi-processor. In a sense, if a particular interrupt source is not
available to all processors, the system is not truly homogeneous.

One way to approach this problem is to assign sub-ranges within
Interrupt_Id to each interrupt subsystem, such that @lquotes@;similar@rquotes@; interrupt
sources (e.g. a timer) in different subsystems get a distinct id.
@end{Discussion}

Implementations are allowed to impose timing or other limitations on the
execution of interrupt handlers.
@begin{Reason}
These limitations are often necessary to ensure proper behavior of the
implementation.
@end{Reason}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
Other forms of handlers are allowed to be supported, in which
case@Chg{Version=[2],New=[],Old=[,]} the rules of this
@Chg{Version=[3],New=[subclause],Old=[@Chg{Version=[2],New=[clause],Old=[subclause]}]}
should be adhered to.

The active priority of the execution of an interrupt handler is allowed to
vary from one occurrence of the same interrupt to another.

@end{ImplPerm}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
If the Ceiling_Locking policy is not in effect, the implementation
should provide means for the application to specify which interrupts
are to be blocked during protected actions, if the underlying system allows
for @Chg{Version=[2],New=[finer-grained],Old=[a finer-grain]} control of
interrupt blocking.
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[If the Ceiling_Locking policy is not in effect and the target system
allows for finer-grained control of interrupt blocking, a means for the
application to specify which interrupts are to be blocked during protected
actions should be provided.]}]}

@end{ImplAdvice}

@begin{Notes}

The default treatment for an interrupt can be to keep the
interrupt pending or to deliver it to an implementation-defined
handler. Examples of actions that an implementation-defined
handler is allowed to perform include aborting the partition, ignoring
(i.e., discarding occurrences of) the interrupt, or queuing one
or more occurrences of the interrupt for possible later delivery
when a user-defined handler is attached to that interrupt.

It is a bounded error to call Task_Identification.Current_Task
(see @RefSecNum{The Package Task_Identification}) from an interrupt handler.

The rule that an exception propagated from an interrupt handler has no effect
is modeled after the rule about exceptions propagated out of task bodies.

@end{Notes}

@LabeledSubClause{Protected Procedure Handlers}

@begin{NotIso}
@ChgAdded{Version=[3],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 1
through 6 were moved to @RefSec{Obsolescent Features}.>}]}@Comment{This message
should be deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@begin{Syntax}
@begin{SyntaxText}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Type=[Leading],KeepNext=[T],Text=[The form of a
@nt{pragma} Interrupt_Handler is as follows:]}
@end{SyntaxText}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@DeletedPragmaSyn<Version=[3],InitialVersion=[0],@ChgDeleted{Version=[3],
Text=[@key{pragma} @prag(Interrupt_Handler)(@SynI{handler_}@Syn2{name});]}>

@begin{SyntaxText}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Type=[Leading],KeepNext=[T],Text=[The form of a
@nt{pragma} Attach_Handler is as follows:]}
@end{SyntaxText}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@DeletedPragmaSyn<Version=[3],InitialVersion=[0],@ChgDeleted{Version=[3],
Text=[@key{pragma} @prag(Attach_Handler)(@SynI{handler_}@Syn2{name}, @Syn2{expression});]}>
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[For the Interrupt_Handler and Attach_Handler
pragmas, the @SynI{handler_}@nt{name} shall resolve to denote a protected
procedure with a parameterless profile.]}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[For the Attach_Handler pragma, the expected type
for the @nt{expression} is Interrupts.Interrupt_Id
(see @RefSecNum{The Package Interrupts}).]}
@end{Resolution}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For a parameterless protected
procedure, the following language-defined representation aspects may be
specified:]}
@begin{Description}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Interrupt_Handler@\
The type of aspect Interrupt_Handler is Boolean.
If directly specified, the aspect_definition shall be a static expression.
@Redundant[This aspect is never inherited;] if not directly specified,
the aspect is False.@AspectDefn{Interrupt_Handler}]}

@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Interrupt_Handler],
  Text=[@ChgAdded{Version=[3],Text=[Protected procedure may be attached to
    interrupts.]}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Attach_Handler@\
The aspect Attach_Handler is an @nt{expression}, which shall be of type
Interrupts.Interrupt_Id. @Redundant[This aspect is never
inherited.]@AspectDefn{Attach_Handler}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Attach_Handler],
    Text=[@ChgAdded{Version=[3],Text=[Protected procedure is attached to an
      interrupt.]}]}

@end{Description}

@end{StaticSem}

@begin{Legality}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0033-1],ARef=[AI05-0229-1]}
@Chg{Version=[3],New=[If either the],Old=[The]} Attach_Handler
@Chg{Version=[3],New=[or Interrupt_Handler aspect are specified for a
protected procedure, the],Old=[pragma is only allowed immediately within the
@nt{protected_definition} where the corresponding subprogram is declared.
The]} corresponding @nt{protected_@!type_@!declaration}
or @nt{single_@!protected_@!declaration}
shall be a library@Chg{Version=[2],New=[-],Old=[]}level
declaration@Chg{Version=[3],New=[ and shall not be declared within a
generic body. @PDefn{generic contract issue}In addition
to the places where @LegalityTitle normally apply
(see @RefSecNum{Generic Instantiation}), this rule also applies
in the private part of an instance of a generic unit],Old=[]}.

@begin{Discussion}
In the case of a @nt{protected_type_declaration},
an @nt{object_declaration} of an object of that type
need not be at library level.

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0033-1],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[We cannot allow these aspects in protected
declarations in a generic body, because legality rules are not checked for
instance bodies, and these should not be allowed if the instance is not at the
library level. The protected types can be declared in the private part if this
is desired. Note that while the 'Access to use the handler would provide the
check in the case of Interrupt_Handler, there is no other check for
Attach_Handler. Since these aspects are so similar, we want the rules to be the
same.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00253-01],ARef=[AI95-00303-01]}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0033-1]}
@ChgDeleted{Version=[3],Text=[The Interrupt_Handler pragma is only
allowed immediately within @Chg{Version=[2],New=[the],Old=[a]}
@nt{protected_definition}@Chg{Version=[2],New=[ where the
corresponding subprogram is declared],Old=[]}.
The cor@!responding @nt{protected_@!type_declaration} @Chg{Version=[2],New=[or
@nt{single_@!protected_@!declaration} ],Old=[]}shall
be a library@Chg{Version=[2],New=[-],Old=[]}level
declaration.@Chg{Version=[2],New=[],Old=[ In addition, any
@nt{object_@!declaration} of such a type shall be a library level declaration.]}]}
@end{Legality}

@begin{RunTime}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
If the @Chg{Version=[3],New=[],Old=[pragma ]}Interrupt_Handler
@Chg{Version=[3],New=[aspect of a protected procedure is True],Old=[appears in a
@nt{protected_definition}]}, then the
@Chg{Version=[3],New=[],Old=[corresponding ]}procedure
@Chg{Version=[3],New=[may],Old=[can]}
be attached dynamically, as a handler, to
interrupts (see @RefSecNum{The Package Interrupts}).
@Redundant[Such procedures are allowed to be attached to multiple interrupts.]

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Defn2{Term=[creation], Sec=(of a protected object)}
@Defn2{Term=[initialization], Sec=(of a protected object)}
The @nt{expression} @Chg{Version=[3],New=[specified for],Old=[in]} the
Attach_Handler @Chg{Version=[3],New=[aspect of a protected procedure @i<P>
is evaluated as part of the creation of the protected object that
contains @i<P>.
The value of the @nt{expression} identifies],Old=[pragma @Redundant[as evaluated at
object creation time] specifies]} an interrupt. As part
of the initialization of that object,
@Chg{Version=[3],New=[@i<P> (],Old=[if the Attach_Handler pragma is
specified, ]}the @SynI{handler}
procedure@Chg{Version=[3],New=[)],Old=[]}
is attached to the @Chg{Version=[3],New=[identified],Old=[specified]} interrupt.
@IndexCheck{Reserved_Check}
A check is made that the corresponding interrupt is not reserved.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
Program_Error is raised if the check fails, and the existing treatment
for the interrupt is not affected.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Defn2{Term=[initialization], Sec=(of a protected object)}
@IndexCheck{Ceiling_Check}
If the Ceiling_Locking policy (see @RefSecNum{Priority Ceiling Locking}) is
in effect@Chg{Version=[2],New=[,],Old=[]} then upon the initialization of a
protected object @Chg{Version=[3],New=[that contains a protected
procedure ],Old=[]}@Chg{Version=[2],New=[for which],Old=[that]} either
@Chg{Version=[3],New=[the],Old=[an]}
Attach_Handler@Chg{Version=[3],New=[ aspect is specified],Old=[]} or
@Chg{Version=[3],New=[the ],Old=[]} Interrupt_Handler
@Chg{Version=[3],New=[aspect is True],Old=[pragma applies to one of its
procedures]},
a check is made that the @Chg{Version=[3],New=[initial ],Old=[]}ceiling
priority @Chg{Version=[3],New=[of the object],Old=[defined in the
@nt{protected_definition}]} is in the range of System.@!Interrupt_Priority.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
If the check fails, Program_Error is raised.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0068],ARef=[AI95-00121-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Defn2{Term=[finalization], Sec=(of a protected object)}
When a protected object is finalized, for any of its procedures that are
attached to interrupts, the handler is detached. If the handler was
attached by a procedure in the Interrupts package or if no user
handler was previously attached to the interrupt, the default treatment is
restored. @Chg{New=[If @Chg{Version=[3],New=[the],Old=[an]} Attach_@!Handler
@Chg{Version=[3],New=[aspect],Old=[pragma]} was
@Chg{Version=[3],New=[specified],Old=[used]} and the most recently
attached handler for the same interrupt is the same as the one that was
attached at the time the protected object was initialized],
Old=[Otherwise, @Redundant[that is, if an
Attach_@!Handler pragma was specified]]}, the previous handler is
restored.
@begin{Discussion}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0068],ARef=[AI95-00121-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00303-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Chg{New=[If all protected objects for interrupt handlers are declared at the
library@Chg{Version=[2],New=[ ],Old=[-]}level],Old=[Since only library-level
protected procedures can be attached as
handlers using the Interrupts package]}, the finalization discussed above
occurs only as part of the finalization of all library-level packages in
a partition.
@Chg{New=[However, objects of a protected type containing
@Chg{Version=[3],New=[procedures with ],Old=[]}an Attach_@!Handler
@Chg{Version=[3],New=[aspect specified],Old=[pragma]} need not be at the library level.
Thus, an implementation needs to be
able to restore handlers during the execution of the program.@Chg{Version=[2],
New=[ (An object with an Interrupt_@!Handler @Chg{Version=[3],New=[aspect],Old=[pragma]}
also need not be at the library level, but such
a handler cannot be attached to an interrupt using the Interrupts package.)],
Old=[]}],Old=[]}
@end{Discussion}

When a handler is attached to an interrupt, the interrupt is blocked
@Redundant[(subject to the @ImplPermName in @RefSecNum{Interrupt Support})]
during the execution of every protected action on the protected object
containing the handler.

@end{RunTime}

@begin{Erron}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
If the Ceiling_Locking policy (see @RefSecNum{Priority Ceiling Locking}) is
in effect and an interrupt is delivered to a handler, and the interrupt
hardware priority is higher than the ceiling priority of the corresponding
protected object, the execution of the program is erroneous.

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0068],ARef=[AI95-00121-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0229-1]}
@ChgAdded{Version=[1],Text=[@PDefn2{Term=(erroneous execution),Sec=(cause)}
If the handlers for a given interrupt attached via
@Chg{Version=[3],New=[aspect],Old=[pragma]} Attach_Handler
are not attached and detached in a stack-like (LIFO) order,
program execution is erroneous. In particular, when a protected object is
finalized, the execution is erroneous if any of the procedures of the
protected object are attached to interrupts via
@Chg{Version=[3],New=[aspect],Old=[pragma]} Attach_@!Handler and
the most recently attached handler for the same interrupt is not the same as
the one that was attached at the time the protected object was initialized.]}
@begin{Discussion}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0068],ARef=[AI95-00121-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@ChgAdded{Version=[1],Text=[This simplifies implementation of the Attach_@!Handler
@Chg{Version=[3],New=[aspect],Old=[pragma]} by not requiring a check that the
current handler is the same as the one attached by the initialization of a
protected object.]}
@end{Discussion}
@end{Erron}

@begin{Metrics}
@Leading@Keepnext@;The following metric shall be documented by the implementation:
@ChgNote{This was @begin{enumerate}, which is wrong}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
The worst@Chg{Version=[2],New=[-],Old=[ ]}case overhead for an interrupt
handler that is a parameterless
protected procedure, in clock cycles. This is the execution time not
directly attributable to the handler procedure or the interrupted execution.
It is estimated as C @en@; (A+B), where A is how long it takes to complete a given
sequence of instructions without any interrupt, B is how long it takes to
complete a normal call to a given protected procedure, and C is how long it
takes to complete the same sequence of instructions when it is interrupted by
one execution of the same procedure called via an interrupt.
@begin{ImplNote}
The instruction sequence and interrupt handler used to measure interrupt
handling overhead should be chosen so as to maximize the execution time cost
due to cache misses. For example, if the processor has cache memory and the
activity of an interrupt handler could invalidate the contents of cache memory, the handler should be written such that it invalidates all of the cache memory.
@end{ImplNote}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The metrics for interrupt handlers.]}]}
@ChgNote{This was @end{enumerate}, which is wrong}
@end{Itemize}
@end{Metrics}

@begin{ImplPerm}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
When the @Chg{Version=[3],New=[aspects],Old=[pragmas]} Attach_Handler or
Interrupt_Handler @Chg{Version=[3],New=[are specified for],Old=[apply to]}
a protected procedure, the implementation is allowed to impose
implementation-defined restrictions on the
corresponding @nt{protected_@!type_@!declaration} and @nt{protected_@!body}.
@begin{Ramification}
The restrictions may be on the constructs that are allowed within them,
and on ordinary calls (i.e. not via interrupts) on protected operations in
these protected objects.
@end{Ramification}
@ChgImplDef{Version=[3],Kind=[Revised],InitialVersion=[2],
Text=[@Chg{Version=[2],
New=[Any restrictions on a protected procedure or its containing type when
@Chg{Version=[3],New=[an aspect],Old=[a @nt{pragma}]}
Attach_handler or Interrupt_Handler
@Chg{Version=[3],New=[is specified],Old=[applies]}.],Old=[]}]}

An implementation may use a different mechanism for invoking a protected
procedure in response to a hardware interrupt than is used for a call
to that protected procedure from a task.
@begin{Discussion}
This is despite the fact that the priority of an interrupt handler
(see @RefSecNum{Task Priorities}) is modeled after a hardware task calling the
handler.
@end{Discussion}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Defn{notwithstanding}
Notwithstanding what this subclause says elsewhere,
the Attach_Handler and Interrupt_Handler
@Chg{Version=[3],New=[aspects],Old=[pragmas]} are allowed to be used
for other, implementation defined, forms of interrupt handlers.
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
For example, if an implementation wishes to allow interrupt
handlers to have parameters, it is allowed to do so via these
@Chg{Version=[3],New=[aspects],Old=[pragmas]};
it need not invent implementation-defined
@Chg{Version=[3],New=[aspects],Old=[pragmas]} for the purpose.
@end{Ramification}
@ChgImplDef{Version=[3],Kind=[Revised],InitialVersion=[2],
Text=[@Chg{Version=[2],
New=[Any other forms of interrupt handler supported by the
Attach_Handler and Interrupt_Handler
@Chg{Version=[3],New=[aspects],Old=[pragmas]}.],Old=[]}]}

@end{ImplPerm}

@begin{ImplAdvice}

Whenever possible, the implementation should allow interrupt handlers to be
called directly by the hardware.
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Interrupt handlers should be called directly by the hardware.]}]}

Whenever practical, the implementation should
detect violations of any implementation-defined restrictions
before run time.
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Violations of any implementation-defined restrictions on interrupt
handlers should be detected before run time.]}]}

@end{ImplAdvice}

@begin{Notes}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
The Attach_Handler @Chg{Version=[3],New=[aspect may],Old=[pragma can]}
provide static attachment of handlers to
interrupts if the implementation supports preelaboration of protected
objects. (See @RefSecNum{Preelaboration Requirements}.)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
@Chg{Version=[2],New=[A],Old=[The ceiling priority of a]}
protected object that
@Chg{Version=[2],New=[has a (protected) procedure],
Old=[one of its procedures is]}
attached to an interrupt should
@Chg{Version=[2],New=[have a ceiling priority],
Old=[be]} at least as high as the highest
processor priority at which that interrupt will ever be delivered.

Protected procedures can also be attached dynamically to interrupts
via operations declared in the predefined package Interrupts.

An example of a possible implementation-defined restriction is
disallowing the use of the standard storage pools within the body of
a protected procedure that is an interrupt handler.

@end{Notes}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00253-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @b[Amendment Correction:] Corrected the wording so that the rules for the
  use of Attach_Handler and Interrupt_Handler are identical. This means
  that uses of pragma Interrupt_Handler outside of the target protected type
  or single protected object are now illegal.]}
@end{Incompatible95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0068],ARef=[AI95-00121-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified the meaning of
  @lquotes@;the previous handler@rquotes when finalizing protected objects
  containing interrupt handlers.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00303-01]}
  @ChgAdded{Version=[2],Text=[Dropped the requirement that an object of a
  type containing an Interrupt_Handler pragma must be declared at the library
  level. This was a generic contract model violation. This change is not
  an extension, as an attempt to attach such a handler with a routine in
  package Interrupts will fail an accessibility check anyway. Moreover,
  implementations can retain the rule as an implementation-defined
  restriction on the use of the type, as permitted by the @ImplPermTitle
  above.]}
@end{Diffword95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Aspects Interrupt_Handler and Attach_Handler are new; @nt{pragma}s
  Interrupt_Handler and Attach_Handler are now obsolescent.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0033-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Added missing generic contract wording for the aspects Attach_Handler and
  Interrupt_Handler.]}
@end{DiffWord2005}



@LabeledSubClause{The Package Interrupts}

@begin{StaticSem}

@Leading@Keepnext@;The following language-defined packages exist:
@begin{example}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0167-1]}
@key{with} System;@ChildUnit{Parent=[Ada],Child=[Interrupts]}@Chg{Version=[3],New=[
@key{with} System.Multiprocessors;],Old=[]}
@key[package] Ada.Interrupts @key[is]
   @key[type] @AdaTypeDefn{Interrupt_Id} @key[is] @RI{implementation-defined};
   @key[type] @AdaTypeDefn{Parameterless_Handler} @key[is]
      @key[access] @key[protected] @key[procedure];

@ChgRef{Version=[1], Kind=[Deleted]}
@Chg[New=<>,Old=<@ @;@comment{Empty paragraph to hang junk paragraph number from original RM}>]

   @key[function] @AdaSubDefn{Is_Reserved} (Interrupt : Interrupt_Id)
      @key[return] Boolean;

   @key[function] @AdaSubDefn{Is_Attached} (Interrupt : Interrupt_Id)
      @key[return] Boolean;

   @key[function] @AdaSubDefn{Current_Handler} (Interrupt : Interrupt_Id)
      @key[return] Parameterless_Handler;

   @key[procedure] @AdaSubDefn{Attach_Handler}
      (New_Handler : @key[in] Parameterless_Handler;
       Interrupt   : @key[in] Interrupt_Id);

   @key[procedure] @AdaSubDefn{Exchange_Handler}
      (Old_Handler : @key[out] Parameterless_Handler;
       New_Handler : @key[in] Parameterless_Handler;
       Interrupt   : @key[in] Interrupt_Id);

   @key[procedure] @AdaSubDefn{Detach_Handler}
      (Interrupt : @key[in] Interrupt_Id);

   @key[function] @AdaSubDefn{Reference} (Interrupt : Interrupt_Id)
      @key{return} System.Address;

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0167-1]}
@ChgAdded{Version=[3],Text=[   @key[function] @AdaSubDefn{Get_CPU} (Interrupt : Interrupt_Id)
      @key{return} System.Multiprocessors.CPU_Range;]}

@key[private]
   ... -- @RI{not specified by the language}
@key[end] Ada.Interrupts;


@key[package] Ada.Interrupts.Names @key[is]@ChildUnit{Parent=[Ada.Interrupts],Child=[Names]}
   @RI{implementation-defined} : @key[constant] Interrupt_Id :=
     @RI{implementation-defined};
      . . .
   @RI{implementation-defined} : @key[constant] Interrupt_Id :=
     @RI{implementation-defined};
@key[end] Ada.Interrupts.Names;
@end{example}

@end{StaticSem}

@begin{RunTime}

The Interrupt_Id type is an implementation-defined discrete type used to
identify interrupts.

The Is_Reserved function returns True if and only if the specified
interrupt is reserved.

The Is_Attached function returns True if and only if a user-specified
interrupt handler is attached to the interrupt.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0069],ARef=[AI95-00166-01]}
The Current_Handler function returns a value that represents the
attached handler of the interrupt. If no user-defined handler is attached to
the interrupt, Current_Handler returns @Chg{New=[@key{null}],
Old=[a value that designates the default treatment; calling Attach_Handler
or Exchange_Handler with this value restores the default treatment]}.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
The Attach_Handler procedure attaches the specified handler to the
interrupt, overriding any existing treatment (including a user handler)
in effect for that interrupt.
If New_Handler is @key[null], the default treatment is restored.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
If New_Handler designates a protected procedure @Chg{Version=[3],New=[for],
Old=[to]} which the @Chg{Version=[3],New=[aspect],
Old=[pragma]}
Interrupt_@!Handler @Chg{Version=[3],New=[is False],Old=[does not apply]},
Program_Error is raised. In
this case, the operation does not modify the existing interrupt treatment.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0069],ARef=[AI95-00166-01]}
The Exchange_Handler procedure operates in the same manner as Attach_Handler
with the addition that the value returned in Old_Handler designates the
previous treatment for the specified interrupt.@Chg{New=[ If the previous
treatment is not a user-defined handler, @key[null] is returned.],Old=[]}
@begin{Ramification}
Calling Attach_Handler or Exchange_Handler with this value for New_Handler
restores the previous handler.

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0069],ARef=[AI95-00166-01]}
@ChgAdded{Version=[1],Text=[If the application uses only parameterless procedures as
handlers (other types of handlers may be provided by the implementation, but are not
required by the standard), then if Old_Handler is not @key(null), it may be
called to execute the previous handler. This provides a way to cascade
application interrupt handlers. However, the default handler cannot be
cascaded this way (Old_Handler must be @key(null) for the default handler).]}
@end{Ramification}

The Detach_Handler procedure restores the default treatment for the
specified interrupt.

For all operations defined in this package that take a parameter of type
Interrupt_Id, with the exception of Is_Reserved and Reference, a check is
made that the specified interrupt is not reserved.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
Program_Error is raised if this check fails.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
If, by using the Attach_Handler, Detach_Handler, or Exchange_Handler
procedures, an attempt is made to
detach a handler that was attached statically (using the
@Chg{Version=[3],New=[aspect],Old=[pragma]}
Attach_Handler), the handler is not detached and Program_Error is
raised.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
The Reference function returns a value of type System.Address that can
be used to attach a task entry@Chg{Version=[2],New=[],Old=[,]} via an
address clause (see @RefSecNum{Interrupt Entries}) to the interrupt
specified by Interrupt. This function raises Program_Error if attaching
task entries to interrupts (or to this particular interrupt) is not supported.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0153-3]}
@ChgAdded{Version=[3],Text=[The function Get_CPU returns the processor on which
the handler for Interrupt is executed. If the handler can execute on more than
one processor the value System.Multiprocessors.Not_A_Specific_CPU is returned.]}

@end{RunTime}

@begin{ImplReq}

At no time during attachment or exchange of handlers shall the current handler
of the corresponding interrupt be undefined.

@end{ImplReq}

@begin{DocReq}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
If the Ceiling_Locking policy (see @RefSecNum{Priority Ceiling Locking}) is
in effect@Chg{Version=[2],New=[,],Old=[]} the implementation shall document
the default ceiling priority
assigned to a protected object that contains
@Chg{Version=[3],New=[a protected procedure that specifies ],Old=[]}either
the Attach_Handler or
Interrupt_Handler @Chg{Version=[3],New=[aspects],Old=[pragmas]},
but @Chg{Version=[3],New=[does not specify],Old=[not]} the
Interrupt_Priority @Chg{Version=[3],New=[aspect],Old=[pragma]}.
@Redundant[This default need not be the same for all interrupts.]
@ChgDocReq{Version=[3],Kind=[RevisedAdded],InitialVersion=[2],
Text=[@ChgAdded{Version=[2],
Text=[If the Ceiling_Locking policy is in effect, the default ceiling priority
for a protected object that @Chg{Version=[3],New=[specifies],Old=[contains]}
an interrupt handler @Chg{Version=[3],New=[aspect],Old=[pragma]}.]}]}

@end{DocReq}

@begin{ImplAdvice}

If implementation-defined forms of interrupt handler procedures are supported,
such as protected procedures with parameters, then for each such form of a
handler, a type analogous to Parameterless_@!Handler should be specified in a
child package of Interrupts, with the same operations as in the
predefined package Interrupts.

@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[If implementation-defined forms of interrupt handler procedures are
supported, then for each such form of a
handler, a type analogous to Parameterless_@!Handler should be specified in a
child package of Interrupts, with the same operations as in the
predefined package Interrupts.]}]}

@end{ImplAdvice}

@begin{Notes}

The package Interrupts.Names contains implementation-defined
names (and constant values) for the interrupts that are supported by
the implementation.

@end{Notes}

@begin{Examples}
@Leading@Keepnext@i{Example of interrupt handlers:}
@begin{example}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
Device_Priority : @key[constant]
  @key[array] (1..5) of System.Interrupt_Priority := ( ... );@Softpage
@key[protected] @key[type] Device_Interface
  (Int_Id : Ada.Interrupts.Interrupt_Id) @Chg{Version=[3],New=[
     @key[with] Interrupt_Priority => Device_Priority(Int_Id) ],Old=[]}@key[is]
  @key[procedure] Handler@Chg{Version=[3],New=[
     @key[with] Attach_Handler => Int_Id],Old=[;
  @key[pragma] Attach_Handler(Handler, Int_Id)]};
  ...
  @Chg{Version=[3],New=[],Old=[@key[pragma] Interrupt_Priority(Device_Priority(Int_Id));
]}@key[end] Device_Interface;@Softpage
  ...
Device_1_Driver : Device_Interface(1);
  ...
Device_5_Driver : Device_Interface(5);
  ...
@end{example}
@end{Examples}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0069],ARef=[AI95-00166-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that the value
  returned by Current_Handler and Exchange_Handler for the default treatment
  is null.]}
@end{Diffword95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0167-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
  Function Get_CPU is added to Interrupts. If Interrupts is referenced in
  a @nt{use_clause}, and an entity @i<E> with a @nt{defining_identifier} of
  Get_CPU is defined in a package that is also referenced in a @nt{use_clause},
  the entity @i<E> may no longer be use-visible, resulting in errors. This
  should be rare and is easily fixed if it does occur.]}
@end{Incompatible2005}



@ISOOnlyRMNewPageVer{Version=[3]}@Comment{For ISO version of Ada 2012 Standard}
@LabeledClause{Preelaboration Requirements}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Redundant[This @Chg{Version=[3],New=[subclause],Old=[clause]} specifies
additional implementation and documentation
requirements for the Preelaborate pragma (see @RefSecNum{Elaboration Control}).]
@end{Intro}

@begin{ImplReq}

The implementation shall not incur any run-time overhead for the elaboration
checks of subprograms and @ntf{protected_bodies} declared in preelaborated
library units.
@Defn2{Term=[preelaborated],Sec=[implementation requirements]}

The implementation shall not execute any memory write operations after
load time for the elaboration of constant objects
declared immediately within the
declarative region of a preelaborated library package, so long as the
subtype and initial expression (or default initial expressions if
initialized by default) of the @nt<object_declaration> satisfy the
following restrictions.
@Defn{load time}
The meaning of @i{load time} is implementation defined.
@begin{Discussion}
On systems where the image of the partition is initially copied from
disk to RAM, or from ROM to RAM, prior to starting execution of the
partition,
the intention is that @lquotes@;load time@rquotes@; consist of this initial copying
step.
On other systems, load time and run time might actually be interspersed.
@end{Discussion}
@begin{itemize}
Any @nt<subtype_mark> denotes a statically constrained subtype, with
statically constrained subcomponents, if any;

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00161-01]}
@ChgAdded{Version=[2],Text=[no @nt{subtype_mark} denotes a controlled type, a
private type, a private extension, a generic formal private type, a generic
formal derived type, or a descendant of such a type;]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[For an
  implementation that uses the registration method of finalization, a
  controlled object will require some code executed to register the object
  at the appropriate point. The other types are those that @i{might} have a
  controlled component. None of these types were allowed in preelaborated
  units in Ada 95. These types are covered by the @ImplAdviceTitle, of course,
  so they should still execute as little code as possible.]}
@end{Reason}

any @nt<constraint> is a static constraint;

any @nt<allocator> is for an access-to-constant type;

any uses of predefined operators appear only within static expressions;

any @ntf<primaries> that are @nt<name>s, other than @nt<attribute_reference>s
for the Access or Address attributes, appear only within static expressions;
@begin{ramification}
This cuts out @nt<attribute_reference>s that are not static, except for
Access and Address.
@end{Ramification}

any @nt<name> that is not part of a static expression is an expanded
name or @nt<direct_name> that statically denotes some entity;
@begin{Ramification}
This cuts out @nt<function_call>s and @nt<type_conversion>s that are not
static, including calls on attribute functions like 'Image and 'Value.
@end{Ramification}

any @nt<discrete_choice> of an @nt<array_aggregate> is static;

no language-defined check associated with the elaboration of the
@nt<object_declaration> can fail.
@begin{Reason}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
The intent is that aggregates all of whose scalar subcomponents are static@Chg{Version=[2],New=[],Old=[,]}
and all of whose access subcomponents are @key(null), allocators for
access-to-constant types, or X'Access, will be supported with no run-time
code generated.
@end{Reason}
@end{itemize}

@end{ImplReq}

@begin{DocReq}

The implementation shall document any circumstances under which the
elaboration of a preelaborated package causes code to be executed at run time.
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Any circumstances when the elaboration of a preelaborated package
causes code to be executed.]}]}

The implementation shall document whether the method used for initialization
of preelaborated variables allows a partition to be restarted without
reloading.
@ChgDocReq{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Whether a partition can be restarted without reloading.]}]}
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Implementation-defined aspects of preelaboration.]}]}
@begin{discussion}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
This covers the issue of the @Chg{Version=[2],New=[run-time system],Old=[RTS]}
itself being restartable,
so that need not be a separate @DocReqName.
@end{discussion}

@end{DocReq}

@begin{ImplAdvice}

It is recommended that preelaborated packages be implemented in such a
way that there should be little or no code executed at run time for the
elaboration of entities not already covered by the @ImplReqName@;s.

@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Preelaborated packages should be implemented such that little or no
code is executed at run time for the elaboration of entities.]}]}
@end{ImplAdvice}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00161-01]}
  @ChgAdded{Version=[2],Text=[Added wording to exclude the additional kinds
  of types allowed in preelaborated units from the @ImplReqTitle.]}
@end{DiffWord95}


@LabeledRevisedClause{Version=[4],New=[Aspect Discard_Names],
Old=[Pragma Discard_Names]}

@begin{Intro}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0072-1]}
@Redundant[@Chg{Version=[4],New=[Specifying the aspect],Old=[A @nt{pragma}]}
Discard_Names @Chg{Version=[4],New=[can],Old=[may]} be used to request a
reduction in storage used for the names of
@Chg{Version=[4],New=[],Old=[certain ]}entities@Chg{Version=[4],New=[ with
runtime name text],Old=[]}.]
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0072-1]}
@ChgAdded{Version=[4],Text=[An entity with @i{runtime name text} is a
nonderived enumeration first subtype, a tagged first subtype, or an
exception.@Defn{entity with runtime name text}@Defn2{Term=[runtime name text],Sec=[entity with]}]}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0072-1]}
@ChgAdded{Version=[4],Type=[Leading],Text=[For an entity with runtime name
text, the following language-defined representation aspect may be specified:]}

@begin{Description}
@ChgRef{Version=[4],Kind=[Added]}
@ChgAdded{Version=[4],Text=[Discard_Names@\The type of aspect Discard_Names
is Boolean. If directly specified, the @nt{aspect_definition} shall be
a static expression. If not specified (including by inheritance), the
aspect is False.@AspectDefn{Discard_Names}]}

  @ChgAspectDesc{Version=[4],Kind=[Added],Aspect=[Discard_Names],
    Text=[@ChgAdded{Version=[4],Text=[Requests a reduction in storage for names associated
          with an entity.]}]}
@end{Description}
@end{StaticSem}

@begin{Syntax}
@begin{SyntaxText}
@Leading@Keepnext@;The form of a @nt{pragma} Discard_Names is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Discard_Names)[([On => ] @Syn2[local_name])];'

@begin{SyntaxText}
A @nt{pragma} Discard_Names is allowed only immediately within a
@nt{declarative_part}, immediately within a @nt{package_specification},
or as a configuration pragma.
@PDefn2{Term=[configuration pragma], Sec=(Discard_Names)}
@PDefn2{Term=[pragma, configuration], Sec=(Discard_Names)}
@end{SyntaxText}
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0072-1]}
The @nt{local_name} (if present) shall denote
@Chg{Version=[4],New=[an entity with runtime name text],Old=[a nonderived
enumeration @Redundant[first] subtype,
a tagged @Redundant[first] subtype, or an exception]}.
The pragma @Chg{Version=[4],New=[specifies that the aspect Discard_Names
for],Old=[applies to]} the type or
exception@Chg{Version=[4],New=[ has the value True],Old=[]}.
Without a @nt{local_name}, the pragma @Chg{Version=[4],New=[specifies that],
Old=[applies to]} all
@Chg{Version=[4],New=[],Old=[such ]}entities@Chg{Version=[4],New=[ with
runtime name text],Old=[]}
declared after the pragma, within the same declarative
region@Chg{Version=[4],New=[ have the value True for aspect
Discard_Names],Old=[]}.
Alternatively, the pragma can be used as a configuration pragma.
@Chg{Version=[4],New=[If the configuration pragma Discard_Names applies to a
compilation unit, all entities with runtime name text declared in the
compilation unit have the value True for the aspect Discard_Names.],Old=[If the
pragma applies to a type, then it applies also to all descendants of the type]}.

@begin{Ramification}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0072-1]}
  @ChgAdded{Version=[4],Text=[If the aspect is specified for a type, then
  it is inherited by all descendants of the type. The aspect cannot be
  specified as False on a derived type (because specifying the aspect is
  not allowed on derived enumeration types, and by rule applying to all
  aspects for other types (see @RefSecNum{Aspect Specifications})).]}
@end{Ramification}

@end{Legality}

@begin{StaticSem}
@PDefn2{Term=[representation pragma], Sec=(Discard_Names)}
@PDefn2{Term=[pragma, representation], Sec=(Discard_Names)}
If a @nt{local_name} is given, then
a @nt{pragma} Discard_Names is a representation pragma.

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgRef{Version=[4],Kind=[Deleted],ARef=[AI12-0072-1]}@ChgNote{Explicit now.}
  @ChgDeleted{Version=[4],Text=[@Chg{Version=[3],New=[Representation
  pragmas automatically specify aspects of the same name,
  so Discard_Names can be used as an @nt{aspect_mark} in an
  @nt{aspect_specification} instead of using the pragma on individual
  entities.],Old=[]}]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01],ARef=[AI95-00400-01]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0072-1]}
If the @Chg{Version=[4],New=[aspect Discard_Names is True
for],Old=[pragma applies to]} an enumeration type,
then the semantics of the @Chg{Version=[2],New=[Wide_Wide_Image],Old=[Wide_Image]}
and @Chg{Version=[2],New=[Wide_Wide_Value],Old=[Wide_Value]} attributes
are implementation defined for that type@Redundant[; the
semantics of Image@Chg{Version=[2],New=[, Wide_Image,],Old=[ and]}
Value@Chg{Version=[2],New=[, and Wide_Value],Old=[]} are still defined in
terms of @Chg{Version=[2],New=[Wide_Wide_Image],Old=[Wide_Image]}
and @Chg{Version=[2],New=[Wide_Wide_Value],Old=[Wide_Value]}].
In addition, the semantics of Text_IO.Enumeration_IO are implementation
defined.
If the @Chg{Version=[4],New=[aspect Discard_Names is True
for],Old=[pragma applies to]} a tagged type,
then the semantics of the Tags.@!@Chg{Version=[2],New=[Wide_Wide_@!Expanded_@!Name],
Old=[Expanded_@!Name]} function
are implementation defined for that type@Chg{Version=[2],New=[@Redundant[; the
semantics of Tags.@!Expanded_@!Name and Tags.@!Wide_@!Expanded_@!Name are still
defined in terms of Tags.@!Wide_Wide_@!Expanded_@!Name]], Old=[]}.
If the @Chg{Version=[4],New=[aspect Discard_Names is True
for],Old=[pragma applies to]} an exception,
then the semantics of the Exceptions.@!@Chg{Version=[2],New=[Wide_Wide_@!Exception_@!Name],
Old=[Exception_@!Name]} function
are implementation defined for that exception@Chg{Version=[2],New=[@Redundant[; the
semantics of Exceptions.@!Exception_@!Name and Exceptions.@!Wide_@!Exception_@!Name
are still defined in terms of Exceptions.@!Wide_Wide_@!Exception_@!Name]], Old=[]}.

@ChgImplDef{Version=[4],Kind=[Revised],InitialVersion=[0],
Text=[The semantics of @Chg{Version=[4],New=[some attributes and functions of
an entity for which aspect],Old=[pragma]}
Discard_Names@Chg{Version=[4],New=[ is True],Old=[]}.]}
@begin{Ramification}
  The Width attribute is still defined in terms of Image.

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0072-1]}
  The semantics of S'@Chg{Version=[2],New=[Wide_Wide_Image],Old=[Wide_Image]}
  and S'@Chg{Version=[2],New=[Wide_Wide_Value],Old=[Wide_Value]} are
  implementation defined for any subtype of an enumeration type
  @Chg{Version=[4],New=[for],Old=[to]} which
  the @Chg{Version=[4],New=[aspect is True],Old=[pragma applies]}.
  (The pragma@Chg{Version=[4],New=[, if used,],Old=[]} actually names
  the first subtype, of course.)
@end{Ramification}
@end{StaticSem}

@begin{ImplAdvice}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0072-1]}
If the @Chg{Version=[4],New=[aspect Discard_Names is True
for],Old=[pragma applies to]} an entity, then the implementation should
reduce the amount of storage used for storing names associated with that
entity.
@ChgImplAdvice{Version=[4],Kind=[Revised],InitialVersion=[2],
Text=[@ChgAdded{Version=[2],
Text=[If @Chg{Version=[4],New=[aspect],Old=[@nt{pragma}]} Discard_Names
@Chg{Version=[4],New=[is True for],Old=[applies to]} an entity, then
the amount of storage used for storing names associated with that entity
should be reduced.]}]}
@begin{Reason}
A typical implementation of the Image attribute for enumeration types is
to store a table containing the names of all the enumeration literals.
@Chg{Version=[4],New=[Aspect],Old=[Pragma]} Discard_Names allows the
implementation to avoid storing such a
table without having to prove that the Image attribute is never used
(which can be difficult in the presence of separate compilation).

We did not specify the semantics of the Image attribute
@Chg{Version=[4],New=[when aspect Discard_Names is True],Old=[in the presence
of this pragma]} because different semantics might be desirable in
different situations.
In some cases, it might make sense to use the Image attribute to print
out a useful value that can be used to identify the entity given
information in compiler-generated listings.
In other cases, it might make sense to get an error at compile time or
at run time.
In cases where memory is plentiful, the simplest implementation makes
sense: ignore the @Chg{Version=[4],New=[aspect],Old=[pragma]}.
Implementations that are capable of avoiding the extra storage in cases
where the Image attribute is never used might also wish to ignore the
@Chg{Version=[4],New=[aspect],Old=[pragma]}.

The same applies to the Tags.Expanded_Name and Exceptions.Exception_Name
functions.
@end{Reason}
@end{ImplAdvice}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01],ARef=[AI95-00400-01]}
  @ChgAdded{Version=[2],Text=[Updated the wording to reflect that the double
  wide image and value functions are now the master versions that the others
  are defined from.]}
@end{DiffWord95}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0072-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Defined the pragma in terms of
  the aspect Discard_Names, and added a missing definition of the meaning of
  the configuration pragma. This is not intended to make any semantic change
  (Ada 2012 has an aspect Discard_Names defined via blanket rules for
  representation pragmas in @RefSecNum{Operational and Representation Aspects}
  and @RefSecNum{Aspect Specifications}), just to clarify the meaning.]}
@end{DiffWord2012}


@LabeledClause{Shared Variable Control}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1],ARef=[AI05-0299-1]}
@Redundant[This @Chg{Version=[3],New=[subclause defines],Old=[clause specifies]}
representation @Chg{Version=[3],New=[aspects],Old=[pragmas]} that control the
use of shared variables.]
@end{Intro}

@begin{NotIso}
@ChgAdded{Version=[3],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 2
through 6 were moved to @RefSec{Obsolescent Features}.>}]}@Comment{This message
should be deleted if the paragraphs are ever renumbered.}
@end{NotIso}

@begin{Syntax}
@begin{SyntaxText}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Type=[Leading],Text=[The form for pragmas Atomic,
Volatile, Atomic_Components, and Volatile_Components is as follows:]}
@end{SyntaxText}

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@DeletedPragmaSyn<Version=[3],InitialVersion=[0],@ChgDeleted{Version=[3],
Text=[@key{pragma} @prag(Atomic)(@Syn2{local_name});]}>

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@DeletedPragmaSyn<Version=[3],InitialVersion=[0],@ChgDeleted{Version=[3],
Text=[@key{pragma} @prag(Volatile)(@Syn2{local_name});]}>

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@DeletedPragmaSyn<Version=[3],InitialVersion=[0],@ChgDeleted{Version=[3],
Text=[@key{pragma} @prag(Atomic_Components)(@SynI{array_}@Syn2{local_name});]}>

@ChgRef{Version=[3],Kind=[DeletedNoDelMsg]}
@DeletedPragmaSyn<Version=[3],InitialVersion=[0],@ChgDeleted{Version=[3],
Text=[@key{pragma} @prag(Volatile_Components)(@SynI{array_}@Syn2{local_name});]}>

@end{Syntax}

@Comment{Original: @begin{Intro}, Replaced with below for Ada 2012}
@begin{StaticSem}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For an @nt{object_declaration}, a
@nt{component_declaration}, or a @nt{full_type_declaration}, the following
representation aspects may be specified:]}

@begin{Description}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Atomic@\The type of aspect Atomic is
Boolean.@AspectDefn{Atomic}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Atomic],
    Text=[@ChgAdded{Version=[3],Text=[Declare that a type, object, or component is
      atomic.]}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Independent@\The type of aspect Independent is
Boolean.@AspectDefn{Independent}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Independent],
    Text=[@ChgAdded{Version=[3],Text=[Declare that a type, object, or component
      is independently addressable.]}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Volatile@\The type of aspect Volatile is
Boolean.@AspectDefn{Volatile}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Volatile],
    Text=[@ChgAdded{Version=[3],Text=[Declare that a type, object, or component
      is volatile.]}]}

@end{Description}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For a @nt{full_type_declaration} of
an array type (including the anonymous type of an @nt{object_declaration}
of an anonymous array object), the following representation aspects may be
specified:]}

@begin{Description}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Atomic_Components@\The type of aspect
Atomic_Components is Boolean.@AspectDefn{Atomic_Components}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Atomic_Components],
    Text=[@ChgAdded{Version=[3],Text=[Declare that the components of
      an array type or object are atomic.]}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Volatile_Components@\The type of aspect
Volatile_Components is Boolean.@AspectDefn{Volatile_Components}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Volatile_Components],
    Text=[@ChgAdded{Version=[3],Text=[Declare that the components of
      an array type or object are volatile.]}]}

@end{Description}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For a @nt{full_type_declaration}
(including the anonymous type of an @nt{object_declaration} of an anonymous
array object), the following representation aspect may be specified:]}

@begin{Description}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Independent_Components@\The type of aspect
Independent_Components is Boolean.@AspectDefn{Independent_Components}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Independent_Components],
    Text=[@ChgAdded{Version=[3],Text=[Declare that the components of an array
      or record type, or an array object, are independently addressable.]}]}

@end{Description}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[If any of these aspects are directly
specified, the @nt{aspect_definition} shall be a static expression. If not
specified (including by inheritance), each of these aspects is False.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00272-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Defn{atomic}
An @i{atomic} type is one @Chg{Version=[3],New=[for],Old=[to]} which
@Chg{Version=[3],New=[the aspect],Old=[a pragma]} Atomic
@Chg{Version=[3],New=[is True],Old=[applies]}.
An @i{atomic} object (including a component)
is one @Chg{Version=[3],New=[for],Old=[to]} which
@Chg{Version=[3],New=[the aspect],Old=[a pragma]} Atomic
@Chg{Version=[3],New=[is True],Old=[applies]},
or a component of an array @Chg{Version=[3],New=[for],Old=[to]} which
@Chg{Version=[3],New=[the aspect],Old=[a pragma]} Atomic_Components
@Chg{Version=[3],New=[is True for the associated type],Old=[applies]},
or any object of an atomic type@Chg{Version=[2],
New=[, other than objects obtained by evaluating a slice],Old=[]}.
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00272-01]}
  @ChgAdded{Version=[2],Text=[A slice of an atomic array object is not itself
  atomic. That's necessary as executing a read or write of a dynamic number
  of components in a single instruction is not possible on many targets.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Defn{volatile}
A @i{volatile} type is one @Chg{Version=[3],New=[for],Old=[to]} which
@Chg{Version=[3],New=[the aspect],Old=[a pragma]} Volatile
@Chg{Version=[3],New=[is True],Old=[applies]}.
A @i{volatile} object (including a component)
is one @Chg{Version=[3],New=[for],Old=[to]} which
@Chg{Version=[3],New=[the aspect],Old=[a pragma]} Volatile
@Chg{Version=[3],New=[is True],Old=[applies]},
or a component of an array @Chg{Version=[3],New=[for],Old=[to]} which
@Chg{Version=[3],New=[the aspect],Old=[a pragma]} Volatile_@!Components
@Chg{Version=[3],New=[is True for the associated type],Old=[applies]},
or any object of a volatile type.
In addition, every atomic type or object is also defined to be volatile.
Finally, if an object is volatile, then so
are all of its subcomponents @Redundant[(the same does not apply to atomic)].

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0009-1],ARef=[AI05-0229-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0001-1]}
@ChgAdded{Version=[3],Text=[When True, the aspects Independent and
Independent_Components @i<specify as independently addressable> the named object
or component(s), or in the case of a type, all objects or components
of that type. All atomic objects @Chg{Version=[4],New=[and aliased
objects ],Old=[]}are considered to be specified as independently
addressable.@Defn{specified as independently addressable}@Defn2{Term=[independently addressable],Sec=[specified]}]}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[If the compiler cannot guarantee that an object
(including a component) for which aspect Independent or aspect
Independent_Components is True is independently addressable from
any other nonoverlapping object, then the aspect specification
must be rejected.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Similarly, an atomic object (including atomic
components) is always independently addressable from any other nonoverlapping
object. Any representation item which would prevent this from being true should
be rejected, notwithstanding what this Standard says elsewhere (specifically,
in the Recommended Level of Support).]}
@end{Ramification}

@Comment{Original: @end{Intro}, Replaced with below for Ada 2012}
@end{StaticSem}

@begin{NotIso}
@ChgAdded{Version=[3],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraph 9
was moved to @RefSec{Obsolescent Features}.>}]}@Comment{This message
should be deleted if the paragraphs are ever renumbered.}@ChgNote{We use
this message rather than the "automatic one" to get rid of the resolution
subheader}
@end{NotIso}

@begin{Resolution}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[The @nt{local_name} in an Atomic or Volatile
pragma shall resolve to denote either an @nt{object_declaration}, a
noninherited @nt{component_@!declaration}, or a @nt{full_type_@!declaration}.
The @SynI{array_}@nt{local_name} in an Atomic_@!Components or
Volatile_@!Components pragma shall resolve to denote the declaration of an array
type or an array object of an anonymous type.]}
@end{Resolution}

@begin{Legality}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[If aspect Independent_Components is specified for a
@nt{full_type_declaration}, the declaration shall be that of an array or record
type.]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0001-1]}
@Defn{indivisible}
It is illegal to @Chg{Version=[3],New=[specify],Old=[apply]}
either @Chg{Version=[3],New=[of the aspects],Old=[an]} Atomic or
Atomic_Components @Chg{Version=[3],New=[],Old=[pragma ]}to
@Chg{Version=[3],New=[have the value True for ],Old=[]}an
object or type if the implementation cannot support the indivisible
@Chg{Version=[4],New=[and independent ],Old=[]}reads
and updates required by the @Chg{Version=[3],New=[aspect],Old=[pragma]}
(see below).

@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0001-1]}
It is illegal to specify the Size attribute of an atomic object, the
Component_Size attribute for an array type with atomic components, or the layout
attributes of an atomic component, in a way that prevents the implementation
from performing the required indivisible
@Chg{Version=[4],New=[and independent ],Old=[]}reads and updates.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0142-4],ARef=[AI05-0218-1]}
If an atomic object is passed as a parameter, then
@Chg{Version=[3],New=[],Old=[the type of ]}the formal
parameter shall either @Chg{Version=[3],New=[have an],Old=[be]} atomic
@Chg{Version=[3],New=[type ],Old=[]}or allow pass by
copy@Chg{Version=[3],New=[],Old=[ @Redundant[(that
is, not be a nonatomic by-reference type)]]}. If an atomic object is used
as an actual for a generic formal object of mode @key{in out}, then the
type of the generic formal object shall be atomic. If the @nt<prefix> of
an @nt<attribute_reference> for an Access attribute denotes an atomic
object @Redundant[(including a component)], then the designated type of
the resulting access type shall be atomic. If an atomic type is used as
an actual for a generic formal derived type, then the ancestor of the
formal type shall be atomic@Chg{Version=[3],New=[],Old=[ or allow pass by copy]}.
Corresponding rules apply to volatile objects and types.

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0142-4]}
  @ChgAdded{Version=[3],Text=[A formal parameter allows pass by copy
  if it is not @key[aliased] and it is of a type that allows pass by copy (that
  is, is not a by-reference type).]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0218-1]}
@ChgAdded{Version=[3],Text=[If a volatile type is used as an actual for a
generic formal array type, then the element type of the formal type shall be
volatile.]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
If @Chg{Version=[3],New=[an aspect],Old=[a pragma]} Volatile,
Volatile_Components, Atomic, or Atomic_Components
@Chg{Version=[3],New=[is directly specified to have the value
True for],Old=[applies to]} a stand-alone constant object, then
@Chg{Version=[3],New=[the aspect],Old=[a pragma]} Import shall
also @Chg{Version=[3],New=[be specified as True for],Old=[apply to]} it.
@begin{Ramification}
Hence, no initialization expression is allowed for such a constant. Note
that a constant that is atomic or volatile because of its type is allowed.
@end{Ramification}
@begin{Reason}
Stand-alone constants that are explicitly specified as Atomic or Volatile
only make sense if they are being manipulated outside the Ada program.
From the Ada perspective the object is read-only. Nevertheless, if
imported and atomic or volatile, the implementation should presume it
might be altered externally.
For an imported stand-alone constant that is not atomic or
volatile, the implementation can assume that it will not be
altered.
@end{Reason}
@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0218-1]}
  @ChgAdded{Version=[3],Text=[Volatile_Components and Atomic_Components actually
  are aspects of the anonymous array type; this rule only applies when the
  aspect is specified directly on the constant object and not when the (named)
  array type has the aspect.]}
@end{Honest}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0009-1],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[It is illegal to specify the aspect Independent or
Independent_Components as True for a component, object or type if the
implementation cannot provide the independent addressability required by the
aspect (see @RefSecNum{Shared Variables}).]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0009-1],ARef=[AI05-0229-1]}
@ChgAdded{Version=[3],Text=[It is illegal to specify a representation aspect for
a component, object or type for which the aspect Independent or
Independent_Components is True, in a way that prevents the implementation from
providing the independent addressability required by the aspect.]}

@end{Legality}

@begin{NotIso}
@ChgAdded{Version=[3],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraph 14
was moved to @RefSec{Obsolescent Features}.>}]}@Comment{This message
should be deleted if the paragraphs are ever renumbered.}@ChgNote{We use
this message rather than the "automatic one" to get rid of the resolution
subheader}
@end{NotIso}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0229-1]}
@ChgDeleted{Version=[3],Text=[@PDefn2{Term=[representation pragma], Sec=(Atomic)}
@PDefn2{Term=[pragma, representation], Sec=(Atomic)}
@PDefn2{Term=[representation pragma], Sec=(Volatile)}
@PDefn2{Term=[pragma, representation], Sec=(Volatile)}
@PDefn2{Term=[representation pragma], Sec=(Atomic_Components)}
@PDefn2{Term=[pragma, representation], Sec=(Atomic_Components)}
@PDefn2{Term=[representation pragma], Sec=(Volatile_Components)}
@PDefn2{Term=[pragma, representation], Sec=(Volatile_Components)}
These @nt{pragma}s are representation pragmas
(see @RefSecNum{Operational and Representation Aspects}).]}
@end{StaticSem}

@begin{RunTime}

For an atomic object (including an atomic component) all reads and
updates of the object as a whole are indivisible.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0117-1],ARef=[AI05-0275-1]}
@Chg{Version=[3],New=[All tasks of the program (on all processors) that
read or update volatile variables see the same order of
updates to the variables. A use of an atomic variable or other mechanism may be
necessary to avoid erroneous execution and to ensure that access to
nonatomic volatile variables is sequential (see @RefSecNum{Shared Variables}).],
Old=[For a volatile object all reads and updates of the object as a whole are
performed directly to memory.]}

@begin{ImplNote}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0117-1],ARef=[AI05-0275-1]}
@Chg{Version=[3],New=[To ensure this, on a multiprocessor, any read or
update of an atomic object may require the use of an appropriate memory
barrier.],Old=[This precludes any use of register temporaries,
caches, and other similar optimizations for that object.]}
@end{ImplNote}

@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0275-1]}
@ChgAdded{Version=[3],Text=[From @RefSecNum{Shared Variables} it follows that
(in non-erroneous programs) accesses to variables, including those shared by
multiple tasks, are always sequential. This guarantees that no task will ever
see partial updates of any variable. For volatile variables (including atomic
variables), the above rule additionally specifies that all tasks see the same
order of updates.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0275-1]}
@ChgAdded{Version=[3],Text=[If for a shared variable @i<X>, a read of @i<X>
occurs sequentially after an update of @i<X>, then the read will return the
updated value if @i<X> is volatile or atomic, but may or or may not return the
updated value if @i<X> is nonvolatile. For nonvolatile accesses, a signaling
action is needed in order to share the updated value.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0275-1]}
@ChgAdded{Version=[3],Text=[Because accesses to the same atomic variable by
different tasks establish a sequential order between the actions of those tasks,
implementations may be required to emit memory barriers around such updates or
use atomic instructions that imply such barriers.]}
@end{Discussion}

@Defn2{Term=[sequential], Sec=(actions)}
Two actions are sequential (see @RefSecNum{Shared Variables}) if each
is the read or update of the same atomic object.

@PDefn2{Term=[by-reference type], Sec=(atomic or volatile)}
If a type is atomic or volatile and it is not a by-copy type, then the type
is defined to be a by-reference type. If any subcomponent of a type is
atomic or volatile, then the type is defined to be a by-reference type.

If an actual parameter is atomic or volatile, and the corresponding
formal parameter is not, then the parameter is passed by copy.
@begin{ImplNote}
Note that in the case where such a parameter is normally passed by
reference, a copy of the actual will have to be produced at the call-site,
and a pointer to the copy passed to the formal parameter. If the actual
is atomic, any copying has to use indivisible read on the way in, and
indivisible write on the way out.
@end{ImplNote}
@begin{Reason}
It has to be known at compile time whether an atomic or a volatile parameter
is to be passed by copy or by reference. For some types, it is unspecified
whether parameters are passed by copy or by reference. The above rules
further specify the parameter passing rules involving atomic and volatile
types and objects.
@end{Reason}

@end{RunTime}

@begin{ImplReq}

@PDefn2{Term=[external effect], Sec=(volatile/atomic objects)}
The external effect of a program
(see @RefSecNum(Conformity of an Implementation with the Standard))
is defined to include each read and update
of a volatile or atomic object. The implementation shall not
generate any memory reads or updates of atomic or volatile
objects other than those specified by the program.
@begin{Discussion}
The presumption is that volatile or atomic objects might reside in an
@lquotes@;active@rquotes@; part of the address space where each read has a
potential side effect, and at the very least might deliver a different value.

@Leading@;The rule above and the definition of external effect are intended to
prevent (at least) the following incorrect optimizations, where V is
a volatile variable:
@begin{itemize}
X:= V; Y:=V; cannot be allowed to be translated as Y:=V; X:=V;

Deleting redundant loads: X:= V; X:= V; shall read the value of V from
memory twice.

Deleting redundant stores: V:= X; V:= X; shall write into V twice.

Extra stores: V:= X+Y; should not translate to something like V:= X; V:= V+Y;

Extra loads: X:= V; Y:= X+Z; X:=X+B; should not translate to something like
Y:= V+Z; X:= V+B;

Reordering of loads from volatile variables: X:= V1; Y:= V2; (whether or not
V1 = V2) should not translate to Y:= V2; X:= V1;

Reordering of stores to volatile variables: V1:= X; V2:= X; should not
translate to V2:=X; V1:= X;
@end{itemize}
@end{Discussion}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@ChgRef{Version=[4],Kind=[Deleted],ARef=[AI12-0001-1]}
@ChgDeleted{Version=[4],Text=[If @Chg{Version=[3],New=[the],Old=[a pragma]} Pack
@Chg{Version=[3],New=[aspect is True for],Old=[applies to]} a type any of whose
subcomponents are atomic,
the implementation shall not pack the atomic subcomponents more tightly
than that for which it can support indivisible reads and updates.]}
@begin{ImplNote}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0009-1]}
@ChgRef{Version=[4],Kind=[Deleted],ARef=[AI12-0001-1]}
@ChgDeleted{Version=[4],Text=[@Chg{Version=[3],New=[Usually, specifying aspect Pack for such a type
will be illegal as the Recommended Level of Support cannot be achieved;
otherwise, a],Old=[A]} warning might be appropriate if
no packing whatsoever can be achieved.]}
@end{ImplNote}
@end{ImplReq}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00259-01]}
@ChgAdded{Version=[2],Text=[A load or store of a volatile object whose size is
a multiple of System.Storage_Unit and whose alignment is nonzero, should be
implemented by accessing exactly the bits of the object and no others.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[A load or store of a volatile object whose size is
a multiple of System.Storage_Unit and whose alignment is nonzero, should be
implemented by accessing exactly the bits of the object and no others.]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Since any object can be a volatile object,
  including packed array components and bit-mapped record components, we
  require the above only when it is reasonable to assume that the machine
  can avoid accessing bits outside of the object.]}
@end{Reason}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This implies that the load or store of a
  volatile object that meets the above requirement should not be combined
  with that of any other object, nor should it access any bits not
  belonging to any other object. This means that the suitability of the
  implementation for memory-mapped I/O can be determined from its
  documentation, as any cases where the implementation does not follow
  @ImplAdviceTitle must be documented.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00259-01]}
@ChgAdded{Version=[2],Text=[A load or store of an atomic object should, where
possible, be implemented by a single load or store instruction.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[A load or store of an atomic object should be implemented by a single
load or store instruction.]}]}

@end{ImplAdvice}

@begin{Notes}

An imported volatile or atomic constant behaves as a constant (i.e.
read-only) with respect to other parts of the Ada program, but can
still be modified by an @lquotes@;external source.@rquotes@;

@ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0001-1]}
@ChgAdded{Version=[4],Text=[Specifying the Pack aspect cannot override the
effect of specifying an Atomic or Atomic_Components aspect.]}

@end{Notes}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
Pragma Atomic replaces Ada 83's pragma Shared.
The name @lquotes@;Shared@rquotes@; was confusing,
because the pragma was not used to mark variables as shared.
@end{Incompatible83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00259-01]}
  @ChgAdded{Version=[2],Text=[Added @ImplAdviceTitle to clarify the meaning
  of Atomic and Volatile in machine terms. The documentation that this
  advice applies will make the use of Ada implementations more predictable
  for low-level (such as device register) programming.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00272-01]}
  @ChgAdded{Version=[2],Text=[Added wording to clarify that a slice of an
  object of an atomic type is not atomic, just like a component of an atomic
  type is not (necessarily) atomic.]}
@end{Diffword95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0218-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Plugged a hole involving volatile components of formal types when the formal
  type's component has a nonvolatile type. This was done by making certain
  actual types illegal for formal derived and formal array types; these types
  were allowed for Ada 95 and Ada 2005.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0009-1],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Aspects Independent and Independent_Components are new; they eliminate
  ambiguity about independent addressability.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1]}
  @ChgAdded{Version=[3],Text=[Aspects Atomic, Atomic_Components, Volatile,
  and Volatile_Components are
  new; @nt{pragma}s Atomic, Atomic_Components, Volatile, and
  Volatile_Components are now obsolescent.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0117-1],ARef=[AI05-0275-1]}
  @ChgAdded{Version=[3],Text=[Revised the definition of volatile to
  eliminate overspecification and simply focus on the root requirement (that
  all tasks see the same view of volatile objects). This is not an
  inconsistency; "memory" arguably includes on-chip caches so long as those are
  kept consistent. Moreover, it is difficult to imagine a program that could tell
  the difference.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0142-4]}
  @ChgAdded{Version=[3],Text=[Added wording to take explicitly aliased
  parameters (see @RefSecNum{Subprogram Declarations}) into
  account when determining the legality of parameter passing of volatile and
  atomic objects.]}
@end{DiffWord2005}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0001-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Clarified that aliased objects
  are considered to be specified as independently addressable, and also
  eliminated an unnecessary rule.]}
@end{DiffWord2012}


@LabeledRevisedClause{Version=[2],New=[Task Information],
Old=[Task Identification and Attributes]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00266-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Redundant[This @Chg{Version=[3],New=[subclause],Old=[clause]}
describes operations and attributes that can be
used to obtain the identity of a task. In addition,
a package that associates user-defined information with a task is
defined.@Chg{Version=[2],New=[ Finally, a package that associates
termination procedures with a task or set of tasks is defined.],Old=[]}]
@end{Intro}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00266-02]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
  @ChgAdded{Version=[2],Text=[The title and text here were updated to reflect
  the addition of task termination procedures to this @Chg{Version=[3],New=[subclause],Old=[clause]}.]}
@end{DiffWord95}

@LabeledSubClause{The Package Task_Identification}

@begin{StaticSem}
@Leading@Keepnext@;The following language-defined library package exists:
@begin{example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00362-01]}
@key[package] Ada.Task_Identification @key[is]@ChildUnit{Parent=[Ada],Child=[Task_Identification]}@Chg{Version=[2],New=[
   @key[pragma] Preelaborate(Task_Identification);],Old=[]}
   @key[type] @AdaTypeDefn{Task_Id} @key[is] @key{private};@Chg{Version=[2],New=[
   @key[pragma] Preelaborable_Initialization (Task_Id);],Old=[]}
   @AdaSubDefn{Null_Task_Id} : @key{constant} Task_Id;
   @key{function}  "=" (Left, Right : Task_Id) @key{return} Boolean;

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0070],ARef=[AI95-00101-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0189-1]}
   @key{function}  @AdaSubDefn{Image}                  (T : Task_Id) @key{return} String;
   @key[function]  @AdaSubDefn{Current_Task}     @key[return] Task_Id;@Chg{Version=[3],New=[
   @key[function]  @AdaSubDefn{Environment_Task} @key[return] Task_Id;],Old=[]}
   @Key[procedure] @AdaSubDefn{Abort_Task}             (T : @key[in] @Chg{New=[],Old=[@key[out] ]}Task_Id);

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0189-1]}
   @key[function]  @AdaSubDefn{Is_Terminated}          (T : Task_Id) @key{return} Boolean;
   @key[function]  @AdaSubDefn{Is_Callable}            (T : Task_Id) @key{return} Boolean;@Chg{Version=[3],New=[
   @key[function]  @AdaSubDefn{Activation_Is_Complete} (T : Task_Id) @key{return} Boolean;],Old=[]}
@key[private]
   ... -- @RI{not specified by the language}
@key[end] Ada.Task_Identification;
@end{example}
@end{StaticSem}

@begin{RunTime}

A value of the type Task_Id identifies an existent task. The constant
Null_Task_Id does not identify any task. Each object of the type Task_Id
is default initialized to the value of Null_Task_Id.

The function "=" returns True if and only if Left and Right identify the same
task or both have the value Null_Task_Id.

The function Image returns an implementation-defined string that identifies
T. If T equals Null_Task_Id, Image returns an empty string.
@ImplDef{The result of the Task_Identification.Image attribute.}

The function Current_Task returns a value that identifies the calling task.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0189-1]}
@ChgAdded{Version=[3],Text=[The function Environment_Task returns a value that
identifies the environment task.]}

The effect of Abort_Task is the same as the @nt{abort_statement} for the
task identified by T. @Redundant[In addition, if T identifies the
environment task, the entire partition is aborted, See @RefSecNum{Partitions}.]

The functions Is_Terminated and Is_Callable return the value of the
corresponding attribute of the task identified by T.
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0115],ARef=[AI95-00206-01]}
@ChgAdded{Version=[1],Text=[These routines can be called with an argument
identifying the environment task. Is_Terminated will always be False for such a
call, but Is_Callable (usually True) could be False if the environment task is
waiting for the termination of dependent tasks. Thus, a dependent task can use
Is_Callable to determine if the main subprogram has completed.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0189-1]}
@ChgAdded{Version=[3],Text=[The function Activation_Is_Complete returns True if the
task identified by T has completed its activation (whether successfully or not).
It returns False otherwise. If T identifies the environment task,
Activation_Is_Complete returns True after the elaboration of the
@nt{library_item}s of the partition has completed.]}

@Leading@;For @PrefixType{a @nt<prefix> T that is of a task type
@Redundant[(after any implicit dereference)]},
the following attribute is defined:
@begin{Description}

@Attribute{Prefix=<T>, AttrName=<Identity>,
  Text=[Yields a value of the type Task_Id that identifies
    the task denoted by T.]}

@end{Description}
@EndPrefixType{}

@Leading@;For @PrefixType{a @nt<prefix> E that denotes an
@nt<entry_declaration>},
the following attribute is defined:
@begin{Description}
@ChgAttribute{Version=[3],Kind=[Revised],ChginAnnex=[T],
    Leading=<F>, Prefix=<E>, AttrName=<Caller>, ARef=[AI05-0262-1],
    InitialVersion=[0], Text=[Yields a value of the type Task_Id that
       identifies the task whose call is now being serviced. Use of this
       attribute is allowed only inside an
       @Chg{Version=[3],New=[],Old=[@nt{entry_body}
       or ]}@nt{accept_statement}@Chg{Version=[3],New=[,
       or @nt{entry_body} after the
       @nt{entry_barrier}, ],Old=[]}corresponding
       to the @nt{entry_declaration} denoted by E.]}
@end{Description}
@EndPrefixType{}

@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
Program_Error is raised if a value of Null_Task_Id is passed
as a parameter to Abort_Task, Is_Terminated, and Is_Callable.

@PDefn2{Term=[potentially blocking operation],Sec=(Abort_Task)}
@PDefn2{Term=[blocking, potentially],Sec=(Abort_Task)}
Abort_Task is a potentially blocking operation
(see @RefSecNum{Protected Subprograms and Protected Actions}).
@end{RunTime}

@begin{Bounded}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00237-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0004-1]}
@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to call the Current_Task function from
an @Chg{Version=[3],New=[@nt{entry_body}],Old=[entry body]}@Chg{Version=[2],New=[,],Old=[ or an]}
interrupt handler@Chg{Version=[2],New=[, or finalization of a task attribute],Old=[]}.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
Program_Error is raised, or an implementation-defined value of the type
Task_Id is returned.
@ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],
Text=[The value of Current_Task when in
a protected entry@Chg{Version=[2],New=[,],Old=[ or]} interrupt handler@Chg{Version=[2],
New=[, or finalization of a task attribute],Old=[]}.]}
@begin{ImplNote}
This value could be Null_Task_Id, or the ID of some user task, or that of
an internal task created by the implementation.
@end{ImplNote}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00237-01]}@Comment{This really should reference AI05-0004, but we don't have that yet. And that hasn't been approved, either}
@ChgAdded{Version=[2],Text=[An entry barrier is syntactically part of an
@nt{entry_body}, so a call to Current_Task from an entry barrier is also
covered by this rule.]}
@end{Ramification}

@end{Bounded}

@begin{Erron}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
If a value of Task_Id is passed as a parameter to any of the operations
declared in this package (or any language-defined child of this
package), and the corresponding task object no longer exists,
the execution of the program is erroneous.
@end{Erron}

@begin{DocReq}

The implementation shall document the effect of calling Current_Task
from an entry body or interrupt handler.
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[The effect of calling Current_Task from an
entry body or interrupt handler.]}]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The effect of calling Current_Task from an
entry body or interrupt handler.]}]}
@end{DocReq}

@begin{Notes}

This package is intended for use in writing user-defined task scheduling
packages and constructing server tasks. Current_Task can be used in
conjunction with other operations requiring a task as an argument such
as Set_Priority (see @RefSecNum{Dynamic Priorities}).

The function Current_Task and the attribute Caller can return a
Task_Id value that identifies the environment task.

@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00362-01]}
  @ChgAdded{Version=[2],Text=[Task_Identification is now preelaborated,
  so it can be used in preelaborated units.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0070],ARef=[AI95-00101-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the mode of the
  parameter to Abort_Task to @key{in}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00237-01]}
  @ChgAdded{Version=[2],Text=[Corrected the wording to include finalization
  of a task attribute in the bounded error case; we don't want to specify
  which task does these operations.]}
@end{Diffword95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0189-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
  Functions Environment_Task and Activation_Is_Complete are added to
  Task_Identification. If Task_Identification is referenced in a @nt{use_clause}, and an
  entity @i<E> with a @nt{defining_identifier} of Environment_Task or
  Activation_Is_Complete is defined in a package that is also referenced in a
  @nt{use_clause}, the entity @i<E> may no longer be use-visible, resulting in
  errors. This should be rare and is easily fixed if it does occur.]}
@end{Incompatible2005}


@LabeledSubClause{The Package Task_Attributes}

@begin{StaticSem}
@Leading@Keepnext@;The following language-defined generic library package exists:
@begin{example}
@key{with} Ada.Task_Identification; @key{use} Ada.Task_Identification;
@key{generic}
   @key{type} Attribute @key{is} @key{private};
   Initial_Value : @key[in] Attribute;
@key{package} Ada.Task_Attributes @key{is}@ChildUnit{Parent=[Ada],Child=[Task_Attributes]}

   @key{type} @AdaTypeDefn{Attribute_Handle} @key{is} @key{access} @key{all} Attribute;

   @key{function} @AdaSubDefn{Value}(T : Task_Id := Current_Task)
     @key{return} Attribute;

   @key{function} @AdaSubDefn{Reference}(T : Task_Id := Current_Task)
     @key{return} Attribute_Handle;

   @key{procedure} @AdaSubDefn{Set_Value}(Val : @key[in] Attribute;
                       T : @key[in] Task_Id := Current_Task);
   @key{procedure} @AdaSubDefn{Reinitialize}(T : @key[in] Task_Id := Current_Task);

@key{end} Ada.Task_Attributes;
@end{example}

@end{StaticSem}

@begin{RunTime}

When an instance of Task_Attributes is elaborated in
a given active partition, an object of the
actual type corresponding to the formal type Attribute
is implicitly created for each task (of that partition)
that exists and is not yet terminated.
This object acts as a user-defined attribute of the task.
A task created previously
in the partition and not yet terminated has this attribute
from that point on. Each task subsequently created in the partition
will have this attribute when created. In all these cases, the initial value
of the given attribute is Initial_Value.

The Value operation returns the value of the corresponding attribute of T.

The Reference operation returns an access value that designates the
corresponding attribute of T.

The Set_Value operation performs any finalization on the old value of the
attribute of T and assigns Val to that attribute
(see @RefSecNum{Assignment Statements} and
@RefSecNum{Assignment and Finalization}).

The effect of the Reinitialize operation is the same as Set_Value where
the Val parameter is replaced with Initial_Value.
@begin{ImplNote}
In most cases, the attribute memory can be reclaimed at this point.
@end{ImplNote}

@Defn2{Term=[Tasking_Error],Sec=(raised by failure of run-time check)}
For all the operations declared in this package, Tasking_Error is raised
if the task identified by T is terminated.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
Program_Error is raised if the value of T is Null_Task_Id.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00237-01]}
@ChgAdded{Version=[2],Text=[After a task has terminated, all of its attributes
are finalized, unless they have been finalized earlier. When the master of an
instantiation of Ada.Task_Attributes is finalized, the corresponding attribute
of each task is finalized, unless it has been finalized earlier.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is necessary so that a task attribute does
  not outlive its type. For instance, that's possible if the instantiation is
  nested, and the attribute is on a library-level task.]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The task owning an attribute cannot, in general,
  finalize that attribute. That's because the attributes are finalized @i<after>
  the task is terminated; moreover, a task may have attributes as soon as it is
  created; the task may never even have been activated.]}
@end{Ramification}
@end{RunTime}

@begin{Bounded}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0071],ARef=[AI95-00165-01]}
@ChgAdded{Version=[1],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
If the package Ada.Task_Attributes is instantiated with a controlled type and
the controlled type has user-defined Adjust or Finalize operations that in
turn access task attributes by any of the above operations, then a call of
Set_Value of the instantiated package constitutes a bounded error. The call
may perform as expected or may result in forever blocking the calling task and
subsequently some or all tasks of the partition.]}
@end{Bounded}

@begin{Erron}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
It is erroneous to dereference the access value returned by a given
call on Reference after a subsequent call on Reinitialize for
the same task attribute, or after the associated task terminates.
@begin{Reason}
  This allows the storage to be reclaimed for the object
  associated with an attribute upon Reinitialize or task termination.
@end{Reason}

@PDefn2{Term=(erroneous execution),Sec=(cause)}
If a value of Task_Id is passed as a parameter to any of the operations
declared in this package and the corresponding task object no longer exists,
the execution of the program is erroneous.

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0071],ARef=[AI95-00165-01]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00237-01]}
@ChgAdded{Version=[1],Text=[@PDefn2{Term=(erroneous execution),Sec=(cause)}
@Chg{Version=[2],New=[An access],Old=[Accesses]} to @Chg{Version=[2],New=[a ],Old=[]}task
@Chg{Version=[2],New=[attribute],Old=[attributes]} via a value of type
Attribute_Handle @Chg{Version=[2],New=[is],Old=[are]}
erroneous if executed concurrently with @Chg{Version=[2],New=[another such access],
Old=[each other]} or @Chg{Version=[2],New=[a call],Old=[with calls]} of any of the
operations declared in package Task_Attributes.@Chg{Version=[2],New=[ An access
to a task attribute is erroneous if executed
concurrently with or after the finalization of the task attribute.],Old=[]}]}
@begin{Reason}
  @ChgRef{Version=[1],Kind=[Added]}
  @ChgAdded{Version=[1],Text=[There is no requirement of atomicity on accesses
  via a value of type Attribute_Handle.]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[A task attribute can only be accessed after
  finalization through a value of type Attribute_Handle. Operations in
  package Task_Attributes cannot be used to access a task attribute after
  finalization, because either the master of the instance has been or is in the
  process of being left (in which case the instance is out of scope and thus
  cannot be called), or the associated task is already terminated (in which
  case Tasking_Error is raised for any attempt to call a task attribute
  operation).]}
@end{Ramification}
@end{Erron}

@begin{ImplReq}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0071],ARef=[AI95-00165-01]}
@Chg{New=[For a given attribute of a given task, the],Old=[The]} implementation
shall perform @Chg{New=[the operations declared in this package],
Old=[each of the above operations for a given attribute of a given task]}
atomically with respect to any @Chg{New=[of these operations of],
Old=[other of the above operations for]} the same attribute of the same task.
@Chg{New=[The granularity of any locking mechanism necessary to achieve such
atomicity is implementation defined.],Old=[]}
@ChgImplDef{Version=[1],Kind=[Added],Text=[@Chg{New=[Granularity of
locking for Task_Attributes.],Old=[]}]}

@begin{Ramification}
Hence, other than by dereferencing an access value returned by
Reference, an attribute of a given task can be safely read and updated
concurrently by multiple tasks.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00237-01]}
@Chg{Version=[2],New=[After],Old=[When a]} task
@Chg{Version=[2],New=[attributes are finalized],Old=[terminates]},
the implementation shall @Chg{Version=[2],New=[],Old=[finalize
all attributes of the task, and ]}reclaim any
@Chg{Version=[2],New=[],Old=[other ]}storage associated with the attributes.
@end{ImplReq}

@begin{DocReq}

The implementation shall document the limit on the number of attributes
per task, if any, and the limit on the total storage for attribute values
per task, if such a limit exists.

In addition, if these limits can be configured, the implementation shall
document how to configure them.
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[@Chg{New=[Limits on the number and size of task attributes, and how to configure them.],
Old=[Implementation-defined aspects of Task_Attributes.]}]}]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[For package Task_Attributes, limits on the number and size of task
attributes, and how to configure any limits.]}]}
@end{DocReq}

@begin{Metrics}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
The implementation shall document the following metrics: A task calling the
following subprograms shall execute @Chg{Version=[2],New=[at],Old=[in]} a
sufficiently high priority as to not
be preempted during the measurement period. This period shall start just
before issuing the call and end just after the call completes. If the
attributes of task T are accessed by the measurement tests, no other task
shall access attributes of that task during the measurement period.
For all measurements described here, the Attribute type shall be a scalar
@Chg{Version=[2],New=[type ],Old=[]}whose size is equal to the size of the
predefined @Chg{Version=[2],New=[type Integer],Old=[integer size]}.
For each measurement, two cases shall be documented: one
where the accessed attributes are of the calling task @Redundant[(that is,
the default value for the T parameter is used)], and the other, where T
identifies another, nonterminated, task.

@Leading@;The following calls (to subprograms in the Task_Attributes package)
shall be measured:
@begin{Itemize}
a call to Value, where the return value is Initial_Value;

a call to Value, where the return value is not equal to Initial_Value;

a call to Reference, where the return value designates a value equal to
Initial_Value;

a call to Reference, where the return value designates a value not equal
to Initial_Value;

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
a call to Set_Value where the Val parameter is not equal to Initial_Value
and the old attribute value is equal to Initial_Value@Chg{Version=[2],New=[;],Old=[.]}

a call to Set_Value where the Val parameter is not equal to Initial_Value
and the old attribute value is not equal to Initial_Value.

@end{Itemize}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The metrics for the Task_Attributes package.]}]}
@end{Metrics}

@begin{ImplPerm}

An implementation need not actually create the object corresponding
to a task attribute
until its value is set to something other than that of Initial_Value,
or until Reference is called for the task attribute.
Similarly, when the value of the attribute is
to be reinitialized to that of Initial_Value,
the object may instead be finalized and its storage reclaimed, to
be recreated when needed later.
While the object does not exist, the function Value may
simply return Initial_Value, rather than implicitly creating the object.
@begin{Discussion}
  The effect of this permission can only be observed if the assignment
  operation for the corresponding type has side effects.
@end{Discussion}
@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
  This permission means that even though every task has every
  attribute, storage need only be allocated for those attributes
  @Chg{Version=[2],New=[for which function Reference has been invoked],Old=[that
  have been Reference'd]} or set to a value other than that
  of Initial_Value.
@end{ImplNote}

An implementation is allowed to place restrictions on the maximum number of
attributes a task may have, the maximum size of each attribute, and the
total storage size allocated for all the attributes of a task.

@end{ImplPerm}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
Some implementations are targeted to domains in which memory use at run
time must be completely deterministic. For such implementations, it is
recommended that the storage for task attributes will be pre-allocated
statically and not from the heap. This can be accomplished by either
placing restrictions on the number and the size of the @Chg{Version=[2],
New=[],Old=[task's ]}attributes@Chg{Version=[2],New=[ of a task],Old=[]},
or by using the pre-allocated storage for the first N attribute objects,
and the heap for the others. In the latter case, N should be documented.

@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[If the target domain requires deterministic memory use at run
time, storage for task attributes should be pre-allocated
statically and the number of attributes pre-allocated should be documented.]}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We don't mention @lquotes@;restrictions on the
  size and number@rquotes (that is, limits) in the text for the
  Annex, because it is covered by the @DocReqName above, and we try not to
  repeat requirements in the Annex (they're enough work to meet without
  having to do things twice).]}
@end{Discussion}
@end{ImplAdvice}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00237-01]}
@ChgAdded{Version=[2],Text=[Finalization of task attributes and reclamation of
associated storage should be performed as soon as possible after task
termination.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Finalization of task attributes and reclamation of
associated storage should be performed as soon as possible after task
termination.]}]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00237-01]}
  @ChgAdded{Version=[2],Text=[This is necessary because the normative wording
  only says that attributes are finalized @lquotes@;after@rquotes@;
  task termination. Without this advice, waiting until the instance is
  finalized would meet the requirements (it is after termination, but may be
  a very long time after termination). We can't say anything more specific
  than this, as we do not want to require the overhead of an interaction with
  the tasking system to be done at a specific point.]}
@end{Reason}
@begin{Notes}

An attribute always exists (after instantiation), and has the initial value.
It need not occupy memory until the first operation that potentially
changes the attribute value. The same holds true after Reinitialize.

The result of the Reference function should be used with care; it is always
safe to use that result in the task body whose attribute is being
accessed. However, when the result is being used by another task, the
programmer must make sure that the task whose attribute is being accessed
is not yet terminated. Failing to do so could make the program execution
erroneous.

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00434-01]}
@ChgDeleted{Version=[2],Text=[As specified in
@RefSecNum{The Package Task_Identification}, if the parameter T (in a call
on a subprogram of an instance of this package) identifies a nonexistent
task, the execution of the program is erroneous.]}
@end{Notes}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0071],ARef=[AI95-00165-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that use of task
  attribute operations from within a task attribute operation (by an Adjust
  or Finalize call) is a bounded error, and that concurrent use of attribute
  handles is erroneous.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00237-01]}
  @ChgAdded{Version=[2],Text=[Clarified the wording so that the finalization
  takes place after the termination of the task or when the instance is
  finalized (whichever is sooner).]}
@end{Diffword95}

@LabeledAddedSubClause{Version=[2],Name=[The Package Task_Termination]}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00266-02]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The following
language-defined library package exists:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key<with> Ada.Task_Identification;
@key<with> Ada.Exceptions;
@key<package> Ada.Task_Termination @key<is>@ChildUnit{Parent=[Ada],Child=[Task_Termination]}
   @key<pragma> Preelaborate(Task_Termination);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<type> @AdaTypeDefn{Cause_Of_Termination} @key<is> (Normal, Abnormal, Unhandled_Exception);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<type> @AdaTypeDefn{Termination_Handler} @key<is access protected procedure>
     (Cause : @key<in> Cause_Of_Termination;
      T     : @key<in> Ada.Task_Identification.Task_Id;
      X     : @key<in> Ada.Exceptions.Exception_Occurrence);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<procedure> @AdaSubDefn{Set_Dependents_Fallback_Handler}
     (Handler: @key<in> Termination_Handler);
   @key<function> @AdaSubDefn{Current_Task_Fallback_Handler} @key<return> Termination_Handler;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key<procedure> @AdaSubDefn{Set_Specific_Handler}
     (T       : @key<in> Ada.Task_Identification.Task_Id;
      Handler : @key<in> Termination_Handler);
   @key<function> @AdaSubDefn{Specific_Handler} (T : Ada.Task_Identification.Task_Id)
      @key<return> Termination_Handler;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key<end> Ada.Task_Termination;]}
@end{Example}

@end{StaticSem}

@begin{RunTime}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00266-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0202-1]}
@ChgAdded{Version=[2],Text=[@Defn{termination handler}@Defn2{Term=[handler],Sec=[termination]}
The type Termination_Handler identifies a protected
procedure to be executed by the implementation when a task terminates. Such a
protected procedure is called a @i<handler>. In all cases T identifies the task
that is terminating. If the task terminates due to completing the last
statement of its body, or as a result of waiting on a terminate alternative,
@Chg{Version=[3],New=[and the finalization of the task completes normally, ],Old=[]}then
Cause is set to Normal and X is set to Null_Occurrence. If the task
terminates because it is being aborted, then Cause is set to
Abnormal@Chg{Version=[3],New=[;],Old=[ and]} X is set to
Null_Occurrence@Chg{Version=[3],New=[ if the finalization of the task completes normally],Old=[]}.
If the task terminates because of an exception raised
by the execution of its @nt{task_body}, then Cause is set to
Unhandled_Exception@Chg{Version=[3],New=[;],Old=[ and]} X is set to the
associated exception occurrence@Chg{Version=[3],New=[ if the finalization
of the task completes normally],Old=[]}.@Chg{Version=[3],New=[ Independent of
how the task completes, if finalization of the task propagates an exception,
then Cause is either Unhandled_Exception or Abnormal, and X is an exception
occurrence that identifies the Program_Error exception.],Old=[]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00266-02]}
@ChgAdded{Version=[2],Text=[@Defn{fall-back handler}@Defn2{Term=[termination handler],Sec=[fall-back]}
@Defn{specific handler}@Defn2{Term=[termination handler],Sec=[specific]}
@Defn2{Term=[set],Sec=[termination handler]}
@Defn2{Term=[cleared],Sec=[termination handler]}
Each task has two termination handlers, a @i<fall-back handler> and a
@i<specific handler>. The specific handler applies only to the task itself,
while the fall-back handler applies only to the dependent tasks of the task.
A handler is said to be @i<set> if it is associated
with a nonnull value of type Termination_Handler, and @i<cleared> otherwise.
When a task is created, its specific handler and fall-back handler are cleared.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00266-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The procedure Set_Dependents_Fallback_Handler
changes the fall-back handler for the calling
task@Chg{Version=[3],New=[:],Old=[;]} if Handler is @key{null},
that fall-back handler is cleared@Chg{Version=[3],New=[;],Old=[,]}
otherwise@Chg{Version=[3],New=[,],Old=[]} it is set to be Handler.@key{all}.
If a fall-back handler had previously been set it is replaced.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00266-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The function Current_Task_Fallback_Handler returns
the fall-back handler that is currently set for the calling task, if one is
set; otherwise@Chg{Version=[3],New=[,],Old=[]} it returns @key{null}.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00266-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The procedure Set_Specific_Handler changes the
specific handler for the task identified by
T@Chg{Version=[3],New=[:],Old=[;]} if Handler is @key{null}, that
specific handler is cleared@Chg{Version=[3],New=[;],Old=[,]}
otherwise@Chg{Version=[3],New=[,],Old=[]} it is set to be Handler.@key{all}. If
a specific handler had previously been set it is replaced.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[3],Text=[This package cannot portably be used to set
  a handler on the program as a whole. It is possible to call
  Set_Specific_Handler with the environment task's ID. But any call to the
  handler would necessarily be a @BoundedName, as the handler is called
  after the task's finalization has completed. In the case of the environment
  task, that includes any possible protected objects, and calling a protected
  object after it is finalized is a @BoundedName
  (see @RefSecNum{Protected Units and Protected Objects}). This might work in
  a particular implementation, but it cannot be depended upon.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00266-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[The function Specific_Handler returns the specific
handler that is currently set for the task identified by T, if one is set;
otherwise@Chg{Version=[3],New=[,],Old=[]} it returns @key{null}.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00266-02]}
@ChgAdded{Version=[2],Text=[As part of the finalization of a @nt{task_body},
after performing the actions specified in
@RefSecNum{Assignment and Finalization} for finalization of a
master, the specific handler for the task, if one is set, is executed.
If the specific handler is cleared, a search
for a fall-back handler proceeds by recursively following the master
relationship for the task. If a task is found whose fall-back handler is set,
that handler is executed; otherwise, no handler is executed.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00266-02]}
@ChgAdded{Version=[2],Text=[For Set_Specific_Handler or
Specific_Handler, Tasking_Error is raised if the task identified by T has
already terminated. Program_Error is raised if the value of T is
Ada.Task_Identification.Null_Task_Id.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00266-02]}
@ChgAdded{Version=[2],Text=[An exception propagated from a handler that is
invoked as part of the termination of a task has no effect.]}

@end{RunTime}

@begin{Erron}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00266-02]}
@ChgAdded{Version=[2],Text=[For a call of Set_Specific_Handler or
Specific_Handler, if the task identified by T no longer exists, the execution
of the program is erroneous.]}
@end{Erron}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00266-02]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Package Task_Termination is new.]}
@end{Extend95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0202-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Specified what is passed to the
  handler if the finalization of the task fails after it is completed. This was
  not specified at all in Ada 2005, so there is a possibility that some program
  depended on some other behavior of an implementation. But as this case is
  very unlikely (and only occurs when there is already a significant bug in
  the program - so should not occur in fielded systems), we're not listing
  this as an inconsistency.]}
@end{DiffWord2005}
