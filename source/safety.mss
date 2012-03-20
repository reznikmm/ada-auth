@Comment{ $Source: e:\\cvsroot/ARM/Source/safety.mss,v $ }
@Comment{ $Revision: 1.58 $ $Date: 2012/03/20 06:13:59 $ $Author: randy $ }
@Part(safety, Root="ada.mss")

@Comment{$Date: 2012/03/20 06:13:59 $}
@LabeledRevisedNormativeAnnex{Version=[2],
New=[High Integrity Systems], Old=[Safety and Security]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00347-01]}
@Leading@Defn{safety-critical systems}
@Defn{secure systems}
This Annex addresses requirements for @Chg{Version=[2],
New=[high integrity ],Old=[]}systems @Chg{Version=[2],New=[(including],
Old=[that are]} safety@Chg{Version=[2],New=[-],Old=[ ]}critical
@Chg{Version=[2],New=[systems and],Old=[or have]}
security@Chg{Version=[2],New=[-critical systems)],Old=[ constraints]}. It
provides facilities and specifies documentation requirements that relate to
several needs:
@begin{Itemize}
Understanding program execution;

Reviewing object code;

Restricting language constructs whose usage might
complicate the demonstration of program correctness
@end{Itemize}
@ChgNote{The following paragraph is missing a number in the original version.
To give it a number in the new version, it is marked as an insertion.}
@ChgRef{Version=[0],Kind=[Added]}
@Chg{New=[],Old=[@Noparanum@;]}Execution understandability
is supported by pragma Normalize_Scalars, and also by
requirements for the implementation to document the effect of a program
in the presence of a bounded error or where the language rules leave
the effect unspecified.
@PDefn{unspecified}

The @nt[pragma]s Reviewable and Restrictions relate to the other
requirements addressed by this Annex.
@end{Intro}

@begin{Notes}
The @attr[Valid] attribute (see @RefSecNum(The Valid Attribute)) is
also useful in addressing these needs,
to avoid problems that could otherwise arise from scalars
that have values outside their declared range constraints.
@begin{Discussion}

The Annex tries to provide high assurance rather than language features.
However, it is not possible, in general, to test for high assurance. For any
specific language feature, it is possible to demonstrate its presence by a
functional test, as in the ACVC. One can also check for the presence of some
documentation requirements, but it is not easy to determine objectively that
the documentation is @lquotes@;adequate@rquotes@;.

@end{Discussion}
@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
This Annex is new to Ada 95.
@end{Extend83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00347-01]}
  @ChgAdded{Version=[2],Text=[The title of this annex was changed to better
  reflect its purpose and scope. High integrity systems has become the standard
  way of identifying systems that have high reliability requirements; it
  subsumes terms such as safety and security. Moreover, the annex does not
  include any security specific features and as such the previous title is
  somewhat misleading.]}
@end{DiffWord95}

@LabeledClause{Pragma Normalize_Scalars}
@begin{Intro}
This pragma ensures that an otherwise
uninitialized scalar object is set to a
predictable value, but out of range if possible.
@begin[discussion]
The goal of the pragma is to reduce the impact of a bounded error
that results from a reference to an uninitialized scalar object, by
having such a reference violate a range check and thus raise
Constraint_Error.
@end[discussion]
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
@Leading@Keepnext@;The form of a @nt{pragma} Normalize_Scalars is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Normalize_Scalars);'
@end{Syntax}

@begin{LinkTime}
@PDefn2{Term=[configuration pragma], Sec=(Normalize_Scalars)}
@PDefn2{Term=[pragma, configuration], Sec=(Normalize_Scalars)}
Pragma Normalize_Scalars is a configuration pragma.
It applies to all
@nt[compilation_unit]s included in a partition.
@end{LinkTime}

@begin{DocReq}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
If a @nt{pragma} Normalize_Scalars applies,
the implementation shall document the implicit initial @Chg{Version=[2],
New=[values],Old=[value]} for scalar subtypes,
and shall identify each case in which such a value is used
and is not an invalid representation.
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[If
a @nt{pragma} Normalize_Scalars applies, the implicit initial values of
scalar subtypes shall be documented. Such a value should be an
invalid representation when possible; any cases when is it not shall be
documented.]}]}
@begin{Honest}
It's slightly inaccurate to say that the value is a
representation, but the point should be clear anyway.
@end{Honest}
@begin{Discussion}

By providing a type with a size specification so that spare bits are
present, it is possible to force an implementation of Normalize_Scalars to use
an out of range value. This can be tested for by ensuring that
Constraint_Error is raised. Similarly, for an unconstrained integer type, in
which no spare bit is surely present, one can check that the initialization
takes place to the value specified in the documentation of the
implementation. For a floating point type, spare bits might not
be available, but a range constraint can provide the ability to use an out
of range value.

If it is difficult to document the general rule for the implicit initial
value, the implementation might choose instead to record the value on
the object code listing or similar output produced during compilation.

@end{Discussion}
@end{DocReq}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
Whenever possible, the implicit initial @Chg{Version=[2],
New=[values],Old=[value]} for a scalar subtype
should be an invalid representation
(see @RefSecNum{Data Validity}).
@begin{Discussion}

  When an out of range value is used for the initialization,
  it is likely that constraint checks will detect it.
  In addition, it can be detected by the Valid attribute.

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This rule is included in the documentation
  requirements, and thus does not need a separate summary item.]}

@end{Discussion}
@end{ImplAdvice}

@begin{Notes}
The initialization requirement applies to
uninitialized scalar objects that are subcomponents of composite
objects, to allocated objects, and to stand-alone objects. It also
applies to scalar @key{out} parameters. Scalar
subcomponents of composite @key{out} parameters are initialized to the
corresponding part of the actual, by virtue of
@RefSecNum(Parameter Associations).

The initialization requirement does not apply to a scalar for which
pragma Import has been specified,
since initialization of an imported object is performed
solely by the foreign language environment
 (see @RefSecNum[Interfacing Aspects]).

The use of pragma Normalize_Scalars in conjunction with
Pragma Restrictions(No_Exceptions) may result in erroneous execution
(see @RefSecNum[High Integrity Restrictions]).
@begin{Discussion}

Since the effect of an access to an out of range value will often be to
raise Constraint_Error, it is clear that suppressing the exception mechanism
could result in erroneous execution. In particular, the assignment to an
array, with the array index out of range, will result in a write to an
arbitrary store location, having unpredictable effects.

@end{Discussion}
@end{Notes}

@LabeledClause{Documentation of Implementation Decisions}

@begin{DocReq}
@PDefn{unspecified}
The implementation shall document the range of effects for each
situation that the language rules identify as either a
bounded error or as having an unspecified effect.
If the implementation can constrain the effects of erroneous
execution for a given construct,
then it shall document such constraints.
@Redundant[The documentation might be provided either
independently of any compilation unit or partition, or as part of an annotated
listing for a given unit or partition.
See also @RefSecNum(Conformity of an Implementation with the Standard), and
@RefSecNum(Structure).]
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Information regarding bounded errors and erroneous execution.]}]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[
The range of effects for each bounded error and each unspecified effect.
If the effects of a given erroneous construct are
constrained, the constraints shall be documented.]}]}

@end{DocReq}

@begin{Notes}
Among the situations to be
documented are the conventions
chosen for parameter passing, the methods used for the management of
run-time storage, and the method used to evaluate numeric expressions if
this involves extended range or extra precision.
@begin{Discussion}

Look up @lquotes@;unspecified@rquotes@; and @lquotes@;erroneous execution@rquotes@;
in the index for a list of the cases.

The management of run-time storage is particularly important. For safety
applications, it is often necessary to show that a program cannot raise
Storage_Error, and for security applications that information cannot leak
via the run-time system. Users are likely to prefer a simple storage model
that can be easily validated.

The documentation could helpfully take into account that users may well
adopt a subset to avoid some forms of erroneous execution, for instance, not
using the abort statement, so that the effects of a partly completed
@nt{assignment_statement} do not have to be considered in the validation of a
program
(see @RefSecNum{Abort of a Task - Abort of a Sequence of Statements}).
For this reason documentation linked to an actual compilation may be
most useful. Similarly, an implementation may be able to take into
account use of the Restrictions pragma.

@end{Discussion}
@end{Notes}


@LabeledClause{Reviewable Object Code}

@begin{Intro}
Object code review and validation are supported by
pragmas Reviewable and Inspection_Point.
@end{Intro}

@LabeledSubClause{Pragma Reviewable}
@begin{Intro}
This pragma directs the implementation to
provide information to
facilitate analysis and review of a program's
object code, in particular to allow determination of
 execution time and storage usage and to identify the
correspondence between the source and object programs.
@begin{Discussion}

Since the purpose of this pragma is to provide information to the user,
it is hard to objectively test for conformity. In practice, users want
the information in an easily understood and convenient form, but neither
of these properties can be easily measured.

@end{Discussion}
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
@Leading@Keepnext@;The form of a @nt{pragma} Reviewable is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Reviewable);'
@end{Syntax}

@begin{LinkTime}
@PDefn2{Term=[configuration pragma], Sec=(Reviewable)}
@PDefn2{Term=[pragma, configuration], Sec=(Reviewable)}
Pragma Reviewable is a configuration pragma.
It applies to all
@nt[compilation_unit]s included in a partition.
@end{LinkTime}

@begin{ImplReq}
@Leading@;The implementation shall provide the following
information for any compilation unit to which such a
pragma applies:
@begin{Discussion}

The list of requirements can be checked for, even if issues like
intelligibility are not addressed.

@end{Discussion}
@begin{itemize}
Where compiler-generated run-time checks remain;
@begin{Discussion}

A constraint check which is implemented via a check on the upper and lower
bound should clearly be indicated. If a check is implicit in the form of
machine instructions used (such an overflow checking), this should also be
covered by the documentation. It is particularly important to cover those
checks which are not obvious from the source code, such as that for stack
overflow.

@end{Discussion}

An identification of any construct with a language-defined check
that is recognized prior to run time as certain to fail
if executed
(even if the generation of run-time checks has been suppressed);
@begin{Discussion}

In this case, if the compiler determines that a check must fail, the user
should be informed of this. However, since it is not in general possible to
know what the compiler will detect, it is not easy to test for this. In
practice, it is thought that compilers claiming conformity to this Annex
will perform significant optimizations and therefore @i{will} detect such
situations. Of course, such events could well indicate a programmer error.

@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00209-01]}
For each @Chg{Version=[2],New=[read of],Old=[reference to]} a scalar object,
an identification of the @Chg{Version=[2],New=[read],Old=[reference]} as
either @lquotes@;known to be initialized,@rquotes@; or
@lquotes@;possibly uninitialized,@rquotes@;
independent of whether pragma Normalize_Scalars applies;
@begin{Discussion}

This issue again raises the question as to what the compiler has determined.
A lazy implementation could clearly mark all scalars as @lquotes@;possibly
uninitialized@rquotes@;, but this would be very unhelpful to the user. It should be
possible to analyze a range of scalar uses and note the percentage in each
class. Note that an access marked @lquotes@;known to be initialized@rquotes@; does not imply
that the value is in range, since the initialization could be from an
(erroneous) call of unchecked conversion, or by means external to the Ada
program.

@end{Discussion}

Where run-time support routines are implicitly invoked;
@begin{Discussion}

Validators will need to know the calls invoked in order to check for the
correct functionality. For instance, for some safety applications, it may be
necessary to ensure that certain sections of code can execute in a
particular time.

@end{Discussion}

@Leading@Keepnext@;An object code listing, including:
@begin{inneritemize}
Machine instructions, with relative offsets;
@begin{Discussion}

The machine instructions should be in a format that is easily understood,
such as the symbolic format of the assembler. The relative offsets are
needed in numeric format, to check any alignment restrictions that the
architecture might impose.

@end{Discussion}

Where each data object is stored during its lifetime;
@begin{Discussion}

This requirement implies that if the optimizer assigns a variable to a
register, this needs to be evident.

@end{Discussion}

Correspondence with the source program, including an identification of the
 code produced per declaration and per statement.
@begin{Discussion}

This correspondence will be quite complex when extensive optimization is
performed. In particular, address calculation to access some data structures
could be moved from the actual access. However, when all the machine code
arising from a statement or declaration is in one basic block, this must be
indicated by the implementation.

@end{Discussion}
@end{inneritemize}

An identification of each construct for which the implementation detects
the possibility of erroneous execution;
@begin{Discussion}

This requirement is quite vague. In general, it is hard for compilers to detect
erroneous execution and therefore the requirement will be rarely invoked. However,
if the pragma Suppress is used and the compiler can show that a predefined
exception will be raised, then such an identification would be useful.

@end{Discussion}

@Trailing@;For each subprogram, block, task,
or other construct implemented by
reserving and subsequently freeing an area on a run-time stack,
an identification
of the length of the fixed-size portion of the area and an indication
of whether the non-fixed size portion is reserved on the stack
or in a dynamically-managed storage region.
@begin{Discussion}
This requirement is vital for those requiring to show that the storage
available to a program is sufficient. This is crucial in those cases in
which the internal checks for stack overflow are suppressed (perhaps by
@key[pragma] Restrictions(No_Exceptions)).
@end{Discussion}
@end{itemize}

@Leading@;The implementation shall provide the following
information for any partition to which the
pragma applies:
@begin{Itemize}
An object code listing of the entire partition, including
initialization and finalization code as well as
 run-time
system components, and with an identification of those instructions and
data that will be relocated at load time;
@begin{Discussion}
The object code listing should enable a validator to estimate
upper bounds for the time taken by critical parts of a program.
Similarly, by an analysis of the entire partition, it should be possible
to ensure that the storage requirements are suitably bounded,
assuming that the partition was written in an appropriate
manner.@end{discussion}

A description of the run-time model relevant to the partition.
@begin{Discussion}
For example,
a description of the storage model is vital,
since the Ada language does not explicitly define such a model.
@end{Discussion}
@end{itemize}
@end{ImplReq}

@Chgnote{The following paragraph is missing a number in the original version.
To give it a number in the new version, it is marked as an insertion.}
@ChgRef{Version=[0],Kind=[Added]}
@Chg{New=[],Old=[@Noparanum@;]}The implementation shall provide control- and data-flow
information, both within each compilation unit and across
the compilation units of the partition.
@begin{Discussion}

This requirement is quite vague, since it is unclear what control and data
flow information the compiler has produced. It is really a plea not to throw
away information that could be useful to the validator. Note that the data
flow information is relevant to the detection of @lquotes@;possibly uninitialized@rquotes@;
objects referred to above.

@end{Discussion}
@begin{ImplAdvice}
The implementation should provide the above
information in both a human-readable
and machine-readable form,
and should document the latter so as to ease further
processing by automated tools.
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The information produced by @nt{pragma} Reviewable should be
provided in both a human-readable and machine-readable form, and the
latter form should be documented.]}]}

Object code listings should be provided both in a symbolic
format and also in an appropriate numeric format (such as
hexadecimal or octal).
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Object code listings should be provided both in a symbolic
format and in a numeric format.]}]}
@begin{Reason}

This is to enable other tools to perform any analysis that the user
needed to aid validation.
The format should be in some agreed form.

@end{Reason}
@end{ImplAdvice}

@begin{Notes}
The order of elaboration of library units will be documented
even in the absence of @nt[pragma] Reviewable
(see @RefSecNum{Program Execution}).
@end{Notes}

@begin[discussion]
There might be some interactions between pragma Reviewable and compiler
optimizations. For example,
an implementation may disable some
optimizations when pragma Reviewable is in force
if it would be overly complicated to
provide
the detailed information to allow review of the optimized object code.
See also @nt<pragma> Optimize (@RefSecNum{Pragmas}).
@end[discussion]

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00209-01]}
  @ChgAdded{Version=[2],Text=[The wording was clarified that pragma Reviewable
  applies to each read of an object, as it makes no sense to talk about the
  state of an object that will immediately be overwritten.]}
@end{DiffWord95}

@LabeledSubClause{Pragma Inspection_Point}
@begin{Intro}
An occurrence of a pragma Inspection_Point identifies a set of objects each of
whose values is to be
available at the point(s) during program execution corresponding to the
position of the pragma in the compilation unit.
The purpose of such a pragma is to facilitate code validation.
@begin{Discussion}

Inspection points are a high level equivalent of break points used by
debuggers.

@end{Discussion}
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
@Leading@Keepnext@;The form of a @nt{pragma} Inspection_Point is as follows:
@end{SyntaxText}

@PragmaSyn`@key{pragma} @prag(Inspection_Point)[(@SynI{object_}@Syn2{name} {, @SynI{object_}@Syn2{name}})];'
@end{Syntax}

@begin{Legality}
A pragma Inspection_Point is allowed wherever a
@nt[declarative_item]
or @nt[statement] is allowed.
Each @SynI{object_}name shall statically denote the declaration of an object.
@begin{Discussion}

The static denotation is required, since no dynamic evaluation of a name
is involved in this pragma.

@end{Discussion}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0093],ARef=[AI95-00207-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
@Defn{inspection point}
An @i{inspection point} is a point in the object code
corresponding to the occurrence of a pragma Inspection_@!Point in the
compilation unit.
@Defn{inspectable object}
An object is @i{inspectable} at an inspection point if the corresponding
pragma Inspection_Point either has an argument denoting that object,
or has no arguments@Chg{New=[ and the @Chg{Version=[2],New=[declaration
of the ],Old=[]}object is visible at the inspection point],Old=[]}.
@begin{ramification}
If a pragma Inspection_Point is in an in-lined subprogram, there
might be numerous inspection points in the object code corresponding to
the single occurrence of the pragma in the source; similar considerations
apply if such a pragma is in a generic, or in a loop that has
been @lquotes@;unrolled@rquotes@; by an optimizer.

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0093],ARef=[AI95-00207-01]}
@ChgAdded{Version=[1],Text=[The short form of the pragma is a convenient shorthand for
listing all objects which could be explicitly made inspectable by the long
form of the pragma; thus only visible objects are made inspectable by it.
Objects that are not visible at the point of the pragma are not made
inspectable by the short form pragma. This is necessary so that implementations
need not keep information about (or prevent optimizations on) a unit simply
because some other unit @i<might> contain a short form Inspection_Point
pragma.]}
@end{ramification}
@begin{Discussion}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0093],ARef=[AI95-00207-01]}
If the short form of the pragma is used, then all@Chg{New=[ visible],Old=[]}
objects are inspectable.
This implies that @Chg{New=[global objects from other compilation units],
Old=[objects out of scope at the point of the pragma]} are inspectable. A good
interactive debugging system could provide information
similar to a post-mortem dump at such inspection points. The annex does
not require that any inspection facility is provided, merely that the
information is available to understand the state of the machine at those
points.
@end{Discussion}
@end{StaticSem}

@begin{RunTime}
Execution of a pragma Inspection_Point has no effect.
@begin{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
Although an inspection point has no (semantic) effect,
the removal or adding @Chg{Version=[2],New=[of ],Old=[]}a new point could
change the machine code generated by the compiler.

@end{Discussion}
@end{RunTime}

@begin{ImplReq}
Reaching an inspection point is an external interaction
with respect to the values
of the inspectable objects at that point
(see @RefSecNum{Conformity of an Implementation with the Standard}).
@begin{Ramification}
The compiler is inhibited from moving an assignment to
an inspectable variable past an inspection point for that variable.
On the other hand, the evaluation of an expression that might raise
an exception may be moved past an inspection point
(see @RefSecNum[Exceptions and Optimization]).@end{ramification}
@end{ImplReq}

@begin{DocReq}
For each inspection point,
 the implementation shall
identify a mapping between each inspectable object and the machine resources
(such as memory locations or registers) from which the object's value
can be obtained.
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Implementation-defined aspects of pragma Inspection_Point.]}]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[For each inspection point, a mapping between each inspectable object
and the machine resources where the object's value can be obtained shall
be provided.]}]}
@end{DocReq}

@begin{Notes}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00209-01]}
The implementation is not allowed to perform @lquotes@;dead store elimination@rquotes@; on
the last assignment to a variable prior to a point where the
variable is inspectable.
Thus an inspection point has the effect of an
implicit @Chg{Version=[2],New=[read of],Old=[reference to]} each of its
inspectable objects.

Inspection points are useful in maintaining a correspondence between the
state of the program in source code terms, and the machine state during
the program's execution. Assertions about the values of program objects
can be tested in machine terms at inspection points. Object code between
inspection points can be processed by automated tools to
verify programs mechanically.
@begin{Discussion}

Although it is not a requirement of the annex, it would be useful if the
state of the stack and heap could be interrogated. This would allow users
to check that a program did not have a `storage leak'.

@end{Discussion}

The identification of the mapping from source program objects to machine
resources is allowed to be in the form of an
annotated object listing, in human-readable or tool-processable form.
@begin{Discussion}

In principle, it is easy to check an implementation for this pragma, since
one merely needs to check the content of objects against those values
known from the source listing. In practice, one needs a tool similar
to an interactive debugger to perform the check.

@end{Discussion}
@end{Notes}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0093],ARef=[AI95-00207-01]}
@ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the definition of
the Inspection_Point pragma to apply to only variables visible at the point
of the pragma. Otherwise, the compiler would have to assume that some
other code somewhere could have a pragma Inspection_Point, preventing many
optimizations (such as unused object elimination).]}
@end{DiffWord95}


@LabeledRevisedClause{Version=[2],New=[High Integrity Restrictions],Old=[Safety and Security Restrictions]}
@begin{Intro}
This clause defines restrictions that can be used with pragma
Restrictions (see @RefSecNum(Pragma Restrictions and Pragma Profile)); these
facilitate the demonstration of program correctness by allowing
tailored versions of the run-time system.
@begin{Discussion}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
Note that the restrictions are absolute. If a partition has 100 library
units and just one needs Unchecked_Conversion, then the pragma cannot be
used to ensure the other 99 units do not use Unchecked_Conversion. Note also
that these are restrictions on all Ada code within a partition, and
therefore it @Chg{Version=[3],New=[might],Old=[may]} not be evident
from the specification of a package whether a restriction can be imposed.

@end{Discussion}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00347-01],ARef=[AI95-00394-01]}
@ChgDeleted{Version=[2],Text=[The following restrictions,
the same as in @RefSecNum{Tasking Restrictions},
apply in this Annex:
 No_Task_@!Hierarchy,
 No_Abort_@!Statement,
 No_Implicit_@!Heap_@!Allocation,
 Max_@!Task_@!Entries is 0,
 Max_@!Asynchronous_@!Select_@!Nesting is 0, and
 Max_@!Tasks is 0.
@redundant[The last three restrictions are checked prior to program execution.]]}

@Trailing@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00394-01]}
The following @Chg{Version=[2],New=[@SynI{restriction_}@nt{identifier}s are
language defined:],Old=[additional restrictions apply in this Annex.]}

@leading@keepnext@b{Tasking-related restriction:}

@begin{Description}
@Trailing@;@Defn2{Term=[restrictions],Sec=(No_Protected_Types)}@Chg{Version=[3],New=[@Defn{No_Protected_Types restriction}],
Old=[]}No_Protected_Types @\There
are no declarations of protected types or protected objects.
@end{Description}

@leading@keepnext@b{Memory-management related restrictions:}

@begin{Description}
@Defn2{Term=[restrictions],Sec=(No_Allocators)}@Chg{Version=[3],New=[@Defn{No_Allocators restriction}],
   Old=[]}No_Allocators @\There are no
occurrences of an @nt{allocator}.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0042],ARef=[AI95-00130]}
@Defn2{Term=[restrictions],Sec=(No_Local_Allocators)}@Chg{Version=[3],New=[@Defn{No_Local_Allocators restriction}],
Old=[]}No_Local_Allocators @\@nt{Allocator}s
are prohibited in subprograms, generic subprograms,
tasks, and entry bodies@Chg{New=[],Old=[; instantiations of generic packages
are also prohibited in these contexts]}.
@begin[Ramification]
Thus @nt{allocator}s are permitted only in expressions whose
evaluation can only be performed before the main subprogram is invoked.
@end[Ramification]
@begin[Reason]
@ChgRef{Version=[1],Kind=[Deleted],Ref=[8652/0042],ARef=[AI95-00130]}
@ChgNote{The associated rule has been deleted.}
@ChgDeleted{Version=[1],Text=[The reason for the prohibition against
instantiations of
generic packages is to avoid contract model violations.
An alternative would be to prohibit @nt{allocator}s from generic
packages, but it seems preferable to allow generality on the
defining side and then place the restrictions on the usage (instantiation),
rather than inhibiting what can be in the generic while
liberalizing where they can be instantiated.]}
@end[Reason]

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0152-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[restrictions],Sec=(No_Anonymous_Allocators)}@Defn{No_Anonymous_Allocators restriction}
No_Anonymous_Allocators @\There are no @nt{allocator}s of anonymous access types.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0190-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[restrictions],Sec=(No_Coextensions)}@Defn{No_Coextensions restriction}
No_Coextensions @\There are no coextensions. See @RefSecNum{Operations of Access Types}.]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0190-1]}
@ChgAdded{Version=[3],Text=[@Defn2{Term=[restrictions],
Sec=(No_Access_Parameter_Allocators)}@Defn{No_Access_Parameter_Allocators restriction}
No_Access_Parameter_Allocators @\@nt{Allocator}s
are not permitted as the actual parameter to an access parameter.
See @RefSecNum{Subprogram Declarations}.]}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00394-01]}
@Comment{We can't reuse this paragraph for the above, because there is no
way to have the hanging marker in both the added and deleted text (and since it
isn't in Ada 2005 at all).}
@ChgDeleted{Version=[2],Text=[@Defn2{Term=[restrictions],
Sec=(No_Unchecked_Deallocation)}No_Unchecked_Deallocation @\Semantic
dependence on Unchecked_Deallocation is not allowed.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[2],Text=[This restriction would be useful in those
  contexts in which heap storage is needed on program start-up, but need not be
  increased subsequently. The danger of a dangling pointer can therefore be
  avoided.]}
@end{Discussion}

@Trailing@;Immediate_Reclamation @\Except for storage occupied by objects created by
@nt{allocator}s and not deallocated via unchecked deallocation, any storage
reserved at run time for an object is immediately reclaimed when the
object no longer exists.
@Defn2{Term=[restrictions],Sec=(Immediate_Reclamation)}@Chg{Version=[3],New=[@Defn{Immediate_Reclamation restriction}],
Old=[]}
@begin{Discussion}
Immediate reclamation would apply to storage created by the compiler, such
as for a return value from a function whose size is not known at the
call site.
@end{Discussion}
@end{Description}

@leading@keepnext@b{Exception-related restriction:}

@begin{Description}
@Trailing@Defn2{Term=[restrictions],Sec=(No_Exceptions)}@Chg{Version=[3],New=[@Defn{No_Exceptions restriction}],
   Old=[]}No_Exceptions @\@nt{Raise_statement}s
and @nt{exception_handler}s are not allowed.
No language-defined run-time checks are generated;
however, a run-time check performed automatically by the hardware
is permitted.
@begin{Discussion}

This restriction mirrors a method of working that is quite common in the
safety area. The programmer is required to show that exceptions cannot be
raised. Then a simplified run-time system is used without exception
handling. However, some hardware checks may still be enforced. If the
software check would have failed, or if the hardware check actually fails,
then the execution of the program is unpredictable.
There are obvious dangers in this approach, but it is similar to
programming at the assembler level.

@end{Discussion}
@end{Description}

@leading@keepnext@b{Other restrictions:}

@begin{Description}
@Defn2{Term=[restrictions],Sec=(No_Floating_Point)}@Chg{Version=[3],New=[@Defn{No_Floating_Point restriction}],
   Old=[]}No_Floating_Point @\Uses of predefined floating point types and
operations, and declarations of new floating point types, are
not allowed.
@begin{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
The intention is to avoid the use of floating point hardware at run time,
but this is expressed in
language terms. It is conceivable that floating point is used implicitly in
some contexts, say fixed point type conversions of high accuracy. However,
the @ImplReqTitle below make it clear that the restriction would apply
to the @lquotes@;run-time system@rquotes@; and hence not be allowed.
This @Chg{Version=[2],New=[restriction],Old=[parameter]} could be used to
inform a compiler that a variant of the architecture is being used which does
not have floating point instructions.

@end{Discussion}

@Defn2{Term=[restrictions],Sec=(No_Fixed_Point)}@Chg{Version=[3],New=[@Defn{No_Fixed_Point restriction}],
   Old=[]}No_Fixed_Point @\Uses of predefined fixed point types and
operations, and declarations of new fixed point types, are
not allowed.
@begin{Discussion}

This restriction would have the side effect of prohibiting the
@nt{delay_relative_statement}.
As with the No_Floating_Point restriction, this might be used to
avoid any question of rounding errors. Unless an Ada run-time is written in
Ada, it seems hard to rule out implicit use of fixed point, since at the
machine level, fixed point is virtually the same as integer arithmetic.

@end{Discussion}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00394-01]}
@ChgDeleted{Version=[2],Text=[@Defn2{Term=[restrictions],
Sec=(No_Unchecked_Conversion)}No_Unchecked_Conversion @\Semantic dependence
on the predefined generic Unchecked_Conversion is not allowed.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[2],Text=[Most critical applications would require
  some restrictions or additional validation checks on uses of unchecked
  conversion. If the application does not require the functionality, then
  this restriction provides a means of ensuring the design requirement has
  been satisfied. The same applies to several of the following restrictions.]}
@end{Discussion}

No_Access_Subprograms @\The declaration of access-to-subprogram types
is not allowed.
@Defn2{Term=[restrictions],Sec=(No_Access_Subprograms)}@Chg{Version=[3],New=[@Defn{No_Access_Subprograms restriction}],
Old=[]}
@begin{Discussion}@ChgNote{Moved from above}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[Most critical applications would require
  some restrictions or additional validation checks on uses of
  access-to-subprogram types. If the application does not require the
  functionality, then this restriction provides a means of ensuring the design
  requirement has been satisfied. The same applies to several of the following
  restrictions, and to restriction No_Dependence => Ada.Unchecked_Conversion.]}
@end{Discussion}

@Defn2{Term=[restrictions],Sec=(No_Unchecked_Access)}@Chg{Version=[3],New=[@Defn{No_Unchecked_Access restriction}],
   Old=[]}No_Unchecked_Access @\The @attr[Unchecked_Access] attribute
is not allowed.

@Defn2{Term=[restrictions],Sec=(No_Dispatch)}@Chg{Version=[3],New=[@Defn{No_Dispatch restriction}],
Old=[]}No_Dispatch @\Occurrences of T'Class are not allowed, for any
(tagged)
subtype T.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@Defn2{Term=[restrictions],Sec=(No_IO)}@Chg{Version=[3],New=[@Defn{No_IO restriction}],
   Old=[]}No_IO @\Semantic dependence on
any of the library units
Sequential_IO, Direct_IO, Text_IO, Wide_Text_IO, @Chg{Version=[2],
New=[Wide_Wide_Text_IO, ],Old=[]}or Stream_IO is not allowed.
@begin{Discussion}

Excluding the input-output facilities of an implementation may be needed
in those environments which cannot support the supplied functionality.
A program in such an environment is likely to require some low level
facilities or a call on a non-Ada feature.

@end{Discussion}

@Defn2{Term=[restrictions],Sec=(No_Delay)}@Chg{Version=[3],New=[@Defn{No_Delay restriction}],
Old=[]}No_Delay @\@nt[Delay_Statement]s
and semantic dependence on package Calendar
are not allowed.
@begin[Ramification]
This implies that @nt[delay_alternative]s in a
@nt[select_statement] are prohibited.

The purpose of this restriction is to avoid the need for timing facilities
within the run-time system.

@end[ramification]

@Defn2{Term=[restrictions],Sec=(No_Recursion)}@Chg{Version=[3],New=[@Defn{No_Recursion restriction}],
   Old=[]}No_Recursion @\As part of the execution of a subprogram, the same
subprogram is not invoked.

@Defn2{Term=[restrictions],Sec=(No_Reentrancy)}@Chg{Version=[3],New=[@Defn{No_Reentrancy restriction}],
   Old=[]}No_Reentrancy @\During the execution of a subprogram by a task, no other
task invokes the same subprogram.

@end{description}
@end{StaticSem}

@begin{ImplReq}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00394-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[An implementation of this Annex
shall support:]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[the restrictions defined in this subclause; and]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0189-1]}
@ChgAdded{Version=[2],Text=[the following restrictions defined in
@RefSecNum{Tasking Restrictions}: No_Task_Hierarchy,
No_Abort_Statement, No_Implicit_Heap_Allocation@Chg{Version=[3],New=[,
No_Standard_Allocators_After_Elaboration], Old=[]}; and]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00347-01]}
@ChgAdded{Version=[2],Text=[the @key{pragma} Profile(Ravenscar); and]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00347-01]}
  @ChgAdded{Version=[2],Text=[The reference to pragma Profile(Ravenscar) is
  intended to show that properly restricted tasking is appropriate for use
  in high integrity systems. The Ada 95
  Annex seemed to suggest that tasking was inappropriate for such systems.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Type=[Leading],Text=[the following uses of
@SynI{restriction_parameter_}@nt{identifier}s defined in
@RefSecNum{Tasking Restrictions}@Redundant[, which are checked
prior to program execution]:]}
@begin{InnerItemize}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[Max_Task_Entries => 0,]}

  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[Max_Asynchronous_Select_Nesting => 0, and]}

  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[Max_Tasks => 0.]}
@end{InnerItemize}
@end{Itemize}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0263-1],ARef=[AI05-0272-1]}
If an implementation supports @nt[pragma] Restrictions for a particular
argument, then except for the restrictions No_Unchecked_Deallocation,
No_Unchecked_Conversion, No_Access_Subprograms,
@Chg{Version=[3],New=[],Old=[and ]}No_Unchecked_Access,
@Chg{Version=[3],New=[No_Specification_of_Aspect, No_Use_of_Attribute,
No_Use_of_Pragma, and the equivalent use of No_Dependence, ],Old=[]}the
associated restriction applies to the run-time system.
@begin[reason]
Permission is granted for the run-time system to use the specified
otherwise-restricted features, since the use of these features may
simplify the run-time system by allowing more of it to be written
in Ada.
@end[reason]
@begin{Discussion}

The restrictions that are applied to the partition are also applied to the
run-time system. For example, if No_Floating_Point is specified,
then an implementation that uses floating point for implementing the delay
statement (say) would require that No_Floating_Point is
only used in conjunction with No_Delay. It is clearly important that
restrictions are effective so that Max_Tasks=0 does imply that tasking is
not used, even implicitly (for input-output, say).

An implementation of tasking could be produced based upon a run-time
system written in Ada in which the rendezvous was controlled by
protected types. In this case, No_Protected_Types could only be used in
conjunction with Max_Task_Entries=0. Other implementation dependencies
could be envisaged.

If the run-time system is not written in Ada, then the wording needs to be
applied in an appropriate fashion.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0263-1]}
@ChgAdded{Version=[3],Text=["the equivalent use of No_Dependence" refers
to @exam{No_Dependence => Ada.Unchecked_Conversion} and the like, not all
uses of No_Dependence.]}

@end{Discussion}
@end{ImplReq}

@begin{DocReq}
If a pragma Restrictions(No_Exceptions) is specified, the implementation
shall document the effects of all constructs where language-defined checks are
still performed automatically (for example, an overflow check performed
by the processor).
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Implementation-defined aspects of pragma Restrictions.]}]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[
If a pragma Restrictions(No_Exceptions) is specified, the effects of all
constructs where language-defined checks are still performed.]}]}
@begin{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
The documentation requirements here are quite difficult to satisfy. One
method is to review the object code generated and determine the checks that
are still present, either explicitly, or implicitly within the architecture.
As another example from that of overflow, consider the question of
@Chg{Version=[2],New=[dereferencing],Old=[deferencing]} a null pointer.
This could be undertaken by a memory access trap
when checks are performed. When checks are suppressed via the argument
No_Exceptions, it would not be necessary to have the memory access trap
mechanism enabled.

@end{Discussion}
@end{DocReq}

@begin{Erron}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
Program execution is erroneous if pragma Restrictions(No_Exceptions)
has been specified and the conditions arise under which a generated
language-defined run-time check would fail.
@begin{Discussion}

The situation here is very similar to the application of pragma Suppress.
Since users are removing some of the protection the language
provides, they had better be careful!

@end{Discussion}

@PDefn2{Term=(erroneous execution),Sec=(cause)}
Program execution is erroneous if pragma Restrictions(No_Recursion)
has been specified and a subprogram is invoked as part of its
own execution,
or if pragma Restrictions(No_Reentrancy) has been specified and
during the execution of a subprogram by a task, another task invokes
the same subprogram.
@begin{Discussion}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
In practice, many implementations @Chg{Version=[3],New=[might],Old=[may]}
not exploit the absence of recursion
or need for reentrancy, in which case the program execution would be
unaffected by the use of recursion or reentrancy, even though the program is
still formally erroneous.

@end{Discussion}
@end{Erron}

@ChgNote{I can't find any reason in the normative wording for this item;
therefore I've removed it. The notes below (commented out anyway) refer only
to a nonstandard mode, which is irrelevant in the Standard.}
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Any restrictions on pragma Restrictions.]}]}

@begin{Notes}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00394-01]}
@ChgAdded{Version=[2],Text=[Uses of @Syni{restriction_parameter_}@nt{identifier}
No_Dependence defined in @RefSecNum{Language-Defined Restrictions and Profiles}:
No_Dependence => Ada.@!Unchecked_@!Deallocation and No_Dependence =>
Ada.@!Unchecked_@!Conversion may be appropriate for high-integrity systems.
Other uses of No_Dependence can also be appropriate for high-integrity
systems.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The specific mention of these two uses
  is meant to replace the identifiers now banished to
  @RefSec{Dependence Restriction Identifiers}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Restriction No_Dependence => Ada.Unchecked_Deallocation
  would be useful in those
  contexts in which heap storage is needed on program start-up, but need not be
  increased subsequently. The danger of a dangling pointer can therefore be
  avoided.]}@ChgNote{Moved down from above.}
@end{Discussion}

@end{Notes}

@begin{comment}
@begin{Notes}
The standard mode for pragma Restrictions is that a
compilation unit
(or a partition if at link time) is illegal
if it makes use of a feature identified in the pragma.
However, an implementation is allowed to have a mode of operation
where it issues a warning message versus rejecting the unit or partition,
and where the run-time system supplied for the partition
includes support for the identified feature.

An implementation need not independently support the restrictions
identified above, but may instead require that if a particular
restriction is identified in a @nt(pragma) Restrictions, then
other restrictions (the exact set of which is implementation defined)
need also to be identified.

In such a case, if the additional
restrictions are not specified in a @nt(pragma) Restrictions, and
a unit or partition uses such a feature, the implementation should
@begin{itemize}
issue a warning that the pragma is being ignored (that is, the
full RTS is being used),

identify to the user the additional restrictions that need to be
specified in the pragma in order to obtain the reduced RTS, and
identify any constructs that would use any of the features so
proscribed
@end{itemize}
@end{Notes}
@end{comment}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0042],ARef=[AI95-00130-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  No_Local_Allocators no longer prohibits generic instantiations.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[Wide_Wide_Text_IO (which is new) is added to the
  No_IO restriction.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00347-01]}
  @ChgAdded{Version=[2],Text=[The title of this clause was changed to match the
  change to the Annex title. Pragma Profile(Ravenscar) is part of this annex.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00394-01]}
  @ChgAdded{Version=[2],Text=[Restriction No_Dependence is used instead of
  special @i<restriction_>@nt<identifier>s. The old names are banished to
  Obsolescent Features (see @RefSecNum{Dependence Restriction Identifiers}).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00394-01]}
  @ChgAdded{Version=[2],Text=[The bizarre wording @lquotes@;apply in this
  Annex@rquotes (which no one quite can explain the meaning of) is banished.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0152-1],ARef=[AI05-0190-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 95}Restrictions
  No_Anonymous_Allocators, No_Coextensions, and No_Access_Parameter_Allocators
  are new.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0189-1]}
  @ChgAdded{Version=[3],Text=[New restriction
  No_Standard_Allocators_After_Elaboration is added to the list of
  restrictions that are required by this annex.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0263-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Ada 2005 restriction No_Dependence
  is added where needed (this was missed in Ada 2005).]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0272-1]}
  @ChgAdded{Version=[3],Text=[Restrictions against individual aspects, pragmas,
  and attributes do not apply to the run-time system, in order that an
  implementation can use whatever aspects, pragmas, and attributes are needed
  to do the job. For instance, attempting to write a run-time system for Linux
  that does not use the Import aspect would be very difficult and probably is
  not what the user is trying to prevent anyway.]}
@end{DiffWord2005}


@LabeledAddedClause{Version=[2],Name=[Pragma Detect_Blocking]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00305-01]}
@ChgAdded{Version=[2],Text=[The following @nt{pragma} forces an implementation
to detect potentially blocking operations within a protected operation.]}
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00305-01]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[The form of a
@nt{pragma} Detect_Blocking is as follows:]}
@end{SyntaxText}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@AddedPragmaSyn<Version=[2],@ChgAdded{Version=[2],
Text=[@key{pragma} @prag<Detect_Blocking>;]}>
@end{Syntax}

@begin{LinkTime}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00305-01]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=[configuration pragma], Sec=(Detect_Blocking)}
@PDefn2{Term=[pragma, configuration], Sec=(Detect_Blocking)}
A @nt{pragma} Detect_Blocking is a configuration
pragma.]}
@end{LinkTime}

@begin{RunTime}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00305-01]}
@ChgAdded{Version=[2],Text=[An implementation is required to detect a
potentially blocking operation within a protected operation, and to raise
Program_Error (see @RefSecNum{Protected Subprograms and Protected Actions}).]}
@end{RunTime}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00305-01]}
@ChgAdded{Version=[2],Text=[An implementation is allowed to reject a
@nt{compilation_unit} if a potentially blocking operation is present directly
within an @nt{entry_body} or the body of a protected subprogram.]}
@end{ImplPerm}

@begin{Notes}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00305-01]}
@ChgAdded{Version=[2],Text=[An operation that causes a task to be blocked
within a foreign language domain is not defined to be potentially blocking,
and need not be detected.]}
@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00305-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Pragma Detect_Blocking is new.]}
@end{Extend95}

@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
@LabeledAddedClause{Version=[2],Name=[Pragma Partition_Elaboration_Policy]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00265-01]}
@ChgAdded{Version=[2],Text=[This clause defines a @nt{pragma}
for user control over elaboration policy.]}
@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00265-01]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[The form of a
@nt{pragma} Partition_Elaboration_Policy is as follows:]}
@end{SyntaxText}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@AddedPragmaSyn<Version=[2],@ChgAdded{Version=[2],
Text=[@key{pragma} @prag<Partition_Elaboration_Policy> (@SynI<policy_>@Syn2<identifier>);]}>

@begin{SyntaxText}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The @SynI<policy_>@nt{identifier} shall be either
Sequential, Concurrent or an implementation-defined identifier.]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@Chg{Version=[2],New=[Implementation-defined
@SynI<policy_>@nt<identifier>s allowed in a @nt{pragma} Partition_Elaboration_Policy.],Old=[]}]}
@end{SyntaxText}
@end{Syntax}

@begin{LinkTime}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00265-01]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=[configuration pragma], Sec=(Partition_Elaboration_Policy)}
@PDefn2{Term=[pragma, configuration], Sec=(Partition_Elaboration_Policy)}
A @nt{pragma} Partition_Elaboration_Policy is a configuration pragma.
It specifies the elaboration policy for a partition.
At most one elaboration policy shall be specified for a partition.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00265-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[If the Sequential policy is
specified for a partition@Chg{Version=[3],New=[,],Old=[]} then pragma
Restrictions (No_Task_Hierarchy) shall also be specified for the partition.]}
@end{LinkTime}

@begin{RunTime}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00265-01]}
@ChgAdded{Version=[2],Text=[Notwithstanding@Defn{notwithstanding} what this
International Standard
says elsewhere, this @nt{pragma} allows partition elaboration rules concerning
task activation and interrupt attachment to be changed. If the
@SynI{policy_}@nt{identifier} is Concurrent, or if there is no pragma
Partition_Elaboration_Policy defined for the partition, then the rules defined
elsewhere in this Standard apply.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00265-01],ARef=[AI95-00421-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[If the partition elaboration policy
is Sequential, then task activation and interrupt attachment are performed in
the following sequence of steps:]}

@begin{Itemize}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The activation of all library-level tasks and the
  attachment of interrupt handlers are deferred until all library units are
  elaborated.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The interrupt handlers are attached by the
  environment task.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The environment task is suspended while the
  library-level tasks are activated.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The environment task executes the main subprogram
  (if any) concurrently with these executing tasks.]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00265-01],ARef=[AI95-00421-01]}
@ChgAdded{Version=[2],Text=[If several dynamic interrupt handler attachments for
the same interrupt are deferred, then the most recent call of
Attach_Handler or Exchange_Handler determines which handler is attached.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00265-01],ARef=[AI95-00421-01]}
@ChgAdded{Version=[2],Text=[If any deferred task activation fails, Tasking_Error
is raised at the beginning of the sequence of statements of the
body of the environment task prior to calling the
main subprogram.]}

@end{RunTime}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00265-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[If the partition elaboration policy is Sequential
and the Environment task becomes permanently blocked during
elaboration@Chg{Version=[3],New=[,],Old=[]} then the partition is deadlocked and
it is recommended that the partition be immediately terminated.]}
@ChgImplAdvice{Version=[3],Kind=[Revised],InitialVersion=[2],
Text=[@ChgAdded{Version=[2],
Text=[If the partition elaboration policy is Sequential
and the Environment task becomes permanently blocked during
elaboration@Chg{Version=[3],New=[,],Old=[]} then
the partition should be immediately terminated.]}]}

@end{ImplAdvice}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00265-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[If the partition elaboration policy is Sequential
and any task activation fails@Chg{Version=[3],New=[,],Old=[]} then an
implementation may immediately terminate the active partition to mitigate the
hazard posed by continuing to execute with a subset of the tasks being active.]}
@end{ImplPerm}

@begin{Notes}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00421-01]}
@ChgAdded{Version=[2],Text=[If any deferred task activation fails, the
environment task is unable to handle the Tasking_Error exception and completes
immediately. By contrast, if the partition elaboration policy is Concurrent,
then this exception could be handled within a library unit.]}
@end{Notes}


@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00265-01],ARef=[AI95-00421-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @nt{Pragma} Partition_Elaboration_Policy is new.]}
@end{Extend95}




