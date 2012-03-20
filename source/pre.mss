@Part(predef, Root="ada.mss")

@Comment{$Date: 2012/02/19 01:58:38 $}
@LabeledNormativeAnnex{Predefined Language Environment}

@comment{$Source: e:\\cvsroot/ARM/Source/pre.mss,v $}
@comment{$Revision: 1.49 $}
@comment{$RLB: Eliminated includes. $}

@begin{Intro}
@Leading@keepnext
@redundant[@Defn{Language-Defined Library Units}
@Defn{predefined environment}
This Annex contains the specifications of library units that shall be
provided by every implementation.
There are three root library units:
Ada, Interfaces, and System;
other library units are children of these:]

@Leading@Keepnext
@ChgRef{Version=(1),Kind=(Revised),Ref=(8652/0047),ARef=(AI95-00081-01)}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00424-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0001-1],ARef=[AI05-0049-1],ARef=[AI05-0069-1],ARef=[AI05-0111-3],ARef=[AI05-0136-1],ARef=[AI05-0137-1],ARef=[AI05-0166-1],ARef=[AI05-0168-1]}
@ @*@ @;@comment{paragraph number here, paragraph numbers seem to intrude on
the RHS column, misaligning it. Thus we have two lines, as small as possible.}
@begin{Display}
@TabClear{}@TabSet{L2, L4, L6, L8, L10, L12, L14, L16}
@begin{TwoCol}
@Noparanum@redundant[@shrink<Standard @em @RefSecNum{The Package Standard}
@\Ada @em @RefSecNum{The Package Ada}
@Chg{Version=[2],New=(@\@\Assertions @em @RefSecNum{Pragmas Assert and Assertion_Policy}
), Old=()}@\@\Asynchronous_Task_Control @em @RefSecNum{Asynchronous Task Control}
@\@\Calendar @em @RefSecNum{Delay Statements, Duration, and Time}
@Chg{Version=[2],New=(@\@\@\Arithmetic @em @RefSecNum{Formatting, Time Zones, and other operations for Time}
@\@\@\Formatting @em @RefSecNum{Formatting, Time Zones, and other operations for Time}
@\@\@\Time_Zones @em @RefSecNum{Formatting, Time Zones, and other operations for Time}
), Old=()}@\@\Characters @em @RefSecNum{The Packages Characters, Wide_Characters, and Wide_Wide_Characters}
@Chg{Version=[2],New=(@\@\@\Conversions @em @RefSecNum{The Package Characters.Conversions}
), Old=()}@\@\@\Handling @em @RefSecNum{The Package Characters.Handling}
@\@\@\Latin_1 @em @RefSecNum{The Package Characters.Latin_1}
@\@\Command_Line @em @RefSecNum{The Package Command_Line}
@Chg{Version=[2],New=(@\@\Complex_Text_IO @em @RefSecNum{Complex Input-Output}
@\@\Containers @em @RefSecNum{The Package Containers}
@Chg{Version=[3],New=(@\@\@\Bounded_Doubly_Linked_Lists
@\@\@\@\@\@\@em @RefSecNum{The Generic Package Containers.Bounded_Doubly_Linked_Lists}
@\@\@\Bounded_Hashed_Maps @em @RefSecNum{The Generic Package Containers.Bounded_Hashed_Maps}
@\@\@\Bounded_Hashed_Sets @em @RefSecNum{The Generic Package Containers.Bounded_Hashed_Sets}
@\@\@\Bounded_Multiway_Trees @em @RefSecNum{The Generic Package Containers.Bounded_Multiway_Trees}
@\@\@\Bounded_Ordered_Maps @em @RefSecNum{The Generic Package Containers.Bounded_Ordered_Maps}
@\@\@\Bounded_Ordered_Sets @em @RefSecNum{The Generic Package Containers.Bounded_Ordered_Sets}
@\@\@\Bounded_Priority_Queues @em @RefSecNum{The Generic Package Containers.Bounded_Priority_Queues}
@\@\@\Bounded_Synchronized_Queues
@\@\@\@\@\@\ @em @RefSecNum{The Generic Package Containers.Bounded_Synchronized_Queues}
@\@\@\Bounded_Vectors @em @RefSecNum{The Generic Package Containers.Bounded_Vectors}
), Old=()}@\@\@\Doubly_Linked_Lists @em @RefSecNum{The Generic Package Containers.Doubly_Linked_Lists}
@\@\@\Generic_Array_Sort @em @RefSecNum{Array Sorting}
@\@\@\Generic_Constrained_Array_Sort
@\@\@\@\@\@\@em @RefSecNum{Array Sorting}
@Chg{Version=[3],New=(@\@\@\Generic_Sort @em @RefSecNum{Array Sorting}
), Old=()}@\@\@\Hashed_Maps @em @RefSecNum{The Generic Package Containers.Hashed_Maps}
@\@\@\Hashed_Sets @em @RefSecNum{The Generic Package Containers.Hashed_Sets}
@\@\@\Indefinite_Doubly_Linked_Lists
@\@\@\@\@\@\@em @RefSecNum{The Generic Package Containers.Indefinite_Doubly_Linked_Lists}
@\@\@\Indefinite_Hashed_Maps @em @RefSecNum{The Generic Package Containers.Indefinite_Hashed_Maps}
@\@\@\Indefinite_Hashed_Sets @em @RefSecNum{The Generic Package Containers.Indefinite_Hashed_Sets}
@Chg{Version=[3],New=(@\@\@\Indefinite_Holders @em @RefSecNum{The Generic Package Containers.Indefinite_Holders}
@\@\@\Indefinite_Multiway_Trees @em @RefSecNum{The Generic Package Containers.Indefinite_Multiway_Trees}
), Old=()}@\@\@\Indefinite_Ordered_Maps @em @RefSecNum{The Generic Package Containers.Indefinite_Ordered_Maps}
@\@\@\Indefinite_Ordered_Sets @em @RefSecNum{The Generic Package Containers.Indefinite_Ordered_Sets}
@\@\@\Indefinite_Vectors @em @RefSecNum{The Generic Package Containers.Indefinite_Vectors}), Old=()}>@NewColumnVer{Version=[3]}@Noparanum@shrink<@Chg{Version=[2],
New=[@Chg{Version=[3],New=[Standard (@i{...continued})
@\Ada (@i{...continued})
@\@\Containers (@i{...continued})
@\@\@\Multiway_Trees @em @RefSecNum{The Generic Package Containers.Multiway_Trees}
], Old=()}@\@\@\Ordered_Maps @em @RefSecNum{The Generic Package Containers.Ordered_Maps}
@\@\@\Ordered_Sets @em @RefSecNum{The Generic Package Containers.Ordered_Sets}
@Chg{Version=[3],New=(@\@\@\Synchronized_Queue_Interfaces
@\@\@\@\@\@\@em @RefSecNum{The Generic Package Containers.Synchronized_Queue_Interfaces}
@\@\@\Unbounded_Priority_Queues
@\@\@\@\@\@\@em @RefSecNum{The Generic Package Containers.Unbounded_Priority_Queues}
@\@\@\Unbounded_Synchronized_Queues
@\@\@\@\@\@\@em @RefSecNum{The Generic Package Containers.Unbounded_Synchronized_Queues}
), Old=()}@\@\@\Vectors @em @RefSecNum{The Generic Package Containers.Vectors}
], Old=()}>@NewColumnVer{Version=[2]}@Noparanum@shrink<@Chg{Version=[2],
New=[@Chg{Version=[3],New=[],Old=[Standard (@i{...continued})
@\Ada (@i{...continued})
]}],Old=[]}@\@\Decimal @em @RefSecNum{The Package Decimal}
@\@\Direct_IO @em @RefSecNum{The Generic Package Direct_IO}
@Chg{Version=[2],New=(@\@\Directories @em @RefSecNum{The Package Directories}
@Chg{Version=[3],New=(@\@\@\Hierarchical_File_Names @em @RefSecNum{The Package Directories.Hierarchical_File_Names}
), Old=()}@\@\@\Information @em @RefSecNum{The Package Directories}
@\@\Dispatching @em @RefSecNum{The Task Dispatching Model}
@\@\@\EDF @em @RefSecNum{Earliest Deadline First Dispatching}
@Chg{Version=[3],New=[@\@\@\Non_Preemptive @em @RefSecNum{Non-Preemptive Dispatching}
], Old=[]}@\@\@\Round_Robin @em @RefSecNum{Round Robin Dispatching}
), Old=()}@\@\Dynamic_Priorities @em @RefSecNum{Dynamic Priorities for Tasks}
@Chg{Version=[2],New=(@\@\Environment_Variables @em @RefSecNum{The Package Environment_Variables}
), Old=()}@\@\Exceptions @em @RefSecNum{The Package Exceptions}
@Chg{Version=[2],New=(@\@\Execution_Time @em @RefSecNum{Execution Time}
@\@\@\Group_Budgets @em @RefSecNum{Group Execution Time Budgets}
@Chg{Version=[3],New=(@\@\@\Interrupts @em @RefSecNum{Execution Time of Interrupt Handlers}
), Old=()}@\@\@\Timers @em @RefSecNum{Execution Time Timers}
), Old=()}@\@\Finalization @em @RefSecNum{Assignment and Finalization}
@Chg{New=(@\@\Float_Text_IO @em @RefSecNum{Input-Output for Real Types}
@\@\Float_Wide_Text_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
@Chg{Version=[2],New=(@\@\Float_Wide_Wide_Text_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
), Old=()}@\@\Integer_Text_IO @em @RefSecNum{Input-Output for Integer Types}
@\@\Integer_Wide_Text_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
@Chg{Version=[2],New=(@\@\Integer_Wide_Wide_Text_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
), Old=()}), Old=()}@\@\Interrupts @em @RefSecNum{The Package Interrupts}
@\@\@\Names @em @RefSecNum{The Package Interrupts}
@\@\IO_Exceptions @em @RefSecNum{Exceptions in Input-Output}
@Chg{Version=[3],New=(@\@\Iterator_Interfaces @em @RefSecNum{User-Defined Iterator Types}
@\@\Locales @em @RefSecNum{The Package Locales}
), Old=()}>@NewColumnVer{Version=[3]}@Noparanum@shrink<@Chg{Version=[3],
New=[Standard (@i{...continued})
@\Ada (@i{...continued})
],Old=[]}@\@\Numerics @em @RefSecNum{The Numerics Packages}
@Chg{Version=[2],New=(@\@\@\Complex_Arrays @em @RefSecNum{Complex Vectors and Matrices}
), Old=()}@\@\@\Complex_Elementary_Functions @em @RefSecNum{Complex Elementary Functions}
@\@\@\Complex_Types @em @RefSecNum{Complex Types}
@\@\@\Discrete_Random @em @RefSecNum{Random Number Generation}
@\@\@\Elementary_Functions @em @RefSecNum{Elementary Functions}
@\@\@\Float_Random @em @RefSecNum{Random Number Generation}
@Chg{Version=[2],New=(@\@\@\Generic_Complex_Arrays @em @RefSecNum{Complex Vectors and Matrices}
), Old=()}@\@\@\Generic_Complex_Elementary_Functions
@\@\@\@\@\@\@em @RefSecNum{Complex Elementary Functions}
@\@\@\Generic_Complex_Types @em @RefSecNum{Complex Types}
@\@\@\Generic_Elementary_Functions @em @RefSecNum{Elementary Functions}
@Chg{Version=[2],New=(@\@\@\Generic_Real_Arrays @em @RefSecNum{Real Vectors and Matrices}
@\@\@\Real_Arrays @em @RefSecNum{Real Vectors and Matrices}
), Old=()}@\@\Real_Time @em @RefSecNum{Monotonic Time}
@Chg{Version=[2],New=(@\@\@\Timing_Events @em @RefSecNum{Timing Events}
), Old=()}@\@\Sequential_IO @em @RefSecNum{The Generic Package Sequential_IO}
@\@\Storage_IO @em @RefSecNum{The Generic Package Storage_IO}
@\@\Streams @em @RefSecNum{The Package Streams}
@\@\@\Stream_IO @em @RefSecNum{The Package Streams.Stream_IO}
>@NewColumnVer{Version=[0]}@NewColumnVer{Version=[1]}@NewColumnVer{Version=[2]}@Noparanum@shrink<@Chg{Version=[3],
New=[],Old=[Standard (@i{...continued})
@\Ada (@i{...continued})
]}@\@\Strings @em @RefSecNum{The Package Strings}
@\@\@\Bounded @em @RefSecNum{Bounded-Length String Handling}
@Chg{Version=[3],New=(@\@\@\@\Equal_Case_Insensitive @em @RefSecNum{String Comparison}
), Old=()}@Chg{Version=[2],New=(@\@\@\@\Hash @em @RefSecNum{String Hashing}
@Chg{Version=[3],New=(@\@\@\@\Hash_Case_Insensitive @em @RefSecNum{String Hashing}
@\@\@\@\Less_Case_Insensitive @em @RefSecNum{String Comparison}
), Old=()}), Old=()}@\@\@\Fixed @em @RefSecNum{Fixed-Length String Handling}
@Chg{Version=[3],New=(@\@\@\@\Equal_Case_Insensitive @em @RefSecNum{String Comparison}
), Old=()}@Chg{Version=[2],New=(@\@\@\@\Hash @em @RefSecNum{String Hashing}
@Chg{Version=[3],New=(@\@\@\@\Hash_Case_Insensitive @em @RefSecNum{String Hashing}
@\@\@\@\Less_Case_Insensitive @em @RefSecNum{String Comparison}
@\@\@\Equal_Case_Insensitive @em @RefSecNum{String Comparison}
), Old=()}), Old=()}@\@\@\Hash @em @RefSecNum{String Hashing}
@Chg{Version=[3],New=(@\@\@\Hash_Case_Insensitive @em @RefSecNum{String Hashing}
@\@\@\Less_Case_Insensitive @em @RefSecNum{String Comparison}
), Old=()}@\@\@\Maps @em @RefSecNum{The Package Strings.Maps}
@\@\@\@\Constants @em @RefSecNum{String-Handling Sets and Mappings}
@\@\@\Unbounded @em @RefSecNum{Unbounded-Length String Handling}
@Chg{Version=[3],New=(@\@\@\@\Equal_Case_Insensitive @em @RefSecNum{String Comparison}
), Old=()}@Chg{Version=[2],New=(@\@\@\@\Hash @em @RefSecNum{String Hashing}
@Chg{Version=[3],New=(@\@\@\@\Hash_Case_Insensitive @em @RefSecNum{String Hashing}
@\@\@\@\Less_Case_Insensitive @em @RefSecNum{String Comparison}
@\@\@\UTF_Encoding @em @RefSecNum{String Encoding}
@\@\@\@\Conversions @em @RefSecNum{String Encoding}
@\@\@\@\Strings @em @RefSecNum{String Encoding}
@\@\@\@\Wide_Strings @em @RefSecNum{String Encoding}
@\@\@\@\Wide_Wide_Strings @em @RefSecNum{String Encoding}
), Old=()}), Old=()}@\@\@\Wide_Bounded @em @RefSecNum{Wide_String Handling}
@Chg{Version=[2],New=(@\@\@\@\Wide_Hash @em @RefSecNum{Wide_String Handling}),
Old=()}>@NewColumnVer{Version=[3]}@Noparanum@shrink<@Chg{Version=[3],
New=[Standard (@i{...continued})
@\Ada (@i{...continued})
@\@\Strings (@i{...continued})
],Old=[]}@\@\@\Wide_Fixed @em @RefSecNum{Wide_String Handling}
@Chg{Version=[2],New=(@\@\@\@\Wide_Hash @em @RefSecNum{Wide_String Handling}
@\@\@\Wide_Hash @em @RefSecNum{Wide_String Handling}
), Old=()}@\@\@\Wide_Maps @em @RefSecNum{Wide_String Handling}
@\@\@\@\Wide_Constants @em @RefSecNum{Wide_String Handling}
@\@\@\Wide_Unbounded @em @RefSecNum{Wide_String Handling}
@Chg{Version=[2],New=(@\@\@\@\Wide_Hash @em @RefSecNum{Wide_String Handling}
@\@\@\Wide_Wide_Bounded @em @RefSecNum{Wide_Wide_String Handling}
@\@\@\@\Wide_Wide_Hash @em @RefSecNum{Wide_Wide_String Handling}
@\@\@\Wide_Wide_Fixed @em @RefSecNum{Wide_Wide_String Handling}
@\@\@\@\Wide_Wide_Hash @em @RefSecNum{Wide_Wide_String Handling}
@\@\@\Wide_Wide_Hash @em @RefSecNum{Wide_Wide_String Handling}
@\@\@\Wide_Wide_Maps @em @RefSecNum{Wide_Wide_String Handling}
@\@\@\@\Wide_Wide_Constants @em @RefSecNum{Wide_Wide_String Handling}
@\@\@\Wide_Wide_Unbounded @em @RefSecNum{Wide_Wide_String Handling}
@\@\@\@\Wide_Wide_Hash @em @RefSecNum{Wide_Wide_String Handling}
@Chg{Version=[3],New=[@\@\Synchronous_Barriers @em @RefSecNum{Synchronous Barriers}
], Old=[]}), Old=()}@\@\Synchronous_Task_Control @em @RefSecNum{Synchronous Task Control}
@Chg{Version=[3],New=[@\@\@\EDF @em @RefSecNum{Synchronous Task Control}
], Old=[]}@\@\Tags @em @RefSecNum{Tagged Types and Type Extensions}
@Chg{Version=[2],New=(@\@\@\Generic_Dispatching_Constructor @em @RefSecNum{Tagged Types and Type Extensions}
), Old=()}@\@\Task_Attributes @em @RefSecNum{The Package Task_Attributes}
@\@\Task_Identification @em @RefSecNum{The Package Task_Identification}
@Chg{Version=[2],New=(@\@\Task_Termination @em @RefSecNum{The Package Task_Termination}), Old=()}>
@NewColumnVer{Version=[2]}@Noparanum@shrink<@Chg{Version=[2],
New=[@Chg{Version=[3],New=[],Old=[Standard (@i{...continued})
@\Ada (@i{...continued})
]}], Old=()}@\@\Text_IO @em @RefSecNum{The Package Text_IO}
@Chg{Version=[2],New=(@\@\@\Bounded_IO @em @RefSecNum{Input-Output for Bounded Strings}
),Old=()}@\@\@\Complex_IO @em @RefSecNum{Complex Input-Output}
@\@\@\Editing @em @RefSecNum{The Package Text_IO.Editing}
@\@\@\Text_Streams @em @RefSecNum{The Package Text_IO.Text_Streams}
@Chg{Version=[2],New=(@\@\@\Unbounded_IO @em @RefSecNum{Input-Output for Unbounded Strings}
), Old=()}@\@\Unchecked_Conversion @em @RefSecNum{Unchecked Type Conversions}
@Chg{Version=[3],New=(@\@\Unchecked_Deallocate_Subpool @em @RefSecNum{Subpool Reclamation}
), Old=()}@\@\Unchecked_Deallocation @em @RefSecNum{Unchecked Storage Deallocation}
@Chg{Version=[2],New=(@\@\Wide_Characters @em @RefSecNum{The Packages Characters, Wide_Characters, and Wide_Wide_Characters}
@Chg{Version=[3],New=(@\@\@\Handling @em @RefSecNum{The Package Wide_Characters.Handling}
),Old=()}),Old=()}@\@\Wide_Text_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
@\@\@\Complex_IO @em @RefSecNum{The Package Wide_Text_IO.Complex_IO}
@\@\@\Editing @em @RefSecNum{The Package Wide_Text_IO.Editing}
@\@\@\Text_Streams @em @RefSecNum{The Package Wide_Text_IO.Text_Streams}
@Chg{Version=[2],New=(@\@\@\Wide_Bounded_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
@\@\@\Wide_Unbounded_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
@\@\Wide_Wide_Characters @em @RefSecNum{The Packages Characters, Wide_Characters, and Wide_Wide_Characters}
@Chg{Version=[3],New=(@\@\@\Handling @em @RefSecNum{The Package Wide_Wide_Characters.Handling}
),Old=()}@\@\Wide_Wide_Text_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
@\@\@\Complex_IO @em @RefSecNum{The Package Wide_Wide_Text_IO.Complex_IO}
@\@\@\Editing @em @RefSecNum{The Package Wide_Wide_Text_IO.Editing}
@\@\@\Text_Streams @em @RefSecNum{The Package Wide_Wide_Text_IO.Text_Streams}
@\@\@\Wide_Wide_Bounded_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
@\@\@\Wide_Wide_Unbounded_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}), Old=()}>

@NewColumnVer{Version=[3]}@Noparanum@shrink<@\Interfaces @em @RefSecNum{The Package Interfaces}
@\@\C @em @RefSecNum{Interfacing with C and C++}
@\@\@\Pointers @em @RefSecNum{The Generic Package Interfaces.C.Pointers}
@\@\@\Strings @em @RefSecNum{The Package Interfaces.C.Strings}
@\@\COBOL @em @RefSecNum{Interfacing with COBOL}
@\@\Fortran @em @RefSecNum{Interfacing with Fortran}>

@NewColumnVer{Version=[3]}@Noparanum@shrink<@\System @em @RefSecNum{The Package System}
@\@\Address_To_Access_Conversions @em @RefSecNum{The Package System.Address_To_Access_Conversions}
@\@\Machine_Code @em @RefSecNum{Machine Code Insertions}
@Chg{Version=[3],New=(@\@\Multiprocessors @em @RefSecNum{Multiprocessor Implementation}
@\@\@\Dispatching_Domains @em @RefSecNum{Multiprocessor Dispatching Domains}
),Old=()}@\@\RPC @em @RefSecNum{Partition Communication Subsystem}
@\@\Storage_Elements @em @RefSecNum{The Package System.Storage_Elements}
@\@\Storage_Pools @em @RefSecNum{Storage Management}@Chg{Version=[3],New=(
@\@\@\Subpools @em @RefSecNum{Storage Subpools}), Old=()}>]
@end{TwoCol}
@end{Display}
@begin{Discussion}
In running text, we generally leave out the @lquotes@;Ada.@rquotes@; when referring to a
child of Ada.
@end{Discussion}
@begin{Reason}
We had no strict rule for which of Ada, Interfaces, or System should be
the parent of a given library unit.
However, we have tried to place as many things as possible under Ada,
except that interfacing is a separate category,
and we have tried to place library units whose use is highly
nonportable under System.
@end{Reason}
@end{Intro}

@begin{ImplReq}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
The implementation shall ensure that each
language@Chg{Version=[2],New=[-],Old=[]}defined subprogram is
reentrant@Chg{Version=[2],New=[@Defn{reentrant}],Old=[]}@ChgNote{Suggested by Gary Dismukes} in
the sense that concurrent calls on the same subprogram perform as specified,
so long as all parameters that could be passed by reference
denote nonoverlapping objects.
@begin{Ramification}
  For example, simultaneous calls to Text_IO.Put will work properly,
  so long as they are going to two different files.
  On the other hand, simultaneous output to the same file constitutes
  erroneous use of shared variables.
@end{Ramification}
@begin{Honest}
  Here, @lquotes@;language defined subprogram@rquotes@; means a language defined library
  subprogram, a subprogram declared in the visible part of a language
  defined library package, an instance of a language defined
  generic library subprogram,
  or a subprogram declared in the visible part
  of an instance of a language defined generic library package.
@end{Honest}
@begin{Ramification}
  The rule implies that any data local to the private part or
  body of the package has to be somehow protected against
  simultaneous access.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0048-1]}
@ChgAdded{Version=[3],Text=[If a descendant of a language-defined tagged
type is declared, the implementation
shall ensure that each inherited language-defined subprogram behaves as
described in this International Standard. In particular, overriding
a language-defined subprogram shall
not alter the effect of any inherited language-defined subprogram.]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This means that internally the implementation
  must not do redispatching unless it is required by the Standard.
  So when we say that some subprogram Bar is equivalent to
  Foo, overriding Foo for a derived type doesn't change the semantics of
  Bar, and in particular it means that Bar may no longer be equivalent to
  Foo. The word @ldquote@;equivalent@rdquote is always a bit of a lie anyway.]}
@end{Reason}
@end{ImplReq}

@begin{ImplPerm}
The implementation may restrict the replacement of language-defined
compilation units.
The implementation may restrict children of language-defined library
units (other than Standard).
@begin{Ramification}
For example, the implementation may say,
@lquotes@;you cannot compile a library unit called System@rquotes@;
or @lquotes@;you cannot compile a child of package System@rquotes@;
or @lquotes@;if you compile a library unit called System,
it has to be a package, and it has to contain at least
the following declarations: ...@rquotes@;.
@end{Ramification}
@end{ImplPerm}

@begin{DiffWord83}
Many of Ada 83's language-defined library units are now children of Ada
or System.
For upward compatibility, these are renamed as root library units
(see @RefSecNum{Renamings of Library Units}).

The order and lettering of the annexes has been changed.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0047],ARef=[AI95-00081-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Units missing from the list of
  predefined units were added.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00424-01]}
  @ChgAdded{Version=[2],Text=[Added new units to the list of
  predefined units.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0048-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added wording to ban
  redispatching unless it is explicitly required, in order to safeguard
  portability when overriding language-defined routines.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0060-1],ARef=[AI05-0206-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a permission to
  omit pragma Remote_Types from language-defined units if Annex E is
  not supported. This was later removed, as a better method of supporting
  the reason is now available.
  Note that this requires all implementations to provide minimal support for
  the Remote_Types categorization even if Annex E is not supported; being
  unable to compile language-defined units is not allowed.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0001-1],ARef=[AI05-0049-1],ARef=[AI05-0069-1],ARef=[AI05-0111-3],ARef=[AI05-0136-1],ARef=[AI05-0137-1],ARef=[AI05-0166-1],ARef=[AI05-0168-1]}
  @ChgAdded{Version=[3],Text=[Added various new units to the
  list of predefined units.]}
@end{DiffWord2005}
