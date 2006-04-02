@Part(predef, Root="ada.mss")

@Comment{$Date: 2006/03/21 22:47:18 $}
@LabeledNormativeAnnex{Predefined Language Environment}

@comment{$Source: e:\\cvsroot/ARM/Source/pre.mss,v $}
@comment{$Revision: 1.32 $}
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
@\@\@\Doubly_Linked_Lists @em @RefSecNum{The Package Containers.Doubly_Linked_Lists}
@\@\@\Generic_Array_Sort @em @RefSecNum{Array Sorting}
@\@\@\Generic_Constrained_Array_Sort
@\@\@\@\@\@\@em @RefSecNum{Array Sorting}
@\@\@\Hashed_Maps @em @RefSecNum{The Package Containers.Hashed_Maps}
@\@\@\Hashed_Sets @em @RefSecNum{The Package Containers.Hashed_Sets}
@\@\@\Indefinite_Doubly_Linked_Lists
@\@\@\@\@\@\@em @RefSecNum{The Package Containers.Indefinite_Doubly_Linked_Lists}
@\@\@\Indefinite_Hashed_Maps @em @RefSecNum{The Package Containers.Indefinite_Hashed_Maps}
@\@\@\Indefinite_Hashed_Sets @em @RefSecNum{The Package Containers.Indefinite_Hashed_Sets}
@\@\@\Indefinite_Ordered_Maps @em @RefSecNum{The Package Containers.Indefinite_Ordered_Maps}
@\@\@\Indefinite_Ordered_Sets @em @RefSecNum{The Package Containers.Indefinite_Ordered_Sets}
@\@\@\Indefinite_Vectors @em @RefSecNum{The Package Containers.Indefinite_Vectors}
@\@\@\Ordered_Maps @em @RefSecNum{The Package Containers.Ordered_Maps}
@\@\@\Ordered_Sets @em @RefSecNum{The Package Containers.Ordered_Sets}
@\@\@\Vectors @em @RefSecNum{The Package Containers.Vectors}
), Old=()}@\@\Decimal @em @RefSecNum{The Package Decimal}
@\@\Direct_IO @em @RefSecNum{The Generic Package Direct_IO}
@Chg{Version=[2],New=(@\@\Directories @em @RefSecNum{The Package Directories}
@\@\@\Information @em @RefSecNum{The Package Directories}
@\@\Dispatching @em @RefSecNum{The Task Dispatching Model}
@\@\@\EDF @em @RefSecNum{Earliest Deadline First Dispatching}
@\@\@\Round_Robin @em @RefSecNum{Round Robin Dispatching}
), Old=()}@\@\Dynamic_Priorities @em @RefSecNum{Dynamic Priorities}>
@NewColumn@Comment{This should be conditional for version 2, but not with the current generator}
@Noparanum@shrink<@Chg{Version=[2],New=[Standard (@i{...continued})
@\Ada (@i{...continued})
@\@\Environment_Variables @em @RefSecNum{The Package Environment_Variables}
], Old=()}@\@\Exceptions @em @RefSecNum{The Package Exceptions}
@Chg{Version=[2],New=(@\@\Execution_Time @em @RefSecNum{Execution Time}
@\@\@\Group_Budgets @em @RefSecNum{Group Execution Time Budgets}
@\@\@\Timers @em @RefSecNum{Execution Time Timers}
), Old=()}@\@\Finalization @em @RefSecNum{User-Defined Assignment and Finalization}
@Chg{New=(@\@\Float_Text_IO @em @RefSecNum{Input-Output for Real Types}
@\@\Float_Wide_Text_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
@Chg{Version=[2],New=(@\@\Float_Wide_Wide_Text_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
), Old=()}@\@\Integer_Text_IO @em @RefSecNum{Input-Output for Integer Types}
@\@\Integer_Wide_Text_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
@Chg{Version=[2],New=(@\@\Integer_Wide_Wide_Text_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
), Old=()}), Old=()}@\@\Interrupts @em @RefSecNum{The Package Interrupts}
@\@\@\Names @em @RefSecNum{The Package Interrupts}
@\@\IO_Exceptions @em @RefSecNum{Exceptions in Input-Output}
@\@\Numerics @em @RefSecNum{The Numerics Packages}
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
@\@\@\Stream_IO @em @RefSecNum{The Package Streams.Stream_IO}>
@NewColumn
@Noparanum@shrink<Standard (@i{...continued})
@\Ada (@i{...continued})
@\@\Strings @em @RefSecNum{The Package Strings}
@\@\@\Bounded @em @RefSecNum{Bounded-Length String Handling}
@Chg{Version=[2],New=(@\@\@\@\Hash @em @RefSecNum{String Hashing}
), Old=()}@\@\@\Fixed @em @RefSecNum{Fixed-Length String Handling}
@Chg{Version=[2],New=(@\@\@\@\Hash @em @RefSecNum{String Hashing}
@\@\@\Hash @em @RefSecNum{String Hashing}
), Old=()}@\@\@\Maps @em @RefSecNum{The Package Strings.Maps}
@\@\@\@\Constants @em @RefSecNum{String-Handling Sets and Mappings}
@\@\@\Unbounded @em @RefSecNum{Unbounded-Length String Handling}
@Chg{Version=[2],New=(@\@\@\@\Hash @em @RefSecNum{String Hashing}
), Old=()}@\@\@\Wide_Bounded @em @RefSecNum{Wide_String Handling}
@Chg{Version=[2],New=(@\@\@\@\Wide_Hash @em @RefSecNum{Wide_String Handling}
), Old=()}@\@\@\Wide_Fixed @em @RefSecNum{Wide_String Handling}
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
), Old=()}@\@\Synchronous_Task_Control @em @RefSecNum{Synchronous Task Control}
@\@\Tags @em @RefSecNum{Tagged Types and Type Extensions}
@Chg{Version=[2],New=(@\@\@\Generic_Dispatching_Constructor @em @RefSecNum{Tagged Types and Type Extensions}
), Old=()}@\@\Task_Attributes @em @RefSecNum{The Package Task_Attributes}
@\@\Task_Identification @em @RefSecNum{The Package Task_Identification}
@Chg{Version=[2],New=(@\@\Task_Termination @em @RefSecNum{The Package Task_Termination}), Old=()}>
@NewColumn@Comment{This should be conditional for version 2, but not with the current generator}
@Noparanum@shrink<@Chg{Version=[2],New=[Standard (@i{...continued})
@\Ada (@i{...continued})
], Old=()}@\@\Text_IO @em @RefSecNum{The Package Text_IO}
@Chg{Version=[2],New=(@\@\@\Bounded_IO @em @RefSecNum{Input-Output for Bounded Strings}
),Old=()}@\@\@\Complex_IO @em @RefSecNum{Complex Input-Output}
@\@\@\Editing @em @RefSecNum{The Package Text_IO.Editing}
@\@\@\Text_Streams @em @RefSecNum{The Package Text_IO.Text_Streams}
@Chg{Version=[2],New=(@\@\@\Unbounded_IO @em @RefSecNum{Input-Output for Unbounded Strings}
), Old=()}@\@\Unchecked_Conversion @em @RefSecNum{Unchecked Type Conversions}
@\@\Unchecked_Deallocation @em @RefSecNum{Unchecked Storage Deallocation}
@Chg{Version=[2],New=(@\@\Wide_Characters @em @RefSecNum{The Packages Characters, Wide_Characters, and Wide_Wide_Characters}
),Old=()}@\@\Wide_Text_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
@\@\@\Complex_IO @em @RefSecNum{The Package Wide_Text_IO.Complex_IO}
@\@\@\Editing @em @RefSecNum{The Package Wide_Text_IO.Editing}
@\@\@\Text_Streams @em @RefSecNum{The Package Wide_Text_IO.Text_Streams}
@Chg{Version=[2],New=(@\@\@\Wide_Bounded_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
@\@\@\Wide_Unbounded_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
@\@\Wide_Wide_Characters @em @RefSecNum{The Packages Characters, Wide_Characters, and Wide_Wide_Characters}
@\@\Wide_Wide_Text_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
@\@\@\Complex_IO @em @RefSecNum{The Package Wide_Wide_Text_IO.Complex_IO}
@\@\@\Editing @em @RefSecNum{The Package Wide_Wide_Text_IO.Editing}
@\@\@\Text_Streams @em @RefSecNum{The Package Wide_Wide_Text_IO.Text_Streams}
@\@\@\Wide_Wide_Bounded_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}
@\@\@\Wide_Wide_Unbounded_IO @em @RefSecNum{Wide Text Input-Output and Wide Wide Text Input-Output}), Old=()}>

@Noparanum@shrink<@\Interfaces @em @RefSecNum{The Package Interfaces}
@\@\C @em @RefSecNum{Interfacing with C and C++}
@\@\@\Pointers @em @RefSecNum{The Generic Package Interfaces.C.Pointers}
@\@\@\Strings @em @RefSecNum{The Package Interfaces.C.Strings}
@\@\COBOL @em @RefSecNum{Interfacing with COBOL}
@\@\Fortran @em @RefSecNum{Interfacing with Fortran}>

@Noparanum@shrink<@\System @em @RefSecNum{The Package System}
@\@\Address_To_Access_Conversions @em @RefSecNum{The Package System.Address_To_Access_Conversions}
@\@\Machine_Code @em @RefSecNum{Machine Code Insertions}
@\@\RPC @em @RefSecNum{Partition Communication Subsystem}
@\@\Storage_Elements @em @RefSecNum{The Package System.Storage_Elements}
@\@\Storage_Pools @em @RefSecNum{Storage Management}>]
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
non-portable under System.
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
(see @RefSecNum{Renamings of Ada 83 Library Units}).

The order and lettering of the annexes has been changed.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0047],ARef=[AI95-00081-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Units missing from the list of
  predefined units were added.]}
@end{DiffWord95}
