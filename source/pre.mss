@Part(predef, Root="ada.mss")

@Comment{$Date: 2000/05/26 05:03:28 $}
@LabeledNormativeAnnex{Predefined Language Environment}

@comment{$Source: e:\\cvsroot/ARM/Source/pre.mss,v $}
@comment{$Revision: 1.14 $}
@comment{$RLB: Eliminated includes. $}

@begin{Intro}
@redundant[
@Defn{Language-Defined Library Units}
@Defn{predefined environment}
This Annex contains the specifications of library units that shall be
provided by every implementation.
There are three root library units:
Ada, Interfaces, and System;
other library units are children of these:

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0047]}@ @;@comment{paragraph number here}
@begin{DisplaywithoutParanum}
@TabClear{}
@TabSet{L2, L4, L6, L8, L10, L12, L14, L16}
@begin{TwoCol}
@shrink<Standard @em @RefSecNum{The Package Standard}
@\Ada @em @RefSecNum{The Package Ada}
@\@\Asynchronous_Task_Control @em @RefSecNum{Asynchronous Task Control}
@\@\Calendar @em @RefSecNum{Delay Statements, Duration, and Time}
@\@\Characters @em @RefSecNum{The Package Characters}
@\@\@\Handling @em @RefSecNum{The Package Characters.Handling}
@\@\@\Latin_1 @em @RefSecNum{The Package Characters.Latin_1}
@\@\Command_Line @em @RefSecNum{The Package Command_Line}
@\@\Decimal @em @RefSecNum{The Package Decimal}
@\@\Direct_IO @em @RefSecNum{The Generic Package Direct_IO}
@\@\Dynamic_Priorities @em @RefSecNum{Dynamic Priorities}
@\@\Exceptions @em @RefSecNum{The Package Exceptions}
@\@\Finalization @em @RefSecNum{User-Defined Assignment and Finalization}
@Chg{New=[@\@\Float_Text_IO @em @RefSecNum{Input-Output for Real Types}
@\@\Float_Wide_Text_IO @em @RefSecNum{Wide Text Input-Output}
@\@\Integer_Text_IO @em @RefSecNum{Input-Output for Integer Types}
@\@\Integer_Wide_Text_IO @em @RefSecNum{Wide Text Input-Output}
], Old=[]}@\@\Interrupts @em @RefSecNum{The Package Interrupts}
@\@\@\Names @em @RefSecNum{The Package Interrupts}
@\@\IO_Exceptions @em @RefSecNum{Exceptions in Input-Output}
@\@\Numerics @em @RefSecNum{The Numerics Packages}
@\@\@\Complex_Elementary_Functions @em @RefSecNum{Complex Elementary Functions}
@\@\@\Complex_Types @em @RefSecNum{Complex Types}
@\@\@\Discrete_Random @em @RefSecNum{Random Number Generation}
@\@\@\Elementary_Functions @em @RefSecNum{Elementary Functions}
@\@\@\Float_Random @em @RefSecNum{Random Number Generation}
@\@\@\Generic_Complex_Elementary_Functions
@\@\@\@\@\@\@em @RefSecNum{Complex Elementary Functions}
@\@\@\Generic_Complex_Types @em @RefSecNum{Complex Types}
@\@\@\Generic_Elementary_Functions @em @RefSecNum{Elementary Functions}
@\@\Real_Time @em @RefSecNum{Monotonic Time}
@\@\Sequential_IO @em @RefSecNum{The Generic Package Sequential_IO}
@\@\Storage_IO @em @RefSecNum{The Generic Package Storage_IO}
@\@\Streams @em @RefSecNum{The Package Streams}
@\@\@\Stream_IO @em @RefSecNum{The Package Streams.Stream_IO}>
@NewColumn
@shrink<Standard (@i{...continued})
@\Ada (@i{...continued})
@\@\Strings @em @RefSecNum{The Package Strings}
@\@\@\Bounded @em @RefSecNum{Bounded-Length String Handling}
@\@\@\Fixed @em @RefSecNum{Fixed-Length String Handling}
@\@\@\Maps @em @RefSecNum{The Package Strings.Maps}
@\@\@\@\Constants @em @RefSecNum{String-Handling Sets and Mappings}
@\@\@\Unbounded @em @RefSecNum{Unbounded-Length String Handling}
@\@\@\Wide_Bounded @em @RefSecNum{Wide_String Handling}
@\@\@\Wide_Fixed @em @RefSecNum{Wide_String Handling}
@\@\@\Wide_Maps @em @RefSecNum{Wide_String Handling}
@\@\@\@\Wide_Constants @em @RefSecNum{Wide_String Handling}
@\@\@\Wide_Unbounded @em @RefSecNum{Wide_String Handling}
@\@\Synchronous_Task_Control @em @RefSecNum{Synchronous Task Control}
@\@\Tags @em @RefSecNum{Tagged Types and Type Extensions}
@\@\Task_Attributes @em @RefSecNum{The Package Task_Attributes}
@\@\Task_Identification @em @RefSecNum{The Package Task_Identification}
@\@\Text_IO @em @RefSecNum{The Package Text_IO}
@\@\@\Complex_IO @em @RefSecNum{Complex Input-Output}
@\@\@\Editing @em @RefSecNum{The Package Text_IO.Editing}
@\@\@\Text_Streams @em @RefSecNum{The Package Text_IO.Text_Streams}
@\@\Unchecked_Conversion @em @RefSecNum{Unchecked Type Conversions}
@\@\Unchecked_Deallocation @em @RefSecNum{Unchecked Storage Deallocation}
@\@\Wide_Text_IO @em @RefSecNum{Wide Text Input-Output}
@\@\@\Complex_IO @em @RefSecNum{Complex Input-Output}
@\@\@\Editing @em @RefSecNum{The Package Wide_Text_IO.Editing}
@\@\@\Text_Streams @em @RefSecNum{The Package Wide_Text_IO.Text_Streams}>

@shrink<@\Interfaces @em @RefSecNum{The Package Interfaces}
@\@\C @em @RefSecNum{Interfacing with C}
@\@\@\Pointers @em @RefSecNum{The Generic Package Interfaces.C.Pointers}
@\@\@\Strings @em @RefSecNum{The Package Interfaces.C.Strings}
@\@\COBOL @em @RefSecNum{Interfacing with COBOL}
@\@\Fortran @em @RefSecNum{Interfacing with Fortran}>

@shrink<@\System @em @RefSecNum{The Package System}
@\@\Address_To_Access_Conversions @em @RefSecNum{The Package System.Address_To_Access_Conversions}
@\@\Machine_Code @em @RefSecNum{Machine Code Insertions}
@\@\RPC @em @RefSecNum{Partition Communication Subsystem}
@\@\Storage_Elements @em @RefSecNum{The Package System.Storage_Elements}
@\@\Storage_Pools @em @RefSecNum{Storage Management}>
@end{TwoCol}
@end{DisplaywithoutParanum}
]
@begin{Discussion}
In running text, we generally leave out the ``Ada.'' when referring to a
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
The implementation shall ensure that each language defined subprogram is
reentrant in the sense that concurrent calls on the same subprogram
perform as specified,
so long as all parameters that could be passed by reference
denote nonoverlapping objects.
@begin{Ramification}
  For example, simultaneous calls to Text_IO.Put will work properly,
  so long as they are going to two different files.
  On the other hand, simultaneous output to the same file constitutes
  erroneous use of shared variables.
@end{Ramification}
@begin{Honest}
  Here, ``language defined subprogram'' means a language defined library
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
``you cannot compile a library unit called System''
or ``you cannot compile a child of package System''
or ``if you compile a library unit called System,
it has to be a package, and it has to contain at least
the following declarations: ...''.
@end{Ramification}
@end{ImplPerm}

@begin{DiffWord83}
Many of Ada 83's language-defined library units are now children of Ada
or System.
For upward compatibility, these are renamed as root library units
(see @RefSecNum{Renamings of Ada 83 Library Units}).

The order and lettering of the annexes has been changed.
@end{DiffWord83}

