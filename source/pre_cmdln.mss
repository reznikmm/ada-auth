@comment{ $Source: e:\\cvsroot/ARM/Source/pre_cmdln.mss,v $ }
@comment{ $Revision: 1.33 $ $Date: 2019/02/09 03:46:57 $ $Author: randy $ }
@Part(predefcmdln, Root="ada.mss")

@Comment{$Date: 2019/02/09 03:46:57 $}
@LabeledClause{The Package Command_Line}
@begin{Intro}
The package Command_Line allows a program to obtain the values of its
arguments and to set the exit status code to be returned on normal termination.
@ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],
Text=[The meaning of Argument_Count,
Argument, and Command_Name@Chg{Version=[2],New=[ for package Command_Line. The
bounds of type Command_Line.Exit_Status],Old=[]}.]}
@end{Intro}

@begin{StaticSem}
@Leading@Keepnext@;The library package Ada.Command_Line has the following declaration:
@begin{Example}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0241-1]}
@key[package] Ada.Command_Line@Chg{Version=[5],New=[],Old=[ @key[is]]}@ChildUnit{Parent=[Ada],Child=[Command_Line]}
  @Chg{Version=[5],New=[@key[with]],Old=[@key[pragma]]} Preelaborate@Chg{Version=[5],New=[, Nonblocking @key[is]],Old=[(Command_Line);]}

  @key[function] @AdaSubDefn{Argument_Count} @key[return] Natural;

  @key[function] @AdaSubDefn{Argument} (Number : @key[in] Positive) @key[return] String;

  @key[function] @AdaSubDefn{Command_Name} @key[return] String;

  @key[type] @AdaTypeDefn{Exit_Status} @key[is] @RI{implementation-defined integer type};

  @AdaObjDefn{Success} : @key[constant] Exit_Status;
  @AdaObjDefn{Failure} : @key[constant] Exit_Status;

  @key[procedure] @AdaSubDefn{Set_Exit_Status} (Code : @key[in] Exit_Status);

@key[private]
  ... -- @RI{not specified by the language}
@key[end] Ada.Command_Line;
@comment{Blank line}
@end{example}
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[function] Argument_Count @key[return] Natural;
@end{Example}
@Trailing@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
If the external execution environment supports passing arguments to
a program, then Argument_Count returns
the number of arguments passed to the program
invoking the function. Otherwise@Chg{Version=[3],New=[,],Old=[]}
it returns 0.
The meaning of @lquotes@;number of arguments@rquotes@; is implementation defined.

@begin{Example}@Keepnext
@key[function] Argument (Number : @key[in] Positive) @key[return] String;
@end{Example}
@Trailing@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0259-1]}
If the external execution environment supports passing arguments to
a program, then
Argument returns an implementation-defined value@Chg{Version=[5],New=[ with
lower bound 1],Old=[]} corresponding to
the argument at relative position Number.
If Number is outside the range 1..Argument_Count, then
Constraint_Error is propagated.
@begin{Ramification}
If the external execution environment does not support
passing arguments to a program, then Argument(N) for any N will
raise Constraint_Error, since Argument_Count is 0.@end{ramification}

@begin{Example}@Keepnext
@key[function] Command_Name @key[return] String;
@end{Example}
@Trailing@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0259-1]}
If the external execution environment supports passing arguments to
a program, then
Command_Name returns an implementation-defined value@Chg{Version=[5],New=[ with
lower bound 1],Old=[]} corresponding to
the name of the command invoking the program;
otherwise@Chg{Version=[3],New=[,],Old=[]}
Command_Name returns the null string.

@Comment{This is missing; leading the following paragraph glued to "Command_Name"}
@begin{Example}@Keepnext
@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[@key[type] Exit_Status @key[is] @RI{implementation-defined integer type};],Old=[]}
@end{Example}
@Trailing@;
The type Exit_Status represents the range of exit
status values supported by the external execution environment.
The constants Success and Failure correspond to success and failure,
respectively.

@begin{Example}@Keepnext
@key[procedure] Set_Exit_Status (Code : @key[in] Exit_Status);
@end{Example}
If the external execution environment supports returning an exit status
from a program, then Set_Exit_Status sets Code as the status. Normal
termination of a program returns as the exit status the value most
recently set by Set_Exit_Status, or, if no such value has been set, then the
value Success. If a program terminates abnormally, the status set by
Set_Exit_Status is ignored, and an implementation-defined exit status value
is set.

@ChgNote{An incorrect index entry; presentation AI-00005}
@Chg{New=[],Old=[@PDefn{unspecified}]}
If the external execution environment does not support returning
an exit value from a program, then Set_Exit_Status does nothing.
@end{DescribeCode}
@end{StaticSem}

@begin{ImplPerm}
An alternative declaration is allowed
for package Command_Line if different functionality is appropriate
for the external execution environment.
@end{ImplPerm}

@begin{Notes}
Argument_Count, Argument, and Command_Name
correspond to the C language's argc, argv[n] (for n>0) and argv[0],
respectively.
@begin{Honest}
The correspondence of Argument_Count to argc is not direct @em
argc would be one more than Argument_Count, since the argc count
includes the command name, whereas Argument_Count does not.
@end{Honest}
@end{Notes}

@begin{Extend83}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Defn{extensions to Ada 83}
This @Chg{Version=[3],New=[subclause],Old=[clause]} is new in Ada 95.
@end{Extend83}

@begin{DiffWord2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0259-1]}
  @ChgAdded{Version=[5],Text=[@b<Correction:> Defined the lower bound of
  functions Argument and Command_Name. This could be inconsistent if someone
  depended on the lower bound of these routines (and it wasn't 1), but such
  code was never portable (even to later versions of the same implementation).
  Thus we don't document it as an inconsistency.]}
@end{Diffword2012}


@LabeledAddedSubClause{Version=[5],Name=[The Packages Wide_Command_Line and Wide_Wide_Command_Line]}

@begin{Intro}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0021-1]}
@ChgAdded{Version=[5],Text=[The packages Wide_Command_Line and
Wide_Wide_Command_Line allow a program to obtain the values of its arguments
and to set the exit status code to be returned on normal termination.]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0021-1]}
@ChgAdded{Version=[5],Text=[The specification of package Wide_Command_Line is
the same as for Command_Line, except that each occurrence of String is
replaced by Wide_String.@ChildUnit{Parent=[Ada],Child=[Wide_Command_Line]}]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0021-1]}
@ChgAdded{Version=[5],Text=[The specification of package
Wide_Wide_Command_Line is the same as for Command_Line, except that each
occurrence of String is replaced by
Wide_Wide_String.@ChildUnit{Parent=[Ada],Child=[Wide_Wide_Command_Line]}]}

@end{StaticSem}


@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0021-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}
  These packages are new.]}
@end{Extend2012}

