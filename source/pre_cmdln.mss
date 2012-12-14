@comment{ $Source: e:\\cvsroot/ARM/Source/pre_cmdln.mss,v $ }
@comment{ $Revision: 1.29 $ $Date: 2012/11/28 23:53:06 $ $Author: randy $ }
@Part(predefcmdln, Root="ada.mss")

@Comment{$Date: 2012/11/28 23:53:06 $}
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
@key[package] Ada.Command_Line @key[is]@ChildUnit{Parent=[Ada],Child=[Command_Line]}
  @key[pragma] Preelaborate(Command_Line);

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
@Trailing@;If the external execution environment supports passing arguments to
a program, then
Argument returns an implementation-defined value corresponding to
the argument at relative position Number.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
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
If the external execution environment supports passing arguments to
a program, then
Command_Name returns an implementation-defined value corresponding to
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