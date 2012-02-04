@comment{ $Source: e:\\cvsroot/ARM/Source/pre_environ.mss,v $ }
@comment{ $Revision: 1.12 $ $Date: 2012/01/28 08:23:02 $ $Author: randy $ }
@Part(predefenviron, Root="ada.mss")

@Comment{$Date: 2012/01/28 08:23:02 $}

@LabeledAddedClause{Version=[2],Name=[The Package Environment_Variables]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@ChgAdded{Version=[2],Text=[@Defn{environment variable}
The package Environment_Variables allows a program to
read or modify environment variables. Environment variables are name-value
pairs, where both the name and value are strings. The definition of what
constitutes an @i{environment variable}, and the meaning of the name and value,
are implementation defined.]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@Chg{Version=[2],New=[The
definition and meaning of an environment variable.],Old=[]}]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The library package
Environment_Variables has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} Ada.Environment_Variables @key{is}@ChildUnit{Parent=[Ada],Child=[Environment_Variables]}
   @key{pragma} Preelaborate(Environment_Variables);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Value} (Name : @key{in} String) @key{return} String;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0285-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Value} (Name : @key{in} String; Default : @key{in} String) @key{return} String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Exists} (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Set} (Name : @key{in} String; Value : @key{in} String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Clear} (Name : @key{in} String);
   @key{procedure} @AdaSubDefn{Clear};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0248-1]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Iterate}@Chg{Version=[3],New=[],Old=[ (]}
      @Chg{Version=[3],New=[(],Old=[ ]}Process : @key{not null access procedure} (Name, Value : @key{in} String));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[@key{end} Ada.Environment_Variables;]}
@end{Example}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Value (Name : @key{in} String) @key{return} String;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If the external execution
environment supports environment variables, then Value returns the value of the
environment variable with the given name. If no environment variable with the
given name exists, then Constraint_Error is propagated. If the execution
environment does not support environment variables, then Program_Error is
propagated.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],KeepNext=[T],Text=[@key{function} Value (Name : @key{in} String; Default : @key{in} String) @key{return} String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0285-1]}
@ChgAdded{Version=[3],Type=[Trailing],Text=[If the external execution
environment supports environment variables, then Value returns the value of the
environment variable with the given name. If no environment variable with the
given name exists, then Default is returned. If the execution
environment does not support environment variables, then Program_Error is
propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Exists (Name : @key{in} String) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If the external execution environment
supports environment variables and an environment variable with the given name
currently exists, then Exists returns True;
otherwise@Chg{Version=[3],New=[,],Old=[]}
it returns False.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Set (Name : @key{in} String; Value : @key{in} String);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[If the external execution
environment supports environment variables, then Set first clears any existing
environment variable with the given name, and then defines a single new
environment variable with the given name and value.
Otherwise@Chg{Version=[3],New=[,],Old=[]} Program_Error is
propagated.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[If implementation-defined circumstances prohibit
the definition of an environment variable with the given name and value, then
Constraint_Error is propagated.]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],
Text=[@Chg{Version=[2],New=[The circumstances where an environment variable
cannot be defined.],Old=[]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[It is implementation defined
whether there exist values for which the call Set(Name, Value) has the same
effect as Clear (Name).]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],
Text=[@Chg{Version=[2],New=[Environment names for which Set has the effect of
Clear.],Old=[]}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Clear (Name : @key{in} String);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If the external execution
environment supports environment variables, then Clear deletes all existing
environment @Chg{Version=[3],New=[variables],Old=[variable]} with the given
name. Otherwise@Chg{Version=[3],New=[,],Old=[]} Program_Error is propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Clear;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[If the external execution
environment supports environment variables,
then Clear deletes all existing environment variables.
Otherwise@Chg{Version=[3],New=[,],Old=[]} Program_Error is
propagated.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0248-1]}
@ChgAdded{Version=[2],Text=[@key{procedure} Iterate@Chg{Version=[3],New=[],Old=[ (]}
   @Chg{Version=[3],New=[(],Old=[  ]}Process : @key{not null access procedure} (Name, Value : @key{in} String));]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[If the external execution environment supports
environment variables, then Iterate calls the subprogram designated by Process
for each existing environment variable, passing the name and value of that
environment variable.
Otherwise@Chg{Version=[3],New=[,],Old=[]} Program_Error is
propagated.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[If several environment variables exist that have
the same name, Process is called once for each such variable.]}

@end{DescribeCode}
@end{StaticSem}

@begin{Bounded}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error to call Value if more than one environment variable
exists with the given name; the possible outcomes are that:]}
@begin{itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[one of the values is returned, and that
same value is returned in subsequent calls in the absence of changes to
the environment; or]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Program_Error is propagated.]}
@end{Itemize}
@end{Bounded}

@begin{Erron}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=(erroneous execution),Sec=(cause)}
Making calls to the procedures Set or Clear concurrently with calls to any
subprogram of package Environment_Variables, or to any instantiation of Iterate,
results in erroneous execution.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Making calls to the procedures Set or Clear in the actual subprogram
corresponding to the Process parameter of Iterate results in erroneous
execution.]}
@end{Erron}

@begin{DocReq}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@ChgAdded{Version=[2],Text=[An implementation shall document how the
operations of this package behave if
environment variables are changed by external mechanisms (for instance,
calling operating system services).]}
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[The behavior of package Environment_Variables when environment variables
are changed by external mechanisms.]}]}
@end{DocReq}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@ChgAdded{Version=[2],Text=[An implementation running on a system that does
not support environment variables is permitted to define the operations of
package Environment_Variables with the semantics corresponding to the case
where the external execution environment does support environment variables. In
this case, it shall provide a mechanism to initialize a nonempty set of
environment variables prior to the execution of a partition.]}
@end{ImplPerm}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@ChgAdded{Version=[2],Text=[If the execution environment supports subprocesses,
the currently defined environment variables should be used to initialize the
environment variables of a subprocess.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[If the execution environment supports subprocesses,
the current environment variables should be used to initialize the
environment variables of a subprocess.]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Changes to the environment variables made outside the control of this package
should be reflected immediately in the effect of the operations of this package.
Changes to the environment variables made using this package should be reflected
immediately in the external execution environment. This package should not
perform any buffering of the environment variables.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Changes to the environment variables made outside the control of
Environment_Variables should be reflected immediately.]}]}

@end{ImplAdvice}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
Package Environment_Variables is new.]}
@end{Extend95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0285-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
  A new overloaded function Value is added to Environment_Variables.
  If Environment_Variables is referenced in a @nt{use_clause}, and an entity
  @i<E> with the name Value is defined in a package that is also referenced in a
  @nt{use_clause}, the entity @i<E> may no longer be use-visible, resulting in
  errors. This should be rare and is easily fixed if it does occur.]}
@end{Incompatible2005}

