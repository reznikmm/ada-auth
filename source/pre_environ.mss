@comment{ $Source: e:\\cvsroot/ARM/Source/pre_environ.mss,v $ }
@comment{ $Revision: 1.1 $ $Date: 2004/12/07 05:17:08 $ $Author: Randy $ }
@Part(predefenviron, Root="ada.mss")

@Comment{$Date: 2004/12/07 05:17:08 $}

@LabeledAddedClause{Version=[2],Name=[The Package Environment_Variables]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@Chg{Version=[2],New=[@Defn{environment variable}
The package Environment_Variables allows a program to
read or modify the environment variables. Environment variables are name-value
pairs, where both the name and value are strings. The definition of what
constitutes an @i{environment variable}, and the meaning of the name and value,
are implementation defined.],Old=[]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@Chg{Version=[2],New=[The
definition and meaning of an environment variable.],Old=[]}]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@Chg{Version=[2],New=[@Leading@Keepnext@;The library package Environment_Variables
has the following declaration:],Old=[]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@key{package} Ada.Environment_Variables @key{is}@ChildUnit{Parent=[Ada],Child=[Environment_Variables]}
   @key{pragma} Preelaborate (Environment_Variables);],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[   @key{function} @AdaSubDefn{Value} (Name : @key{in} String) @key{return} String;],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[   @key{function} @AdaSubDefn{Exists} (Name : @key{in} String) @key{return} Boolean;],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[   @key{procedure} @AdaSubDefn{Set} (Name : @key{in} String; Value : @key{in} String);],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[   @key{procedure} @AdaSubDefn{Clear} (Name : @key{in} String);
   @key{procedure} @AdaSubDefn{Clear};],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[   @key{procedure} @AdaSubDefn{Iterate} (
       Process : @key{not null access procedure} (Name, Value : @key{in} String));],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@key{end} Ada.Environment_Variables;],Old=[]}
@end{Example}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@Keepnext@;@key{function} Value (Name : @key{in} String) @key{return} String;],Old=[]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@Chg{Version=[2],New=[If the external execution environment supports
environment variables, then Value returns the value of the environment variable
with the given name. If no environment variable with the given name exists,
then Constraint_Error is propagated. If the execution environment does not
support environment variables, then Program_Error is propagated.],Old=[]}

**** The rest of this clause has yet to be inserted ****

@end{DescribeCode}
@end{StaticSem}




@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00370-01]}
@Chg{Version=[2],New=[@Defn{extensions to Ada 95}
Package Ada.Environment_Variables is new.],Old=[]}
@end{Extend95}

