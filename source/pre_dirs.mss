@comment{ $Source: e:\\cvsroot/ARM/Source/pre_dirs.mss,v $ }
@comment{ $Revision: 1.2 $ $Date: 2004/12/08 01:09:47 $ $Author: Randy $ }
@Part(predefdirs, Root="ada.mss")

@Comment{$Date: 2004/12/08 01:09:47 $}

@LabeledAddedClause{Version=[2],Name=[The Package Directories]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@Chg{Version=[2],New=[The package Ada.Directories provides operations
for manipulating files and directories, and their names.],Old=[]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The library package
Directories has the following declaration:],Old=[]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@key{with} Ada.IO_Exceptions;
@key{with} Ada.Calendar;
@key{package} Ada.Directories @key{is}@ChildUnit{Parent=[Ada],Child=[Directories]}],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[   -- @RI[Directory and file operations:]],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[   @key{function} @AdaSubDefn{Current_Directory} @key{return} String;
   @key{procedure} @AdaSubDefn{Set_Directory} (Directory : in String);],Old=[]}


**** The rest of this clause has yet to be inserted ****

@end{Example}
@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@Chg{Version=[2],New=[@Defn{extensions to Ada 95}
Package Ada.Directories is new.],Old=[]}
@end{Extend95}

