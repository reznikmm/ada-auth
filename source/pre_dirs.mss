@comment{ $Source: e:\\cvsroot/ARM/Source/pre_dirs.mss,v $ }
@comment{ $Revision: 1.3 $ $Date: 2004/12/11 06:27:57 $ $Author: Randy $ }
@Part(predefdirs, Root="ada.mss")

@Comment{$Date: 2004/12/11 06:27:57 $}

@LabeledAddedClause{Version=[2],Name=[The Package Directories]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[The package Ada.Directories provides operations
for manipulating files and directories, and their names.]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The library package
Directories has the following declaration:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{with} Ada.IO_Exceptions;
@key{with} Ada.Calendar;
@key{package} Ada.Directories @key{is}@ChildUnit{Parent=[Ada],Child=[Directories]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[Directory and file operations:]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Current_Directory} @key{return} String;
   @key{procedure} @AdaSubDefn{Set_Directory} (Directory : in String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Create_Directory} (New_Directory : @key{in} String;
                               Form : @key{in} String := "");]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_Directory} (Directory : @key{in} String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Create_Path} (New_Directory : @key{in} String;
                          Form : @key{in} String := "");]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_Tree} (Directory : @key{in} String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_File} (Name : @key{in} String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Rename} (Old_Name, New_Name : @key{in} String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Copy_File} (Source_Name, Target_Name : @key{in} String;
                        Form : @key{in} String := "");]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI[File and directory name operations:]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Full_Name} (Name : @key{in} String) @key{return} String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Simple_Name} (Name : @key{in} String) @key{return} String;]}

**** The rest of this clause has yet to be inserted ****

@end{Example}
@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
Package Ada.Directories is new.]}
@end{Extend95}

