@comment{ $Source: e:\\cvsroot/ARM/Source/pre_dirs.mss,v $ }
@comment{ $Revision: 1.4 $ $Date: 2004/12/12 05:36:22 $ $Author: Randy $ }
@Part(predefdirs, Root="ada.mss")

@Comment{$Date: 2004/12/12 05:36:22 $}

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

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Containing_Directory} (Name : @key{in} String) @key{return} String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Extension} (Name : @key{in} String) @key{return} String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Base_Name} (Name : @key{in} String) @key{return} String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Compose} (Containing_Directory : @key{in} String := "";
                     Name : @key{in} String;
                     Extension : @key{in} String := "") @key{return} String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{File and directory queries:}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{File_Kind} @key{is} (Directory, Ordinary_File, Special_File);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{File_Size} @key{is range} 0 .. @RI{implementation-defined};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Exists} (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Kind} (Name : @key{in} String) @key{return} File_Kind;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Size} (Name : @key{in} String) @key{return} File_Size;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Modification_Time} (Name : @key{in} String) @key{return} Ada.Calendar.Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Directory searching:}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Directory_Entry_Type} @key{is limited private};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Filter_Type} @key{is array} (File_Kind) @key{of} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} @AdaTypeDefn{Search_Type} @key{is limited private};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Start_Search} (Search : @key{in out} Search_Type;
                           Directory : @key{in} String;
                           Pattern : @key{in} String;
                           Filter : @key{in} Filter_Type := (@key{others} => True));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{End_Search} (Search : @key{in out} Search_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{More_Entries} (Search : @key{in} Search_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Get_Next_Entry} (Search : @key{in out} Search_Type;
                             Directory_Entry : @key{out} Directory_Entry_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   -- @RI{Operations on Directory Entries:}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Simple_Name} (Directory_Entry : @key{in} Directory_Entry_Type)
       @key{return} String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Full_Name} (Directory_Entry : @key{in} Directory_Entry_Type)
       @key{return} String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Kind} (Directory_Entry : @key{in} Directory_Entry_Type)
       @key{return} File_Kind;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Size} (Directory_Entry : @key{in} Directory_Entry_Type)
       @key{return} File_Size;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Modification_Time} (Directory_Entry : @key{in} Directory_Entry_Type)
       @key{return} Ada.Calendar.Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @AdaDefn{Status_Error} : @key{exception renames} Ada.IO_Exceptions.Status_Error;
   @AdaDefn{Name_Error} : @key{exception renames} Ada.IO_Exceptions.Name_Error;
   @AdaDefn{Use_Error} : @key{exception renames} Ada.IO_Exceptions.Use_Error;
   @AdaDefn{Device_Error} : @key{exception renames} Ada.IO_Exceptions.Device_Error;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{private}
    -- @RI{Not specified by the language.}
@key{end} Ada.Directories;]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[External files may be classified as directories,
special files, or ordinary
files. A @i<directory> is an external file that is a container for files on
the target system. A @i<special file> is an external file that cannot be
created or read by a predefined Ada Input-Output package. External files that
are not special files or directories are called @i<ordinary files>.
@Defn{directory}
@Defn{special file}
@Defn{ordinary file}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[A @i<file name> is a string identifying an external
file. Similarly, a
@i<directory name> is a string identifying a directory. The interpretation of
file names and directory names is implementation-defined.
@Defn{directory name}
@Defn{file name}]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@Chg{Version=[2],New=[The
interpretation of file names and directory names.],Old=[]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[The @i<full name> of an external file is a
full specification of the name of
the file. If the external environment allows alternative specifications of the
name (for example, abbreviations), the full name should not use such
alternatives. A full name typically will include the names of all of
directories that contain the item. The @i<simple name> of an external file is
the name of the item, not including any containing directory names. Unless
otherwise specified, a file name or directory name parameter to a predefined
Ada input-output subprogram can be a full name, a simple name, or any other
form of name supported by the implementation.
@Defn2{Term=[full name],Sec=[of a file]}
@Defn2{Term=[simple name],Sec=[of a file]}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[The @i<default directory> is the directory that is
used if a directory or
file name is not a full name (that is, when the name does not fully identify
all of the containing directories).
@Defn{default directory}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[A @i<directory entry> is a single item in a
directory, identifying a single
external file (including directories and special files).
@Defn{directory entry}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[For each function that returns a string, the
lower bound of the returned value is 1.]}

**** The rest of this clause has yet to be inserted ****

@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
Package Ada.Directories is new.]}
@end{Extend95}

