@comment{ $Source: e:\\cvsroot/ARM/Source/pre_dirs.mss,v $ }
@comment{ $Revision: 1.49 $ $Date: 2012/11/28 23:53:05 $ $Author: randy $ }
@Part(predefdirs, Root="ada.mss")

@Comment{$Date: 2012/11/28 23:53:05 $}

@RMNewPageVer{Version=[2]}@Comment{For printed version of Ada 2005 RM}
@LabeledAddedClause{Version=[2],Name=[The Package Directories]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[The package Directories provides operations
for manipulating files and directories, and their names.]}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@ChgAdded{Version=[2],Text=[The notes for this @Chg{Version=[3],New=[subclause],Old=[clause]}
contain the expected
interpretations of some of the operations on various target systems.
@lquotes@;Unix@rquotes@; refers to the UNIX@latin1(174) operating system,
and in most cases also covers Unix-like systems such as Linux and POSIX.
@lquotes@;Windows@latin1(174)@rquotes@; refers to the Microsoft@latin1(174)
Windows@latin1(174) 2000 operating system and usually also covers most
other versions that use the Win32 API.]}
@end{Discussion}
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
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Current_Directory} @key{return} String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Set_Directory} (Directory : in String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Create_Directory} (New_Directory : @key{in} String;
                               Form          : @key{in} String := "");]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_Directory} (Directory : @key{in} String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Create_Path} (New_Directory : @key{in} String;
                          Form          : @key{in} String := "");]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_Tree} (Directory : @key{in} String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Delete_File} (Name : @key{in} String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Rename} (Old_Name, New_Name : @key{in} String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Copy_File} (Source_Name,
                        Target_Name : @key{in} String;
                        Form        : @key{in} String := "");]}

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
                     Name                 : @key{in} String;
                     Extension            : @key{in} String := "") @key{return} String;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0049-1]}
@ChgAdded{Version=[3],Text=[   @Key{type} @AdaTypeDefn{Name_Case_Kind} @key{is}
      (Unknown, Case_Sensitive, Case_Insensitive, Case_Preserving);]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0049-1]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Name_Case_Equivalence} (Name : @key{in} String) @key{return} Name_Case_Kind;]}

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
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Start_Search} (Search    : @key{in out} Search_Type;
                           Directory : @key{in} String;
                           Pattern   : @key{in} String;
                           Filter    : @key{in} Filter_Type := (@key{others} => True));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{End_Search} (Search : @key{in out} Search_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{More_Entries} (Search : @key{in} Search_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Get_Next_Entry} (Search : @key{in out} Search_Type;
                             Directory_Entry : @key{out} Directory_Entry_Type);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} Search (
      Directory : @key{in} String;
      Pattern   : @key{in} String;
      Filter    : @key{in} Filter_Type := (@key{others} => True);
      Process   : @key{not null access procedure} (
          Directory_Entry : @key{in} Directory_Entry_Type));]}

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
@ChgAdded{Version=[2],Text=[   @AdaExcDefn{Status_Error} : @key{exception renames} Ada.IO_Exceptions.Status_Error;
   @AdaExcDefn{Name_Error}   : @key{exception renames} Ada.IO_Exceptions.Name_Error;
   @AdaExcDefn{Use_Error}    : @key{exception renames} Ada.IO_Exceptions.Use_Error;
   @AdaExcDefn{Device_Error} : @key{exception renames} Ada.IO_Exceptions.Device_Error;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
@ChgAdded{Version=[2],Text=[@key{private}
    @Chg{Version=[3],New=[... ],Old=[]}-- @RI{@Chg{Version=[3],New=[not],Old=[Not]} specified by the language@Chg{Version=[3],New=[],Old=[.]}}
@key{end} Ada.Directories;]}

@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[External files may be classified as directories,
special files, or ordinary
files. A @i<directory> is an external file that is a container for files on
the target system. A @i<special file> is an external file that cannot be
created or read by a predefined Ada input-output package. External files that
are not special files or directories are called @i<ordinary files>.
@Defn{directory}
@Defn{special file}
@Defn{ordinary file}]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[A directory is an external file, although it may
  not have a name on some targets. A directory is not a special file, as it
  can be created and read by Directories.]}
@end{Ramification}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Devices and soft links are examples of
  special files on Windows@latin1(174) and Unix.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Even if an implementation provides a package
  to create and read soft links, such links are still special files.]}
@end{Discussion}

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
alternatives. A full name typically will include the names of all of the
directories that contain the item. The @i<simple name> of an external file is
the name of the item, not including any containing directory names. Unless
otherwise specified, a file name or directory name parameter in a call to
a predefined Ada input-output subprogram can be a full name, a simple name,
or any other form of name supported by the implementation.
@Defn2{Term=[full name],Sec=[of a file]}
@Defn2{Term=[simple name],Sec=[of a file]}]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The full name on Unix is a complete path to the
   root. For Windows@latin1(174), the full name includes a complete path, as well as a disk
   name ("C:") or network share name. For both systems, the simple name is
   the part of the name following the last '/' (or '\' for Windows@latin1(174)). For
   example, in the name "/usr/randy/ada-directories.ads",
   "ada-directories.ads" is the simple name.]}
@end{Discussion}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[It is possible for a file or directory name to be
    neither a full name nor a simple name. For instance, the Unix name
    "../parent/myfile" is neither a full name nor a simple name.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[The @i<default directory> is the directory that is
used if a directory or
file name is not a full name (that is, when the name does not fully identify
all of the containing directories).
@Defn{default directory}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The default directory is the one maintained by
   the familiar @lquotes@;cd@rquotes@; command on Unix and Windows@latin1(174). Note that
   Windows@latin1(174) maintains
   separate default directories for each disk drive; implementations should
   use the natural implementation.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[A @i<directory entry> is a single item in a
directory, identifying a single
external file (including directories and special files).
@Defn{directory entry}]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[For each function that returns a string, the
lower bound of the returned value is 1.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Type=[Leading],
Text=[The following file and directory operations are provided:]}

@begin{DescribeCode}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Current_Directory @key{return} String;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the full directory name
for the current default directory.
The name returned shall be suitable for a future call to Set_Directory.
The exception Use_Error is propagated if a default directory is not
supported by the external environment.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Set_Directory (Directory : @key{in} String);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Sets the current default directory.
The exception Name_Error is
propagated if the string given as Directory does not identify an existing
directory. The exception Use_Error is propagated if the external environment
does not support making Directory (in the absence of Name_Error) a default
directory.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Create_Directory (New_Directory : @key{in} String;
                            Form          : @key{in} String := "");]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Creates a directory with name
New_Directory. The Form parameter can be
used to give system-dependent characteristics of the directory; the
interpretation of the Form parameter is implementation-defined. A null string
for Form specifies the use of the default options of the implementation of the
new directory. The exception Name_Error is propagated if the string given as
New_Directory does not allow the identification of a directory. The exception
Use_Error is propagated if the external environment does not support the
creation of a directory with the given name (in the absence of Name_Error) and
form.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Delete_Directory (Directory : @key{in} String);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0231-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Deletes an existing empty directory
with name Directory. The exception
Name_Error is propagated if the string given as Directory does not identify an
existing directory. The exception Use_Error is propagated if the
@Chg{Version=[3],New=[directory is not empty or the ],Old=[]}external
environment does not support the deletion of the directory
@Chg{Version=[3],New=[],Old=[(or some portion of its contents) ]}with
the given name (in the absence of Name_Error).]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Create_Path (New_Directory : @key{in} String;
                       Form          : @key{in} String := "");]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0271-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Creates zero or more directories with
name New_Directory. Each
nonexistent directory named by New_Directory is created.@Redundant[ For example, on a
typical Unix system, Create_Path ("/usr/me/my"); would create directory "me" in
directory "usr", then create directory "my" in directory "me".] The Form parameter
can be used to give system-dependent characteristics of the directory; the
interpretation of the Form parameter is implementation-defined. A null string
for Form specifies the use of the default options of the implementation of the
new directory. The exception Name_Error is propagated if the string given as
New_Directory does not allow the identification of any directory. The exception
Use_Error is propagated if the external environment does not support the
creation of any directories with the given name (in the absence of Name_Error)
and form.@Chg{Version=[3],New=[ If Use_Error is propagated, it is unspecified
whether a portion of the directory path is created.],Old=[]}]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Delete_Tree (Directory : @key{in} String);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Deletes an existing directory with
name Directory. The directory and
all of its contents (possibly including other directories) are deleted. The
exception Name_Error is propagated if the string given as Directory does not
identify an existing directory. The exception Use_Error is propagated if the
external environment does not support the deletion of the directory or some
portion of its contents with the given name (in the absence of Name_Error). If
Use_Error is propagated, it is unspecified whether a portion of the contents of
the directory is deleted.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Delete_File (Name : @key{in} String);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Deletes an existing ordinary or
special file with name Name. The exception
Name_Error is propagated if the string given as Name does not identify an
existing ordinary or special external file. The exception Use_Error is
propagated if the external environment does not support the deletion of the
file with the given name (in the absence of Name_Error).]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Rename (Old_Name, New_Name : @key{in} String);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0231-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Renames an existing external file
(including directories) with name Old_Name
to New_Name. The exception Name_Error is propagated if the string given as
Old_Name does not identify an existing external file@Chg{Version=[3],New=[
or if the string given as New_Name does not allow the identification
of an external file],Old=[]}. The exception Use_Error
is propagated if the external environment does not support the renaming of the
file with the given name (in the absence of Name_Error). In particular,
Use_Error is propagated if a file or directory already exists with name
New_Name.]}

@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This operation is expected to work within a
  single directory, and implementers are encouraged to support it across
  directories on a single device. Copying files from one device to another
  is discouraged (that's what Copy_File is for). However, there is no
  requirement to detect file copying by the target system. If the target
  system has an API that gives that for @lquotes@;free@rquotes, it can be
  used. For Windows@latin1(174), for instance, MoveFile can be used to
  implement Rename.]}
@end{ImplNote}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Copy_File (Source_Name,
                     Target_Name : @key{in} String;
                     Form        : @key{in} String@Chg{Version=[3],New=[ := ""],Old=[]});]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0271-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Copies the contents of the existing
external file with name Source_Name to an external file with name Target_Name.
The resulting external file is a duplicate of the source external file. The
Form parameter can be used to give system-dependent characteristics of the
resulting external file; the interpretation of the Form parameter is
implementation-defined. Exception Name_Error is propagated if the string given
as Source_Name does not identify an existing external ordinary or special file,
or if the string given as Target_Name does not allow the identification of an
external file. The exception Use_Error is propagated if the external
environment does not support creating the file with the name given by
Target_Name and form given by Form, or copying of the file with the name given
by Source_Name (in the absence of Name_Error).@Chg{Version=[3],New=[ If
Use_Error is propagated, it is unspecified whether a portion of the file
is copied.],Old=[]}]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Name_Error is always raised if Source_Name
  identifies a directory. It is up to the implementation whether
  special files can be copied, or if Use_Error will be raised.]}
@end{Ramification}

@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The following file and directory
name operations are provided:]}

@begin{DescribeCode}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Full_Name (Name : @key{in} String) @key{return} String;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the full name corresponding
to the file name specified by
Name. The exception Name_Error is propagated if the string given as Name does
not allow the identification of an external file (including directories and
special files).]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Full name means that no abbreviations are
  used in the returned name, and that it is a full specification of the name.
  Thus, for Unix and Windows@latin1(174), the result should be a full path that does not
  contain any "." or ".." directories. Typically, the default directory
  is used to fill in any missing information.]}
@end{Discussion}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Simple_Name (Name : @key{in} String) @key{return} String;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the simple name portion of
the file name specified by Name.
The exception Name_Error is propagated if the string given as Name does not
allow the identification of an external file (including directories and special
files).]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Containing_Directory (Name : @key{in} String) @key{return} String;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the name of the containing
directory of the external file
(including directories) identified by Name. (If more than one directory can
contain Name, the directory name returned is implementation-defined.) The
exception Name_Error is propagated if the string given as Name does not allow
the identification of an external file. The exception Use_Error is propagated
if the external file does not have a containing directory.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This is purely a string manipulation function.
  If Name is not given as a full name, the containing directory probably
  won't be one, either. For example, if Containing_Directory ("..\AARM\RM-A-8")
  is called on Windows@latin1(174), the result should be "..\AARM". If there is no
  path at all on the name, the result should be "." (which represents the
  current directory). Use Full_Name on the result of Containing_Directory
  if the full name is needed.]}
@end{Discussion}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Extension (Name : @key{in} String) @key{return} String;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the extension name
corresponding to Name. The extension name
is a portion of a simple name (not including any separator characters),
typically used to identify the file class. If the external environment does not
have extension names, then the null string is returned. The exception
Name_Error is propagated if the string given as Name does not allow the
identification of an external file.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[For Unix and Windows@latin1(174), the extension is the
  portion of the simple name following the rightmost period. For example,
  in the simple name "RM-A-8.html", the extension is "html".]}
@end{Discussion}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Base_Name (Name : @key{in} String) @key{return} String;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the base name corresponding
to Name. The base name is the
remainder of a simple name after removing any extension and extension
separators. The exception Name_Error is propagated if the string given as Name
does not allow the identification of an external file (including directories and
special files).]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[For Unix and Windows@latin1(174), the base name is the
  portion of the simple name preceding the rightmost period (except for the
  special directory names "." and "..", whose Base_Name is "." and "..").
  For example, in the simple name "RM-A-8.html", the base name is "RM-A-8".]}
@end{Discussion}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Compose (Containing_Directory : @key{in} String := "";
                  Name                 : @key{in} String;
                  Extension            : @key{in} String := "") @key{return} String;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the name of the external
file with the specified
Containing_Directory, Name, and Extension. If Extension is the null string,
then Name is interpreted as a simple name;
otherwise@Chg{Version=[3],New=[,],Old=[]} Name is interpreted as a
base name. The exception Name_Error is propagated if the string given as
Containing_Directory is not null and does not allow the identification of a
directory, or if the string given as Extension is not null and is not a
possible extension, or if the string given as Name is not a possible simple
name (if Extension is null) or base name (if Extension is nonnull).]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The above definition implies that if
  the Extension is null, for Unix and Windows@latin1(174) no '.' is added to Name.]}
@end{Ramification}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If Name is null, Name_Error should be raised,
  as nothing is not a possible simple name or base name.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Generally, Compose(Containing_Directory(F),
  Base_Name(F),Extension(F)) = F. However, this is not true on Unix or
  Windows@latin1(174) for file names that end with a '.';
  Compose(Base_Name("Fooey."),Extension("Fooey.")) = "Fooey".
  This is not a problem for Windows@latin1(174), as the names have the same meaning with
  or without the '.', but these are different names for Unix. Thus,
  care needs to be taken on Unix; if Extension is null, Base_Name should
  be avoided. (That's not usually a problem with file names generated by a
  program.)]}
@end{Discussion}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0049-1]}
@ChgAdded{Version=[3],Keepnext=[T],Text=[@key{function} Name_Case_Equivalence (Name : @key{in} String) @key{return} Name_Case_Kind;]}
@end{Example}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0049-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[Returns the file name equivalence rule for the
directory containing Name. Raises Name_Error if Name is not a full name. Returns
Case_Sensitive if file names that differ only in the case of letters are
considered different names. If file names that differ only in the case of
letters are considered the same name, then Case_Preserving is returned if
names have the case of the file name used when a file is created; and
Case_Insensitive is returned otherwise. Returns Unknown if the file name
equivalence is not known.]}
@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[Unix, Linux, and their relatives are Case_Sensitive
  systems. Microsoft@latin1(174) Windows@latin1(174) is a Case_Preserving system
  (unless the rarely used POSIX mode is used). Ancient systems like CP/M and
  early MS-DOS were Case_Insensitive systems (file names were always in UPPER
  CASE). Unknown is provided in case it is impossible to tell (such as could be
  the case for network files).]}
@end{ImplNote}
@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The following file and directory
queries and types are provided:]}

@begin{DescribeCode}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{type} File_Kind @key{is} (Directory, Ordinary_File, Special_File);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[The type File_Kind represents the
kind of file represented by an external file or directory.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{type} File_Size @key{is range} 0 .. @RI<implementation-defined>;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[The type File_Size represents the
size of an external file.]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@Chg{Version=[2],New=[The
maximum value for a file size in Directories.],Old=[]}]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Exists (Name : @key{in} String) @key{return} Boolean;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns True if an external file
represented by Name exists, and False
otherwise. The exception Name_Error is propagated if the string given as Name
does not allow the identification of an external file (including directories
and special files).]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Kind (Name : @key{in} String) @key{return} File_Kind;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the kind of
external file represented by Name. The exception
Name_Error is propagated if the string given as Name does not allow the
identification of an existing external file.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Size (Name : @key{in} String) @key{return} File_Size;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the size of the
external file represented by Name. The size of
an external file is the number of stream elements contained in the file.
If the external file is not an ordinary file, the
result is implementation-defined. The exception Name_Error is propagated if the
string given as Name does not allow the identification of an existing external
file. The exception Constraint_Error is propagated if the file size is not a
value of type File_Size.]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@Chg{Version=[2],New=[The
result for Directories.Size for a directory or special file],Old=[]}]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Trailing],Text=[We allow raising Constraint_Error,
  so that an implementation for a system with 64-bit file sizes does not
  need to support full numerics on 64-bit integers just to implement
  this package. Of course, if 64-bit integers are available on such a system,
  they should be used when defining type File_Size.]}
@end{Discussion}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Modification_Time (Name : @key{in} String) @key{return} Ada.Calendar.Time;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the time that the
external file represented by Name was most
recently modified. If the external file is not an ordinary file, the result is
implementation-defined. The exception Name_Error is propagated if the string
given as Name does not allow the identification of an existing external file.
The exception Use_Error is propagated if the external environment does not
support reading the modification time of the file with the name given by
Name (in the absence of Name_Error).]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@Chg{Version=[2],New=[The
result for Directories.Modification_Time for a directory or special file.],
Old=[]}]}

@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The following directory searching
operations and types are provided:]}

@begin{DescribeCode}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{type} Directory_Entry_Type @key{is limited private};]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[The type Directory_Entry_Type represents a single item in a directory.
These items can only be created by the Get_Next_Entry procedure in this
package. Information about the item can be obtained from the functions declared
in this package. A default-initialized object of this type is invalid; objects
returned from Get_Next_Entry are valid.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{type} Filter_Type @key{is array} (File_Kind) @key{of} Boolean;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[The type Filter_Type specifies which directory entries are provided
from a search operation. If the Directory component is True, directory entries
representing directories are provided. If the Ordinary_File component is True,
directory entries representing ordinary files are provided. If the Special_File
component is True, directory entries representing special files are provided.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{type} Search_Type @key{is limited private};]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[The type Search_Type contains the
state of a directory search. A
default-initialized Search_Type object has no entries available (function
More_Entries returns False). Type Search_Type
needs finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Start_Search (Search    : @key{in out} Search_Type;
                        Directory : @key{in} String;
                        Pattern   : @key{in} String;
                        Filter    : @key{in} Filter_Type := (@key{others} => True));]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Starts a search
in the directory named by
Directory for entries matching Pattern@Chg{Version=[3],New=[ and Filter],Old=[]}.
Pattern represents a pattern for matching file names. If Pattern is
@Chg{Version=[3],New=[the ],Old=[]}null@Chg{Version=[3],New=[ string],Old=[]},
all items in the directory are matched; otherwise,
the interpretation of Pattern is implementation-defined. Only items that match
Filter will be returned. After a successful call on Start_Search, the object
Search may have entries available, but it may have no entries available if no
files or directories match Pattern and Filter. The exception Name_Error is
propagated if the string given by Directory does not identify an existing
directory, or if Pattern does not allow the identification of any possible
external file or directory. The exception Use_Error is propagated if the
external environment does not support the searching of the directory with the
given name (in the absence of Name_Error). When Start_Search propagates
Name_Error or Use_Error, the object Search will have no entries available.]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@Chg{Version=[2],New=[The
interpretation of a nonnull search pattern in Directories.],Old=[]}]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} End_Search (Search : @key{in out} Search_Type);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Ends the search represented
by Search. After a successful call on
End_Search, the object Search will have no entries available.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The only way that a call to End_Search could be
  unsuccessful if Device_Error (see @RefSecNum{Exceptions in Input-Output}) is
  raised because of an underlying failure (or bug).]}
@end{Ramification}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} More_Entries (Search : @key{in} Search_Type) @key{return} Boolean;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns True if more entries are
available to be returned by a call
to Get_Next_Entry for the specified search object, and False otherwise.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Get_Next_Entry (Search : @key{in out} Search_Type;
                          Directory_Entry : @key{out} Directory_Entry_Type);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the next Directory_Entry
for the search described by Search
that matches the pattern and filter. If no further matches are available,
Status_Error is raised. It is implementation-defined as to whether the results
returned by this @Chg{Version=[3],New=[subprogram],Old=[routine]} are altered
if the contents of the directory are
altered while the Search object is valid (for example, by another program). The
exception Use_Error is propagated if the external environment does not support
continued searching of the directory represented by Search.]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@Chg{Version=[2],New=[The
results of a Directories search if the contents of the directory are
altered while a search is in progress.],Old=[]}]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Search (
    Directory : @key{in} String;
    Pattern   : @key{in} String;
    Filter    : @key{in} Filter_Type := (@key{others} => True);
    Process   : @key{not null access procedure} (
        Directory_Entry : @key{in} Directory_Entry_Type));]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Searches in the directory named by
Directory for entries matching Pattern@Chg{Version=[3],New=[ and Filter],Old=[]}.
The subprogram designated by Process is called with each matching entry in
turn. Pattern represents a pattern for matching file names. If Pattern is
@Chg{Version=[3],New=[the ],Old=[]}null@Chg{Version=[3],New=[ string],Old=[]},
all items in the directory are matched;
otherwise, the interpretation of Pattern is implementation-defined. Only
items that match Filter will be returned.
The exception Name_Error is propagated if the string given by Directory
does not identify an existing directory, or if Pattern does not allow the
identification of any possible external file or directory. The exception
Use_Error is propagated if the external environment does not support the
searching of the directory with the given name (in the absence of
Name_Error).]}
@Comment{The implementation-defined case is handled above.}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[@lquotes@;In turn@rquotes means that the
  calls to the subprogram designated by Process are not made in parallel;
  they can be made in any order but must be in sequence.]}
@end{Discussion}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Simple_Name (Directory_Entry : @key{in} Directory_Entry_Type)
     @key{return} String;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the simple external name
of the external file (including
directories) represented by Directory_Entry. The format of the name returned is
implementation-defined. The exception Status_Error is propagated if
Directory_Entry is invalid.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Full_Name (Directory_Entry : @key{in} Directory_Entry_Type)
     @key{return} String;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the full external name of
the external file (including
directories) represented by Directory_Entry. The format of the name returned is
implementation-defined. The exception Status_Error is propagated if
Directory_Entry is invalid.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Kind (Directory_Entry : @key{in} Directory_Entry_Type)
     @key{return} File_Kind;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the kind of external file
represented by Directory_Entry. The
exception Status_Error is propagated if Directory_Entry is invalid.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Size (Directory_Entry : @key{in} Directory_Entry_Type)
     @key{return} File_Size;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the size of the external
file represented by Directory_Entry.
The size of an external file is the number of stream elements contained in
the file. If the external file represented by
Directory_Entry is not an ordinary file, the result is implementation-defined.
The exception Status_Error is propagated if Directory_Entry is invalid. The
exception Constraint_Error is propagated if the file size is not a value of
type File_Size.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Modification_Time (Directory_Entry : @key{in} Directory_Entry_Type)
     @key{return} Ada.Calendar.Time;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the time that the external
file represented by Directory_Entry
was most recently modified. If the external file represented by Directory_Entry
is not an ordinary file, the result is implementation-defined. The exception
Status_Error is propagated if Directory_Entry is invalid. The exception
Use_Error is propagated if the external environment does not support
reading the modification time of the file represented by Directory_Entry.]}

@end{DescribeCode}

@end{StaticSem}

@begin{ImplReq}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[For Copy_File, if Source_Name identifies an
existing external ordinary file created by a predefined Ada input-output
package, and Target_Name and Form can be used in the Create operation of that
input-output package with mode Out_File without raising an exception, then
Copy_File shall not propagate Use_Error.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This means that Copy_File will copy any file
  that the Ada programmer could copy (by writing some possibly complicated
  Ada code).]}
@end{Discussion}
@end{ImplReq}

@begin{ImplAdvice}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[If other information about a file
(such as the owner or creation date) is available in a directory entry,
the implementation
should provide functions in a child package Directories.Information to
retrieve it.@ChildUnit{Parent=[Ada.Directories],Child=[Information]}]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Package Directories.Information should be provided to retrieve
other information about a file.]}]}

@begin{ImplNote}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For Windows@latin1(174),
Directories.Information should contain at least the following routines:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} Ada.Directories.Information @key{is}
    -- @RI[System-specific directory information.]
    -- @RI[Version for the Microsoft@latin1(174) Windows@latin1(174) operating system.]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Creation_Time (Name : @key{in} String) @key{return} Ada.Calendar.Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Last_Access_Time (Name : @key{in} String) @key{return} Ada.Calendar.Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Read_Only (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Needs_Archiving (Name : @key{in} String) @key{return} Boolean;
        -- @RI[This generally means that the file needs to be backed up.]
        -- @RI[The flag is only cleared by backup programs.]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Compressed (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Encrypted (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Hidden (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_System (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Offline (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Temporary (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Sparse (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Not_Indexed (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Creation_Time (Directory_Entry : @key{in} Directory_Entry_Type)
         @key{return} Ada.Calendar.Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Last_Access_Time (Directory_Entry : @key{in} Directory_Entry_Type)
         @key{return} Ada.Calendar.Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Read_Only (Directory_Entry : @key{in} Directory_Entry_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Needs_Archiving (Directory_Entry : @key{in} Directory_Entry_Type) @key{return} Boolean;
        -- @RI[This generally means that the file needs to be backed up.]
        -- @RI[The flag is only cleared by backup programs.]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Compressed (Directory_Entry : @key{in} Directory_Entry_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Encrypted (Directory_Entry : @key{in} Directory_Entry_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Hidden (Directory_Entry : @key{in} Directory_Entry_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_System (Directory_Entry : @key{in} Directory_Entry_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Offline (Directory_Entry : @key{in} Directory_Entry_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Temporary (Directory_Entry : @key{in} Directory_Entry_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Sparse (Directory_Entry : @key{in} Directory_Entry_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Not_Indexed (Directory_Entry : @key{in} Directory_Entry_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    -- @RI[Additional implementation-defined subprograms allowed here.]
@key{end} Ada.Directories.Information;]}
@end{Example}


@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For Unix-like systems (Unix,
POSIX, Linux, etc.), Directories.Information should contain at least the
following routines:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} Ada.Directories.Information @key{is}
    -- @RI[System-specific directory information.]
    -- @RI[Unix and similar systems version.]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Last_Access_Time (Name : @key{in} String) @key{return} Ada.Calendar.Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Last_Status_Change_Time (Name : @key{in} String) @key{return} Ada.Calendar.Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{type} Permission @key{is}
      (Others_Execute, Others_Write, Others_Read,
       Group_Execute,  Group_Write,  Group_Read,
       Owner_Execute,  Owner_Write,  Owner_Read,
       Set_Group_ID,   Set_User_ID);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{type} Permission_Set_Type @key{is array} (Permission) @key{of} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Permission_Set (Name : @key{in} String) @key{return} Permission_Set_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Owner (Name : @key{in} String) @key{return} String;
        -- @RI[Returns the image of the User_Id. If a definition of User_Id]
        -- @RI[is available, an implementation-defined version of Owner]
        -- @RI[returning User_Id should also be defined.]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
@ChgAdded{Version=[2],Text=[    @key{function} Group (Name : @key{in} String) @key{return} String;
        -- @RI[Returns the image of the @Chg{Version=[3],New=[Group_Id],Old=[User_Id]}. If a definition of Group_Id]
        -- @RI[is available, an implementation-defined version of Group]
        -- @RI[returning Group_Id should also be defined.]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Block_Special_File (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Character_Special_File (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_FIFO (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Symbolic_Link (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Socket (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Last_Access_Time (Directory_Entry : @key{in} Directory_Entry_Type)
       @key{return} Ada.Calendar.Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Last_Status_Change_Time (Directory_Entry : @key{in} Directory_Entry_Type)
       @key{return} Ada.Calendar.Time;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Permission_Set (Directory_Entry : @key{in} Directory_Entry_Type)
       @key{return} Permission_Set_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Owner (Directory_Entry : @key{in} Directory_Entry_Type) @key{return} String;
       -- @RI[See Owner above.]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Group (Directory_Entry : @key{in} Directory_Entry_Type) @key{return} String;
       -- @RI[See Group above.]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Block_Special_File (Directory_Entry : @key{in} Directory_Entry_Type)
       @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Character_Special_File (Directory_Entry : @key{in} Directory_Entry_Type)
       @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_FIFO (Directory_Entry : @key{in} Directory_Entry_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Symbolic_Link (Directory_Entry : @key{in} Directory_Entry_Type)
       @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    @key{function} Is_Socket (Directory_Entry : @key{in} Directory_Entry_Type) @key{return} Boolean;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[    -- @RI[Additional implementation-defined subprograms allowed here.]
@key{end} Ada.Directories.Information;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[We give these definitions to give guidance so that
every implementation for a given target is not unnecessarily different.
Implementers are encouraged to make packages for other targets as similar to
these as possible.]}

@end{ImplNote}


@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0231-1]}
@ChgAdded{Version=[2],Text=[Start_Search and Search should raise
@Chg{Version=[3],New=[Name_Error],Old=[Use_Error]} if Pattern is malformed, but
not if it could represent a file in the directory but does not actually do so.]}

@ChgImplAdvice{Version=[3],Kind=[Revised],InitialVersion=[2],
Text=[@ChgAdded{Version=[2],
Text=[Directories.Start_Search and Directories.Search should raise
@Chg{Version=[3],New=[Name_Error],Old=[Use_Error]} for malformed patterns.]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Rename should be supported at least when both
New_Name and Old_Name are simple names and New_Name does not identify an
existing external file.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Directories.Rename should be supported at least when both New_Name and
Old_Name are simple names and New_Name does not identify an existing
external file.]}]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[@Lquotes@;Supported@rquotes@; includes raising
  an exception if either name is malformed, the file to rename doesn't exist,
  insufficient permission for the operation exists, or similar problems. But
  this advice requires implementations to document what they do, and tells
  implementers that simply raising Use_Error isn't acceptable.]}
@end{Discussion}

@end{ImplAdvice}

@begin{Notes}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The operations Containing_Directory,
Full_Name, Simple_Name,
Base_Name, Extension, and Compose operate on file names, not external files.
The files identified by these operations do not need to exist. Name_Error is
raised only if the file name is malformed and cannot possibly identify a file.
Of these operations, only the result of Full_Name depends on the current
default directory; the result of the others depends only on their parameters.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Using access types, values of Search_Type and
Directory_Entry_Type can be
saved and queried later. However, another task or application can modify or
delete the file represented by a Directory_Entry_Type value or the directory
represented by a Search_Type value; such a value can only give the information
valid at the time it is created. Therefore, long-term storage of these values
is not recommended.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[If the target system does not support directories
inside of directories, then Kind will never return Directory and
Containing_Directory will always raise Use_Error.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[If the target system does not support creation or
deletion of directories, then Create_Directory, Create_Path, Delete_Directory,
and Delete_Tree will always propagate Use_Error.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[To move a file or directory to a different
location, use Rename. Most target systems will allow renaming of files from one
directory to another. If the target file or directory might already exist, it
should be deleted first.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[While Rename is only guaranteed to
  work for name changes within a single directory, its unlikely that
  implementers would purposely prevent functionality present in the underlying
  system from working. To move a file totally portably, it's necessary to
  handle failure of the Rename and fall back to Copy_File and Delete:]}
@begin{Example}
@ChgAdded{Version=[2],Text=[@key[begin]
   Rename (Source, Target);
@key[exception]
   @key[when] Use_Error =>
      Copy_File (Source, Target);
      Delete (Source);
@key[end];]}
@end{Example}
@end{Discussion}
@end{Notes}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Package Ada.Directories is new.]}
@end{Extend95}

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0231-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}@b<Correction>:
  Clarified when and which
  exceptions are raised for Start_Search, Search, Delete_Directory, and
  Rename. If an implementation followed the original incorrect wording, it might
  raise Use_Error instead of Name_Error for Start_Search and Search,
  Name_Error instead of Use_Error for Rename, and might have deleted a
  nonempty directory instead of raising Use_Error for Delete_Directory.
  The first two cases are very unlikely to matter in practice, and it unlikely
  that an implementation would have followed the latter implementation
  strategy, as it would be more work and would make Delete_Directory
  identical to Delete_Tree (which is obvious nonsense).]}
@end{Inconsistent2005}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0049-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
  A new enumeration type Name_Case_Kind and a new function
  Name_Case_Equivalence is added to Directories. If Directories
  is referenced in a @nt{use_clause}, and an
  entity @i<E> with a @nt{defining_identifier} of one of the new entities is
  defined in a package that is also referenced in a @nt{use_clause}, the entity
  @i<E> may no longer be use-visible, resulting in errors. This should be rare
  and is easily fixed if it does occur.]}
@end{Incompatible2005}

@begin{Diffword2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0271-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> We now explicitly say that
  the behavior of Create_Path and Copy_File is unspecified when Use_Error
  is raised. Nothing has changed here, as the behavior was (implicitly)
  unspecified in the 2007 Amendment.]}
@end{Diffword2005}



@LabeledAddedSubClause{Version=[3],Name=[The Package Directories.Hierarchical_File_Names]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0049-1]}
@ChgAdded{Version=[3],Text=[The library package Directories.Hierarchical_File_Names is an optional package
providing operations for file name construction and decomposition for
targets with hierarchical file naming.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0049-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[If provided, the library package
Directories.Hierarchical_File_Names has the following declaration:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{package} Ada.Directories.Hierarchical_File_Names @key{is}@ChildUnit{Parent=[Ada.Directories],Child=[Hierarchical_File_Names]}]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Is_Simple_Name} (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Is_Root_Directory_Name} (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Is_Parent_Directory_Name} (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Is_Current_Directory_Name} (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Is_Full_Name} (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Is_Relative_Name} (Name : @key{in} String) @key{return} Boolean;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Simple_Name} (Name : @key{in} String) @key{return} String
      @key{renames} Ada.Directories.Simple_Name;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Containing_Directory} (Name : @key{in} String) @key{return} String
      @key{renames} Ada.Directories.Containing_Directory;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Initial_Directory} (Name : @key{in} String) @key{return} String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Relative_Name} (Name : @key{in} String) @key{return} String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key{function} @AdaSubDefn{Compose} (Directory      : @key{in} String := "";
                     Relative_Name  : @key{in} String;
                     Extension      : @key{in} String := "") @key{return} String;]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{end} Ada.Directories.Hierarchical_File_Names;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0049-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[In addition to the operations provided in package
Directories.Hierarchical_File_Names, the operations in package Directories can
be used with hierarchical file names. In particular, functions Full_Name,
Base_Name, and Extension provide additional capabilities for hierarchical
file names.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{function} Is_Simple_Name (Name : @key{in} String) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Returns True if Name is a simple name, and returns
False otherwise.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{function} Is_Root_Directory_Name (Name : @key{in} String) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Returns True if Name is syntactically a root (a
directory that cannot be decomposed further), and returns False otherwise.]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[For Unix and Unix-like systems, "/" is the root.
  For Windows, "C:\" and "\\Computer\Share" are roots.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{function} Is_Parent_Directory_Name (Name : @key{in} String) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Returns True if Name can be used to indicate
symbolically the parent directory of any directory, and returns False
otherwise.]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Is_Parent_Directory_Name returns True if and only
  if Name is ".." for both Unix and Windows.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{function} Is_Current_Directory_Name (Name : @key{in} String) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Returns True if Name can be used to indicate
symbolically the directory itself for any directory, and returns False
otherwise.]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Is_Current_Directory_Name returns True if and only
  if Name is "." for both Unix and Windows.]}
@end{ImplNote}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{function} Is_Full_Name (Name : @key{in} String) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Returns True if the leftmost directory part of Name
is a root, and returns False otherwise.]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{function} Is_Relative_Name (Name : @key{in} String) @key{return} Boolean;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0049-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[Returns True if Name allows the identification of an
external file (including directories and special files) but is not a full name,
and returns False otherwise.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Relative names include simple names as a special case.
  This function returns False if the syntax of the name is incorrect.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{function} Initial_Directory (Name : @key{in} String) @key{return} String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0049-1],ARef=[AI05-0248-1]}
@ChgAdded{Version=[3],Text=[Returns the leftmost directory
part in Name. @Redundant[That is, it returns a root directory name (for a full
name), or one of a parent directory name, a current directory name, or a simple
name (for a relative name).] The exception Name_Error is propagated if the
string given as Name does not allow the identification of an external file
(including directories and special files).]}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{function} Relative_Name (Name : @key{in} String) @key{return} String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Returns the entire file name except
the Initial_Directory portion. The exception Name_Error is propagated if the
string given as Name does not allow the identification of an external file
(including directories and special files), or if Name has a single part (this
includes if any of Is_Simple_Name, Is_Root_Directory_Name,
Is_Parent_Directory_Name, or Is_Current_Directory_Name are True).]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The result might be a simple name.]}
@end{Ramification}

@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@key{function} Compose (Directory      : @key{in} String := "";
                  Relative_Name  : @key{in} String;
                  Extension      : @key{in} String := "") @key{return} String;]}
@end{Example}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Returns the name of the external file with the
specified Directory, Relative_Name, and Extension. The exception Name_Error is
propagated if the string given as Directory is not the null string and does
not allow the identification of a directory, or if Is_Relative_Name
(Relative_Name) is False, or if the string given as Extension is not the null
string and is not a possible extension, or if Extension is not the null string
and Simple_Name (Relative_Name) is not a base name.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[The result of Compose is a full name if Is_Full_Name
(Directory) is True; result is a relative name otherwise.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Name_Error is raised by Compose if Directory is
  not the null string, and both Is_Full_Name and Is_Relative_Name return
  False.]}
@end{Ramification}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[A common security problem is to include a parent
  directory name in the middle of a file name; this is often used to navigate
  outside of an intended root directory. We considered attempting to prevent
  that case by having Compose detect it and raise an exception. But the extra
  rules necessary were more confusing than helpful.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We can say more about the details of these
  operations by adopting the notation of a subscript to specify how many path
  fragments a particular result has. Then, we can abbreviate "Full Name" as
  "Full" and "Relative Name" as "Rel". In this notation, Unix file name "a/b" is
  a Rel(2), "../c/d" is a Rel(3), and "/a/b" is a Full(2). Rel(1) is equivalent
  to a simple name; thus we don't have to describe that separately.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[In this notation,]}
  @begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[For N>1,
Containing_Directory(Rel(N)) = Leftmost Rel(N-1),
Containing_Directory(Full(N)) = Leftmost Full(N-1),
Else if N = 1, raise Name_Error.]}
  @end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[Similarly,]}
  @begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[For N>1,
Relative_Name(Rel(N)) = Rightmost Rel(N-1),
Relative_Name(Full(N)) = Rightmost Full(N-1),
Else if N = 1, raise Name_Error.]}
  @end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[Finally, for Compose (ignoring the extension here):]}
  @begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Compose (Directory => Full(N), Relative_Name => Rel(M)) => Full(N+M)
Compose (Directory => Rel(N), Relative_Name => Rel(M)) => Rel(N+M)
Name_Error if Relative_Name is a Full(M).]}
  @end{Example}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We didn't try to write wording to reflect these
  details of these functions.]}
@end{Discussion}

@end{DescribeCode}
@end{StaticSem}

@begin{ImplAdvice}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0049-1]}
@ChgAdded{Version=[3],Text=[Directories.Hierarchical_File_Names should be
provided for systems with hierarchical file naming, and should not be provided
on other systems.]}
@ChgImplAdvice{Version=[3],Kind=[AddedNormal],Text=[@ChgAdded{Version=[3],
Text=[Directories.Hierarchical_File_Names should be
provided for systems with hierarchical file naming, and should not be provided
on other systems.]}]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This package should be provided when targeting
  Microsoft@latin1(174) Windows@latin1(174), Unix, Linux, and most Unix-like
  systems.]}
@end{ImplNote}

@end{ImplAdvice}


@begin{Notes}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0049-1]}
@ChgAdded{Version=[3],Text=[These operations operate on file names, not external
files. The files identified by these operations do not need to exist. Name_Error
is raised only as specified or if the file name is malformed and cannot possibly
identify a file. The result of these operations depends only on their
parameters.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0049-1]}
@ChgAdded{Version=[3],Text=[Containing_Directory raises Use_Error if Name does
not have a containing directory, including when any of Is_Simple_Name,
Is_Root_Directory_Name, Is_Parent_Directory_Name, or Is_Current_Directory_Name
are True.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[In particular, the default directory is not used
  to find the containing directory either when Is_Parent_Directory_Name or
  Is_Current_Directory_Name is True. As noted above, these functions operate
  purely on the syntax of the file names and do not attempt to interpret them.
  If interpretation is needed, Directories.Full_Name can be to expand any
  shorthands used before calling Containing_Directory.]}
@end{Ramification}
@end{Notes}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0049-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Package Ada.Directories.Hierarchical_File_Names is new.]}
@end{Extend2005}

