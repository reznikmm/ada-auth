@comment{ $Source: e:\\cvsroot/ARM/Source/pre_dirs.mss,v $ }
@comment{ $Revision: 1.7 $ $Date: 2004/12/16 06:31:50 $ $Author: Randy $ }
@Part(predefdirs, Root="ada.mss")

@Comment{$Date: 2004/12/16 06:31:50 $}

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
@ChgAdded{Version=[2],Text=[   @key{procedure} Search (
      Directory : @key{in} String;
      Pattern : @key{in} String;
      Filter : @key{in} Filter_Type := (@key{others} => True);
      Process : @key{not null access procedure} (Directory_Entry : @key{in} Directory_Entry_Type));]}

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
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[A directory is an external file, although it may
  not have a name on some targets. A directory is not a special file, as it
  can be created and read by Ada.Directories.]}
@end{Ramification}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Devices and soft links are examples of
  special files on Windows and Unix.]}

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
alternatives. A full name typically will include the names of all of
directories that contain the item. The @i<simple name> of an external file is
the name of the item, not including any containing directory names. Unless
otherwise specified, a file name or directory name parameter to a predefined
Ada input-output subprogram can be a full name, a simple name, or any other
form of name supported by the implementation.
@Defn2{Term=[full name],Sec=[of a file]}
@Defn2{Term=[simple name],Sec=[of a file]}]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The full name on Unix is a complete path to the
   root. For Windows, the full name includes a complete path, as well as a disk
   name ("C:") or network share name. For both systems, the simple name is
   the part of the name following the last '/' (or '\' for Windows). For
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
   the familiar @lquotes@;cd@rquotes@; command on Unix and Windows. Note that
   Windows maintains
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
                            Form : @key{in} String := "");]}
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
@ChgAdded{Version=[2],Type=[Trailing],Text=[Deletes an existing empty directory
with name Directory. The exception
Name_Error is propagated if the string given as Directory does not identify an
existing directory. The exception Use_Error is propagated if the external
environment does not support the deletion of the directory (or some portion of
its contents) with the given name (in the absence of Name_Error).]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Create_Path (New_Directory : @key{in} String;
                       Form : @key{in} String := "");]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Creates zero or more directories with
name New_Directory. Each
non-existent directory named by New_Directory is created.@Redundant[ For example, on a
typical Unix system, Create_Path ("/usr/me/my"); would create directory "me" in
directory "usr", then create directory "my" in directory "me".] The Form can be
used to give system-dependent characteristics of the directory; the
interpretation of the Form parameter is implementation-defined. A null string
for Form specifies the use of the default options of the implementation of the
new directory. The exception Name_Error is propagated if the string given as
New_Directory does not allow the identification of any directory. The exception
Use_Error is propagated if the external environment does not support the
creation of any directories with the given name (in the absence of Name_Error)
and form.]}

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
Use_Error is propagated, it is unspecified if a portion of the contents of the
directory are deleted.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Delete_File (Name : @key{in} String);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Deletes an existing ordinary or
special file with Name. The exception
Name_Error is propagated if the string given as Name does not identify an
existing ordinary or special external file. The exception Use_Error is
propagated if the external environment does not support the deletion of the
file with the given name (in the absence of Name_Error).]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Rename (Old_Name, New_Name : @key{in} String);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Renames an existing external file
(including directories) with Old_Name
to New_Name. The exception Name_Error is propagated if the string given as
Old_Name does not identify an existing external file. The exception Use_Error
is propagated if the external environment does not support the renaming of the
file with the given name (in the absence of Name_Error). In particular,
Use_Error is propagated if a file or directory already exists with New_Name.]}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{procedure} Copy_File (Source_Name, Target_Name : @key{in} String;
                     Form : @key{in} String);]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Copies the contents of the existing
external file with Source_Name to
Target_Name. The resulting external file is a duplicate of the source external
file. The Form can be used to give system-dependent characteristics of the
resulting external file; the interpretation of the Form parameter is
implementation-defined. Exception Name_Error is propagated if the string given
as Source_Name does not identify an existing external ordinary or special file
or if the string given as Target_Name does not allow the identification of an
external file. The exception Use_Error is propagated if the external
environment does not support the creating of the file with the name given by
Target_Name and form given by Form, or copying of the file with the name given
by Source_Name (in the absence of Name_Error).]}
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
  Thus, for Unix and Windows, the result should be a full path which does not
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
  is called on Windows, the result should be "..\AARM". If there is no
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
  @ChgAdded{Version=[2],Text=[For Unix and Windows, the extension is the
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
  @ChgAdded{Version=[2],Text=[For Unix and Windows, the base name is the
  portion of the simple name preceding the rightmost period (except for the
  special directory names "." and "..", whose Base_Name is "." and "..").
  For example, in the simple name "RM-A-8.html", the base name is "RM-A-8".]}
@end{Discussion}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Compose (Containing_Directory : @key{in} String := "";
                  Name : @key{in} String;
                  Extension : @key{in} String := "") @key{return} String;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the name of the external
file with the specified
Containing_Directory, Name, and Extension. If Extension is the null string,
then Name is interpreted as a simple name; otherwise Name is interpreted as a
base name. The exception Name_Error is propagated if the string given as
Containing_Directory is not null and does not allow the identification of a
directory, or if the string given as Extension is not null and is not a
possible extension, or if the string given as Name is not a possible simple
name (if Extension is null) or base name (if Extension is non-null).]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The above definition implies that if
  the Extension is null, for Unix and Windows no '.' is added to Name.]}
@end{Ramification}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If Name is null, Name_Error should be raised
  as nothing is not a possible simple name or base name.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Generally, Compose(Containing_Directory(F),
  Base_Name(F),Extension(F)) = F. However, this is not true on Unix or
  Windows for file names that end with a '.';
  Compose(Base_Name("Fooey."),Extension("Fooey.")) = "Fooey".
  This is not a problem for Windows, as the names have the same meaning with
  or without the '.', but these are different names for Unix. Thus,
  care needs to be taken on Unix; if Extension is null, Base_Name should
  be avoided. (That's not usually a problem with file names generated by a
  program.)]}
@end{Discussion}

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
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns True if external file
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
If the external file is discontiguous (not all elements exist), the result is
implementation-defined. If the external file is not an ordinary file, the
result is implementation-defined. The exception Name_Error is propagated if the
string given as Name does not allow the identification of an existing external
file. The exception Constraint_Error is propagated if the file size is not a
value of type File_Size.]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@Chg{Version=[2],New=[The
result for Directories.Size for a directory, special file, or
discontiguous file.],Old=[]}]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Trailing],Text=[We allow raising Constraint_Error,
  so that an implementation for a system with 64-bit file sizes does not
  need to support full numerics on 64-bit integers just to implement
  this package. Of course, if 64-bit integers are available on such a system,
  they should be used when defining type File_Size.]}
@end{Discussion}

@begin{Example}@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Modification_Time (Name : @key{in} String) @key{return} Ada.Calendar;]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Trailing],Text=[Returns the time that the
external file represented by Name was most
recently modified. If the external file is not an ordinary file, the result is
implementation-defined. The exception Name_Error is propagated if the string
given as Name does not allow the identification of an existing external file. Th
exception Use_Error is propagated if the external environment does not support
the reading the modification time of the file with the name given by Name (in
the absence of Name_Error).]}
@ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@Chg{Version=[2],New=[The
result for Directories.Modification_Time for a directory or special file.],
Old=[]}]}

@end{DescribeCode}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The following directory searching
operations and types are provided:]}

@begin{DescribeCode}



**** The rest of this clause has yet to be inserted ****

@end{DescribeCode}


@end{StaticSem}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
@ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
Package Ada.Directories is new.]}
@end{Extend95}

